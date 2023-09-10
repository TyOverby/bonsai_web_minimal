open! Core
open Virtual_dom
open Js_of_ocaml
module Incr = Ui_incr

let print_errorf fmt = ksprintf (fun s -> Firebug.console##error (Js.string s)) fmt

module Request_ids : sig
  type t

  val create : unit -> t

  val set_once_exn
    :  t
    -> animation_frame_id:Dom_html.animation_frame_request_id
    -> set_timeout_id:Dom_html.timeout_id
    -> unit

  val cancelled : t -> bool
  val cancel : t -> unit
end = struct
  type ids =
    | Empty
    | Cancelled
    | Ids of
        { animation_frame_id : Dom_html.animation_frame_request_id
        ; set_timeout_id : Dom_html.timeout_id
        }

  type t = ids ref

  let create () : t = ref Empty

  let set_once_exn (t : t) ~animation_frame_id ~set_timeout_id =
    match !t with
    | Cancelled ->
      (* This should not happen, but let's be defensive. *)
      Dom_html.window##cancelAnimationFrame animation_frame_id;
      Dom_html.window##clearTimeout set_timeout_id
    | Empty -> t := Ids { animation_frame_id; set_timeout_id }
    | Ids _ -> invalid_arg "request_ids already set"
  ;;

  let cancelled x =
    match !x with
    | Cancelled -> true
    | Empty | Ids _ -> false
  ;;

  let cancel (t : t) =
    match !t with
    | Cancelled -> ()
    | Empty -> t := Cancelled
    | Ids { animation_frame_id; set_timeout_id } ->
      Dom_html.window##cancelAnimationFrame animation_frame_id;
      Dom_html.window##clearTimeout set_timeout_id;
      t := Cancelled
  ;;
end

(** [request_animation_frame] notifies the browser that you would like to do some
    computation before the next repaint. Because this needs to occur in the same
    synchronous call (called before the next repaint), returning a Deferred.t will not
    work. Instead, you pass in a job to be run before the repaint.

    Note that if [callback] contains any asynchronous work before doing DOM changes, those
    changes will not be included in the repaint and will be saved until the following one.

    When the tab is in the background, the browsers native requestAnimationFrame function
    will never call the callback, so in order to continue processing events, we set an
    alternate setTimeout at 1 second. *)
let request_animation_frame callback =
  (* We capture the current context to use it later when handling callbacks from
     requestAnimationFrame, since exceptions raised to that would otherwise not go through
     our ordinary Async monitor. *)
  let request_ids = Request_ids.create () in
  let callback () =
    if Request_ids.cancelled request_ids
    then ()
    else (
      Request_ids.cancel request_ids;
      let callback_result = callback () in
      ignore callback_result)
  in
  let animation_frame_id =
    let animation_callback = Js.wrap_callback (fun _ -> callback ()) in
    Dom_html.window##requestAnimationFrame animation_callback
  in
  let set_timeout_id =
    let timeout_callback = Js.wrap_callback (fun _ -> callback ()) in
    (* 1000 ms = 1s;  Chosen because backgrounded tangle sends requests
       at approximately this rate. *)
    let timeout = 1000.0 in
    Dom_html.window##setTimeout timeout_callback timeout
  in
  Request_ids.set_once_exn request_ids ~animation_frame_id ~set_timeout_id
;;

(** The Js_of_ocaml type Dom_html.element doesn't have the correct options for
    their `focus` method. Cast to this in order to work around this bug. *)
type focusable =
  < focus : < preventScroll : bool Js.t Js.readonly_prop > Js.t -> unit Js.meth >

let as_focusable : Dom_html.element Js.t -> focusable Js.t = Js.Unsafe.coerce

(* Adds the necessary attribute to the root node so that it can intercept
   keyboard events.
   https://developer.mozilla.org/en-US/docs/Web/HTML/Global_attributes/tabindex *)
let override_root_element root =
  let open Vdom in
  let should_add_focus_modifiers element =
    element |> Node.Element.attrs |> Attr.Expert.contains_name "disable_tab_index" |> not
  in
  match (root : Node.t) with
  | Element element when should_add_focus_modifiers element ->
    let add_new_attrs attrs =
      Vdom.Attr.(style (Css_gen.outline ~style:`None ()) @ tabindex 0 @ attrs)
    in
    element |> Node.Element.map_attrs ~f:add_new_attrs |> Node.Element
  | _ -> root
;;

let rec get_tag_name (node : Vdom.Node.t) =
  match node with
  | Element e -> Some (Vdom.Node.Element.tag e)
  | Lazy { t; _ } -> get_tag_name (Lazy.force t)
  | None | Text _ | Widget _ -> None
;;

let time_source : Ui_time_source.t = Ui_time_source.create ~start:(Time_ns.now ())

let rec on_document_load f =
  let open Js_of_ocaml in
  match Js.to_string Dom_html.document##.readyState with
  | "complete" -> f ()
  | _ ->
    let handler ret =
      on_document_load f;
      ret
    in
    let _listener_id =
      Dom_html.addEventListenerWithOptions
        Dom_html.document
        (Dom_html.Event.make "readystatechange")
        (Dom_html.handler (fun _ -> handler Js._true))
        ~passive:Js._true
        ~once:Js._true
    in
    ()
;;

let start_bonsai
  (type model action)
  ?(stop = ref false)
  ~bind_to_element_with_id
  ~initial_model
  (module App : Incr_dom_app_intf.Private.S_for_bonsai
    with type Model.t = model
     and type Action.t = action)
  =
  (* This is idempotent and so fine to do. *)
  on_document_load (fun () ->
    let model_v = Incr.Var.create initial_model in
    let model = Incr.Var.watch model_v in
    let model_from_last_display_v = Incr.Var.create initial_model in
    let model_from_last_display = Incr.Var.watch model_from_last_display_v in
    let cutoff =
      Incr.Cutoff.create (fun ~old_value ~new_value ->
        App.Model.cutoff old_value new_value)
    in
    Incr.set_cutoff model cutoff;
    Incr.set_cutoff model_from_last_display cutoff;
    let action_queue = Deque.create () in
    let module Event =
      Vdom.Effect.Define (struct
        module Action = App.Action

        let handle action = Deque.enqueue_back action_queue action
      end)
    in
    (* This registers the [viewport_changed] handler with Virtual_dom. If event handlers
       use the [Vdom.Effect.Viewport_changed] event, we are notified. *)
    let get_view, get_apply_action, get_on_display =
      let obs =
        Incr.observe
          (App.create model ~old_model:model_from_last_display ~inject:Event.inject)
      in
      let fetch (f : _ Incr_dom_app_intf.Private.snapshot -> _) () =
        f (Incr.Observer.value_exn obs)
      in
      ( fetch (fun { view; _ } -> view)
      , fetch (fun { apply_action; _ } -> apply_action)
      , fetch (fun { on_display; _ } -> on_display) )
    in
    Incr.stabilize ();
    let html = get_view () in
    let html_dom = Vdom.Node.to_dom html in
    let elem = Dom_html.getElementById_exn bind_to_element_with_id in
    let parent = Option.value_exn ~here:[%here] (Js.Opt.to_option elem##.parentNode) in
    Dom.replaceChild parent html_dom elem;
    (* we make sure to call [viewport_changed] whenever the window resizes or the scroll
       container in which our HTML is located is scrolled. *)
    let state =
      App.on_startup
        ~schedule_action:(fun a -> Ui_effect.Expert.handle (Event.inject a))
        (Incr.Var.value model_v)
    in
    let prev_html = ref html in
    let prev_elt = ref html_dom in
    let refocus_root_element () =
      let element = !prev_elt in
      (* If the element to focus is an element, cast it into the
         more permissive "focusable" type defined at the top of
         this file, and then focus that. *)
      Dom_html.CoerceTo.element element
      |> Js.Opt.to_option
      |> Option.map ~f:as_focusable
      |> Option.iter ~f:(fun element ->
        element##focus
          (object%js
             val preventScroll = Js._true
          end))
    in
    ignore
    @@ Dom.addEventListener
         Dom_html.window
         Dom_html.Event.blur
         (Dom_html.handler (fun e ->
            (* [Js.Unsafe.*] is like [Obj.magic]. We should be explicit about what we
               expect. *)
            let e : < relatedTarget : Dom_html.element Js.t Js.opt Js.readonly_prop > Js.t
              =
              Js.Unsafe.coerce e
            in
            let related_target = e##.relatedTarget in
            if not (Js.Opt.test related_target) then refocus_root_element ();
            Js._true))
         Js._true;
    let apply_action action =
      if App.action_requires_stabilization action then Incr.stabilize ();
      let new_model =
        (get_apply_action ())
          state
          ~schedule_event:Ui_effect.Expert.handle
          (Incr.Var.latest_value model_v)
          action
      in
      Incr.Var.set model_v new_model
    in
    let rec apply_actions () =
      match Deque.dequeue_front action_queue with
      | None -> ()
      | Some action ->
        apply_action action;
        apply_actions ()
    in
    let perform_update () =
      (* The clock is set only once per call to perform_update, so that all actions that
         occur before each display update occur "at the same time." *)
      let now =
        let date = new%js Js.date_now in
        Time_ns.Span.of_ms date##getTime |> Time_ns.of_span_since_epoch
      in
      Incr.Clock.advance_clock Incr.clock ~to_:now;
      App.advance_clock_to now;
      Incr.stabilize ();
      apply_actions ();
      Incr.stabilize ();
      let html = get_view () in
      let html = override_root_element html in
      let patch = Vdom.Node.Patch.create ~previous:!prev_html ~current:html in
      let elt = Vdom.Node.Patch.apply patch !prev_elt in
      (get_on_display ()) state ~schedule_event:Ui_effect.Expert.handle;
      Incr.Var.set model_from_last_display_v (Incr.Var.value model_v);
      let old_tag_name = get_tag_name !prev_html in
      let new_tag_name = get_tag_name html in
      let tags_the_same = Option.equal String.equal old_tag_name new_tag_name in
      prev_html := html;
      prev_elt := elt;
      (* Changing the tag name causes focus to be lost.  Refocus in that case. *)
      if not tags_the_same then refocus_root_element ()
    in
    (* We use [request_animation_frame] so that browser tells us where it's time to
       refresh the UI. All the actions will be processed and the changes propagated
       to the DOM in one frame. *)
    let rec callback () =
      if !stop
      then ()
      else (
        perform_update ();
        request_animation_frame callback)
    in
    (* We want the root element to start out focused, so perform an initial
       update/render, then immediately focus the root (unless a non-body element already
       has focus).  This focusing can't happen inside of the `callback` because then it
       would refocus root every frame. *)
    perform_update ();
    (match Js.Opt.to_option Dom_html.document##.activeElement with
     | Some el -> if Js.Opt.test (Dom_html.CoerceTo.body el) then refocus_root_element ()
     | None -> refocus_root_element ());
    request_animation_frame callback)
;;

module Private = struct
  let start_bonsai = start_bonsai
  let time_source = time_source
end
