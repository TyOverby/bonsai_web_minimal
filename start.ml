open! Core
module Vdom = Virtual_dom.Vdom
module Incr = Ui_incr

module type Result_spec = sig
  type t
  type extra
  type incoming

  val view : t -> Vdom.Node.t
  val extra : t -> extra
  val incoming : t -> incoming -> unit Vdom.Effect.t
end

module Arrow_deprecated = struct
  module Handle = struct
    module Injector = struct
      type 'a t =
        | Before_app_start of 'a Queue.t
        | Inject of ('a -> unit Vdom.Effect.t)
    end

    type ('input, 'extra, 'incoming) t =
      { mutable injector : 'incoming Injector.t
      ; stop : bool ref
      ; started : bool ref
      }

    let create () =
      { injector = Before_app_start (Queue.create ())
      ; stop = ref false
      ; started = ref true
      }
    ;;

    let set_started t = t.started := true

    let schedule t a =
      match t.injector with
      | Inject f -> f a |> Vdom.Effect.Expert.handle_non_dom_event_exn
      | Before_app_start queue -> Queue.enqueue queue a
    ;;

    let set_inject t inject =
      let prev = t.injector in
      t.injector <- Inject inject;
      match prev with
      | Inject _ -> ()
      | Before_app_start queue -> Queue.iter queue ~f:(schedule t)
    ;;
  end

  module App_input = struct
    type ('input, 'outgoing) t =
      { input : 'input
      ; inject_outgoing : 'outgoing -> unit Vdom.Effect.t
      }
    [@@deriving fields ~getters ~iterators:create]

    let create = Fields.create
  end

  module App_result = struct
    type ('extra, 'incoming) t =
      { view : Vdom.Node.t
      ; extra : 'extra
      ; inject_incoming : 'incoming -> unit Vdom.Effect.t
      }
    [@@deriving fields ~iterators:create]

    let of_result_spec
      (type result extra incoming)
      (module Result : Result_spec
        with type t = result
         and type extra = extra
         and type incoming = incoming)
      (r : Result.t)
      =
      { view = Result.view r
      ; extra = Result.extra r
      ; inject_incoming = Result.incoming r
      }
    ;;
  end

  let start_generic_poly
    (type input action_input input_and_inject model dynamic_action static_action result
    extra incoming outgoing)
    ~(get_app_result : result -> (extra, incoming) App_result.t)
    ~(get_app_input :
       input:input -> inject_outgoing:(outgoing -> unit Vdom.Effect.t) -> input_and_inject)
    ~(initial_input : input)
    ~bind_to_element_with_id
    ~(computation : result Bonsai.Private.Computation.t)
    ~fresh
    ({ model
     ; input = _
     ; dynamic_action
     ; static_action
     ; apply_static
     ; apply_dynamic
     ; run
     ; reset = _
     } as info :
      ( model
      , dynamic_action
      , static_action
      , action_input
      , result )
      Bonsai.Private.Computation.info)
    : (input, extra, incoming) Handle.t
    =
    let input_var = Incr.Var.create initial_input in
    let handle = Handle.create () in
    let input =
      let%map.Incr input = Incr.Var.watch input_var in
      get_app_input ~input
    in
    let prev_lifecycle = ref Bonsai.Private.Lifecycle.Collection.empty in
    let bonsai_clock = Bonsai.Time_source.create ~start:(Time_ns.now ()) in
    let module Incr_dom_app = struct
      module Model = struct
        type t = model

        let cutoff = phys_equal
      end

      module State = struct
        type t = unit
      end

      module Action = struct
        let sexp_of_dynamic_action =
          Bonsai.Private.Meta.Action.Type_id.to_sexp dynamic_action
        ;;

        let sexp_of_static_action =
          Bonsai.Private.Meta.Action.Type_id.to_sexp static_action
        ;;

        type t =
          | Dynamic of dynamic_action
          | Static of static_action
        [@@deriving sexp_of]
      end

      let action_requires_stabilization = function
        | Action.Dynamic _ -> true
        | Static _ -> false
      ;;

      let advance_clock_to to_ =
        Bonsai.Time_source.advance_clock bonsai_clock ~to_;
        Bonsai.Time_source.Private.flush bonsai_clock
      ;;

      let create
        model
        ~old_model:_
        ~inject
        (run :
          ( model
          , dynamic_action
          , static_action
          , action_input
          , result )
          Bonsai.Private.Computation.eval_fun)
        =
        let open Incr.Let_syntax in
        let environment =
          Bonsai.Private.Environment.(empty |> add_exn ~key:fresh ~data:input)
        in
        let inject_dynamic a = inject (Action.Dynamic a) in
        let inject_static a = inject (Action.Static a) in
        let snapshot =
          run
            ~environment
            ~path:Bonsai.Private.Path.empty
            ~clock:bonsai_clock
            ~model
            ~inject_dynamic
            ~inject_static
        in
        let%map view =
          let%map { App_result.view; extra = _; inject_incoming } =
            snapshot |> Bonsai.Private.Snapshot.result >>| get_app_result
          in
          Handle.set_inject handle inject_incoming;
          view
        and apply_action =
          let%map input =
            snapshot
            |> Bonsai.Private.Snapshot.input
            |> Bonsai.Private.Input.to_incremental
          in
          fun () ~schedule_event model action ->
            match action with
            | Action.Dynamic action ->
              apply_dynamic
                ~inject_dynamic
                ~inject_static
                ~schedule_event
                (Some input)
                model
                action
            | Action.Static action ->
              apply_static ~inject_dynamic ~inject_static ~schedule_event model action
        and on_display =
          let%map lifecycle = Bonsai.Private.Snapshot.lifecycle_or_empty snapshot in
          fun () ~schedule_event ->
            Handle.set_started handle;
            schedule_event
              (Bonsai.Private.Lifecycle.Collection.diff !prev_lifecycle lifecycle);
            Bonsai.Time_source.Private.trigger_after_display bonsai_clock;
            prev_lifecycle := lifecycle
        in
        let update_visibility model ~schedule_event:_ = model in
        { Incr_dom_app_intf.Private.view; apply_action; update_visibility; on_display }
      ;;

      let create model ~old_model ~inject =
        let safe_start computation =
          let (T info') = Bonsai.Private.gather computation in
          match
            Bonsai.Private.Meta.(
              ( Model.Type_id.same_witness info.model.type_id info'.model.type_id
              , Action.Type_id.same_witness info.dynamic_action info'.dynamic_action
              , Action.Type_id.same_witness info.static_action info'.static_action
              , Input.same_witness info.input info'.input ))
          with
          | Some T, Some T, Some T, Some T -> create model ~old_model ~inject info'.run
          | _ ->
            print_endline
              "Not starting debugger. An error occurred while attempting to instrument \
               the computation; the resulting computation does not typecheck. Reusing \
               previously gathered run information to execute";
            create model ~old_model ~inject run
        in
        safe_start computation
      ;;

      let on_startup ~schedule_action:_ _ = ()
    end
    in
    Incr_dom_start_app.Private.start_bonsai
      ~bind_to_element_with_id
      ~initial_model:model.default
      ~stop:handle.stop
      (module Incr_dom_app);
    handle
  ;;

  let start_generic
    ~optimize
    ~get_app_result
    ~initial_input
    ~bind_to_element_with_id
    ~component
    =
    let fresh = Type_equal.Id.create ~name:"" sexp_of_opaque in
    let var =
      Bonsai.Private.Value.named App_input fresh |> Bonsai.Private.conceal_value
    in
    let computation =
      component var
      |> Bonsai.Private.reveal_computation
      |> if optimize then Bonsai.Private.pre_process else Fn.id
    in
    let (T info) = Bonsai.Private.gather computation in
    start_generic_poly
      ~get_app_result
      ~initial_input
      ~bind_to_element_with_id
      ~computation
      ~fresh
      info
  ;;

  let start ?(optimize = true) ~initial_input ~bind_to_element_with_id component =
    start_generic
      ~optimize
      ~get_app_result:Fn.id
      ~get_app_input:App_input.create
      ~initial_input
      ~bind_to_element_with_id
      ~component
  ;;
end

module Proc = struct
  module Handle = struct
    include Arrow_deprecated.Handle

    type ('extra, 'incoming) t = (unit, 'extra, 'incoming) Arrow_deprecated.Handle.t
  end

  module Result_spec = struct
    module type S = Result_spec

    module No_extra = struct
      type extra = unit

      let extra _ = ()
    end

    module No_incoming = struct
      type incoming = Nothing.t

      let incoming _ = Nothing.unreachable_code
    end

    let just_the_view =
      (module struct
        type t = Vdom.Node.t

        let view = Fn.id

        include No_extra
        include No_incoming
      end : S
        with type t = Vdom.Node.t
         and type extra = unit
         and type incoming = Nothing.t)
    ;;
  end

  let start_and_get_handle
    result_spec
    ?(optimize = true)
    ~bind_to_element_with_id
    computation
    =
    let bonsai =
      Fn.const computation
      |> Bonsai.Arrow_deprecated.map
           ~f:(Arrow_deprecated.App_result.of_result_spec result_spec)
    in
    Arrow_deprecated.start ~optimize ~initial_input:() ~bind_to_element_with_id bonsai
  ;;

  let start ?(bind_to_element_with_id = "app") component =
    let (_ : _ Handle.t) =
      start_and_get_handle Result_spec.just_the_view ~bind_to_element_with_id component
    in
    ()
  ;;
end

let start = Proc.start
