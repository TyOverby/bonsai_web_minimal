(** A [Component] captures the basic operations that need to be provided in order to build
    an Incr_dom app.  The same type can be used both at the top-level of an app,
    as well as for defining individual components within a larger app.

    The {!Component.t} is often constructed incrementally, and is always created
    incrementally at the top-level, as is required by {!App_intf.S}. *)

open Virtual_dom

(** The with_extra type allows the component to expose extra information that might be
    useful to the user of the component. This allows the module in which the component is
    defined to expose functions that can be used to query incrementally computed
    information about the the component. *)
type ('action, 'model, 'state, 'extra) with_extra

type ('action, 'model, 'state) t = ('action, 'model, 'state, unit) with_extra

(** [apply_action] is called to update the model in response to the arrival of a scheduled
    action. The function may have side effects, which among other things can trigger more
    actions. *)
val apply_action
  :  ('action, 'model, 'state, _) with_extra
  -> 'action
  -> 'state
  -> schedule_action:('action -> unit)
  -> 'model

(** [view] is called to obtain the virtual DOM which is then applied to the actual DOM. *)
val view : _ with_extra -> Vdom.Node.t

(** [extra] allows you to expose extra information that was derived from the model. Note
    that for most components, [extra] has type [unit]. *)
val extra : (_, _, _, 'extra) with_extra -> 'extra

(** [on_display] is called every time the DOM is updated, with the model just before the
    update and the model just after the update. Use [on_display] to initiate actions. *)
val on_display
  :  ('action, _, 'state, _) with_extra
  -> 'state
  -> schedule_action:('action -> unit)
  -> unit

(** Though this create function is not incremental, it is usually called in the context of
    an incremental computation function, like the one in {!App_intf.S}. If some
    arguments are not supplied, defaults (which either return the model supplied or unit)
    are included in the component.

    Note that the functions [apply_action], [update_visibility] and [on_display] are
    allowed to perform effects, but the incremental computation that produces [create]
    should itself be functional. *)
val create
  :  ?apply_action:('action -> 'state -> schedule_action:('action -> unit) -> 'model)
  -> ?on_display:('state -> schedule_action:('action -> unit) -> unit)
  -> 'model
  -> Vdom.Node.t
  -> ('action, 'model, 'state) t

(** Like {!create}, but allows for the specification of the [extra] parameter, which
    allows for component-specific state to be exposed. *)
val create_with_extra
  :  ?apply_action:('action -> 'state -> schedule_action:('action -> unit) -> 'model)
  -> ?on_display:('state -> schedule_action:('action -> unit) -> unit)
  -> extra:'extra
  -> 'model
  -> Vdom.Node.t
  -> ('action, 'model, 'state, 'extra) with_extra
