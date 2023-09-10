open! Core
open! Bonsai.For_open
module Vdom := Virtual_dom.Vdom

val start : ?bind_to_element_with_id:string -> Vdom.Node.t Computation.t -> unit
