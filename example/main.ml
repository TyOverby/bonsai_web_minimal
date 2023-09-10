open! Core
open! Bonsai_web

let () = Start.start (Bonsai.const (Vdom.Node.text "hello world"))
