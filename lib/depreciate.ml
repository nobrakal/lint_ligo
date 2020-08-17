open Rules

let _make_msg {dep; dep_version; dep_replacement; dep_message} =
  let with_default f = Option.fold ~none:"" ~some:f in
  let repl = with_default (fun x -> "A possible replacement is " ^ x ^ ".") dep_replacement in
  let mess = with_default (fun x -> " " ^ x ^ ".") dep_message in
  dep ^ " was depreciated in version "  ^ dep_version ^ "." ^ repl ^ mess

let depreciate _ast _depreciated = []
