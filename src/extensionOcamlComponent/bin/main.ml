open Js_of_ocaml

let () = print_endline "Hello, World!"

let _ =
  Js.export "backend"
    (object%js
      method test x =
          x + 11
      method printMessage (_ : int) =
        print_endline "Hello World from the ocaml backend!"
        ; Firebug.console##log "maybe this will print"
    end)