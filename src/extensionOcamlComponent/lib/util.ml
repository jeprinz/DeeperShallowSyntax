(* Throughout this codebase, we work with generic tree *)
type 'label tree = Node of 'label * ('label tree list)
let node : 'label -> 'label tree list -> 'label tree =
  fun l kids -> Node(l, kids)

let find_index (xs : 'a list) (value : 'a) : int option =
  let rec impl (xs : 'a list) (acc : int) : int option =
    match xs with
    | x :: xs -> if x = value then Some acc else impl xs (1 + acc)
    | [] -> None
  in impl xs 0

let find_map_index (f : int -> 'a -> 'b option) (xs : 'a list) : 'a option =
  let rec impl (xs : 'a list) (acc : int) : 'a option =
    match xs with
    | x :: xs -> (
      match f acc x with
      | None -> impl xs (1 + acc)
      | Some res -> Some res
    )
    | [] -> None
  in impl xs 0

let sequence (l : 'a option list) : 'a list option =
  List.fold_right (fun x acc -> Option.bind x (fun x -> Option.bind acc (fun acc -> Some (x :: acc)))) l (Some [])

let rec allPairs (f : 't -> 't -> bool) (l : 't list) : bool =
  match l with
  | [] -> true
  | x :: xs ->
    List.for_all (f x) xs && allPairs f xs

let log_time (msg : string) : unit =
  print_endline ("At " ^ msg ^ " time " ^ string_of_float (Sys.time ()) ^ "s")

exception Error of string