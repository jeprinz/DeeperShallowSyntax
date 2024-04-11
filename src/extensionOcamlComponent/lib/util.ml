let find_index (xs : 'a list) (value : 'a) : int option =
  let rec impl (xs : 'a list) (acc : int) : int option =
    match xs with
    | x :: xs -> if x = value then Some acc else impl xs (1 + acc)
    | [] -> None
  in impl xs 0