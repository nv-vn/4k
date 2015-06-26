let filter2 f l r =
  if List.length l <> List.length r then
    failwith "Can't filter lists of incompatible sizes" |> (fun _ -> [])
  else
    let rec loop res = function
      | ([], []) -> res
      | (a :: t, a' :: t') ->
        begin
          if f a a' then
            loop (res @ [a']) (t, t')
          else
            loop res (t, t')
        end
      | (_, _) -> failwith "Error while filtering lists" |> (fun _ -> [])
    in loop [] (l, r)
let multimap2 f l r =
  if List.length l <> List.length r then
    failwith "Can't filter lists of incompatible sizes" |> (fun _ -> [])
  else
    let rec loop res = function
      | ([], []) -> res
      | (a :: t, a' :: t') ->
        begin
          let x = f a a' in
          loop (res @ x) (t, t')
        end
      | (_, _) -> failwith "Error while filtering lists" |> (fun _ -> [])
    in loop [] (l, r)
let expand f =
  let rec inner progress head = function
    | [] -> progress
    | a :: t -> inner (progress @ [f a head]) (f a head) t
  in inner [] 0.0
