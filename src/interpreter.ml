open Ast

let prefix_plus = function
  | List l -> List l
  | _ -> failwith "Invalid application of prefix function" |> (fun _ -> List [])
let prefix_minus = function
  | List l -> List (List.map (fun n -> -.n) l)
  | _ -> failwith "Invalid application of prefix function" |> (fun _ -> List [])
let prefix_times = function
  | List l -> List (List.map (fun n -> if n > 0.0 then 1.0 else if n < 0.0 then -1.0 else 0.0) l)
  | _ -> failwith "Invalid application of prefix function" |> (fun _ -> List [])
let prefix_divide = function
  | List l -> List (List.map (fun n -> 1.0 /. n) l)
  | _ -> failwith "Invalid application of prefix function" |> (fun _ -> List [])

let infix_plus a b =
  match (a, b) with
  | (List a', List b') ->
    begin
      if List.length a' <> List.length b' then
        failwith "Size error" |> (fun _ -> List [])
      else
        List (List.map2 (fun l r -> l +. r) a' b')
    end
  | _ -> failwith "Invalid application of infix function" |> (fun _ -> List [])
let infix_minus a b =
  match (a, b) with
  | (List a', List b') ->
    begin
      if List.length a' <> List.length b' then
        failwith "Size error" |> (fun _ -> List [])
      else
        List (List.map2 (fun l r -> l -. r) a' b')
    end
  | _ -> failwith "Invalid application of infix function" |> (fun _ -> List [])
let infix_times a b =
  match (a, b) with
  | (List a', List b') ->
    begin
      if List.length a' <> List.length b' then
        failwith "Size error" |> (fun _ -> List [])
      else
        List (List.map2 (fun l r -> l *. r) a' b')
    end
  | _ -> failwith "Invalid application of infix function" |> (fun _ -> List [])
let infix_divide a b =
  match (a, b) with
  | (List a', List b') ->
    begin
      if List.length a' <> List.length b' then
        failwith "Size error" |> (fun _ -> List [])
      else
        List (List.map2 (fun l r -> l /. r) a' b')
    end
  | _ -> failwith "Invalid application of infix function" |> (fun _ -> List [])
let infix_reduce a b =
  match (a, b) with
  | (List a', List b') ->
    begin
      if List.length a' <> List.length b' then
        failwith "Size error" |> (fun _ -> List [])
      else
        List (Util.multimap2 (fun l r -> if l < 0.0 then Batteries.List.make (-int_of_float l) 0.0 else BatList.make (int_of_float l) r) a' b')
    end
  | _ -> failwith "Invalid application of infix function" |> (fun _ -> List [])

let interpret = function
  | List l -> List l
  | Operation a ->
    begin
      match a with
      | Prefix (op, ast) ->
        begin
          match op with
          | Plus -> prefix_plus ast
          | Minus -> prefix_minus ast
          | Times -> prefix_times ast
          | Divide -> prefix_divide ast
          | _ -> failwith "Invalid operation" |> (fun _ -> List [])
        end
      | Infix (op, left, right) ->
        begin
          match op with
          | Plus -> infix_plus left right
          | Minus -> infix_minus left right
          | Times -> infix_times left right
          | Divide -> infix_divide left right
          | Reduce -> infix_reduce left right
          | _ -> failwith "Invalid operation" |> (fun _ -> List [])
        end
      | ReduceOp (op, ast) ->
        begin
          match ast with
          | List l ->
            begin
              match op with
              | Plus -> List [Batteries.List.reduce ( +. ) l]
              | Minus -> List [Batteries.List.reduce ( -. ) l]
              | Times -> List [Batteries.List.reduce ( *. ) l]
              | Divide -> List [Batteries.List.reduce ( /. ) l]
              | _ -> failwith "Invalid operation in reduction" |> (fun _ -> List [])
            end
          | _ -> failwith "Invalid application of reduction" |> (fun _ -> List [])
        end
      | _ -> failwith "Invalid usage of operator" |> (fun _ -> List [])
    end
