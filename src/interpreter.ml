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
      | _ -> failwith "Invalid usage of operator" |> (fun _ -> List [])
    end
