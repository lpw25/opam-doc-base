
module type Name = sig
  include OpamMisc.ABSTRACT
  val compare : t -> t -> int
end

module Package = OpamPackage.Name

module Library = OpamLibrary.Name

module MakeName(S : sig val of_string: string -> string end) = struct

  type t = string

  let to_string x = x

  let of_string = S.of_string

  let to_json x = `String x

  let compare n1 n2 =
    match compare (String.lowercase n1) (String.lowercase n2) with
    | 0 -> compare n1 n2
    | i -> i

  module O = struct
    type t = string
    let compare = compare
    let to_string = to_string
    let to_json = to_json
  end

  module Set = OpamMisc.Set.Make(O)

  module Map = OpamMisc.Map.Make(O)

end

let check_ident x =
  let len = String.length x in
  if len = 0 then false
  else
    match x.[0] with
    | 'A' .. 'Z' | 'a' .. 'z' | '_'->
        let ok = ref true in
        for i = 1 to (len - 1) do
          match x.[i] with
          | 'A' .. 'Z'
          | 'a' .. 'z'
          | '0' .. '9'
          | '_' | '\'' -> ()
          | c -> ok := false
        done;
        !ok
    | c -> false

let check_uident x =
  let len = String.length x in
  if len = 0 then false
  else
    match x.[0] with
    | 'A' .. 'Z' ->
        let ok = ref true in
        for i = 1 to (len - 1) do
          match x.[i] with
          | 'A' .. 'Z'
          | 'a' .. 'z'
          | '0' .. '9'
          | '_' | '\'' -> ()
          | c -> ok := false
        done;
        !ok
    | c -> false

let check_lident x =
  let len = String.length x in
  if len = 0 then false
  else if x = ":=" then true
  else match x.[0] with
    | 'a' .. 'z' | '_' ->
        let ok = ref true in
        for i = 1 to (len - 1) do
          match x.[i] with
          | 'A' .. 'Z'
          | 'a' .. 'z'
          | '0' .. '9'
          | '_' | '\'' -> ()
          | c -> ok := false
        done;
        !ok
    | c -> false

let check_tident x =
  let len = String.length x in
  if len = 0 then false
  else match x.[0] with
    | '#' -> len > 1 && check_lident (String.sub x 1 (len-1))
    | 'a' .. 'z' | '_' ->
        let ok = ref true in
        for i = 1 to (len - 1) do
          match x.[i] with
          | 'A' .. 'Z'
          | 'a' .. 'z'
          | '0' .. '9'
          | '_' | '\'' -> ()
          | c -> ok := false
        done;
        !ok
    | c -> false

let check_operator x =
  let len = String.length x in
  if len = 0 then None
  else
    let first, last =
      if (len > 2) && (x.[0] = '(') && (x.[len - 1] = ')') then 1, len-2
      else 0, len-1
    in
    if not (String.contains "=<>@^|&+-*/$%!?~" x.[first]) then None
    else
      let ok = ref true in
      for i = first + 1 to last - 1 do
        if not (String.contains "!$%&*+-./:<=>?@^|~" x.[i]) then ok := false
      done;
      if !ok then Some (String.sub x first ((last - first) + 1))
      else None

module UName = MakeName(struct

  let of_string x =
    if check_uident x then x
    else failwith ("Invalid uppercase name " ^ x)

end)

module LName = MakeName(struct

  let of_string x =
    if check_lident x then x
    else failwith ("Invalid lowercase name " ^ x)

end)

module IName = MakeName(struct

  let of_string x =
    if check_ident x then x
    else failwith ("Invalid name " ^ x)

end)

module Module = UName

module ModuleType = IName

module Type = MakeName(struct

  let of_string x =
    if check_tident x then x
    else failwith ("Invalid type name " ^ x)

end)

module Value = MakeName(struct

  let of_string x =
    if check_lident x then x
    else match check_operator x with
      | Some x -> "(" ^ x ^ ")"
      | None -> failwith ("Invalid name " ^ x)

end)

module Constructor = UName

module Field = LName

module Exn = UName
