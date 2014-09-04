
(*
 * Copyright (c) 2014 Leo White <lpw25@cl.cam.ac.uk>
 *
 * Permission to use, copy, modify, and distribute this software for any
 * purpose with or without fee is hereby granted, provided that the above
 * copyright notice and this permission notice appear in all copies.
 *
 * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
 * WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
 * ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
 * ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
 * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 *)

module Package = OpamPackage

module Library = OpamLibrary

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
  else match x with
    | ":=" -> true
    | _ -> match x.[0] with
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

module UName = struct

  type t = string

  let to_string x = x

  let of_string x =
    if check_uident x then x
    else failwith ("Invalid uppercase name " ^ x)

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

module LName = struct

  type t = string

  let to_string x = x

  let of_string x =
    if check_lident x then x
    else failwith ("Invalid lowercase name " ^ x)

  let to_json s = `String s

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

module TName = struct

  type t = string

  let to_string x = x

  let of_string x =
    if check_tident x then x
    else failwith ("Invalid type name " ^ x)

  let to_json s = `String s

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

module ValName = struct

  type t = string

  let to_string x = x

  let of_string x =
    if check_lident x then x
    else match check_operator x with
      | Some x -> "(" ^ x ^ ")"
      | None -> failwith ("Invalid name " ^ x)

  let to_json s = `String s

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

let lib_sep = '/'

let mod_sep = '.'

module Module = struct

  module Name = UName

  type t =
    { library: Library.t option;
      parent: t option;
      name: Name.t; }

  let create lib name = { library = Some lib; parent = None; name }

  let create_submodule par name = { library = None; parent = Some par; name }

  let parent md = md.parent

  let name md = md.name

  let rec library md =
    match md.library, md.parent with
    | Some lib, None -> lib
    | None, Some par -> library par
    | _, _ -> assert false

  let package md = OpamLibrary.package (library md)

  let rec to_string md =
    match md.library, md.parent with
    | Some lib, None ->
        Printf.sprintf "%s%c%s"
          (OpamLibrary.to_string lib) lib_sep
          (Name.to_string md.name)
    | None, Some par ->
        Printf.sprintf "%s%c%s"
          (to_string par) mod_sep
          (Name.to_string md.name)
    | _ -> assert false

  let of_string s = failwith "Not implemented"

  let rec compare md1 md2 =
    let c =
      match md1.library, md1.parent, md2.library, md2.parent with
      | Some lib1, None, Some lib2, None -> OpamLibrary.compare lib1 lib2
      | None, Some par1, None, Some par2 -> compare par1 par2
      | Some _, None, None, Some par2 ->
          let c = compare md1 par2 in
          if c = 0 then -1 else c
      | None, Some par1, Some _, None ->
          let c = compare par1 md2 in
          if c = 0 then 1 else c
      | _, _, _, _ -> assert false
    in
    if c = 0 then Name.compare md1.name md2.name
    else c

  let rec to_json md =
    match md.library, md.parent with
    | Some lib, None ->
        `O [ ("library", OpamLibrary.to_json lib);
             ("name", `String (Name.to_string md.name)); ]
    | None, Some par ->
        `O [ ("parent", to_json par);
             ("name", `String (Name.to_string md.name)); ]
    | _ -> assert false

  module O = struct
    type tmp = t
    type t = tmp
    let compare = compare
    let to_string = to_string
    let to_json = to_json
  end

  module Set = OpamMisc.Set.Make(O)

  module Map = OpamMisc.Map.Make(O)

end

module ModuleType = struct

  module Name = UName

  type t =
    { parent: Module.t;
      name: Name.t; }

  let create parent name = { parent; name }

  let parent mty = mty.parent

  let name mty = mty.name

  let library mty = Module.library mty.parent

  let package md = OpamLibrary.package (library md)

  let to_string mty =
    Printf.sprintf "%s%c%s"
      (Module.to_string mty.parent) mod_sep
      (Name.to_string mty.name)

  let of_string s = failwith "Not implemented"

  let compare mty1 mty2 =
    match Module.compare mty1.parent mty2.parent with
    | 0 -> Name.compare mty1.name mty2.name
    | i -> i

  let to_json mty =
    `O [ ("parent", Module.to_json mty.parent);
         ("name", `String (Name.to_string mty.name)); ]

  module O = struct
    type tmp = t
    type t = tmp
    let to_string = to_string
    let compare = compare
    let to_json = to_json
  end

  module Set = OpamMisc.Set.Make(O)

  module Map = OpamMisc.Map.Make(O)

end


module Type = struct

  module Name = TName

  type t =
    { parent: Module.t;
      name: Name.t; }

  let create parent name = { parent; name }

  let parent mty = mty.parent

  let name mty = mty.name

end

module Value = struct

  module Name = ValName

  type t =
    { parent: Module.t;
      name: Name.t; }

  let create parent name = { parent; name }

  let parent mty = mty.parent

  let name mty = mty.name

end

module Constructor = struct

  module Name = UName

end

module Field = struct

  module Name = LName

end

module Exn = struct

  module Name = UName

end

type resolver = Module.Name.t -> Module.t option

let rec find_module res = function
  | Path.Pident id ->
      if Ident.persistent id then res (Ident.name id)
      else None
  | Path.Pdot(p, s, _) -> begin
      match find_module res p with
      | None -> None
      | Some md -> Some (Module.create_submodule md s)
    end
  | Path.Papply _ -> None

let find_module_type res = function
  | Path.Pident id -> None
  | Path.Pdot(p, s, _) -> begin
      match find_module res p with
      | None -> None
      | Some md -> Some (ModuleType.create md s)
    end
  | Path.Papply _ -> None

let find_type res = function
  | Path.Pident id -> None
  | Path.Pdot(p, s, _) -> begin
      match find_module res p with
      | None -> None
      | Some md -> Some (Type.create md s)
    end
  | Path.Papply _ -> None

let find_value res = function
  | Path.Pident id -> None
  | Path.Pdot(p, s, _) -> begin
      match find_module res p with
      | None -> None
      | Some md -> Some (Value.create md s)
    end
  | Path.Papply _ -> None

let rec lookup_module res = function
  | Longident.Lident s -> res s
  | Longident.Ldot(p, s) ->
      if not (check_uident s) then None
      else begin
        match lookup_module res p with
        | None -> None
        | Some md -> Some (Module.create_submodule md s)
      end
  | Longident.Lapply _ -> None

let lookup_module_type res = function
  | Longident.Lident s -> None
  | Longident.Ldot(p, s) ->
      if not (check_uident s) then None
      else begin
        match lookup_module res p with
        | None -> None
        | Some md -> Some (ModuleType.create md s)
      end
  | Longident.Lapply _ -> None

let lookup_type res = function
  | Longident.Lident s -> None
  | Longident.Ldot(p, s) ->
      if not (check_lident s) then None
      else begin
        match lookup_module res p with
        | None -> None
        | Some md -> Some (Type.create md s)
      end
  | Longident.Lapply _ -> None

let lookup_value res = function
  | Longident.Lident s -> None
  | Longident.Ldot(p, s) ->
      if not (check_lident s) then None
      else begin
        match lookup_module res p with
        | None -> None
        | Some md -> Some (Value.create md s)
      end
  | Longident.Lapply _ -> None

let lookup_module res s = lookup_module res (Longident.parse s)
let lookup_module_type res s = lookup_module_type res (Longident.parse s)
let lookup_type res s = lookup_type res (Longident.parse s)
let lookup_value res s = lookup_value res (Longident.parse s)
