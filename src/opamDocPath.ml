
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

module Name = OpamDocName

module Package = OpamPackage

module Library = OpamLibrary

type module_ =
  { parent: parent;
    name: Name.Module.t; }

and modtype =
  { parent: parent;
    name: Name.ModuleType.t; }

and parent =
  | Lib of Library.t
  | Module of module_
  | ModType of modtype

let lib_sep = '/'

let module_sep = '.'

let modtype_sep = ':'

let rec module_to_string (md : module_) =
  parent_to_string md.parent
  ^ Name.Module.to_string md.name

and modtype_to_string (mty : modtype) =
  parent_to_string mty.parent
  ^ Name.ModuleType.to_string mty.name

and parent_to_string = function
  | Lib lib ->
      Printf.sprintf "%s%c"
        (Library.to_string lib) lib_sep
  | Module md ->
      Printf.sprintf "%s%c"
        (module_to_string md) module_sep
  | ModType mty ->
      Printf.sprintf "%s%c"
        (modtype_to_string mty) modtype_sep

let rec parent_library = function
  | Lib l -> l
  | Module md -> parent_library md.parent
  | ModType mty -> parent_library mty.parent

let parent_list p =
  let rec loop p acc =
    match p with
    | Lib _ -> p :: acc
    | Module md -> loop md.parent (p :: acc)
    | ModType mty -> loop mty.parent (p :: acc)
  in
    loop p []

let compare_parent p1 p2 =
  let compare_name p1 p2 =
    match p1, p2 with
    | Lib lib1, Lib lib2 -> Library.compare lib1 lib2
    | Module md1, Module md2 -> Name.Module.compare md1.name md2.name
    | ModType mty1, ModType mty2 -> Name.ModuleType.compare mty1.name mty2.name
    | Module _, ModType _ -> -1
    | ModType _, Module _ -> 1
    | _, _ -> assert false
  in
  let rec compare_list l1 l2 =
    match l1, l2 with
    | [], [] -> 0
    | p1 :: rest1, p2 :: rest2 ->
        let c = compare_name p1 p2 in
        if c <> 0 then c
        else compare_list rest1 rest2
    | [], _ -> 1
    | _, [] -> -1
  in
    compare_list (parent_list p1) (parent_list p2)

let compare_module (md1 : module_) (md2 : module_) =
  let c = compare_parent md1.parent md2.parent in
  if c = 0 then Name.Module.compare md1.name md2.name
  else c

let compare_modtype (mty1 : modtype) (mty2 : modtype) =
  let c = compare_parent mty1.parent mty2.parent in
  if c = 0 then Name.ModuleType.compare mty1.name mty2.name
  else c

let rec module_to_json (md : module_) =
  `O [ ("parent", parent_to_json md.parent);
       ("name", `String (Name.Module.to_string md.name)); ]

and modtype_to_json (mty : modtype) =
  `O [ ("parent", parent_to_json mty.parent);
       ("name", `String (Name.ModuleType.to_string mty.name)); ]

and parent_to_json = function
  | Lib lib -> `O [("library", Library.to_json lib)]
  | Module md -> `O [("module", module_to_json md)]
  | ModType mty -> `O [("modtype", modtype_to_json mty)]


module Module = struct

  type t = module_

  let create parent name : t = { parent; name }

  let parent (md : t) = md.parent

  let name (md : t) = md.name

  let library (md : t) = parent_library md.parent

  let package (md : t) = OpamLibrary.package (library md)

  let to_string = module_to_string

  let of_string s = failwith "Not implemented"

  let compare = compare_module

  let to_json = module_to_json

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

  type t = modtype

  let create parent name : t = { parent; name }

  let parent (mty : t) = mty.parent

  let name (mty : t) = mty.name

  let library (mty : t) = parent_library mty.parent

  let package (mty : t) = OpamLibrary.package (library mty)

  let to_string = modtype_to_string

  let of_string s = failwith "Not implemented"

  let compare = compare_modtype

  let to_json = modtype_to_json

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

  type t =
    { parent: parent;
      name: Name.Type.t; }

  let create parent name : t = { parent; name }

  let parent ty = ty.parent

  let name ty = ty.name

end

module Value = struct

  type t =
    { parent: parent;
      name: Name.Value.t; }

  let create parent name : t = { parent; name }

  let parent v = v.parent

  let name v = v.name

end

type resolver = Name.Module.t -> Module.t option

let rec find_module res = function
  | Path.Pident id ->
      if Ident.persistent id then res (Name.Module.of_string (Ident.name id))
      else None
  | Path.Pdot(p, s, _) -> begin
      match find_module res p with
      | None -> None
      | Some md -> Some (Module.create (Module md) (Name.Module.of_string s))
    end
  | Path.Papply _ -> None

let find_module_type res = function
  | Path.Pident id -> None
  | Path.Pdot(p, s, _) -> begin
      match find_module res p with
      | None -> None
      | Some md ->
          Some (ModuleType.create (Module md) (Name.ModuleType.of_string s))
    end
  | Path.Papply _ -> None

let find_type res = function
  | Path.Pident id -> None
  | Path.Pdot(p, s, _) -> begin
      match find_module res p with
      | None -> None
      | Some md -> Some (Type.create (Module md) (Name.Type.of_string s))
    end
  | Path.Papply _ -> None

let find_value res = function
  | Path.Pident id -> None
  | Path.Pdot(p, s, _) -> begin
      match find_module res p with
      | None -> None
      | Some md -> Some (Value.create (Module md) (Name.Value.of_string s))
    end
  | Path.Papply _ -> None

let rec lookup_module res = function
  | Longident.Lident s -> res (Name.Module.of_string s)
  | Longident.Ldot(p, s) ->
      begin
        match lookup_module res p with
        | None -> None
        | Some md ->
            Some (Module.create (Module md) (Name.Module.of_string s))
      end
  | Longident.Lapply _ -> None

let lookup_module_type res = function
  | Longident.Lident s -> None
  | Longident.Ldot(p, s) ->
      begin
        match lookup_module res p with
        | None -> None
        | Some md ->
            Some (ModuleType.create (Module md) (Name.ModuleType.of_string s))
      end
  | Longident.Lapply _ -> None

let lookup_type res = function
  | Longident.Lident s -> None
  | Longident.Ldot(p, s) ->
      begin
        match lookup_module res p with
        | None -> None
        | Some md -> Some (Type.create (Module md) (Name.Type.of_string s))
      end
  | Longident.Lapply _ -> None

let lookup_value res = function
  | Longident.Lident s -> None
  | Longident.Ldot(p, s) ->
      begin
        match lookup_module res p with
        | None -> None
        | Some md -> Some (Value.create (Module md) (Name.Value.of_string s))
      end
  | Longident.Lapply _ -> None

let lookup_module res s = lookup_module res (Longident.parse s)
let lookup_module_type res s = lookup_module_type res (Longident.parse s)
let lookup_type res s = lookup_type res (Longident.parse s)
let lookup_value res s = lookup_value res (Longident.parse s)
