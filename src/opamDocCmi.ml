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

open OpamDocPath
open Documentation
open Asttypes
open Parsetree
open Types
open OpamDocTypes

exception Not_implemented

let map_opt f = function
  | None -> None
  | Some x -> Some (f x)

let rec read_style : Documentation.style_kind -> style = function
  | SK_bold -> Bold
  | SK_italic -> Italic
  | SK_emphasize -> Emphasize
  | SK_center -> Center
  | SK_left -> Left
  | SK_right -> Right
  | SK_superscript -> Superscript
  | SK_subscript -> Subscript
  | SK_custom _ -> raise Not_implemented

and read_text_element res : Documentation.text_element -> text = function
  | Raw s -> [Raw s]
  | Code s -> [Code s]
  | Style(sk, txt) -> [Style(read_style sk, read_text res txt)]
  | List l -> [List(List.map (read_text res) l)]
  | Enum l -> [Enum(List.map (read_text res) l)]
  | Newline -> [Newline]
  | Title(_, _, txt) -> [Title (read_text res txt)]
  | Ref(RK_module, s, txt) -> begin
      match lookup_module res s, txt with
      | None, None -> [Raw s]
      | None, Some txt -> read_text res txt
      | Some p, None -> [Ref(Module p, None)]
      | Some p, Some txt -> [Ref(Module p, Some (read_text res txt))]
    end
  | Ref(RK_module_type, s, txt) -> begin
      match lookup_module_type res s, txt with
      | None, None -> [Raw s]
      | None, Some txt -> read_text res txt
      | Some p, None -> [Ref(ModuleType p, None)]
      | Some p, Some txt -> [Ref(ModuleType p, Some (read_text res txt))]
    end
  | Ref(RK_type, s, txt) -> begin
      match lookup_type res s, txt with
      | None, None -> [Raw s]
      | None, Some txt -> read_text res txt
      | Some p, None -> [Ref(Type p, None)]
      | Some p, Some txt -> [Ref(Type p, Some (read_text res txt))]
    end
  | Ref(RK_value, s, txt) -> begin
      match lookup_value res s, txt with
      | None, None -> [Raw s]
      | None, Some txt -> read_text res txt
      | Some p, None -> [Ref(Val p, None)]
      | Some p, Some txt -> [Ref(Val p, Some (read_text res txt))]
    end
  | Ref(RK_element, s, txt) -> begin
      match lookup_value res s, txt with
      | None, None -> [Raw s]
      | None, Some txt -> read_text res txt
      | Some p, None -> [Ref(Val p, None)]
      | Some p, Some txt -> [Ref(Val p, Some (read_text res txt))]
    end
  | Ref(_, _, Some txt) -> read_text res txt
  | Ref(_, s, None) -> [Raw s]
  | _ -> raise Not_implemented

and read_text res txt =
  List.concat (List.map (read_text_element res) txt)

let read_documentation res : Documentation.t -> text = function
  | Cinfo(txt, _) -> begin
      try
        read_text res txt
      with Not_implemented -> []
    end
  | Cstop -> assert false

let rec read_attributes res : Parsetree.attributes -> doc = function
  | ({txt = "doc"}, PDoc(d, _)) :: rest ->
      let rec loop = function
        | ({txt = "doc"}, PDoc(d, _)) :: rest ->
            let d = read_documentation res d in
            let rest = loop rest in
              Newline :: d @ rest
        | _ :: rest -> loop rest
        | [] -> []
      in
      let d = read_documentation res d in
      let rest = loop rest in
        { info = d @ rest }
  | _ :: rest -> read_attributes res rest
  | [] -> { info = [] }

let read_label lbl =
  let len = String.length lbl in
    if len = 0 then None
    else if lbl.[0] = '?' then
      Some (Default (String.sub lbl 1 (len - 1)))
    else Some (Label lbl)

let rec read_type_expr res (typ : Types.type_expr) : type_expr =
  match typ.desc with
  | Tvar v -> Var v
  | Tarrow(lbl, arg, ret, _) ->
      Arrow(read_label lbl, read_type_expr res arg, read_type_expr res ret)
  | Ttuple typs -> Tuple (List.map (read_type_expr res) typs)
  | Tconstr(p, typs, _) ->
      let p =
        match find_type res p with
        | None -> Unknown (Path.name p)
        | Some p -> Known p
      in
      let typs = List.map (read_type_expr res) typs in
        Constr(p, typs)
  | Tlink typ -> read_type_expr res typ
  | Tsubst _ -> assert false
  | _ -> raise Not_implemented

let read_value_description res id (v : Types.value_description) =
  { name = Value.Name.of_string (Ident.name id);
    doc = read_attributes res v.val_attributes;
    type_ = read_type_expr res v.val_type; }

let rec read_type_param (typ : Types.type_expr) =
  match typ.desc with
  | Tvar v -> v
  | Tlink typ -> read_type_param typ
  | Tsubst _ -> assert false
  | _ -> raise Not_implemented

let read_constructor_declaration res (cd : Types.constructor_declaration) =
  { name = Constructor.Name.of_string (Ident.name cd.cd_id);
    doc = read_attributes res cd.cd_attributes;
    args = List.map (read_type_expr res) cd.cd_args;
    ret = map_opt (read_type_expr res) cd.cd_res; }

let read_label_declaration res (ld : Types.label_declaration) : field =
  { name = Field.Name.of_string (Ident.name ld.ld_id);
    doc = read_attributes res ld.ld_attributes;
    type_ = read_type_expr res ld.ld_type; }

let read_type_kind res : Types.type_kind -> type_decl option = function
  | Type_abstract -> None
  | Type_variant cds ->
      Some (Variant (List.map (read_constructor_declaration res) cds))
  | Type_record(lds, _) ->
      Some (Record (List.map (read_label_declaration res) lds))
  | Type_open -> raise Not_implemented

let read_type_declaration res id (decl : Types.type_declaration) =
  { name = Type.Name.of_string (Ident.name id);
    doc = read_attributes res decl.type_attributes;
    param = List.map read_type_param decl.type_params;
    manifest = map_opt (read_type_expr res) decl.type_manifest;
    decl = read_type_kind res decl.type_kind; }

let rec read_module_declaration res path api id (md : Types.module_declaration) =
  let name = Module.Name.of_string (Ident.name id) in
  let path = Module.create_submodule path name in
  let doc = read_attributes res md.md_attributes in
  let (desc : nested_module_desc), api =
    match md.md_type with
    | Mty_ident p ->
        let p : module_type_path =
          match find_module_type res p with
          | None -> Unknown (Path.name p)
          | Some p -> Known p
        in
        let modl : module_ =
          { path = path; doc; alias = None;
            type_path = Some p; type_ = None }
        in
        let api =
          { api with modules = Module.Map.add path modl api.modules }
        in
          Type (Path p), api
    | Mty_signature sg ->
        let sg, api = read_signature res path api [] sg in
        let modl : module_ =
          { path = path; doc; alias = None;
            type_path = None; type_ = Some sg }
        in
        let api =
          { api with modules = Module.Map.add path modl api.modules }
        in
          Type Signature, api
    | Mty_functor _ -> raise Not_implemented
    | Mty_alias p ->
        let p : module_path =
          match find_module res p with
          | None -> Unknown (Path.name p)
          | Some p -> Known p
        in
        let modl : module_ =
          { path = path; doc; alias = Some p;
            type_path = None; type_ = None }
        in
        let api =
          { api with modules = Module.Map.add path modl api.modules }
        in
          Alias p, api
  in
  let md : nested_module = { name; doc; desc } in
    md, api

and read_modtype_declaration res path' api id (mtd : Types.modtype_declaration) =
  let name = ModuleType.Name.of_string (Ident.name id) in
  let path = ModuleType.create path' name in
  let doc = read_attributes res mtd.mtd_attributes in
  let desc, api =
    match mtd.mtd_type with
    | None ->
        let mty =
          { path = path; doc; alias = None; expr = None; }
        in
        let api =
          { api with
              module_types = ModuleType.Map.add path mty api.module_types }
        in
          Abstract, api
    | Some (Mty_ident p) ->
        let p : module_type_path =
          match find_module_type res p with
          | None -> Unknown (Path.name p)
          | Some p -> Known p
        in
        let mty =
          { path = path; doc; alias = Some p; expr = None; }
        in
        let api =
          { api with
              module_types = ModuleType.Map.add path mty api.module_types }
        in
          Manifest (Path p), api
    | Some (Mty_signature sg) ->
        (* TODO use correct path when paths can include parent module types *)
        let sg, api = read_signature res path' api [] sg in
        let mty =
          { path = path; doc; alias = None; expr = Some sg; }
        in
        let api =
          { api with
              module_types = ModuleType.Map.add path mty api.module_types }
        in
          Manifest Signature, api
    | Some (Mty_functor _) -> raise Not_implemented
    | Some (Mty_alias _) -> assert false
  in
  let mtd = { name; doc; desc = desc; } in
    mtd, api

and read_signature res path api (acc : signature) = function
  | Sig_value(id, v) :: rest -> begin
      try
        let v = read_value_description res id v in
          read_signature res path api ((Val v) :: acc) rest
      with Not_implemented -> read_signature res path api acc rest
    end
  | Sig_type(id, decl, Trec_first) :: rest -> begin
      try
        let decl = read_type_declaration res id decl in
        let rec loop acc' = function
          | Sig_type(id, decl, Trec_next) :: rest ->
              let decl = read_type_declaration res id decl in
                loop (decl :: acc') rest
          | rest ->
              read_signature res path api (Types(List.rev acc') :: acc) rest
        in
          loop [decl] rest
      with Not_implemented -> read_signature res path api acc rest
    end
  | Sig_type(id, decl, _) :: rest -> begin
      try
        let decl = read_type_declaration res id decl in
            read_signature res path api ((Types [decl]) ::  acc) rest
      with Not_implemented -> read_signature res path api acc rest
    end
  | Sig_module(id, md, Trec_first) :: rest -> begin
      try
        let md, api = read_module_declaration res path api id md in
        let rec loop api acc' = function
          | Sig_module(id, md, Trec_next) :: rest ->
              let md, api = read_module_declaration res path api id md in
                loop api (md :: acc') rest
          | rest ->
              read_signature res path api (Modules(List.rev acc') :: acc) rest
        in
          loop api [md] rest
      with Not_implemented -> read_signature res path api acc rest
    end
  | Sig_module(id, md, _) :: rest -> begin
      try
        let md, api = read_module_declaration res path api id md in
          read_signature res path api ((Modules [md]) :: acc) rest
      with Not_implemented -> read_signature res path api acc rest
    end
  | Sig_modtype(id, mtd) :: rest -> begin
      try
        let mtd, api = read_modtype_declaration res path api id mtd in
          read_signature res path api ((ModuleType mtd) :: acc) rest
      with Not_implemented -> read_signature res path api acc rest
    end
  | _ :: rest -> read_signature res path api acc rest
  | [] -> Signature (List.rev acc), api

let read_interface res path intf =
  let api =
    { modules = Module.Map.empty;
      module_types = ModuleType.Map.empty }
  in
  let sg, api = read_signature res path api [] intf in
  let modl =
    { path = path; doc = {info = []}; alias = None;
      type_path = None; type_ = Some sg }
  in
    { api with modules = Module.Map.add path modl api.modules }
