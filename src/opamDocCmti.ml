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
open Typedtree
open OpamDocTypes

let read_comment res : Parsetree.attribute -> doc option = function
  | ({txt = "comment"}, PDoc(d, _)) ->
      let info = OpamDocCmi.read_documentation res d in
      Some {info}
  | _ -> None

let rec read_module_declaration res path api md =
  match md.md_type.mty_desc with
  | Tmty_signature sg ->
      let name = Module.Name.of_string (Ident.name md.md_id) in
      let path = Module.create_submodule path name in
      let doc = OpamDocCmi.read_attributes res md.md_attributes in
      let sg, api = read_signature res path api sg in
      let modl =
        { path = path; doc; alias = None;
          type_path = None; type_ = Some sg }
      in
      let api =
        { api with modules = Module.Map.add path modl api.modules }
      in
      let md : nested_module = { name; doc; desc = Type Signature } in
      md, api
  | _ ->
      let id = md.md_id in
      let md =
        { Types.md_type = md.md_type.mty_type;
          md_attributes = md.md_attributes;
          md_loc = md.md_loc; }
      in
      OpamDocCmi.read_module_declaration res path api id md

and read_modtype_declaration res path' api mtd =
  match mtd.mtd_type with
  | Some {mty_desc = Tmty_signature sg} ->
      let name = ModuleType.Name.of_string (Ident.name mtd.mtd_id) in
      let path = ModuleType.create path' name in
      let doc = OpamDocCmi.read_attributes res mtd.mtd_attributes in
      (* TODO use correct path when paths can include parent module types *)
      let sg, api = read_signature res path' api sg in
      let mty =
        { path = path; doc; alias = None; expr = Some sg; }
      in
      let api =
        { api with
          module_types = ModuleType.Map.add path mty api.module_types }
      in
      let mtd =
        { name;
          doc = OpamDocCmi.read_attributes res mtd.mtd_attributes;
          desc = Manifest Signature; }
      in
      mtd, api
  | _ ->
      let id = mtd.mtd_id in
      let mtd =
        { Types.mtd_type =
            (match mtd.mtd_type with
             | None -> None
             | Some mty -> Some mty.mty_type);
          mtd_attributes = mtd.mtd_attributes;
          mtd_loc = mtd.mtd_loc; }
      in
      OpamDocCmi.read_modtype_declaration res path' api id mtd

and read_signature_item res path api item : (signature_item * api) option =
  match item.sig_desc with
  | Tsig_value vd ->
      let v = OpamDocCmi.read_value_description res vd.val_id vd.val_val in
      Some (Val v, api)
  | Tsig_type decls ->
      let decls =
        List.map (fun decl ->
            OpamDocCmi.read_type_declaration res decl.typ_id decl.typ_type
          ) decls
      in
      Some (Types decls, api)
  | Tsig_exception e ->
      let e = OpamDocCmi.read_extension_constructor res e.ext_id e.ext_type in
      Some (Exn e, api)
  | Tsig_module md ->
      let md, api = read_module_declaration res path api md in
      Some (Modules [md], api)
  | Tsig_recmodule mds ->
      let mds, api =
        List.fold_left
          (fun (mds, api) md ->
             let md, api = read_module_declaration res path api md in
             (md :: mds), api)
          ([], api) mds
      in
      Some (Modules (List.rev mds), api)
  | Tsig_modtype mtd ->
      let mtd, api = read_modtype_declaration res path api mtd in
      Some (ModuleType mtd, api)
  | Tsig_attribute attr -> begin
      match read_comment res attr with
      | None -> None
      | Some doc -> Some (Comment doc, api)
    end
  | _ -> Some (SIG_todo (Module.to_string path), api)

and read_signature res path api sg =
  let items, api =
    List.fold_left
      (fun (items, api) item ->
         match read_signature_item res path api item with
         | None -> (items, api)
         | Some (item, api) -> (item :: items, api))
      ([], api) sg.sig_items
  in
  Signature (List.rev items), api

let read_interface_tree res path intf =
  let api =
    { modules = Module.Map.empty;
      module_types = ModuleType.Map.empty }
  in
  let sg, api = read_signature res path api intf in
  let doc, (sg : module_type_expr) =
    match sg with
    | Signature (Comment doc :: items) -> doc, Signature items
    | Signature items -> {info = []}, Signature items
  in
  let modl =
    { path = path; doc; alias = None;
      type_path = None; type_ = Some sg }
  in
  { api with modules = Module.Map.add path modl api.modules }
