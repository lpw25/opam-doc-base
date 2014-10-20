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

module Name = OpamDocName

let map_opt f = function
  | None -> None
  | Some x -> Some (f x)

let iter_opt f = function
  | None -> ()
  | Some x -> f x

let rec read_style : Documentation.style_kind -> style = function
  | SK_bold -> Bold
  | SK_italic -> Italic
  | SK_emphasize -> Emphasize
  | SK_center -> Center
  | SK_left -> Left
  | SK_right -> Right
  | SK_superscript -> Superscript
  | SK_subscript -> Subscript
  | SK_custom s -> Custom s

and read_text_element res : Documentation.text_element -> text = function
  | Raw s -> [Raw s]
  | Code s -> [Code s]
  | PreCode s -> [PreCode s]
  | Verbatim s -> [Verbatim s]
  | Style(sk, txt) -> [Style(read_style sk, read_text res txt)]
  | List l -> [List(List.map (read_text res) l)]
  | Enum l -> [Enum(List.map (read_text res) l)]
  | Newline -> [Newline]
  | Title(i, l, txt) -> [Title (i, l, read_text res txt)]
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
  | Ref(RK_link, uri, txt) -> begin
      match txt with
      | None   -> [Ref (Link uri, None)]
      | Some t -> [Ref (Link uri, Some (read_text res t))]
    end
  | Ref(_, s, _)  -> [TEXT_todo ("ref:"^s)]
  | Special_ref _ -> [TEXT_todo "special-ref"]
  | Target (target, code) -> [Target (target, code)]

and read_text res txt =
  List.concat (List.map (read_text_element res) txt)

let read_tag res: Documentation.tag -> tag = function
  | Author s -> Author s
  | Version v -> Version v
  | See (r, t) -> See (r, read_text res t)
  | Since s -> Since s
  | Before (s, t) -> Before (s, read_text res t)
  | Deprecated t -> Deprecated (read_text res t)
  | Param (s, t) -> Param (s, read_text res t)
  | Raised_exception (s, t) -> Raise (s, read_text res t)
  | Return_value t -> Return (read_text res t)
  | Custom (s, t) -> Tag (s, read_text res t)

let read_documentation res : Documentation.t -> text * tag list = function
  | Cinfo(txt, tags) -> read_text res txt, List.map (read_tag res) tags
  | Cstop ->
      (* FIXME: need a better story for handling ocamldoc stop comments *)
      [], []

let rec read_attributes res : Parsetree.attributes -> doc = function
  | ({txt = "doc"}, PDoc(d, _)) :: rest ->
      let rec loop = function
        | ({txt = "doc"}, PDoc(d, _)) :: rest ->
            let d, t = read_documentation res d in
            let rest, tags = loop rest in
            (Newline :: d @ rest), t @ tags
        | _ :: rest -> loop rest
        | [] -> [], []
      in
      let d, t = read_documentation res d in
      let rest, tags = loop rest in
      { info = d @ rest;
        tags = t @ tags; }
  | _ :: rest -> read_attributes res rest
  | [] -> { info = []; tags = []; }

let read_label lbl =
  let len = String.length lbl in
  if len = 0 then None
  else if lbl.[0] = '?' then
    Some (Default (String.sub lbl 1 (len - 1)))
  else Some (Label lbl)

(* Handle type variable names *)

let used_names = ref []
let name_counter = ref 0
let reserved_names = ref []

let reset_names () = used_names := []; name_counter := 0; reserved_names := []

let reserve_name = function
  | Some name ->
      if not (List.mem name !reserved_names) then
        reserved_names := name :: !reserved_names
  | None -> ()

let rec next_name () =
  let name =
    if !name_counter < 26
    then String.make 1 (Char.chr(97 + !name_counter))
    else String.make 1 (Char.chr(97 + !name_counter mod 26)) ^
           string_of_int(!name_counter / 26)
  in
    incr name_counter;
    if List.mem name !reserved_names then next_name ()
    else name

let rec fresh_name base =
  let current_name = ref base in
  let i = ref 0 in
  while List.exists (fun (_, name') -> !current_name = name') !used_names do
    current_name := base ^ (string_of_int !i);
    i := !i + 1;
  done;
  !current_name

let name_of_type (ty : Types.type_expr) =
  try
    List.assq ty !used_names
  with Not_found ->
    let base =
      match ty.desc with
      | Tvar (Some name) | Tunivar (Some name) -> name
      | _ -> next_name ()
    in
    let name = fresh_name base in
    if name <> "_" then used_names := (ty, name) :: !used_names;
    name

let remove_names tyl =
  used_names := List.filter (fun (ty,_) -> not (List.memq ty tyl)) !used_names

(* Handle recursive types and shared row variables *)

let aliased = ref []
let used_aliases = ref []

let reset_aliased () = aliased := []; used_aliases := []

let is_aliased px = List.memq px !aliased

let add_alias ty =
  let px = Btype.proxy ty in
  if not (List.memq px !aliased) then begin
    aliased := px :: !aliased;
    match px.desc with
    | Tvar name | Tunivar name -> reserve_name name
    | _ -> ()
  end

let aliasable (ty : Types.type_expr) =
  match ty.desc with
  | Tvar _ | Tunivar _ | Tpoly _ -> false
  | _ -> true

let used_alias (px : Types.type_expr) = List.memq px !used_aliases

let use_alias (px : Types.type_expr) = used_aliases := px :: !used_aliases

let visited_rows = ref []

let reset_visited_rows () = visited_rows := []

let is_row_visited px = List.memq px !visited_rows

let visit_row row px =
  if not (Btype.static_row row) then
    visited_rows := px :: !visited_rows

let visit_object ty px =
  if Ctype.opened_object ty then
    visited_rows := px :: !visited_rows

let namable_row row =
  row.row_name <> None &&
  List.for_all
    (fun (_, f) ->
       match Btype.row_field_repr f with
       | Reither(c, l, _, _) ->
           row.row_closed && if c then l = [] else List.length l = 1
       | _ -> true)
    row.row_fields

let mark_type ty =
  let rec loop visited ty =
    let ty = Btype.repr ty in
    let px = Btype.proxy ty in
    if List.memq px visited && aliasable ty then add_alias px else
      let visited = px :: visited in
      match ty.desc with
      | Tvar name -> reserve_name name
      | Tarrow(_, ty1, ty2, _) ->
          loop visited ty1;
          loop visited ty2
      | Ttuple tyl -> List.iter (loop visited) tyl
      | Tconstr(p, tyl, _) ->
          List.iter (loop visited) tyl
      | Tvariant row ->
          if is_row_visited px then add_alias px else
           begin
            let row = Btype.row_repr row in
            visit_row row px;
            match row.row_name with
            | Some(p, tyl) when namable_row row ->
                List.iter (loop visited) tyl
            | _ ->
                Btype.iter_row (loop visited) row
           end
      | Tobject (fi, nm) ->
          if is_row_visited px then add_alias px else
           begin
            visit_object ty px;
            match !nm with
            | None ->
                let fields, _ = Ctype.flatten_fields fi in
                List.iter
                  (fun (_, kind, ty) ->
                    if Btype.field_kind_repr kind = Fpresent then
                      loop visited ty)
                  fields
            | Some (_, l) ->
                List.iter (loop visited) (List.tl l)
          end
      | Tfield(_, kind, ty1, ty2) when Btype.field_kind_repr kind = Fpresent ->
          loop visited ty1;
          loop visited ty2
      | Tfield(_, _, _, ty2) ->
          loop visited ty2
      | Tnil -> ()
      | Tpoly (ty, tyl) ->
          List.iter (fun t -> add_alias t) tyl;
          loop visited ty
      | Tunivar name -> reserve_name name
      | Tsubst ty -> loop visited ty
      | Tlink _ -> assert false
      | _ -> ()
  in
  loop [] ty

let mark_type_parameter ty =
  add_alias ty;
  mark_type ty

let mark_type_kind = function
  | Type_abstract -> ()
  | Type_variant cds ->
      List.iter
        (fun cd ->
           List.iter mark_type cd.cd_args;
           iter_opt mark_type cd.cd_res)
        cds
  | Type_record(lds, _) ->
      List.iter (fun ld -> mark_type ld.ld_type) lds
  | Type_open -> ()

let mark_extension_constructor (_, ext) =
  List.iter mark_type ext.ext_args;
  iter_opt mark_type ext.ext_ret_type

let rec read_type_expr res (typ : Types.type_expr) : type_expr =
  let typ = Btype.repr typ in
  let px = Btype.proxy typ in
  if used_alias px then Var (name_of_type typ)
  else begin
    let alias =
      if not (is_aliased px && aliasable typ) then None
      else begin
        use_alias px;
        Some (name_of_type typ)
      end
    in
    let typ =
      match typ.desc with
      | Tvar _ -> Var (name_of_type typ)
      | Tarrow(lbl, arg, ret, _) ->
          let label = read_label lbl in
          let typ = match label with
            | None
            | Some (Label _)   -> read_type_expr res arg
            | Some (Default _) ->
                let is_option t =
                  Name.Type.to_string (Type.name t) = "option"
                in
                match read_type_expr res arg with
                | Constr(Known t, [x]) when is_option t -> x
                | Constr(Unknown "option", [x]) -> x
                | _ -> assert false (* Optional labels are *always* optional *)
          in
          Arrow(label, typ, read_type_expr res ret)
      | Ttuple typs -> Tuple (List.map (read_type_expr res) typs)
      | Tconstr(p, typs, _) ->
          let p =
            match find_type res p with
            | None -> Unknown (Path.name p)
            | Some p -> Known p
          in
          let typs = List.map (read_type_expr res) typs in
          Constr(p, typs)
      | Tvariant row -> read_row res px row
      | Tobject (fi, nm) -> read_object res fi !nm
      | Tnil | Tfield _ -> read_object res typ None
      | Tpoly (typ, []) -> read_type_expr res typ
      | Tpoly (typ, tyl) ->
          let tyl = List.map Btype.repr tyl in
          let vars = List.map name_of_type tyl in
          let typ = Poly(vars, read_type_expr res typ) in
            remove_names tyl;
            typ
      | Tunivar _ -> Var (name_of_type typ)
      | Tsubst typ -> read_type_expr res typ
      | Tlink _ -> assert false
      | _ -> TYPE_EXPR_todo (name_of_type typ)
    in
      match alias with
      | None -> typ
      | Some name -> Alias(typ, name)
  end

and read_row res px row =
  let row = Btype.row_repr row in
  let fields =
    if row.row_closed then
      List.filter (fun (_, f) -> Btype.row_field_repr f <> Rabsent)
        row.row_fields
    else row.row_fields in
  let sorted_fields = List.sort (fun (p,_) (q,_) -> compare p q) fields in
  let present =
    List.filter
      (fun (_, f) ->
         match Btype.row_field_repr f with
         | Rpresent _ -> true
         | _ -> false)
      sorted_fields in
  let all_present = List.length present = List.length sorted_fields in
  match row.row_name with
  | Some(p, typs) when namable_row row ->
      let p =
        match find_type res p with
        | None -> Unknown (Path.name p)
        | Some p -> Known p
      in
      let args = List.map (read_type_expr res) typs in
      if row.row_closed && all_present then
        Constr (p, args)
      else
        let kind =
          if all_present then Open else Closed (List.map fst present)
        in
        Variant {kind; elements = [Type(p, args)]}
  | _ ->
      let elements =
        List.map
          (fun (l, f) ->
            match Btype.row_field_repr f with
              | Rpresent None ->
                  Constructor(l, true, [])
              | Rpresent (Some typ) ->
                  Constructor(l, false, [read_type_expr res typ])
              | Reither(c, typs, _, _) ->
                  let typs =
                    List.map (read_type_expr res) typs
                  in
                    Constructor(l, c, typs)
              | Rabsent -> assert false)
          sorted_fields
      in
      let kind =
        if all_present then
          if row.row_closed then Fixed
          else Open
        else Closed (List.map fst present)
      in
      Variant {kind; elements}

and read_object res fi nm =
  match nm with
  | None ->
      let (fields, rest) = Ctype.flatten_fields fi in
      let present_fields =
        List.fold_right
          (fun (n, k, t) l ->
             match Btype.field_kind_repr k with
             | Fpresent -> (n, t) :: l
             | _ -> l)
          fields []
      in
      let sorted_fields =
        List.sort (fun (n, _) (n', _) -> compare n n') present_fields
      in
      let methods =
        List.map
          (fun (name, typ) -> {name; type_ = read_type_expr res typ})
          sorted_fields
      in
      let open_ =
        match rest.desc with
        | Tvar _ | Tunivar _ -> true
        | Tconstr _ -> true
        | Tnil -> false
        | _ -> assert false
      in
      Object {methods; open_}
  | Some (p, _ :: tyl) ->
      let p =
        match find_type res p with
        | None -> Unknown (Path.name p)
        | Some p -> Known p
      in
      let args = List.map (read_type_expr res) tyl in
      Class (p, args)
  | _ -> assert false


let read_type_scheme res (typ : Types.type_expr) : type_expr =
  reset_names ();
  reset_aliased ();
  reset_visited_rows ();
  mark_type typ;
  read_type_expr res typ

let read_value_description res id (v : Types.value_description): val_ =
  { name = Name.Value.of_string (Ident.name id);
    doc = read_attributes res v.val_attributes;
    type_ = read_type_scheme res v.val_type; }

let rec read_type_param res (typ : Types.type_expr) =
  match read_type_expr res typ with
  | Var v -> v
  | _ -> "todo"

let read_constructor_declaration res (cd : Types.constructor_declaration)
  : constructor =
  { name = Name.Constructor.of_string (Ident.name cd.cd_id);
    doc = read_attributes res cd.cd_attributes;
    args = List.map (read_type_expr res) cd.cd_args;
    ret = map_opt (read_type_expr res) cd.cd_res; }

let read_label_declaration res (ld : Types.label_declaration) : field =
  { name = Name.Field.of_string (Ident.name ld.ld_id);
    doc = read_attributes res ld.ld_attributes;
    type_ = read_type_expr res ld.ld_type; }

let read_type_kind res : Types.type_kind -> type_decl option = function
  | Type_abstract -> None
  | Type_variant cds ->
      Some (Variant (List.map (read_constructor_declaration res) cds))
  | Type_record(lds, _) ->
      Some (Record (List.map (read_label_declaration res) lds))
  | Type_open ->  Some Extensible

let read_type_declaration res id (decl : Types.type_declaration) =
  reset_names ();
  reset_aliased ();
  reset_visited_rows ();
  List.iter mark_type_parameter decl.type_params;
  iter_opt mark_type decl.type_manifest;
  mark_type_kind decl.type_kind;
  { name = Name.Type.of_string (Ident.name id);
    doc = read_attributes res decl.type_attributes;
    param = List.map (read_type_param res) decl.type_params;
    private_ = (decl.type_private = Private);
    manifest = map_opt (read_type_expr res) decl.type_manifest;
    decl = read_type_kind res decl.type_kind; }

let read_extension_constructor res id (e: Types.extension_constructor): constructor =
  { name = Name.Constructor.of_string (Ident.name id);
    doc = read_attributes res e.ext_attributes;
    args = List.map (read_type_expr res) e.ext_args;
    ret = map_opt (read_type_expr res) e.ext_ret_type; }

let read_type_extension res ((id, ext) as first)  rest =
  reset_names ();
  reset_aliased ();
  reset_visited_rows ();
  List.iter mark_type_parameter ext.ext_type_params;
  mark_extension_constructor first;
  List.iter mark_extension_constructor rest;
  let type_path =
    match find_type res ext.ext_type_path with
    | None -> Unknown (Path.name ext.ext_type_path)
    | Some p -> Known p
  in
  let type_params = List.map (read_type_param res) ext.ext_type_params in
  let doc = read_attributes res ext.ext_attributes in
  let private_ = (ext.ext_private = Private) in
  let constructors =
    List.map
      (fun (id, ext) -> read_extension_constructor res id ext)
      (first :: rest)
  in
    { type_path; type_params;
      doc; private_;
      constructors; }

let rec read_module_declaration res parent id (md : Types.module_declaration) =
  let name = Name.Module.of_string (Ident.name id) in
  let path = Module.create parent name in
  let doc = read_attributes res md.md_attributes in
    match md.md_type with
    | Mty_ident p ->
        let p : module_type_path =
          match find_module_type res p with
          | None -> Unknown (Path.name p)
          | Some p -> Known p
        in
          { path = path; doc; alias = None;
            type_path = Some p; type_ = None }
    | Mty_signature sg ->
        let parent : parent = Module path in
        let sg = read_signature res parent [] sg in
          { path = path; doc; alias = None;
            type_path = None; type_ = Some sg }
    | Mty_functor _ ->
        let sg = MODULE_TYPE_EXPR_todo ("functor:"^Module.to_string path) in
        { path = path; doc; alias = None;
          type_path = None; type_ = Some sg }
    | Mty_alias p ->
        let p : module_path =
          match find_module res p with
          | None -> Unknown (Path.name p)
          | Some p -> Known p
        in
          { path = path; doc; alias = Some p;
            type_path = None; type_ = None }

and read_modtype_declaration res parent id
                             (mtd : Types.modtype_declaration) =
  let name = Name.ModuleType.of_string (Ident.name id) in
  let path = ModuleType.create parent name in
  let doc = read_attributes res mtd.mtd_attributes in
    match mtd.mtd_type with
    | None ->
        { path = path; doc; alias = None; expr = None; }
    | Some (Mty_ident p) ->
        let p : module_type_path =
          match find_module_type res p with
          | None -> Unknown (Path.name p)
          | Some p -> Known p
        in
          { path = path; doc; alias = Some p; expr = None; }
    | Some (Mty_signature sg) ->
        let sg = read_signature res (ModType path) [] sg in
          { path = path; doc; alias = None; expr = Some sg; }
    | Some (Mty_functor _) ->
        let sg = MODULE_TYPE_EXPR_todo ("functor:"^ModuleType.to_string path) in
          { path = path; doc; alias = None; expr = Some sg; }
    | Some (Mty_alias _) -> assert false

and read_signature res parent (acc : signature) = function
  | Sig_value(id, v) :: rest ->
      let v = read_value_description res id v in
      read_signature res parent ((Val v) :: acc) rest
  | Sig_type(id, decl, Trec_first) :: rest ->
      let decl = read_type_declaration res id decl in
      let rec loop acc' = function
        | Sig_type(id, decl, Trec_next) :: rest ->
            let decl = read_type_declaration res id decl in
            loop (decl :: acc') rest
        | rest ->
            read_signature res parent (Types(List.rev acc') :: acc) rest
      in
      loop [decl] rest
  | Sig_type(id, decl, _) :: rest ->
      let decl = read_type_declaration res id decl in
      read_signature res parent ((Types [decl]) ::  acc) rest
  | Sig_typext (id, ext, Text_first) :: rest ->
      let rec loop acc' = function
        | Sig_typext(id, ext, Text_next) :: rest -> loop ((id, ext) :: acc') rest
        | rest ->
            let ext = read_type_extension res (id, ext) (List.rev acc') in
              read_signature res parent ((TypExt ext) :: acc) rest
      in
        loop [] rest
  | Sig_typext (id, ext, Text_next) :: rest ->
      let ext = read_type_extension res (id, ext) [] in
        read_signature res parent ((TypExt ext) :: acc) rest
  | Sig_typext (id, ext, Text_exception) :: rest ->
      reset_names ();
      reset_aliased ();
      reset_visited_rows ();
      List.iter mark_type_parameter ext.ext_type_params;
      mark_extension_constructor (id, ext);
      let constr = read_extension_constructor res id ext in
        read_signature res parent ((Exn constr) :: acc) rest
  | Sig_module(id, md, Trec_first) :: rest ->
      let md = read_module_declaration res parent id md in
      let rec loop acc' = function
        | Sig_module(id, md, Trec_next) :: rest ->
            let md = read_module_declaration res parent id md in
            loop (md :: acc') rest
        | rest ->
            read_signature res parent (Modules(List.rev acc') :: acc) rest
      in
      loop [md] rest
  | Sig_module(id, md, _) :: rest ->
      let md = read_module_declaration res parent id md in
      read_signature res parent ((Modules [md]) :: acc) rest
  | Sig_modtype(id, mtd) :: rest ->
      let mtd = read_modtype_declaration res parent id mtd in
      read_signature res parent ((ModuleType mtd) :: acc) rest
  | x :: rest ->
      read_signature res parent
        (SIG_todo (parent_to_string parent) :: acc) rest
  | [] -> Signature (List.rev acc)

let read_interface res path intf =
  let sg = read_signature res (Module path) [] intf in
    { path = path; doc = {info = []; tags = []; }; alias = None;
      type_path = None; type_ = Some sg }
