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

(** Type of documentation *)

open OpamDocPath

(** {3 Packages} *)

type package =
  { path: Package.t;
    libraries: Library.Name.t list; }

(** {3 Libraries} *)

type library =
  { path: Library.t;
    modules: Module.Name.t list; }

(** {3 Top-level items} *)

(* module PATH [= ALIAS] : [TYPE_PATH] [=] [TYPE] *)
type module_ =
  { path: Module.t;
    doc: doc;
    alias: module_path option;
    type_path: module_type_path option;
    type_: module_type_expr option; }

(* module type PATH [= ALIAS] [= EXPR] *)
and module_type =
  { path: ModuleType.t;
    doc: doc;
    alias: module_type_path option;
    expr: module_type_expr option; }

(** {3 Modules} *)

and module_type_expr =
  | Signature of signature

and signature = signature_item list

and signature_item =
  | Val of val_
  | Types of type_ list
  | Exn of exn_
  | Modules of nested_module list
  | ModuleType of nested_module_type
  | Comment of doc
  | SIG_todo of string

(** {3 Nested modules} *)

and nested_module =
  { name: Module.Name.t;
    doc: doc;
    desc: nested_module_desc; }

and nested_module_desc =
  | Alias of module_path
  | Type of nested_module_type_expr
  | MODULE_todo of string

and nested_module_type =
  { name: ModuleType.Name.t;
    doc: doc;
    desc: nested_module_type_desc; }

and nested_module_type_desc =
  | Manifest of nested_module_type_expr
  | Abstract
  | MODULE_TYPE_todo of string

and nested_module_type_expr =
  | Signature
  | Path of module_type_path

(** {3 Types} *)

and type_ =
  { name: Type.Name.t;
    doc: doc;
    param: string list;
    manifest: type_expr option;
    decl: type_decl option; }

and type_decl =
  | Variant of constructor list
  | Record of field list
  | TYPE_todo of string

and constructor =
  { name: Constructor.Name.t;
    doc: doc;
    args: type_expr list;
    ret: type_expr option; }

and field =
  { name: Field.Name.t;
    doc: doc;
    type_: type_expr; }

(** {3 Values} *)

and val_ =
  { name: Value.Name.t;
    doc: doc;
    type_: type_expr; }

(** {3 Type expressions} *)

and type_expr =
  | Var of string
  | Alias of type_expr * string
  | Arrow of label option * type_expr * type_expr
  | Tuple of type_expr list
  | Constr of type_path * type_expr list
  | TYPE_EXPR_todo of string

and label =
  | Label of string
  | Default of string

(** {3 Exceptions} *)

and exn_ =
  { name: Exn.Name.t;
    doc: doc;
    args: type_expr list;
    ret: type_expr option; }

(** {3 Paths} *)

and module_path =
  | Known of Module.t
  | Unknown of string

and module_type_path =
  | Known of ModuleType.t
  | Unknown of string

and type_path =
  | Known of Type.t
  | Unknown of string

(** {3 Documentation} *)

and doc =
  { info: text;
    tags: tag list; }

and text = text_element list

and text_element =
  | Raw of string
  | Code of string
  | PreCode of string
  | Verbatim of string
  | Style of style * text
  | List of text list
  | Enum of text list
  | Newline
  | Title of int * string option * text
  | Ref of reference * text option
  | Target of string option * string
  | TEXT_todo of string

and style =
  | Bold
  | Italic
  | Emphasize
  | Center
  | Left
  | Right
  | Superscript
  | Subscript
  | Custom of string

and reference =
  | Module of Module.t
  | ModuleType of ModuleType.t
  | Type of Type.t
  | Val of Value.t
  | Link of string

and tag =
  | Author of string
  | Version of string
  | See of Documentation.see_ref * text
  | Since of string
  | Before of string * text
  | Deprecated of text
  | Param of string * text
  | Raised_exception of string * text
  | Return_value of text
  | Custom of string * text

type api =
  { modules: module_ OpamDocPath.Module.Map.t;
    module_types: module_type OpamDocPath.ModuleType.Map.t; }
