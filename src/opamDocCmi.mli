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

val read_interface: OpamDocPath.resolver -> OpamDocPath.Module.t ->
  Types.signature -> OpamDocTypes.module_

(* TODO remove these from the interface when OpamDocCmti is finished *)
val read_module_declaration: OpamDocPath.resolver -> OpamDocPath.parent ->
  Ident.t -> Types.module_declaration -> OpamDocTypes.module_

val read_modtype_declaration: OpamDocPath.resolver -> OpamDocPath.parent ->
  Ident.t -> Types.modtype_declaration -> OpamDocTypes.module_type

val read_type_declaration: OpamDocPath.resolver -> Ident.t ->
  Types.type_declaration -> OpamDocTypes.type_

val read_value_description: OpamDocPath.resolver -> Ident.t ->
  Types.value_description -> OpamDocTypes.val_

val read_extension_constructor: OpamDocPath.resolver -> Ident.t ->
  Types.extension_constructor -> OpamDocTypes.exn_

val read_attributes: OpamDocPath.resolver -> Parsetree.attributes ->
  OpamDocTypes.doc

val read_documentation: OpamDocPath.resolver -> Documentation.t ->
  OpamDocTypes.text * OpamDocTypes.tag list
