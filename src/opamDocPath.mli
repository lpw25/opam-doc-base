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

(** Documentation paths *)

(** Packages *)
(* TODO better module type *)
module Package: module type of struct include OpamPackage end

(** Libraries *)
(* TODO better module type *)
module Library: module type of struct include OpamLibrary end

type module_

type modtype

type parent =
  | Lib of Library.t
  | Module of module_
  | ModType of modtype

val parent_to_string : parent -> string

(** Modules *)
module Module : sig

  include OpamMisc.ABSTRACT with type t = module_

  (** Create a new module path *)
  val create: parent -> OpamDocName.Module.t -> t

  (** Return the module's parent module *)
  val parent: t -> parent

  (** Return the module's name *)
  val name: t -> OpamDocName.Module.t

  (** Return the module's library *)
  val library: t -> OpamLibrary.t

  (** Return the module's package *)
  val package: t -> OpamPackage.t

end

(** Module types *)
module ModuleType : sig

  include OpamMisc.ABSTRACT with type t = modtype

  (** Create a new module type path *)
  val create: parent -> OpamDocName.ModuleType.t -> t

  (** Return the module type's parent module *)
  val parent: t -> parent

  (** Return the module type's name *)
  val name: t -> OpamDocName.ModuleType.t

  (** Return the module type's library *)
  val library: t -> OpamLibrary.t

  (** Return the module type's package *)
  val package: t -> OpamPackage.t

end

(** Types *)
module Type : sig

  type t

  (** Create a new type path *)
  val create: parent -> OpamDocName.Type.t -> t

  (** Return the type's parent module *)
  val parent: t -> parent

  (** Return the type's name *)
  val name: t -> OpamDocName.Type.t

end

(** Values *)
module Value : sig

  type t

  (** Create a new value path *)
  val create: parent -> OpamDocName.Value.t -> t

  (** Return the module's parent module *)
  val parent: t -> parent

  (** Return the value's name *)
  val name: t -> OpamDocName.Value.t

end

type resolver = OpamDocName.Module.t -> Module.t option

val find_module: resolver -> Path.t -> Module.t option

val find_module_type: resolver -> Path.t -> ModuleType.t option

val find_type: resolver -> Path.t -> Type.t option

val find_value: resolver -> Path.t -> Value.t option

val lookup_module: resolver -> string -> Module.t option

val lookup_module_type: resolver -> string -> ModuleType.t option

val lookup_type: resolver -> string -> Type.t option

val lookup_value: resolver -> string -> Value.t option
