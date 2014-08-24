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

(** Modules *)
module Module : sig

  (** Module names *)
  module Name : OpamMisc.ABSTRACT

  include OpamMisc.ABSTRACT

  (** Create a new module path *)
  val create: OpamLibrary.t -> Name.t -> t

  (** Create a new submodule path *)
  val create_submodule: t -> Name.t -> t

  (** Return the module's parent module *)
  val parent: t -> t option

  (** Return the module's name *)
  val name: t -> Name.t

  (** Return the module's library *)
  val library: t -> OpamLibrary.t

  (** Return the module's package *)
  val package: t -> OpamPackage.t

end

(** Module types *)
module ModuleType : sig

  (** Module type names *)
  module Name : OpamMisc.ABSTRACT

  include OpamMisc.ABSTRACT

  (** Create a new module type path *)
  val create: Module.t -> Name.t -> t

  (** Return the module type's parent module *)
  val parent: t -> Module.t

  (** Return the module type's name *)
  val name: t -> Name.t

  (** Return the module type's library *)
  val library: t -> OpamLibrary.t

  (** Return the module type's package *)
  val package: t -> OpamPackage.t

end

(** Types *)
module Type : sig

  (** Type names *)
  module Name : OpamMisc.ABSTRACT

  type t

  (** Create a new type path *)
  val create: Module.t -> Name.t -> t

  (** Return the type's parent module *)
  val parent: t -> Module.t

  (** Return the type's name *)
  val name: t -> Name.t

end

(** Values *)
module Value : sig

  (** Value names *)
  module Name : OpamMisc.ABSTRACT

  type t

  (** Create a new value path *)
  val create: Module.t -> Name.t -> t

  (** Return the module's parent module *)
  val parent: t -> Module.t

  (** Return the value's name *)
  val name: t -> Name.t

end

(** Constructors *)
module Constructor : sig

  (** Constructor names *)
  module Name : OpamMisc.ABSTRACT

end

(** Fields *)
module Field : sig

  (** Field names *)
  module Name : OpamMisc.ABSTRACT

end

type resolver = Module.Name.t -> Module.t option

val find_module: resolver -> Path.t -> Module.t option

val find_module_type: resolver -> Path.t -> ModuleType.t option

val find_type: resolver -> Path.t -> Type.t option

val find_value: resolver -> Path.t -> Value.t option

val lookup_module: resolver -> string -> Module.t option

val lookup_module_type: resolver -> string -> ModuleType.t option

val lookup_type: resolver -> string -> Type.t option

val lookup_value: resolver -> string -> Value.t option
