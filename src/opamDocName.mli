
module type Name = sig
  include OpamMisc.ABSTRACT
  val compare : t -> t -> int
end

(* TODO better module type *)
module Package: module type of struct include OpamPackage.Name end

(* TODO better module type *)
module Library: module type of struct include OpamLibrary.Name end

module Module : Name

module ModuleType : Name

module Type : Name

module Value : Name

module Constructor : Name

module Field : Name
