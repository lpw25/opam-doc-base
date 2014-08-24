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

open Assemblage

(* OCamlfind packages *)
let lib_pkgs = [pkg "opam-lib"; pkg "opam-lib.client"; pkg "opam-units"; pkg "xmlm"; pkg "compiler-libs.common";]
let bin_pkgs = [pkg "cmdliner"]

(* Warnings flags *)
let no_w30 = Flags.( v (`Compile `Byte) ["-w -30"]
                     @@@ v (`Compile `Native) ["-w -30"] )

(* Doc flag *)
let doc = Flags.( v (`Compile `Byte) ["-doc"]
                     @@@ v (`Compile `Native) ["-doc"] )


(* Compilation units *)
let opamDocPath = unit "opamDocPath" (`Path ["src"])
let opamDocTypes = unit ~flags:no_w30 "opamDocTypes" (`Path ["src"])
let opamDocCmi = unit "opamDocCmi" (`Path ["src"])
let opamDocCmti = unit "opamDocCmti" (`Path ["src"])
let opamDocXml = unit "opamDocXml" (`Path ["src"])
let opamDocBaseConfig  = unit "opamDocBaseConfig" (`Path ["src"])
let opamDocState = unit "opamDocState" (`Path ["src"])

let lib_units =
  [opamDocPath; opamDocTypes; opamDocCmi; opamDocCmti;
   opamDocXml; opamDocBaseConfig; opamDocState;]

let testCmi = unit "testCmi" (`Path ["src"])
let testCmti = unit "testCmti" (`Path ["src"])
let testState = unit "testState" (`Path ["src"])

(* Binary and library *)
let l = lib ~flags:doc ~deps:lib_pkgs "opam-doc-base" (`Units lib_units)

let testCmi = bin ~deps:(l :: bin_pkgs) "opam-doc-test-cmi" (`Units [testCmi])
let testCmti = bin ~deps:(l :: bin_pkgs) "opam-doc-test-cmti" (`Units [testCmti])
let testState = bin ~deps:(l :: bin_pkgs) "opam-doc-test-state" (`Units [testState])

let () = assemble (project "opam-doc-base" [l; testCmi; testCmti; testState])
