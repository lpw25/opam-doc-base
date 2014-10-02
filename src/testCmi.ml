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
open OpamDocTypes

let test cmi =
  let cmi_info = Cmi_format.read_cmi cmi in
  let pkg = OpamPackage.of_string "foo.1" in
  let lib = OpamLibrary.create pkg (OpamLibrary.Name.of_string "bar") in
  let name = OpamDocName.Module.of_string cmi_info.Cmi_format.cmi_name in
  let md = Module.create (Lib lib) name in
  let res _ = None in
  let api = OpamDocCmi.read_interface res md cmi_info.Cmi_format.cmi_sign in
  let intf = Module.Map.find md api.modules in
  let buf = Buffer.create 1024 in
  let output = Xmlm.make_output (`Buffer buf) in
  let () = OpamDocXml.module_to_xml output intf in
  Buffer.output_buffer stdout buf;
  ()

open Cmdliner

let cmi =
  let doc = "The cmi to test with." in
  Arg.(required & pos 0 (some string) None & info [] ~docv:"CMI" ~doc)

let test =
  let doc = "Test opam-doc-base on cmi files" in
  let info = Term.info ~doc "opam-doc-test-cmi" in
  (Term.(pure test $cmi), info)

let () =
  try
    match Term.eval test with
      `Error _ -> exit 1
    | _ -> exit 0
  with OpamGlobals.Exit i -> exit i
