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

let test package library module_ =
  OpamGlobals.root_dir := OpamGlobals.default_opam_dir;
  let t = OpamState.load_state "opam-units" in
  let s = OpamUnitsState.load_state t in
  let r = OpamDocState.load_state t s in
  let pkg = Package.of_string package in
  let lib_name = Library.Name.of_string library in
  let lib = Library.create pkg lib_name in
  let md_name = Module.Name.of_string module_ in
  let md = Module.create lib md_name in
  let intf = OpamDocState.load_module r md in
  let buf = Buffer.create 1024 in
  let output = Xmlm.make_output (`Buffer buf) in
  let () = OpamDocXml.module_to_xml output intf in
    Buffer.output_buffer stdout buf;
    ()

open Cmdliner

let package =
  let doc = "The package to lookup." in
  Arg.(required & pos 0 (some string) None & info [] ~docv:"PACKAGE" ~doc)

let library =
  let doc = "The library to lookup." in
  Arg.(required & pos 1 (some string) None & info [] ~docv:"LIBRARY" ~doc)

let module_ =
  let doc = "The module to lookup." in
  Arg.(required & pos 2 (some string) None & info [] ~docv:"MODULE" ~doc)

let test =
  let doc = "Test opam-doc-base state" in
  let info = Term.info ~doc "opam-doc-test-state" in
    (Term.(pure test $package $library $module_), info)

let () =
  try
    match Term.eval test with
      `Error _ -> exit 1
    | _ -> exit 0
  with OpamGlobals.Exit i -> exit i
