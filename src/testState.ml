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

open OpamState.Types
open OpamDocPath

let parse_module_path pkg lib md =
  let pkg = Package.of_string pkg in
  let lib = Library.create pkg (Library.Name.of_string lib) in
  let rec loop = function
    | Longident.Lident s ->
        let name = Module.Name.of_string s in
        Module.create lib name
    | Longident.Ldot(p, s) ->
        let name = Module.Name.of_string s in
        Module.create_submodule (loop p) name
    | Longident.Lapply _ ->
        OpamGlobals.error_and_exit "Bad module identifier %s" md
  in
  loop (Longident.parse md)

let check_package t pkg =
  if Package.Set.mem pkg t.installed then ()
  else
    OpamGlobals.error_and_exit "Unknown package %s"
      (Package.to_string pkg)

let check_library t s lib =
  let pkg = Library.package lib in
  check_package t pkg;
  let libraries =
    try
      Package.Map.find pkg (OpamUnitsState.package_libraries s)
    with Not_found -> Library.Set.empty
  in
  if Library.Set.mem lib libraries then ()
  else
    OpamGlobals.error_and_exit "Unknown library %s"
      (Library.to_string lib)

let check_module t s r md =
  let lib = Module.library md in
  check_library t s lib;
  if Module.Set.mem md (OpamDocState.modules r) then ()
  else
    OpamGlobals.error_and_exit "Unknown module %s"
      (Module.to_string md)

let test package library module_ =
  OpamGlobals.root_dir := OpamGlobals.default_opam_dir;
  let md = parse_module_path package library module_ in
  let t = OpamState.load_state "opam-units" in
  let s = OpamUnitsState.load_state t in
  let r = OpamDocState.load_state t s in
  check_module t s r md;
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
