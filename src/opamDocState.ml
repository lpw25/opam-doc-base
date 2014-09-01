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
open OpamState.Types

type state =
  { timestamp: float;
    unit_timestamp: float OpamUnit.Map.t;
    modules: Module.Set.t;
    module_types: ModuleType.Set.t;
    root: OpamFilename.Dir.t; }

let read_state file =
  if not (OpamFilename.exists file) then None
  else begin
    let ic = OpamFilename.open_in file in
    set_binary_mode_in ic true;
    try
      let magic_no = OpamDocBaseConfig.cache_magic_number in
      let magic_len = String.length magic_no in
      let buffer = Bytes.create magic_len in
      really_input ic buffer 0 magic_len;
      if buffer <> magic_no then begin
        close_in ic;
        let pre_magic_len = magic_len - 2 in
        let pre_magic_no = String.sub magic_no 0 pre_magic_len in
        let pre_buffer = Bytes.sub buffer 0 pre_magic_len in
        let msg =
          if pre_buffer <> pre_magic_no then "is not a cache file."
          else if buffer < magic_no then
            "is not a cache file for this version of opam-doc-base.@.\
             It seems to be for an older version of opam-doc-base."
          else
            "is not a cache file for this version of opam-doc-base.@.\
             It seems to be for an newer version of opam-doc-base."
        in
          OpamGlobals.warning "%s %s" (OpamFilename.prettify file) msg;
          None
      end else begin
        let r : state = input_value ic in
        close_in ic;
        Some r
      end
    with End_of_file | Failure _ ->
      close_in ic;
      OpamGlobals.warning
        "Cache file %s is corrupted."
        (OpamFilename.prettify file);
      None
  end

let write_state file r =
  let magic_no = OpamDocBaseConfig.cache_magic_number in
  let oc = OpamFilename.open_out file in
    set_binary_mode_out oc true;
    try
      output_string oc magic_no;
      output_value oc r;
      close_out oc
    with exn ->
      close_out oc;
      raise exn

let read_xml parser file =
  if not (OpamFilename.exists file) then
    OpamGlobals.error_and_exit "Missing file %s" (OpamFilename.prettify file);
  let ic = OpamFilename.open_in file in
    try
      let input = Xmlm.make_input ~strip:true (`Channel ic) in
      let result = parser input in
        close_in ic;
        result
    with End_of_file | Failure _ ->
      close_in ic;
      OpamGlobals.error_and_exit
        "File %s is corrupted."
        (OpamFilename.prettify file)

let write_xml parser file x =
  let oc = OpamFilename.open_out file in
  try
    let output = Xmlm.make_output ~indent:(Some 2) (`Channel oc) in
      parser output x;
      close_out oc
  with exn ->
    close_out oc;
    raise exn

(* TODO Don't create directories if we are doing a lookup *)

let package_directory r pkg =
  let pkg_str = Package.to_string pkg in
  let dir = OpamFilename.OP.(r.root / pkg_str) in
    if not (OpamFilename.exists_dir dir) then OpamFilename.mkdir dir;
    dir

let package_file r pkg =
  let pkg_dir = package_directory r pkg in
    OpamFilename.OP.(pkg_dir // OpamDocBaseConfig.pkg_idx_name)

let library_directory r lib =
  let pkg_dir = package_directory r (Library.package lib) in
  let lib_str = Library.Name.to_string (Library.name lib) in
  let dir = OpamFilename.OP.(pkg_dir / lib_str) in
    if not (OpamFilename.exists_dir dir) then OpamFilename.mkdir dir;
    dir

let library_file r lib =
  let lib_dir = library_directory r lib in
    OpamFilename.OP.(lib_dir // OpamDocBaseConfig.lib_idx_name)

let modules_directory r lib =
  let lib_dir = library_directory r lib in
  let dir =
    OpamFilename.OP.(lib_dir / OpamDocBaseConfig.modules_dir_name)
  in
    if not (OpamFilename.exists_dir dir) then OpamFilename.mkdir dir;
    dir

let module_file r md =
  let mod_dir = modules_directory r (Module.library md) in
  let rec loop acc md =
    let name = (Module.Name.to_string (Module.name md)) in
    let acc = name ^ "." ^ acc in
      match Module.parent md with
      | None -> acc
      | Some par -> loop acc par
  in
  let mod_str = loop "xml" md in
    OpamFilename.OP.(mod_dir // mod_str)

let module_types_directory r lib =
  let lib_dir = library_directory r lib in
  let dir =
    OpamFilename.OP.(lib_dir / OpamDocBaseConfig.module_types_dir_name)
  in
    if not (OpamFilename.exists_dir dir) then OpamFilename.mkdir dir;
    dir

let module_type_file r mty =
  let mty_dir = module_types_directory r (ModuleType.library mty) in
  let name = (ModuleType.Name.to_string (ModuleType.name mty)) in
  let rec loop acc md =
    let name = (Module.Name.to_string (Module.name md)) in
    let acc = name ^ "." ^ acc in
      match Module.parent md with
      | None -> acc
      | Some par -> loop acc par
  in
  let mty_str = loop (name ^ ".xml") (ModuleType.parent mty) in
    OpamFilename.OP.(mty_dir // mty_str)

let read_module r path =
  let file = module_file r path in
    read_xml OpamDocXml.module_of_xml file

let write_module r path md =
  let file = module_file r path in
    write_xml OpamDocXml.module_to_xml file md

let read_module_type r path =
  let file = module_type_file r path in
    read_xml OpamDocXml.module_type_of_xml file

let write_module_type r path mty =
  let file = module_type_file r path in
    write_xml OpamDocXml.module_type_to_xml file mty

let read_library r path =
  let file = library_file r path in
    read_xml OpamDocXml.library_of_xml file

let write_library r path lib =
  let file = library_file r path in
    write_xml OpamDocXml.library_to_xml file lib

let read_package r path =
  let file = package_file r path in
    read_xml OpamDocXml.package_of_xml file

let write_package r path pkg =
  let file = package_file r path in
    write_xml OpamDocXml.package_to_xml file pkg

let cmi_error file = function
  | Cmi_format.Not_an_interface _ ->
      OpamGlobals.error_and_exit
        "%s is not a compiled interface."
        (OpamFilename.prettify file)
  | Cmi_format.Wrong_version_interface(_, msg) ->
      OpamGlobals.error_and_exit
        "%s is not a compiled interface for this version of OCaml.@.\
         It seems to be for %s version of OCaml."
        (OpamFilename.prettify file) msg
  | Cmi_format.Corrupted_interface _ ->
      OpamGlobals.error_and_exit
        "Compiled interface %s is corrupted."
        (OpamFilename.prettify file)

let read_cmi file =
  try
    let cmi_info = Cmi_format.read_cmi (OpamFilename.to_string file) in
      cmi_info.Cmi_format.cmi_sign
  with Cmi_format.Error err -> cmi_error file err

let cmt_error file = function
  | Cmt_format.Not_a_typedtree _ ->
      OpamGlobals.error_and_exit
        "%s is not a compiled interface."
        (OpamFilename.prettify file)

let read_cmti file =
  try
    let cmt_info = Cmt_format.read_cmt (OpamFilename.to_string file) in
      match cmt_info.Cmt_format.cmt_annots with
      | Cmt_format.Interface sg -> sg
      | _ ->
          OpamGlobals.error_and_exit
            "%s is not a compiled interface."
            (OpamFilename.prettify file)
  with
  | Cmi_format.Error err -> cmi_error file err
  | Cmt_format.Error err -> cmt_error file err

let resolve_name import_graph unit =
  let imports = OpamUnit.Graph.pred import_graph unit in
  let conv_unit unit =
    let name =
      Module.Name.of_string
        (OpamUnit.Name.to_string (OpamUnit.name unit))
    in
    let lib = OpamUnit.library unit in
    let md = Module.create lib name in
      name, md
  in
  let map =
    List.fold_left
      (fun acc import ->
         let name, md = conv_unit import in
           Module.Name.Map.add name md acc)
      Module.Name.Map.empty imports
  in
  let name, md = conv_unit unit in
  let map = Module.Name.Map.add name md map in
  let resolve name =
      try
        Some (Module.Name.Map.find name map)
      with Not_found -> None
  in
    resolve

let add_module path md r =
  write_module r path md;
  { r with modules = Module.Set.add path r.modules }

let add_module_type path md r =
  write_module_type r path md;
  { r with module_types = ModuleType.Set.add path r.module_types }

let add_unit import_graph s unit r =
  let name =
    Module.Name.of_string (OpamUnit.Name.to_string (OpamUnit.name unit))
  in
  let lib = OpamUnit.library unit in
  let md = Module.create lib name in
  let res = resolve_name import_graph unit in
  let cmi_file = OpamUnit.Map.find unit (OpamUnitsState.unit_file s) in
  let cmti_file =
    OpamFilename.add_extension (OpamFilename.chop_extension cmi_file) "cmt"
  in
  let api, timestamp =
    if OpamFilename.exists cmti_file then begin
      let stats = Unix.stat (OpamFilename.to_string cmti_file) in
      let timestamp = stats.Unix.st_mtime in
      let sg = read_cmti cmti_file in
      let api = OpamDocCmti.read_interface_tree res md sg in
        api, timestamp
    end else begin
      let stats = Unix.stat (OpamFilename.to_string cmi_file) in
      let timestamp = stats.Unix.st_mtime in
      let sg = read_cmi cmi_file in
      let api = OpamDocCmi.read_interface res md sg in
        api, timestamp
    end
  in
  let r = Module.Map.fold add_module api.modules r in
  let r = ModuleType.Map.fold add_module_type api.module_types r in
  let unit_timestamp = OpamUnit.Map.add unit timestamp r.unit_timestamp in
    { r with unit_timestamp }

let add_library path units r =
  let modules =
    OpamUnit.Set.fold
      (fun unit acc ->
       let name = OpamUnit.name unit in
       let md = Module.Name.of_string (OpamUnit.Name.to_string name) in
         md :: acc)
      units []
  in
  let lib = {path; modules} in
    write_library r path lib;
    r

let add_package path libs r =
  let libraries =
    OpamLibrary.Set.fold
      (fun lib acc -> (OpamLibrary.name lib) :: acc)
      libs []
  in
  let pkg = {path; libraries} in
    write_package r path pkg;
    r

let remove_unit _ = failwith "undefined"

let remove_library _ = failwith "undefined"

let remove_package _ = failwith "undefined"

let init_units s r =
  let units = OpamUnitsState.units s in
  let import_graph = OpamUnitsState.unit_import_graph s in
    OpamUnit.Set.fold (add_unit import_graph s) units r

let init_libraries s r =
  let libraries = OpamUnitsState.library_units s in
    OpamLibrary.Map.fold add_library libraries r

let init_packages t s r =
  let packages = t.installed in
  let package_libraries = OpamUnitsState.package_libraries s in
    OpamPackage.Set.fold
      (fun pkg r ->
         let libs =
           try
             OpamPackage.Map.find pkg package_libraries
           with Not_found -> OpamLibrary.Set.empty
         in
           add_package pkg libs r)
      packages r

let init_state (t : OpamState.state) s =
  let share_dir =
    OpamPath.Switch.share t.root t.switch OpamDocBaseConfig.package_name
  in
  OpamGlobals.msg
    "Creating documentation state in %s ...\n"
    (OpamFilename.prettify_dir share_dir);
  let r =
    { timestamp = 0.0;
      root = OpamFilename.OP.(share_dir / OpamDocBaseConfig.root_name);
      unit_timestamp = OpamUnit.Map.empty;
      modules = Module.Set.empty;
      module_types = ModuleType.Set.empty;}
  in
  let r = init_units s r in
  let r = init_libraries s r in
  let r = init_packages t s r in
  let file = OpamFilename.OP.(share_dir // OpamDocBaseConfig.cache_file) in
    write_state file r;
    r

let load_state t s = init_state t s

let modules s = s.modules

let module_types s = s.module_types

let load_module r md = read_module r md

let load_module_type r mty = read_module_type r mty

let load_library r lib = read_library r lib

let load_package r pkg = read_package r pkg
