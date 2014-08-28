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

(* XML tag names *)

let package_n: Xmlm.name = ("","package")
let library_n: Xmlm.name = ("","library")

let module_n: Xmlm.name = ("","module")
let modules_n: Xmlm.name = ("","modules")
let module_type_n: Xmlm.name = ("","module-type")
let signature_n: Xmlm.name = ("","signature")
let alias_n: Xmlm.name = ("","alias")

let type_n: Xmlm.name = ("","type")
let types_n: Xmlm.name = ("","types")
let param_n: Xmlm.name = ("","param")
let manifest_n: Xmlm.name = ("","manifest")
let variant_n: Xmlm.name = ("","variant")
let record_n: Xmlm.name = ("","record")
let constructor_n: Xmlm.name = ("","constructor")
let field_n: Xmlm.name = ("","field")
let arg_n: Xmlm.name = ("","arg")
let return_n: Xmlm.name = ("","return")

let val_n: Xmlm.name = ("","val")

let var_n: Xmlm.name = ("","var")
let arrow_n: Xmlm.name = ("","arrow")
let tuple_n: Xmlm.name = ("","tuple")
let constr_n: Xmlm.name = ("","constr")
let label_n: Xmlm.name = ("","label")
let default_n: Xmlm.name = ("","default")

let path_n: Xmlm.name = ("","path")
let name_n: Xmlm.name = ("","name")

let comment_n: Xmlm.name = ("","comment")
let doc_n: Xmlm.name = ("","doc")
let info_n: Xmlm.name = ("","info")
let code_n: Xmlm.name = ("","code")
let bold_n: Xmlm.name = ("","bold")
let italic_n: Xmlm.name = ("","italic")
let emph_n: Xmlm.name = ("","emph")
let center_n: Xmlm.name = ("","center")
let left_n: Xmlm.name = ("","left")
let right_n: Xmlm.name = ("","right")
let superscript_n: Xmlm.name = ("","superscript")
let subscript_n: Xmlm.name = ("","subscript")
let list_n: Xmlm.name = ("","list")
let enum_n: Xmlm.name = ("","enum")
let newline_n: Xmlm.name = ("","newline")
let item_n: Xmlm.name = ("","item")
let title_n: Xmlm.name = ("","title")
let ref_n: Xmlm.name = ("","ref")


(* XML parser combinators *)

module Parser : sig

  (* Expose functional nature of parser to allow eta expansion *)
  type 'a t = 'a contra -> 'a co
    and 'a contra and 'a co

  val pure: 'a -> 'a t

  type open_ = Open

  val open_: Xmlm.name -> open_ t

  type close = Close

  val close: Xmlm.name -> close t

  val data: string t

  val dtd: Xmlm.dtd t

  val (@@): 'a t -> 'a t -> 'a t

  val (%): ('a -> 'b) t -> 'a t -> 'b t

  val (!!): 'a -> 'a t

  val opt: 'a t -> 'a option t

  val list: 'a t -> 'a list t

  val seq: 'a t -> 'a list t

  val opt_list: 'a list t -> 'a list t

  val run: 'a t -> Xmlm.input -> 'a

end = struct

  type expected =
    | Dtd
    | Open of Xmlm.name
    | Data
    | Close of Xmlm.name

  type 'a reply =
    | Ok of 'a * expected list
    | Error of expected list

  type 'a consumed =
    | Consumed of 'a t
    | Empty of 'a reply

  and 'a t = Xmlm.input -> 'a consumed

  type 'a contra = Xmlm.input
  type 'a co = 'a consumed

  let pure x input = Empty (Ok(x, []))

  type open_ = Open

  let open_ n input =
    if Xmlm.eoi input then Empty (Error [Open n])
    else
      match Xmlm.peek input with
      | `El_start (n', _) when n = n' ->
          ignore (Xmlm.input input);
          Consumed (pure Open)
      | s -> Empty (Error [Open n])

  type close = Close

  let close n input =
    if Xmlm.eoi input then Empty (Error [Close n])
    else
      match Xmlm.peek input with
      | `El_end ->
          ignore (Xmlm.input input);
          Consumed (pure Close)
      | s -> Empty (Error [Close n])

  let data input =
    if Xmlm.eoi input then Empty (Error [Data])
    else
      match Xmlm.peek input with
      | `Data s ->
          ignore (Xmlm.input input);
          Consumed (pure s)
      | s -> Empty (Error [Data])

  let dtd input =
    if Xmlm.eoi input then Empty (Error [Dtd])
    else
      match Xmlm.peek input with
      | `Dtd d ->
          ignore (Xmlm.input input);
          Consumed (pure d)
      | s -> Empty (Error [Dtd])

  let (@@) p1 p2 input =
    match p1 input with
    | Empty (Error e) -> begin
        match p2 input with
        | Empty (Error e') -> Empty (Error (e @ e'))
        | Empty (Ok(a, e')) -> Empty (Ok(a, (e @ e')))
        | consumed -> consumed
      end
    | Empty (Ok(a, e)) -> begin
        match p2 input with
        | Empty (Error e') -> Empty (Ok(a, (e @ e')))
        | Empty (Ok(_, e')) -> Empty (Ok(a, (e @ e')))
        | consumed -> consumed
      end
    | consumed -> consumed

  let rec (%) p1 p2 input =
    match p1 input with
    | Empty (Error e) -> Empty (Error e)
    | Empty (Ok(a, e)) -> begin
        match p2 input with
        | Empty (Error e') -> Empty (Error (e @ e'))
        | Empty (Ok(b, e')) -> Empty (Ok(a b, (e @ e')))
        | Consumed p -> Consumed (pure a % p)
      end
    | Consumed p -> Consumed (p % p2)

  let (!!) = pure

  let none = None
  let some x = Some x
  let opt p = !!none @@ !!some %p

  let nil = []
  let cons hd tl = hd :: tl
  let rec list p input =
    let parser = !!nil @@ !!cons %p %(list p) in
      parser input

  let seq p = !!cons %p %(list p)

  let opt_list p = !!nil @@ p

  let expected_msg = function
    | Dtd -> "doctype"
    | Open(_, name) -> "<" ^ name ^ ">"
    | Data -> "data"
    | Close(_, name) -> "</" ^ name ^ ">"

  let found_msg = function
    | `Data s -> "\"" ^ s ^ "\""
    | `Dtd _ -> "doctype"
    | `El_start((_, name), _) -> "<" ^ name ^ ">"
    | `El_end -> "<\\ ... >"

  let show_error (line, column) msg =
      OpamGlobals.error_and_exit "Parse error:%d.%d: %s" line column msg

  let expected_error input e =
    let expected = String.concat " or " (List.map expected_msg e) in
    let pos = Xmlm.pos input in
      if Xmlm.eoi input then begin
        let msg =
          Printf.sprintf "expected %s but found end of file" expected
        in
          show_error pos msg
      end else begin
        let found = found_msg (Xmlm.peek input) in
        let msg =
          Printf.sprintf "expected  %s but found %s" expected found
        in
          show_error pos msg
     end

  let xmlm_error pos err =
    let msg = Xmlm.error_message err in
      show_error pos msg

  let rec run p input =
    match p input with
    | Empty(Ok(a, _)) -> a
    | Consumed p -> run p input
    | Empty(Error e) -> expected_error input e
    | exception (Xmlm.Error(pos, err)) -> xmlm_error pos err

end

type open_ = Parser.open_ = Open
type close = Parser.close = Close

(* XML parsers *)

let string_in =
  let action = function
    | None -> ""
    | Some s -> s
  in
    Parser.( !!action %(opt data) )

let name_in =
  let action Open s Close = s in
    Parser.( !!action %(open_ name_n) %data %(close name_n) )

let package_t_in =
  let action Open name Close = Package.of_string name in
    Parser.( !!action %(open_ package_n) %name_in %(close package_n) )

let library_t_in =
  let action Open package name Close =
    let name = Library.Name.of_string name in
      Library.create package name
  in
    Parser.( !!action %(open_ library_n)
                        %package_t_in %name_in
                      %(close library_n) )

let rec module_t_in input =
  let action Open create name Close =
    let name = Module.Name.of_string name in
      create name
  in
  let parser =
    Parser.( !!action %(open_ module_n)
                        %module_parent_in %name_in
                      %(close module_n) )
  in
    parser input

and module_parent_in input =
  let library lib = Module.create lib in
  let module_ md = Module.create_submodule md in
  let parser =
    Parser.(   !!library %library_t_in
            @@ !!module_ %module_t_in  )
  in
    parser input

let module_type_t_in =
  let action Open parent name Close =
    let name = ModuleType.Name.of_string name in
      ModuleType.create parent name
  in
    Parser.( !!action %(open_ module_type_n)
                        %module_t_in %name_in
                      %(close module_type_n) )

let type_t_in =
  let action Open parent name Close =
    let name = Type.Name.of_string name in
      Type.create parent name
  in
    Parser.( !!action %(open_ type_n) %module_t_in %name_in %(close type_n) )

let value_t_in =
  let action Open parent name Close =
    let name = Value.Name.of_string name in
      Value.create parent name
  in
    Parser.( !!action %(open_ val_n) %module_t_in %name_in %(close val_n) )

let reference_in =
  let module_ Open path Close = Module path in
  let module_type Open path Close = ModuleType path in
  let type_ Open path Close = Type path in
  let val_ Open path Close = Val path in
    Parser.(    !!module_ %(open_ module_n) %module_t_in %(close module_n)
             @@ !!module_type %(open_ module_type_n)
                                %module_type_t_in
                              %(close module_type_n)
             @@ !!type_ %(open_ type_n) %type_t_in %(close type_n)
             @@ !!val_ %(open_ val_n) %value_t_in %(close val_n) )

let rec text_element_in input =
  let raw s = Raw s in
  let code Open s Close = Code s in
  let bold Open txt Close = Style(Bold, txt) in
  let italic Open txt Close = Style(Italic, txt) in
  let emph Open txt Close = Style(Emphasize, txt) in
  let center Open txt Close = Style(Center, txt) in
  let left Open txt Close = Style(Left, txt) in
  let right Open txt Close = Style(Right, txt) in
  let super Open txt Close = Style(Superscript, txt) in
  let sub Open txt Close = Style(Subscript, txt) in
  let list_ Open items Close = List items in
  let enum Open items Close = Enum items in
  let newline Open Close = Newline in
  let title Open txt Close = Title txt in
  let reference Open rf txto Close = Ref(rf, txto) in
  let parser =
    Parser.(   !!raw %data
            @@ !!code %(open_ code_n) %string_in %(close code_n)
            @@ !!bold %(open_ bold_n) %(list text_element_in) %(close bold_n)
            @@ !!italic %(open_ italic_n) %(list text_element_in) %(close italic_n)
            @@ !!emph %(open_ emph_n) %(list text_element_in) %(close emph_n)
            @@ !!center %(open_ center_n) %(list text_element_in) %(close center_n)
            @@ !!left %(open_ left_n) %(list text_element_in) %(close left_n)
            @@ !!right %(open_ right_n) %(list text_element_in) %(close right_n)
            @@ !!super %(open_ superscript_n)
                         %(list text_element_in)
                       %(close superscript_n)
            @@ !!sub %(open_ subscript_n) %(list text_element_in) %(close subscript_n)
            @@ !!list_ %(open_ list_n) %(list item_in) %(close list_n)
            @@ !!enum %(open_ enum_n) %(list item_in) %(close enum_n)
            @@ !!newline %(open_ newline_n) %(close newline_n)
            @@ !!title %(open_ title_n) %(list text_element_in) %(close title_n)
            @@ !!reference %(open_ ref_n)
                             %reference_in
                             %(opt (list text_element_in))
                           %(close ref_n) )
  in
    parser input

and item_in input =
  let action Open txt Close = txt in
  let parser =
    Parser.( !!action %(open_ item_n) %(list text_element_in) %(close item_n) )
  in
    parser input

let doc_in =
  let none = {info = []} in
  let doc Open info Close = {info} in
    Parser.( !!none
             @@ !!doc %(open_ doc_n) %(list text_element_in) %(close doc_n) )

let type_path_in =
  let known Open path Close = Known path in
  let unknown string = Unknown string in
    Parser.(   !!known %(open_ path_n) %type_t_in %(close path_n)
            @@ !!unknown %name_in )

let module_type_path_in =
  let known Open path Close : module_type_path = Known path in
  let unknown string : module_type_path = Unknown string in
    Parser.(   !!known %(open_ path_n) %module_type_t_in %(close path_n)
            @@ !!unknown %name_in )

let module_path_in =
  let known Open path Close : module_path = Known path in
  let unknown string : module_path = Unknown string in
    Parser.(   !!known %(open_ path_n) %module_t_in %(close path_n)
            @@ !!unknown %name_in )

let label_in =
  let label Open string Close = Label string in
  let default Open string Close = Default string in
    Parser.(   !!label %(open_ label_n) %data %(close label_n)
            @@ !!default %(open_ default_n) %data %(close default_n) )

let rec type_expr_in input =
  let var Open string Close = Var string in
  let alias Open typ Open string Close Close = Alias(typ, string) in
  let arrow Open lbl arg ret Close = Arrow(lbl, arg, ret) in
  let tuple Open typs Close = Tuple typs in
  let constr Open path typs Close = Constr(path, typs) in
  let parser =
    Parser.(   !!var %(open_ var_n) %data %(close var_n)
            @@ !!alias %(open_ alias_n)
                         %type_expr_in %(open_ var_n) %data %(close var_n)
                       %(close alias_n)
            @@ !!arrow %(open_ arrow_n)
                         %(opt label_in) %type_expr_in %type_expr_in
                       %(close arrow_n)
            @@ !!tuple %(open_ tuple_n) %(list type_expr_in) %(close tuple_n)
            @@ !!constr %(open_ constr_n)
                          %type_path_in %(list type_expr_in)
                        %(close constr_n) )
  in
    parser input

let val_in =
  let action Open name doc type_ Close =
    {name = Value.Name.of_string name; doc; type_}
  in
    Parser.( !!action %(open_ val_n)
                        %name_in %doc_in %type_expr_in
                      %(close val_n) )

let field_in =
  let action Open name doc type_ Close : field =
    {name = Field.Name.of_string name; doc; type_}
  in
    Parser.( !!action %(open_ field_n)
                        %name_in %doc_in %type_expr_in
                      %(close field_n) )

let ret_in =
  let action Open typ Close = typ in
    Parser.( !!action %(open_ return_n) %type_expr_in %(close return_n) )

let arg_in =
  let action Open typ Close = typ in
    Parser.( !!action %(open_ arg_n) %type_expr_in %(close arg_n) )

let constructor_in =
  let action Open name doc args ret Close =
    {name = Constructor.Name.of_string name; doc; args; ret;}
  in
    Parser.( !!action %(open_ constructor_n)
                        %name_in %doc_in %(list arg_in) %(opt ret_in)
                      %(close constructor_n) )

let type_kind_in =
  let abstract = None in
  let variant Open cstrs Close = Some (Variant cstrs) in
  let record Open fields Close = Some (Record fields) in
    Parser.(   !!abstract
            @@ !!variant %(open_ variant_n)
                           %(seq constructor_in)
                         %(close variant_n)
            @@ !!record %(open_ record_n) %(seq field_in) %(close record_n) )

let manifest_in =
  let action Open typ Close = typ in
    Parser.( !!action %(open_ manifest_n) %type_expr_in %(close manifest_n) )

let param_in =
  let action Open string Close = string in
    Parser.( !!action %(open_ param_n) %data %(close param_n) )

let type_decl_in =
  let action Open name doc param manifest decl Close =
    {name = Type.Name.of_string name; doc; param; manifest; decl}
  in
    Parser.( !!action %(open_ type_n)
                        %name_in %doc_in %(list param_in)
                        %(opt manifest_in) %type_kind_in
                      %(close type_n) )

let nested_module_type_in =
  let action Open name doc desc Close =
    {name = ModuleType.Name.of_string name; doc; desc}
  in
  let abstract = Abstract in
  let path path = Manifest (Path path) in
  let sign Open Close = Manifest Signature in
    Parser.( !!action %(open_ module_type_n)
                         %name_in %doc_in
                         %(   !!abstract
                           @@ !!path %module_type_path_in
                           @@ !!sign %(open_ signature_n)
                                     %(close signature_n) )
                      %(close module_type_n) )

let nested_module_in =
  let action Open name doc desc Close : nested_module =
    {name = Module.Name.of_string name; doc; desc}
  in
  let alias Open path Close : nested_module_desc= Alias path in
  let path path : nested_module_desc = Type (Path path) in
  let sign Open Close : nested_module_desc = Type Signature in
    Parser.( !!action %(open_ module_n) %name_in %doc_in
                        %(   !!alias %(open_ alias_n)
                                       %module_path_in
                                     %(close alias_n)
                          @@ !!path %module_type_path_in
                          @@ !!sign %(open_ signature_n) %(close signature_n) )
                     %(close module_n) )

let signature_item_in =
  let val_ v : signature_item = Val v in
  let type_ t = Types [t] in
  let types Open ts Close = Types ts in
  let module_ md = Modules [md] in
  let modules Open mds Close = Modules mds in
  let module_type mtd : signature_item = ModuleType mtd in
  let comment Open info Close = Comment {info} in
    Parser.(   !!val_ %val_in
            @@ !!type_ %type_decl_in
            @@ !!types %(open_ types_n) %(seq type_decl_in) %(close types_n)
            @@ !!module_ %nested_module_in
            @@ !!modules %(open_ modules_n)
                             %(seq nested_module_in)
                           %(close modules_n)
            @@ !!module_type %nested_module_type_in
            @@ !!comment %(open_ comment_n)
                             %(list text_element_in)
                           %(close comment_n) )

let module_type_expr_in =
  let action Open sg Close : module_type_expr = Signature sg in
    Parser.( !!action %(open_ signature_n)
                        %(list signature_item_in)
                      %(close signature_n) )

let module_alias_in =
  let action Open path Close = path in
    Parser.( !!action %(open_ alias_n) %module_path_in %(close alias_n) )


let module_type_in =
  let action Open Open path Close doc alias expr Close =
    { path; doc; alias; expr; }
  in
    Parser.( !!action %(open_ module_type_n)
                        %(open_ path_n)
                          %module_type_t_in
                        %(close path_n)
                        %doc_in
                        %(opt module_type_path_in) %(opt module_type_expr_in)
                      %(close module_type_n) )

let module_in =
  let action Open Open path Close doc alias type_path type_ Close =
    { path; doc; alias; type_path; type_; }
  in
  Parser.( !!action %(open_ module_n)
                      %(open_ path_n)
                        %module_t_in
                      %(close path_n)
                      %doc_in %(opt module_alias_in)
                      %(opt module_type_path_in) %(opt module_type_expr_in)
                    %(close module_n) )

let library_in =
  let action Open path modules Close = { path; modules; } in
  let modl Open name Close = Module.Name.of_string name in
    Parser.( !!action %(open_ library_n)
                        %library_t_in
                        %(list (!!modl %(open_ module_n) %name_in  %(close module_n)))
                      %(close library_n) )

let package_in =
  let action Open path libraries Close = { path; libraries; } in
  let lib Open name Close = Library.Name.of_string name in
    Parser.( !!action %(open_ package_n)
                        %package_t_in
                        %(list (!!lib %(open_ library_n) %name_in  %(close library_n)))
                      %(close package_n) )

let file p =
  let action dtd x = x in
    Parser.( !!action %dtd %p )

let module_of_xml input = Parser.run (file module_in) input

let module_type_of_xml input = Parser.run (file module_type_in) input

let library_of_xml input = Parser.run (file library_in) input

let package_of_xml input = Parser.run (file package_in) input

(* XML printer utilities *)

let open_ output n =
  Xmlm.output output (`El_start (n, []))

let close output n =
  Xmlm.output output `El_end

let data output s =
  Xmlm.output output (`Data s)

let dtd output d =
  Xmlm.output output (`Dtd d)

let opt p output o =
  match o with
  | None -> ()
  | Some x -> p output x

let rec list p output l =
  match l with
  | [] -> ()
  | x :: xs ->
      p output x;
      list p output xs

(* XML printers *)

let string_out output str =
  if String.length str = 0 then ()
  else data output str

let name_out output name =
  open_ output name_n;
  data output name;
  close output name_n

let package_t_out output package =
  let name = Package.to_string package in
    open_ output package_n;
    name_out output name;
    close output package_n

let library_t_out output lib =
  let package = Library.package lib in
  let name = Library.Name.to_string (Library.name lib) in
    open_ output library_n;
    package_t_out output package;
    name_out output name;
    close output library_n

let rec module_t_out output md =
  let parent_out output md =
    match Module.parent md with
    | Some par -> module_t_out output par
    | None -> library_t_out output (Module.library md)
  in
  let name = Module.Name.to_string (Module.name md) in
    open_ output module_n;
    parent_out output md;
    name_out output name;
    close output module_n

let module_type_t_out output mty =
  let parent = ModuleType.parent mty in
  let name = ModuleType.Name.to_string (ModuleType.name mty) in
    open_ output module_type_n;
    module_t_out output parent;
    name_out output name;
    close output module_type_n

let type_t_out output typ =
  let parent = Type.parent typ in
  let name = Type.Name.to_string (Type.name typ) in
    open_ output type_n;
    module_t_out output parent;
    name_out output name;
    close output type_n

let value_t_out output v =
  let parent = Value.parent v in
  let name = Value.Name.to_string (Value.name v) in
    open_ output val_n;
    module_t_out output parent;
    name_out output name;
    close output val_n

let reference_out output = function
  | Module md -> module_t_out output md
  | ModuleType mty -> module_type_t_out output mty
  | Type typ -> type_t_out output typ
  | Val v -> value_t_out output v

let rec text_element_out output = function
  | Raw s -> data output s
  | Code s ->
      open_ output code_n;
      string_out output s;
      close output code_n
  | Style(Bold, txt) ->
      open_ output bold_n;
      list text_element_out output txt;
      close output bold_n
  | Style(Italic, txt) ->
      open_ output italic_n;
      list text_element_out output txt;
      close output italic_n
  | Style(Emphasize, txt) ->
      open_ output emph_n;
      list text_element_out output txt;
      close output emph_n
  | Style(Center, txt) ->
      open_ output center_n;
      list text_element_out output txt;
      close output center_n
  | Style(Left, txt) ->
      open_ output left_n;
      list text_element_out output txt;
      close output left_n
  | Style(Right, txt) ->
      open_ output right_n;
      list text_element_out output txt;
      close output right_n
  | Style(Superscript, txt) ->
      open_ output superscript_n;
      list text_element_out output txt;
      close output superscript_n
  | Style(Subscript, txt) ->
      open_ output subscript_n;
      list text_element_out output txt;
      close output subscript_n
  | List items ->
      open_ output list_n;
      list item_out output items;
      close output list_n
  | Enum items ->
      open_ output enum_n;
      list item_out output items;
      close output enum_n
  | Newline ->
      open_ output newline_n;
      close output newline_n
  | Title txt ->
      open_ output title_n;
      list text_element_out output txt;
      close output title_n
  | Ref(rf, txto) ->
      open_ output ref_n;
      reference_out output rf;
      opt (list text_element_out) output txto;
      close output ref_n

and item_out output txt =
  open_ output item_n;
  list text_element_out output txt;
  close output item_n

let doc_out output {info} =
  match info with
  | [] -> ()
  | l ->
      open_ output doc_n;
      list text_element_out output info;
      close output doc_n

let type_path_out output = function
  | Known path ->
      open_ output path_n;
      type_t_out output path;
      close output path_n
  | Unknown str -> name_out output str

let module_type_path_out output : module_type_path -> unit = function
  | Known path ->
      open_ output path_n;
      module_type_t_out output path;
      close output path_n
  | Unknown str -> name_out output str

let module_path_out output : module_path -> unit = function
  | Known path ->
      open_ output path_n;
      module_t_out output path;
      close output path_n
  | Unknown str -> name_out output str

let label_out output = function
  | Label s ->
      open_ output label_n;
      data output s;
      close output label_n
  | Default s ->
      open_ output default_n;
      data output s;
      close output default_n

let rec type_expr_out output = function
  | Var v ->
      open_ output var_n;
      data output v;
      close output var_n
  | Alias(typ, v) ->
      open_ output alias_n;
      type_expr_out output typ;
      open_ output var_n;
      data output v;
      close output var_n;
      close output alias_n
  | Arrow(lbl, arg, ret) ->
      open_ output arrow_n;
      opt label_out output lbl;
      type_expr_out output arg;
      type_expr_out output ret;
      close output arrow_n
  | Tuple typs ->
      open_ output tuple_n;
      list type_expr_out output typs;
      close output tuple_n
  | Constr(path, typs) ->
      open_ output constr_n;
      type_path_out output path;
      list type_expr_out output typs;
      close output constr_n

let val_out output {name; doc; type_} =
  open_ output val_n;
  name_out output (Value.Name.to_string name);
  doc_out output doc;
  type_expr_out output type_;
  close output val_n

let field_out output ({name; doc; type_} : field) =
  open_ output field_n;
  name_out output (Field.Name.to_string name);
  doc_out output doc;
  type_expr_out output type_;
  close output field_n

let ret_out output typ =
  open_ output return_n;
  type_expr_out output typ;
  close output return_n

let arg_out output typ =
  open_ output arg_n;
  type_expr_out output typ;
  close output arg_n

let constructor_out output {name; doc; args; ret;} =
  open_ output constructor_n;
  name_out output (Constructor.Name.to_string name);
  doc_out output doc;
  list arg_out output args;
  opt ret_out output ret;
  close output constructor_n

let type_kind_out output = function
  | None -> ()
  | Some (Variant cstrs) ->
      open_ output variant_n;
      list constructor_out output cstrs;
      close output variant_n
  | Some (Record fields) ->
      open_ output record_n;
      list field_out output fields;
      close output record_n

let manifest_out output typ =
  open_ output manifest_n;
  type_expr_out output typ;
  close output manifest_n

let param_out output v =
  open_ output param_n;
  data output v;
  close output param_n

let type_decl_out output {name; doc; param; manifest; decl} =
  open_ output type_n;
  name_out output (Type.Name.to_string name);
  doc_out output doc;
  list param_out output param;
  opt manifest_out output manifest;
  type_kind_out output decl;
  close output type_n

let nested_module_type_out output {name; doc; desc} =
  let module_type_desc_out output = function
    | Abstract -> ()
    | Manifest (Path path) -> module_type_path_out output path
    | Manifest Signature ->
        open_ output signature_n;
        close output signature_n
  in
    open_ output module_type_n;
    name_out output (ModuleType.Name.to_string name);
    doc_out output doc;
    module_type_desc_out output desc;
    close output module_type_n

let nested_module_out output ({name; doc; desc} : nested_module) =
 let module_desc_out output : nested_module_desc -> unit = function
   | Alias path ->
       open_ output alias_n;
       module_path_out output path;
       close output alias_n
   | Type (Path path) -> module_type_path_out output path
   | Type Signature ->
       open_ output signature_n;
       close output signature_n
 in
   open_ output module_n;
   name_out output (Module.Name.to_string name);
   doc_out output doc;
   module_desc_out output desc;
   close output module_n

let signature_item_out output : signature_item -> unit = function
  | Val v -> val_out output v
  | Types [t] -> type_decl_out output t
  | Types ts ->
      open_ output types_n;
      list type_decl_out output ts;
      close output types_n
  | Modules [md] -> nested_module_out output md
  | Modules mds ->
      open_ output modules_n;
      list nested_module_out output mds;
      close output modules_n
  | ModuleType mtd -> nested_module_type_out output mtd
  | Comment {info} ->
      open_ output comment_n;
      list text_element_out output info;
      close output comment_n


let module_type_expr_out output (Signature sg : module_type_expr) =
    open_ output signature_n;
    list signature_item_out output sg;
    close output signature_n

let module_alias_out output path =
    open_ output alias_n;
    module_path_out output path;
    close output alias_n

let module_type_out output { path; doc; alias; expr; } =
  open_ output module_type_n;
  open_ output path_n;
  module_type_t_out output path;
  close output path_n;
  doc_out output doc;
  opt module_type_path_out output alias;
  opt module_type_expr_out output expr;
  close output module_type_n

let module_out output { path; doc; alias; type_path; type_; } =
  open_ output module_n;
  open_ output path_n;
  module_t_out output path;
  close output path_n;
  doc_out output doc;
  opt module_alias_out output alias;
  opt module_type_path_out output type_path;
  opt module_type_expr_out output type_;
  close output module_n

let library_out output { path; modules; } =
  let mod_out output name =
    open_ output module_n;
    name_out output (Module.Name.to_string name);
    close output module_n;
  in
    open_ output library_n;
    library_t_out output path;
    list mod_out output modules;
    close output library_n

let package_out output { path; libraries; } =
  let lib_out output name =
    open_ output library_n;
    name_out output (Library.Name.to_string name);
    close output library_n;
  in
    open_ output package_n;
    package_t_out output path;
    list lib_out output libraries;
    close output package_n

let file p output x =
  dtd output None;
  p output x

let module_to_xml output md = file module_out output md

let module_type_to_xml output mty = file module_type_out output mty

let library_to_xml output lib = file library_out output lib

let package_to_xml output pkg = file package_out output pkg
