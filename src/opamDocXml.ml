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

module Name = OpamDocName

type input = {
  source : string option;
  input  : Xmlm.input;
}

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
let private_n: Xmlm.name = ("","private")
let manifest_n: Xmlm.name = ("","manifest")
let variant_n: Xmlm.name = ("","variant")
let record_n: Xmlm.name = ("","record")
let extensible_n: Xmlm.name =("","extensible")
let constructor_n: Xmlm.name = ("","constructor")
let field_n: Xmlm.name = ("","field")
let arg_n: Xmlm.name = ("","arg")
let return_n: Xmlm.name = ("","return")

let extension_n: Xmlm.name =("","extension")

let exn_n: Xmlm.name = ("","exn")

let class_n : Xmlm.name =("","class")

let val_n: Xmlm.name = ("","val")

let var_n: Xmlm.name = ("","var")
let arrow_n: Xmlm.name = ("","arrow")
let tuple_n: Xmlm.name = ("","tuple")
let constr_n: Xmlm.name = ("","constr")
let object_n: Xmlm.name = ("","object")
let poly_n: Xmlm.name = ("","poly")

let label_n: Xmlm.name = ("","label")
let default_n: Xmlm.name = ("","default")

let fixed_n: Xmlm.name = ("","fixed")
let closed_n: Xmlm.name = ("","closed")
let open_n: Xmlm.name = ("","open")

let method_n: Xmlm.name = ("","method")

let path_n: Xmlm.name = ("","path")
let name_n: Xmlm.name = ("","name")

let comment_n: Xmlm.name = ("","comment")
let doc_n: Xmlm.name = ("","doc")
let info_n: Xmlm.name = ("","info")
let code_n: Xmlm.name = ("","code")
let precode_n: Xmlm.name = ("", "precode")
let verbatim_n: Xmlm.name = ("", "verbatim")
let bold_n: Xmlm.name = ("","bold")
let italic_n: Xmlm.name = ("","italic")
let emph_n: Xmlm.name = ("","emph")
let center_n: Xmlm.name = ("","center")
let left_n: Xmlm.name = ("","left")
let right_n: Xmlm.name = ("","right")
let superscript_n: Xmlm.name = ("","superscript")
let subscript_n: Xmlm.name = ("","subscript")
let custom_n: Xmlm.name = ("","custom")
let style_n: Xmlm.name = ("","style")

let list_n: Xmlm.name = ("","list")
let enum_n: Xmlm.name = ("","enum")
let newline_n: Xmlm.name = ("","newline")
let block_n: Xmlm.name = ("","block")
let item_n: Xmlm.name = ("","item")
let title_n: Xmlm.name = ("","title")
let ref_n: Xmlm.name = ("","ref")
let level_n: Xmlm.name = ("","level")
let label_n: Xmlm.name = ("","label")
let target_n: Xmlm.name = ("","target")
let link_n: Xmlm.name = ("","link")

let author_n: Xmlm.name = ("","author")
let version_n: Xmlm.name = ("","version")
let see_n: Xmlm.name = ("","see")
let since_n: Xmlm.name = ("","since")
let before_n: Xmlm.name = ("","before")
let deprecated_n: Xmlm.name = ("","deprecated")
let param_n: Xmlm.name = ("","param")
let raise_n: Xmlm.name = ("","raise")
let return_n: Xmlm.name = ("","return")
let id_n: Xmlm.name = ("","id")

let tag_n: Xmlm.name = ("","tag")

let url_n: Xmlm.name = ("","url")
let file_n: Xmlm.name = ("","file")
let doc_n: Xmlm.name = ("","doc")

let todo_n: Xmlm.name = ("","todo")

(* XML parser combinators *)

module Parser : sig

  (* Expose functional nature of parser to allow eta expansion *)
  type 'a t = 'a contra -> 'a co
  and 'a contra and 'a co

  val pure: 'a -> 'a t

  type open_ = Open

  val open_: Xmlm.name -> (open_ * Xmlm.attribute list) t

  type close = Close

  val close: Xmlm.name -> close t

  val data: string t

  val dtd: Xmlm.dtd t

  val (@@): 'a t -> 'a t -> 'a t

  val (%): ('a -> 'b) t -> 'a t -> 'b t

  val (!!): 'a -> 'a t

  val opt: 'a t -> 'a option t

  val flag: Xmlm.name -> bool t

  val list: 'a t -> 'a list t

  val seq: 'a t -> 'a list t

  val opt_list: 'a list t -> 'a list t

  val run: 'a t -> input -> 'a

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
      | `El_start (n', attrs) when n = n' ->
          ignore (Xmlm.input input);
          Consumed (pure (Open, attrs))
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

  let on (Open, _) Close = true
  let off = false
  let flag n = !!on %(open_ n) %(close n) @@ !!off

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

  let string_of_source = function
    | None -> "?"
    | Some p -> p

  let show_error source (line, column) msg =
    let s = string_of_source source in
    OpamGlobals.error_and_exit "Parse error:<%s>.%d.%d: %s" s line column msg

  let expected_error ({ input; source }) e =
    let expected = String.concat " or " (List.map expected_msg e) in
    let pos = Xmlm.pos input in
    if Xmlm.eoi input then begin
      let msg =
        Printf.sprintf "expected %s but found end of file" expected
      in
      show_error source pos msg
    end else begin
      let found = found_msg (Xmlm.peek input) in
      let msg =
        Printf.sprintf "expected %s but found %s" expected found
      in
      show_error source pos msg
    end

  let xmlm_error source pos err =
    let msg = Xmlm.error_message err in
    show_error source pos msg

  let rec run p input =
    match p input.input with
    | Empty(Ok(a, _)) -> a
    | Consumed p -> run p input
    | Empty(Error e) -> expected_error input e
    | exception (Xmlm.Error(pos, err)) -> xmlm_error input.source pos err

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

let int_in =
  let action = function
    | None   -> 0
    | Some i -> try int_of_string i with Failure _ -> 0
  in
  Parser.( !!action %(opt data) )

let name_in =
  let action (Open, _) s Close = s in
  Parser.( !!action %(open_ name_n) %data %(close name_n) )

let package_t_in =
  let action (Open, _) name Close = Package.of_string name in
  Parser.( !!action %(open_ package_n) %name_in %(close package_n) )

let library_t_in =
  let action (Open, _) package name Close =
    let name = Name.Library.of_string name in
    Library.create package name
  in
  Parser.( !!action %(open_ library_n)
           %package_t_in %name_in
           %(close library_n) )

let rec module_t_in input =
  let action (Open, _) parent name Close =
    let name = Name.Module.of_string name in
    Module.create parent name
  in
  let parser =
    Parser.( !!action %(open_ module_n)
             %parent_in %name_in
             %(close module_n) )
  in
  parser input

and module_type_t_in input =
  let action (Open, _) parent name Close =
    let name = Name.ModuleType.of_string name in
    ModuleType.create parent name
  in
  let parser =
    let open Parser in
      !!action %(open_ module_type_n) %parent_in %name_in %(close module_type_n)
  in
    parser input

and parent_in input =
  let library lib : parent = Lib lib in
  let module_ md : parent = Module md in
  let modtype mty : parent = ModType mty in
  let parser =
    let open Parser in
    !!library %library_t_in
    @@ !!module_ %module_t_in
    @@ !!modtype %module_type_t_in
  in
  parser input

let type_t_in =
  let action (Open, _) parent name Close =
    let name = Name.Type.of_string name in
    Type.create parent name
  in
  let open Parser in
  !!action %(open_ type_n) %parent_in %name_in %(close type_n)

let value_t_in =
  let action (Open, _) parent name Close =
    let name = Name.Value.of_string name in
    Value.create parent name
  in
  let open Parser in
  !!action %(open_ val_n) %parent_in %name_in %(close val_n)

let see_ref_in =
  let url (Open, _) s Close = Documentation.See_url s in
  let file (Open, _) s Close = Documentation.See_file s in
  let doc (Open, _) s Close = Documentation.See_doc s in
  let open Parser in
  !!url %(open_ url_n) %string_in %(close url_n)
  @@ !!file %(open_ file_n) %string_in %(close file_n)
  @@ !!doc %(open_ doc_n) %string_in %(close doc_n)

let reference_in =
  let module_ path = Module path in
  let module_type path = ModuleType path in
  let type_ path = Type path in
  let val_ path = Val path in
  let link_ (Open, _) path Close = Link path in
  let open Parser in
  !!module_ %module_t_in
  @@ !!module_type %module_type_t_in
  @@ !!type_ %type_t_in
  @@ !!val_ %value_t_in
  @@ !!link_ %(open_ link_n) %string_in %(close link_n)

let rec text_element_in input =
  let raw s = Raw s in
  let code (Open, _) s Close = Code s in
  let precode (Open, _) s Close = PreCode s in
  let verbatim (Open, _) s Close = Verbatim s in
  let bold (Open, _) txt Close = Style(Bold, txt) in
  let italic (Open, _) txt Close = Style(Italic, txt) in
  let emph (Open, _) txt Close = Style(Emphasize, txt) in
  let center (Open, _) txt Close = Style(Center, txt) in
  let left (Open, _) txt Close = Style(Left, txt) in
  let right (Open, _) txt Close = Style(Right, txt) in
  let super (Open, _) txt Close = Style(Superscript, txt) in
  let sub (Open, _) txt Close = Style(Subscript, txt) in
  let custom (Open, attrs) txt Close =
    let c = try List.assoc custom_n attrs with Not_found -> "?" in
    Style(Custom c, txt) in
  let list_ (Open, _) items Close = List items in
  let enum (Open, _) items Close = Enum items in
  let newline (Open, _) Close = Newline in
  let title (Open, attrs) txt Close =
    let level =
      try int_of_string (List.assoc level_n attrs)
      with Not_found | Failure _ -> 0
    in
    let label = try Some (List.assoc label_n attrs) with Not_found -> None in
    Title (level, label, txt)
  in
  let reference (Open, _) rf txto Close = Ref(rf, txto) in
  let target (Open, attrs) code Close =
    let target = try Some (List.assoc target_n attrs) with Not_found -> None in
    Target (target, code)
  in
  let todo (Open, _) msg Close = TEXT_todo msg in
  let parser =
    let open Parser in
    !!raw %data
    @@ !!code %(open_ code_n) %string_in %(close code_n)
    @@ !!precode %(open_ precode_n) %string_in %(close precode_n)
    @@ !!verbatim %(open_ verbatim_n) %string_in %(close verbatim_n)
    @@ !!bold %(open_ bold_n) %text_in %(close bold_n)
    @@ !!italic %(open_ italic_n) %text_in %(close italic_n)
    @@ !!emph %(open_ emph_n) %text_in %(close emph_n)
    @@ !!center %(open_ center_n) %text_in %(close center_n)
    @@ !!left %(open_ left_n) %text_in %(close left_n)
    @@ !!right %(open_ right_n) %text_in %(close right_n)
    @@ !!super %(open_ superscript_n) %text_in %(close superscript_n)
    @@ !!sub %(open_ subscript_n) %text_in %(close subscript_n)
    @@ !!custom %(open_ style_n) %text_in %(close style_n)
    @@ !!list_ %(open_ list_n) %(list item_in) %(close list_n)
    @@ !!enum %(open_ enum_n) %(list item_in) %(close enum_n)
    @@ !!newline %(open_ newline_n) %(close newline_n)
    @@ !!title %(open_ title_n) %text_in %(close title_n)
    @@ !!reference %(open_ ref_n) %reference_in %(opt text_in) %(close ref_n)
    @@ !!target %(open_ target_n) %string_in %(close target_n)
    @@ !!todo %(open_ todo_n) %string_in %(close todo_n)
  in
  parser input

and text_in input = Parser.list text_element_in input

and item_in input =
  let action (Open, _) txt Close = txt in
  let parser =
    let open Parser in
    !!action %(open_ item_n) %text_in %(close item_n)
  in
  parser input

let with_tag_in tag_n =
  let action (Open, _) t Close = t in
  Parser.(!!action %(open_ tag_n) %string_in %(close tag_n))

let version_in = with_tag_in version_n
let id_in = with_tag_in id_n
let exn_name_in = with_tag_in exn_n

let tag_in =
  let author (Open, _) a Close = Author a in
  let version (Open, _) v Close = Version v in
  let see (Open, _) r t Close = See (r, t) in
  let since (Open, _) s Close = Since s in
  let before (Open, _) s t Close = Before (s, t) in
  let deprecated (Open, _) t Close = Deprecated t in
  let param (Open, _) s t Close = Param (s, t) in
  let raise_ (Open, _) s t Close = Raise (s, t) in
  let return (Open, _) t Close = Return t in
  let custom (Open, _) s t Close = Tag (s, t) in
  let open Parser in
  !!author %(open_ author_n) %string_in %(close author_n)
  @@ !!deprecated %(open_ deprecated_n) %text_in %(close since_n)
  @@ !!param %(open_ param_n) %id_in %text_in %(close param_n)
  @@ !!raise_ %(open_ raise_n) %exn_name_in %text_in %(close raise_n)
  @@ !!return %(open_ return_n) %text_in %(close return_n)
  @@ !!version %(open_ version_n) %string_in %(close version_n)
  @@ !!see %(open_ see_n) %see_ref_in %text_in %(close see_n)
  @@ !!since %(open_ since_n) %string_in %(close since_n)
  @@ !!before %(open_ before_n) %version_in %text_in %(close since_n)
  @@ !!custom %(open_ tag_n) %name_in %text_in %(close tag_n)

let tags_in = Parser.list tag_in

let doc_in =
  let none = { info = []; tags = []; } in
  let doc (Open, attrs) info tags Close = {info; tags} in
  let open Parser in
  !!none
  @@ !!doc %(open_ doc_n) %text_in %tags_in %(close doc_n)

let type_path_in =
  let known (Open, _) path Close = Known path in
  let unknown string = Unknown string in
  let open Parser in
  !!known %(open_ path_n) %type_t_in %(close path_n)
  @@ !!unknown %name_in

let module_type_path_in =
  let known (Open, _) path Close : module_type_path = Known path in
  let unknown string : module_type_path = Unknown string in
  let open Parser in
  !!known %(open_ path_n) %module_type_t_in %(close path_n)
  @@ !!unknown %name_in

let module_path_in =
  let known (Open, _) path Close : module_path = Known path in
  let unknown string : module_path = Unknown string in
  let open Parser in
  !!known %(open_ path_n) %module_t_in %(close path_n)
  @@ !!unknown %name_in

let label_in =
  let label (Open, _) string Close = Label string in
  let default (Open, _) string Close = Default string in
  let open Parser in
  !!label %(open_ label_n) %data %(close label_n)
  @@ !!default %(open_ default_n) %data %(close default_n)

let variant_kind_in =
  let fixed (Open, _) Close = Fixed in
  let closed (Open, _) names Close = Closed names in
  let openk (Open, _) Close = OpamDocTypes.Open in
    Parser.( !!fixed %(open_ fixed_n) %(close fixed_n)
             @@ !!closed %(open_ closed_n) %(list name_in) %(close closed_n)
             @@ !!openk %(open_ open_n) %(close open_n) )

let var_in =
  let action (Open, _) string Close = string in
    Parser.( !!action %(open_ var_n) %data %(close var_n) )

let rec type_expr_in input =
  let var string = Var string in
  let alias (Open, _) typ (Open, _) string Close Close = Alias(typ, string) in
  let arrow (Open, _) lbl arg ret Close = Arrow(lbl, arg, ret) in
  let tuple (Open, _) typs Close = Tuple typs in
  let constr (Open, _) path typs Close = Constr(path, typs) in
  let variant (Open, _) kind elements Close = Variant {kind; elements} in
  let object_ (Open, attrs) methods Close =
    let open_ =
      try bool_of_string (List.assoc open_n attrs)
      with Not_found | Failure _ -> false
    in
      Object {methods; open_}
  in
  let class_ (Open, _) path typs Close = Class(path, typs) in
  let poly (Open, _) vars typ Close = Poly(vars, typ) in
  let todo (Open, _) msg Close = TYPE_EXPR_todo msg in
  let parser =
    let open Parser in
    !!var %var_in
    @@ !!alias %(open_ alias_n) %type_expr_in %(open_ var_n) %data
       %(close var_n) %(close alias_n)
    @@ !!arrow %(open_ arrow_n) %(opt label_in) %type_expr_in %type_expr_in
       %(close arrow_n)
    @@ !!tuple %(open_ tuple_n) %(list type_expr_in) %(close tuple_n)
    @@ !!constr %(open_ constr_n) %type_path_in %(list type_expr_in)
       %(close constr_n)
    @@ !!variant %(open_ variant_n) %variant_kind_in
       %(list variant_element_in) %(close variant_n)
    @@ !!object_ %(open_ object_n) %(list method_in) %(close object_n)
    @@ !!class_ %(open_ class_n) %type_path_in %(list type_expr_in) %(close class_n)
    @@ !!poly %(open_ poly_n) %(list var_in) %type_expr_in %(close poly_n)
    @@ !!todo %(open_ todo_n) %string_in %(close todo_n)
  in
  parser input

and variant_element_in input =
  let type_ (Open, _) path typs Close : variant_element = Type(path, typs) in
  let constructor (Open, _) name typs Close = Constructor(name, typs) in
  let parser =
    Parser.( !!type_ %(open_ type_n) %type_path_in
                     %(list type_expr_in) %(close type_n)
              @@ !!constructor %(open_ constructor_n) %name_in
                     %(list (opt type_expr_in)) %(close constructor_n) )
  in
  parser input

and method_in input =
  let action (Open, _) name type_ Close = {name; type_} in
  let parser =
    Parser.( !!action %(open_ method_n) %name_in
                      %type_expr_in %(close method_n) )
  in
  parser input

let val_in =
  let action (Open, _) name doc type_ Close: val_ =
    {name = Name.Value.of_string name; doc; type_}
  in
  let open Parser in
  !!action %(open_ val_n) %name_in %doc_in %type_expr_in %(close val_n)

let field_in =
  let action (Open, _) name doc type_ Close : field =
    {name = Name.Field.of_string name; doc; type_}
  in
  let open Parser in
  !!action %(open_ field_n) %name_in %doc_in %type_expr_in %(close field_n)

let ret_in =
  let action (Open, _) typ Close = typ in
  Parser.( !!action %(open_ return_n) %type_expr_in %(close return_n) )

let arg_in =
  let action (Open, _) typ Close = typ in
  Parser.( !!action %(open_ arg_n) %type_expr_in %(close arg_n) )

let constructor_in =
  let action (Open, _) name doc args ret Close: constructor =
    {name = Name.Constructor.of_string name; doc; args; ret;}
  in
  let open Parser in
  !!action %(open_ constructor_n) %name_in %doc_in %(list arg_in) %(opt ret_in)
  %(close constructor_n)

let type_kind_in =
  let abstract = None in
  let variant (Open, _) cstrs Close : type_decl option = Some (Variant cstrs) in
  let record (Open, _) fields Close = Some (Record fields) in
  let extensible (Open, _) Close = Some Extensible in
  let open Parser in
  !!abstract
  @@ !!variant %(open_ variant_n) %(seq constructor_in) %(close variant_n)
  @@ !!record %(open_ record_n) %(seq field_in) %(close record_n)
  @@ !!extensible %(open_ extensible_n) %(close extensible_n)

let manifest_in =
  let action (Open, _) typ Close = typ in
  Parser.( !!action %(open_ manifest_n) %type_expr_in %(close manifest_n) )

let param_in =
  let action (Open, _) string Close = string in
  Parser.( !!action %(open_ param_n) %data %(close param_n) )

let type_decl_in =
  let action (Open, _) name doc param private_ manifest decl Close =
    {name = Name.Type.of_string name; doc; param; private_; manifest; decl}
  in
  let open Parser in
  !!action %(open_ type_n) %name_in %doc_in %(list param_in) %(flag private_n)
           %(opt manifest_in) %type_kind_in %(close type_n)

let type_ext_in =
  let action (Open, _) type_path doc type_params private_ constructors Close =
    {type_path; doc; type_params; private_; constructors}
  in
  let open Parser in
  !!action %(open_ extension_n) %type_path_in %doc_in %(list param_in) %(flag private_n)
  %(list constructor_in) %(close extension_n)

let rec signature_item_in input =
  let val_ v : signature_item = Val v in
  let type_ t = Types [t] in
  let ext ext = TypExt ext in
  let exn_ (Open, _) constr Close = Exn constr in
  let types (Open, _) ts Close = Types ts in
  let module_ md = Modules [md] in
  let modules (Open, _) mds Close = Modules mds in
  let module_type mtd : signature_item = ModuleType mtd in
  let comment (Open, _) info tags Close = Comment {info; tags} in
  let todo (Open, _) msg Close = SIG_todo msg in
  let open Parser in
  let parser =
    !!val_ %val_in
    @@ !!type_ %type_decl_in
    @@ !!types %(open_ types_n) %(seq type_decl_in) %(close types_n)
    @@ !!ext %type_ext_in
    @@ !!exn_ %(open_ exn_n) %constructor_in %(close exn_n)
    @@ !!module_ %module_in
    @@ !!modules %(open_ modules_n) %(seq module_in) %(close modules_n)
    @@ !!module_type %module_type_in
    @@ !!comment %(open_ comment_n) %text_in %tags_in %(close comment_n)
    @@ !!todo %(open_ todo_n) %string_in %(close todo_n)
  in
    parser input

and module_type_expr_in input =
  let sg (Open, _) sg Close : module_type_expr = Signature sg in
  let todo (Open, _) msg Close = MODULE_TYPE_EXPR_todo msg in
  let open Parser in
  let parser =
    !!sg %(open_ signature_n) %(list signature_item_in) %(close signature_n)
    @@ !!todo %(open_ todo_n) %string_in %(close todo_n)
  in
    parser input

and module_alias_in input =
  let action (Open, _) path Close = path in
  let open Parser in
  let parser =
    !!action %(open_ alias_n) %module_path_in %(close alias_n)
  in
    parser input

and module_type_in input =
  let action (Open, _) (Open, _) path Close doc alias expr Close =
    { path; doc; alias; expr; }
  in
  let open Parser in
  let parser =
    !!action %(open_ module_type_n) %(open_ path_n) %module_type_t_in
    %(close path_n) %doc_in %(opt module_type_path_in) %(opt module_type_expr_in)
    %(close module_type_n)
  in
    parser input

and module_in input =
  let action (Open, _) (Open, _) path Close doc alias type_path type_ Close =
    { path; doc; alias; type_path; type_; }
  in
  let open Parser in
  let parser =
    !!action %(open_ module_n) %(open_ path_n) %module_t_in %(close path_n)
    %doc_in %(opt module_alias_in) %(opt module_type_path_in)
    %(opt module_type_expr_in) %(close module_n)
  in
    parser input

let library_in =
  let action (Open, _) path modules Close = { path; modules; } in
  let modl (Open, _) name Close = Name.Module.of_string name in
  let open Parser in
  !!action %(open_ library_n) %library_t_in %(list (
      !!modl %(open_ module_n) %name_in  %(close module_n))
    ) %(close library_n)

let package_in =
  let action (Open, _) path libraries Close = { path; libraries; } in
  let lib (Open, _) name Close = Name.Library.of_string name in
  let open Parser in
  !!action %(open_ package_n) %package_t_in %(list (
      !!lib %(open_ library_n) %name_in  %(close library_n))
    ) %(close package_n)

let file p =
  let action dtd x = x in
  Parser.( !!action %dtd %p )

let module_of_xml input = Parser.run (file module_in) input

let module_type_of_xml input = Parser.run (file module_type_in) input

let library_of_xml input = Parser.run (file library_in) input

let package_of_xml input = Parser.run (file package_in) input

(* XML printer utilities *)

let open_ ?(attrs=[]) output n =
  Xmlm.output output (`El_start (n, attrs))

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

let flag n output b =
  if b then begin
    open_ output n;
    close output n
  end

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

let int_out output i =
  if i = 0 then ()
  else data output (string_of_int i)

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
  let name = Name.Library.to_string (Library.name lib) in
  open_ output library_n;
  package_t_out output package;
  name_out output name;
  close output library_n

let rec module_t_out output md =
  let parent = Module.parent md in
  let name = Name.Module.to_string (Module.name md) in
  open_ output module_n;
  parent_out output parent;
  name_out output name;
  close output module_n

and module_type_t_out output mty =
  let parent = ModuleType.parent mty in
  let name = Name.ModuleType.to_string (ModuleType.name mty) in
  open_ output module_type_n;
  parent_out output parent;
  name_out output name;
  close output module_type_n

and parent_out output = function
  | Lib lib -> library_t_out output lib
  | Module md -> module_t_out output md
  | ModType mty -> module_type_t_out output mty

let type_t_out output typ =
  let parent = Type.parent typ in
  let name = Name.Type.to_string (Type.name typ) in
  open_ output type_n;
  parent_out output parent;
  name_out output name;
  close output type_n

let value_t_out output v =
  let parent = Value.parent v in
  let name = Name.Value.to_string (Value.name v) in
  open_ output val_n;
  parent_out output parent;
  name_out output name;
  close output val_n

let link_t_out output href =
  open_ output link_n;
  string_out output href;
  close output link_n

let reference_out output = function
  | Module md -> module_t_out output md
  | ModuleType mty -> module_type_t_out output mty
  | Type typ -> type_t_out output typ
  | Val v -> value_t_out output v
  | Link s -> link_t_out output s

let todo_out output msg =
  open_ output todo_n;
  string_out output msg;
  close output todo_n

let rec text_element_out output = function
  | Raw s -> data output s
  | Code s ->
      open_ output code_n;
      string_out output s;
      close output code_n
  | PreCode s ->
      open_ output precode_n;
      string_out output s;
      close output precode_n
  | Verbatim s ->
      open_ output verbatim_n;
      string_out output s;
      close output verbatim_n
  | Style(Bold, txt) ->
      open_ output bold_n;
      text_out output txt;
      close output bold_n
  | Style(Italic, txt) ->
      open_ output italic_n;
      text_out output txt;
      close output italic_n
  | Style(Emphasize, txt) ->
      open_ output emph_n;
      text_out output txt;
      close output emph_n
  | Style(Center, txt) ->
      open_ output center_n;
      text_out output txt;
      close output center_n
  | Style(Left, txt) ->
      open_ output left_n;
      text_out output txt;
      close output left_n
  | Style(Right, txt) ->
      open_ output right_n;
      text_out output txt;
      close output right_n
  | Style(Superscript, txt) ->
      open_ output superscript_n;
      text_out output txt;
      close output superscript_n
  | Style(Subscript, txt) ->
      open_ output subscript_n;
      text_out output txt;
      close output subscript_n
  | Style (Custom c, txt) ->
      open_ output ~attrs:[custom_n, c] style_n;
      text_out output txt;
      close output style_n
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
  | Title (level, label, txt) ->
      let attrs =
        (level_n, string_of_int level)
        :: match label with None -> [] | Some l -> [label_n, l]
      in
      open_ ~attrs output title_n;
      text_out output txt;
      close output title_n
  | Ref(rf, txto) ->
      open_ output ref_n;
      reference_out output rf;
      opt text_out output txto;
      close output ref_n
  | Target (target, code) ->
      let attrs = match target with
        | None   -> []
        | Some t -> [target_n, t]
      in
      open_ output ~attrs target_n;
      string_out output code;
      close output target_n
  | TEXT_todo msg -> todo_out output msg

and text_out output = list text_element_out output

and item_out output txt =
  open_ output item_n;
  text_out output txt;
  close output item_n

let see_ref_out output x =
  let open Documentation in
  let out n s =
    open_ output n;
    string_out output s;
    close output n
  in
  match x with
  | See_url s -> out url_n s
  | See_file s -> out file_n s
  | See_doc s -> out doc_n s

let with_tag_out tag_n output s =
  open_ output tag_n;
  string_out output s;
  close output tag_n

let id_out = with_tag_out id_n
let version_out = with_tag_out version_n
let exn_name_out = with_tag_out exn_n

let tag_out output = function
  | Author s ->
      open_ output author_n;
      string_out output s;
      close output author_n
  | Deprecated t ->
      open_ output deprecated_n;
      text_out output t;
      close output deprecated_n
  | Param (s, t) ->
      open_ output param_n;
      id_out output s;
      text_out output t;
      close output param_n
  | Raise (s, t) ->
      open_ output raise_n;
      exn_name_out output s;
      text_out output t;
      close output raise_n
  | Return t ->
      open_ output return_n;
      text_out output t;
      close output return_n
  | See (r, t) ->
      open_ output see_n;
      see_ref_out output r;
      text_out output t;
      close output see_n
  | Since s ->
      open_ output since_n;
      string_out output s;
      close output since_n
  | Before (s, t) ->
      open_ output before_n;
      version_out output s;
      text_out output t;
      close output before_n
  | Version v -> version_out output v
  | Tag (s, t) ->
      open_ output tag_n;
      name_out output s;
      text_out output t;
      close output tag_n

let tags_out output = list tag_out output

let doc_out output {info; tags} =
  match info, tags with
  | [], [] -> ()
  | _ ->
      open_ output doc_n;
      text_out output info;
      tags_out output tags;
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

let variant_kind_out output = function
  | Fixed ->
      open_ output fixed_n;
      close output fixed_n
  | Closed names ->
      open_ output closed_n;
      list name_out output names;
      close output closed_n
  | Open ->
      open_ output open_n;
      close output open_n

let var_out output v =
  open_ output var_n;
  data output v;
  close output var_n

let rec type_expr_out output = function
  | Var v -> var_out output v
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
  | Variant {kind; elements} ->
      open_ output variant_n;
      variant_kind_out output kind;
      list variant_element_out output elements;
      close output variant_n
  | Object {methods; open_ = o} ->
      let attrs =
        [open_n, string_of_bool o]
      in
      open_ ~attrs output object_n;
      list method_out output methods;
      close output object_n
  | Class(path, typs) ->
      open_ output class_n;
      type_path_out output path;
      list type_expr_out output typs;
      close output class_n
  | Poly(vars, typ) ->
    open_ output poly_n;
    list var_out output vars;
    type_expr_out output typ;
    close output poly_n
  | TYPE_EXPR_todo msg -> todo_out output msg

and variant_element_out output = function
  | Type(path, typs) ->
      open_ output type_n;
      type_path_out output path;
      list type_expr_out output typs;
      close output type_n
  | Constructor(name, typs) ->
      open_ output constructor_n;
      name_out output name;
      list (opt type_expr_out) output typs;
      close output constructor_n

and method_out output {name; type_} =
  open_ output method_n;
  name_out output name;
  type_expr_out output type_;
  close output method_n

let val_out output ({name; doc; type_}: val_) =
  open_ output val_n;
  name_out output (Name.Value.to_string name);
  doc_out output doc;
  type_expr_out output type_;
  close output val_n

let field_out output ({name; doc; type_} : field) =
  open_ output field_n;
  name_out output (Name.Field.to_string name);
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

let constructor_out output ({name; doc; args; ret;}: constructor) =
  open_ output constructor_n;
  name_out output (Name.Constructor.to_string name);
  doc_out output doc;
  list arg_out output args;
  opt ret_out output ret;
  close output constructor_n

let type_kind_out output (tk : type_decl option) =
  match tk with
  | None -> ()
  | Some (Variant cstrs) ->
      open_ output variant_n;
      list constructor_out output cstrs;
      close output variant_n
  | Some (Record fields) ->
      open_ output record_n;
      list field_out output fields;
      close output record_n
  | Some Extensible ->
      open_ output extensible_n;
      close output extensible_n

let manifest_out output typ =
  open_ output manifest_n;
  type_expr_out output typ;
  close output manifest_n

let param_out output v =
  open_ output param_n;
  data output v;
  close output param_n

let type_decl_out output {name; doc; param; private_; manifest; decl} =
  open_ output type_n;
  name_out output (Name.Type.to_string name);
  doc_out output doc;
  list param_out output param;
  flag private_n output private_;
  opt manifest_out output manifest;
  type_kind_out output decl;
  close output type_n

let type_ext_out output {type_path; doc; type_params; private_; constructors} =
  open_ output extension_n;
  type_path_out output type_path;
  doc_out output doc;
  list param_out output type_params;
  flag private_n output private_;
  list constructor_out output constructors;
  close output extension_n

let rec signature_item_out output : signature_item -> unit = function
  | Val v -> val_out output v
  | Types [t] -> type_decl_out output t
  | Types ts ->
      open_ output types_n;
      list type_decl_out output ts;
      close output types_n
  | TypExt ext -> type_ext_out output ext
  | Exn constr ->
      open_ output exn_n;
      constructor_out output constr;
      close output exn_n
  | Modules [md] -> module_out output md
  | Modules mds ->
      open_ output modules_n;
      list module_out output mds;
      close output modules_n
  | ModuleType mtd -> module_type_out output mtd
  | Comment {info; tags} ->
      open_ output comment_n;
      text_out output info;
      tags_out output tags;
      close output comment_n
  | SIG_todo msg -> todo_out output msg

and module_type_expr_out output = function
  | Signature sg ->
      open_ output signature_n;
      list signature_item_out output sg;
      close output signature_n
  | MODULE_TYPE_EXPR_todo msg -> todo_out output msg

and module_alias_out output path =
  open_ output alias_n;
  module_path_out output path;
  close output alias_n

and module_type_out output { path; doc; alias; expr; } =
  open_ output module_type_n;
  open_ output path_n;
  module_type_t_out output path;
  close output path_n;
  doc_out output doc;
  opt module_type_path_out output alias;
  opt module_type_expr_out output expr;
  close output module_type_n

and module_out output { path; doc; alias; type_path; type_; } =
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
    name_out output (Name.Module.to_string name);
    close output module_n;
  in
  open_ output library_n;
  library_t_out output path;
  list mod_out output modules;
  close output library_n

let package_out output { path; libraries; } =
  let lib_out output name =
    open_ output library_n;
    name_out output (Name.Library.to_string name);
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
