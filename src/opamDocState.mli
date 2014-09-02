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

type state

val load_state: OpamState.state -> OpamUnitsState.state -> state

val modules: state -> OpamDocPath.Module.Set.t

val module_types: state -> OpamDocPath.ModuleType.Set.t

val load_module: state -> OpamDocPath.Module.t -> OpamDocTypes.module_

val load_module_type: state -> OpamDocPath.ModuleType.t ->
  OpamDocTypes.module_type

val load_library: state -> OpamDocPath.Library.t -> OpamDocTypes.library

val load_package: state -> OpamDocPath.Package.t -> OpamDocTypes.package

