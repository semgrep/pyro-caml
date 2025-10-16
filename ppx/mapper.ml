(* Austin Theriault
 *
 * Copyright (C) Semgrep, Inc.
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public License
 * version 2.1 as published by the Free Software Foundation, with the
 * special exception on linking described in file LICENSE.
 *
 * This library is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the file
 * LICENSE for more details.
 *)

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)

(* nosemgrep: no-ref-declarations-at-top-scope *)
let auto = ref false

open Ppxlib
open Ast_helper
open Asttypes
open Parsetree
open Location

let ppx_identifier = "pyro_profile"

let mkloc txt loc = {txt; loc}

let mknoloc txt = mkloc txt !Ast_helper.default_loc

let digest x = Digest.to_hex (Digest.string (Marshal.to_string x []))

let error loc code =
  let open Printf in
  let message = function
    | `Too_many_attributes ->
        "too many attributes"
    | `Expecting_payload l ->
        sprintf "expecting payload in [%s]"
          (* nosemgrep: no-list-map *)
          (String.concat "," (List.map (sprintf "\"%s\"") l))
    | `Payload_not_a_string ->
        "payload is not a string"
    | `Payload_not_an_expression ->
        "payload is not an expression"
    | `Provide_a_name ->
        "this pyro_profile annotation requires a name argument"
  in
  Location.Error.raise
    (Location.Error.make ~loc ~sub:[]
       (Printf.sprintf "ppx_pyro_caml: %s" (message code)) )

let has_name key {attr_name= {txt; _}; _} = txt = key

let remove_attribute key = List.filter (fun x -> not (has_name key x))

let has_attribute ?(auto = false) key l =
  if auto || List.exists (has_name key) l then Some (remove_attribute key l)
  else None

type pyro_profile = Constant of string | Dynamic of Parsetree.expression

let get_payload key = function
  | { attr_name= {txt; _}
    ; attr_payload=
        PStr
          [ { pstr_desc=
                Pstr_eval
                  ({pexp_desc= Pexp_constant (Pconst_string (x, _, None)); _}, _)
            ; _ } ]
    ; _ }
    when txt = key ->
      Some (Some (Constant x))
  | {attr_name= {txt; _}; attr_payload= PStr []; _} when txt = key ->
      Some None
  | _ ->
      None

let get_string_payload key ({attr_loc; _} as e) =
  match get_payload key e with
  | Some None ->
      Some None
  | Some (Some (Constant x)) ->
      Some (Some x)
  | Some (Some _) ->
      error attr_loc `Payload_not_a_string
  | None ->
      None

let has_identifier_attribute ?auto = has_attribute ?auto ppx_identifier

let payload_of_string x = PStr [Str.eval (Exp.constant (Const.string x))]

let var x loc = Exp.ident (mkloc (Longident.parse x) loc)

let string_of_loc (l : Location.t) =
  let file = l.loc_start.pos_fname in
  let line = l.loc_start.pos_lnum in
  Printf.sprintf "%s:%d" file line

let unit = Exp.construct (mknoloc (Longident.parse "()")) None

let raise_ident = "Stdlib.raise"

let enter_pyro_caml loc =
  let var e = var e loc in
  let enter = "Pyro_caml.enter" in
  let record_cs = "Printexc.get_callstack" in
  let max_int = "Pyro_caml.max_frames" in
  Exp.apply (var enter)
    [(Nolabel, Exp.apply (var record_cs) [(Nolabel, var max_int)])]

let exit_pyro_caml loc =
  let var e = var e loc in
  let exit_ = "Pyro_caml.exit_" in
  Exp.apply (var exit_) [(Nolabel, unit)]

let wrap_pyro_caml expr =
  let loc = expr.pexp_loc in
  let var e = var e loc in
  Exp.sequence (enter_pyro_caml loc)
    (Exp.let_ Nonrecursive
       [ Vb.mk
           (Pat.var (mknoloc "r"))
           (Exp.try_ expr
              [ Exp.case
                  (Pat.var (mknoloc "e"))
                  (Exp.sequence (exit_pyro_caml loc)
                     (Exp.apply (var raise_ident) [(Nolabel, var "e")]) ) ] ) ]
       (Exp.sequence (exit_pyro_caml loc) (var "r")) )

let rec wrap_pyro_caml_method ({pexp_desc; _} as expr) =
  match pexp_desc with
  | Pexp_fun (label, def, pat, e) ->
      {expr with pexp_desc= Pexp_fun (label, def, pat, wrap_pyro_caml_method e)}
  | Pexp_poly (e, typ) ->
      {expr with pexp_desc= Pexp_poly (wrap_pyro_caml_method e, typ)}
  | _ ->
      wrap_pyro_caml expr

let rec not_a_constant expr =
  match expr.pexp_desc with
  | Pexp_constant _ | Pexp_ident _ ->
      false
  | Pexp_coerce (e, _, _) | Pexp_poly (e, _) | Pexp_constraint (e, _) ->
      not_a_constant e
  | _ ->
      true

let rec name_of_pattern pat =
  match pat.ppat_desc with
  | Ppat_var {txt; _} ->
      Some txt
  | Ppat_constraint (pat, _) ->
      name_of_pattern pat
  | _ ->
      None

let rec translate_pvb_expr expr =
  match expr.pexp_desc with
  | Pexp_fun (arg_label, exp_opt, pattern, ({pexp_desc= Pexp_fun _; _} as e'))
    when e'.pexp_loc.loc_ghost ->
      let e' = translate_pvb_expr e' in
      {expr with pexp_desc= Pexp_fun (arg_label, exp_opt, pattern, e')}
  | Pexp_fun (arg_label, exp_opt, pattern, e') ->
      let body_with_profiling = wrap_pyro_caml e' in
      { expr with
        pexp_desc= Pexp_fun (arg_label, exp_opt, pattern, body_with_profiling)
      }
  | _ ->
      expr

let translate_value_bindings value_binding auto vbs =
  (* nosemgrep: no-list-map *)
  List.map
    (fun vb ->
      match has_identifier_attribute ~auto vb.pvb_attributes with
      | Some [] ->
          let pvb_expr = translate_pvb_expr vb.pvb_expr in
          { vb with
            pvb_expr
          ; pvb_attributes= remove_attribute ppx_identifier vb.pvb_attributes }
      | Some (_ :: _) ->
          error vb.pvb_loc `Too_many_attributes
      | None ->
          value_binding vb )
    vbs

let mapper =
  object (self)
    inherit [bool] Ast_traverse.fold_map as super

    method! module_binding ({pmb_name; _} as binding) (auto as acc) =
      let acc = match pmb_name.txt with None -> acc | Some _ -> auto in
      let result, _ = super#module_binding binding acc in
      (result, auto)

    method! structure l auto =
      let _, results =
        List.fold_left
          (fun (auto, acc) expr ->
            match expr with
            | {pstr_desc= Pstr_attribute attr; pstr_loc; _} as pstr -> (
              match get_string_payload ppx_identifier attr with
              | Some (Some "auto") ->
                  (true, acc)
              | Some (Some "auto-off") ->
                  (false, acc)
              | None ->
                  (auto, pstr :: acc)
              | _ ->
                  error pstr_loc (`Expecting_payload ["auto"; "auto-off"]) )
            | {pstr_desc= Pstr_value (rec_flag, vbs); pstr_loc} ->
                let value_binding vb = fst (self#value_binding vb auto) in
                let vbs = translate_value_bindings value_binding auto vbs in
                let str = Str.value ~loc:pstr_loc rec_flag vbs in
                (auto, str :: acc)
            | sti ->
                let sti, _ = super#structure_item sti auto in
                (auto, sti :: acc) )
          (auto, []) l
      in
      (List.rev results, auto)

    method! class_field class_field (auto as acc) =
      match class_field with
      | { pcf_desc= Pcf_method (loc, privat, Cfk_concrete (flag, expr))
        ; pcf_loc
        ; pcf_attributes
        ; _ } -> (
          let has_attr =
            match
              (* nosemgrep: no-list-filter-map *)
              (List.filter_map (get_payload ppx_identifier) pcf_attributes, auto)
            with
            | [None], _ | _, true ->
                Some (Constant loc.txt)
            | [], false ->
                None
            | [Some _], _ | _ :: _ :: _, _ ->
                error pcf_loc `Too_many_attributes
          in
          match has_attr with
          | None ->
              super#class_field class_field acc
          | Some _ ->
              let expr =
                wrap_pyro_caml_method (fst (self#expression expr acc))
              in
              ( { class_field with
                  pcf_desc= Pcf_method (loc, privat, Cfk_concrete (flag, expr))
                ; pcf_attributes= remove_attribute ppx_identifier pcf_attributes
                }
              , acc ) )
      | _ ->
          super#class_field class_field acc

    method! class_expr class_expr (_ as acc) =
      match class_expr with
      | {pcl_desc= Pcl_let (rec_flag, vbs, body); _} ->
          let vbs =
            let value_binding vb = fst (self#value_binding vb acc) in
            translate_value_bindings value_binding false vbs
          in
          let body, _ = self#class_expr body acc in
          ({class_expr with pcl_desc= Pcl_let (rec_flag, vbs, body)}, acc)
      | _ ->
          super#class_expr class_expr acc

    method! expression expr (_ as acc) =
      let expr =
        match expr with
        | {pexp_desc= Pexp_let (rec_flag, vbs, body); _} as expr ->
            let vbs =
              let value_binding vb = fst (self#value_binding vb acc) in
              translate_value_bindings value_binding false vbs
            in
            let body = fst (self#expression body acc) in
            {expr with pexp_desc= Pexp_let (rec_flag, vbs, body)}
        | expr ->
            fst (super#expression expr acc)
      in
      let {pexp_attributes; pexp_loc; _} = expr in
      (* nosemgrep: no-list-filter-map *)
      match List.filter_map (get_payload ppx_identifier) pexp_attributes with
      | [None] ->
          ( { expr with
              pexp_attributes= remove_attribute ppx_identifier pexp_attributes
            }
            |> wrap_pyro_caml
          , acc )
      | [] ->
          (expr, acc)
      | [Some _] | _ ->
          error pexp_loc `Too_many_attributes
  end

let remove_attributes =
  object
    inherit Ast_traverse.map as super

    method! structure l =
      let l =
        List.filter
          (function
            | {pstr_desc= Pstr_attribute attr; _}
              when has_identifier_attribute [attr] <> None ->
                false
            | _ ->
                true )
          l
      in
      super#structure l

    method! attributes attributes =
      super#attributes
        ( match has_identifier_attribute attributes with
        | Some attrs ->
            attrs
        | None ->
            attributes )
  end

let has_disable l =
  let disable = ref false in
  let f = function
    | {pstr_desc= Pstr_attribute attr; pstr_loc; _} as pstr -> (
      match get_string_payload ppx_identifier attr with
      | Some (Some "disable") ->
          disable := true ;
          None
      | Some (Some "auto-off") | Some (Some "auto") | None ->
          Some pstr
      | _ ->
          error pstr_loc (`Expecting_payload ["auto"; "auto-off"; "disable"]) )
    | i ->
        Some i
  in
  (* nosemgrep: no-list-filter-map *)
  let res = List.filter_map f l in
  (!disable, res)

let toplevel_mapper auto =
  object
    inherit Ast_traverse.map

    method! signature si = si

    method! structure l =
      match l with
      | [] ->
          []
      | l ->
          let disable, l = has_disable l in
          if disable then l
          else
            let l, _ = mapper#structure l auto in
            l
  end
