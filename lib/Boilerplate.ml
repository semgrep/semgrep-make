(**
   Boilerplate to be used as a template when mapping the make CST
   to another type of tree.
*)

module R = Tree_sitter_run.Raw_tree

(* Disable warnings against unused variables *)
[@@@warning "-26-27"]

(* Disable warning against unused 'rec' *)
[@@@warning "-39"]

type env = unit

let token (env : env) (tok : Tree_sitter_run.Token.t) =
  R.Token tok

let blank (env : env) () =
  R.Tuple []

let map_imm_tok_addp (env : env) (tok : CST.imm_tok_addp) =
  (* "addprefix" *) token env tok

let map_imm_tok_prec_p1_at (env : env) (tok : CST.imm_tok_prec_p1_at) =
  (* "@" *) token env tok

let map_imm_tok_d (env : env) (tok : CST.imm_tok_d) =
  (* "D" *) token env tok

let map_imm_tok_pats (env : env) (tok : CST.imm_tok_pats) =
  (* "patsubst" *) token env tok

let map_imm_tok_prec_p1_lt (env : env) (tok : CST.imm_tok_prec_p1_lt) =
  (* "<" *) token env tok

let map_imm_tok_file (env : env) (tok : CST.imm_tok_file) =
  (* "file" *) token env tok

let map_imm_tok_strip (env : env) (tok : CST.imm_tok_strip) =
  (* "strip" *) token env tok

let map_tok_rep1_choice_pat_549beab (env : env) (tok : CST.tok_rep1_choice_pat_549beab) =
  (* tok_rep1_choice_pat_549beab *) token env tok

let map_tok_rep1_choice_choice_pat_c610685 (env : env) (tok : CST.tok_rep1_choice_choice_pat_c610685) =
  (* tok_rep1_choice_choice_pat_c610685 *) token env tok

let map_rawline (env : env) (tok : CST.rawline) =
  (* rawline *) token env tok

let map_imm_tok_prec_p1_slash (env : env) (tok : CST.imm_tok_prec_p1_slash) =
  (* "/" *) token env tok

let map_imm_tok_filt (env : env) (tok : CST.imm_tok_filt) =
  (* "filter-out" *) token env tok

let map_tok_prec_p1_star (env : env) (tok : CST.tok_prec_p1_star) =
  (* tok_prec_p1_star *) token env tok

let map_imm_tok_word_ (env : env) (tok : CST.imm_tok_word_) =
  (* "wordlist" *) token env tok

let map_tok_prec_p1_lt (env : env) (tok : CST.tok_prec_p1_lt) =
  (* tok_prec_p1_lt *) token env tok

let map_imm_tok_origin (env : env) (tok : CST.imm_tok_origin) =
  (* "origin" *) token env tok

let map_imm_tok_prec_p1_plus (env : env) (tok : CST.imm_tok_prec_p1_plus) =
  (* "+" *) token env tok

let map_anon_choice_EQ_67cadb9 (env : env) (x : CST.anon_choice_EQ_67cadb9) =
  (match x with
  | `EQ tok -> R.Case ("EQ",
      (* "=" *) token env tok
    )
  | `COLONEQ tok -> R.Case ("COLONEQ",
      (* ":=" *) token env tok
    )
  | `COLONCOLONEQ tok -> R.Case ("COLONCOLONEQ",
      (* "::=" *) token env tok
    )
  | `QMARKEQ tok -> R.Case ("QMARKEQ",
      (* "?=" *) token env tok
    )
  | `PLUSEQ tok -> R.Case ("PLUSEQ",
      (* "+=" *) token env tok
    )
  )

let map_imm_tok_call (env : env) (tok : CST.imm_tok_call) =
  (* "call" *) token env tok

let map_imm_tok_words (env : env) (tok : CST.imm_tok_words) =
  (* "words" *) token env tok

let map_imm_tok_colon (env : env) (tok : CST.imm_tok_colon) =
  (* ":" *) token env tok

let map_imm_tok_base (env : env) (tok : CST.imm_tok_base) =
  (* "basename" *) token env tok

let map_tok_prec_p1_endef (env : env) (tok : CST.tok_prec_p1_endef) =
  (* tok_prec_p1_endef *) token env tok

let map_imm_tok_fore (env : env) (tok : CST.imm_tok_fore) =
  (* "foreach" *) token env tok

let map_imm_tok_prec_p1_star (env : env) (tok : CST.imm_tok_prec_p1_star) =
  (* "*" *) token env tok

let map_imm_tok_word (env : env) (tok : CST.imm_tok_word) =
  (* "word" *) token env tok

let map_tok_prec_p1_slash (env : env) (tok : CST.tok_prec_p1_slash) =
  (* tok_prec_p1_slash *) token env tok

let map_imm_tok_dir (env : env) (tok : CST.imm_tok_dir) =
  (* "dir" *) token env tok

let map_tok_prec_p1_plus (env : env) (tok : CST.tok_prec_p1_plus) =
  (* tok_prec_p1_plus *) token env tok

let map_imm_tok_subst (env : env) (tok : CST.imm_tok_subst) =
  (* "subst" *) token env tok

let map_imm_tok_prec_p1_hat (env : env) (tok : CST.imm_tok_prec_p1_hat) =
  (* "^" *) token env tok

let map_imm_tok_adds (env : env) (tok : CST.imm_tok_adds) =
  (* "addsuffix" *) token env tok

let map_imm_tok_filter (env : env) (tok : CST.imm_tok_filter) =
  (* "filter" *) token env tok

let map_imm_tok_absp (env : env) (tok : CST.imm_tok_absp) =
  (* "abspath" *) token env tok

let map_imm_tok_last (env : env) (tok : CST.imm_tok_last) =
  (* "lastword" *) token env tok

let map_imm_tok_wild (env : env) (tok : CST.imm_tok_wild) =
  (* "wildcard" *) token env tok

let map_imm_tok_prec_p1_qmark (env : env) (tok : CST.imm_tok_prec_p1_qmark) =
  (* "?" *) token env tok

let map_word (env : env) (tok : CST.word) =
  (* word *) token env tok

let map_imm_tok_semi (env : env) (tok : CST.imm_tok_semi) =
  (* ";" *) token env tok

let map_imm_tok_rpar (env : env) (tok : CST.imm_tok_rpar) =
  (* ")" *) token env tok

let map_imm_tok_error (env : env) (tok : CST.imm_tok_error) =
  (* "error" *) token env tok

let map_imm_tok_info (env : env) (tok : CST.imm_tok_info) =
  (* "info" *) token env tok

let map_imm_tok_sort (env : env) (tok : CST.imm_tok_sort) =
  (* "sort" *) token env tok

let map_imm_tok_real (env : env) (tok : CST.imm_tok_real) =
  (* "realpath" *) token env tok

let map_imm_tok_firs (env : env) (tok : CST.imm_tok_firs) =
  (* "firstword" *) token env tok

let map_imm_tok_pat_ba5cc43 (env : env) (tok : CST.imm_tok_pat_ba5cc43) =
  (* pattern [\r\n]+ *) token env tok

let map_imm_tok_bslash_pat_7b301fa (env : env) (tok : CST.imm_tok_bslash_pat_7b301fa) =
  (* imm_tok_bslash_pat_7b301fa *) token env tok

let map_imm_tok_eval (env : env) (tok : CST.imm_tok_eval) =
  (* "eval" *) token env tok

let map_imm_tok_prec_p1_perc (env : env) (tok : CST.imm_tok_prec_p1_perc) =
  (* "%" *) token env tok

let map_imm_tok_value (env : env) (tok : CST.imm_tok_value) =
  (* "value" *) token env tok

let map_imm_tok_pat_9713f58 (env : env) (tok : CST.imm_tok_pat_9713f58) =
  (* pattern [\t ]+ *) token env tok

let map_imm_tok_or (env : env) (tok : CST.imm_tok_or) =
  (* "or" *) token env tok

let map_tok_prec_n1_pat_d857316 (env : env) (tok : CST.tok_prec_n1_pat_d857316) =
  (* tok_prec_n1_pat_d857316 *) token env tok

let map_tok_prec_p1_perc (env : env) (tok : CST.tok_prec_p1_perc) =
  (* tok_prec_p1_perc *) token env tok

let map_tok_prec_p1_qmark (env : env) (tok : CST.tok_prec_p1_qmark) =
  (* tok_prec_p1_qmark *) token env tok

let map_imm_tok_find (env : env) (tok : CST.imm_tok_find) =
  (* "findstring" *) token env tok

let map_imm_tok_if (env : env) (tok : CST.imm_tok_if) =
  (* "if" *) token env tok

let map_imm_tok_suffix (env : env) (tok : CST.imm_tok_suffix) =
  (* "suffix" *) token env tok

let map_tok_prec_p1_dash (env : env) (tok : CST.tok_prec_p1_dash) =
  (* tok_prec_p1_dash *) token env tok

let map_imm_tok_lcurl (env : env) (tok : CST.imm_tok_lcurl) =
  (* "{" *) token env tok

let map_tok_prec_p1_hat (env : env) (tok : CST.tok_prec_p1_hat) =
  (* tok_prec_p1_hat *) token env tok

let map_tok_prec_p1_at (env : env) (tok : CST.tok_prec_p1_at) =
  (* tok_prec_p1_at *) token env tok

let map_imm_tok_lpar (env : env) (tok : CST.imm_tok_lpar) =
  (* "(" *) token env tok

let map_anon_choice_DOLLAR_5ccb3ec (env : env) (x : CST.anon_choice_DOLLAR_5ccb3ec) =
  (match x with
  | `DOLLAR tok -> R.Case ("DOLLAR",
      (* "$" *) token env tok
    )
  | `DOLLARDOLLAR tok -> R.Case ("DOLLARDOLLAR",
      (* "$$" *) token env tok
    )
  )

let map_imm_tok_pat_5058f1a (env : env) (tok : CST.imm_tok_pat_5058f1a) =
  (* pattern . *) token env tok

let map_imm_tok_join (env : env) (tok : CST.imm_tok_join) =
  (* "join" *) token env tok

let map_imm_tok_warn (env : env) (tok : CST.imm_tok_warn) =
  (* "warning" *) token env tok

let map_imm_tok_flavor (env : env) (tok : CST.imm_tok_flavor) =
  (* "flavor" *) token env tok

let map_imm_tok_f (env : env) (tok : CST.imm_tok_f) =
  (* "F" *) token env tok

let map_imm_tok_notdir (env : env) (tok : CST.imm_tok_notdir) =
  (* "notdir" *) token env tok

let map_imm_tok_and (env : env) (tok : CST.imm_tok_and) =
  (* "and" *) token env tok

let map_anon_choice_tok_prec_p1_at_dedbfd9 (env : env) (x : CST.anon_choice_tok_prec_p1_at_dedbfd9) =
  (match x with
  | `Tok_prec_p1_at x -> R.Case ("Tok_prec_p1_at",
      map_tok_prec_p1_at env x
    )
  | `Tok_prec_p1_perc x -> R.Case ("Tok_prec_p1_perc",
      map_tok_prec_p1_perc env x
    )
  | `Tok_prec_p1_lt x -> R.Case ("Tok_prec_p1_lt",
      map_tok_prec_p1_lt env x
    )
  | `Tok_prec_p1_qmark x -> R.Case ("Tok_prec_p1_qmark",
      map_tok_prec_p1_qmark env x
    )
  | `Tok_prec_p1_hat x -> R.Case ("Tok_prec_p1_hat",
      map_tok_prec_p1_hat env x
    )
  | `Tok_prec_p1_plus x -> R.Case ("Tok_prec_p1_plus",
      map_tok_prec_p1_plus env x
    )
  | `Tok_prec_p1_slash x -> R.Case ("Tok_prec_p1_slash",
      map_tok_prec_p1_slash env x
    )
  | `Tok_prec_p1_star x -> R.Case ("Tok_prec_p1_star",
      map_tok_prec_p1_star env x
    )
  )

let map_anon_choice_imm_tok_d_c6ab400 (env : env) (x : CST.anon_choice_imm_tok_d_c6ab400) =
  (match x with
  | `Imm_tok_d x -> R.Case ("Imm_tok_d",
      map_imm_tok_d env x
    )
  | `Imm_tok_f x -> R.Case ("Imm_tok_f",
      map_imm_tok_f env x
    )
  )

let map_undefine_directive (env : env) ((v1, v2, v3) : CST.undefine_directive) =
  let v1 = (* "undefine" *) token env v1 in
  let v2 = (* word *) token env v2 in
  let v3 = map_imm_tok_pat_ba5cc43 env v3 in
  R.Tuple [v1; v2; v3]

let map_define_directive (env : env) ((v1, v2, v3, v4, v5, v6, v7, v8, v9) : CST.define_directive) =
  let v1 = (* "define" *) token env v1 in
  let v2 = (* word *) token env v2 in
  let v3 =
    (match v3 with
    | Some x -> R.Option (Some (
        map_imm_tok_pat_9713f58 env x
      ))
    | None -> R.Option None)
  in
  let v4 =
    (match v4 with
    | Some x -> R.Option (Some (
        map_anon_choice_EQ_67cadb9 env x
      ))
    | None -> R.Option None)
  in
  let v5 =
    (match v5 with
    | Some x -> R.Option (Some (
        map_imm_tok_pat_9713f58 env x
      ))
    | None -> R.Option None)
  in
  let v6 = map_imm_tok_pat_ba5cc43 env v6 in
  let v7 =
    (match v7 with
    | Some xs -> R.Option (Some (
        R.List (List.map (token env (* rawline *)) xs)
      ))
    | None -> R.Option None)
  in
  let v8 = map_tok_prec_p1_endef env v8 in
  let v9 = map_imm_tok_pat_ba5cc43 env v9 in
  R.Tuple [v1; v2; v3; v4; v5; v6; v7; v8; v9]

let rec map_anon_choice_var_beaec2e (env : env) (x : CST.anon_choice_var_beaec2e) =
  (match x with
  | `Var x -> R.Case ("Var",
      map_variable env x
    )
  | `Func x -> R.Case ("Func",
      map_function_ env x
    )
  | `DOLLARDOLLAR tok -> R.Case ("DOLLARDOLLAR",
      (* "$$" *) token env tok
    )
  | `SLASHSLASH tok -> R.Case ("SLASHSLASH",
      (* "//" *) token env tok
    )
  )

and map_arguments (env : env) ((v1, v2) : CST.arguments) =
  let v1 = map_shell_command env v1 in
  let v2 =
    R.List (List.map (fun (v1, v2) ->
      let v1 = (* "," *) token env v1 in
      let v2 = map_shell_command env v2 in
      R.Tuple [v1; v2]
    ) v2)
  in
  R.Tuple [v1; v2]

and map_function_ (env : env) (x : CST.function_) =
  (match x with
  | `Func_call (v1, v2, v3, v4, v5, v6) -> R.Case ("Func_call",
      let v1 = map_anon_choice_DOLLAR_5ccb3ec env v1 in
      let v2 = map_imm_tok_lpar env v2 in
      let v3 =
        (match v3 with
        | `Imm_tok_subst x -> R.Case ("Imm_tok_subst",
            map_imm_tok_subst env x
          )
        | `Imm_tok_pats x -> R.Case ("Imm_tok_pats",
            map_imm_tok_pats env x
          )
        | `Imm_tok_strip x -> R.Case ("Imm_tok_strip",
            map_imm_tok_strip env x
          )
        | `Imm_tok_find x -> R.Case ("Imm_tok_find",
            map_imm_tok_find env x
          )
        | `Imm_tok_filter x -> R.Case ("Imm_tok_filter",
            map_imm_tok_filter env x
          )
        | `Imm_tok_filt x -> R.Case ("Imm_tok_filt",
            map_imm_tok_filt env x
          )
        | `Imm_tok_sort x -> R.Case ("Imm_tok_sort",
            map_imm_tok_sort env x
          )
        | `Imm_tok_word x -> R.Case ("Imm_tok_word",
            map_imm_tok_word env x
          )
        | `Imm_tok_words x -> R.Case ("Imm_tok_words",
            map_imm_tok_words env x
          )
        | `Imm_tok_word_ x -> R.Case ("Imm_tok_word_",
            map_imm_tok_word_ env x
          )
        | `Imm_tok_firs x -> R.Case ("Imm_tok_firs",
            map_imm_tok_firs env x
          )
        | `Imm_tok_last x -> R.Case ("Imm_tok_last",
            map_imm_tok_last env x
          )
        | `Imm_tok_dir x -> R.Case ("Imm_tok_dir",
            map_imm_tok_dir env x
          )
        | `Imm_tok_notdir x -> R.Case ("Imm_tok_notdir",
            map_imm_tok_notdir env x
          )
        | `Imm_tok_suffix x -> R.Case ("Imm_tok_suffix",
            map_imm_tok_suffix env x
          )
        | `Imm_tok_base x -> R.Case ("Imm_tok_base",
            map_imm_tok_base env x
          )
        | `Imm_tok_adds x -> R.Case ("Imm_tok_adds",
            map_imm_tok_adds env x
          )
        | `Imm_tok_addp x -> R.Case ("Imm_tok_addp",
            map_imm_tok_addp env x
          )
        | `Imm_tok_join x -> R.Case ("Imm_tok_join",
            map_imm_tok_join env x
          )
        | `Imm_tok_wild x -> R.Case ("Imm_tok_wild",
            map_imm_tok_wild env x
          )
        | `Imm_tok_real x -> R.Case ("Imm_tok_real",
            map_imm_tok_real env x
          )
        | `Imm_tok_absp x -> R.Case ("Imm_tok_absp",
            map_imm_tok_absp env x
          )
        | `Imm_tok_error x -> R.Case ("Imm_tok_error",
            map_imm_tok_error env x
          )
        | `Imm_tok_warn x -> R.Case ("Imm_tok_warn",
            map_imm_tok_warn env x
          )
        | `Imm_tok_info x -> R.Case ("Imm_tok_info",
            map_imm_tok_info env x
          )
        | `Imm_tok_origin x -> R.Case ("Imm_tok_origin",
            map_imm_tok_origin env x
          )
        | `Imm_tok_flavor x -> R.Case ("Imm_tok_flavor",
            map_imm_tok_flavor env x
          )
        | `Imm_tok_fore x -> R.Case ("Imm_tok_fore",
            map_imm_tok_fore env x
          )
        | `Imm_tok_if x -> R.Case ("Imm_tok_if",
            map_imm_tok_if env x
          )
        | `Imm_tok_or x -> R.Case ("Imm_tok_or",
            map_imm_tok_or env x
          )
        | `Imm_tok_and x -> R.Case ("Imm_tok_and",
            map_imm_tok_and env x
          )
        | `Imm_tok_call x -> R.Case ("Imm_tok_call",
            map_imm_tok_call env x
          )
        | `Imm_tok_eval x -> R.Case ("Imm_tok_eval",
            map_imm_tok_eval env x
          )
        | `Imm_tok_file x -> R.Case ("Imm_tok_file",
            map_imm_tok_file env x
          )
        | `Imm_tok_value x -> R.Case ("Imm_tok_value",
            map_imm_tok_value env x
          )
        )
      in
      let v4 =
        (match v4 with
        | Some x -> R.Option (Some (
            map_imm_tok_pat_9713f58 env x
          ))
        | None -> R.Option None)
      in
      let v5 = map_arguments env v5 in
      let v6 = (* ")" *) token env v6 in
      R.Tuple [v1; v2; v3; v4; v5; v6]
    )
  | `Shell_func (v1, v2, v3, v4, v5, v6) -> R.Case ("Shell_func",
      let v1 = map_anon_choice_DOLLAR_5ccb3ec env v1 in
      let v2 = map_imm_tok_lpar env v2 in
      let v3 = (* "shell" *) token env v3 in
      let v4 =
        (match v4 with
        | Some x -> R.Option (Some (
            map_imm_tok_pat_9713f58 env x
          ))
        | None -> R.Option None)
      in
      let v5 = map_shell_command env v5 in
      let v6 = (* ")" *) token env v6 in
      R.Tuple [v1; v2; v3; v4; v5; v6]
    )
  )

and map_list_ (env : env) ((v1, v2, v3) : CST.list_) =
  let v1 = map_primary env v1 in
  let v2 =
    R.List (List.map (fun (v1, v2) ->
      let v1 =
        (match v1 with
        | `Imm_tok_pat_9713f58 x -> R.Case ("Imm_tok_pat_9713f58",
            map_imm_tok_pat_9713f58 env x
          )
        | `Imm_tok_bslash_pat_7b301fa x -> R.Case ("Imm_tok_bslash_pat_7b301fa",
            map_imm_tok_bslash_pat_7b301fa env x
          )
        )
      in
      let v2 = map_primary env v2 in
      R.Tuple [v1; v2]
    ) v2)
  in
  let v3 =
    (match v3 with
    | Some x -> R.Option (Some (
        map_imm_tok_pat_9713f58 env x
      ))
    | None -> R.Option None)
  in
  R.Tuple [v1; v2; v3]

and map_primary (env : env) (x : CST.primary) =
  (match x with
  | `Word tok -> R.Case ("Word",
      (* word *) token env tok
    )
  | `Arch (v1, v2, v3, v4) -> R.Case ("Arch",
      let v1 = (* word *) token env v1 in
      let v2 = map_imm_tok_lpar env v2 in
      let v3 = map_target_pattern env v3 in
      let v4 = map_imm_tok_rpar env v4 in
      R.Tuple [v1; v2; v3; v4]
    )
  | `Var x -> R.Case ("Var",
      map_variable env x
    )
  | `Func x -> R.Case ("Func",
      map_function_ env x
    )
  | `Conc (v1, v2) -> R.Case ("Conc",
      let v1 = map_primary env v1 in
      let v2 = R.List (List.map (map_primary env) v2) in
      R.Tuple [v1; v2]
    )
  | `Str x -> R.Case ("Str",
      map_string_ env x
    )
  )

and map_shell_command (env : env) (x : CST.shell_command) =
  map_text env x

and map_string_ (env : env) (x : CST.string_) =
  (match x with
  | `DQUOT_opt_rep1_choice_var_DQUOT (v1, v2, v3) -> R.Case ("DQUOT_opt_rep1_choice_var_DQUOT",
      let v1 = (* "\"" *) token env v1 in
      let v2 =
        (match v2 with
        | Some x -> R.Option (Some (
            map_string__ env x
          ))
        | None -> R.Option None)
      in
      let v3 = (* "\"" *) token env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `SQUOT_opt_rep1_choice_var_SQUOT (v1, v2, v3) -> R.Case ("SQUOT_opt_rep1_choice_var_SQUOT",
      let v1 = (* "'" *) token env v1 in
      let v2 =
        (match v2 with
        | Some x -> R.Option (Some (
            map_string__ env x
          ))
        | None -> R.Option None)
      in
      let v3 = (* "'" *) token env v3 in
      R.Tuple [v1; v2; v3]
    )
  )

and map_string__ (env : env) (xs : CST.string__) =
  R.List (List.map (fun x ->
    (match x with
    | `Var x -> R.Case ("Var",
        map_variable env x
      )
    | `Func x -> R.Case ("Func",
        map_function_ env x
      )
    | `Tok_prec_n1_pat_d857316 x -> R.Case ("Tok_prec_n1_pat_d857316",
        map_tok_prec_n1_pat_d857316 env x
      )
    )
  ) xs)

and map_target_pattern (env : env) (x : CST.target_pattern) =
  map_list_ env x

and map_text (env : env) (x : CST.text) =
  (match x with
  | `Tok_rep1_choice_choice_pat_c610685_rep_choice_var_opt_tok_rep1_choice_choice_pat_c610685 (v1, v2) -> R.Case ("Tok_rep1_choice_choice_pat_c610685_rep_choice_var_opt_tok_rep1_choice_choice_pat_c610685",
      let v1 = map_tok_rep1_choice_choice_pat_c610685 env v1 in
      let v2 =
        R.List (List.map (fun (v1, v2) ->
          let v1 = map_anon_choice_var_beaec2e env v1 in
          let v2 =
            (match v2 with
            | Some x -> R.Option (Some (
                map_tok_rep1_choice_choice_pat_c610685 env x
              ))
            | None -> R.Option None)
          in
          R.Tuple [v1; v2]
        ) v2)
      in
      R.Tuple [v1; v2]
    )
  | `Choice_var_rep_opt_tok_rep1_choice_choice_pat_c610685_choice_var_opt_tok_rep1_choice_choice_pat_c610685 (v1, v2, v3) -> R.Case ("Choice_var_rep_opt_tok_rep1_choice_choice_pat_c610685_choice_var_opt_tok_rep1_choice_choice_pat_c610685",
      let v1 = map_anon_choice_var_beaec2e env v1 in
      let v2 =
        R.List (List.map (fun (v1, v2) ->
          let v1 =
            (match v1 with
            | Some x -> R.Option (Some (
                map_tok_rep1_choice_choice_pat_c610685 env x
              ))
            | None -> R.Option None)
          in
          let v2 = map_anon_choice_var_beaec2e env v2 in
          R.Tuple [v1; v2]
        ) v2)
      in
      let v3 =
        (match v3 with
        | Some x -> R.Option (Some (
            map_tok_rep1_choice_choice_pat_c610685 env x
          ))
        | None -> R.Option None)
      in
      R.Tuple [v1; v2; v3]
    )
  )

and map_variable (env : env) (x : CST.variable) =
  (match x with
  | `Var_ref (v1, v2) -> R.Case ("Var_ref",
      let v1 = map_anon_choice_DOLLAR_5ccb3ec env v1 in
      let v2 =
        (match v2 with
        | `Choice_imm_tok_lpar_choice_word_RPAR x -> R.Case ("Choice_imm_tok_lpar_choice_word_RPAR",
            (match x with
            | `Imm_tok_lpar_choice_word_RPAR (v1, v2, v3) -> R.Case ("Imm_tok_lpar_choice_word_RPAR",
                let v1 = map_imm_tok_lpar env v1 in
                let v2 = map_primary env v2 in
                let v3 = (* ")" *) token env v3 in
                R.Tuple [v1; v2; v3]
              )
            | `Imm_tok_lcurl_choice_word_RCURL (v1, v2, v3) -> R.Case ("Imm_tok_lcurl_choice_word_RCURL",
                let v1 = map_imm_tok_lcurl env v1 in
                let v2 = map_primary env v2 in
                let v3 = (* "}" *) token env v3 in
                R.Tuple [v1; v2; v3]
              )
            )
          )
        | `Imm_tok_pat_5058f1a x -> R.Case ("Imm_tok_pat_5058f1a",
            map_imm_tok_pat_5058f1a env x
          )
        )
      in
      R.Tuple [v1; v2]
    )
  | `Subs_ref (v1, v2) -> R.Case ("Subs_ref",
      let v1 = map_anon_choice_DOLLAR_5ccb3ec env v1 in
      let v2 =
        (match v2 with
        | `Imm_tok_lpar_choice_word_COLON_choice_word_EQ_choice_word_RPAR (v1, v2, v3, v4, v5, v6, v7) -> R.Case ("Imm_tok_lpar_choice_word_COLON_choice_word_EQ_choice_word_RPAR",
            let v1 = map_imm_tok_lpar env v1 in
            let v2 = map_primary env v2 in
            let v3 = (* ":" *) token env v3 in
            let v4 = map_primary env v4 in
            let v5 = (* "=" *) token env v5 in
            let v6 = map_primary env v6 in
            let v7 = (* ")" *) token env v7 in
            R.Tuple [v1; v2; v3; v4; v5; v6; v7]
          )
        | `Imm_tok_lcurl_choice_word_COLON_choice_word_EQ_choice_word_RCURL (v1, v2, v3, v4, v5, v6, v7) -> R.Case ("Imm_tok_lcurl_choice_word_COLON_choice_word_EQ_choice_word_RCURL",
            let v1 = map_imm_tok_lcurl env v1 in
            let v2 = map_primary env v2 in
            let v3 = (* ":" *) token env v3 in
            let v4 = map_primary env v4 in
            let v5 = (* "=" *) token env v5 in
            let v6 = map_primary env v6 in
            let v7 = (* "}" *) token env v7 in
            R.Tuple [v1; v2; v3; v4; v5; v6; v7]
          )
        )
      in
      R.Tuple [v1; v2]
    )
  | `Auto_var (v1, v2) -> R.Case ("Auto_var",
      let v1 = map_anon_choice_DOLLAR_5ccb3ec env v1 in
      let v2 =
        (match v2 with
        | `Choice_imm_tok_prec_p1_at x -> R.Case ("Choice_imm_tok_prec_p1_at",
            (match x with
            | `Imm_tok_prec_p1_at x -> R.Case ("Imm_tok_prec_p1_at",
                map_imm_tok_prec_p1_at env x
              )
            | `Imm_tok_prec_p1_perc x -> R.Case ("Imm_tok_prec_p1_perc",
                map_imm_tok_prec_p1_perc env x
              )
            | `Imm_tok_prec_p1_lt x -> R.Case ("Imm_tok_prec_p1_lt",
                map_imm_tok_prec_p1_lt env x
              )
            | `Imm_tok_prec_p1_qmark x -> R.Case ("Imm_tok_prec_p1_qmark",
                map_imm_tok_prec_p1_qmark env x
              )
            | `Imm_tok_prec_p1_hat x -> R.Case ("Imm_tok_prec_p1_hat",
                map_imm_tok_prec_p1_hat env x
              )
            | `Imm_tok_prec_p1_plus x -> R.Case ("Imm_tok_prec_p1_plus",
                map_imm_tok_prec_p1_plus env x
              )
            | `Imm_tok_prec_p1_slash x -> R.Case ("Imm_tok_prec_p1_slash",
                map_imm_tok_prec_p1_slash env x
              )
            | `Imm_tok_prec_p1_star x -> R.Case ("Imm_tok_prec_p1_star",
                map_imm_tok_prec_p1_star env x
              )
            )
          )
        | `Choice_imm_tok_lpar_choice_tok_prec_p1_at_opt_choice_imm_tok_d_RPAR x -> R.Case ("Choice_imm_tok_lpar_choice_tok_prec_p1_at_opt_choice_imm_tok_d_RPAR",
            (match x with
            | `Imm_tok_lpar_choice_tok_prec_p1_at_opt_choice_imm_tok_d_RPAR (v1, v2, v3, v4) -> R.Case ("Imm_tok_lpar_choice_tok_prec_p1_at_opt_choice_imm_tok_d_RPAR",
                let v1 = map_imm_tok_lpar env v1 in
                let v2 = map_anon_choice_tok_prec_p1_at_dedbfd9 env v2 in
                let v3 =
                  (match v3 with
                  | Some x -> R.Option (Some (
                      map_anon_choice_imm_tok_d_c6ab400 env x
                    ))
                  | None -> R.Option None)
                in
                let v4 = (* ")" *) token env v4 in
                R.Tuple [v1; v2; v3; v4]
              )
            | `Imm_tok_lcurl_choice_tok_prec_p1_at_opt_choice_imm_tok_d_RCURL (v1, v2, v3, v4) -> R.Case ("Imm_tok_lcurl_choice_tok_prec_p1_at_opt_choice_imm_tok_d_RCURL",
                let v1 = map_imm_tok_lcurl env v1 in
                let v2 = map_anon_choice_tok_prec_p1_at_dedbfd9 env v2 in
                let v3 =
                  (match v3 with
                  | Some x -> R.Option (Some (
                      map_anon_choice_imm_tok_d_c6ab400 env x
                    ))
                  | None -> R.Option None)
                in
                let v4 = (* "}" *) token env v4 in
                R.Tuple [v1; v2; v3; v4]
              )
            )
          )
        )
      in
      R.Tuple [v1; v2]
    )
  )

let map_target_or_pattern_assignment (env : env) ((v1, v2, v3) : CST.target_or_pattern_assignment) =
  let v1 = map_target_pattern env v1 in
  let v2 = (* ":" *) token env v2 in
  let v3 =
    (match v3 with
    | Some x -> R.Option (Some (
        map_imm_tok_pat_9713f58 env x
      ))
    | None -> R.Option None)
  in
  R.Tuple [v1; v2; v3]

let map_paths (env : env) ((v1, v2) : CST.paths) =
  let v1 = map_primary env v1 in
  let v2 =
    R.List (List.map (fun (v1, v2) ->
      let v1 =
        (match v1 with
        | `Imm_tok_colon x -> R.Case ("Imm_tok_colon",
            map_imm_tok_colon env x
          )
        | `Imm_tok_semi x -> R.Case ("Imm_tok_semi",
            map_imm_tok_semi env x
          )
        )
      in
      let v2 = map_primary env v2 in
      R.Tuple [v1; v2]
    ) v2)
  in
  R.Tuple [v1; v2]

let map_unexport_directive (env : env) (x : CST.unexport_directive) =
  (match x with
  | `Unex_imm_tok_pat_ba5cc43 (v1, v2) -> R.Case ("Unex_imm_tok_pat_ba5cc43",
      let v1 = (* "unexport" *) token env v1 in
      let v2 = map_imm_tok_pat_ba5cc43 env v2 in
      R.Tuple [v1; v2]
    )
  | `Unex_list_imm_tok_pat_ba5cc43 (v1, v2, v3) -> R.Case ("Unex_list_imm_tok_pat_ba5cc43",
      let v1 = (* "unexport" *) token env v1 in
      let v2 = map_target_pattern env v2 in
      let v3 = map_imm_tok_pat_ba5cc43 env v3 in
      R.Tuple [v1; v2; v3]
    )
  )

let map_conditional_args_cmp (env : env) (x : CST.conditional_args_cmp) =
  (match x with
  | `LPAR_opt_choice_word_COMMA_opt_choice_word_RPAR (v1, v2, v3, v4, v5) -> R.Case ("LPAR_opt_choice_word_COMMA_opt_choice_word_RPAR",
      let v1 = (* "(" *) token env v1 in
      let v2 =
        (match v2 with
        | Some x -> R.Option (Some (
            map_primary env x
          ))
        | None -> R.Option None)
      in
      let v3 = (* "," *) token env v3 in
      let v4 =
        (match v4 with
        | Some x -> R.Option (Some (
            map_primary env x
          ))
        | None -> R.Option None)
      in
      let v5 = (* ")" *) token env v5 in
      R.Tuple [v1; v2; v3; v4; v5]
    )
  | `Choice_word_choice_word (v1, v2) -> R.Case ("Choice_word_choice_word",
      let v1 = map_primary env v1 in
      let v2 = map_primary env v2 in
      R.Tuple [v1; v2]
    )
  )

let map_normal_prerequisites (env : env) (x : CST.normal_prerequisites) =
  map_list_ env x

let map_include_directive (env : env) (x : CST.include_directive) =
  (match x with
  | `Incl_list_imm_tok_pat_ba5cc43 (v1, v2, v3) -> R.Case ("Incl_list_imm_tok_pat_ba5cc43",
      let v1 = (* "include" *) token env v1 in
      let v2 = map_target_pattern env v2 in
      let v3 = map_imm_tok_pat_ba5cc43 env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Sinc_list_imm_tok_pat_ba5cc43 (v1, v2, v3) -> R.Case ("Sinc_list_imm_tok_pat_ba5cc43",
      let v1 = (* "sinclude" *) token env v1 in
      let v2 = map_target_pattern env v2 in
      let v3 = map_imm_tok_pat_ba5cc43 env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `DASH_list_imm_tok_pat_ba5cc43 (v1, v2, v3) -> R.Case ("DASH_list_imm_tok_pat_ba5cc43",
      let v1 = (* "-include" *) token env v1 in
      let v2 = map_target_pattern env v2 in
      let v3 = map_imm_tok_pat_ba5cc43 env v3 in
      R.Tuple [v1; v2; v3]
    )
  )

let map_shell_text_without_split (env : env) (x : CST.shell_text_without_split) =
  (match x with
  | `Tok_rep1_choice_pat_549beab_rep_choice_var_opt_tok_rep1_choice_pat_549beab (v1, v2) -> R.Case ("Tok_rep1_choice_pat_549beab_rep_choice_var_opt_tok_rep1_choice_pat_549beab",
      let v1 = map_tok_rep1_choice_pat_549beab env v1 in
      let v2 =
        R.List (List.map (fun (v1, v2) ->
          let v1 = map_anon_choice_var_beaec2e env v1 in
          let v2 =
            (match v2 with
            | Some x -> R.Option (Some (
                map_tok_rep1_choice_pat_549beab env x
              ))
            | None -> R.Option None)
          in
          R.Tuple [v1; v2]
        ) v2)
      in
      R.Tuple [v1; v2]
    )
  | `Choice_var_rep_opt_tok_rep1_choice_pat_549beab_choice_var_opt_tok_rep1_choice_pat_549beab (v1, v2, v3) -> R.Case ("Choice_var_rep_opt_tok_rep1_choice_pat_549beab_choice_var_opt_tok_rep1_choice_pat_549beab",
      let v1 = map_anon_choice_var_beaec2e env v1 in
      let v2 =
        R.List (List.map (fun (v1, v2) ->
          let v1 =
            (match v1 with
            | Some x -> R.Option (Some (
                map_tok_rep1_choice_pat_549beab env x
              ))
            | None -> R.Option None)
          in
          let v2 = map_anon_choice_var_beaec2e env v2 in
          R.Tuple [v1; v2]
        ) v2)
      in
      let v3 =
        (match v3 with
        | Some x -> R.Option (Some (
            map_tok_rep1_choice_pat_549beab env x
          ))
        | None -> R.Option None)
      in
      R.Tuple [v1; v2; v3]
    )
  )

let map_variable_assignment (env : env) ((v1, v2, v3, v4, v5, v6, v7) : CST.variable_assignment) =
  let v1 =
    (match v1 with
    | Some x -> R.Option (Some (
        map_target_or_pattern_assignment env x
      ))
    | None -> R.Option None)
  in
  let v2 = (* word *) token env v2 in
  let v3 =
    (match v3 with
    | Some x -> R.Option (Some (
        map_imm_tok_pat_9713f58 env x
      ))
    | None -> R.Option None)
  in
  let v4 = map_anon_choice_EQ_67cadb9 env v4 in
  let v5 =
    (match v5 with
    | Some x -> R.Option (Some (
        map_imm_tok_pat_9713f58 env x
      ))
    | None -> R.Option None)
  in
  let v6 =
    (match v6 with
    | Some x -> R.Option (Some (
        map_shell_command env x
      ))
    | None -> R.Option None)
  in
  let v7 = map_imm_tok_pat_ba5cc43 env v7 in
  R.Tuple [v1; v2; v3; v4; v5; v6; v7]

let map_vpath_directive (env : env) (x : CST.vpath_directive) =
  (match x with
  | `Vpath_imm_tok_pat_ba5cc43 (v1, v2) -> R.Case ("Vpath_imm_tok_pat_ba5cc43",
      let v1 = (* "vpath" *) token env v1 in
      let v2 = map_imm_tok_pat_ba5cc43 env v2 in
      R.Tuple [v1; v2]
    )
  | `Vpath_word_imm_tok_pat_ba5cc43 (v1, v2, v3) -> R.Case ("Vpath_word_imm_tok_pat_ba5cc43",
      let v1 = (* "vpath" *) token env v1 in
      let v2 = (* word *) token env v2 in
      let v3 = map_imm_tok_pat_ba5cc43 env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Vpath_word_paths_imm_tok_pat_ba5cc43 (v1, v2, v3, v4) -> R.Case ("Vpath_word_paths_imm_tok_pat_ba5cc43",
      let v1 = (* "vpath" *) token env v1 in
      let v2 = (* word *) token env v2 in
      let v3 = map_paths env v3 in
      let v4 = map_imm_tok_pat_ba5cc43 env v4 in
      R.Tuple [v1; v2; v3; v4]
    )
  )

let map_prerequisites (env : env) (x : CST.prerequisites) =
  (match x with
  | `Normal_preres x -> R.Case ("Normal_preres",
      map_normal_prerequisites env x
    )
  | `Opt_normal_preres_BAR_list (v1, v2, v3) -> R.Case ("Opt_normal_preres_BAR_list",
      let v1 =
        (match v1 with
        | Some x -> R.Option (Some (
            map_normal_prerequisites env x
          ))
        | None -> R.Option None)
      in
      let v2 = (* "|" *) token env v2 in
      let v3 = map_target_pattern env v3 in
      R.Tuple [v1; v2; v3]
    )
  )

let map_shell_text_with_split (env : env) ((v1, v2) : CST.shell_text_with_split) =
  let v1 = map_shell_text_without_split env v1 in
  let v2 = map_imm_tok_bslash_pat_7b301fa env v2 in
  R.Tuple [v1; v2]

let map_export_directive (env : env) (x : CST.export_directive) =
  (match x with
  | `Export_imm_tok_pat_ba5cc43 (v1, v2) -> R.Case ("Export_imm_tok_pat_ba5cc43",
      let v1 = (* "export" *) token env v1 in
      let v2 = map_imm_tok_pat_ba5cc43 env v2 in
      R.Tuple [v1; v2]
    )
  | `Export_list_imm_tok_pat_ba5cc43 (v1, v2, v3) -> R.Case ("Export_list_imm_tok_pat_ba5cc43",
      let v1 = (* "export" *) token env v1 in
      let v2 = map_target_pattern env v2 in
      let v3 = map_imm_tok_pat_ba5cc43 env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Export_var_assign (v1, v2) -> R.Case ("Export_var_assign",
      let v1 = (* "export" *) token env v1 in
      let v2 = map_variable_assignment env v2 in
      R.Tuple [v1; v2]
    )
  )

let map_override_directive (env : env) (x : CST.override_directive) =
  (match x with
  | `Over_define_dire (v1, v2) -> R.Case ("Over_define_dire",
      let v1 = (* "override" *) token env v1 in
      let v2 = map_define_directive env v2 in
      R.Tuple [v1; v2]
    )
  | `Over_var_assign (v1, v2) -> R.Case ("Over_var_assign",
      let v1 = (* "override" *) token env v1 in
      let v2 = map_variable_assignment env v2 in
      R.Tuple [v1; v2]
    )
  | `Over_unde_dire (v1, v2) -> R.Case ("Over_unde_dire",
      let v1 = (* "override" *) token env v1 in
      let v2 = map_undefine_directive env v2 in
      R.Tuple [v1; v2]
    )
  )

let map_variable_definition (env : env) (x : CST.variable_definition) =
  (match x with
  | `Vpath_assign (v1, v2, v3, v4, v5) -> R.Case ("Vpath_assign",
      let v1 = (* "VPATH" *) token env v1 in
      let v2 =
        (match v2 with
        | Some x -> R.Option (Some (
            map_imm_tok_pat_9713f58 env x
          ))
        | None -> R.Option None)
      in
      let v3 = map_anon_choice_EQ_67cadb9 env v3 in
      let v4 = map_paths env v4 in
      let v5 = map_imm_tok_pat_ba5cc43 env v5 in
      R.Tuple [v1; v2; v3; v4; v5]
    )
  | `Reci_assign (v1, v2, v3, v4, v5) -> R.Case ("Reci_assign",
      let v1 = (* ".RECIPEPREFIX" *) token env v1 in
      let v2 =
        (match v2 with
        | Some x -> R.Option (Some (
            map_imm_tok_pat_9713f58 env x
          ))
        | None -> R.Option None)
      in
      let v3 = map_anon_choice_EQ_67cadb9 env v3 in
      let v4 = map_shell_command env v4 in
      let v5 = map_imm_tok_pat_ba5cc43 env v5 in
      R.Tuple [v1; v2; v3; v4; v5]
    )
  | `Var_assign x -> R.Case ("Var_assign",
      map_variable_assignment env x
    )
  | `Shell_assign (v1, v2, v3, v4, v5, v6) -> R.Case ("Shell_assign",
      let v1 = (* word *) token env v1 in
      let v2 =
        (match v2 with
        | Some x -> R.Option (Some (
            map_imm_tok_pat_9713f58 env x
          ))
        | None -> R.Option None)
      in
      let v3 = (* "!=" *) token env v3 in
      let v4 =
        (match v4 with
        | Some x -> R.Option (Some (
            map_imm_tok_pat_9713f58 env x
          ))
        | None -> R.Option None)
      in
      let v5 = map_shell_command env v5 in
      let v6 = map_imm_tok_pat_ba5cc43 env v6 in
      R.Tuple [v1; v2; v3; v4; v5; v6]
    )
  | `Define_dire x -> R.Case ("Define_dire",
      map_define_directive env x
    )
  )

let map_conditional_directives (env : env) (x : CST.conditional_directives) =
  (match x with
  | `Ifeq_dire (v1, v2, v3) -> R.Case ("Ifeq_dire",
      let v1 = (* "ifeq" *) token env v1 in
      let v2 = map_conditional_args_cmp env v2 in
      let v3 = map_imm_tok_pat_ba5cc43 env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Ifneq_dire (v1, v2, v3) -> R.Case ("Ifneq_dire",
      let v1 = (* "ifneq" *) token env v1 in
      let v2 = map_conditional_args_cmp env v2 in
      let v3 = map_imm_tok_pat_ba5cc43 env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Ifdef_dire (v1, v2, v3) -> R.Case ("Ifdef_dire",
      let v1 = (* "ifdef" *) token env v1 in
      let v2 = map_primary env v2 in
      let v3 = map_imm_tok_pat_ba5cc43 env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Ifndef_dire (v1, v2, v3) -> R.Case ("Ifndef_dire",
      let v1 = (* "ifndef" *) token env v1 in
      let v2 = map_primary env v2 in
      let v3 = map_imm_tok_pat_ba5cc43 env v3 in
      R.Tuple [v1; v2; v3]
    )
  )

let map_recipe_line (env : env) ((v1, v2, v3) : CST.recipe_line) =
  let v1 =
    (match v1 with
    | Some x -> R.Option (Some (
        (match x with
        | `Tok_prec_p1_at x -> R.Case ("Tok_prec_p1_at",
            map_tok_prec_p1_at env x
          )
        | `Tok_prec_p1_dash x -> R.Case ("Tok_prec_p1_dash",
            map_tok_prec_p1_dash env x
          )
        | `Tok_prec_p1_plus x -> R.Case ("Tok_prec_p1_plus",
            map_tok_prec_p1_plus env x
          )
        )
      ))
    | None -> R.Option None)
  in
  let v2 =
    (match v2 with
    | Some (v1, v2, v3) -> R.Option (Some (
        let v1 = map_shell_text_with_split env v1 in
        let v2 =
          R.List (List.map (fun (v1, v2) ->
            let v1 =
              (match v1 with
              | Some tok -> R.Option (Some (
                  (* "\t" *) token env tok
                ))
              | None -> R.Option None)
            in
            let v2 = map_shell_text_with_split env v2 in
            R.Tuple [v1; v2]
          ) v2)
        in
        let v3 =
          (match v3 with
          | Some tok -> R.Option (Some (
              (* "\t" *) token env tok
            ))
          | None -> R.Option None)
        in
        R.Tuple [v1; v2; v3]
      ))
    | None -> R.Option None)
  in
  let v3 = map_shell_text_without_split env v3 in
  R.Tuple [v1; v2; v3]

let map_prefixed_recipe_line (env : env) ((v1, v2, v3) : CST.prefixed_recipe_line) =
  let v1 = (* "\t" *) token env v1 in
  let v2 =
    (match v2 with
    | Some x -> R.Option (Some (
        map_recipe_line env x
      ))
    | None -> R.Option None)
  in
  let v3 = map_imm_tok_pat_ba5cc43 env v3 in
  R.Tuple [v1; v2; v3]

let map_attached_recipe_line (env : env) ((v1, v2) : CST.attached_recipe_line) =
  let v1 = (* ";" *) token env v1 in
  let v2 =
    (match v2 with
    | Some x -> R.Option (Some (
        map_recipe_line env x
      ))
    | None -> R.Option None)
  in
  R.Tuple [v1; v2]

let rec map_anon_choice_cond_8c6f069 (env : env) (x : CST.anon_choice_cond_8c6f069) =
  (match x with
  | `Cond x -> R.Case ("Cond",
      map_conditional env x
    )
  | `Pref_recipe_line x -> R.Case ("Pref_recipe_line",
      map_prefixed_recipe_line env x
    )
  )

and map_anon_choice_recipe_9acf1ce (env : env) (x : CST.anon_choice_recipe_9acf1ce) =
  (match x with
  | `Recipe x -> R.Case ("Recipe",
      map_recipe env x
    )
  | `Imm_tok_pat_ba5cc43 x -> R.Case ("Imm_tok_pat_ba5cc43",
      map_imm_tok_pat_ba5cc43 env x
    )
  )

and map_conditional (env : env) ((v1, v2, v3, v4, v5, v6) : CST.conditional) =
  let v1 = map_conditional_directives env v1 in
  let v2 =
    (match v2 with
    | Some x -> R.Option (Some (
        map_conditional_consequence env x
      ))
    | None -> R.Option None)
  in
  let v3 = R.List (List.map (map_elsif_directive env) v3) in
  let v4 =
    (match v4 with
    | Some x -> R.Option (Some (
        map_else_directive env x
      ))
    | None -> R.Option None)
  in
  let v5 = (* "endif" *) token env v5 in
  let v6 = map_imm_tok_pat_ba5cc43 env v6 in
  R.Tuple [v1; v2; v3; v4; v5; v6]

and map_conditional_consequence (env : env) (xs : CST.conditional_consequence) =
  R.List (List.map (fun x ->
    (match x with
    | `Thing x -> R.Case ("Thing",
        map_thing env x
      )
    | `Pref_recipe_line x -> R.Case ("Pref_recipe_line",
        map_prefixed_recipe_line env x
      )
    )
  ) xs)

and map_directive (env : env) (x : CST.directive) =
  (match x with
  | `Incl_dire x -> R.Case ("Incl_dire",
      map_include_directive env x
    )
  | `Vpath_dire x -> R.Case ("Vpath_dire",
      map_vpath_directive env x
    )
  | `Export_dire x -> R.Case ("Export_dire",
      map_export_directive env x
    )
  | `Unex_dire x -> R.Case ("Unex_dire",
      map_unexport_directive env x
    )
  | `Over_dire x -> R.Case ("Over_dire",
      map_override_directive env x
    )
  | `Unde_dire x -> R.Case ("Unde_dire",
      map_undefine_directive env x
    )
  | `Priv_dire (v1, v2) -> R.Case ("Priv_dire",
      let v1 = (* "private" *) token env v1 in
      let v2 = map_variable_assignment env v2 in
      R.Tuple [v1; v2]
    )
  | `Cond x -> R.Case ("Cond",
      map_conditional env x
    )
  )

and map_else_directive (env : env) ((v1, v2, v3) : CST.else_directive) =
  let v1 = (* "else" *) token env v1 in
  let v2 = map_imm_tok_pat_ba5cc43 env v2 in
  let v3 =
    (match v3 with
    | Some x -> R.Option (Some (
        map_conditional_consequence env x
      ))
    | None -> R.Option None)
  in
  R.Tuple [v1; v2; v3]

and map_elsif_directive (env : env) ((v1, v2, v3) : CST.elsif_directive) =
  let v1 = (* "else" *) token env v1 in
  let v2 = map_conditional_directives env v2 in
  let v3 =
    (match v3 with
    | Some x -> R.Option (Some (
        map_conditional_consequence env x
      ))
    | None -> R.Option None)
  in
  R.Tuple [v1; v2; v3]

and map_recipe (env : env) (x : CST.recipe) =
  (match x with
  | `Atta_recipe_line_imm_tok_pat_ba5cc43_rep_choice_cond (v1, v2, v3) -> R.Case ("Atta_recipe_line_imm_tok_pat_ba5cc43_rep_choice_cond",
      let v1 = map_attached_recipe_line env v1 in
      let v2 = map_imm_tok_pat_ba5cc43 env v2 in
      let v3 =
        R.List (List.map (map_anon_choice_cond_8c6f069 env) v3)
      in
      R.Tuple [v1; v2; v3]
    )
  | `Imm_tok_pat_ba5cc43_rep1_choice_cond (v1, v2) -> R.Case ("Imm_tok_pat_ba5cc43_rep1_choice_cond",
      let v1 = map_imm_tok_pat_ba5cc43 env v1 in
      let v2 =
        R.List (List.map (map_anon_choice_cond_8c6f069 env) v2)
      in
      R.Tuple [v1; v2]
    )
  )

and map_rule (env : env) (x : CST.rule) =
  (match x with
  | `Ordi_rule (v1, v2, v3, v4, v5) -> R.Case ("Ordi_rule",
      let v1 = map_target_pattern env v1 in
      let v2 =
        (match v2 with
        | `COLON tok -> R.Case ("COLON",
            (* ":" *) token env tok
          )
        | `AMPCOLON tok -> R.Case ("AMPCOLON",
            (* "&:" *) token env tok
          )
        | `COLONCOLON tok -> R.Case ("COLONCOLON",
            (* "::" *) token env tok
          )
        )
      in
      let v3 =
        (match v3 with
        | Some x -> R.Option (Some (
            map_imm_tok_pat_9713f58 env x
          ))
        | None -> R.Option None)
      in
      let v4 =
        (match v4 with
        | Some x -> R.Option (Some (
            map_prerequisites env x
          ))
        | None -> R.Option None)
      in
      let v5 = map_anon_choice_recipe_9acf1ce env v5 in
      R.Tuple [v1; v2; v3; v4; v5]
    )
  | `Static_pat_rule (v1, v2, v3, v4, v5, v6, v7, v8) -> R.Case ("Static_pat_rule",
      let v1 = map_target_pattern env v1 in
      let v2 = (* ":" *) token env v2 in
      let v3 =
        (match v3 with
        | Some x -> R.Option (Some (
            map_imm_tok_pat_9713f58 env x
          ))
        | None -> R.Option None)
      in
      let v4 = map_target_pattern env v4 in
      let v5 = (* ":" *) token env v5 in
      let v6 =
        (match v6 with
        | Some x -> R.Option (Some (
            map_imm_tok_pat_9713f58 env x
          ))
        | None -> R.Option None)
      in
      let v7 =
        (match v7 with
        | Some x -> R.Option (Some (
            map_target_pattern env x
          ))
        | None -> R.Option None)
      in
      let v8 = map_anon_choice_recipe_9acf1ce env v8 in
      R.Tuple [v1; v2; v3; v4; v5; v6; v7; v8]
    )
  )

and map_thing (env : env) (x : CST.thing) =
  (match x with
  | `Rule x -> R.Case ("Rule",
      map_rule env x
    )
  | `Var_defi x -> R.Case ("Var_defi",
      map_variable_definition env x
    )
  | `Dire x -> R.Case ("Dire",
      map_directive env x
    )
  | `Func_imm_tok_pat_ba5cc43 (v1, v2) -> R.Case ("Func_imm_tok_pat_ba5cc43",
      let v1 = map_function_ env v1 in
      let v2 = map_imm_tok_pat_ba5cc43 env v2 in
      R.Tuple [v1; v2]
    )
  )

let map_makefile (env : env) (xs : CST.makefile) =
  R.List (List.map (map_thing env) xs)

let map_comment (env : env) (tok : CST.comment) =
  (* comment *) token env tok

let dump_tree root =
  map_makefile () root
  |> Tree_sitter_run.Raw_tree.to_channel stdout

let map_extra (env : env) (x : CST.extra) =
  match x with
  | Comment (_loc, x) -> ("comment", "comment", map_comment env x)

let dump_extras (extras : CST.extras) =
  List.iter (fun extra ->
    let ts_rule_name, ocaml_type_name, raw_tree = map_extra () extra in
    let details =
      if ocaml_type_name <> ts_rule_name then
        Printf.sprintf " (OCaml type '%s')" ocaml_type_name
      else
        ""
    in
    Printf.printf "%s%s:\n" ts_rule_name details;
    Tree_sitter_run.Raw_tree.to_channel stdout raw_tree
  ) extras
