open Unification
open TypeSystem
open Parsing

(* An implementation of typechecking generically for any language *)

type preErrorMessage = {
  pos : spanPosition;
  preMessage : sub -> string;
}

type errorMessage = {
  pos : spanPosition;
  message : string;
}

let show_errorMessasge (e : errorMessage) : string =
  "Error at " ^ show_position e.pos.left ^ "-" ^ show_position e.pos.right ^ ": " ^ e.message

type sortConstraint = {
  pos : spanPosition;
  sort : term;
}

type disequalityConstraint = {
  pos : spanPosition;
  disequality : term * term;
}

type equalityConstraint = spanPosition * (term * term)

(*
  Inputs an ast,
  and outputs a list of error messages.
  Also, later, I should make this output a list of warning or other kinds of messages, in order to
  deal with holes.
*)
let typecheck (lang : inductive) (topSort : term) (prog : program) : errorMessage list =
  let sub : sub ref = ref IntMap.empty in
  let equations : equalityConstraint list ref = ref [] in
  let hiddenJudgements : sortConstraint list ref = ref [] in
  let disequalityConstraints : disequalityConstraint list ref = ref [] in
  let errorMessages : preErrorMessage list ref = ref [] in
  let makeError (errorMessage : preErrorMessage) : unit =
    errorMessages := errorMessage :: !errorMessages
  in
  let ctrLookup : fullConstructor StringMap.t = makeFastConstructorLookup lang in

  (* Either returns a list of new constraints to be solved, or returns None if nothing can be done here.
    If it returns Some, then it also statefully updates the equations, sub, and disequalityConstraints.
     *)
  let processConstraint (ct : sortConstraint) : sortConstraint list option =
    (* print_endline ("Processing constraint: " ^ show_term (metaSubst !sub ct.sort)); *)
    let fittingCtrs = List.filter_map (fun fctr ->
      let ctr = freshenRule fctr in
      let newEqs = [(ct.sort,ctr.conclusion)] @ ctr.equalities in
      Option.bind (unifyPartially !sub ((List.map (fun eq -> ct.pos, eq) newEqs) @ !equations)) (fun (sub', equations') ->
        (* If any of the disequalities is actually in fact equal, then this constructor isn't possible: *)
        if
          not (List.for_all (fun (t1, t2) -> norm (metaSubst sub' t1) <> norm (metaSubst sub' t2)) ctr.disequalities)
          then None else
        Some (sub', (List.map (fun eq -> ct.pos, eq) ctr.equalities) @ equations', ctr)
      )
    ) lang in
    match fittingCtrs with
    | [] -> 
        makeError {pos = ct.pos; preMessage = fun sub -> "Constraint had no solution: " ^ show_term (metaSubst sub ct.sort);};
        Some []
    | (sub', equations', ctr) :: [] ->
        (* If there is exactly one matching constructor, then we need to update the various enviroment to reflect the new constraints added.*)
        sub := sub';
        equations := equations';
        disequalityConstraints := (List.map (fun disequality -> {disequality; pos = ct.pos}) ctr.disequalities) @ !disequalityConstraints;
        let sorts = ctr.premises @ ctr.hiddenPremises in
        Some (List.map (fun sort -> {pos= ct.pos; sort}) sorts)
    | _ ->
      (* print_endline ("Constraint had multiple solutions: " ^ show_term (metaSubst !sub ct.sort) ^ ". They were " ^ (String.concat "," (List.map (fun (_, _, ctr) -> ctr.name) fittingCtrs))); *)
      None
  in

  let rec processConstraints (_ : unit) : unit =
    let cts, changed = List.fold_right (fun j (acc, anyChanged) -> 
        match processConstraint j with
        | None -> j :: acc, anyChanged
        | Some js -> js @ acc, true
      ) !hiddenJudgements ([], false)
    in
    hiddenJudgements := cts;
    if changed then processConstraints () else ()
  in

  let processDisequalities (_ : unit) : unit =
    disequalityConstraints := List.fold_right (fun {pos; disequality=(t1, t2);} acc ->
      let t1' = norm (metaSubst !sub t1) in
      let t2' = norm (metaSubst !sub t2) in
      if t1' = t2' then (makeError {pos; preMessage = fun sub -> "Should not have been equal: " ^ show_term (metaSubst sub t1') ^ " = " ^ show_term (metaSubst sub t2')}; acc) else
      if reducedAreDefinitelyUnequal t1' t2' then acc else {pos; disequality = (t1', t2')} :: acc
      ) !disequalityConstraints []
  in

  (*TODO: Do something with equalities and disequalities!*)
  let rec typecheckImpl (sort : term) (prog : program) : unit =
    match prog with
    | Node ((AstString label, lpos, rpos), _) ->
      (* TODO: If the input sort is something like (Regex s "the_regex"), then it should do something in this case.  *)
      (match matchRegexSort sort with
      | Some (s, _regex) -> equations := ({left=lpos; right=rpos}, (s, Const label)) :: !equations (* TODO: I should probably have a StringLiteral constructor, instead of just Const. *)
      | None -> raise (Error "no"))
    | Node ((AstNode label, lpos, rpos) , kids) ->
      let pos = {left = lpos; right = rpos;} in
      let _ = if not (StringMap.mem label ctrLookup) then print_endline ("Not foudn:::: " ^ label) else () in
      let ctr = freshenRule (StringMap.find label ctrLookup) in
      (* processConstraints (); *)
      (* print_endline("At ctr. " ^ ctr.name ^ " unifying: " ^ show_term sort ^ " with " ^ show_term ctr.conclusion ^ " and sub is " ^ show_sub !sub); *)
      match unifyPartially !sub ((List.map (fun e -> (pos, e)) ((sort, ctr.conclusion) :: ctr.equalities)) @ !equations) with
      | None ->
        (* TODO: I could have it keep track of whichever parts of the sub did work successfully, and still try unifying the children. For now, I'll go with the simple option.*)
        makeError {pos; preMessage = (fun sub -> "unification failed here: " ^ (show_term (metaSubst sub sort)) ^ " did not unify with " ^ (show_term (metaSubst sub ctr.conclusion)));};
        ()
      | Some (sub', equations') ->
        hiddenJudgements := List.map (fun t -> {sort = t; pos}) ctr.hiddenPremises @ !hiddenJudgements;
        disequalityConstraints := List.map (fun t -> {disequality = t; pos}) ctr.disequalities @ !disequalityConstraints;
        sub := sub';
        equations := equations';
        (* print_endline ("Now after unify, sub is: " ^ show_sub !sub); *)
        let _ = List.map2 (fun sort kid ->
          typecheckImpl sort kid
        ) ctr.premises kids
        in
        ()
    (* let ctr = StringMap.find  *)
  in
  
  typecheckImpl topSort prog;
  processConstraints ();
  processDisequalities (); (* TODO: Should I call this throught inference as well? What would be the benefit - no facts can be deduced which help elsewhere?*)
  let leftoverConstraintErrors : preErrorMessage list = List.map (fun ({pos; sort} : sortConstraint) -> {pos; preMessage= fun sub -> "Constraint unresolved at end: " ^ show_term (metaSubst sub sort)}) !hiddenJudgements in
  let leftoverDisequalityErrors : preErrorMessage list = List.map (fun {pos; disequality= (t1, t2)} ->
    {pos; preMessage = fun sub -> "Disequality unresolved at end: (" ^ show_term (metaSubst sub t1) ^ " ?= " ^ show_term (metaSubst sub t2) ^ ")"}) !disequalityConstraints in
  let leftoverEqualities : preErrorMessage list =
    List.map (fun (pos, (t1, t2)) ->
    {pos; preMessage = fun sub -> "Equality unresolved at end: (" ^ show_term (metaSubst sub t1) ^ " ?= " ^ show_term (metaSubst sub t2) ^ ")"}) !equations in
  (List.map (fun {pos; preMessage} -> {pos; message = preMessage !sub})
    (leftoverEqualities @ leftoverConstraintErrors @ leftoverDisequalityErrors @ !errorMessages))
