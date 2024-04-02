open Unification
open TypeSystem
open Parsing

(* An implementation of typechecking generically for any language *)

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

(*
  Inputs an ast,
  and outputs a list of error messages.
  Also, later, I should make this output a list of warning or other kinds of messages, in order to
  deal with holes.
*)
let typecheck (lang : inductive) (topSort : term) (prog : program) : errorMessage list =
  let sub : sub ref = ref IntMap.empty in
  let equations : equation list ref = ref [] in
  let hiddenJudgements : sortConstraint list ref = ref [] in
  let disequalityConstraints : disequalityConstraint list ref = ref [] in
  let errorMessages : errorMessage list ref = ref [] in
  let makeError (errorMessage : errorMessage) : unit =
    errorMessages := errorMessage :: !errorMessages
  in
  let ctrLookup : fullConstructor StringMap.t = makeFastConstructorLookup lang in

  (* Either returns a list of new constraints to be solved, or returns None if nothing can be done here.
    If it returns Some, then it also statefully updates the equations, sub, and disequalityConstraints.
     *)
  let processConstraint (ct : sortConstraint) : sortConstraint list option =
    let fittingCtrs = List.filter_map (fun fctr ->
      let ctr = freshenRule fctr in
      let newEqs = [(ct.sort,ctr.conclusion)] @ ctr.equalities in
      Option.bind (unifyPartially !sub (newEqs @ !equations)) (fun (sub', equations') ->
        (* If any of the disequalities is actually in fact equal, then this constructor isn't possible: *)
        if
          not (List.for_all (fun (t1, t2) -> norm (metaSubst sub' t1) <> norm (metaSubst sub' t2)) ctr.disequalities)
          then None else
        Some (sub', ctr.equalities @ equations', ctr)
      )
    ) lang in
    match fittingCtrs with
    | [] -> 
        makeError {pos = ct.pos; message = "Constraint had no solution: " ^ show_term (metaSubst !sub ct.sort);};
        Some []
    | (sub', equations', ctr) :: [] ->
        (* If there is exactly one matching constructor, then we need to update the various enviroment to reflect the new constraints added.*)
        sub := sub';
        equations := equations';
        disequalityConstraints := (List.map (fun disequality -> {disequality; pos = ct.pos}) ctr.disequalities) @ !disequalityConstraints;
        let sorts = ctr.premises @ ctr.hiddenPremises in
        Some (List.map (fun sort -> {pos= ct.pos; sort}) sorts)
    | _ ->
      print_endline ("Constraint had multiple solutions: " ^ show_term (metaSubst !sub ct.sort) ^ ". They were " ^ (String.concat "," (List.map (fun (_, _, ctr) -> ctr.name) fittingCtrs)));
      None
  in

  let processConstraints (_ : unit) : unit =
    hiddenJudgements := List.fold_right (fun j acc -> 
        match processConstraint j with
        | None -> j :: acc
        | Some js -> js @ acc
      ) !hiddenJudgements []
  in

  let processDisequalities (_ : unit) : unit =
    disequalityConstraints := List.fold_right (fun {pos; disequality=(t1, t2);} acc ->
      let t1' = norm (metaSubst !sub t1) in
      let t2' = norm (metaSubst !sub t2) in
      if t1' = t2' then (makeError {pos; message = "Should not have been equal: " ^ show_term t1' ^ " = " ^ show_term t2'}; acc) else
      if reducedAreDefinitelyUnequal t1' t2' then acc else {pos; disequality = (t1', t2')} :: acc
      ) !disequalityConstraints []
  in

  (*TODO: Do something with equalities and disequalities!*)
  let rec typecheckImpl (sort : term) (prog : program) : unit =
    match prog with
    | Node ((AstString label, _, _), _) ->
      (* TODO: If the input sort is something like (Regex s "the_regex"), then it should do something in this case.  *)
      (match matchRegexSort sort with
      | Some (s, _regex) -> equations := (s, Const label) :: !equations (* TODO: I should probably have a StringLiteral constructor, instead of just Const. *)
      | None -> raise (Error "no"))
    | Node ((AstNode label, lpos, rpos) , kids) ->
      let pos = {left = lpos; right = rpos;} in
      let ctr = freshenRule (StringMap.find label ctrLookup) in
      processConstraints ();
      match unifyPartially !sub ((sort, ctr.conclusion) :: ctr.equalities @ !equations) with
      | None ->
        (* TODO: I could have it keep track of whichever parts of the sub did work successfully, and still try unifying the children. For now, I'll go with the simple option.*)
        makeError {pos; message = "unification failed here";};
        ()
      | Some (sub', equations') ->
        hiddenJudgements := List.map (fun t -> {sort = t; pos}) ctr.hiddenPremises @ !hiddenJudgements;
        disequalityConstraints := List.map (fun t -> {disequality = t; pos}) ctr.disequalities @ !disequalityConstraints;
        sub := sub';
        equations := equations';
        let _ = List.map2 (fun sort kid ->
          typecheckImpl sort kid
        ) ctr.premises kids
        in
        ()
    (* let ctr = StringMap.find  *)
  in
  
  typecheckImpl topSort prog;
  print_endline "At end of program here";
  processConstraints ();
  processDisequalities (); (* TODO: Should I call this throught inference as well? What would be the benefit - no facts can be deduced which help elsewhere?*)
  let leftoverConstraintErrors = List.map (fun ({pos; sort} : sortConstraint) -> {pos; message= "Constraint unresolved at end: " ^ show_term (metaSubst !sub sort)}) !hiddenJudgements in
  let leftoverDisequalityErrors = List.map (fun {pos; disequality= (t1, t2)} ->
    {pos; message = "Disequality unresolved at end: (" ^ show_term (metaSubst !sub t1) ^ " ?= " ^ show_term (metaSubst !sub t2) ^ ")"}) !disequalityConstraints in
  leftoverConstraintErrors @ leftoverDisequalityErrors @ !errorMessages
