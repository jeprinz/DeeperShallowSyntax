open Unification
open TypeSystem
open Parsing

(* An implementation of typechecking generically for any language *)

type errorMessage = {
  pos : spanPosition;
  message : string;
}

type sortConstraint = {
  pos : spanPosition;
  sort : term;
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
  let errorMessages : errorMessage list ref = ref [] in
  let makeError (errorMessage : errorMessage) : unit =
    errorMessages := errorMessage :: !errorMessages
  in
  let ctrLookup : fullConstructor StringMap.t = makeFastConstructorLookup lang in

  (* Either returns a list of new constraints to be solved, or returns None if nothing can be done here. *)
  let processConstraint (ct : sortConstraint) : sortConstraint list option =
    let fittingCtrs = List.filter_map (fun fctr ->
      let ctr = freshenRule fctr in
      let newEqs = [(ct.sort,ctr.conclusion)] @ ctr.equalities in
      (* TODO: Do something with disequalities!*)
      Option.bind (unifyPartially !sub (newEqs @ !equations)) (fun (sub', equations') ->
        Some (sub', equations', ctr)
      )
    ) lang in
    match fittingCtrs with
    | [] -> 
        makeError {pos = ct.pos; message = "Constraint had no solution";};
        Some []
    | (sub', equations', ctr) :: [] ->
        sub := sub';
        equations := equations';
        let sorts = ctr.premises @ ctr.hiddenPremises in
        Some (List.map (fun sort -> {pos= ct.pos; sort}) sorts)
    | _ -> None
  in

  let processConstraints : unit =
    hiddenJudgements := List.fold_right (fun j acc -> 
        match processConstraint j with
        | None -> j :: acc
        | Some js -> js @ acc
      ) [] !hiddenJudgements
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
      processConstraints;
      match unifyPartially !sub !equations with
      | None ->
        (* TODO: I could have it keep track of whichever parts of the sub did work successfully, and still try unifying the children. For now, I'll go with the simple option.*)
        makeError {pos; message = "unification failed here";};
        ()
      | Some (sub', equations') ->
        hiddenJudgements := List.map (fun t -> {sort = t; pos}) ctr.hiddenPremises @ !hiddenJudgements;
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
  processConstraints;
  !errorMessages
