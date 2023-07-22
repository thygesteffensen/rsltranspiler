module TranspilerTest.Compare

open NUnit.Framework
open Transpiler.Ast

let rec compareValueExpression ve1 ve2 =
    match ve1, ve2 with
    | ValueLiteral(vl1, _), ValueLiteral(vl2, _) -> Assert.AreEqual(vl1, vl2)
    | VName accessor1, VName accessor2 -> compareAccessor (accessor1, accessor2)
    | VPName accessor1, VPName accessor2 -> compareAccessor (accessor1, accessor2)
    | Quantified((quantifier1, _pos1), typings1, valueExpr1), Quantified((quantifier2, _pos2), typings2, valueExpr2) ->
        Assert.AreEqual(quantifier1, quantifier2)

        Assert.AreEqual(
            List.length typings1,
            List.length typings2,
            "Typing list length must be equal for quantified expression"
        )

        List.iter2 compareTyping typings1 typings2
        compareValueExpression valueExpr1 valueExpr2
    | Infix(lhs1, infixOp1, rhs1), Infix(lhs2, infixOp2, rhs2) ->
        Assert.AreEqual(infixOp1, infixOp2)
        compareValueExpression lhs1 lhs2
        compareValueExpression rhs1 rhs2
    | VeList vel1, VeList vel2 ->
        Assert.AreEqual(List.length vel1, List.length vel2, $"VeList {vel1} length is not equal to {vel2} length")
        List.iter2 compareValueExpression vel1 vel2
    | LogicalNegation (ve1, _pos1), LogicalNegation(ve2, _pos2) ->
        compareValueExpression ve1 ve2
    | _ -> Assert.Fail($"Value Expression {ve1} are not equal to {ve2}")

and compareAccessor (a1: Accessor, a2) =
    match a1, a2 with
    | ASimple(s1, _), ASimple(s2, _) -> Assert.AreEqual(s1, s2)
    | AGeneric((s1, _), valueExpr1), AGeneric((s2, _), valueExpr2) ->
        Assert.AreEqual(s1, s2)
        List.iter2 compareValueExpression valueExpr1 valueExpr2
    | _ -> Assert.Fail($"Accessor {a1} are not equal to {a2}")

and compareTypeExpression te1 te2 =
    match te1, te2 with
    | Literal tl1, Literal tl2 -> Assert.AreEqual(tl1, tl2)
    | TName(s1, _), TName(s2, _) -> Assert.AreEqual(s1, s2)
    | Product typeExpr1, Product typeExpr2 -> List.iter2 compareTypeExpression typeExpr1 typeExpr2
    | Set typeExpr1, Set typeExpr2 -> compareTypeExpression typeExpr1 typeExpr2
    | List typeExpr1, List typeExpr2 -> compareTypeExpression typeExpr1 typeExpr2
    | Map(typeExpr11, typeExpr12), Map(typeExpr21, typeExpr22) ->
        Assert.AreEqual(typeExpr11, typeExpr21)
        Assert.AreEqual(typeExpr12, typeExpr22)
    | Sub(typings1, valueExpression1), Sub(typings2, valueExpression2) ->
        List.iter2 compareTyping typings1 typings2
        compareValueExpression valueExpression1 valueExpression2
    | _ -> Assert.Fail($"Type Expression {te1} are not equal to {te2}")

and compareTyping t1 t2 =
    match t1, t2 with
    | SingleTyping(id1, typeExpr1), SingleTyping(id2, typeExpr2) ->
        compareIdentifier id1 id2
        compareTypeExpression typeExpr1 typeExpr2

and compareIdentifier id1 id2 =
    match id1, id2 with
    | ISimple(s1, _), ISimple(s2, _) -> Assert.AreEqual(s1, s2)
    | IGeneric((s1, _), typings1), IGeneric((s2, _), typings2) ->
        Assert.AreEqual(s1, s2)
        Assert.AreEqual(List.length typings1, List.length typings2, $"Typing list length must be equal for {s1} ")
        List.iter2 compareTyping typings1 typings2
    | _ -> Assert.Fail($"Identifier ${id1} are not equal to {id2}")

let compareIds (id1, _) (id2, _) = Assert.AreEqual(id1, id2)

let compareTypeDefinition ((id1, _), td1) ((id2, _), td2) =
    Assert.AreEqual(id1, id2)

    match td1, td2 with
    | Abstract, Abstract -> ()
    | Concrete typeExpr1, Concrete typeExpr2 -> compareTypeExpression typeExpr1 typeExpr2
    | Union ids1, Union ids2 ->
        Assert.AreEqual(List.length ids1, List.length ids1, "Union set must be equal size.")
        List.iter2 compareIds ids1 ids2
    | _ -> Assert.Fail($"Type definition {id1} are not equal to {id2}")




let compareValueDeclaration vd1 vd2 =
    match vd1, vd2 with
    | ExplicitValue(id1, typeExpr1, valueExpr1), ExplicitValue(id2, typeExpr2, valueExpr2) ->
        compareIdentifier id1 id2
        compareTypeExpression typeExpr1 typeExpr2
        compareValueExpression valueExpr1 valueExpr2
    | ImplicitValue, ImplicitValue -> failwith "todo"
    | ExplicitFunction, ExplicitFunction -> failwith "todo"
    | ImplicitFunction, ImplicitFunction -> failwith "todo"
    | GenericValue(id1, typings1, typeExpr1), GenericValue(id2, typings2, typeExpr2) ->
        compareIdentifier id1 id2
        List.iter2 compareTyping typings1 typings2
        compareTypeExpression typeExpr1 typeExpr2
    | Typing(SingleTyping(id1, typeExpr1)), Typing(SingleTyping(id2, typeExpr2)) ->
        compareIdentifier id1 id2
        compareTypeExpression typeExpr1 typeExpr2
    | _ -> Assert.Fail($"Value Declaration {vd1} are not equal to {vd2}")

let compareTransitionR ts1 ts2 =
    match ts1, ts2 with
    | Variable vd1, Variable vd2 -> List.iter2 compareValueDeclaration vd1 vd2
    | InitConstraint ve1, InitConstraint ve2 -> compareValueExpression ve1 ve2
    | TransitionRule(valueExpr1, l1), TransitionRule(valueExpr2, l2) ->
        compareValueExpression valueExpr1 valueExpr2
        Assert.AreEqual(List.length l1, List.length l2, "Named transition rules are not the same number")
        List.iter2 (fun (_, e1) (_, e2) -> compareValueExpression e1 e2) l1 l2
    | _ -> Assert.Fail($"Transition System {ts2} are not equal to {ts2}")


let compareDeclaration dec1 dec2 =
    match dec1, dec2 with
    | Value v1, Value v2 ->
        Assert.AreEqual(List.length v1, List.length v2, "Value Declaration list be equal length")
        List.iter2 compareValueDeclaration v1 v2
    | TypeDeclaration td1, TypeDeclaration td2 ->
        Assert.AreEqual(List.length td1, List.length td2)
        List.iter2 compareTypeDefinition td1 td2
    | AxiomDeclaration ad1, AxiomDeclaration ad2 -> List.iter2 compareValueExpression ad1 ad2
    | TransitionSystemDeclaration((s1, _), transitionSystems1), TransitionSystemDeclaration((s2, _), transitionSystems2) ->
        Assert.AreEqual(s1, s2)
        List.iter2 compareTransitionR transitionSystems1 transitionSystems2
    | _ -> Assert.Fail($"Declaration {dec1} \nare not equal to\n{dec2}")

let compareClass class1 class2 =
    Assert.AreEqual(List.length class1, List.length class2, "Class decl list must be equal")
    List.iter2 compareDeclaration class1 class2

let compareScheme (((id1, _), class1): Scheme, ((id2, _), class2): Scheme) =
    Assert.AreEqual(id1, id2)

    compareClass class1 class2
