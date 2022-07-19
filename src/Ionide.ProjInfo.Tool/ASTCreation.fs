module CodeGenerator.ASTCreation

open FSharp.Compiler.Syntax
open FSharp.Compiler.Text
open FSharp.Compiler.Xml
open Fantomas

type Definition = { MemberName: string; Uri: string }

let zeroRange: Range = Range.Zero
let zeroXml: PreXmlDoc = PreXmlDoc.Empty
let mkIdent (name: string) : Ident = Ident(name, zeroRange)

let mkLid (name: string) : LongIdent =
    name.Split('.') |> Array.toList |> List.map mkIdent

let mkLids (name: string) : LongIdentWithDots =
    LongIdentWithDots.LongIdentWithDots(mkLid name, [])

/// SynType
let mkSynTypeLongIdent (name: string) : SynType = SynType.LongIdent(mkLids name)

/// SynPat
let mkSynTypeApp (typeName: SynType) (typeArgs: SynType list) (isPostFix: bool) : SynType =
    SynType.App(typeName, None, typeArgs, [], None, isPostFix, zeroRange)

let mkSynTypeTypeOfConcretePostFix (typeName: string) (t: string) : SynType =
    mkSynTypeApp (mkSynTypeLongIdent t) [ mkSynTypeLongIdent typeName ] true

let mkSynPatNamed (name: string) : SynPat =
    SynPat.Named(mkIdent name, false, None, zeroRange)

let mkSynPatTypedSimple (name: string) (typ: string) : SynPat =
    SynPat.Typed(mkSynPatNamed name, mkSynTypeLongIdent typ, zeroRange)

let mkSynPatTyped (name: string) (t: SynType) : SynPat =
    SynPat.Typed(mkSynPatNamed name, t, zeroRange)

let mkSynPatTuple (ts: SynPat list) : SynPat = SynPat.Tuple(false, ts, zeroRange)

let unitPat: SynPat = SynPat.Const(SynConst.Unit, zeroRange)

let mkSynPatParen (pat: SynPat) : SynPat = SynPat.Paren(pat, zeroRange)

let mkSynPatParenTyped (name: string) (typ: SynType) : SynPat =
    SynPat.Paren(SynPat.Typed(mkSynPatNamed name, typ, zeroRange), zeroRange)

let mkSynPatLongIdentSimple (name: string) (argPats: SynArgPats) (accessibility: SynAccess option) : SynPat =
    SynPat.LongIdent(mkLids name, None, None, None, argPats, accessibility, zeroRange)

let mkSynPatConstString (value: string) : SynPat =
    SynPat.Const(SynConst.String(value, SynStringKind.Regular, zeroRange), zeroRange)

let synPatWild: SynPat = SynPat.Wild(zeroRange)

let mkTypedParameter (name: string) (targetType: SynType) : SynPat =
    SynPat.Paren(SynPat.Typed(mkSynPatNamed name, targetType, zeroRange), zeroRange)

let mkBinding (memberFlags: SynMemberFlags option) (attributes: SynAttributes) (signature: SynPat) (bodyExpr: SynExpr) : SynBinding =
    SynBinding.SynBinding(
        None,
        SynBindingKind.Normal,
        false,
        false,
        attributes,
        zeroXml,
        SynValData.SynValData(memberFlags, SynValInfo.SynValInfo([], SynArgInfo.SynArgInfo([], false, None)), None),
        signature,
        None,
        bodyExpr,
        zeroRange,
        DebugPointAtBinding.NoneAtLet,
        { EqualsRange = Some zeroRange
          LetKeyword = Some zeroRange }
    )

let mkLocalValueBinding (attributes: SynAttributes) (name: string) (expr: SynExpr) : SynBinding =
    mkBinding None attributes (mkSynPatNamed name) expr

let mkSynConstInt32 (value: int) : SynConst = SynConst.Int32(value)

let mkSynConstInt64 (value: int64) : SynConst = SynConst.Int64(value)

/// SynExpr
let unitExpr: SynExpr = SynExpr.Const(SynConst.Unit, zeroRange)

let mkSynExprIdent (name: string) : SynExpr = SynExpr.Ident(mkIdent name)

let mkSynExprApp (flag: ExprAtomicFlag) (isInfix: bool) (funcExpr: SynExpr) (argExpr: SynExpr) : SynExpr =
    SynExpr.App(flag, isInfix, funcExpr, argExpr, zeroRange)

let mkSynExprAppAtomic (funcExpr: SynExpr) (argExpr: SynExpr) : SynExpr =
    mkSynExprApp ExprAtomicFlag.Atomic false funcExpr argExpr

let mkSynExprAppNonAtomic (funcExpr: SynExpr) (argExpr: SynExpr) : SynExpr =
    mkSynExprApp ExprAtomicFlag.NonAtomic false funcExpr argExpr

let mkSynExprInfixApp (operator: string) (argExpr: SynExpr) : SynExpr =
    mkSynExprApp ExprAtomicFlag.NonAtomic true (mkSynExprIdent operator) argExpr

let mkSynExprInfixPipeRight (pipedExpr: SynExpr) (toExpr: SynExpr) : SynExpr =
    mkSynExprAppNonAtomic (mkSynExprInfixApp "op_PipeRight" pipedExpr) toExpr

let mkSynExprInfixComposeRight (pipedExpr: SynExpr) (toExpr: SynExpr) : SynExpr =
    mkSynExprAppNonAtomic (mkSynExprInfixApp "op_ComposeRight" pipedExpr) toExpr

let mkSynExprInfixAddition (addExpr: SynExpr) (toExpr: SynExpr) : SynExpr =
    mkSynExprAppNonAtomic (mkSynExprInfixApp "op_Addition" addExpr) toExpr

let mkSynExprAppNonAtomicExprMultipleArgs (es: SynExpr list) : SynExpr =
    let rec loop (es: SynExpr list) (finalContinuation: SynExpr -> SynExpr) : SynExpr =
        match es with
        | [] -> unitExpr
        | [ h ] -> finalContinuation h
        | h :: rest -> loop rest (fun e -> mkSynExprAppNonAtomic e h |> finalContinuation)

    loop (List.rev es) id

let mkSynExprLongIdent (name: string) : SynExpr =
    SynExpr.LongIdent(false, mkLids name, None, zeroRange)

let mkSynExprParen (e: SynExpr) : SynExpr =
    SynExpr.Paren(e, zeroRange, None, zeroRange)

let mkSynExprTyped (t: SynType) (e: SynExpr) : SynExpr = SynExpr.Typed(e, t, zeroRange)

let mkSynExprLetOrUse (binding: SynBinding) (expr: SynExpr) : SynExpr =
    SynExpr.LetOrUse(false, false, [ binding ], expr, zeroRange, { InKeyword = None })

let mkSynExprSeq (e1: SynExpr) (e2: SynExpr) : SynExpr =
    SynExpr.Sequential(DebugPointAtSequential.SuppressBoth, true, e1, e2, zeroRange)

let mkSynExprNew (name: string) (expr: SynExpr) : SynExpr =
    SynExpr.New(false, mkSynTypeLongIdent name, expr, zeroRange)

let mkSynExprConst (synConst: SynConst) : SynExpr = SynExpr.Const(synConst, zeroRange)

let mkSynExprConstString stringKind (value: string) : SynExpr =
    SynExpr.Const(SynConst.String(value, stringKind, zeroRange), zeroRange)

let mkSynExprConstRegularString (value: string) : SynExpr =
    mkSynExprConstString SynStringKind.Regular value

let mkSynExprRecord (fields: (string * SynExpr) list) : SynExpr =
    let fields =
        fields |> List.map (fun (n, v) -> SynExprRecordField(RecordFieldName(mkLids n, true), Some zeroRange, Some v, None))

    SynExpr.Record(None, None, fields, zeroRange)

let mkSynExprTuple (es: SynExpr list) : SynExpr = SynExpr.Tuple(false, es, [], zeroRange)

let mkSynExprLambda (pats: SynPat list) (bodyExpr: SynExpr) =
    SynExpr.Lambda(false, false, SynSimplePats.SimplePats([], zeroRange), unitExpr, Some(pats, bodyExpr), zeroRange, { ArrowRange = Some zeroRange })

let mkSynExprMatch (matchExpr: SynExpr) (clauses: SynMatchClause list) : SynExpr =
    SynExpr.Match(zeroRange, DebugPointAtBinding.NoneAtDo, matchExpr, zeroRange, clauses, zeroRange)

let mkSynMatchClause (pattern: SynPat) (whenExpr: SynExpr option) (resultExpr: SynExpr) : SynMatchClause =
    SynMatchClause.SynMatchClause(
        pattern,
        whenExpr,
        resultExpr,
        zeroRange,
        DebugPointAtTarget.No,
        { ArrowRange = Some zeroRange
          BarRange = Some zeroRange }
    )

let mkSynExprIfThenElse (ifExpr: SynExpr) (thenExpr: SynExpr) (elseExpr: SynExpr option) : SynExpr =
    SynExpr.IfThenElse(
        ifExpr,
        thenExpr,
        elseExpr,
        DebugPointAtBinding.NoneAtLet,
        false,
        zeroRange,
        { ElseKeyword = Option.map (fun _ -> zeroRange) elseExpr
          IfKeyword = zeroRange
          IfToThenRange = zeroRange
          IsElif = false
          ThenKeyword = zeroRange }
    )

let mkSynExprArrayOrListOfSeqExpr (isArray: bool) (xs: SynExpr list) : SynExpr =
    match xs with
    | [] -> SynExpr.ArrayOrList(isArray, [], zeroRange)
    | [ single ] -> SynExpr.ArrayOrListComputed(isArray, single, zeroRange)
    | [ a; b ] -> SynExpr.ArrayOrListComputed(isArray, (mkSynExprSeq a b), zeroRange)
    | xs ->
        let rec loop (xs: SynExpr list) : SynExpr =
            match xs with
            | []
            | [ _ ] -> unitExpr
            | [ a; b ] -> mkSynExprSeq a b
            | h :: tail -> mkSynExprSeq h (loop tail)

        let seqExpr = loop xs
        SynExpr.ArrayOrListComputed(isArray, seqExpr, zeroRange)

let noneExpr: SynExpr = mkSynExprIdent "None"
let falseExpr: SynExpr = SynExpr.Const(SynConst.Bool(false), zeroRange)

let mkSynComponentInfo (typeName: string) (attributes: SynAttributes) : SynComponentInfo =
    SynComponentInfo.SynComponentInfo(attributes, None, [], [ mkIdent typeName ], zeroXml, false, None, zeroRange)

let mkSynField (name: string) (typeName: SynType) : SynField =
    SynField.SynField([], false, Some(mkIdent name), typeName, false, zeroXml, None, zeroRange)

let mkStaticMember (name: string) (typarDecls: SynValTyparDecls option) (parameters: SynPat list) (bodyExpr: SynExpr) : SynMemberDefn =
    let binding =
        let name = SynPat.LongIdent(mkLids name, None, None, typarDecls, SynArgPats.Pats parameters, None, zeroRange)

        mkBinding
            (Some
                { IsInstance = false
                  IsDispatchSlot = false
                  IsOverrideOrExplicitImpl = false
                  IsFinal = false
                  MemberKind = SynMemberKind.Member
                  Trivia =
                    { AbstractRange = None
                      DefaultRange = None
                      MemberRange = Some zeroRange
                      OverrideRange = None
                      StaticRange = Some zeroRange } })
            []
            name
            bodyExpr

    SynMemberDefn.Member(binding, zeroRange)

let mkImplicitCtor () =
    SynMemberDefn.ImplicitCtor(None, [], SynSimplePats.SimplePats([], zeroRange), None, zeroXml, zeroRange)

let mkInterface ifType members =
    SynMemberDefn.Interface(ifType, None, members, zeroRange)


let mkOverrideMember (name: string) (typarDecls: SynValTyparDecls option) (parameters: SynPat list) (bodyExpr: SynExpr) : SynMemberDefn =
    let binding =
        let name = SynPat.LongIdent(mkLids name, None, None, typarDecls, SynArgPats.Pats parameters, None, zeroRange)

        mkBinding
            (Some
                { IsInstance = true
                  IsDispatchSlot = false
                  IsOverrideOrExplicitImpl = true
                  IsFinal = false
                  MemberKind = SynMemberKind.Member
                  Trivia =
                    { AbstractRange = None
                      DefaultRange = None
                      MemberRange = None
                      OverrideRange = Some zeroRange
                      StaticRange = None } })
            []
            name
            bodyExpr

    SynMemberDefn.Member(binding, zeroRange)

let implicitCtor: SynMemberDefn =
    SynMemberDefn.ImplicitCtor(None, [], SynSimplePats.SimplePats([], zeroRange), None, zeroXml, zeroRange)

let mkMember (name: string) (typarDecls: SynValTyparDecls option) (attributes: SynAttributes) (parameters: SynPat list) (bodyExpr: SynExpr) : SynMemberDefn =
    let binding =
        let name = SynPat.LongIdent(mkLids name, None, None, typarDecls, SynArgPats.Pats parameters, None, zeroRange)

        mkBinding
            (Some
                { IsInstance = true
                  IsDispatchSlot = false
                  IsOverrideOrExplicitImpl = false
                  IsFinal = false
                  MemberKind = SynMemberKind.Member
                  Trivia =
                    { AbstractRange = None
                      DefaultRange = None
                      MemberRange = Some zeroRange
                      OverrideRange = None
                      StaticRange = None } })
            attributes
            name
            bodyExpr

    SynMemberDefn.Member(binding, zeroRange)

let mkSynTypeDefnSimpleReprRecord (fields: SynField list) : SynTypeDefnSimpleRepr =
    SynTypeDefnSimpleRepr.Record(None, fields, zeroRange)

let mkTypeDefnRecordWithAttributes (name: string) (fields: SynField list) (members: SynMemberDefn list) (attributes: SynAttributes) =
    SynTypeDefn.SynTypeDefn(
        mkSynComponentInfo name attributes,
        SynTypeDefnRepr.Simple(mkSynTypeDefnSimpleReprRecord fields, zeroRange),
        members,
        None,
        zeroRange,
        { EqualsRange = Some zeroRange
          TypeKeyword = Some zeroRange
          WithKeyword = None }
    )

// not very functional and not suitable for partial application,
// but tried to keep the principle of ordering params as in other functions
let mkTypeDefnRecord (name: string) (fields: SynField list) (members: SynMemberDefn list) : SynTypeDefn =
    mkTypeDefnRecordWithAttributes name fields members []

let mkSynTypeDefnSimpleReprObjectModel (members: SynMemberDefn list) : SynTypeDefnRepr =
    match members with
    | [] -> SynTypeDefnRepr.ObjectModel(SynTypeDefnKind.Class, [], zeroRange)
    | _ -> SynTypeDefnRepr.ObjectModel(SynTypeDefnKind.Unspecified, members, zeroRange)


let mkTypeDefnObjectModel (name: string) (members: SynMemberDefn list) : SynTypeDefn =
    SynTypeDefn.SynTypeDefn(
        mkSynComponentInfo name [],
        mkSynTypeDefnSimpleReprObjectModel members,
        [],
        None,
        zeroRange,
        { EqualsRange = Some zeroRange
          TypeKeyword = Some zeroRange
          WithKeyword = None }
    )

let mkSynTypeDefnSimpleReprUnion (ao: SynAccess option) (cases: SynUnionCase list) : SynTypeDefnSimpleRepr =
    SynTypeDefnSimpleRepr.Union(ao, cases, zeroRange)

let mkTypeDefnUnion (name: string) (ao: SynAccess option) (attributes: SynAttributes) (cases: SynUnionCase list) (members: SynMemberDefn list) : SynTypeDefn =
    SynTypeDefn.SynTypeDefn(
        mkSynComponentInfo name attributes,
        SynTypeDefnRepr.Simple(mkSynTypeDefnSimpleReprUnion ao cases, zeroRange),
        members,
        None,
        zeroRange,
        { EqualsRange = Some zeroRange
          TypeKeyword = Some zeroRange
          WithKeyword = None }
    )

let mkSynTypeDefnSimpleReprEnum (enumCases: SynEnumCase list) : SynTypeDefnSimpleRepr =
    SynTypeDefnSimpleRepr.Enum(enumCases, zeroRange)

let mkTypeDefnEnum (name: string) (attributes: SynAttributes) (enumCases: SynEnumCase list) (members: SynMemberDefn list) : SynTypeDefn =
    SynTypeDefn.SynTypeDefn(
        mkSynComponentInfo name attributes,
        SynTypeDefnRepr.Simple(mkSynTypeDefnSimpleReprEnum enumCases, zeroRange),
        members,
        None,
        zeroRange,
        { EqualsRange = Some zeroRange
          TypeKeyword = Some zeroRange
          WithKeyword = None }
    )

let mkSynEnumCase (name: string) (value: int) : SynEnumCase =
    SynEnumCase.SynEnumCase(
        [],
        mkIdent name,
        mkSynConstInt32 value,
        zeroRange,
        zeroXml,
        zeroRange,
        { EqualsRange = zeroRange
          BarRange = Some zeroRange }
    )

let mkSynUnionCase (name: string) (cases: SynField list) : SynUnionCase =
    let fields = SynUnionCaseKind.Fields cases
    SynUnionCase.SynUnionCase([], mkIdent name, fields, zeroXml, None, zeroRange, { BarRange = Some zeroRange })

let mkTypes (types: SynTypeDefn list) : SynModuleDecl = SynModuleDecl.Types(types, zeroRange)

let mkLet (binding: SynBinding) : SynModuleDecl =
    SynModuleDecl.Let(false, [ binding ], zeroRange)

let mkOpenDecl (name: string) : SynModuleDecl =
    SynModuleDecl.Open(SynOpenDeclTarget.ModuleOrNamespace(mkLid name, zeroRange), zeroRange)

let mkSynAttribute (name: string) (argExpr: SynExpr) : SynAttributeList =
    { Attributes =
        [ { TypeName = mkLids name
            ArgExpr = argExpr
            Target = None
            AppliesToGetterAndSetter = false
            Range = zeroRange } ]
      Range = zeroRange }

let requireQualifiedAccess: SynAttributes = [ mkSynAttribute "RequireQualifiedAccess" unitExpr ]

let literal: SynAttributes = [ mkSynAttribute "Literal" unitExpr ]
