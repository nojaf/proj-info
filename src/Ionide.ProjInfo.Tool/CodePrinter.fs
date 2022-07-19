module Ionide.ProjInfo.Tool.CodePrinter

open System.IO
open FSharp.Compiler.CodeAnalysis
open Ionide.ProjInfo.Types
open FSharp.Compiler.Text
open FSharp.Compiler.Syntax
open Fantomas
open CodeGenerator.ASTCreation

let private checker = FSharpChecker.Create()

let mkSynExprArrayOrListOfSeqExprOfStrings (isArray: bool) (isVerbatim: bool) (v: string list) : SynExpr =
    v
    |> List.map (
        if isVerbatim then
            mkSynExprConstString SynStringKind.Verbatim
        else
            mkSynExprConstRegularString
    )
    |> mkSynExprArrayOrListOfSeqExpr isArray

let private mdlFromSource (sourceText: string) =
    let parsingOptions =
        { FSharpParsingOptions.Default with
            SourceFiles = [| "dummy.fs" |]
            IsExe = true
            LangVersionText = "preview" }

    let untypedRes = checker.ParseFile("dummy.fs", SourceText.ofString sourceText, parsingOptions) |> Async.RunSynchronously

    match untypedRes.ParseTree with
    | ParsedInput.ImplFile (ParsedImplFileInput.ParsedImplFileInput(modules = [ SynModuleOrNamespace(decls = [ decl ]) ])) -> decl
    | _ -> failwithf $"invalid AST, %A{untypedRes.ParseTree}"

let private projectFunctionName (projectFileName: string) : string =
    Path
        .GetFileNameWithoutExtension(projectFileName)
        .Replace(".", "")

let private mapProject (project: ProjectOptions) : SynModuleDecl list =
    let projectName = projectFunctionName project.ProjectFileName

    if project.ProjectFileName.EndsWith(".csproj") then
        let bodyExpr =
            SynExpr.App(
                ExprAtomicFlag.Atomic,
                false,
                mkSynExprLongIdent "FSharpReferencedProject.CreatePortableExecutable",
                SynExpr.Paren(
                    SynExpr.Tuple(
                        false,
                        [ SynExpr.Const(SynConst.String(project.TargetPath, SynStringKind.Verbatim, zeroRange), zeroRange)
                          mkSynExprIdent "getTimeStamp"
                          mkSynExprIdent "loader" ],
                        [],
                        zeroRange
                    ),
                    zeroRange,
                    Some zeroRange,
                    zeroRange
                ),
                zeroRange
            )

        [ mkLet (mkBinding None [] (mkSynPatLongIdentSimple projectName (SynArgPats.Pats []) None) bodyExpr) ]
    else
        let mkSynExprRecordField name expr =
            SynExprRecordField((mkLids name, true), Some zeroRange, Some expr, None)

        let recordInstance: SynExpr =
            let referencedProjects =
                project.ReferencedProjects
                |> List.filter (fun p -> p.ProjectFileName.EndsWith(".fsproj"))
                |> List.map (fun p ->
                    let functionName = projectFunctionName p.ProjectFileName

                    if p.ProjectFileName.EndsWith(".csproj") then
                        mkSynExprIdent functionName
                    else
                        SynExpr.App(ExprAtomicFlag.NonAtomic, false, mkSynExprIdent $"memo{functionName}", (mkSynExprConstRegularString functionName), zeroRange))
                |> mkSynExprArrayOrListOfSeqExpr true

            let stampExpr = noneExpr
            // System.Text.RegularExpressions.Regex.Match(project.ProjectFileName, @"\d+").Value |> (int64)
            // (mkSynExprAppNonAtomic (mkSynExprIdent "Some") (mkSynExprConst (mkSynConstInt64 stamp)))

            SynExpr.Record(
                None,
                None,
                [ mkSynExprRecordField "ProjectFileName" (mkSynExprConstString SynStringKind.Verbatim project.ProjectFileName)
                  mkSynExprRecordField "ProjectId" noneExpr
                  mkSynExprRecordField "SourceFiles" (mkSynExprArrayOrListOfSeqExprOfStrings true true project.SourceFiles)
                  mkSynExprRecordField "OtherOptions" (mkSynExprArrayOrListOfSeqExprOfStrings true true project.OtherOptions)
                  mkSynExprRecordField "ReferencedProjects" referencedProjects
                  mkSynExprRecordField "IsIncompleteTypeCheckEnvironment" falseExpr
                  mkSynExprRecordField "UseScriptResolutionRules" falseExpr
                  mkSynExprRecordField "LoadTime" (mkSynExprLongIdent "DateTime.Now")
                  mkSynExprRecordField "UnresolvedReferences" noneExpr
                  mkSynExprRecordField "OriginalLoadReferences" (SynExpr.ArrayOrList(false, [], zeroRange))
                  mkSynExprRecordField "Stamp" stampExpr ],
                zeroRange
            )

        let createFSharpExpr =
            SynExpr.App(
                ExprAtomicFlag.Atomic,
                false,
                mkSynExprLongIdent "FSharpReferencedProject.CreateFSharp",
                SynExpr.Paren(
                    SynExpr.Tuple(
                        false,
                        [ SynExpr.Const(SynConst.String(project.TargetPath, SynStringKind.Verbatim, zeroRange), zeroRange)
                          mkSynExprIdent "projectOptions" ],
                        [],
                        zeroRange
                    ),
                    zeroRange,
                    Some zeroRange,
                    zeroRange
                ),
                zeroRange
            )

        let args = [ synPatWild ]
        let bodyExpr = mkSynExprLetOrUse (mkLocalValueBinding [] "projectOptions" recordInstance) createFSharpExpr

        [ mkLet (mkBinding None [] (mkSynPatLongIdentSimple projectName (SynArgPats.Pats args) None) bodyExpr)
          mkLet (mkLocalValueBinding [] $"memo{projectName}" (mkSynExprAppNonAtomic (mkSynExprIdent "memoization") (mkSynExprIdent projectName))) ]

let printCode (projects: ProjectOptions list) =
    let ast =
        let decls: SynModuleDecl list =
            [ yield mkOpenDecl "System"
              yield mkOpenDecl "System.Collections.Generic"
              yield mkOpenDecl "FSharp.Compiler.CodeAnalysis"
              yield mdlFromSource "let private getTimeStamp () = DateTime.Now"
              yield mdlFromSource "let private loader _ = None"
              yield
                  mdlFromSource
                      """
let memoization (f: string -> 'a) =
    let cache = Dictionary<_, _>()

    (fun x ->
        match cache.TryGetValue(x) with
        | true, cachedValue -> cachedValue
        | _ ->
            let result = f x
            cache.Add(x, result)
            result)
"""
              yield! (List.filter (fun p -> p.ProjectFileName.EndsWith(".fsproj")) projects) |> List.collect mapProject ]

        ParsedInput.ImplFile(
            ParsedImplFileInput.ParsedImplFileInput(
                "tmp.fs",
                false,
                QualifiedNameOfFile.QualifiedNameOfFile(mkIdent "Tmp$fs"),
                [],
                [],
                [ SynModuleOrNamespace.SynModuleOrNamespace(mkLid "FSharp.Compiler.Service.Tests.LargeAppProjects", true, SynModuleOrNamespaceKind.NamedModule, decls, zeroXml, [], None, zeroRange) ],
                (true, true)
            )
        )

    CodeFormatter.FormatASTAsync(ast, "tmp.fs", [], None, FormatConfig.FormatConfig.Default)
    |> Async.RunSynchronously
    |> fun source -> File.WriteAllText(@"C:\Users\nojaf\Downloads\fcs-project.fs", source)
