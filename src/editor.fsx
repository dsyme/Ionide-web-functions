#r "../node_modules/fable-core/Fable.Core.dll"
#r "../node_modules/fable-powerpack/Fable.PowerPack.dll"
#load "Fable.Import.Monaco.fsx"
#load "utils.fsx"
#load "dto.fsx"
#load "service.fsx"

open System
open Fable.Core
open Fable.Core.JsInterop
open Fable.Import
open Fable.Import.monaco
open Fable.PowerPack
open Utils
open Dto
open Service

[<RequireQualifiedAccess>]
module CodeRange =
    type CodeRange = Fable.Import.monaco.Range

    /// Converts Range DTO to VS Code Range.
    let fromDTO (range: Range) : CodeRange =
        CodeRange (float range.StartLine,
                   float range.StartColumn,
                   float range.EndLine,
                   float range.EndColumn)

let hoverProvider = {
    new languages.HoverProvider
    with
        member this.provideHover(model, position, token) =
            promise {
                let createCommentBlock (comment: string) : MarkedString[] =
                    comment.Split '\n'
                    |> Array.filter(String.IsNullOrWhiteSpace>>not)
                    |> Array.mapi (fun i line ->
                        let v =
                            if i = 0 && not (String.IsNullOrWhiteSpace line)
                            then "\n" + line.Trim()
                            else line.Trim()
                        Markdown.replaceXml v
                    )
                    |> String.concat "\n\n"
                    |> String.trim
                    |> Case1
                    |> Array.singleton

                let! o = tooltip (model.getValue()) (!! position.lineNumber) (!! position.column)
                match o with
                | None -> return !!null
                | Some o ->
                let res = (o |> Array.collect id).[0]
                let h = createEmpty<languages.Hover>
                let w = model.getWordAtPosition(position |> unbox)
                let range = Range(position.lineNumber, w.startColumn, position.lineNumber, w.endColumn )

                let markStr lang (value:string) : MarkedString =
                    createObj [
                        "language" ==> lang
                        "value" ==> value.Trim()
                    ] |> Case2

                let fsharpBlock (lines: string[]) : MarkedString =
                    lines |> String.concat "\n" |> markStr "fsharp"

                let sigContent =
                    let lines =
                        res.Signature
                        |> String.split [|'\n'|]
                        |> Array.filter (not << String.IsNullOrWhiteSpace)

                    match lines |> Array.splitAt (lines.Length - 1) with
                    | (h, [| StartsWith "Full name:" fullName |]) ->
                        [| yield fsharpBlock h
                           yield Case1 ("_" + fullName + "_") |]
                    | _ -> [| fsharpBlock lines |]


                let commentContent =
                    res.Comment
                    |> String.replace "&lt;" "<"
                    |> String.replace "&gt;" ">"
                    |> createCommentBlock


                h.contents <- !! (Array.append sigContent commentContent |> ResizeArray)
                h.range <- !!range

                Browser.console.log h
                return h
            } |> Case2
}

let completionProvider = {
    new languages.CompletionItemProvider
    with
        member this.provideCompletionItems(model, position, token) =
            promise {
                let! o = completion (model.getValue()) (!! position.lineNumber) (!! position.column)
                let convertToInt code =
                    match code with
                    | "C" -> 6      (*  CompletionItemKind.Class      *)
                    | "E" -> 12     (*  CompletionItemKind.Enum       *)
                    | "S" -> 6      (*  CompletionItemKind.Value      *)
                    | "I" -> 7      (*  CompletionItemKind.Interface  *)
                    | "N" -> 8      (*  CompletionItemKind.Module     *)
                    | "M" -> 1      (*  CompletionItemKind.Method     *)
                    | "P" -> 9      (*  CompletionItemKind.Property   *)
                    | "F" -> 4      (*  CompletionItemKind.Field      *)
                    | "T" -> 6      (*  CompletionItemKind.Class      *)
                    | _   -> 0

                return
                    match o with
                    | None -> !!null
                    | Some o ->
                        let lineStr = model.getLineContent position.lineNumber
                        let chars = lineStr.ToCharArray ()
                        let noSpaces = chars |> Array.filter ((<>) ' ')
                        let spacesCount = chars |> Array.take (int position.column) |> Array.filter ((=) ' ') |> Array.length
                        let index = int position.column - spacesCount - 1
                        let prevChar = noSpaces.[index]

                        o |> Array.choose (fun c ->
                            if prevChar = '.' && c.GlyphChar = "K" then
                                None
                            else
                                let result = createEmpty<languages.CompletionItem>
                                result.kind <- c.GlyphChar |> convertToInt |> unbox
                                result.label <- c.Name
                                result.insertText <- Some c.ReplacementText
                                Some result)

                |> ResizeArray
            } |> U4.Case2

        member this.resolveCompletionItem(item, token) =
            promise {
                // let! o = helptext {Symbol = item.label }
                // let res = (o.Data.Overloads |> Array.fold (fun acc n -> (n |> Array.toList) @ acc ) []).Head
                // item.documentation <- Some res.Comment
                // item.detail <- Some res.Signature

                //TODO

                return item
            } |> Case2

        member this.triggerCharacters with get () =  ResizeArray(["."]) |> Some
            // and set v = ()

}

let signatureProvider = {
    new languages.SignatureHelpProvider
    with
        member this.signatureHelpTriggerCharacters with get () = ResizeArray(["("; ","])

        member this.provideSignatureHelp(model, position, token) =
            promise {
                let! o = methods (model.getValue()) (!! position.lineNumber) (!! position.column)
                match o with
                | None -> return !!null
                | Some o ->
                    let res = createEmpty<languages.SignatureHelp>
                    let sigs =  o.Overloads |> Array.map (fun c ->
                        let tip = c.Tip.[0].[0]
                        let si = createEmpty<languages.SignatureInformation>
                        si.label <- tip.Signature
                        si.documentation <- tip.Comment
                        si.parameters <- ResizeArray ()
                        c.Parameters |> Array.iter (fun p ->
                            let pi = createEmpty<languages.ParameterInformation>
                            pi.label <- p.Name
                            pi.documentation <- p.CanonicalTypeTextForSorting
                            si.parameters.Add(pi )
                        )
                        si)

                    res.activeParameter <- float (o.CurrentParameter)
                    res.activeSignature <-
                        sigs
                        |> Array.sortBy (fun n -> n.parameters.Count)
                        |> Array.findIndex (fun s -> s.parameters.Count >= o.CurrentParameter )
                        |> (+) 1
                        |> float
                    res.signatures <- ResizeArray sigs


                    return res
            } |> Case2
}

let highlighterProvider = {
    new languages.DocumentHighlightProvider
    with
        member this.provideDocumentHighlights(model, position, token) =
            promise {
                let! o = symbolUse (model.getValue()) (!! position.lineNumber) (!! position.column)
                match o with
                | None -> return !!null
                | Some o ->
                return
                    o.Uses |> Array.map (fun d ->
                        let res = createEmpty<languages.DocumentHighlight>
                        res.range <- Range (float d.StartLine, float d.StartColumn, float d.EndLine, float d.EndColumn) |> unbox
                        res.kind <- (0 |> unbox)
                        res )

                    |> ResizeArray
            } |> Case2
}

let renameProvider = {
    new languages.RenameProvider
    with
        member this.provideRenameEdits(model, position, newName, token) =
            promise {
                let! o = symbolUse (model.getValue()) (!! position.lineNumber) (!! position.column)
                match o with
                | None -> return !!null
                | Some o ->
                let we = createEmpty<languages.WorkspaceEdit>
                we.edits <-
                    o.Uses |> Array.map (fun s ->
                        let range = Range(float s.StartLine, (float s.EndColumn) - (float o.Name.Length), float s.EndLine, float s.EndColumn)
                        let re = createEmpty<languages.IResourceEdit>
                        re.newText <- newName
                        re.resource <- model.uri
                        re.range <- range |> unbox
                        re)  |> ResizeArray
                return we
            } |> Case2
}

let definitionProvider = {
    new languages.DefinitionProvider
    with
        member this.provideDefinition(model, position, token) =
            promise {
                let! o = symbolUse (model.getValue()) (!! position.lineNumber) (!! position.column)
                match o with
                | None -> return !!null
                | Some o ->
                let o = o.Uses.[0]
                let d = createEmpty<languages.Location>
                d.range <- Range(float o.StartLine, float o.StartColumn, float o.EndLine, float o.EndColumn) |> unbox
                d.uri <- model.uri
                return d |> Case1
            } |> Case2

}

let referenceProvider = {
    new languages.ReferenceProvider
    with
        member this.provideReferences(model, position, ctx, token) =
            promise {
                let! o = symbolUse (model.getValue()) (!! position.lineNumber) (!! position.column)
                match o with
                | None -> return !!null
                | Some o ->
                return
                    o.Uses |> Array.map (fun d ->
                        let res = createEmpty<languages.Location>
                        res.range <- Range (float d.StartLine, float d.StartColumn, float d.EndLine, float d.EndColumn) |> unbox
                        res.uri <- model.uri
                        res )
                    |> ResizeArray
            } |> Case2

}

let documentSymbolProvider = {
    new languages.DocumentSymbolProvider
    with
        member this.provideDocumentSymbols (model, token) =
            promise {
                let convertToKind code =
                    match code with
                    | "C" -> languages.SymbolKind.Class     (*  CompletionItemKind.Class      *)
                    | "E" -> languages.SymbolKind.Enum      (*  CompletionItemKind.Enum       *)
                    | "S" -> languages.SymbolKind.Property      (*  CompletionItemKind.Value      *)
                    | "I" -> languages.SymbolKind.Interface     (*  CompletionItemKind.Interface  *)
                    | "N" -> languages.SymbolKind.Module      (*  CompletionItemKind.Module     *)
                    | "M" -> languages.SymbolKind.Method     (*  CompletionItemKind.Method     *)
                    | "P" -> languages.SymbolKind.Property      (*  CompletionItemKind.Property   *)
                    | "F" -> languages.SymbolKind.Variable     (*  CompletionItemKind.Field      *)
                    | "T" -> languages.SymbolKind.Class      (*  CompletionItemKind.Class      *)
                    | "Fc" -> languages.SymbolKind.Function
                    | _   -> 0 |> unbox

                let! o = declarations (model.getValue()) (!! 1) (!! 1)
                match o with
                | None -> return !!null
                | Some o ->
                return
                    o |> Array.collect (fun syms ->
                        let oc = createEmpty<languages.SymbolInformation>
                        oc.name <- syms.Declaration.Name
                        oc.kind <- syms.Declaration.GlyphChar |> convertToKind
                        oc.containerName <- Some syms.Declaration.Glyph
                        let loc = createEmpty<languages.Location>
                        loc.range <- !!CodeRange.fromDTO syms.Declaration.BodyRange
                        loc.uri <- model.uri
                        oc.location <- loc
                        let ocs =  syms.Nested |> Array.map (fun sym ->
                            let oc = createEmpty<languages.SymbolInformation>
                            oc.name <- sym.Name
                            oc.kind <- sym.GlyphChar |> convertToKind
                            oc.containerName <- Some sym.Glyph
                            let loc = createEmpty<languages.Location>
                            loc.range <- !!CodeRange.fromDTO sym.BodyRange
                            loc.uri <- model.uri
                            oc.location <- loc
                            oc )
                        ocs |> Array.append (Array.create 1 oc))
                    |> ResizeArray

            } |> Case2
}


[<Emit("_monaco = monaco")>]
let hack : unit = failwith "JS only"
hack

let dom = Browser.document.getElementsByClassName("container").[0]

let options = createEmpty<editor.IEditorConstructionOptions>
options.value <- Some "let t = 1"
options.language <- Some "fsharp"

let services = createEmpty<editor.IEditorOverrideServices>

monaco.languages.Globals.registerHoverProvider("fsharp", hoverProvider)
monaco.languages.Globals.registerCompletionItemProvider("fsharp", completionProvider)
monaco.languages.Globals.registerSignatureHelpProvider("fsharp", signatureProvider)
monaco.languages.Globals.registerDocumentHighlightProvider("fsharp", highlighterProvider)
monaco.languages.Globals.registerRenameProvider("fsharp", renameProvider)
monaco.languages.Globals.registerDefinitionProvider("fsharp", definitionProvider)
monaco.languages.Globals.registerReferenceProvider("fsharp", referenceProvider)
monaco.languages.Globals.registerDocumentSymbolProvider("fsharp", documentSymbolProvider)

monaco.editor.Globals.create(dom |> unbox, options, services )