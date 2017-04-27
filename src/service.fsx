#r "../node_modules/fable-core/Fable.Core.dll"
#r "../node_modules/fable-powerpack/Fable.PowerPack.dll"
#load "Fable.Import.Axios.fsx"
#load "dto.fsx"

open Dto
open Fable.Core
open Fable.Core.JsInterop
open Fable.Import
open Fable.Import.Axios
open Fable.PowerPack

let url = "http://localhost:7071/api/FsAutoComplete"

let request<'a> (r : Request) =
    Globals.axios.post (url, r)
    |> Promise.map(fun r ->
        if r.data = "null" then None else r.data |> JS.JSON.parse |> unbox<'a> |> Some)

let private request'<'a> kind cnt line col =  request<'a> {kind = kind; content = cnt; line = line; column = col}

let parse cnt line col = request'<ErrorResp> "parse" cnt line col
let declarations cnt line col = request'<Symbols[]> "declarations" cnt line col
let completion cnt line col = request'<Completion[]> "completion" cnt line col
let tooltip cnt line col = request'<OverloadSignature[][]> "tooltip" cnt line col
let typesig cnt line col = request'<string> "typesig" cnt line col
let symbolUse cnt line col = request'<SymbolUses> "symbolUse" cnt line col
let help cnt line col = request'<string> "help" cnt line col
let findDeclaration cnt line col = request'<Declaration> "findDeclaration" cnt line col
let methods cnt line col = request'<Method> "methods" cnt line col
