
[<RequireQualifiedAccess>]
module String =
    let trim (s: string) = s.Trim()
    let replace (oldVal: string) (newVal: string) (str: string) : string =
        match str with
        | null -> null
        | _ -> str.Replace (oldVal, newVal)
    let split seperator (s : string) = s.Split seperator

    let endWith ending (s : string) = s.EndsWith ending

    let startWith ending (s : string) = s.StartsWith ending

[<AutoOpen>]
module Patterns =
    let (|StartsWith|_|) (pat: string) (str: string)  =
        match str with
        | null -> None
        | _ when str.StartsWith pat -> Some str
        | _ -> None

    let (|Contains|_|) (pat: string) (str: string)  =
        match str with
        | null -> None
        | _ when str.Contains pat -> Some str
        | _ -> None

[<RequireQualifiedAccess>]
module Array =
    let splitAt (n: int) (xs: 'a[]) : 'a[] * 'a[] =
        match xs with
        | [||] | [|_|] -> xs, [||]
        | _ when n >= xs.Length || n < 0 -> xs, [||]
        | _ -> xs.[0..n-1], xs.[n..]

module Markdown =
    open System.Text.RegularExpressions
    let private replacePatterns =
        [ Regex "<c>(((?!<c>)(?!<\/c>).)*)<\/c>", fun x -> sprintf "`%s`" x ]

    /// Replaces XML tags with Markdown equivalents.
    let replaceXml (str: string) : string =
        replacePatterns
        |> List.fold (fun res (regex: Regex, formatter: string -> string) ->
            // repeat replacing with same pattern to handle nested tags, like `<c>..<c>..</c>..</c>`
            let rec loop res : string =
                match regex.Match res with
                | m when m.Success -> loop <| res.Replace(m.Groups.[0].Value, formatter (m.Groups.[1].Value))
                | _ -> res
            loop res
        ) str

