type Request = {
    kind : string
    line : int
    column : int
    content : string
}

type OverloadSignature = {
    Signature: string
    Comment: string
}

type Error = {
    /// 1-indexed first line of the error block
    StartLine : int
    /// 1-indexed first column of the error block
    StartColumn : int
    /// 1-indexed last line of the error block
    EndLine : int
    /// 1-indexed last column of the error block
    EndColumn : int
    /// Description of the error
    Message : string
    ///Severity of the error - warning or error
    Severity : string
    /// Type of the Error
    Subcategory : string
    ///File Name
    FileName : string
    }

type ErrorResp = {
    File : string
    Errors : Error []
}

type Declaration = {
    File : string
    Line : int
    Column : int
}

type Completion = {
    Name : string
    ReplacementText : string
    Glyph : string
    GlyphChar: string
}

type SymbolUse = {
    FileName : string
    StartLine : int
    StartColumn : int
    EndLine : int
    EndColumn : int
    IsFromDefinition : bool
    IsFromAttribute : bool
    IsFromComputationExpression : bool
    IsFromDispatchSlotImplementation : bool
    IsFromPattern : bool
    IsFromType : bool
}

type SymbolUses = {
    Name : string
    Uses : SymbolUse array
}

type Helptext = {
    Name : string
    Overloads: OverloadSignature [] []
}

type OverloadParameter = {
    Name : string
    CanonicalTypeTextForSorting : string
    Display : string
    Description : string
}
type Overload = {
    Tip : OverloadSignature [] []
    TypeText : string
    Parameters : OverloadParameter []
    IsStaticArguments : bool
}

type Method = {
    Name : string
    CurrentParameter : int
    Overloads : Overload []
}

type CompilerLocation = {
    Fcs : string
    Fsi : string
    MSBuild : string
}

type Range = {
    StartColumn: int
    StartLine: int
    EndColumn: int
    EndLine: int
}

type Pos = {
    Line: int
    Col: int
}

type Symbol = {
    UniqueName: string
    Name: string
    Glyph: string
    GlyphChar: string
    IsTopLevel: bool
    Range: Range
    BodyRange : Range
    File : string
    EnclosingEntity : string
    IsAbstract : bool
}

type Symbols = {
    Declaration : Symbol;
    Nested : Symbol []
}

type Fix = {
    FromRange: Range
    FromText: string
    ToText: string
}

type Lint = {
    Info : string
    Input : string
    Range : Range
    Fix: Fix
}

type ProjectFilePath = string
type SourceFilePath = string
type ProjectReferencePath = string

type Project = {
    Project: ProjectFilePath
    Files: SourceFilePath list
    Output: string
    References: ProjectReferencePath list
    Logs: Map<string, string>
}

type OpenNamespace = {
    Namespace : string
    Name : string
    Type : string
    Line : int
    Column : int
    MultipleNames : bool
}

type QualifySymbol = {
    Name : string
    Qualifier : string
}

type ResolveNamespace = {
    Opens : OpenNamespace []
    Qualifies: QualifySymbol []
    Word : string
}

type UnionCaseGenerator = {
    Text : string
    Position : Pos
}
