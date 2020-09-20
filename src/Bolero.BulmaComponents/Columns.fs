module Bolero.BulmaComponents.Columns

open Bolero
open Bolero.Html

type Breakpoint =
    | Normal
    | Mobile
    | Tablet
    | Touch
    | Desktop
    | Widescreen
    | FullHD
    override this.ToString() =
        match this with
        | Normal -> ""
        | Mobile -> "mobile"
        | Tablet -> "tablet"
        | Touch -> "touch"
        | Desktop -> "desktop"
        | Widescreen -> "widescreen"
        | FullHD -> "fullhd"

type BreakpointModel<'T> =
    {
        Normal: 'T option
        Mobile: 'T option
        Tablet: 'T option
        Touch: 'T option
        Desktop: 'T option
        Widescreen: 'T option
        FullHD: 'T option
    }

let private defaultBpModel =
    {
        Normal = None
        Mobile = None
        Tablet = None
        Touch = None
        Desktop = None
        Widescreen = None
        FullHD = None
    }

let inline breakpointsToStrings (bps: BreakpointModel<'T>) =
    [
        Option.map (string) bps.Normal
        Option.map (string >> sprintf "%s-mobile") bps.Mobile
        Option.map (string >> sprintf "%s-tablet") bps.Tablet
        Option.map (string >> sprintf "%s-touch") bps.Touch
        Option.map (string >> sprintf "%s-desktop") bps.Desktop
        Option.map (string >> sprintf "%s-widescreen") bps.Widescreen
        Option.map (string >> sprintf "%s-fullhd") bps.FullHD
    ]

type ColumnSize =
    | ColThreeQuarters
    | ColTwoThirds
    | ColHalf
    | ColOneThird
    | ColOneQuarter
    | ColFull

    | ColFourFifths
    | ColThreeFifths
    | ColTwoFifths
    | ColOneFifth

    | Col1
    | Col2
    | Col3
    | Col4
    | Col5
    | Col6
    | Col7
    | Col8
    | Col9
    | Col10
    | Col11
    | Col12
    override this.ToString() =
        match this with
        | ColThreeQuarters -> "is-three-quarters"
        | ColTwoThirds -> "is-two-thirds"
        | ColHalf -> "is-half"
        | ColOneThird -> "is-one-third"
        | ColOneQuarter -> "is-one-quarter"
        | ColFull -> "is-full"

        | ColFourFifths -> "is-four-fifths"
        | ColThreeFifths -> "is-three-fifths"
        | ColTwoFifths -> "is-two-fifths"
        | ColOneFifth -> "is-one-fifth"

        | Col1 -> "is-1"
        | Col2 -> "is-2"
        | Col3 -> "is-3"
        | Col4 -> "is-4"
        | Col5 -> "is-5"
        | Col6 -> "is-6"
        | Col7 -> "is-7"
        | Col8 -> "is-8"
        | Col9 -> "is-9"
        | Col10 -> "is-10"
        | Col11 -> "is-11"
        | Col12 -> "is-12"

type Offset =
    | OffsetThreeQuarters
    | OffsetTwoThirds
    | OffsetHalf
    | OffsetOneThird
    | OffsetOneQuarter

    | OffsetFourFifths
    | OffsetThreeFifths
    | OffsetTwoFifths
    | OffsetOneFifth

    | Offset1
    | Offset2
    | Offset3
    | Offset4
    | Offset5
    | Offset6
    | Offset7
    | Offset8
    | Offset9
    | Offset10
    | Offset11
    | Offset12
    override this.ToString() =
        match this with
        | OffsetThreeQuarters -> "is-offset-three-quarters"
        | OffsetTwoThirds -> "is-offset-two-quarters"
        | OffsetHalf -> "is-offset-half"
        | OffsetOneThird -> "is-offset-one-third"
        | OffsetOneQuarter -> "is-offset-one-quarter"

        | OffsetFourFifths -> "is-offset-four-fifths"
        | OffsetThreeFifths -> "is-offset-three-fifths"
        | OffsetTwoFifths -> "is-offset-two-fifths"
        | OffsetOneFifth -> "is-offset-one-fifth"

        | Offset1 -> "is-offset-1"
        | Offset2 -> "is-offset-2"
        | Offset3 -> "is-offset-3"
        | Offset4 -> "is-offset-4"
        | Offset5 -> "is-offset-5"
        | Offset6 -> "is-offset-6"
        | Offset7 -> "is-offset-7"
        | Offset8 -> "is-offset-8"
        | Offset9 -> "is-offset-9"
        | Offset10 -> "is-offset-10"
        | Offset11 -> "is-offset-11"
        | Offset12 -> "is-offset-12"

type ColumnModel =
    {
        Sizes: Choice<ColumnSize, BreakpointModel<ColumnSize>> option
        Offset: Offset option
        Narrow: Choice<unit, BreakpointModel<string>> option
        Content: Node option
    }
    interface INodeable with
        member this.ToNode() =
            let sizeToString choice =
                match choice with
                | Choice1Of2 size ->
                    Some size
                    |> Option.map string
                    |> List.singleton
                | Choice2Of2 bpSizes ->
                    breakpointsToStrings bpSizes

            let narrowString choice =
                match choice with
                | Choice1Of2 _ -> Some "is-narrow" |> List.singleton
                | Choice2Of2 y -> breakpointsToStrings y

            let classes =
                [
                    [
                        Some "column"
                        Option.map string this.Offset
                    ]

                    this.Sizes
                    |> Option.map sizeToString
                    |> Option.defaultValue []
                    
                    this.Narrow
                    |> Option.map narrowString
                    |> Option.defaultValue []
                ]
                |> List.concat
                |> List.choose id

            div [
                attr.classes classes
            ] [
                cond this.Content <| function
                | Some n -> n
                | None -> empty
            ]

type VariableGapSize =
    | Gap0
    | Gap1
    | Gap2
    | Gap3
    | Gap4
    | Gap5
    | Gap6
    | Gap7
    | Gap8
    override this.ToString() =
        match this with
        | Gap0 -> "is-0"
        | Gap1 -> "is-1"
        | Gap2 -> "is-2"
        | Gap3 -> "is-3"
        | Gap4 -> "is-4"
        | Gap5 -> "is-5"
        | Gap6 -> "is-6"
        | Gap7 -> "is-7"
        | Gap8 -> "is-8"

type ColumnsModel =
    {
        Columns: ColumnModel list
        IsMobile: bool
        IsDesktop: bool
        IsGapless: bool
        IsMultiline: bool
        IsVCentered: bool
        IsCentered: bool
        // Variable gaps requires CSS Variables support
        IsVariable: Choice<VariableGapSize, BreakpointModel<VariableGapSize>> option
    }
    interface INodeable with
        member this.ToNode() =
            let variableStrings choice =
                match choice with
                | Choice1Of2 varGap ->
                    Some varGap
                    |> Option.map string
                    |> List.singleton
                | Choice2Of2 bpSizes ->
                    breakpointsToStrings bpSizes

            let classes =
                [
                    [
                        Some "columns"
                        Option.boolMap "is-mobile" this.IsMobile
                        Option.boolMap "is-desktop" this.IsDesktop
                        Option.boolMap "is-gapless" this.IsGapless
                        Option.boolMap "is-multiline" this.IsMultiline
                        Option.boolMap "is-variable" this.IsVariable.IsSome
                        Option.boolMap "is-vcentered" this.IsVCentered
                        Option.boolMap "is-centered" this.IsVCentered
                    ]

                    this.IsVariable
                    |> Option.map variableStrings
                    |> Option.defaultValue []
                ]
                |> List.concat
                |> List.choose id

            div [
                attr.classes classes
            ] [
                forEach this.Columns createNode
            ]

let createColumn content =
    {
        Sizes = None
        Offset = None
        Narrow = None
        Content = Some content
    }

let withSize s model =
    { model with Sizes = Some (Choice1Of2 s) }

let withBreakpointSizes bps model =
    { model with Sizes = Some (Choice2Of2 bps) }

let private breakpointModel bpm =
    match bpm with
    | Some (Choice2Of2 x) -> x
    | Some _ | None -> defaultBpModel

let withBreakpointSize s bp model =
    { model with
        Sizes =
            let bpm = breakpointModel model.Sizes
            let newBpm =
                match bp with
                | Normal -> { bpm with Normal = Some s }
                | Mobile -> { bpm with Mobile = Some s }
                | Tablet -> { bpm with Tablet = Some s }
                | Touch -> { bpm with Touch = Some s }
                | Desktop -> { bpm with Desktop = Some s }
                | Widescreen -> { bpm with Widescreen = Some s }
                | FullHD -> { bpm with FullHD = Some s }

            Some <| Choice2Of2 newBpm
    }

let withOffset offset model =
    { model with Offset = Some offset }

let setNarrow model =
    { model with Narrow = Some (Choice1Of2 ()) }

let setNarrowBreakpoint bp model =
    { model with
        Narrow =
            let bpm = breakpointModel model.Narrow
            let narrowStr = Some "is-narrow"
            let newBpm =
                match bp with
                | Normal -> { bpm with Normal = narrowStr }
                | Mobile -> { bpm with Mobile = narrowStr }
                | Tablet -> { bpm with Tablet = narrowStr }
                | Touch -> { bpm with Touch = narrowStr }
                | Desktop -> { bpm with Desktop = narrowStr }
                | Widescreen -> { bpm with Widescreen = narrowStr }
                | FullHD -> { bpm with FullHD = narrowStr }

            Some <| Choice2Of2 { newBpm with Mobile = narrowStr }
    }

let emptyColumns() =
    {
        Columns = []
        IsMobile = false
        IsDesktop = false
        IsGapless = false
        IsMultiline = false
        IsVCentered = false
        IsCentered = false
        IsVariable = None
    }

let addColumns cols model =
    { model with Columns = model.Columns @ cols }

let addColumn col model =
    { model with Columns = model.Columns @ [ col ] }

let setMobile model =
    { model with IsMobile = true }

let setDesktop model =
    { model with IsDesktop = true }

let setGapless model =
    { model with IsGapless = true }

let setMultiline model =
    { model with IsMultiline = true }

let setVCentered model =
    { model with IsVCentered = true }

let setCentered model =
    { model with IsCentered = true }

let setVariable gap model =
    { model with IsVariable = Some <| Choice1Of2 gap }

let setVariableBreakpoint gap bp model =
    { model with
        IsVariable =
            let bpm = breakpointModel model.IsVariable
            let newBpm =
                match bp with
                | Normal -> { bpm with Normal = Some gap }
                | Mobile -> { bpm with Mobile = Some gap }
                | Tablet -> { bpm with Tablet = Some gap }
                | Touch -> { bpm with Touch = Some gap }
                | Desktop -> { bpm with Desktop = Some gap }
                | Widescreen -> { bpm with Widescreen = Some gap }
                | FullHD -> { bpm with FullHD = Some gap }

            Some <| Choice2Of2 newBpm
    }