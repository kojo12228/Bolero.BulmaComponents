module Bolero.BulmaComponents.Layouts

open Bolero
open Bolero.Html

module Container =
    type ContainerBreakpoint =
        | Widescreen
        | FullHD
        | MaxDesktop
        | MaxWidescreen
        | Fluid
        override this.ToString() =
            match this with
            | Widescreen -> "is-widescreen"
            | FullHD -> "is-fullhd"
            | MaxDesktop -> "is-max-desktop"
            | MaxWidescreen -> "is-max-widescreen"
            | Fluid -> "is-fluid"

    type ContainerModel =
        {
            Breakpoint: ContainerBreakpoint option
            Content: Node
        }
        interface INodeable with
            member this.ToNode() =
                let classes =
                    [
                        Some "container"
                        Option.map string this.Breakpoint
                    ]
                    |> List.choose id

                div [
                    attr.classes classes
                ] [
                    this.Content
                ]

    let createContainer content =
        {
            Breakpoint = None
            Content = content
        }

    let withBreakpoint bp model =
        { model with Breakpoint = bp }

module Level =
    type LevelItem =
        {
            TextCentered: bool
            Content: Node
        }
        interface INodeable with
            member this.ToNode() =
                let classes =
                    [
                        Some "level-item"
                        Option.boolMap "has-text-centered" this.TextCentered
                    ]
                    |> List.choose id

                div [
                    attr.classes classes
                ] [
                    this.Content
                ]

    type LevelModel =
        {
            IsMobile: bool
            LevelLeft: LevelItem list
            LevelRight: LevelItem list
            LevelCentered: LevelItem list
        }
        interface INodeable with
            member this.ToNode() =
                let classes =
                    [
                        Some "level"
                        Option.boolMap "is-mobile" this.IsMobile
                    ]
                    |> List.choose id

                nav [
                    attr.classes classes
                ] [
                    cond (List.isEmpty this.LevelLeft) <| function
                    | true -> empty
                    | false -> 
                        div [
                            attr.``class`` "level-left"
                        ] [
                            forEach this.LevelLeft createNode
                        ]

                    cond (List.isEmpty this.LevelRight) <| function
                    | true -> empty
                    | false -> 
                        div [
                            attr.``class`` "level-right"
                        ] [
                            forEach this.LevelRight createNode
                        ]

                    forEach this.LevelCentered createNode
                ]

    let createCenteredLevel content =
        {
            IsMobile = false
            LevelLeft = []
            LevelRight = []
            LevelCentered = content
        }

    let createLeftRightLevel leftContent rightContent =
        {
            IsMobile = false
            LevelLeft = leftContent
            LevelRight = rightContent
            LevelCentered = []
        }

    let createLeftLevel content =
        createLeftRightLevel content []

    let createRightLevel content =
        createLeftRightLevel [] content

    let levelItem content =
        {
            TextCentered = false
            Content = content
        }

    let setTextCentered model =
        { model with TextCentered = true }

    let setMobile model =
        { model with IsMobile = true }

module MediaObject =
    type MediaObjModel =
        {
            MediaLeft: Node option
            MediaContent: Node option
            MediaRight: Node option
        }
        interface INodeable with
            member this.ToNode() =
                article [
                    attr.``class`` "media"
                ] [
                    cond this.MediaLeft <| function
                    | None -> empty
                    | Some n ->
                        figure [ attr.``class`` "media-left" ] [ n ]

                    cond this.MediaContent <| function
                    | None -> empty
                    | Some n ->
                        div [ attr.``class`` "media-content" ] [ n ]

                    cond this.MediaRight <| function
                    | None -> empty
                    | Some n ->
                        div [ attr.``class`` "media-right" ] [ n ]
                ]

    let createBlackMediaObj() =
        {
            MediaLeft = None
            MediaContent = None
            MediaRight = None
        }

    let withLeft n model =
        { model with MediaLeft = n }

    let withContent n model =
        { model with MediaContent = n }

    let withRight n model =
        { model with MediaRight = n }

    let createMediaObj left content right =
        {
            MediaLeft = Some left
            MediaContent = Some content
            MediaRight = Some right
        }

module Footer =
    type FooterModel =
        {
            Content: Node
        }
        interface INodeable with
            member this.ToNode() =
                footer [
                    attr.``class`` "footer"
                ] [
                    div [
                        attr.``class`` "content has-text-centered"
                    ] [
                        this.Content
                    ]
                ]

    let createFooter() =
        { Content = empty }

    let withText t model  =
        { model with Content = text t }

    let withContent n model =
        { model with Content = n }

    let createBasicFooterNode t =
        createFooter()
        |> withText t
        |> createNode

