module Bolero.BulmaComponents.Elements

open System
open Bolero
open Bolero.Html
open Microsoft.AspNetCore.Components.Web

module Box =
    let box n =
        div [
            attr.``class`` "box"
        ] [
            n
        ]

module Button =
    type Type =
        | ButtonA
        | ButtonBtn
        | ButtonInputSubmit
        | ButtonInputReset

    type State =
        | Hover
        | Focus
        | Active
        override this.ToString() =
            match this with
            | Hover -> "is-hovered"
            | Focus -> "is-focused"
            | Active -> "is-active"

    type Content =
        | Text of string
        | Custom of Node

    type ButtonModel =
        {
            ButtonType: Type
            Size: Size option
            Color: ComponentColor option
            IsLight: bool
            IsFullWidth: bool
            IsOutlined: bool
            IsInverted: bool
            IsRounded: bool
            IsLoading: bool
            IsDisabled: bool
            Content: Content option
            Href: string option
            OnClick: (MouseEventArgs -> unit) option
        }
        interface INodeable with
            member this.ToNode() =
                let classes =
                    [
                        Some "button"
                        this.Size |> Option.map (fun x -> x.ToStringWithNormal())
                        Option.map (fun x -> x.ToString()) this.Color
                        Option.boolMap "is-light" this.IsLight
                        Option.boolMap "is-fullwidth" this.IsFullWidth
                        Option.boolMap "is-outlined" this.IsOutlined
                        Option.boolMap "is-inverted" this.IsInverted
                        Option.boolMap "is-rounded" this.IsRounded
                        Option.boolMap "is-loading" this.IsLoading
                    ]
                    |> List.choose id
                    |> String.concat " "

                match this.ButtonType with
                | ButtonA ->
                    a [
                        attr.``class`` classes
                        attr.disabled (this.IsDisabled)
                        attr.href (Option.valOrNull this.Href)
                        on.click (Option.valOrIgnore this.OnClick)
                    ] [
                        cond this.Content <| function
                        | Some (Text t) -> text t
                        | Some (Custom n) -> n
                        | None -> empty
                    ]
                | ButtonBtn ->
                    button [
                        attr.``class`` classes
                        attr.disabled (this.IsDisabled)
                        on.click (Option.valOrIgnore this.OnClick)
                    ] [
                        cond this.Content <| function
                        | Some (Text t) -> text t
                        | Some (Custom n) -> n
                        | None -> empty
                    ]
                | ButtonInputReset ->
                    input [
                        attr.``class`` classes
                        attr.``type`` "submit"
                        attr.disabled (this.IsDisabled)
                        on.click (Option.valOrIgnore this.OnClick)
                        attr.value (
                            match this.Content with
                            | Some (Text t) -> t
                            | _ -> null
                        )
                    ]
                | ButtonInputSubmit ->
                    input [
                        attr.``class`` classes
                        attr.``type`` "reset"
                        attr.disabled (this.IsDisabled)
                        on.click (Option.valOrIgnore this.OnClick)
                        attr.value (
                            match this.Content with
                            | Some (Text t) -> t
                            | _ -> null
                        )
                    ]

    let createButton() =
        {
            ButtonType = ButtonBtn
            Size = None
            Color = None
            IsLight = false
            IsFullWidth = false
            IsOutlined = false
            IsInverted = false
            IsRounded = false
            IsLoading = false
            IsDisabled = false
            Content = None
            Href = None
            OnClick = None
        }

    let createButtonA() =
        { createButton() with ButtonType = ButtonA }

    let createFormSubmitButton() =
        { createButton() with ButtonType = ButtonInputSubmit }

    let createFormResetButton() =
        { createButton() with ButtonType = ButtonInputReset }

    let withSize s model = { model with Size = Some s }

    let withColor c model = { model with Color = Some c }

    let setLight model = { model with IsLight = true}

    let setFullWidth model = { model with IsFullWidth = true }

    let setOutlined model = { model with IsOutlined = true }

    let setInverted model = { model with IsInverted = true }

    let setRounded model = { model with IsRounded = true }

    let setDisabled model = { model with IsDisabled = true }

    let setTextContent t model = { model with Content = Some (Text t) }

    let setNodeContent n model = { model with Content = Some (Custom n) }

    let setHref href model = { model with Href = Some href }

    let setOnClick f model = { model with OnClick = Some f }

    let createBasicButtonNode text =
        createButton()
        |> setTextContent text
        |> createNode

    let createBasicAButtonNode text =
        createButtonA()
        |> setTextContent text
        |> createNode

    let createBasicFormSubmitButton text =
        createFormSubmitButton()
        |> setTextContent text
        |> createNode

    let createBasicFormResetButton text =
        createFormResetButton()
        |> setTextContent text
        |> createNode

module Content =
    let surroundContent n =
        div [ attr.``class`` "content" ] [ n ]

module ButtonList =

    type ButtonsListModel =
        {
            Alignment: Alignment option
            HasAddons: bool
            Buttons: Button.ButtonModel seq
        }
        interface INodeable with
            member this.ToNode() =
                let classes =
                    [
                        Some "buttons"
                        Option.map string this.Alignment
                        Option.boolMap "has-addons" this.HasAddons
                    ]
                    |> List.choose id
                    |> String.concat " "

                div [
                    attr.``class`` classes
                ] [
                    forEach this.Buttons createNode
                ]

    let createButtonList buttons =
        {
            Alignment = None
            HasAddons = false
            Buttons = buttons
        }

    let createButtonListNode buttons =
        buttons
        |> createButtonList
        |> createNode

    let withAlignment align model =
        { model with Alignment = align }

    let setHasAddons model =
        { model with HasAddons = true }

    let addButton button model =
        { model with Buttons = Seq.append model.Buttons (Seq.singleton button) }

module DeleteButton =
    type DeleteModel =
        {
            OnClick: (MouseEventArgs -> unit) option
            Size: Size option
        }
        interface INodeable with
            member this.ToNode() =
                let classes =
                    [
                        Some "delete"
                        this.Size |> Option.map (fun x -> x.ToStringWithoutNormal())
                    ]
                    |> List.choose id
                    |> String.concat " "

                button [
                    attr.``class`` "delete"
                    on.click (Option.valOrIgnore this.OnClick)
                ] []

    let createDelete() =
        {
            OnClick = None
            Size = None
        }

    let withOnClick f model =
        { model with OnClick = Some f }

    let withSize s model =
        { model with Size = Some s }

    let deleteWithClick f =
        {
            OnClick = Some f
            Size = None
        }

module Icon =
    type IconModel =
        {
            Size: Size option
            Color: ComponentColor option
            IconClass: string
        }
        interface INodeable with
            member this.ToNode() =
                let classes =
                    [
                        Some "icon"
                        Option.map textColorToString this.Color
                        this.Size |> Option.map (fun x -> x.ToStringWithoutNormal())
                    ]
                    |> List.choose id

                span [
                    attr.classes classes
                ] [
                    i [ attr.``class`` this.IconClass ] []
                ]

    let createIcon iClass =
        {
            Size = None
            Color = None
            IconClass = iClass
        }

    let withColor c model =
        { model with Color = Some c}

    let withSize s model =
        { model with Size = Some s }

module Notification =
    type NotificationModel =
        {
            ClickDelete: (MouseEventArgs -> unit) option
            CustomDelete: DeleteButton.DeleteModel option
            Content: Node
            IsLight: bool
            Color: ComponentColor option
        }
        interface INodeable with
            member this.ToNode() =
                let classes =
                    [
                        Some "notification"
                        Option.boolMap "is-light" this.IsLight
                        Option.map colorToString this.Color
                    ]
                    |> List.choose id
                    |> String.concat " "

                div [
                    attr.``class`` classes
                ] [
                    cond this.CustomDelete <| function
                    | Some dm -> createNode dm
                    | None ->
                        cond this.ClickDelete <| function
                        | Some f ->
                            DeleteButton.deleteWithClick f
                            |> createNode
                        | None -> empty

                    this.Content
                ]

    let createNotification() =
        {
            ClickDelete = None
            CustomDelete = None
            Content = empty
            IsLight = false
            Color = None
        }

    let withOnDeleteClick f model =
        { model with ClickDelete = Some f }

    let withCustomDelete dm model =
        { model with CustomDelete = Some dm }

    let withText t model =
        { model with Content = text t }

    let withContent n model =
        { model with Content = n }

    let setLight model =
        { model with IsLight = true }

    let withColor c model =
        { model with Color = Some c }

    let notificationWithTextAndClick t f =
        createNotification()
        |> withOnDeleteClick f
        |> withText t

module Progress =
    type ProgressModel =
        {
            Percentage: int option
            IsIndeterminate: bool
            Size: Size option
            Color: ComponentColor option
        }
        interface INodeable with
            member this.ToNode() =
                let classes =
                    [
                        Some "progress"
                        this.Size |> Option.map (fun x -> x.ToStringWithoutNormal())
                        Option.map string this.Color
                    ]
                    |> List.choose id
                    |> String.concat " "

                let percentageSquashed =
                    this.Percentage
                    |> Option.map (min 100 >> max 0)

                progress [
                    attr.``class`` classes
                    attr.value (
                        if this.IsIndeterminate
                        then Nullable()
                        else Option.valOrNullable percentageSquashed)
                    attr.max 100 
                ] [
                    text (string percentageSquashed + "%")
                ]

    let createProgress() =
        {
            Percentage = None
            IsIndeterminate = false
            Size = None
            Color = None
        }

    let withPercentage p model =
        { model with Percentage = Some p }

    let setIndeterminate model =
        { model with IsIndeterminate = true }

    let setSize s model =
        { model with Size = Some s }

    let setColor c model =
        { model with Color = Some c }

    let createBasicIndetProgressNode() =
        {
            createProgress() with
                Percentage = Some 50
                IsIndeterminate = true
        }
        |> createNode

module Tag =
    type TagModel =
        {
            Color: ComponentColor option
            IsLight: bool
            Size: Size option
            IsDelete: bool
            Delete: DeleteButton.DeleteModel option
            IsRounded: bool
            Link: string option
            Content: Node option
        }
        interface INodeable with
            member this.ToNode() =
                let classes =
                    [
                        Some "tag"
                        Option.map string this.Color
                        Option.boolMap "is-light" this.IsLight
                        this.Size |> Option.map (fun x -> x.ToStringWithNormal())
                        Option.boolMap "is-rounded" this.IsRounded
                        Option.boolMap "is-delete" this.IsDelete
                    ]
                    |> List.choose id
                    |> String.concat " "

                cond this.Link <| function
                | Some href ->
                    a [
                        attr.``class`` classes
                        attr.href href
                    ] [
                        cond this.Content <| function
                        | Some n -> n
                        | None -> empty
                    ]
                | None ->
                    span [
                        attr.``class`` classes
                    ] [
                        cond this.Content <| function
                        | Some n -> n
                        | None -> empty

                        cond this.Delete <| function
                        | Some del -> createNode del
                        | None -> empty
                    ]


    let private groupSize s =
        match s with
        | Small -> "are-small"
        | Normal -> ""
        | Medium -> "are-medium"
        | Large -> "are-large"

    type TagGroupModel =
        {
            Tags: TagModel list
            Size: Size option
            HasAddons: bool
        }
        interface INodeable with
            member this.ToNode() =
                let classes =
                    [
                        Some "tags"
                        Option.map groupSize this.Size
                        Option.boolMap "has-addons" this.HasAddons
                    ]
                    |> List.choose id

                div [
                    attr.``classes`` classes
                ] [
                    forEach this.Tags createNode
                ]

    let createTag() =
        {
            Color = None
            IsLight = false
            Size = None
            IsDelete = false
            Delete = None
            IsRounded = false
            Link = None
            Content = None
        }

    let withColor c model =
        { model with Color = Some c }

    let setLight model =
        { model with IsLight = true }

    let setSize s (model: TagModel) =
        { model with Size = Some s }

    let setAsDelete model =
        { model with IsDelete = true }

    let setRounded model =
        { model with IsRounded = true }

    let withLink href model =
        { model with Link = Some href }

    let withText t model =
        { model with Content = Some <| text t }

    let withContent n model =
        { model with Content = Some n }

    let createTagGroup() =
        {
            Tags = []
            Size = None
            HasAddons = false
        }

    let addBasicTag t model =
        { model with Tags = model.Tags @ [ createTag() |> withText t ] }

    let addTag tag model =
        { model with Tags = model.Tags @ [ tag ] }

    let setGroupSize s (model: TagGroupModel) =
        { model with Size = Some s }

    let setAsAddons model = 
        { model with HasAddons = true }

    let createBasicTagPair (t1, c1) (t2, c2) =
        createTagGroup()
        |> addTag (
            createTag()
            |> withText t1
            |> withColor c1
        )
        |> addTag (
            createTag()
            |> withText t2
            |> withColor c2
        )
        |> setAsAddons

module Table =
    type TableModel =
        {
            IsBordered: bool
            IsStriped: bool
            IsNarrow: bool
            IsHoverable: bool
            IsFullWidth: bool
            IsInContainer: bool
            IsFirstColumnHeader: bool
            Header: Choice<string list, Node list> option
            Body: Choice<string [,], Node [,]>
            Footer: Choice<string list, Node list> option
            SelectedRow: int option
        }
        interface INodeable with
            member this.ToNode() =
                let tableClasses =
                    [
                        Some "table"
                        Option.boolMap "is-bordered" this.IsBordered
                        Option.boolMap "is-striped" this.IsStriped
                        Option.boolMap "is-narrow" this.IsNarrow
                        Option.boolMap "is-hoverable" this.IsHoverable
                        Option.boolMap "is-fullwidth" this.IsFullWidth
                    ]
                    |> List.choose id

                let array2dToSeqofSeq (a2d: 'T [,]) =
                    seq {
                        for i in 0 .. a2d.GetLength(0) - 1 ->
                            seq {
                                for j in 0 .. a2d.GetLength(1) - 1 -> a2d.[i, j]
                            }
                    }

                let bodyNode =
                    let createRows f a2d =
                        let xss = array2dToSeqofSeq a2d

                        forEach (Seq.indexed xss) <| (fun (i, xs) ->
                            cond this.IsFirstColumnHeader <| function
                            | false ->
                                tr [
                                    attr.``class`` (
                                        match this.SelectedRow with
                                        | Some x when x = i -> "is-selected"
                                        | _ -> null
                                    )
                                ] [
                                    forEach xs <| (fun x -> td [] [ f x ] )
                                ]
                            | true ->
                                let first = Seq.tryHead xs
                                let rest = Seq.tail xs

                                tr [
                                    attr.``class`` (
                                        match this.SelectedRow with
                                        | Some x when x = i -> "is-selected"
                                        | _ -> null
                                    )
                                ] [
                                    cond first <| function
                                    | Some x -> th [] [ f x ]
                                    | None -> empty

                                    forEach rest <| (fun x -> td [] [ f x ] )
                                ]
                        )

                    tbody [] [
                        cond this.Body <| function
                        | Choice1Of2 strBody ->
                            createRows text strBody
                        | Choice2Of2 nBody ->
                            createRows id nBody
                    ]

                let headerNode =
                    let createHeader f xs =
                        thead [] [
                            tr [] [
                                forEach xs <| (fun x ->
                                    th [] [ f x ]
                                )
                            ]
                        ]

                    cond this.Header <| function
                    | Some (Choice1Of2 strHeaders) ->
                        createHeader text strHeaders
                    | Some (Choice2Of2 nHeaders) ->
                        createHeader id nHeaders
                    | None -> empty

                let footerNode =
                    let createFooter f xs =
                        tfoot [] [
                            tr [] [
                                forEach xs <| (fun x ->
                                    th [] [ f x ]
                                )
                            ]
                        ]

                    cond this.Footer <| function
                    | Some (Choice1Of2 strFooters) ->
                        createFooter text strFooters
                    | Some (Choice2Of2 nFooters) ->
                        createFooter id nFooters
                    | None -> empty

                let table =
                    table [
                        attr.classes tableClasses
                    ] [
                        headerNode
                        bodyNode
                        footerNode
                    ]

                cond this.IsInContainer <| function
                | true ->
                    div [
                        attr.``class`` "table-container"
                    ] [
                        table
                    ]
                | false ->
                    table

    let createTable() =
        {
            IsBordered = false
            IsStriped = false
            IsNarrow = false
            IsHoverable = false
            IsFullWidth = false
            IsInContainer = false
            IsFirstColumnHeader = false
            Header = None
            Body = Choice1Of2 (Array2D.init 0 0 (fun _ _ -> ""))
            Footer = None
            SelectedRow = None
        }

    let setBordered model =
        { model with IsBordered = true }

    let setStriped model =
        { model with IsStriped = true }

    let setNarrow model =
        { model with IsNarrow = true }

    let setHoverable model =
        { model with IsHoverable = true }

    let setFullWidth model =
        { model with IsFullWidth = true }

    let setInContainer model =
        { model with IsInContainer = true }

    let setFirstColumnAsHeader model =
        { model with IsFirstColumnHeader = true }

    let withBodyText bodyArr model =
        { model with Body = Choice1Of2 bodyArr }

    let withBodyNode bodyArr model =
        { model with Body = Choice2Of2 bodyArr }

    let withHeadersText headerArr model =
        { model with Header = Some <| Choice1Of2 headerArr }

    let withHeadersNode headerArr model =
        { model with Header = Some <| Choice2Of2 headerArr }

    let withFootersText footerArr model =
        { model with Footer = Some <| Choice1Of2 footerArr }

    let withFootersNode footerArr model =
        { model with Footer = Some <| Choice2Of2 footerArr }

    let withSelectedRow rowIndex model =
        { model with SelectedRow = Some rowIndex }