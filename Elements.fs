module Bolero.BulmaComponents.Elements

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

    type Size =
        | Small
        | Normal
        | Medium
        | Large
        override this.ToString() =
            match this with
            | Small -> "is-small"
            | Normal -> "is-normal"
            | Medium -> "is-medium"
            | Large -> "is-large"

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
                        attr.href (if Option.isSome this.Href then this.Href.Value else null)
                        on.click (if Option.isSome this.OnClick then this.OnClick.Value else ignore)
                    ] [
                        cond this.Content <| function
                        | Some (Text t) -> text t
                        | Some (Custom n) -> n
                        | None -> empty
                    ]
                | ButtonBtn ->
                    button [
                        attr.``class`` classes
                        if this.IsDisabled then attr.disabled()
                        if Option.isSome this.OnClick then on.click this.OnClick.Value
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
                        on.click (if Option.isSome this.OnClick then this.OnClick.Value else ignore)
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
                        on.click (if Option.isSome this.OnClick then this.OnClick.Value else ignore)
                        attr.value (
                            match this.Content with
                            | Some (Text t) -> t
                            | _ -> null
                        )
                    ]

    let createButton() =
        {
            ButtonType = ButtonBtn
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
    type Size =
        | Small
        | Medium
        | Large
        override this.ToString() =
            match this with
            | Small -> "is-small"
            | Medium -> "is-medium"
            | Large -> "is-large"

    type DeleteNodeModel =
        {
            OnClick: (MouseEventArgs -> unit) option
            Size: Size option
        }
        interface INodeable with
            member this.ToNode() =
                let classes =
                    [
                        Some "delete"
                        Option.map string this.Size
                    ]
                    |> List.choose id
                    |> String.concat " "

                button [
                    attr.``class`` "delete"
                    on.click (if Option.isSome this.OnClick then this.OnClick.Value else ignore)
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

