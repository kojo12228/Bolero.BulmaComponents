module Bolero.BulmaComponents.Form

open System
open Bolero
open Bolero.Html
open Microsoft.AspNetCore.Components.Web

module Input =
    type AlphaNumericInputType =
        | Text
        | Password
        | Email
        | Tel
        override this.ToString() =
            match this with
            | Text -> "text"
            | Password -> "password"
            | Email -> "email"
            | Tel -> "tel"

    type State =
        | Hover
        | Focus
        override this.ToString() =
            match this with
            | Hover -> "is-hovered"
            | Focus -> "is-focused"

    type AlphaNumericInputModel =
        {
            Label: string option
            Placeholder: string option
            Text: string
            Type: AlphaNumericInputType option
            Color: ComponentColor option
            Size: Size option
            IsRounded: bool
            State: State option
            IsLoading: bool
            IsDisabled: bool
            IsReadonly: bool
            IsStatic: bool
            LeftIcon: Node option
            RightIcon: Node option
            BindEvent: string -> unit
        }
        interface INodeable with
            member this.ToNode() =
                let classes =
                    [
                        Some "input"
                        Option.map colorToString this.Color
                        this.Size |> Option.map (fun x -> x.ToStringWithoutNormal())
                        Option.boolMap "is-rounded" this.IsRounded
                        Option.map string this.State
                        Option.boolMap "is-static" this.IsStatic
                    ]
                    |> List.choose id

                div [ attr.``class`` "field" ] [
                    div [
                        [
                            Some "control"
                            Option.boolMap "is-loading" this.IsLoading
                        ]
                        |> List.choose id
                        |> attr.classes
                    ] [
                        cond this.Label <| function
                        | Some str -> label [ attr.``class``  "label"] [ text str ]
                        | None -> empty

                        input [
                            attr.classes classes
                            attr.``type`` (
                                this.Type
                                |> Option.map string
                                |> Option.toObj)
                            attr.disabled this.IsDisabled
                            attr.readonly this.IsReadonly
                            attr.placeholder (Option.toObj this.Placeholder)

                            bind.input.string this.Text this.BindEvent
                        ]
                        
                        cond this.LeftIcon <| function
                        | Some n -> n
                        | None -> empty

                        cond this.RightIcon <| function
                        | Some n -> n
                        | None -> empty
                    ]
                ]

    let createInput () =
        {
            Label = None
            Placeholder = None
            Text = ""
            Type = None
            Color = None
            Size = None
            IsRounded = false
            State = None
            IsLoading = false
            IsDisabled = false
            IsReadonly = false
            IsStatic = false
            LeftIcon = None
            RightIcon = None
            BindEvent = ignore
        }

    let withText t model =
        { model with Text = t }

    let withLabel label model =
        { model with Label = Some label }

    let withPlaceholder placeholder model =
        { model with Placeholder = Some placeholder }

    let withInputType inputType model =
        { model with Type = Some inputType }

    let withColor color model =
        { model with Color = Some color }

    let withSize size model =
        { model with Size = Some size }

    let withState state model =
        { model with State = Some state }

    let setRounded model =
        { model with IsRounded = true }

    let setLoading model =
        { model with IsRounded = true }

    let setDisabled model =
        { model with IsDisabled = true }

    let setReadonly model =
        { model with IsReadonly = true }

    let setStatic model =
        { model with IsStatic = true }

    let withLeftIcon n model =
        { model with LeftIcon = Some n }

    let withRightIcon n model =
        { model with RightIcon = Some n }

    let bindEvent func model =
        { model with BindEvent = func }

    let createInputWithEvent func =
        createInput() |> bindEvent func

    let createInputWithPlaceholder placeholder =
        createInput() |> withPlaceholder placeholder

module Radio =
    type RadioModel<'T when 'T : comparison> =
        {
            RadioButtonMap: Map<'T, string>
            RadioButtonContent: Map<'T, Node>
            GroupName: string option
            BindEvent: 'T -> unit
        }
        interface INodeable with
            member this.ToNode() =
                let buttons = Map.toList this.RadioButtonMap
                div [ attr.``class`` "field" ] [
                    div [ attr.``class`` "control" ] [
                        forEach buttons <| fun (key, value) ->
                            label [ attr.``class`` "radio" ] [
                                input [
                                    attr.``type`` "radio"
                                    attr.name (Option.toObj this.GroupName)
                                    bind.change.string value (fun _ -> this.BindEvent key)
                                ]

                                cond (Map.tryFind key this.RadioButtonContent) <| function
                                | Some n -> n
                                | None -> empty
                            ]
                    ]
                ]

    let createRadioGroup() =
        {
            RadioButtonMap = Map.empty
            RadioButtonContent = Map.empty
            GroupName = None
            BindEvent = ignore
        }

    let withMap map model =
        { model with RadioButtonMap = map }

    let withContent map model =
        { model with RadioButtonContent = map }

    let withName name model =
        { model with GroupName = Some name }

    let setBindEvent func model =
        { model with BindEvent = func }

module Checkbox =
    let createCheckbox content =
        div [ attr.``class`` "field" ] [
            div [ attr.``class`` "control" ] [
                label [ attr.``class`` "checkbox" ] [
                    input [ attr.``type`` "checkbox" ]

                    content
                ]
            ]
        ]

    let createCheckboxBind content callback isChecked =
        div [ attr.``class`` "field" ] [
            div [ attr.``class`` "control" ] [
                label [ attr.``class`` "checkbox" ] [
                    input [
                        attr.``type`` "checkbox"
                        bind.checked isChecked callback
                    ]

                    content
                ]
            ]
        ]

module TextArea =
    type State =
        | Hover
        | Focus
        | Active
        override this.ToString() =
            match this with
            | Hover -> "is-hovered"
            | Focus -> "is-focused"
            | Active -> "is-active"

    type TextAreaModel =
        {
            Label: string option
            Placeholder: string option
            Text: string
            Rows: int option
            Color: ComponentColor option
            Size: Size option
            State: State option
            IsLoading: bool
            IsDisabled: bool
            IsReadonly: bool
            IsFixedSize: bool
            BindEvent: string -> unit
            
        }
        interface INodeable with
            member this.ToNode() =
                let classes =
                    [
                        Some "textarea"
                        Option.map colorToString this.Color
                        this.Size |> Option.map (fun x -> x.ToStringWithoutNormal())
                        Option.map string this.State
                        Option.boolMap "has-fixed-size" this.IsFixedSize
                    ]
                    |> List.choose id

                div [ attr.``class`` "field" ] [
                    div [
                        [
                            Some "control"
                            Option.boolMap "is-loading" this.IsLoading
                        ]
                        |> List.choose id
                        |> attr.classes
                    ] [
                        cond this.Label <| function
                        | Some str -> label [ attr.``class``  "label"] [ text str ]
                        | None -> empty

                        textarea [
                            attr.classes classes
                            attr.disabled this.IsDisabled
                            attr.readonly this.IsReadonly
                            attr.rows (this.Rows |> Option.map string |> Option.toObj)
                            attr.placeholder (Option.toObj this.Placeholder)

                            bind.input.string this.Text this.BindEvent
                        ] []
                    ]
                ]

    let createTextArea() =
        {
            Label = None
            Placeholder = None
            Text = ""
            Rows = None
            Color = None
            Size = None
            State = None
            IsLoading = false
            IsDisabled = false
            IsReadonly = false
            IsFixedSize = false
            BindEvent = ignore
        }

    let withText t model =
        { model with Text = t }

    let withLabel label model =
        { model with Label = Some label }

    let withPlaceholder placeholder model =
        { model with Placeholder = Some placeholder }

    let withRows noRows model =
        { model with Rows = Some noRows }

    let withColor c model =
        { model with Color = Some c }

    let withSize s model =
        { model with Size = Some s }

    let withState s model =
        { model with State = Some s }

    let setLoading model =
        { model with IsLoading = true }

    let setDisabled model =
        { model with IsDisabled = true }

    let setReadonly model =
        { model with IsReadonly = true }

    let setFixedSize model =
        { model with IsFixedSize = true }

    let bindEvent func model =
        { model with BindEvent = func }

module Select =
    begin end

module Form =
    type FormElement =
        | AlphaNumericInput
        | RadioInput
        | CheckboxInput
        | FileInput
        | TextArea
        | Select
        | Button
        | RawNode