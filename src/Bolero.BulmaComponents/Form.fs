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
        }

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

module Radio =
    begin end

module Checkbox =
    begin end

module File =
    begin end

module TextArea =
    begin end

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