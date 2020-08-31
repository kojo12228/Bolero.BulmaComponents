module Bolero.BulmaComponents.Layouts

open Bolero
open Bolero.Html

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

