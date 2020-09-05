module Demo.Client.Common.DocSection

open Bolero
open Bolero.Html

open Bolero.BulmaComponents
open Bolero.BulmaComponents.Elements

type DocSectionModel =
    {
        Title: string
        TitleIdMap: Map<string, string>
        NamespaceSubtitle: string
        DocButtonText: string
        DocButtonHref: string
        Content: Node
    }
    interface INodeable with
        member this.ToNode() =
            Box.box <| concat [
                Title.titleH3 this.Title
                |> Title.withId (Map.find this.Title this.TitleIdMap)
                |> createNode

                Title.subtitleH5 this.NamespaceSubtitle
                |> createNode

                this.Content

                Button.createButtonA()
                |> Button.setTextContent this.DocButtonText
                |> Button.setHref this.DocButtonHref
                |> Button.withColor Primary
                |> createNode
            ]