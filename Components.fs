module Bolero.BulmaComponents.Components

open Bolero
open Bolero.Html

type Nodeable =
    abstract member ToNode: unit -> Node

let createNode (nodeable: 'T when 'T :> Nodeable) =
    nodeable.ToNode()

type ComponentTemplate = Template<"Templates/components.html">

module Breadcrumbs =
    type Aligment =
        | AlignLeft
        | AlignCenter
        | AlignRight
        override this.ToString() =
            match this with
            | AlignLeft -> ""
            | AlignCenter -> "is-centered"
            | AlignRight -> "is-right"

    type Separator =
        | Slash
        | Arrow
        | Bullet
        | Dot
        | Succeeds
        override this.ToString() =
            match this with
            | Slash -> ""
            | Arrow -> "has-arrow-separator"
            | Bullet -> "has-bullet-separator"
            | Dot -> "has-dot-separator"
            | Succeeds -> "has-succeeds-separator"

    type Size =
        | Small
        | Medium
        | Large
        override this.ToString() =
            match this with
            | Small -> "is-small"
            | Medium -> "is-medium"
            | Large -> "is-large"

    type BreadcrumbItem =
        | GeneralItem of href: string * title: string
        | TailItem of href: string * title: string

    type BreadcrumbModel =
        {
            Alignment: Aligment option
            Separator: Separator option
            Size: Size option
            Items: BreadcrumbItem list
        }
        interface Nodeable with
            member this.ToNode() =
                ComponentTemplate
                    .Breadcrumb()
                    .Alignment(
                        match this.Alignment with
                        | Some al -> al.ToString()
                        | None -> ""
                    )
                    .Separator(
                        match this.Separator with
                        | Some sp -> sp.ToString()
                        | None -> ""
                    )
                    .Size(
                        match this.Separator with
                        | Some sz -> sz.ToString()
                        | None -> ""
                    )
                    .BreadcrumbItems(
                        forEach this.Items <| (fun it ->
                            match it with
                            | GeneralItem (href, title) ->
                                ComponentTemplate
                                    .BreadcrumbItem()
                                    .Href(href)
                                    .Title(title)
                                    .Elt()
                            | TailItem (href, title) ->
                                ComponentTemplate
                                    .BreadcrumbActiveItem()
                                    .Href(href)
                                    .Title(title)
                                    .Elt()
                        )
                    )
                    .Elt()

    let createBreadcrumb() =
        { 
            Alignment = None
            Separator  = None
            Size = None
            Items = []
        }

    let createBreadcrumbNode() =
        createBreadcrumb()
        |> createNode

    let withAlignment align model =
        { model with Alignment = align }

    let withSeparator separator model =
        { model with Separator = separator }

    let withSize size model =
        { model with Size = size }

    let withItems items (title, href) model =
        let itemsModelled =
            [
                for (title, href) in items ->
                    GeneralItem (href, title)
            ]
            |> List.append [ TailItem (href, title) ]

        { model with Items = List.append itemsModelled model.Items }

    let createBreadcrumbNodeWithItems items (title, href) =
        createBreadcrumb()
        |> withItems items (title, href)