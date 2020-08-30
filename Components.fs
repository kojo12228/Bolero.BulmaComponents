module Bolero.BulmaComponents.Components

open Bolero
open Bolero.Html

module Breadcrumbs =
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
            Alignment: Alignment option
            Separator: Separator option
            Size: Size option
            Items: BreadcrumbItem list
        }
        interface INodeable with
            member this.ToNode() =
                let breadcrumbClass =
                    [
                        Some "breadcrumb"
                        Option.map (fun x -> x.ToString()) this.Alignment
                        Option.map (fun x -> x.ToString()) this.Separator
                        Option.map (fun x -> x.ToString()) this.Size
                    ]
                    |> List.choose id
                    |> String.concat " "

                nav [
                    attr.``class`` breadcrumbClass
                ] [
                    forEach this.Items <| (fun it ->
                        match it with
                        | GeneralItem (href, title) ->
                            li [] [
                                a [ attr.href href ] [ text title ]
                            ]
                        | TailItem (href, title) ->
                            li [
                                attr.``class`` "is-active"
                            ] [
                                a [ attr.href href ] [ text title ]
                            ]
                    )
                ]

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
            ] @ [ TailItem (href, title) ]

        { model with Items = List.append itemsModelled model.Items }

    let createBreadcrumbNodeWithItems items (title, href) =
        createBreadcrumb()
        |> withItems items (title, href)