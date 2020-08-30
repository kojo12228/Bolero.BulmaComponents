module Bolero.BulmaComponents.Components

open System
open Bolero
open Bolero.Html
open Microsoft.AspNetCore.Components.Web

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

module Navbar =
    type FixedPosition =
        | FixedTop
        | FixedBottom
        override this.ToString() =
            match this with
            | FixedTop -> "is-fixed-top"
            | FixedBottom -> "is-fixed-bottom"

    type DropdownOptions =
        {
            IsRight: bool
            IsArrowless: bool
            IsBoxed: bool
        }
        with
            static member Default =
                {
                    IsRight = false
                    IsArrowless = false
                    IsBoxed = false
                }

    type NavbarItem =
        | Link of title: string * href: string option
        | Dropdown of title: string * NavbarItem list * DropdownOptions
        | Divider
        | Custom of Node
        interface INodeable with
            member this.ToNode() =
                match this with
                | Link (title, hrefOpt) ->
                    a [
                        attr.``class`` "navbar-item"
                        attr.href (if Option.isSome hrefOpt then hrefOpt.Value else null)
                    ] [
                        text title
                    ]
                | Dropdown (title, subItems, options) ->
                    let navItemClasses =
                        "navbar-item has-dropdown is-hoverable"
                    let dropdownClasses =
                        [
                            Some "navbar-dropdown"
                            Option.boolMap "is-right" options.IsRight
                            Option.boolMap "is-arrowless" options.IsArrowless
                            Option.boolMap "is-boxed" options.IsBoxed
                        ]
                        |> List.choose id
                        |> String.concat " "

                    div [
                        attr.``class`` navItemClasses
                    ] [
                        a [ attr.``class`` "navbar-link" ] [ text title ]
                        div [ attr.``class`` dropdownClasses ] [
                            forEach subItems createNode
                        ]
                    ]
                | Divider -> hr [ attr.``class`` "navbar-divider" ]
                | Custom c -> c

    type NavbarBrand =
        | BrandText of title: string * href: string option
        | BrandImage of imgScr: string * href: string option * width: int option * height: int option
        | BrandCustom of NavbarItem

    type NavbarModel =
        {
            BurgerMenuOpen: bool
            ToggleFunc: MouseEventArgs -> unit
        }

    type NavbarNodeModel =
        {
            Model: NavbarModel option
            Color: ComponentColor option
            IsTransparent: bool
            FixPosition: FixedPosition option
            Brand: NavbarBrand option
            Items: NavbarItem list * NavbarItem list
        }
        interface INodeable with
            member this.ToNode() =
                let threeItemList = [0..2]

                let navBrand node =
                    let aTagClasses =
                        [
                            Some "navbar-burger burger"
                            Option.bind (fun x -> Option.boolMap "is-active" x.BurgerMenuOpen) this.Model
                        ]
                        |> List.choose id
                        |> String.concat " "

                    div [ attr.``class`` "navbar-brand" ] [
                        node
                        a [
                            "role" => "button"
                            attr.``class`` aTagClasses
                            attr.aria "label" "menu"
                            attr.aria "expanded" "false"
                            "data-target" => "navbarmenu"
                            on.click (match this.Model with | Some f -> f.ToggleFunc | None -> ignore)
                        ] [
                            forEach threeItemList <| (fun _ -> span [ attr.aria "hidden" "true" ] [])
                        ]
                    ]

                let navbarClasses =
                    [
                        Some "navbar"
                        Option.map colorToString this.Color
                        Option.boolMap "is-transparent" this.IsTransparent
                        match this.FixPosition with
                        | Some FixedBottom -> Some "is-fixed-bottom"
                        | Some FixedTop -> Some "is-fixed-top"
                        | None -> None
                    ]
                    |> List.choose id
                    |> String.concat " "

                let navMenuClasses =
                    [
                        Some "navbar-menu"
                        Option.bind (fun x -> Option.boolMap "is-active" x.BurgerMenuOpen) this.Model
                    ]
                    |> List.choose id
                    |> String.concat " "


                nav [
                    attr.``class`` navbarClasses
                    "role" => "navigation"
                    attr.aria "label" "main navigation"
                ] [
                    cond this.Brand <| function
                    | Some (BrandText (title, hrefOpt)) ->
                        navBrand <| a [
                            attr.``class`` "navbar-item"
                            attr.href (if Option.isSome hrefOpt then hrefOpt.Value else null)
                        ] [
                            text title
                        ]
                    | Some (BrandImage (src, hrefOpt, widthOpt, heightOpt)) ->
                        navBrand <| a [
                            attr.``class`` "navbar-item"
                            attr.href (if Option.isSome hrefOpt then hrefOpt.Value else null)
                        ] [
                            img [
                                attr.src src
                                attr.href (if Option.isSome hrefOpt then hrefOpt.Value else null)
                                attr.width (if Option.isSome widthOpt then Nullable(widthOpt.Value) else Nullable())
                                attr.height (if Option.isSome heightOpt then Nullable(heightOpt.Value) else Nullable())
                            ]
                        ]
                    | Some (BrandCustom c) ->
                        navBrand <| createNode c
                    | None -> empty

                    div [
                        attr.id "navbarmenu"
                        attr.``class`` navMenuClasses
                    ] [
                        concat [
                            div [ attr.``class`` "navbar-start" ] [
                                forEach (fst this.Items) createNode
                            ]
                            div [ attr.``class`` "navbar-start" ] [
                                forEach (snd this.Items) createNode
                            ]
                        ]
                    ]
                ]

    let createEmptyNavbar() =
        {
            Model = None
            Color = None
            IsTransparent = false
            FixPosition = None
            Brand = None
            Items = ([], [])
        }

    let withColor color model =
        { model with Color = Some color }

    let setTransparent model =
        { model with IsTransparent = true }

    let setFixedPosition position model =
        { model with FixPosition = position }

    let setBrandText text model =
        { model with Brand = Some (BrandText text) }

    let setBrand brandSection model =
        { model with Brand = Some brandSection }

    let addLeftMenuItem menuItem model =
        {
            model with
                Items =
                    let leftItems,rightItems = model.Items
                    leftItems @ [menuItem], rightItems
        }

    let addRightMenuItem menuItem model =
        {
            model with
                Items =
                    let leftItems,rightItems = model.Items
                    leftItems, rightItems @ [menuItem]
        }

    let setModel (model: NavbarModel) (nodeModel: NavbarNodeModel) =
        { nodeModel with Model = Some model }

    let setBurgerStatus (isOpen: bool) f (nodeModel: NavbarNodeModel) =
        {
            nodeModel with
                Model =
                    Some {
                        BurgerMenuOpen = isOpen
                        ToggleFunc = f
                    }
        }

    let createNavbarWithTitle title =
        createEmptyNavbar()
        |> setBrandText (title, None)