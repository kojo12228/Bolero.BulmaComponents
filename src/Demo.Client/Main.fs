module Demo.Client.Main

open System
open Elmish
open Bolero
open Bolero.Html
open Bolero.Remoting
open Bolero.Remoting.Client
open Bolero.Templating.Client

open Bolero.BulmaComponents
open Bolero.BulmaComponents.Elements
open Bolero.BulmaComponents.Components

open Demo.Client.Pages

/// Routing endpoints definition.
type Page =
    | [<EndPoint "/">] Home
    | [<EndPoint "/columns">] Columns
    | [<EndPoint "/layout">] Layout
    | [<EndPoint "/form">] Form
    | [<EndPoint "/elements">] Elements
    | [<EndPoint "/components">] Components

/// The Elmish application's model.
type Model =
    {
        page: Page
        navMenuOpen: bool
        pageModels:
            {|
                columns: Columns.Model
                components: Components.Model
                elements: Elements.Model
                form: Form.Model
                layout: Layout.Model
            |}
    }

let initModel =
    {
        page = Home
        navMenuOpen = false
        pageModels =
            {|
                columns = Columns.initModel
                components = Components.initModel
                elements = Elements.initModel
                form = Form.initModel
                layout = Layout.initModel
            |}
    }

/// The Elmish application's update messages.
type Message =
    | SetPage of Page
    | ToggleNavMenu

    | ColumnsMessage of Columns.Message
    | ComponentsMessage of Components.Message
    | ElementsMessage of Elements.Message
    | FormMessage of Form.Message
    | LayoutMessage of Layout.Message

let update message model =
    match message with
    | SetPage page ->
        { model with page = page; navMenuOpen = false }, Cmd.none
    | ToggleNavMenu ->
        { model with navMenuOpen = not model.navMenuOpen }, Cmd.none

    | ColumnsMessage msg ->
        let columnModel, cmd = Columns.update msg model.pageModels.columns
        { model with pageModels = {| model.pageModels with columns = columnModel |} }, Cmd.map ColumnsMessage cmd
    | ComponentsMessage msg ->
        let componentsModel, cmd = Components.update msg model.pageModels.components
        { model with pageModels = {| model.pageModels with components = componentsModel |} }, Cmd.map ComponentsMessage cmd
    | ElementsMessage msg ->
        let elementsModel, cmd = Elements.update msg model.pageModels.elements
        { model with pageModels = {| model.pageModels with elements = elementsModel |} }, Cmd.map ElementsMessage cmd
    | FormMessage msg ->
        let formModel, cmd = Form.update msg model.pageModels.form
        { model with pageModels = {| model.pageModels with form = formModel |} }, Cmd.map FormMessage cmd
    | LayoutMessage msg ->
        let layoutModel, cmd = Layout.update msg model.pageModels.layout
        { model with pageModels = {| model.pageModels with layout = layoutModel |} }, Cmd.map LayoutMessage cmd

/// Connects the routing system to the Elmish application.
let router = Router.infer SetPage (fun model -> model.page)

let navbar model dispatch =
    Navbar.createNavbarWithTitle "Bolero x Bulma"
    |> Navbar.addLeftMenuItem (Navbar.Link ("Columns", Some "/columns"))
    |> Navbar.addLeftMenuItem (Navbar.Link ("Layout", Some "/layout"))
    |> Navbar.addLeftMenuItem (Navbar.Link ("Form", Some "/form"))
    |> Navbar.addLeftMenuItem (Navbar.Link ("Elements", Some "/elements"))
    |> Navbar.addLeftMenuItem (Navbar.Link ("Components", Some "/components"))
    |> Navbar.withColor Dark
    |> Navbar.setBurgerStatus model.navMenuOpen (fun _ -> dispatch ToggleNavMenu)
    |> createNode

let homePage model dispatch =
    concat [
        Title.titleH1 "Bolero.BulmaComponents Library Demo"
        |> createNode

        Content.surroundContent (
            text "Explore the sections above to see usage of the Bolero.BulmaComponents Library"
        )
    ]

let view model dispatch =
    concat [
        navbar model dispatch

        section [ attr.``class`` "section" ] [
            div [ attr.``class`` "container" ] [
                cond model.page <| function
                | Home -> homePage model dispatch
                | Columns -> Columns.view model.pageModels.columns (ColumnsMessage >> dispatch)
                | Layout -> Layout.view model.pageModels.layout (LayoutMessage >> dispatch)
                | Form -> Form.view model.pageModels.form (FormMessage >> dispatch)
                | Elements -> Elements.view model.pageModels.elements (ElementsMessage >> dispatch)
                | Components -> Components.view model.pageModels.components (ComponentsMessage >> dispatch)
            ]
        ]
    ]

type MyApp() =
    inherit ProgramComponent<Model, Message>()

    override this.Program =
        Program.mkProgram (fun _ -> initModel, Cmd.none) update view
        |> Program.withRouter router
