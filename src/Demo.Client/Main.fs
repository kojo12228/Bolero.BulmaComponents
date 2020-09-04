module Demo.Client.Main

open System
open Elmish
open Bolero
open Bolero.Html
open Bolero.Remoting
open Bolero.Remoting.Client
open Bolero.Templating.Client

open Bolero.BulmaComponents
open Bolero.BulmaComponents.Components

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
    }

let initModel =
    {
        page = Home
        navMenuOpen = false
    }

/// The Elmish application's update messages.
type Message =
    | SetPage of Page
    | ToggleNavMenu

let update message model =
    match message with
    | SetPage page ->
        { model with page = page }, Cmd.none
    | ToggleNavMenu ->
        { model with navMenuOpen = not model.navMenuOpen }, Cmd.none

/// Connects the routing system to the Elmish application.
let router = Router.infer SetPage (fun model -> model.page)

let homePage model dispatch =
    empty

let view model dispatch =
    Navbar.createNavbarWithTitle "Bolero x Bulma"
    |> Navbar.addLeftMenuItem (Navbar.Link ("Columns", Some "/columns"))
    |> Navbar.addLeftMenuItem (Navbar.Link ("Layout", Some "/layout"))
    |> Navbar.addLeftMenuItem (Navbar.Link ("Form", Some "/form"))
    |> Navbar.addLeftMenuItem (Navbar.Link ("Elements", Some "/elements"))
    |> Navbar.addLeftMenuItem (Navbar.Link ("Components", Some "/components"))
    |> Navbar.withColor Dark
    |> Navbar.setBurgerStatus model.navMenuOpen (fun _ -> dispatch ToggleNavMenu)
    |> createNode

type MyApp() =
    inherit ProgramComponent<Model, Message>()

    override this.Program =
        Program.mkProgram (fun _ -> initModel, Cmd.none) update view
        |> Program.withRouter router
#if DEBUG
        |> Program.withHotReload
#endif
