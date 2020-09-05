module Demo.Client.Pages.Elements

open System
open Elmish
open Bolero
open Bolero.Html

open Bolero.BulmaComponents
open Bolero.BulmaComponents.Elements

open Demo.Client.Common.DocSection

type Model = unit

let initModel = ()

let titleIdMap =
    [
        "Box", "box"
        "Button", "button"
        "List of Buttons", "button-list"
        "Content", "content"
        "Delete button", "delete-button"
        "Icon", "icon"
        "Notification", "notification"
        "Progress bar", "progress"
        "Table", "table"
        "Title", "title"
    ]
    |> Map.ofList

type Message = unit

let update message model =
    (), Cmd.none

let private elementsNamespace m =
    sprintf "Bolero.BulmaComponents.Elements.%s" m

let private documentationLink el =
    sprintf "https://bulma.io/documentation/elements/%s" el

let private viewBox() =
    {
        Title = "Box"
        TitleIdMap = titleIdMap
        NamespaceSubtitle = elementsNamespace "Box"
        DocButtonText = "Box documentation"
        DocButtonHref = documentationLink "box"
        Content = empty
    }
    |> createNode

let viewButton() =
    {
        Title = "Button"
        TitleIdMap = titleIdMap
        NamespaceSubtitle = elementsNamespace "Button"
        DocButtonText = "Button documentation"
        DocButtonHref = documentationLink "button"
        Content = empty
    }
    |> createNode

let viewButtonList() =
    {
        Title = "List of Buttons"
        TitleIdMap = titleIdMap
        NamespaceSubtitle = elementsNamespace "ButtonList"
        DocButtonText = "List of buttons documentation"
        DocButtonHref = documentationLink "button/#list-of-buttons"
        Content = empty
    }
    |> createNode

let viewContent() =
    {
        Title = "Content"
        TitleIdMap = titleIdMap
        NamespaceSubtitle = elementsNamespace "Content"
        DocButtonText = "Content documentation"
        DocButtonHref = documentationLink "content"
        Content = empty
    }
    |> createNode

let viewDeleteButton() =
    {
        Title = "Delete button"
        TitleIdMap = titleIdMap
        NamespaceSubtitle = elementsNamespace "DeleteButton"
        DocButtonText = "Delete documentation"
        DocButtonHref = documentationLink "delete"
        Content = empty
    }
    |> createNode

let viewIcon() =
    {
        Title = "Icon"
        TitleIdMap = titleIdMap
        NamespaceSubtitle = elementsNamespace "Icon"
        DocButtonText = "Icon documentation"
        DocButtonHref = documentationLink "icon"
        Content = empty
    }
    |> createNode

let viewNotification() =
    {
        Title = "Notification"
        TitleIdMap = titleIdMap
        NamespaceSubtitle = elementsNamespace "Notification"
        DocButtonText = "Notification documentation"
        DocButtonHref = documentationLink "notification"
        Content = empty
    }
    |> createNode

let viewProgress() =
    {
        Title = "Progress bar"
        TitleIdMap = titleIdMap
        NamespaceSubtitle = elementsNamespace "Progress"
        DocButtonText = "Progress bars documentation"
        DocButtonHref = documentationLink "progress"
        Content = empty
    }
    |> createNode

let viewTable() =
    {
        Title = "Table"
        TitleIdMap = titleIdMap
        NamespaceSubtitle = elementsNamespace "Table"
        DocButtonText = "Table documentation"
        DocButtonHref = documentationLink "table"
        Content = empty
    }
    |> createNode

let viewTitle() =
    {
        Title = "Title"
        TitleIdMap = titleIdMap
        NamespaceSubtitle = elementsNamespace "Title"
        DocButtonText = "Title documentation"
        DocButtonHref = documentationLink "title"
        Content = empty
    }
    |> createNode

let view model dispatch =
    concat [
        Title.titleH2 "Elements"
        |> createNode

        viewBox()
        viewButton()
        viewButtonList()
        viewContent()
        viewDeleteButton()
        viewNotification()
        viewProgress()
        viewTable()
        viewTitle()
    ]