module Demo.Client.Pages.Columns

open System
open Elmish
open Bolero
open Bolero.Html

open Bolero.BulmaComponents
open Bolero.BulmaComponents.Columns
open Bolero.BulmaComponents.Elements

open Demo.Client.Common.DocSection

type Model = unit

let initModel = ()

type Message = unit

let update message model =
    (), Cmd.none

let coloredCard text =
    Notification.createNotification()
    |> Notification.withText text
    |> Notification.withColor Primary
    |> createNode

let coloredCard2 text =
    Notification.createNotification()
    |> Notification.withText text
    |> Notification.withColor Info
    |> createNode

let viewColumns() =
    let content =
        concat [
            emptyColumns()
            |> addColumns [
                for _ in 0 .. 3 ->
                    createColumn (coloredCard "column")
            ]
            |> createNode

            fsCodeBlock [
                "let coloredCard text ="
                "    Notification.createNotification()"
                "    |> Notification.withText text"
                "    |> Notification.withColor Primary"
                "    |> createNode"
                ""
                "emptyColumns()"
                "|> addColumns ["
                "    for _ in 0 .. 3 ->"
                "        createColumn (coloredCard \"column\")"
                "]"
                "|> createNode"
            ]

            Content.surroundContent (
                p [] [
                    text "Support for setting size:"
                ]
            )

            emptyColumns()
            |> addColumns [
                createColumn (coloredCard "column is-4") |> withSize Col4
                createColumn (coloredCard "column is-2") |> withSize Col2
                createColumn (coloredCard "column is-4") |> withSize Col4
                createColumn (coloredCard "column is-2") |> withSize Col2
            ]
            |> createNode

            fsCodeBlock [
                "emptyColumns()"
                "|> addColumns ["
                "    createColumn (coloredCard \"column is-4\") |> withSize Col4"
                "    createColumn (coloredCard \"column is-2\") |> withSize Col2"
                "    createColumn (coloredCard \"column is-4\") |> withSize Col4"
                "    createColumn (coloredCard \"column is-2\") |> withSize Col2"
                "]"
                "|> createNode"
            ]

            Content.surroundContent (
                p [] [
                    text "Support for setting breakpoints, multiline and many other options:"
                ]
            )

            emptyColumns()
            |> addColumns [
                for _ in 0 .. 6 ->
                    createColumn (coloredCard2 "column is-6 is-4-desktop")
                    |> withBreakpointSize Col6 Normal
                    |> withBreakpointSize Col4 Desktop
            ]
            |> setMultiline
            |> createNode

            fsCodeBlock [
                "emptyColumns()"
                "|> addColumns ["
                "    for _ in 0 .. 6 ->"
                "        createColumn (coloredCard2 \"column is-6 is-4-desktop\")"
                "        |> withBreakpointSize Col6 Normal"
                "        |> withBreakpointSize Col4 Desktop"
                "]"
                "|> setMultiline"
                "|> createNode"
            ]
        ]

    {
        Title = "Columns"
        TitleIdMap = [ "Columns", "cols" ] |> Map.ofList
        NamespaceSubtitle = "Bolero.BulmaComponents.Columns"
        DocButtonText = "Columns documentation"
        DocButtonHref = "https://bulma.io/documentation/columns/"
        Content = content
    }
    |> createNode

let view model dispatch =
    concat [
        Title.titleH2 "Elements"
        |> createNode

        viewColumns()
    ]