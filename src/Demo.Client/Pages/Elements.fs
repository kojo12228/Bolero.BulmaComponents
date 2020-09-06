module Demo.Client.Pages.Elements

open System
open Elmish
open Bolero
open Bolero.Html

open Bolero.BulmaComponents
open Bolero.BulmaComponents.Elements

open Demo.Client.Common.DocSection

type Model =
    {
        buttonClickMsg: string option
        buttonListClickMsg: string option
        deleteClickMsg: string option
    }

let initModel =
    {
        buttonClickMsg = None
        buttonListClickMsg = None
        deleteClickMsg = None
    }

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

type Message =
    | ExampleButtonClicked
    | ButtonListClicked of string
    | DeleteClicked

let update message model =
    match message with
    | ExampleButtonClicked ->
        { model with buttonClickMsg = Some "Button click event registered" }, Cmd.none
    | ButtonListClicked msg ->
        { model with buttonListClickMsg = Some msg }, Cmd.none
    | DeleteClicked ->
        { model with deleteClickMsg = Some "Delete button clicked" }, Cmd.none

let private elementsNamespace m =
    sprintf "Bolero.BulmaComponents.Elements.%s" m

let private documentationLink el =
    sprintf "https://bulma.io/documentation/elements/%s" el

let private viewBox() =
    let content =
        concat [
            Content.surroundContent (
                p [] [
                    text "A simple box to surround other components."
                ]
            )

            Box.box (
                text "Example box"
            )

            fsCodeBlock [ "Box.box (text \"Example bbox\")" ]
        ]

    {
        Title = "Box"
        TitleIdMap = titleIdMap
        NamespaceSubtitle = elementsNamespace "Box"
        DocButtonText = "Box documentation"
        DocButtonHref = documentationLink "box"
        Content = content
    }
    |> createNode

let viewButton model dispatch =
    let content =
        concat [
            Content.surroundContent (
                p [] [
                    text "A styled button, with a click event"
                ]
            )

            Button.createButton()
            |> Button.setTextContent "Example button"
            |> Button.withColor Success
            |> Button.setRounded
            |> Button.setOnClick (fun _ -> dispatch ExampleButtonClicked)
            |> createNode

            cond model.buttonClickMsg <| function
            | None -> empty
            | Some msg -> Content.surroundContent <| p [] [ strong [] [ text msg ] ]

            fsCodeBlock [
                "Button.createButton()"
                "|> Button.setTextContent \"Example button\""
                "|> Button.withColor Success"
                "|> Button.setRounded"
                "|> Button.setOnClick (fun _ -> dispatch ExampleButtonClicked)"
                "|> createNode"
            ]
        ]

    {
        Title = "Button"
        TitleIdMap = titleIdMap
        NamespaceSubtitle = elementsNamespace "Button"
        DocButtonText = "Button documentation"
        DocButtonHref = documentationLink "button"
        Content = content
    }
    |> createNode

let viewButtonList model dispatch =
    let content =
        concat [
            Content.surroundContent (
                p [] [
                    text "Grouping together buttons (separate from field button group).
                    Note, this control onlly works with button tags, not a or input tags."
                ]
            )

            ButtonList.createButtonList [
                Button.createButton()
                |> Button.setTextContent "Button 1"
                |> Button.setOnClick (
                    fun _ ->
                        dispatch (ButtonListClicked "Button 1 clicked"))

                Button.createButton()
                |> Button.setTextContent "Button 2"
                |> Button.setOnClick (
                    fun _ ->
                        dispatch (ButtonListClicked "Button 2 clicked"))

                Button.createButton()
                |> Button.setTextContent "Button 3"
                |> Button.setOnClick (
                    fun _ ->
                        dispatch (ButtonListClicked "Button 3 clicked"))
            ]
            |> createNode

            cond model.buttonListClickMsg <| function
            | None -> empty
            | Some msg -> Content.surroundContent <| p [] [ strong [] [ text msg ] ]

            fsCodeBlock [
                "ButtonList.createButtonList ["
                "    Button.createButton()"
                "    |> Button.setTextContent \"Button 1\""
                "    |> Button.setOnClick ("
                "        fun _ ->"
                "            dispatch (ButtonListClicked \"Button 1 clicked\"))"
                ""
                "    Button.createButton()"
                "    |> Button.setTextContent \"Button 2\""
                "    |> Button.setOnClick ("
                "        fun _ ->"
                "            dispatch (ButtonListClicked \"Button 2 clicked\"))"
                ""
                "    Button.createButton()"
                "    |> Button.setTextContent \"Button 3\""
                "    |> Button.setOnClick ("
                "        fun _ ->"
                "            dispatch (ButtonListClicked \"Button 3 clicked\"))"
                "]"
                "|> createNode"
            ]
        ]

    {
        Title = "List of Buttons"
        TitleIdMap = titleIdMap
        NamespaceSubtitle = elementsNamespace "ButtonList"
        DocButtonText = "List of buttons documentation"
        DocButtonHref = documentationLink "button/#list-of-buttons"
        Content = content
    }
    |> createNode

let viewContent() =
    let content =
        concat [
            Content.surroundContent (
                p [] [
                    text "The "
                    code [] [ text "content" ]
                    text " allows for working directly with, mainly text, HTML tags."
                ]
            )

            Content.surroundContent (
                p [] [
                    text "Lists without "
                    code [] [ text "Content.surroundContent" ]
                ]
            )

            ul [] [
                li [] [ text "Item alpha"]
                li [] [ text "Item beta" ]
                li [] [ text "Item gamma" ]
            ]
            ol [] [
                li [] [ text "Item 1" ]
                li [] [ text "Item 2" ]
                li [] [ text "Item 3" ]
            ]

            Content.surroundContent (
                p [] [
                    text "Lists with "
                    code [] [ text "Content.surroundContent" ]
                ]
            )

            Content.surroundContent (
                concat [
                    ul [] [
                        li [] [ text "Item alpha"]
                        li [] [ text "Item beta" ]
                        li [] [ text "Item gamma" ]
                    ]
                    ol [] [
                        li [] [ text "Item 1" ]
                        li [] [ text "Item 2" ]
                        li [] [ text "Item 3" ]
                    ]
                ]
            )

            fsCodeBlock [
                "Content.surroundContent ("
                "    concat ["
                "        ul [] ["
                "            li [] [ text \"Item alpha\"]"
                "            li [] [ text \"Item beta\" ]"
                "            li [] [ text \"Item gamma\" ]"
                "        ]"
                "        ol [] ["
                "            li [] [ text \"Item 1\" ]"
                "            li [] [ text \"Item 2\" ]"
                "            li [] [ text \"Item 3\" ]"
                "        ]"
                "    ]"
                ")"
            ]
        ]

    {
        Title = "Content"
        TitleIdMap = titleIdMap
        NamespaceSubtitle = elementsNamespace "Content"
        DocButtonText = "Content documentation"
        DocButtonHref = documentationLink "content"
        Content = content
    }
    |> createNode

let viewDeleteButton model dispatch =
    let content =
        concat [
            Content.surroundContent (
                p [] [
                    text "A delete button/icon to be used with other components."
                ]
            )

            DeleteButton.createDelete()
            |> DeleteButton.withOnClick (fun _ -> dispatch DeleteClicked)
            |> createNode

            DeleteButton.createDelete()
            |> DeleteButton.withSize Large
            |> createNode

            cond model.deleteClickMsg <| function
            | None -> empty
            | Some msg -> Content.surroundContent <| p [] [ strong [] [ text msg ] ]

            fsCodeBlock [
                "DeleteButton.createDelete()"
                "|> DeleteButton.withOnClick (fun _ -> dispatch DeleteClicked)"
                "|> createNode"
                ""
                "DeleteButton.createDelete()"
                "|> DeleteButton.withSize Large"
                "|> createNode"
            ]
        ]

    {
        Title = "Delete button"
        TitleIdMap = titleIdMap
        NamespaceSubtitle = elementsNamespace "DeleteButton"
        DocButtonText = "Delete documentation"
        DocButtonHref = documentationLink "delete"
        Content = content
    }
    |> createNode

let viewIcon() =
    let content =
        concat [
            Content.surroundContent (
                p [] [
                    text "Allows for working with icons. Note, the implementation
                    is independent of a specific icon library, which you will
                    need to add. These examples use Font Awesome."
                ]
            )

            Icon.createIcon "fas fa-info-circle"
            |> Icon.withColor Info
            |> createNode

            fsCodeBlock [
                "Icon.createIcon \"fas fa-info-circle\""
                "|> Icon.withColor Info"
                "|> createNode"
            ]
        ]

    {
        Title = "Icon"
        TitleIdMap = titleIdMap
        NamespaceSubtitle = elementsNamespace "Icon"
        DocButtonText = "Icon documentation"
        DocButtonHref = documentationLink "icon"
        Content = content
    }
    |> createNode

let viewNotification() =
    let content =
        concat [
            Content.surroundContent (
                p [] [
                    text "A simple component for displaying notifications
                    within the website."
                ]
            )

            Notification.createNotification()
            |> Notification.withColor Danger
            |> Notification.withText "Danger notification"
            |> Notification.withOnDeleteClick (ignore)
            |> createNode

            fsCodeBlock [
                "Notification.createNotification()"
                "|> Notification.withColor Danger"
                "|> Notification.withText \"Danger notification\""
                "|> Notification.withOnDeleteClick (ignore)"
                "|> createNode"
            ]
        ]

    {
        Title = "Notification"
        TitleIdMap = titleIdMap
        NamespaceSubtitle = elementsNamespace "Notification"
        DocButtonText = "Notification documentation"
        DocButtonHref = documentationLink "notification"
        Content = content
    }
    |> createNode

let viewProgress() =
    let content =
        concat [
            Progress.createProgress()
            |> Progress.withPercentage 20
            |> Progress.setColor Info
            |> createNode

            Progress.createProgress()
            |> Progress.withPercentage 60
            |> Progress.setColor Success
            |> Progress.setSize Large
            |> createNode

            Progress.createProgress()
            |> Progress.withPercentage 25
            |> Progress.setColor Primary
            |> Progress.setIndeterminate
            |> createNode

            fsCodeBlock [
                "Progress.createProgress()"
                "|> Progress.withPercentage 20"
                "|> Progress.setColor Info"
                "|> createNode"
                " "
                "Progress.createProgress()"
                "|> Progress.withPercentage 60"
                "|> Progress.setColor Success"
                "|> Progress.setSize Large"
                "|> createNode"
                ""
                "Progress.createProgress()"
                "|> Progress.withPercentage 25"
                "|> Progress.setColor Primary"
                "|> Progress.setIndeterminate"
                "|> createNode"
            ]
        ]

    {
        Title = "Progress bar"
        TitleIdMap = titleIdMap
        NamespaceSubtitle = elementsNamespace "Progress"
        DocButtonText = "Progress bars documentation"
        DocButtonHref = documentationLink "progress"
        Content = content
    }
    |> createNode

let viewTable() =
    let content =
        concat [
            Table.createTable()
            |> Table.withHeadersText [ "Column 1"; "Column 2"; "Column 3" ]
            |> Table.withBodyText (array2D [
                [ "1"; "2"; "3" ]
                [ "This row"; "is"; "highlighted" ]
                [ "4"; "5"; "6" ]
            ])
            |> Table.setBordered
            |> Table.withSelectedRow 1
            |> createNode

            fsCodeBlock [
                "Table.createTable()"
                "|> Table.withHeadersText [ \"Column 1\"; \"Column 2\"; \"Column 3\" ]"
                "|> Table.withBodyText (array2D ["
                "    [ \"1\"; \"2\"; \"3\" ]"
                "    [ \"This row\"; \"is\"; \"highlighted\" ]"
                "    [ \"4\"; \"5\"; \"6\" ]"
                "])"
                "|> Table.setBordered"
                "|> Table.withSelectedRow 1"
                "|> createNode"
            ]
        ]

    {
        Title = "Table"
        TitleIdMap = titleIdMap
        NamespaceSubtitle = elementsNamespace "Table"
        DocButtonText = "Table documentation"
        DocButtonHref = documentationLink "table"
        Content = content
    }
    |> createNode

let viewTitle() =
    let content =
        concat [
            Title.titleH1 "A sample title"
            |> createNode
            Title.subtitleH3 "With a sample subtitle"
            |> createNode

            fsCodeBlock [
                "Title.titleH1 \"A sample title\""
                "|> createNode"
                "Title.subtitleH3 \"With a sample subtitle\""
                "|> createNode"
            ]
        ]

    {
        Title = "Title"
        TitleIdMap = titleIdMap
        NamespaceSubtitle = elementsNamespace "Title"
        DocButtonText = "Title documentation"
        DocButtonHref = documentationLink "title"
        Content = content
    }
    |> createNode

let view model dispatch =
    concat [
        Title.titleH2 "Elements"
        |> createNode

        viewBox()
        viewButton model dispatch
        viewButtonList model dispatch
        viewContent()
        viewDeleteButton model dispatch
        viewIcon()
        viewNotification()
        viewProgress()
        viewTable()
        viewTitle()
    ]