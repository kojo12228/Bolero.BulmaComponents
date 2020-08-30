namespace Bolero.BulmaComponents

type INodeable =
    abstract member ToNode: unit -> Bolero.Node

[<AutoOpen>]
module Utils =
    let createNode (nodeable: 'T when 'T :> INodeable) =
        nodeable.ToNode()

module Option =
    let boolMap value cond =
        if cond then Some value else None

[<AutoOpen>]
module Shared =
    type Alignment =
        | AlignLeft
        | AlignCenter
        | AlignRight
        override this.ToString() =
            match this with
            | AlignLeft -> ""
            | AlignCenter -> "is-centered"
            | AlignRight -> "is-right"

    type ComponentColor =
        | White
        | Light
        | Dark
        | Black
        | Primary
        | Link
        | Info
        | Success
        | Warning
        | Danger

    let internal colorToString c =
        match c with
        | White -> "is-white"
        | Light -> "is-light"
        | Dark -> "is-dark"
        | Black -> "is-black"
        | Primary -> "is-primary"
        | Link -> "is-link"
        | Info -> "is-info"
        | Success -> "is-success"
        | Warning -> "is-warning"
        | Danger -> "is-danger"