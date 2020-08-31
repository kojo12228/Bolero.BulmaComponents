namespace Bolero.BulmaComponents

open System

type INodeable =
    abstract member ToNode: unit -> Bolero.Node

[<AutoOpen>]
module Utils =
    let createNode (nodeable: 'T when 'T :> INodeable) =
        nodeable.ToNode()

module Option =
    let internal boolMap value cond =
        if cond then Some value else None

    let internal valOrNull opt =
        match opt with
        | Some value -> value
        | None -> null

    let internal valOrNullable (opt: 'a option) =
        match opt with
        | Some nVal -> Nullable(nVal)
        | None -> Nullable()

    let internal valOrIgnore optFunc =
        match optFunc with
        | Some f -> f
        | None -> ignore

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

    type Size =
        | Small
        | Normal
        | Medium
        | Large
        member this.ToStringWithNormal() =
            match this with
            | Small -> "is-small"
            | Normal -> "is-normal"
            | Medium -> "is-medium"
            | Large -> "is-large"

        member this.ToStringWithoutNormal() =
            match this with
            | Small -> "is-small"
            | Normal -> ""
            | Medium -> "is-medium"
            | Large -> "is-large"

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