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

    let internal defaultNull opt =
        Option.defaultValue null opt

    let internal defaultNullable (opt: 'a option) =
        Option.toNullable opt

    let internal defaultIgnore optFunc =
        Option.defaultValue ignore optFunc

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

    let internal textColorToString c =
        match c with
        | White -> "has-text-white"
        | Light -> "has-text-light"
        | Dark -> "has-text-dark"
        | Black -> "has-text-black"
        | Primary -> "has-text-primary"
        | Link -> "has-text-link"
        | Info -> "has-text-info"
        | Success -> "has-text-success"
        | Warning -> "has-text-warning"
        | Danger -> "has-text-danger"