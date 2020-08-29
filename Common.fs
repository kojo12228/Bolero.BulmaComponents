namespace Bolero

type INodeable =
    abstract member ToNode: unit -> Bolero.Node

[<AutoOpen>]
module Utils =
    let createNode (nodeable: 'T when 'T :> INodeable) =
        nodeable.ToNode()