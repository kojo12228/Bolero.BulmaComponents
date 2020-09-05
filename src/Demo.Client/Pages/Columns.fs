module Demo.Client.Pages.Columns

open System
open Elmish
open Bolero
open Bolero.Html

type Model = unit

let initModel = ()

type Message = unit

let update message model =
    (), Cmd.none

let view model dispatch =
    empty