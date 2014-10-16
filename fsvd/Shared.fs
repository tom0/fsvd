namespace Fsvd
    module Shared =
        open FSharp.Data

        type Response = JsonProvider<"./sample_response.json">
        
        type Direction = North | East | South | West | Stay
        type Tile = Free | Forest | Mine of Option<int> | Hero of int | Tavern
        type AdjVertex = { id: int; tile: Tile; direction: Direction }
        type BfsAdjVertex = { source: Option<BfsAdjVertex>; distance: int; vertex: AdjVertex }

        
        let rec findFirstStep tile =
            match tile with 
            | { source = Some { source = None }; vertex = v } -> v.direction
            | { source = Some x } -> findFirstStep x

        let direction tiles = 
            match tiles with
            | [] -> Stay
            | x::xs -> xs |> List.rev |> List.head |> findFirstStep 
            |> sprintf "%A"

        let distance (tiles:BfsAdjVertex list) = 
            let first = tiles |> List.head

            first.distance