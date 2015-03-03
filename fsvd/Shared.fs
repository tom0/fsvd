namespace Fsvd
    module Shared =
        open FSharp.Data

        type Response = JsonProvider<"./sample_response.json">
        
        type Direction = North | East | South | West | Stay
        type Tile = Free | Forest | Mine of Option<int> | Hero of int | Tavern
        type AdjVertex = { id: int; tile: Tile; direction: Direction }
        type BfsAdjVertex = { source: Option<BfsAdjVertex>; distance: int; vertex: AdjVertex }
        type HeroTiles = { Id: int; Tiles: BfsAdjVertex list }
        
        let rec findFirstStep tile =
            match tile with 
            | Some { source = Some { source = None }; vertex = v } -> v.direction
            | Some { source = Some x } as t -> findFirstStep (Some x)
            | None -> Stay