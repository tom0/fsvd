namespace Fsvd

    module Ai = 
        open Shared

        let ai (doc:Response.Root) (tiles: BfsAdjVertex list) =

            let myHero = doc.Hero
            let game = doc.Game

            let tavernFilter (vertex:BfsAdjVertex) =
                match vertex.vertex.tile with
                | Tavern -> true
                | _ -> false

            let mineFilter (vertex:BfsAdjVertex) =
                match vertex.vertex.tile with
                | Mine None -> true
                | Mine (Some x) when x <> myHero.Id -> true
                | _ -> false

            let filter = 
                if myHero.Life <= 30 then
                    tavernFilter
                else
                    mineFilter
            
            tiles |> List.filter filter |> List.sortBy (fun v -> v.distance) |> List.head