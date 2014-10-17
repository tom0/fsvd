namespace Fsvd

    module Ai = 
        open Shared

        let ai (doc:Response.Root) =

            let myHero = doc.Hero
            let game = doc.Game

            let mineCollector tile =
                match tile with
                | (Mine x) as m when x <> (Some myHero.Id) -> Some(m)
                | _ -> None 

            let tavernCollector tile =
                match tile with
                | Tavern as m -> Some(m)
                | _ -> None 

            match Shared.distance (Game.findTile doc tavernCollector) with
            | Some(healthDistance) when healthDistance <= 1 && myHero.Life < 90 -> tavernCollector
            | _ when myHero.Life < 30 -> tavernCollector
            | _ -> mineCollector