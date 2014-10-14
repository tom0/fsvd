namespace Fsvd

    module Ai =
    
        open Shared


        let mineCollector myHeroId tile =
            match tile with
            | (Mine x) as m when x <> (Some myHeroId) -> Some(m)
            | _ -> None 

        let tavernCollector myHeroId tile =
            match tile with
            | Tavern as m -> Some(m)
            | _ -> None 

        let ai (doc:Response.Root) =

            let myHero = doc.Hero
            let game = doc.Game

            let healthDistance = Shared.distance (Game.findTile doc (tavernCollector myHero.Id))

            if (healthDistance < 3 && myHero.Life <99) then
                tavernCollector myHero.Id
            elif myHero.Life < 30 then
                tavernCollector myHero.Id
            else 
                mineCollector myHero.Id
