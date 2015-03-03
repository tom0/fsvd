namespace Fsvd

    module Ai = 
        open Shared

        let ai (doc:Response.Root) (tiles: HeroTiles[]) =

            let myHero = doc.Hero
            let myHeroTiles = (tiles |> Array.find (fun ht -> ht.Id = doc.Hero.Id)).Tiles
            let otherHeroTiles = 
                tiles 
                |> Array.filter(fun ht -> ht.Id <> doc.Hero.Id)
                |> List.ofArray
                |> List.collect (fun ht -> ht.Tiles)

            let tavernFilter (vertex:BfsAdjVertex) = vertex.vertex.tile = Tavern

            let mineFilter (vertex:BfsAdjVertex) =
                match vertex.vertex.tile with
                | Mine None -> true
                | Mine (Some x) when x <> myHero.Id -> true
                | _ -> false

            let orderedTiles = myHeroTiles |> List.sortBy (fun t -> t.distance)

            let filter = 
                if myHero.Life <= 30 then
                    // Go to a tavern that doesn't have a rival next to it.
                    let tavernsThatRivalsAreCloseTo = 
                        otherHeroTiles 
                        |> List.filter tavernFilter
                        |> List.filter (fun t -> t.distance <= 5)
                        |> Set.ofList

                    (fun t -> (tavernFilter t) && (tavernsThatRivalsAreCloseTo |> Set.exists((=) t) |> not))
                else
                    if myHero.Life <= 50 then
                        // Go to a mine that doesn't have a rival next to it.
                        let minesThatRivalsAreCloseTo = 
                            otherHeroTiles 
                            |> List.filter mineFilter
                            |> List.filter (fun t -> t.distance <= 5)
                            |> Set.ofList

                        (fun t -> (mineFilter t) && (minesThatRivalsAreCloseTo |> Set.exists((=) t) |> not))
                    else
                        mineFilter

            let filteredTiles = myHeroTiles |> List.filter filter |> List.sortBy (fun v -> v.distance) 

            match filteredTiles with
            | [] -> None
            | x::xs -> Some(x)