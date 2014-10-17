namespace Fsvd
    module Bot = 
        open Shared
        open Ai
        open Client
        open Game

        let botloop (client:Client.Client) = 
            let rec innerBotLoop playUrl myHeroId (tiles: BfsAdjVertex list) = 
                let doc = client.Move playUrl (direction tiles) 
                let tilesOfInterest = findTile doc (ai doc)
                if not doc.Game.Finished then
                    innerBotLoop doc.PlayUrl doc.Hero.Id tilesOfInterest
            let cg = client.CreateGame
            try
                System.Diagnostics.Process.Start(cg.ViewUrl) |> ignore
            with 
                | ex -> printf "Couldn't open browser: %s" (ex.ToString())
            printf "ViewUrl: %s" cg.ViewUrl
            let tilesOfInterest = findTile cg (ai cg)
            innerBotLoop cg.PlayUrl cg.Hero.Id tilesOfInterest 