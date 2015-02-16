namespace Fsvd
    module Bot = 
        open Shared
        open Ai
        open Client
        open Game
        open System.Threading.Tasks

        let botloop (client:Client.Client) = 
            let startBrowser (url:string) = 
                try
                    System.Diagnostics.Process.Start(url) |> ignore
                with 
                    | ex -> printf "Couldn't open browser: %s" (ex.ToString())
                printf "ViewUrl: %s" url

            let rec innerBotLoop playUrl myHeroId (tile: BfsAdjVertex) = 
                let firstStep = (findFirstStep tile |> sprintf "%A")
                let doc = client.Move playUrl firstStep 
                let tiles = findTiles doc 
                let nextTile = ai doc (tiles)
                if not doc.Game.Finished then
                    innerBotLoop doc.PlayUrl doc.Hero.Id nextTile

            let cg = client.CreateGame
            Task.Run(fun () -> startBrowser cg.ViewUrl) |> ignore

            let tiles = findTiles cg 
            let tileOfInterest = ai cg tiles
            innerBotLoop cg.PlayUrl cg.Hero.Id tileOfInterest 