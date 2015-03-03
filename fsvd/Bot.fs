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

            let getTargetTile doc =
                let adjacencyLists = adjs doc
                let heroTiles = doc.Game.Heroes |> Array.map (fun h -> { Id = h.Id; Tiles = findTiles doc.Game.Board.Size adjacencyLists h } )
                ai doc heroTiles

            let rec innerBotLoop playUrl (tile: BfsAdjVertex option) = 
                let firstStep = (findFirstStep tile |> sprintf "%A")
                let doc = client.Move playUrl firstStep 
                let nextTile = getTargetTile doc
                if not doc.Game.Finished then
                    innerBotLoop doc.PlayUrl nextTile

            let cg = client.CreateGame
            Task.Run(fun () -> startBrowser cg.ViewUrl) |> ignore

            let nextTile = getTargetTile cg
            innerBotLoop cg.PlayUrl nextTile 