namespace Fsvd

    open FSharp.Data

    module Client = 
        open Shared
        open Ai

        open HttpRequestHeaders
        open HttpContentTypes
        open Game

        [<Literal>]
        let apiKey = "05erfbm8"
        [<Literal>]
        let uriBase = "http://vindinium.org/api/training"


        
        let move playUrl direction = 
            let response = Http.RequestString(playUrl, query = [ "key", apiKey; "dir", direction], headers = [ Accept Json ], httpMethod="POST")
            Response.Parse(response)


        let rec moveHero playUrl myHeroId (tiles: BfsAdjVertex list) = 

            let doc = move playUrl (direction tiles)

            let tilesOfInterest = findTile doc (ai doc)

            if not doc.Game.Finished then
                moveHero doc.PlayUrl doc.Hero.Id tilesOfInterest
            
        let createGame =
            let response = Http.RequestString(uriBase, query = [ "key", apiKey; "turns", "300" ], headers = [ Accept Json ], httpMethod="POST")
            let doc = Response.Parse(response)
            System.Diagnostics.Process.Start(doc.ViewUrl) |> ignore

            let tilesOfInterest = findTile doc (ai doc)
            moveHero doc.PlayUrl doc.Hero.Id tilesOfInterest |> ignore