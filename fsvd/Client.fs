namespace Fsvd
    module Client = 
        open FSharp.Data
        open System
        open Shared
        open Ai

        open HttpRequestHeaders
        open HttpContentTypes
        open Game

        type Client(apiKey: string, live: bool, turns: string) = 

            [<Literal>]
            let uriBase = "http://vindinium.org/api/"

            let headers = [ Accept Json ]

            member this.Move playUrl direction = 
                let response = Http.RequestString(playUrl, query = [ "key", apiKey; "dir", direction], headers = headers, httpMethod="POST")
                Response.Parse(response)

            member this.CreateGame =
                let url = (new Uri(new Uri(uriBase), (if live then "arena" else "training"))).ToString()
                let body = sprintf """{ "turns": "%s" }""" turns
                let response = Http.RequestString(url, query = [ "key", apiKey ], body = TextRequest body, headers = headers, httpMethod="POST")
                Response.Parse(response)