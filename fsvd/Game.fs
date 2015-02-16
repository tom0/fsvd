namespace Fsvd
    module Game = 
    
        open Shared
  
        let parseHeroTile heroChar tileCtor = 
            match heroChar with
            | '1' -> tileCtor 1
            | '2' -> tileCtor 2
            | '3' -> tileCtor 3
            | '4' -> tileCtor 4

        let parseBoardChars rowChars = 
            let rec innerParseRow rowChars acc =
                match rowChars with
                | [] -> List.rev acc
                | '@'::x::xs -> innerParseRow xs ((parseHeroTile x Hero) :: acc)
                | '$'::'-'::xs -> innerParseRow xs ((Mine None) :: acc)
                | '$'::x::xs -> innerParseRow xs ((parseHeroTile x (Some >> Mine)) :: acc)
                | '['::_::xs -> innerParseRow xs (Tavern :: acc)
                | ' '::_::xs -> innerParseRow xs (Free :: acc)
                | _::_::xs -> innerParseRow xs (Forest :: acc)
            innerParseRow rowChars []

        let tavernHeroMineCollector tile = 
            match tile with
            | Tavern as t -> Some(t)
            | Hero x as h -> Some(h)
            | Mine x as m -> Some(m)
            | _ -> None 
        
        let rec bfs (work: BfsAdjVertex list) (adjacencyList:(Tile * seq<AdjVertex>)[]) (collector: Tile -> Option<Tile>) (visited:Set<int>) (acc:BfsAdjVertex list) : BfsAdjVertex list =
            match work with
            | [] -> acc
            | x::xs -> 
                let newWork = 
                    match x.vertex.tile with
                    | Free | Hero _ -> 
                        (snd adjacencyList.[x.vertex.id]) |> 
                        Seq.filter (fun a -> not (visited.Contains a.id)) |>
                        Seq.map (fun a -> { source = Some x; distance = x.distance+1; vertex=a }) |>
                        Seq.toList 
                    | _ -> []
                let newVisited = newWork |> List.fold (fun (a:Set<int>) w -> (a.Add w.vertex.id)) visited
                match collector x.vertex.tile with
                | Some t -> bfs (xs@newWork) adjacencyList collector newVisited (x::acc)
                | None -> bfs (xs@newWork) adjacencyList collector newVisited acc 

        let bfs2 startingId collector (adjacencyList:(Tile * seq<AdjVertex>)[]) =  
            let startingVertex = { id = startingId; tile = fst adjacencyList.[startingId]; direction = Stay }
            let startingBfsVertex = { source = None; distance = 0; vertex = startingVertex }
            bfs [startingBfsVertex] adjacencyList collector (Set.singleton startingVertex.id) []

        let parseBoard size boardString =
            let innerParseBoard size (vertexList:Tile list) =
                let adjacencyDeltas = [(-size, North); (size, South); (-1, West); (1, East)]

                let adjacencyPositions currentPosition = 
                    adjacencyDeltas |> 
                    List.map (fun (x, dir) -> (x+currentPosition, dir)) |> 
                    List.filter (fun (a, _) -> a >= 0 && a < vertexList.Length && System.Math.Abs(currentPosition % size - a % size) <= 1) 

                let adjacencyListPrepare x = 
                    let neighbourIds = adjacencyPositions x
                    let createAdj (y, dir) = { id = y; tile = List.nth vertexList y; direction = dir }
                    neighbourIds |> Seq.map createAdj |> Seq.filter (fun { tile = a } -> match a with
                                                                                         | Free | Hero _ | Tavern | Mine _ -> true
                                                                                         | _ -> false)
                    
                let adjacencyList = vertexList |> Seq.mapi (fun i v -> v, adjacencyListPrepare i)
                adjacencyList |> Seq.toArray
            parseBoardChars (boardString |> Seq.toList) |> innerParseBoard size
            
        let startingPos (doc:Response.Root) = doc.Hero.Pos.Y + doc.Hero.Pos.X*doc.Game.Board.Size
        
        let adjs (doc:Response.Root) = parseBoard doc.Game.Board.Size doc.Game.Board.Tiles 

        let tileCollector tile = 
            match tile with
            | Forest | Tavern | Mine _ | Hero _ as t -> Some(t)
            | _ -> None

        let findTiles (doc:Response.Root) = 
            let starting = (startingPos doc)
            printfn "%A" starting
            bfs2 starting tileCollector (adjs doc)