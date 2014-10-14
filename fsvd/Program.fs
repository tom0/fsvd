// Learn more about F# at http://fsharp.net
// See the 'F# Tutorial' project for more help.

namespace Fsvd
    module Program =
        [<EntryPoint>]
        let main argv = 
            let game = Client.createGame 
            printfn "%A" game
            0 // return an integer exit code
