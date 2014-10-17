// Learn more about F# at http://fsharp.net
// See the 'F# Tutorial' project for more help.

namespace Fsvd
    module Program =
        [<Literal>]
        let defaultApiKey = "05erfbm8"

        type Opts = {
            live: bool
            turns: string
            key: string
            infinite: bool
        }

        let defaultOpts = { live = false; turns = "300"; key = defaultApiKey; infinite = false }

        let parseCommandLine argv = 
            let rec parseCommandLineInner args acc =
                match args with
                | [] -> acc
                | "-live"::xs | "/live"::xs | "-l"::xs | "/l"::xs -> parseCommandLineInner xs { acc with live = true }
                | "-turns"::y::xs | "/turns"::y::xs | "-t"::y::xs | "/t"::y::xs -> parseCommandLineInner xs { acc with turns = y } 
                | "-key"::y::xs | "/key"::y::xs | "-k"::y::xs | "/k"::y::xs -> parseCommandLineInner xs { acc with key = y } 
                | "-infinite"::xs | "/infinite"::xs | "-i"::xs | "/i"::xs -> parseCommandLineInner xs { acc with infinite = true } 
                | _::xs -> parseCommandLineInner xs acc // just ignore unknown opts.
            parseCommandLineInner (Array.toList argv) defaultOpts

        [<EntryPoint>]
        let main argv = 
            let opts = parseCommandLine argv
            let client = new Client.Client(opts.key, opts.live, opts.turns)
            let mutable loop = true
            while loop do
                loop <- opts.infinite 
                Bot.botloop client
            0 // return an integer exit code
