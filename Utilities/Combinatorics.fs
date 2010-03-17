// Learn more about F# at http://fsharp.net

module Combinatorics
    let perms xs = 
        let largestReverseIndex (xs:_[]) = [xs.Length-2..-1..0] |> Seq.tryFind (fun i -> xs.[i] < xs.[i+1])
        let largestHigherIndex (xs:_[]) idx = [xs.Length-1..-1..idx+1] |> Seq.find (fun i -> xs.[idx] < xs.[i]) 
        let nextPerm xs = 
            match largestReverseIndex xs with
            | Some(j) -> 
                let l = largestHigherIndex xs j
                let tmp = Array.copy xs
                tmp.[j] <- xs.[l]
                tmp.[l] <- xs.[j]
                Some(Array.concat [tmp.[0..j]; Array.rev tmp.[j+1..]])
            | None    -> None

        let rec nextPerms xs = 
            seq { yield xs; match nextPerm xs with | Some(a) -> yield! nextPerms a | None -> yield! Seq.empty }
        xs |> Seq.toArray |> Array.sort |> nextPerms


