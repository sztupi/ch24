type Key = 
    | Character of char
    | Shift
    | Enter
    | Space

let toChar key = 
    match key with
    | Some(Character(c)) -> c
    | Some(Shift) -> 'h'
    | Some(Enter) -> 'e'
    | Some(Space) -> 's'
    | None -> 'x'

let solve input = 
    let toDigit ch = if System.Char.IsDigit ch then Some(int ch - 48) else None

    let parse (lines:string []) = 
        let layoutLines = System.Int32.Parse(lines.[0])

        let parseLayout layoutLines = 
            let parseLine line = line |> Seq.map toDigit |> Seq.toList
            layoutLines |> Seq.map parseLine |> Seq.toList

        let parseText textLines : Key seq = 
            let parseChar ch = 
                match ch with
                | c when System.Char.IsLower c -> [Character(System.Char.ToUpper(c))]
                | c when System.Char.IsUpper c -> [Shift; Character(c)]
                | ' ' -> [Space]
                | c -> [Character(c)]
            let parseLine textLine = [Enter] |> Seq.append (textLine |> Seq.collect parseChar)
            textLines |> Seq.collect parseLine
            
        (parseLayout lines.[1..layoutLines], parseText lines.[layoutLines+1..])

    let weightsKeysCounts weightLayout text = 
        let charTypeNumber = text |> Seq.distinct |> Seq.length
        let significantWeights = weightLayout |> List.concat |> List.choose id |> List.sort |> Seq.take charTypeNumber
        let (keys, counts) = text |> Seq.countBy id |> Seq.sortBy snd |> Seq.toList |> List.rev |> List.unzip
        Seq.zip3 significantWeights keys counts

    let createLayout weightLayout keysByWeight =
        let rec transform input output unusedKeys = 
            match input with
            | Some(weight) :: tail  -> match Map.tryFind weight unusedKeys with
                                       | Some(key :: keysTail) -> transform tail (Some(key)::output) (unusedKeys |> Map.remove weight |> Map.add weight keysTail)
                                       | Some([]) | None   -> transform tail (None::output) unusedKeys
            | None :: tail          -> transform tail (None::output) unusedKeys
            | []                    -> output |> List.rev, unusedKeys

        weightLayout 
        |> List.fold (fun (output, unusedKeys) input -> let o, uuk = transform input List.empty unusedKeys in (o::output, uuk)) (List.empty, keysByWeight) 
        |> fst |> List.rev 

    let toString layout = 
        System.String.Join(System.Environment.NewLine, 
            (layout |> List.length |> sprintf "%d") :: (layout |> List.map (fun l -> new System.String(l |> List.map toChar |> List.toArray)) |> List.map (sprintf "%s")))

    let weightLayout, text = input |> parse
    let wkc = weightsKeysCounts weightLayout text
    let fullCost = wkc |> Seq.map (fun (w,_,c) -> w*c) |> Seq.reduce (+)
    let keysByWeight = wkc |> Seq.map (fun (w,k,_) -> w,k) |> Seq.groupBy fst |> Map.ofSeq |> Map.map (fun _ kvps -> kvps |> Seq.map snd |> Seq.toList)
    fullCost, toString (createLayout weightLayout keysByWeight)

open System.IO
['0'..'9'] 
|> List.iter (fun n -> 
    let inputFile = sprintf @"..\..\..\problem_set\2006\inputs\A-%s.in" (n.ToString())
    let answerFile = sprintf @"..\..\..\problem_set\2006\outputs\A-%s.ans" (n.ToString())
    let cost, layout = File.ReadAllLines inputFile |> solve
    printfn "--- solution %s ---" (n.ToString())
    printfn "%d" cost
    printfn "%s" layout
    printfn "--- answer %s ---" (n.ToString())
    printfn "%s" (File.ReadAllText answerFile))