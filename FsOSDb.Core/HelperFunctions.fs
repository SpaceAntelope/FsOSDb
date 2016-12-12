namespace FsOSBd.Core

module HelperFunctions = 
    open System
    open System.IO
    open CookComputing.XmlRpc
    open System.Text.RegularExpressions
    open System.Collections
    open System.Security.Cryptography
    open System.Text

    let cprintfn c fmt = 
        Printf.kprintf
            (fun s ->
                let old = System.Console.ForegroundColor
                try
                  System.Console.ForegroundColor <- c;
                  System.Console.WriteLine s
                finally
                  System.Console.ForegroundColor <- old)
            fmt
    
    let cprintf c fmt = 
        Printf.kprintf
            (fun s ->
                let old = System.Console.ForegroundColor
                try
                  System.Console.ForegroundColor <- c;
                  System.Console.Write s
                finally
                  System.Console.ForegroundColor <- old)
            fmt

    let printfError fmt = cprintf ConsoleColor.Red fmt
    let printfnInfo fmt = 
        match ConfigurationManager.IsSilent with
        | false -> cprintfn ConsoleColor.Cyan fmt         
        | true ->  Printf.kprintf ignore fmt
    let printfInfo fmt = 
        match ConfigurationManager.IsSilent with
        | false -> cprintf ConsoleColor.Cyan fmt         
        | true ->  Printf.kprintf ignore fmt

    let printfResult fmt = cprintf ConsoleColor.Green fmt
    let printfnResult fmt = cprintfn ConsoleColor.Green fmt
    let printfnError fmt = cprintfn ConsoleColor.Red fmt
    let printfWarning fmt = cprintf ConsoleColor.Yellow fmt
    let printfnWarning fmt = cprintfn ConsoleColor.Yellow fmt

    let printLabeled (labels : string []) (values : string []) = 
                let labelWidth = 
                    labels
                    |> Array.map (fun str -> str.Length)
                    |> Array.max
                Array.zip labels values |> Array.iter (fun (label, value) -> 
                                               printfResult "%s: " (label.PadLeft(labelWidth))
                                               printfnInfo "%s" value)

    let printLabeled' (source:(string*string) array) =
        let labelWidth = source |> Array.map(fun (label,_) -> label.Length) |> Array.max
        source |> Array.iter(fun (label, value) ->  printfResult "%s: " (label.PadLeft(labelWidth))
                                                    printfnInfo "%s" value)

    let printPropertyTable (selectProps : 'a -> string []) (headers : string []) (source : 'a []) = 
        printfn ""
        let rows = source |> Array.map selectProps
        
        let widths = 
            rows
            |> Array.append [| headers |]
            |> Array.map (Array.map (fun i -> i.Length))
            |> Array.reduce (fun state item -> headers |> Array.mapi (fun i _ -> Math.Max(state.[i], item.[i])))
        
        let printRow rowIndex (source : string []) = 
            if rowIndex > 0 then 
                printfResult "%3d. " rowIndex
                source |> Array.iteri (fun i prop -> printfInfo "%*s\t" widths.[i] prop)
            else source |> Array.iteri (fun i prop -> printfResult "%*s\t" widths.[i] prop)
            printfn ""
        
        printf "%5s" ""
        printRow 0 headers
        rows |> Array.iteri (fun i item -> printRow (i+1) item)
        printfn ""

    let IsNullOrEmpty = String.IsNullOrEmpty
    let (|IsNullOrEmpty|) str = String.IsNullOrEmpty str
    let ToCamelCase (str:string) = 
        if System.String.IsNullOrEmpty str then ""
        else
            str.Split(' ') 
            |> Array.map (fun sub -> sub.[0].ToString().ToUpper() + sub.Substring(1)+ " ") 
            |> Array.reduce(+)

    let ToLower (str:string) = str.ToLower()
    let Replace pattern (replacement:string) (str:string) = str.Replace(pattern, replacement)
    let Split (c:char) (str:string) = str.Split(c)
    let Trim (str:string) = str.Trim()
    let Join separator (str:string seq) = Seq.reduce(fun state item -> sprintf "%s%s%s" state separator item) str


    let ToArray item = [|item|]  
    let IsEnumerable arr =
        arr.GetType().GetInterfaces()
        |> Array.tryFind(fun iface-> iface = typedefof<System.Collections.IEnumerable>)
        |> Option.isSome


    let (|IsMatch|) pat str = Regex.IsMatch(str, pat)
    let RReplace pat (replace : string) input = Regex.Replace(input, pat, replace)
    let RSplit pat str = Regex.Split(str,pat)    
    let RMatch pat input = 
        match (Regex.Match(input, pat)) with
        | m when m.Success -> Some m.Value
        | m when m.Success |> not -> None

    let RMatchGroup pat (group:string[]) input = 
        match (Regex.Match(input, pat)) with
        | m when m.Success && group |> Array.isEmpty |> not -> 
            group |> Array.map (fun groupName -> m.Groups.[groupName].Value) |> Some
        | m when m.Success -> [|m.Value|]|>Some
        | m when m.Success |> not -> None

    let RIsMatch pat str = System.Text.RegularExpressions.Regex.IsMatch(str, pat)

    let IsNumber str = 
            RMatch "\d+" str    
            |> Option.isNone
            |> not

    let RMatches pat input = 
        Regex.Matches(input, pat)
        |> Seq.cast<Match>
        |> Seq.map(fun m -> m.Value)
        |> Array.ofSeq

    let inline flip f x y = f y x
    let inline defOpt (defaultValue: 'a)  (input: 'a option) = 
        match input with
        | Some x -> x
        | None -> defaultValue
    let inline (<??>) (input: 'a option) (defaultValue: 'a) = defOpt defaultValue input
    let inline decode  (cases:('a*'a)[]) (input:'a) = 
            cases 
            |> Array.tryFind (fst>>(=) input) 
            |> function 
            | Some (_,value) -> value 
            | None -> input

    let CombinePaths leaf branch = System.IO.Path.Combine(branch,leaf)
    let (|IsFile|IsDirectory|IsMissing|) (path:string) =
        if (File.Exists(path)|>not) && (Directory.Exists(path)|> not) then IsMissing
        else if File.GetAttributes path &&& FileAttributes.Directory = FileAttributes.Directory
        then IsDirectory
        else IsFile

    let GetDirFromPath path =
            match path with 
            | IsFile -> Path.GetDirectoryName path
            | IsDirectory -> path
            | IsMissing -> ""


    
    let agent = 
        MailboxProcessor.Start(fun inbox -> 
            async { 
                let list = ArrayList()
                while true do
                    let! msg = inbox.Receive()
                    if msg = "FLUSH!" then
                        [0..(list.Count-1)]
                        |> List.map ( fun i -> string list.[i])
                        |> List.iter (printfn "> ‘%s'") 
                        list.Clear()
                    else list.Add msg |> ignore
            })

    
    let tee title something = 
        printfnInfo "%s:\n%A" title something
        something
    
    let inline teeAgent input = 
        let result = sprintf "%A" input
        agent.Post result
        result

   
    
    let md5 (data : byte array) : string =
        use md5 = MD5.Create()
        (StringBuilder(), md5.ComputeHash(data))
        ||> Array.fold (fun sb b -> sb.Append(b.ToString("x2")))
        |> string

    let checksumMd5 path =
        use stream = File.OpenRead(path)
        use md5 = MD5.Create()
        md5.ComputeHash(stream)
        |> BitConverter.ToString
        |> Replace "-" ""
        |> ToLower

    let UnGZipFromBase64ToFile targetPath (source:string) = 
        let bytes = System.Convert.FromBase64String source
        use ms = new MemoryStream(bytes)
        use fs = File.Create(targetPath)
        use unzip = new Compression.GZipStream(ms, Compression.CompressionMode.Decompress)
        unzip.CopyTo(fs)
        fs.Flush()
        FileInfo(targetPath)

    let UnGZipFromBase64 (source : string)= 
        let bytes = System.Convert.FromBase64String source
        use compressedMs = new MemoryStream(bytes)
        use unzip = new Compression.GZipStream(compressedMs, Compression.CompressionMode.Decompress)
        use decompressedMs = new MemoryStream()
        unzip.CopyTo(decompressedMs)
        unzip.Close()
        compressedMs.Close()
        decompressedMs.ToArray()

    let GZipAndEncodeBase64 (path:string) =         
        use ms = new MemoryStream()
        use zip = new Compression.GZipStream(ms, Compression.CompressionMode.Compress)
        use fs = File.OpenRead(path)
        fs.CopyTo(zip)
        fs.Close()
        zip.Close()
        ms.ToArray()
        |> Convert.ToBase64String

    let WriteAllLines path source = File.WriteAllLines(path,source)
    let WriteBytes path source =
        File.WriteAllBytes(path, source)

    let ComputeMovieHash path = 
        use fs = File.OpenRead(path)
        let sourceLength = 65536L
        let streamLength = fs.Length
        let bufferLength = int64 sizeof<System.Int64>
        let buffer = Array.init (int bufferLength) (fun i -> 0uy)
        
        let rec calcHash index lhash = 
            let bytesRead = fs.Read(buffer, 0, int bufferLength)
            if index >= sourceLength / bufferLength || bytesRead <= 0 then 
                fs.Position <- int64 (System.Math.Max(0L, streamLength - sourceLength))
                lhash
            else calcHash (index + 1L) (lhash + System.BitConverter.ToInt64(buffer, 0))
        
        let lhash = calcHash 0L streamLength |> calcHash 0L
        fs.Close()
        System.BitConverter.GetBytes(lhash)
        |> Array.rev
        |> Array.map (fun b -> b.ToString("x2"))
        |> Array.reduce (+)

    let stringOfStruct (source : XmlRpcStruct) =
        let rec printStruct indent (source: obj) =    
            seq [
                match source with
                | :? (XmlRpcStruct array) -> 
                            yield! Seq.collect (fun str -> (printStruct (indent + 1) str)) (source :?> XmlRpcStruct array)                                        
                | :? XmlRpcStruct ->             
                            let source' = source :?> XmlRpcStruct
                            yield! source'.Keys 
                            |> Seq.cast<string>
                            |> Seq.collect (fun key -> 
                                                seq [ yield sprintf "%s: " (key.PadLeft(indent + key.Length,'\t'))
                                                      yield! printStruct (indent+1) source'.[key] ])
                            yield ""                      
                | _ ->  let str = string source
                        yield str.PadLeft(indent+str.Length, '\t')  
            ] 

        printStruct 0 source
        |> Seq.reduce (fun state item -> if (state.EndsWith(": ")) then (state + item) else (state + "\n" + item)) 

    let GetPaths path (filters:string[]) searchRecursively =
        match path with
        | IsFile -> [|path|]
        | IsDirectory ->
            match searchRecursively with
            | false -> Directory.EnumerateFiles(path)
            | true -> Directory.EnumerateFiles(path, "*", SearchOption.AllDirectories)
            |> Seq.where(fun fileName -> filters |> Array.exists fileName.EndsWith)
            |> Array.ofSeq
    
    let wagnerFischerLazy (s: string) (t: string) =
        let inline min3 one two three = 
            if one < two && one < three then one
            elif two < three then two
            else three

        let m = s.Length
        let n = t.Length
        let d = Array2D.create (m+1) (n+1) -1
        let rec dist =
            function
            | i, 0 -> i
            | 0, j -> j
            | i, j when d.[i,j] <> -1 -> d.[i,j]
            | i, j ->
                let dval = 
                    if s.[i-1] = t.[j-1] then dist (i-1, j-1)
                    else
                        min3
                            (dist (i-1, j)   + 1) // a deletion
                            (dist (i,   j-1) + 1) // an insertion
                            (dist (i-1, j-1) + 1) // a substitution
                d.[i, j] <- dval; dval 
        dist (m, n)

  

  

    