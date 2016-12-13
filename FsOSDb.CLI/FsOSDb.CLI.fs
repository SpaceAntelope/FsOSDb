namespace FsOSBd.CLI

module Program = 
    open Argu
    open CLI
    open FsOSBd.Core
    open FsOSBd.Core.Story
    open FsOSBd.Core.HelperFunctions
    open FsOSBd.Core.RR
    open System.Reflection    
    open ChapterFromCLIArgs

//    let PrintSettings() = 
//        ConfigurationManager.AppSettings.AllKeys |> Array.iter (fun key -> printfn "%s = %A" key ConfigurationManager.AppSettings.[key])
//        ConfigurationManager.AppSettings |> printfn "%A"
    
    let ParseArguements args = 
        let parser = ArgumentParser.Create<CLIArguments>(programName = "FsOSBd.CLI.exe")
        let results = parser.Parse args
        if results.Contains <@ CLIArguments.Version @> then 
            AssemblyName.GetAssemblyName("FsOSBd.exe").Version |> printfnInfo "FsOSBd.CLI %A"
        
        FsOSBd.Core.ConfigurationManager.IsSilent <- results.Contains <@ Silent @>
               
        if results.Contains <@ Log @> then FsOSBd.Core.OSXmlRpc.StartLogging()

        results
    
    let ExecuteSubCommmand(results : ParseResults<CLIArguments>) = 
        match results.GetSubCommand() with
        | CLIArguments.Upload args -> 
            ChapterFromCLIArgs.Upload args
            |> Story<_>.CreateFromChapter
            |> UploadStory.OnceUponATime
            |> Composition.Upload
        | CLIArguments.Convert(args) -> 
            ChapterFromCLIArgs.Convert args
            |> Story<_>.CreateFromChapter
            |> ConvertStory.OnceUponATime
            |> Composition.Convert
        | CLIArguments.Download(args) -> 
            ChapterFromCLIArgs.Download args
            |> Story<_>.CreateFromChapter
            |> DownloadStory.OnceUponATime
            |> Composition.Download
        | CLIArguments.ReportHash(args) -> 
            ChapterFromCLIArgs.Report args
            |> Story<_>.CreateFromChapter
            |> ReportStory.OnceUponATime
            |> Composition.ReportHash
        | CLIArguments.Credentials(args) -> 
            ChapterFromCLIArgs.Credentials args
            |> Story<_>.CreateFromChapter
            |> CredentialsStory.OnceUponATime
            |> Composition.Credentials
    
    let ComposeResultAndShowOutput result = 
        match result with
        | Success story -> 
            match story with
            | Composition.ChapterSuccess.Convert s -> s.Print()
            | Composition.ChapterSuccess.Upload s -> s.Print()
            | Composition.ChapterSuccess.ReportHash s -> s.Print()
            | Composition.ChapterSuccess.Download s -> s.Print()
            //| Composition.ChapterSuccess.Configure s -> s.Print()
            | Composition.ChapterSuccess.Credentials s -> s.Print()
        | Failed s -> printfError "%A" s
    
    [<EntryPoint>]
    let main argv = 
        try 
            try 
                argv
                |> ParseArguements 
                |> ExecuteSubCommmand
                |> ComposeResultAndShowOutput
            with ex -> 
                match ex with
                | :? Argu.ArguParseException as ex -> 
                    ex.Message
                    |> Split '\n'
                    |> fun lines -> 
                        printfnError "%s" lines.[0]
                        lines
                        |> Array.skip 1
                        |> Array.iter (printfnInfo "%s")
                | _ -> printfError "%A" ex
        finally
            if ConfigurationManager.Token
               |> IsNullOrEmpty
               |> not
            then 
                ConfigurationManager.Token
                |> OSDbApi.Logout
                |> ignore
        0
