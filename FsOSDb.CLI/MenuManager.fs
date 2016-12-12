namespace FsOSBd.CLI

module MenuManager = 
    open FsOSBd.Core.HelperFunctions
    open FsOSBd.Core.ValidationManager
    open System
    open FsOSBd.Core
    open FsOSBd.Core.RR
    open FsOSBd.Core.Story
    open FsOSBd.Core.Results
    
    let SelectFromMenu selectionString (printMenu : unit -> int) = 
        let spaceSeparatedNumbers = @"^(\d+\s*)+$"
        
        let rec selectFromMenu selection = 
            match selection with
            | IsMatch spaceSeparatedNumbers true -> 
                selection
                |> RMatches "\\d+"
                |> Array.map int
                |> Success
            | IsNullOrEmpty true -> Common.Finished |> Failed
            | _ -> 
                if printMenu() > 1 then 
                    printfn "Enter a space separated list of the numbers of the subtitles you wish to download or 0 for all, or nothing to exit."
                    selectFromMenu (Console.ReadLine())
                else selectFromMenu "1"
        selectFromMenu selectionString
    
    let FilterFromBase1Index (selectedIndices : int []) source = 
        if selectedIndices.[0] = 0 then source
        else 
            source
            |> Array.mapi (fun i info -> (i + 1), info)
            |> Array.filter (fun (i, _) -> Array.contains i selectedIndices)
            |> Array.map snd
    
    let ShowSubInfoSelectionMenu (story : Story<ChapterDownload>) = 
        let printMenu = 
            (fun () -> 
            story.SubtitleInfo |> SubtitleInfoResultItem.PrintColumns
            story.SubtitleInfo.Length)
        match SelectFromMenu "X" printMenu with
        | Success selection -> { story with SubtitleInfo = story.SubtitleInfo |> FilterFromBase1Index selection } |> Success
        | Failed _ -> Failed(DownloadValidate.Skip |> box)
    
    let ShowBasicMovieSelectionMenu (story : Story<ChapterDownload>) = 
        let printMenu = 
            (fun () -> 
            story.MovieSuggestions |> BasicMovieInfo.PrintColumns
            story.MovieSuggestions.Length)
        match SelectFromMenu "X" printMenu with
        | Success selection -> { story with MovieSuggestions = story.MovieSuggestions |> FilterFromBase1Index selection } |> Success
        | Failed _ -> Failed(box DownloadValidate.Skip)
