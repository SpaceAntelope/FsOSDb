namespace FsOSBd.Core

module DownloadValidate = 
    open ValidationManager
    open Story
    open HelperFunctions
    open System.IO
    open RR
    
    type ErrorCase = 
        | OutputPathError of Path.ErrorCase
        | MoviePathMissing of Path.ErrorCase
        | InvalidCredentials of Common.ErrorCase
        | LogingFailed of Common.ErrorCase
        | SubtitleNotFoundForHash of string
        | NoMoviesSuggested of string
        | MovieFilesNotFoundForPathAndFilter of path : string * filters : string []
        | SubtitleInfoNotFoundForImdbId of ids : string [] * filters : string []
        | NoNewSubfilesFound of string []
        | SubtitleDataNotFoundForSubtitleId of string []
        | Skip
    
    let AssertNumericIdIsValid(numId : string) = 
        RMatch "\d+" numId
        |> Option.isNone
        |> not
    
    let AssertMoviePathIsValid(story : Story<ChapterDownload>) = 
        match story.Chapter.MoviePath with
        | None -> Skip |> Failed
        | Some path -> 
            match path with
            | IsFile | IsDirectory -> Success story
            | IsMissing -> 
                path
                |> Path.FileNotFound
                |> MoviePathMissing
                |> Failed
    
    //            match path with
    //            | IsFile -> 
    //                path
    //                |> Path.AssertFileExists
    //                |> function 
    //            | Success _ -> Success story
    //            | Failed error -> MovieFileMissing error |> Failed
    let ConsolidateMovieFiles(story : Story<ChapterDownload>) = 
        match story.Chapter.MoviePath with
        | Some path -> 
            match path with
            | IsFile -> path |> ToArray
            | IsDirectory -> 
                Directory.EnumerateFiles(path)
                |> Seq.filter (fun fileName -> Array.contains (Path.GetExtension(fileName) |> ToLower) story.Chapter.MediaExtensionsFilter)
                |> Array.ofSeq
            | _ -> [||]
        | None -> [||]
        |> fun paths -> { story with Chapter = { story.Chapter with MediaFilePaths = paths } }
        |> Success
    
    let AssertMovieFilesFound(story : Story<ChapterDownload>) = 
        match story.Chapter.MediaFilePaths with
        | [||] -> 
            (story.Chapter.MoviePath <??> "", story.Chapter.MediaExtensionsFilter)
            |> MovieFilesNotFoundForPathAndFilter
            |> Failed
        | _ -> Success story
    
    let AssertOutputPathIsValid(story : Story<ChapterDownload>) = 
        story.Chapter.OutputPath
        |> Path.AssertDirectoryExists
        |> function 
        | Success _ -> Success story
        | Failed error -> OutputPathError error |> Failed
    
    let AssertSubtitleInfoForHashExists(story : Story<ChapterDownload>) = 
        match story.SubtitleInfo with
        | [||] -> 
            story.Chapter.MoviePath <??> ""
            |> SubtitleNotFoundForHash
            |> Failed
        | _ -> Success story
    
    let AssertDownloadOrStopWithFunction storyFunction (story : Story<ChapterDownload>) = 
        match story.Chapter.SearchOnly with
        | true -> 
            storyFunction story |> ignore
            Failed(Skip)
        | false -> Success story
    
    let AssertSuggestedMoviesFound(story : Story<ChapterDownload>) = 
        match story.MovieSuggestions with
        | [||] -> 
            story.Chapter.Name <??> ""
            |> NoMoviesSuggested
            |> Failed
        | _ -> Success story
    
    let AssertSubtitleInfoForImdbExists(story : Story<ChapterDownload>) = 
        match story.SubtitleInfo with
        | [||] -> 
            let filters = 
                [| story.Chapter.LanguageId <??> ""
                   story.Chapter.Fps <??> -1.0
                   |> string
                   |> decode [| "-1", "" |]
                   
                   story.Chapter.Series <??> (-1, -1) 
                   |> fun (s, e) -> sprintf "season: %d episode: %d" s e |> decode [| "season: -1 episode: -1", "" |] |]
                |> Array.where ((<>) "")
            Failed(SubtitleInfoNotFoundForImdbId(story.Chapter.ImdbId, filters))
        | _ -> Success story
    
    let AssertSubContentExists(story : Story<ChapterDownload>) = 
        match story.SubtitleData with
        | [||] -> Failed(SubtitleDataNotFoundForSubtitleId story.Chapter.SubFileId)
        | _ -> Success story
