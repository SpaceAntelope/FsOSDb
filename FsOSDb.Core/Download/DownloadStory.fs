namespace FsOSBd.Core

module DownloadStory = 
    open Story
    open HelperFunctions
    open Results
    open Requests
    open OSDbApi
    open System.IO
    open RR
    
    (* TODO: Implement --SearchOnly *)
    let ShowSuggestedMovies(story : Story<ChapterDownload>) = 
        story.MovieSuggestions |> BasicMovieInfo.PrintColumns
        Success story
    
    let ShowBasicMovieSelectionMenu(story : Story<ChapterDownload>) = 
        match story.Chapter.FilterMovieSuggestions with
        | None -> Success story
        | Some f -> 
            match f story with
            | Success story -> Success story
            | Failed _ -> Failed DownloadValidate.Skip
    
    let ShowSubInfoSelectionMenu(story : Story<ChapterDownload>) = 
        match story.Chapter.FilterSubtitleInfo with
        | None -> Success story
        | Some f -> 
            match f story with
            | Success story -> Success story
            | Failed _ -> Failed DownloadValidate.Skip
    
    let SuggestMovie(story : Story<ChapterDownload>) = 
        match story.Chapter.Name with
        | None -> 
            printfnInfo "Name suggestion not found"
            Success story
        | Some name -> 
            printfnInfo "Sending suggestion '%s'..." name
            OSDbApi.SuggestMovie story.Token name 
            |> fun { Result = _; Data = basicMovieInfo } -> { story with MovieSuggestions = basicMovieInfo } |> Success
    
    let FilterSuggestedMovies(story : Story<ChapterDownload>) = 
        match story.Chapter.Year with
        | None -> Success story
        | Some year -> { story with MovieSuggestions = story.MovieSuggestions |> Array.filter (fun info -> info.MovieYear = string year) } |> Success
    
    let FilterSubInfoByLanguageAndFps(story : Story<ChapterDownload>) = 
        let stringOfFps = 
            story.Chapter.Fps <??> 0.0
            |> sprintf "%.3f"
            |> Replace "," "."
        
        let langId = story.Chapter.LanguageId <??> ""
        let fpsFilter info = (stringOfFps = info.MovieFPS)
        let langFilter (info : SubtitleInfoResultItem) = (langId = info.SubLanguageID)
        let combined info = (fpsFilter info) && (langFilter info)
        match langId, stringOfFps with
        | lang, fps when fps <> "0.000" && lang <> "" -> combined |> Some
        | _, fps when fps <> "0.000" -> fpsFilter |> Some
        | lang, _ when lang <> "" -> langFilter |> Some
        | _, _ -> None
        |> function 
        | None -> story
        | Some filter -> { story with SubtitleInfo = story.SubtitleInfo |> Array.filter filter }
        |> Success
    
    let FilterSubInfoBySeasonAndEpisode(story : Story<ChapterDownload>) = 
        let patterns = 
            match story.Chapter.Series with
            | None -> []
            | Some(season, episode) -> 
                match (season, episode) with
                | s, e when s > -1 && e > -1 -> 
                    [ (sprintf "[sS]%02i[Ee]%02i" s e)
                      (sprintf "%02ix%02i" s e) ]
                | s, _ when s > -1 -> 
                    [ (sprintf "[sS]%02i[Ee]\d{2}" s)
                      (sprintf "%02ix\d{2}" s) ]
                | _, e when e > -1 -> 
                    [ (sprintf "[sS]\d{2}[Ee]%02i{2}" e)
                      (sprintf "\d{2}x%02i" e) ]
                | _ -> []
        
        let filter (source : SubtitleInfoResultItem) = 
            patterns
            |> List.isEmpty
            || patterns
               |> List.tryFind (fun pat -> 
                      match source.SubFileName with
                      | IsMatch pat true -> true
                      | _ -> false)
               |> Option.isSome
        
        match patterns with
        | [] -> story
        | _ -> { story with SubtitleInfo = story.SubtitleInfo |> Array.filter filter }
        |> Success
    
    let ConsolidateImdbIds(story : Story<ChapterDownload>) = 
        match story.MovieSuggestions with
        | [||] -> Success story
        | _ -> { story with Chapter = { story.Chapter with ImdbId = story.MovieSuggestions |> Array.map (fun info -> info.IDMovieImdb) } } |> Success
    
    let ConsolidateSubIds(story : Story<ChapterDownload>) = 
        match story.SubtitleInfo with
        | [||] -> Success story
        | _ -> 
            { story with Chapter = { story.Chapter with SubFileId = story.SubtitleInfo |> Array.map (fun info -> info.IDSubtitleFile) } } |> Success
    
    let SearchSubsByImdbId(story : Story<ChapterDownload>) = 
        //story.Chapter.ImdbId |> Array.iter (fun imdbId -> printfnInfo "Searching subs by ImdbId '%s' for %s" imdbId story.Chapter.MovieFile)
        story
        |> SearchSubtitleRequest.OfStory
        |> SearchSubtitles story.Token
        |> fun subinfo -> Success { story with SubtitleInfo = subinfo.Data }
    
    let SkipExistingSubFilesUnlessForced(story : Story<ChapterDownload>) = 
        story.SubtitleInfo
        |> Array.filter (fun info -> 
               story.Chapter.OutputPath
               |> Path.GetDirectoryName
               |> CombinePaths info.SubFileName
               |> File.Exists
               |> function 
               | true when story.Chapter.Force |> not -> 
                   printfnInfo "Skipping File '%s'. Use --force to overwrite." info.SubFileName
                   false
               | _ -> true)
        |> function 
        | [||] -> 
            story.SubtitleInfo
            |> Array.map (fun info -> info.SubFileName)
            |> DownloadValidate.NoNewSubfilesFound
            |> Failed
        | subTitleInfo -> Success { story with SubtitleInfo = subTitleInfo }
    
    let DownloadSubtitlesFromId(story : Story<ChapterDownload>) = 
        let index = 
            story.SubtitleInfo
            |> Array.map (fun info -> info.IDSubtitleFile, info.SubFileName)
            |> Map.ofArray
        story.Chapter.SubFileId
        |> Array.map (fun sid -> 
               let sid' = string sid
               index.TryFind(sid') <??> sid' |> printfnInfo "Downloading file %s"
               DownloadSubtitles story.Token [| sid' |])
        |> Array.collect (fun { Result = _; Data = subContents } -> subContents)
        |> fun subContents -> Success { story with SubtitleData = subContents }
    
    let WriteSubContent(story : Story<ChapterDownload>) = 
        let addIndexToFilename i fname = 
            let (path, fname, extension) = 
                (GetDirFromPath fname), (Path.GetFileNameWithoutExtension fname) + sprintf "(%d)" i, (Path.GetExtension fname)
            Path.Combine(path, fname + extension)
        
        let subInfoIndex = 
            story.SubtitleInfo
            |> Array.groupBy (fun info -> info.SubFileName)
            |> Array.map (fun (key, group) -> 
                   key, 
                   group |> Array.mapi (fun i info -> 
                                if i > 0 then { info with SubFileName = addIndexToFilename i (info.SubFileName) }
                                else info))
            |> Array.collect snd
            |> Array.map (fun info -> info.IDSubtitleFile, info)
            |> Map.ofArray
        
        story.SubtitleData |> Array.iter (fun data -> 
                                  let subFileName = 
                                      data.IdSubtitleFile
                                      |> subInfoIndex.TryFind
                                      |> function 
                                      | None -> data.IdSubtitleFile
                                      | Some info -> info.SubFileName
                                  
                                  let fullPath = Path.Combine(story.Chapter.OutputPath, subFileName)
                                  printfnInfo "Writing file '%s'" fullPath
                                  File.WriteAllBytes(fullPath, data.Data |> UnGZipFromBase64))
        story |> Success
    
    let TrySearchByHash(story : Story<ChapterDownload>) = 
        let callApi (story : Story<ChapterDownload>) = 
            printfnInfo "Searching subs by hash for:"
            story.Chapter.MediaFilePaths
            |> Array.map Path.GetFileName
            |> Array.iter (printfnWarning "\t> %s")
            story
            |> SearchSubtitleRequest.OfStory
            |> SearchSubtitles story.Token
            |> fun { Result = _; Data = data } -> Success { story with SubtitleInfo = data }
        match story.Chapter.MoviePath with
        | None -> 
            printfnInfo "No file available to produce hash."
            Success story
        | Some _ -> 
            DownloadValidate.AssertMoviePathIsValid story
            >>= DownloadValidate.ConsolidateMovieFiles
            >>= DownloadValidate.AssertMovieFilesFound
            >>= callApi
            >>= FilterSubInfoByLanguageAndFps
            >>= DownloadValidate.AssertSubtitleInfoForHashExists
            >>= ShowSubInfoSelectionMenu
            >>= SkipExistingSubFilesUnlessForced
            >>= ConsolidateSubIds
    
    let TrySearchByName(story : Story<ChapterDownload>) = 
        match story.Chapter.Name with
        | None -> 
            printfnInfo "No name suggestion available."
            Success story
        | Some name -> 
            printfnInfo "Sending suggestion '%s'..." name
            OSDbApi.SuggestMovie story.Token name
            |> fun { Result = _; Data = basicMovieInfo } -> { story with MovieSuggestions = basicMovieInfo } |> Success
            >>= FilterSuggestedMovies
            >>= DownloadValidate.AssertSuggestedMoviesFound
            >>= DownloadValidate.AssertDownloadOrStopWithFunction(ShowSuggestedMovies) // maybe unnecessary
            >>= ShowBasicMovieSelectionMenu
            >>= ConsolidateImdbIds
    
    let TrySearchByImdbIds(story : Story<ChapterDownload>) = 
        match story.Chapter.ImdbId with
        | [||] -> 
            printfnInfo "Νο Idmb ids available."
            Success story
        | _ -> 
            SearchSubsByImdbId story
            >>= FilterSubInfoByLanguageAndFps
            >>= FilterSubInfoBySeasonAndEpisode
            >>= DownloadValidate.AssertSubtitleInfoForImdbExists
            >>= ShowSubInfoSelectionMenu
            >>= SkipExistingSubFilesUnlessForced
            >>= ConsolidateSubIds
    
    let TryDownloadBySubIds(story : Story<ChapterDownload>) = 
        match story.Chapter.SubFileId with
        | [||] -> 
            printfnInfo "Νο subtitle file ids available."
            Success story
        | _ -> 
            DownloadSubtitlesFromId story
            >>= DownloadValidate.AssertSubContentExists
            >>= WriteSubContent
    
    let OnceUponATime(story : Story<ChapterDownload>) = 
        ValidationManager.Common.AssertCredentials DownloadValidate.InvalidCredentials story
        >>= ValidationManager.Common.AssertLogin DownloadValidate.LogingFailed
        >>= DownloadValidate.AssertOutputPathIsValid
        >>= TrySearchByHash
        >>= TrySearchByName
        >>= TrySearchByImdbIds
        >>= TryDownloadBySubIds
