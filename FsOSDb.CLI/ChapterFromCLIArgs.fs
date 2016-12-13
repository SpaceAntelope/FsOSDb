namespace FsOSBd.CLI

module ChapterFromCLIArgs = 
    open Argu
    open FsOSBd.Core.Results
    open FsOSBd.Core.Story
    open CLI
    open FsOSBd.Core.HelperFunctions
    open FsOSBd.Core.RR
    open FsOSBd.Core
    open System
    open System.Configuration
    open FsOSBd.Core.ConfigurationManager
    
    let Convert(results : ParseResults<ConvertArgs>) = 
        { SourceFile = results.GetResult(<@ SourceFile @>)
          Fps = 
              if results.Contains <@ ConvertArgs.Fps @> then results.GetResult(<@ ConvertArgs.Fps @>) |> Some
              else None
          OffsetMs = results.GetResult(<@ OffsetMs @>, defaultValue = 0L)
          SubFileContent = [||]
          OnlyAfter = results.GetResult(<@ OnlyAfter @>, defaultValue = (0, 0, 0)) |> fun (h, m, s) -> TimeSpan(h, m, s)
          Force = results.Contains(<@ ConvertArgs.Force @>) }
    
    let Upload(results : ParseResults<UploadArgs>) = 
        { MovieFile = results.GetResult(<@ UploadArgs.MovieFile @>)
          SubtitleFile = results.GetResult(<@ SubtitleFile @>)
          Fps = results.GetResult(<@ UploadArgs.Fps @>)
          LanguageId = results.GetResult(<@ UploadArgs.LanguageId @>)
          ImdbId = results.GetResult(<@ UploadArgs.ImdbId @>) }
    
    let Report(results : ParseResults<ReportHashArgs>) = { IDSubMovieFile = results.GetResult(<@ ReportHashArgs.SubFileId @>) }
    
    let Download(results : ParseResults<DownloadArgs>) = 
        let movieFile = results.GetResult(<@ DownloadArgs.MoviePath @>, None)
        let outputPath = results.GetResult(<@ DownloadArgs.OutputPath @>, None)
        
        { MoviePath = movieFile
          OutputPath = 
              match movieFile, outputPath with
              | Some path, None -> path |> GetDirFromPath
              | _, path -> path <??> "."
          Name = results.GetResult(<@ DownloadArgs.Name @>, None)
          Series = GetOptionalResult (<@ DownloadArgs.Series @>) results
          Year = results.GetResult(<@ DownloadArgs.Year @>, None)
          LanguageId = results.GetResult(<@ DownloadArgs.LanguageId @>, None)
          Fps = results.GetResult(<@ DownloadArgs.Fps @>, None)
          ImdbId = 
              results.GetResult(<@ DownloadArgs.ImdbId @>, defaultValue = [])
              |> Array.ofList
              |> Array.collect (RMatches "\d+")
          SubFileId = 
              results.GetResult(<@ DownloadArgs.SubFileId @>, defaultValue = [])
              |> Array.ofList
              |> Array.collect (RMatches "\d+")
          MediaFilePaths = [||]
          MediaExtensionsFilter = 
              results.GetResult(<@ DownloadArgs.MediaExtensions @>, defaultValue = [ ".mkv"; ".mp4" ])
              |> List.map (fun ext -> 
                     if ext.StartsWith(".") then ext
                     else "." + ext
                     |> ToLower)
              |> Array.ofList
          Force = results.Contains(<@ DownloadArgs.Force @>)
          SearchOnly = results.Contains(<@ DownloadArgs.SearchOnly @>)
          FilterSubtitleInfo = Some MenuManager.ShowSubInfoSelectionMenu
          FilterMovieSuggestions = Some MenuManager.ShowBasicMovieSelectionMenu }
    
    let Credentials(results : ParseResults<CredentialAgrs>) = 
        let (username, password) = results.GetResult(<@ CredentialAgrs.LogInfo @>, ("", ""))
        { UserName = username
          Password = password
          Language = results.GetResult(<@ CredentialAgrs.Language @>, None) <??> ""
          UserAgent = results.GetResult(<@ CredentialAgrs.UserAgent @>) }
