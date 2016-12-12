namespace FsOSBd.Core

module Story = 
    open System
    open Results
    open HelperFunctions
    open System.Configuration
    open RR
    
    type Story<'T> = 
        { Chapter : 'T
          Token : string
          SubtitleInfo : SubtitleInfoResultItem []
          SubtitleData : SubContents []
          MovieSuggestions : BasicMovieInfo [] }
        
        static member CreateFromChapter chapter = 
            { Chapter = chapter
              Token = ""
              SubtitleInfo = [||]
              SubtitleData = [||]
              MovieSuggestions = [||] }
        
        member story.Print() = 
            story.Chapter.GetType() /> fun t -> printfnWarning "\n\t\t\t-- Session summary (%s) --" (t.Name)
            |> Reflection.FSharpType.GetRecordFields
            |> Array.map (fun prop -> prop.Name, sprintf "%A" (prop.GetValue(story.Chapter)))
            |> printLabeled'
    
    type ChapterConvert = 
        { SourceFile : string
          Fps : (float * float) option
          OffsetMs : int64
          CummulativeMin : float
          OnlyAfter : TimeSpan
          SubFileContent : string []
          Force : bool }
    
    type ChapterUpload = 
        { MovieFile : string
          SubtitleFile : string
          Fps : float
          LanguageId : string
          ImdbId : int64 }
    
    type ChapterReportHash = 
        { IDSubMovieFile : int64 }
    
    //    type ChapterConfigure = 
    //        { FileName : string
    //          Command : CRUD }
    //        static member CreateFromArgs(results : ParseResults<ConfigureArgs>) = 
    //            { FileName = results.GetResult(<@ ConfigureArgs.FileName @>)
    //              Command = results.GetResult(<@ ConfigureArgs.Command @>) }
    type ChapterCredentials = 
        { UserName : string
          Password : string
          Language : string
          UserAgent : string }
        static member CreateFromSettings = 
            { UserName = ConfigurationManager.AppSettings.["OSDbUserName"]
              Password = ConfigurationManager.AppSettings.["OSDbPassword"]
              Language = ConfigurationManager.AppSettings.["OSDbUserAgent"]
              UserAgent = ConfigurationManager.AppSettings.["OSDbLanguage"] }
    
    type ChapterDownload = 
        { MoviePath : string option
          OutputPath : string
          Name : string option
          Series : (int * int) option
          Year : int option
          LanguageId : string option
          Fps : float option
          ImdbId : string []
          MediaExtensionsFilter : string []
          MediaFilePaths : string []
          SubFileId : string []
          Force : Boolean
          SearchOnly : Boolean 
          FilterSubtitleInfo : (Story<ChapterDownload> -> Result<Story<ChapterDownload>,obj>) option
          FilterMovieSuggestions : (Story<ChapterDownload> -> Result<Story<ChapterDownload>,obj>) option
          }
//        override story.ToString() = 
//            [| ""
//               //sprintf "            Token: %s" story.Token
//               sprintf "             Path: %s" story.FileData.Info.FullName
//               sprintf "            Query: %s" story.ImdbQuery
//               sprintf "           LangID: %s" story.LanguageId
//               sprintf "              FPS: %A" story.Fps
//               sprintf "   Search Results: %d" story.SubtitleInfo.Length
//               sprintf "    Subtitle data: %d" story.SubtitleData.Length
//               sprintf "Movie Suggestions: %d" story.MovieSuggestions.Length
//               "" |]
//            |> HelperFunctions.Join System.Environment.NewLine
//        
//        member story.ImdbQuery = 
//            seq [ yield story.Name |> toCamelCase
//                  if story.Year > 0 then yield sprintf "(%d)" story.Year
//                  if story.Episode > -1 then yield "(TV Series)"
//                  if story.Season > -1 then yield sprintf "s%02de%02d" story.Season story.Episode ]
//            |> Join " "
//            |> Trim
