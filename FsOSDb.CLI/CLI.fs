namespace FsOSBd.CLI

module CLI = 
    open Argu
    
    type CRUD = 
        | Create
        | Read
        | Update
        | Delete
    
    let GetOptionalResult (expr : Quotations.Expr<'Fields -> 'Template>) (results : ParseResults<'Template>) = 
        if results.Contains(expr) then results.GetResult(expr) |> Some
        else None
    
    type ConvertArgs = 
        | [<Unique; MainCommand; AltCommandLine("-s")>] SourceFile of string
        | [<Unique>] Fps of from : float * ``to`` : float
        | [<Unique; AltCommandLine("-off")>] OffsetMs of int64
        //| [<Unique; AltCommandLine("-cmm")>] CumulativeMin of float
        | [<Unique; AltCommandLine("-after")>] OnlyAfter of h : int * min : int * sec : int
        | [<Unique; AltCommandLine("-f")>] Force
        interface IArgParserTemplate with
            member this.Usage = 
                match this with
                | SourceFile _ -> "Subtitle file to be converted. .srt or .ass only"
                | Fps _ -> "FPS conversion: sourceFps targetFps"
                | OffsetMs _ -> "Milliseconds to add or remove from result"
                | OnlyAfter _ -> "Apply conversion only after this timestamp."
                //| CumulativeMin _ ->  "Increase offset from --offsetms every x minutes. Useful if the original subtitle skips regularly for the network logo."
                | Force -> "Overwrite existing converted files."
    
    and UploadArgs = 
        | [<Unique; AltCommandLine("-mov")>] MovieFile of string
        | [<Unique; AltCommandLine("-sub")>] SubtitleFile of string
        | [<Unique; AltCommandLine("-fps")>] Fps of float
        | [<Unique; AltCommandLine("-lang")>] LanguageId of string
        | [<Unique; AltCommandLine("-imdb")>] ImdbId of int64
        interface IArgParserTemplate with
            member this.Usage = 
                match this with
                | MovieFile _ -> "Media file, needed to properly index the subtitles."
                | SubtitleFile _ -> "Subtitle file, to be zipped and uploaded"
                | Fps _ -> "Frames per second. You can find this in the movie file metadata."
                | LanguageId _ -> "Three-letter language code according to http://www.opensubtitles.org/addons/export_languages.php ."
                | ImdbId _ -> "IMDB id for your movie. Can be found on http://www.imdb.com or by using --searchMovie"
    
    and ReportHashArgs = 
        | [<Unique; MainCommand>] SubFileId of int64
        interface IArgParserTemplate with
            member this.Usage = 
                match this with
                | SubFileId _ -> "Value of field 'IDSubMovieFile' as returned from the OSDb."
    
    and DownloadArgs = 
        | [<Unique; AltCommandLine("-mov")>] MoviePath of string option
        | [<Unique; AltCommandLine("-out")>] OutputPath of string option
        | [<Unique; AltCommandLine("-n")>] Name of string option
        | [<Unique; AltCommandLine("-ser")>] Series of season : int * episode : int
        | [<Unique; AltCommandLine("-y")>] Year of int option
        | [<Unique; AltCommandLine("-lang")>] LanguageId of string option
        | [<Unique; AltCommandLine("-f")>] Fps of float option
        | [<Unique; AltCommandLine("-im")>] ImdbId of string list
        | [<Unique; AltCommandLine("-subid")>] SubFileId of string list
        | [<Unique; AltCommandLine("-ext")>] MediaExtensions of string list
        | [<Unique>] Force
        | [<Unique; AltCommandLine("-so")>] SearchOnly
        interface IArgParserTemplate with
            member this.Usage = 
                match this with
                | MoviePath _ -> "Path to media file to search for subtitles by hash."
                | OutputPath _ -> "Path where the resulting subtitles will be written."
                | MediaExtensions _ -> "If --moviepath is folder, filter files to be hashed by extensions. Default is .mkv, *.mp4"
                | Name _ -> "Name of movie or episode for IMDB search, if hash not found. Use quotes for names containing spaces or other separators."
                | Series _ -> "If tv series, filter results by pattern (season, episode)"
                | Year _ -> "Release year"
                | Fps _ -> "Frames per second. You can find this in the movie file metadata."
                | LanguageId _ -> "Three-letter language code according to http://www.opensubtitles.org/addons/export_languages.php ."
                | ImdbId _ -> 
                    "IMDB id for your movie. If set hash search and imdbid search are skipped. Can be found on http://www.imdb.com or by --name. More than one id may be entered in quotes."
                | SubFileId _ -> 
                    "Get subtitle file directly by id. Other search terms ignored except --outpath. More than one id may be entered in quotes."
                | SearchOnly -> "Set this flag if you want to see search results without downloading anything."
                | Force -> "Overwrite existing subtitle files."
                |> sprintf "%s\n"
    
    and ConfigureArgs = 
        | [<Unique; AltCommandLine("file")>] FileName of string
        | [<Unique; AltCommandLine("clr")>] Command of CRUD
        interface IArgParserTemplate with
            member this.Usage = 
                match this with
                | FileName _ -> "Path of configuration file. Default value is FsOSBd.settings"
                | Command _ -> "CRUD commands to be used in conjuction with --filename"
    
    and CredentialAgrs = 
        | [<Unique; AltCommandLine("-log")>] LogInfo of UserName:string*Password:string
        | [<Unique>] Language of string option
        | [<ExactlyOnceAttribute>] UserAgent of string
        interface IArgParserTemplate with
            member this.Usage = 
                match this with
                | LogInfo _ -> "Password and Username for OSDb. It's necessary to register if you're going to be making more than 200 calls a day. Password is stored in plain text for now, so mind your .config file."
                | Language _ -> "Language for OSDb."
                | UserAgent _ -> "UserAgent for OSDb API."
    
    and CLIArguments = 
        | [<Unique; CliPrefix(CliPrefix.None)>] Convert of ParseResults<ConvertArgs>
        | [<Unique; CliPrefix(CliPrefix.None)>] Download of ParseResults<DownloadArgs>
        | [<Unique; CliPrefix(CliPrefix.None)>] ReportHash of ParseResults<ReportHashArgs>
        | [<Unique; CliPrefix(CliPrefix.None)>] Upload of ParseResults<UploadArgs>
        //| [<Unique; CliPrefix(CliPrefix.None)>] Configure of ParseResults<ConfigureArgs>
        | [<Unique; CliPrefix(CliPrefix.None)>] Credentials of ParseResults<CredentialAgrs>
        | [<Unique; AltCommandLine("-log")>] Log
        | [<Unique; AltCommandLine("-s")>] Silent
        | [<Unique; AltCommandLine("-v")>] Version
        //        | [<Unique; AltCommandLine("-p")>] Path of string
        //        | [<Unique; AltCommandLine("-n")>] Name of string
        //        | [<Unique; AltCommandLine("-e")>] Episode of season : int * episode : int
        //        | [<Unique; AltCommandLine("-y")>] Year of int
        //        | [<Unique; AltCommandLine("-l")>] LangId of string
        //        | [<Unique; AltCommandLine("-f")>] Fps of FpsKind
        //        | [<Unique; AltCommandLine("-ul")>] UploadFile of string
        //        | [<Unique; AltCommandLine("-i")>] ImdbId of string
        //        | [<Unique; AltCommandLine("-imdb")>] SearchImdb of string
        //        | [<Unique; AltCommandLine("-m")>] Menu
        //        | [<Unique; AltCommandLine("-rwmh")>] ReportWrongMovieHash of string
        //        | [<Unique>] Force
        //        | [<Unique; AltCommandLine("-subid")>] SubFileId of id : string list
        interface IArgParserTemplate with
            member s.Usage = 
                match s with
                | Convert _ -> "Alter existing subtitle files by changing FPS or offseting the timestamp. For .ass & .srt only."
                | Download _ -> "Search OSDb for subtitles and download results."
                | ReportHash _ -> "Use this to report mismatched or out of sync subtitles. With enough reports the sub gets removed."
                | Upload _ -> "Upload new subtitles to the OSDb."
                //| Configure _ -> "Store or retrieve current options to file."
                | Credentials _ -> "Use at least once to store api credentials in an app.config file for convenience. For now the app.config file will be unencrypted, so take care not to share it."
                | Log -> "Enable xmlrpc logger. Generates xml files of all requests and results."
                | Silent -> "Don't show intermediate information messages."
                | Version -> "Show assembly version."
//                | Path _ -> "File path or folder (use quotes for complex paths)"
//                | Name _ -> "Name of movie or episode. Required for IMDB search. Use quotes for names containing spaces and other weird characters."
//                | Episode _ -> "Episode pattern (season, episode)"
//                | Year _ -> "Release year"
//                | LangId _ -> 
//                    "Filter subtitles by a three-letter language code according to http://www.opensubtitles.org/addons/export_languages.php . Required when uploading subtitles."
//                | Fps _ -> "FPS preference"
//                | UploadFile _ -> "Attempt to upload .srt file"
//                | SearchImdb _ -> "Use available input to search IMDB for info"
//                | SubFileId _ -> "Query OSDb for a particular subtitle id, or several, space separated. Does not need --download"
//                | ImdbId _ -> 
//                    "Imdb id for the movie in --path. Required in order to upload subtitles, or to get relevant available subtitles. Can be found either by using the --SearchImdb switch or by manually looking for the movie on http://www.imdb.com and copying the numeric parth from the url."
//                | Menu -> "If multiple files are found ask the user to chose between them."
//                | Force -> "Overwrite existing files with newer."
//                | ReportWrongMovieHash _ -> 
//                    "If downloaded subtitle doesn't match your movie or is out of sync and so forth, use this command to report it. If enough reports are made the OSDb entry gets removed."
