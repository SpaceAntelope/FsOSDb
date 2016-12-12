namespace FsOSBd.Core

module Results = 
    open CookComputing.XmlRpc
    open OSXmlRpc
    open HelperFunctions
    
    type BaseResult = 
        { Status : string
          Seconds : string
          Source : string }
    
    let BaseResultOfStruct(source : XmlRpcStruct) = 
        { Status = tryGetStringValue "status" source
          Seconds = tryGetStringValue "seconds" source
          Source = (stringOfStruct >> teeAgent) source }
    
    type LoginResult = 
        { Token : string
          Result : BaseResult }
    
    type SearchImdbResultItem = 
        { Id : string
          Title : string }
    
    let SearchImdbResultItemOfStruct(source : XmlRpcStruct) = 
        { Id = tryGetStringValue "id" source
          Title = tryGetStringValue "title" source }
    
    type SearchImdbResult = 
        { Result : BaseResult
          Data : SearchImdbResultItem [] }
    
    let SearchImdbResultOfStruct(source : XmlRpcStruct) = 
        { Result = BaseResultOfStruct source
          Data = 
              source
              |> tryGetArrayValue "data"
              |> Array.map (SearchImdbResultItemOfStruct) }
    
    type BasicMovieInfo = 
        { IDMovieImdb : string
          MovieName : string
          MovieYear : string
          MovieKind : string }
        
        member x.Url = 
            match x.IDMovieImdb with
            | IsMatch "^\d+$" true -> sprintf "http://www.imdb.com/title/tt%s/" x.IDMovieImdb
            | _ -> "-"
        
        static member PrintColumns(info : BasicMovieInfo []) = 
            let printable (item : BasicMovieInfo) = [| item.IDMovieImdb; item.MovieName; item.MovieYear; item.MovieKind; item.Url |]
            printPropertyTable printable [| "Imdb Id"; "Name"; "Year"; "Kind"; "URL" |] info
    
    let BasicMovieInfoOfStruct(source : XmlRpcStruct) = 
        { IDMovieImdb = tryGetStringValue "IDMovieIMDB" source
          MovieName = tryGetStringValue "MovieName" source
          MovieKind = tryGetStringValue "MovieKind" source
          MovieYear = tryGetStringValue "MovieYear" source }
    
    type SuggestMovieResult = 
        { Result : BaseResult
          Data : BasicMovieInfo [] }
    
    let SuggestMovieResultOfStruct (searchString : string) (source : XmlRpcStruct) = 
        { Result = BaseResultOfStruct source
          Data = 
              source.["data"] :?> XmlRpcStruct
              |> tryGetArrayValue searchString
              |> Array.map (BasicMovieInfoOfStruct) }
    
    type MovieInfo = 
        { IDMovie : string
          IDMovieImdb : string
          MovieHash : string
          MovieImdbRating : string
          MovieName : string
          MovieNameEng : string
          MovieReleaseName : string
          MovieYear : string }
    
    let MovieInfoOfStruct(source : XmlRpcStruct) = 
        { IDMovie = tryGetStringValue "IDMovie" source
          IDMovieImdb = tryGetStringValue "IDMovieImdb" source
          MovieHash = tryGetStringValue "MovieHash" source
          MovieImdbRating = tryGetStringValue "MovieImdbRating" source
          MovieName = tryGetStringValue "MovieName" source
          MovieNameEng = tryGetStringValue "MovieNameEng" source
          MovieReleaseName = tryGetStringValue "MovieReleaseName" source
          MovieYear = tryGetStringValue "MovieYear" source }
    
    type SubFile = 
        { MovieInfo : MovieInfo
          MoviefilenameWasAlreadyInDb : string
          HashWasAlreadyInDb : string }
    
    type TryUploadResult = 
        { Result : BaseResult
          Data : SubFile []
          AlreadyInDb : string }
    
    type SubtitleInfoResultItem = 
        { IDSubMovieFile : string
          IDSubtitle : string
          IDSubtitleFile : string
          ISO639 : string
          LanguageName : string
          MovieByteSize : string
          MovieInfo : MovieInfo
          MovieTimeMS : string
          MovieFPS : string
          SubActualCD : string
          SubAddDate : string
          SubAuthorComment : string
          SubBad : string
          SubDownloadLink : string
          SubDownloadsCnt : string
          SubFileName : string
          SubFormat : string
          SubHash : string
          SubLanguageID : string
          SubRating : string
          SubSize : string
          SubSumCD : string
          UserID : string
          UserNickName : string
          ZipDownloadLink : string }
        
        static member CreateEmpty = 
            { IDSubMovieFile = null
              MovieInfo = XmlRpcStruct() |> MovieInfoOfStruct
              MovieByteSize = null
              MovieTimeMS = null
              MovieFPS = null
              IDSubtitleFile = null
              SubFileName = null
              SubActualCD = null
              SubSize = null
              SubHash = null
              IDSubtitle = null
              UserID = null
              SubLanguageID = null
              SubFormat = null
              SubSumCD = null
              SubAuthorComment = null
              SubAddDate = null
              SubBad = null
              SubRating = null
              SubDownloadsCnt = null
              UserNickName = null
              ISO639 = null
              LanguageName = null
              SubDownloadLink = null
              ZipDownloadLink = null }
        
        static member PrintColumns(info : SubtitleInfoResultItem []) = 
            let printable (item : SubtitleInfoResultItem) = 
                [| item.IDSubtitleFile; item.IDSubMovieFile; item.SubFileName; item.SubSize; item.SubLanguageID; item.MovieFPS |]
            printPropertyTable printable [| "IDSubtitleFile"; "IDSubMovieFile"; "SubFileName"; "Size"; "LangId"; "FPS" |] info
    
    let SubtitleInfoResultItemOfStruct(source : XmlRpcStruct) = 
        { IDSubMovieFile = tryGetStringValue "IDSubMovieFile" source
          MovieInfo = MovieInfoOfStruct source
          MovieByteSize = tryGetStringValue "MovieByteSize" source
          MovieTimeMS = tryGetStringValue "MovieTimeMS" source
          MovieFPS = tryGetStringValue "MovieFPS" source
          IDSubtitleFile = tryGetStringValue "IDSubtitleFile" source
          SubFileName = tryGetStringValue "SubFileName" source
          SubActualCD = tryGetStringValue "SubActualCD" source
          SubSize = tryGetStringValue "SubSize" source
          SubHash = tryGetStringValue "SubHash" source
          IDSubtitle = tryGetStringValue "IDSubtitle" source
          UserID = tryGetStringValue "UserID" source
          SubLanguageID = tryGetStringValue "SubLanguageID" source
          SubFormat = tryGetStringValue "SubFormat" source
          SubSumCD = tryGetStringValue "SubSumCD" source
          SubAuthorComment = tryGetStringValue "SubAuthorComment" source
          SubAddDate = tryGetStringValue "SubAddDate" source
          SubBad = tryGetStringValue "SubBad" source
          SubRating = tryGetStringValue "SubRating" source
          SubDownloadsCnt = tryGetStringValue "SubDownloadsCnt" source
          UserNickName = tryGetStringValue "UserNickName" source
          ISO639 = tryGetStringValue "ISO639" source
          LanguageName = tryGetStringValue "LanguageName" source
          SubDownloadLink = tryGetStringValue "SubDownloadLink" source
          ZipDownloadLink = tryGetStringValue "ZipDownloadLink" source }
    
    type SubtitleInfoResult = 
        { Result : BaseResult
          Data : SubtitleInfoResultItem [] }
    
    let SubtitleInfoResultOfStruct(source : XmlRpcStruct) = 
        { Result = BaseResultOfStruct source
          Data = 
              source
              |> tryGetArrayValue "data"
              |> Array.map (SubtitleInfoResultItemOfStruct) }
    
    type SubContents = 
        { IdSubtitleFile : string
          Data : string }
    
    let SubContentsOfStruct(source : XmlRpcStruct) = 
        { IdSubtitleFile = tryGetStringValue "idsubtitlefile" source
          Data = tryGetStringValue "data" source }
    
    type SubtitleDownloadResult = 
        { Result : BaseResult
          Data : SubContents [] }
    
    let SubtitleDownloadResultOfStruct(source : XmlRpcStruct) = 
        { Result = BaseResultOfStruct source
          Data = 
              source
              |> tryGetArrayValue "data"
              |> Array.map (SubContentsOfStruct) }
    
    type UploadSubtitlesResult = 
        { Result : BaseResult
          Data : string
          Subtitles : bool }
    
    type HashResult = 
        { Hash : string
          FileLength : int64 }
    
    type DownloadResult = 
        { Data : byte []
          SubSearchInfo : SubtitleInfoResultItem }
    
    type WriteSubResult = 
        { FileInfo : System.IO.FileInfo
          SubSearchInfo : SubtitleInfoResultItem }
    
    let LoginResultOfStruct(source : XmlRpcStruct) = 
        { Token = tryGetStringValue "token" source
          Result = BaseResultOfStruct source }
    
    let SubFileOfStruct(source : XmlRpcStruct) = 
        { MovieInfo = MovieInfoOfStruct source
          MoviefilenameWasAlreadyInDb = tryGetStringValue "MoviefilenameWasAlreadyInDb" source
          HashWasAlreadyInDb = tryGetStringValue "HashWasAlreadyInDb" source }
    
    let TryUploadResultOfStruct(source : XmlRpcStruct) = 
        { Result = BaseResultOfStruct source
          Data = 
              source
              |> tryGetArrayValue "data"
              |> Array.map (SubFileOfStruct)
          AlreadyInDb = tryGetStringValue "alreadyindb" source }
    
    let UploadSubtitlesResultOfStruct(source : XmlRpcStruct) = 
        { Result = BaseResultOfStruct source
          Data = tryGetStringValue "data" source
          Subtitles = tryGetStringValue "subtitles" source |> System.Convert.ToBoolean }
