namespace FsOSBd.Core

module OSDbApi = 
    open Results
    open Requests
    open OSXmlRpc
    open CookComputing.XmlRpc
    
    let Login userName password userAgent language = Proxy.LogIn userName password language userAgent |> LoginResultOfStruct
    let Logout token = Proxy.LogOut token |> BaseResultOfStruct
    let CheckMovieHash token (hashes : string []) = Proxy.CheckMovieHash token hashes
    let SearchMoviesOnIMDB query token = Proxy.SearchMoviesOnIMDB token query |> SearchImdbResultOfStruct
    
    let SearchSubtitles (token : string) (searchRequests : SearchSubtitleRequest []) = 
        searchRequests
        |> Array.map (fun searchRequest -> searchRequest.ToStruct)
        |> Proxy.SearchSubtitles token
        |> SubtitleInfoResultOfStruct
    
    let DownloadSubtitles token (fileIds : string []) = Proxy.DownloadSubtitles token fileIds |> SubtitleDownloadResultOfStruct
    let SuggestMovie token searchString = Proxy.SuggestMovie token searchString |> SuggestMovieResultOfStruct searchString
    
    let TryUploadSubtitles token (uploadRequest : TryUploadRequest []) = 
        uploadRequest
        |> Array.map (fun request -> request.ToStruct)
        |> Proxy.TryUploadSubtitles token
        |> TryUploadResultOfStruct
    
    let UploadSubtitles token (uploadRequest : UploadSubtitlesRequest) = 
        let request = XmlRpcStruct()
        request.["data"] <- uploadRequest.ToStruct
        Proxy.NonStandard <- XmlRpcNonStandard.AllowInvalidHTTPContent &&& XmlRpcNonStandard.IgnoreDuplicateMembers
        let result = Proxy.UploadSubtitles token request
        result |> UploadSubtitlesResultOfStruct
    
    let ReportWrongMovieHash token idSubMovieFile = Proxy.ReportWrongMovieHash token idSubMovieFile |> BaseResultOfStruct
