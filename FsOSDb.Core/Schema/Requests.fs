namespace FsOSBd.Core

module Requests = 
    open CookComputing.XmlRpc
    open HelperFunctions
    open System.IO
    open Story
    
    type SearchSubtitleRequest = 
        { SubLanguageId : string
          MovieHash : string
          MovieByteSize : string
          ImdbId : string }
        
        member x.ToStruct = 
            let request = XmlRpcStruct()
            request.["sublanguageid"] <- x.SubLanguageId
            request.["moviebytesize"] <- string x.MovieByteSize
            request.["moviehash"] <- x.MovieHash
            request.["imdbid"] <- x.ImdbId
            request
        
        static member OfStory(story : Story<ChapterDownload>) = 
            let fromIdmbId imdbId = 
                { SubLanguageId = story.Chapter.LanguageId <??> "all"
                  MovieHash = ""
                  MovieByteSize = "" //if hash search failed it is basically certain that filtering by size will also yield nothing
                  ImdbId = string imdbId }
            match story.Chapter.MediaFilePaths with
            | [||] when story.Chapter.ImdbId
                        |> Array.isEmpty
                        |> not -> story.Chapter.ImdbId |> Array.map fromIdmbId
            | paths -> 
                paths |> Array.map (fun fileName -> 
                             { SubLanguageId = story.Chapter.LanguageId <??> "all"
                               MovieHash = fileName |> ComputeMovieHash
                               MovieByteSize = FileInfo(fileName).Length |> string
                               ImdbId = "" })
    
    type TryUploadRequest = 
        { SubHash : string
          SubFilename : string
          MovieHash : string
          MovieByteSize : double
          MovieFileName : string
          MovieFps : float option
          MovieTimeMS : int option
          MovieFrames : int option }
        
        member x.ToStruct = 
            let request = XmlRpcStruct()
            request.["subhash"] <- x.SubHash
            request.["subfilename"] <- x.SubFilename
            request.["moviehash"] <- x.MovieHash
            request.["moviebytesize"] <- string x.MovieByteSize
            request.["moviefilename"] <- x.MovieFileName
            request.["movietimems"] <- match x.MovieTimeMS with
                                       | Some x -> string x
                                       | _ -> System.String.Empty
            request.["movieframes"] <- match x.MovieFrames with
                                       | Some x -> string x
                                       | _ -> System.String.Empty
            request.["movieaka"] <- match x.MovieFps with
                                    | Some x -> string x
                                    | _ -> System.String.Empty
            //            if x.MovieTimeMS |> Option.isSome then request.["movietimems"] <- (x.MovieTimeMS |> Option.get)
            //            if x.MovieFrames |> Option.isSome then request.["movieframes"] <- (x.MovieFrames |> Option.get)
            //            if x.MovieFps |> Option.isSome then request.["moviefps"] <- (x.MovieFps |> Option.get)
            request
        
        static member OfStory(story : Story<ChapterUpload>) = 
            let movieInfo = FileInfo(story.Chapter.MovieFile)
            let subInfo = FileInfo(story.Chapter.SubtitleFile)
            { SubHash = subInfo.FullName |> checksumMd5
              SubFilename = subInfo.FullName |> Path.GetFileName
              MovieHash = movieInfo.FullName |> ComputeMovieHash
              MovieByteSize = movieInfo.Length |> double
              MovieFileName = movieInfo.Name |> Path.GetFileName
              MovieFps = 
                  if story.Chapter.Fps > 0.0 then Some story.Chapter.Fps
                  else None
              MovieTimeMS = None
              MovieFrames = None }
    
    type BaseInfo = 
        { IdMovieImdb : string
          SubLanguageId : string
          MovieReleaseName : string
          MovieAKA : string option
          SubAuthorComment : string option }
        
        member x.ToStruct = 
            let request = XmlRpcStruct()
            request.["idmovieimdb"] <- x.IdMovieImdb
            request.["sublanguageid"] <- x.SubLanguageId
            request.["moviereleasename"] <- x.MovieReleaseName
            request.["movieaka"] <- match x.MovieAKA with
                                    | Some x -> x
                                    | _ -> System.String.Empty
            request.["subauthorcomment"] <- match x.SubAuthorComment with
                                            | Some x -> x
                                            | _ -> System.String.Empty
            request
        
        static member OfStory(story : Story<ChapterUpload>) = 
            { IdMovieImdb = story.Chapter.ImdbId |> string
              SubLanguageId = story.Chapter.LanguageId
              MovieReleaseName = story.Chapter.MovieFile |> Path.GetFileNameWithoutExtension
              MovieAKA = None
              SubAuthorComment = None }
    
    type CDItem = 
        { UploadRequest : TryUploadRequest
          SubContent : string }
        
        member x.ToStruct = 
            let request = x.UploadRequest.ToStruct
            request.["subcontent"] <- x.SubContent
            request
        
        static member OfStory(story : Story<ChapterUpload>) = 
            { UploadRequest = story |> TryUploadRequest.OfStory
              SubContent = story.Chapter.SubtitleFile |> GZipAndEncodeBase64 }
    
    type UploadSubtitlesRequest = 
        { BaseInfo : BaseInfo
          CD : CDItem [] }
        
        member x.ToStruct = 
            let request = XmlRpcStruct()
            request.["baseinfo"] <- x.BaseInfo.ToStruct
            x.CD |> Array.iteri (fun i item -> request.["cd" + (i + 1).ToString()] <- item.ToStruct)
            request
        
        static member OfStory(story : Story<ChapterUpload>) = 
            { BaseInfo = story |> BaseInfo.OfStory
              CD = 
                  story
                  |> CDItem.OfStory
                  |> ToArray }
