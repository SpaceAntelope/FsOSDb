namespace FsOSBd.Core

module UploadStory = 
    open Story
    open HelperFunctions
    open ValidationManager
    open Requests
    open RR
    
    let CheckForExistingSubtitles(story : Story<ChapterUpload>) = 
        printfnInfo "Checking if subtitles exist..."
        story
        |> TryUploadRequest.OfStory
        |> ToArray
        |> OSDbApi.TryUploadSubtitles story.Token
        |> UploadValidate.AssertSubtitleInfoNotInDb
        |> function 
        | Success _ -> Success story
        | Failed x -> Failed x
    
    let CallApi(story : Story<ChapterUpload>) = 
        printfnInfo "Sending subtitles..."
        story
        |> UploadSubtitlesRequest.OfStory
        |> OSDbApi.UploadSubtitles story.Token
        |> fun _ -> Success story // { story with Message = sprintf "UploadSubtitleResult:\n%s" result.Result.Source }
    
    let OnceUponATime(story : Story<ChapterUpload>) = 
        ValidationManager.Common.AssertCredentials UploadValidate.InvalidCredentials story
        >>= ValidationManager.Common.AssertLogin UploadValidate.LogingFailed
        >>= UploadValidate.AssertMoviePathIsValid
        >>= UploadValidate.AssertSubtitlePathIsValid
        >>= UploadValidate.AssertLanguageIdIsValid
        >>= UploadValidate.AssertSrtIsValid
        >>= UploadValidate.AssertImdbPageExists
        >>= CheckForExistingSubtitles
        >>= CallApi
