namespace FsOSBd.Core

module UploadValidate = 
    open ValidationManager
    open Story
    open Results
    open RR
    
    type ErrorCase = 
        | MovieFileError of Path.ErrorCase
        | SubtitleFileError of Path.ErrorCase
        | LanguageIdIsInvalid of string
        | ImdbIdNotInDatabase of string
        | SubtitleInfoAlreadyInDb of SubFile
        | InvalidCredentials of Common.ErrorCase    
        | LogingFailed of Common.ErrorCase

    let AssertLanguageIdIsValid(story : Story<ChapterUpload>) = 
        LanguageCodes.CodeIndex.ContainsKey(story.Chapter.LanguageId) |> function 
        | true -> Success story
        | false -> LanguageIdIsInvalid story.Chapter.LanguageId |> Failed
    
    let AssertSubtitlePathIsValid(story : Story<ChapterUpload>) = 
        story.Chapter.SubtitleFile
        |> Path.AssertFileExists
        >>= Path.AssertExtensionIsSubtitle
        |> function 
        | Success _ -> Success story
        | Failed error -> SubtitleFileError error |> Failed
    
    let AssertSrtIsValid(story : Story<ChapterUpload>) = 
        (* TODO: *)
        Success story
    
    let AssertMoviePathIsValid(story : Story<ChapterUpload>) = 
        story.Chapter.MovieFile
        |> Path.AssertFileExists
        |> function 
        | Success _ -> Success story
        | Failed error -> MovieFileError error |> Failed
    
    let AssertImdbPageExists(story : Story<ChapterUpload>) = 
        (* TODO: Query imdb.com ? *)
        Success story
    
    let AssertSubtitleInfoNotInDb(result : TryUploadResult) = 
        match result.AlreadyInDb with
        | "1" -> SubtitleInfoAlreadyInDb result.Data.[0] |> Failed
        | _ -> result |> Success
