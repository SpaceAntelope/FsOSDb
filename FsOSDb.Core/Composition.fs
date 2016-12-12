namespace FsOSBd.Core

module Composition = 
    open ValidationManager
    open Story
    open RR
    open ConfigurationManager
    
    type ErrorCase = 
        | CommonError of Common.ErrorCase
        | PathError of Path.ErrorCase
        | ConvertError of ConvertValidate.ErrorCase
        | UploadError of UploadValidate.ErrorCase
        | ReportError of ReportValidate.ErrorCase
        | DownloadError of DownloadValidate.ErrorCase
        | CredentialsError of CredentialsValidate.ErrorCase
        | Finished
    
    type ChapterSuccess = 
        | Convert of Story<ChapterConvert>
        | Upload of Story<ChapterUpload>
        | ReportHash of Story<ChapterReportHash>
        | Download of Story<ChapterDownload>
        //| Configure of Story<ChapterConfigure>
        | Credentials of Story<ChapterCredentials>
    
    let Download result = 
        match result with
        | Success story -> 
            story
            |> ChapterSuccess.Download
            |> Success
        | Failed error -> 
            error
            |> DownloadError
            |> Failed
    
    let Upload result = 
        match result with
        | Success story -> 
            story
            |> ChapterSuccess.Upload
            |> Success
        | Failed error -> 
            error
            |> UploadError
            |> Failed
    
    let ReportHash result = 
        match result with
        | Success story -> 
            story
            |> ChapterSuccess.ReportHash
            |> Success
        | Failed error -> 
            error
            |> ReportError
            |> Failed
    
    let Convert result = 
        match result with
        | Success story -> 
            story
            |> ChapterSuccess.Convert
            |> Success
        | Failed error -> 
            error
            |> ConvertError
            |> Failed
    
    let Credentials result = 
        match result with
        | Success story -> 
            story
            |> ChapterSuccess.Credentials
            |> Success
        | Failed error -> 
            error
            |> CredentialsError
            |> Failed
//    let Configure result = 
//        match result with
//        | Success story -> 
//            story
//            |> ChapterSuccess.Configure
//            |> Success
//        | Failed error -> error |> ConfigureError |> Failed
