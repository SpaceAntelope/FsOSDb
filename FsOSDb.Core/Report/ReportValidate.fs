namespace FsOSBd.Core

module ReportValidate = 
    open ValidationManager
    open Story
    open Results
    open RR
    
    type ErrorCase = 
        | ReportFailed of Common.ErrorCase
        | InvalidCredentials of Common.ErrorCase
        | LogingFailed of Common.ErrorCase
    
    let AssertReportComplete (story : Story<ChapterReportHash>) (result : BaseResult) = 
        result.Status
        |> Common.ValidateStatus
        |> function 
        | Success _ -> Success story
        | Failed error -> 
            error
            |> ReportFailed
            |> Failed
