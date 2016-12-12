namespace FsOSBd.Core

module CredentialsValidate = 
    open HelperFunctions
    open Story
    open RR
    open ConfigurationManager
    
    type ErrorCase = 
        | MissingUserName
        | MissingPassword
        | MissingUserAgent
        | MissingCredentials
        | ErrorUpdatingCredentials of string
    
    let AssertUserAgentExists(story : Story<ChapterCredentials>) = 
        //        if story.Chapter.UserName |> IsNullOrEmpty then MissingUserName |> Failed
        //        else if story.Chapter.Password |> IsNullOrEmpty then MissingPassword |> Failed
        //        else 
        if story.Chapter.UserAgent |> IsNullOrEmpty then MissingUserAgent |> Failed
        else story |> Success
