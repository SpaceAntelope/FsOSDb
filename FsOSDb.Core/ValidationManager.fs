namespace FsOSBd.Core

module ValidationManager = 
    open HelperFunctions
    open System.IO
    open RR
    open Story
    open Results
    open ConfigurationManager
    
    module Common = 
        type ErrorCase = 
            | XmlRpcOperationFailed of exn
            | UnkownStatus of string
            | ConnectionFailed of exn
            | NotImplemented of string
            | InvalidCredentials of message : string * ChapterCredentials
            | Finished
        
        let ValidateStatus(status : string) = 
            match (status.Split(' ').[0]) with
            | "200" | "206" -> Success status
            | "401" | "402" | "403" | "404" | "405" | "406" | "407" | "408" | "409" | "410" | "411" | "412" | "501" | "503" | "506" | "301" -> 
                status
                |> exn
                |> ConnectionFailed
                |> Failed
            | _ -> 
                status
                |> UnkownStatus
                |> Failed
        
        //ArgumentParser.Create<CredentialAgrs>(programName = "FsOSBd.Core.exe credentials").PrintUsage() |> printfnInfo "%s"
        let ValidateCredentials(story : Story<_>) = 
            let storedCredentials = ChapterCredentials.CreateFromSettings //  ConfigurationManager.LoadCredentials() //ChapterManager.CredentialsFromConfig()
            if storedCredentials.UserAgent |> IsNullOrEmpty then 
                ("UserAgent not found. Please enter a valid osdb user agent to access the database.", storedCredentials)
                |> InvalidCredentials
                |> Failed
            else story |> Success
        
        let AssertCredentials (toLocalErrorType : ErrorCase -> 'a) (story : Story<_>) = 
            match ValidateCredentials story with
            | Success _ -> Success story
            | Failed err -> 
                err
                |> toLocalErrorType
                |> Failed
        
        let AssertLogin (toLocalErrorType : ErrorCase -> 'a) (story : Story<_>) = 
            let { UserName = username; Password = password; Language = language; UserAgent = useragent } = ChapterCredentials.CreateFromSettings
            let { Result = baseResult; Token = token } = OSDbApi.Login username password language useragent
            baseResult.Status
            |> ValidateStatus
            |> function 
            | Failed x -> 
                x
                |> toLocalErrorType
                |> Failed
            | Success _ -> 
                ConfigurationManager.Token <- token
                { story with Token = token } |> Success
    
    module Path = 
        type ErrorCase = 
            | FileNotFound of string
            | PathNotAFile of string
            | PathNotADirectory of string
            | FileNotSubtitleAccordingToExtension of string
        
        let AssertFileExists path = 
            match path with
            | IsMissing -> FileNotFound path |> Failed
            | IsDirectory -> PathNotAFile path |> Failed
            | IsFile -> Success path
        
        let AssertDirectoryExists path = 
            match path with
            | IsMissing -> FileNotFound path |> Failed
            | IsFile -> PathNotADirectory path |> Failed
            | IsDirectory -> Success path
        
        let AssertExtensionIsSubtitle path = 
            path
            |> Path.GetExtension
            |> ToLower
            |> function 
            | ".srt" | ".ass" -> Success path
            | _ -> FileNotSubtitleAccordingToExtension path |> Failed
