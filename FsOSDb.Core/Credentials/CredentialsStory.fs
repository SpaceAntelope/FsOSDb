namespace FsOSBd.Core

module CredentialsStory = 
    open Story
    open RR
    open System.Configuration
    open ConfigurationManager

    let updateSetting key value (configSettings : KeyValueConfigurationCollection) = 
        if configSettings.[key] |> isNull then configSettings.Add(key, value)
        else configSettings.[key].Value <- value
        configSettings
    
    let UpdateCredentials(story : Story<ChapterCredentials>) = 
        try 
            let configFile = ConfigurationManager.OpenExeConfiguration(ConfigurationUserLevel.None)
            configFile.AppSettings.Settings
            |> updateSetting "OSDbUserName" story.Chapter.UserName
            |> updateSetting "OSDbPassword" story.Chapter.Password
            |> updateSetting "OSDbUserAgent" story.Chapter.UserAgent
            |> updateSetting "OSDbLanguage" story.Chapter.Language
            |> ignore
            configFile.Save(ConfigurationSaveMode.Modified)
            ConfigurationManager.RefreshSection(configFile.AppSettings.SectionInformation.Name)
            Success story
        with ex -> 
            ex.Message
            |> CredentialsValidate.ErrorUpdatingCredentials
            |> Failed
    
    let OnceUponATime(story : Story<ChapterCredentials>) = CredentialsValidate.AssertUserAgentExists story >>= UpdateCredentials
