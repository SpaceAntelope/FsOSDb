namespace FsOSBd.Core

module ConvertStory = 
    open ValidationManager
    open Story
    open HelperFunctions
    open System.Text.RegularExpressions
    open System
    open System.IO
    open RR
    
    module Common = 
        let LoadData(story : Story<ChapterConvert>) = 
            { story with Chapter = { story.Chapter with SubFileContent = File.ReadAllLines(story.Chapter.SourceFile) } } |> Success
        
        let ConvertTimeSpan (from : float) (sourceFps, targetFps) offsetMs (source : TimeSpan) = 
            if source.TotalSeconds >= from then 
                let fpsOffset = 
                    if sourceFps = 0.0 || targetFps = 0.0 then source.TotalMilliseconds
                    else sourceFps / targetFps * source.TotalMilliseconds
                
                //let cumulativeFactor = Math.Floor(fpsOffset / Math.Floor(Math.Max(cumulative * 60000.0, 1.0)))
                //(fpsOffset + cumulativeFactor * offsetMs) |> TimeSpan.FromMilliseconds
                (fpsOffset + offsetMs) |> TimeSpan.FromMilliseconds
            else source
        
        let WriteSubtitleFile(story : Story<ChapterConvert>) = 
            let targetFilename = story.Chapter |> ConvertValidate.TargetFileName
            printfnResult "Writing file %s..." targetFilename
            (targetFilename, story.Chapter.SubFileContent) ||> WriteAllLines
            Success story
    
    module Srt = 
        open Common
        
        let TimestampPattern = "(?<h>\d{2}):(?<m>\d{2}):(?<s>\d{2})[,\.](?<ms>\d{3})"
        let TsToString(source : TimeSpan) = String.Format("{0:hh}:{0:mm}:{0:ss},{0:fff}", source)
        
        let TsFromString str = 
            Regex.Match(str, TimestampPattern)
            |> fun m -> 
                [| m.Groups.["h"].Value
                   m.Groups.["m"].Value
                   m.Groups.["s"].Value
                   m.Groups.["ms"].Value |]
                |> Array.map int
            |> fun [| h; m; s; ms |] -> TimeSpan(0, h, m, s, ms)
        
        let (|Index|TimeStamp|Line|Empty|) (input : string) = 
            let str = input |> Trim
            if str |> RIsMatch "^\d$" then Index str
            else if str |> RIsMatch(TimestampPattern + " --> " + TimestampPattern) then TimeStamp str
            else if String.IsNullOrEmpty(str) |> not then Line str
            else Empty
        
        let ConvertData(story : Story<ChapterConvert>) = 
            let convert = 
                match story.Chapter.Fps with
                | Some fps -> ConvertTimeSpan story.Chapter.OnlyAfter.TotalSeconds fps //story.Chapter.CummulativeMin
                | None -> ConvertTimeSpan story.Chapter.OnlyAfter.TotalSeconds (0.0, 0.0) //story.Chapter.CummulativeMin
                <| (float story.Chapter.OffsetMs)
            story.Chapter.SubFileContent
            |> Array.map (function 
                   | Index line -> line
                   | Line line -> line
                   | Empty -> ""
                   | TimeStamp line -> 
                       Regex.Split(line, " --> ")
                       |> Array.map (TsFromString
                                     >> convert
                                     >> TsToString)
                       |> Array.reduce (sprintf "%s --> %s"))
            |> fun content -> { story with Chapter = { story.Chapter with SubFileContent = content } }
    
    module Ass = 
        let TimestampPattern = "(?<h>\d{1,2}):(?<m>\d{2}):(?<s>\d{2})[,\.](?<ms>\d{2})"
        let ExtractDialoguePattern = "0,,((?<open>{)[^}]+(?<close-open>}))?(?<dialogue>.+)"
        let TsToString(source : TimeSpan) = String.Format("{0:hh}:{0:mm}:{0:ss},{0:fff}", source)
        let (|IsDialogue|) (str : string) = str.StartsWith("Dialogue: ")
        
        let TsFromString str = 
            Regex.Match(str, TimestampPattern)
            |> fun m -> 
                [| m.Groups.["h"].Value
                   m.Groups.["m"].Value
                   m.Groups.["s"].Value
                   m.Groups.["ms"].Value |]
                |> Array.map int
            |> fun [| h; m; s; ms |] -> TimeSpan(0, h, m, s, ms)
        
        let ExtractInfo line = 
            match line with
            | IsDialogue true -> 
                let [| start; stop |] = 
                    line
                    |> RMatches TimestampPattern
                    |> Array.map (TsFromString >> TsToString)
                
                let dialogue = 
                    line
                    |> RMatchGroup ExtractDialoguePattern [| "dialogue" |]
                    |> function 
                    | None -> "\n"
                    | Some m -> m.[0] + "\n" //   m.Groups.["dialogue"].Value + "\n"
                
                Some(start, stop, dialogue)
            | _ -> None
        
        let ConvertData(story : Story<ChapterConvert>) = 
            story.Chapter.SubFileContent
            |> Array.choose (ExtractInfo)
            |> Array.mapi (fun i (start, stop, dialog) -> 
                   (dialog.Split('\n')) |> Array.append [| string (i + 1)
                                                           sprintf "%s --> %s" start stop |])
            |> Array.collect (id)
            |> fun content -> { story with Chapter = { story.Chapter with SubFileContent = content } }
    
    let ConvertData(story : Story<ChapterConvert>) = 
        match story.Chapter.SourceFile with
        | ConvertValidate.Srt -> Srt.ConvertData story |> Success
        | ConvertValidate.Ass -> Ass.ConvertData story |> Success
        | ConvertValidate.UnknownExtension -> 
            story.Chapter.SourceFile
            |> Path.GetExtension
            |> ConvertValidate.SubtitleFileInValid
            |> Failed
    
    let OnceUponATime(story : Story<ChapterConvert>) = 
        story
        |> ConvertValidate.AssertPathIsValid
        >>= ConvertValidate.AssertFpsConversionIsValid
        >>= ConvertValidate.AssertTargetFileNoOverwriteUnlessForced
        >>= Common.LoadData
        >>= ConvertValidate.AssertSubIsValid
        >>= ConvertData
        >>= Common.WriteSubtitleFile
