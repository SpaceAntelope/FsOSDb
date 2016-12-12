namespace FsOSBd.Core

module ConvertValidate = 
    open ValidationManager
    open Story
    open System.IO
    open HelperFunctions
    open RR
    
    type ErrorCase = 
        | FileNotSubtitleAccordingToExtension of Path.ErrorCase
        | SubtitleFileInValid of string
        | FpsSourceAndTargetAreEqual of float * float
        | OutputFileWithSameNameFound of string
        | FpsSourceIsZero
        | FpsTargetIsZero
        | PathError of Path.ErrorCase
    
    let AssertPathIsValid(story : Story<ChapterConvert>) = 
        story.Chapter.SourceFile
        |> Path.AssertFileExists
        >>= Path.AssertExtensionIsSubtitle
        |> function 
        | Success _ -> Success story
        | Failed error -> PathError error |> Failed
    
    let AssertSubIsValid(story : Story<ChapterConvert>) = 
        (* TODO: Validate subfiles timestamps *)
        (* TODO: Validate content not empty or whitespace *)
        Success story
    
    let AssertFpsConversionIsValid(story : Story<ChapterConvert>) = 
        match story.Chapter.Fps with
        | Some(source, target) when source = target -> FpsSourceAndTargetAreEqual(source, target) |> Failed
        | Some(source, _) when source = 0.0 -> FpsSourceIsZero |> Failed
        | Some(_, target) when target = 0.0 -> FpsTargetIsZero |> Failed
        | _ -> Success story
    
    let Decode pattern replacement source = 
        if source = pattern then replacement
        else source
    
    let TargetFileName(chapter : ChapterConvert) = 
        [ Path.GetDirectoryName; Path.GetFileNameWithoutExtension; Path.GetExtension ]
        |> List.map ((|>) chapter.SourceFile)
        |> fun [ directory; filename; extension ] -> 
            [ directory
              sprintf "%s[%s][%d][%.3f]" filename (chapter.Fps <??> (0.0, 0.0)
                                           |> snd
                                           |> string
                                           |> Decode "0" "") (chapter.OffsetMs) (chapter.CummulativeMin)
              extension ]
        |> fun [ directory; filename; extension ] -> Path.Combine(directory, filename) + extension
    
    let AssertTargetFileNoOverwriteUnlessForced(story : Story<ChapterConvert>) = 
        let outFile = story.Chapter |> TargetFileName
        match outFile |> File.Exists with
        | true when story.Chapter.Force |> not -> 
            outFile
            |> OutputFileWithSameNameFound
            |> Failed
        | _ -> Success story
    
    let (|Srt|Ass|UnknownExtension|) path = 
        path
        |> Path.GetExtension
        |> ToLower
        |> function 
        | ".srt" -> Srt
        | ".ass" -> Ass
        | _ -> UnknownExtension
