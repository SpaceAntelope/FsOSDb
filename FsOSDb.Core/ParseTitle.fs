namespace FsOpenSubtitles

module ParseTitle = 
    open System
    open System.IO
    open System.Text.RegularExpressions
    open OSHelperFunctions
    
    let SxxExxPattern = @"[Ss]\d{1,2}([Ee]\d{1,2})?"
    let SSxEEPattern = @"\d{1,2}x\d{1,2}"
    let removeBrackets = RReplace @"\[[^\]]+[^p]\]" ""
    let replaceDotsWithSpaces = RReplace @"(\w)\." "$1 "
    let truncateUntilResolution = RReplace @"(.+?[\(\[]?\d{3,4}[ip]?[\)\]]?)(.*)" "$1"
    let removeResolution = RReplace @"\W?\d{3,4}p\W?" ""
    let parenthesizeYear = RReplace @"[^\(](\d{4})[^\)]" "$1"
    
    type Parsed = 
        { Result : string
          FileName : string }
    
    let GetTvEpisodeTitle { Result = title; FileName = _ } = 
        [ RSplit SxxExxPattern title
          RSplit SSxEEPattern title ]
        |> List.where (fun arr -> arr.Length > 1)
        |> List.map (Array.last >> Trim)
        |> List.where (function 
               | IsMatch @"[Ee]\d{1,2}" false -> true
               | _ -> false)
        |> function 
        | [] -> None
        | list -> 
            List.head list
            |> Trim
            |> Some
    
    let GetTvEpisodeSeriesName { Result = title; FileName = _ } = 
        [ RSplit SxxExxPattern title
          RSplit SSxEEPattern title ]
        |> List.where (fun arr -> arr.Length > 1)
        |> List.map (Array.head>>Trim>>Some)
        |> Array.ofList
        //|> List.head
    
    let GetTvEpisodeQuery({ Result = title; FileName = _ } as parsed) = 
        [| GetTvEpisodeTitle parsed
           //Some(GetTvEpisodeSeriesName parsed)
           Some title |]
        |> Array.append (GetTvEpisodeSeriesName parsed)
        |> Array.choose (id)
        |> Array.distinct
        |> Array.map (Trim)
        |> Array.filter (System.String.IsNullOrEmpty >> not)
    
    let GetMovieNameQuery { Result = title; FileName = _ } = 
        [| parenthesizeYear title
           title |]
        |> Array.distinct
        |> Array.map (Trim)
        |> Array.filter (System.String.IsNullOrEmpty >> not)
    
    let GetAnimeEpisodeQuery { Result = title; FileName = _ } = title
    
    let (|MovieQuery|TvEpisodeQuery|AnimeEpisodeQuery|Unknown|) ({ Result = title; FileName = fname } as parsed) = 
        match title with
        | IsMatch SxxExxPattern true | IsMatch SSxEEPattern true -> TvEpisodeQuery(GetTvEpisodeQuery parsed)
        | IsMatch "\(?\d{4}\)?$" true -> MovieQuery(GetMovieNameQuery parsed)
        | IsMatch "OAD" true | IsMatch "OVA" true | IsMatch @"\s\d+" true -> AnimeEpisodeQuery(GetAnimeEpisodeQuery parsed)
        | _ -> Unknown parsed //(str + "   " + (Path.GetFileNameWithoutExtension fileName)) 
    
    let ParseMovieNameFromFileName fileName = 
        fileName
        |> Path.GetFileNameWithoutExtension
        |> removeBrackets
        |> replaceDotsWithSpaces
        |> truncateUntilResolution
        |> removeResolution
        |> fun str -> 
            { Result = ToCamelCase (str.Trim())
              FileName = (Path.GetFileNameWithoutExtension fileName) }
