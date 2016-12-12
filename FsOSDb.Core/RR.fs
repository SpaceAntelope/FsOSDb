namespace FsOSBd.Core

module RR = 
    type Result<'TSuccess, 'TError> = 
        | Success of 'TSuccess
        | Failed of 'TError
        static member Bind f input = 
            match input with
            | Success s -> f s
            | Failed err -> Failed err
    
    let inline (>>=) input f = Result<_, _>.Bind f input
    
    let inline (>=>) switch1 switch2 x = 
        match switch1 x with
        | Success s -> switch2 s
        | Failed f -> Failed f

    let inline (/>) x f = 
        f x |> ignore
        x  