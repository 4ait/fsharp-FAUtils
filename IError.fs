module FAUtils.ErrorManagement

open System

type IError =
    abstract member ToString: unit -> string
    abstract member Exception: unit -> Exception option with get

[<RequireQualifiedAccess>]
module IError = 
    let public FullMessage(error: IError) =
            match error.Exception with
            | Some exceptionData -> $"{error.ToString()}\n\nException: {exceptionData.Message}\n\n{exceptionData.StackTrace}"
            | None -> error.ToString()
    
    
    let public Expect<'TResult, 'Error when 'Error :> IError>(result: Result<'TResult, 'Error>): 'TResult =
        match result with
        | Ok result -> result
        | Error err -> err |> FullMessage |> failwith