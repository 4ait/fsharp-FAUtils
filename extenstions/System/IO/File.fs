[<RequireQualifiedAccess>]
module System.IO.File

open System
open FAUtils.Async
open FAUtils.ErrorManagement

[<RequireQualifiedAccess>]
module FAEx =
    type FileDeleteError =
        | NotFound of filePath: string * ex: Exception option
        | Unknown of ex: Exception option
        
        interface IError with
            member this.Exception =
                match this with
                | NotFound(_, ex) -> ex
                | Unknown ex -> ex
                
            member this.ToString() =
                match this with
                | NotFound(filePath, _) -> $"File {filePath} is not found"
                | Unknown ex ->
                    match ex with
                    | Some ex -> ex.Message
                    | None -> "Unknown error"

    let public Delete file =
        try
            File.Delete(file)
            Ok()
        with
        | :? DirectoryNotFoundException as ex -> Error(NotFound(file, Some ex))
        | ex -> Error(Unknown(Some ex))
    
    let public DeleteAsync file =
        BlockingTask.Run(fun () -> Delete(file))
