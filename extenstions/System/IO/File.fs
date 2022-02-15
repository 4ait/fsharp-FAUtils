[<RequireQualifiedAccess>]
module System.IO.File

open System
open FAUtils.Async
open FAUtils.ErrorManagement

[<RequireQualifiedAccess>]
module FAErr =
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

type FAEx =
    static member Delete file =
        try
            File.Delete(file)
            Ok()
        with
        | :? DirectoryNotFoundException as ex -> Error(FAErr.FileDeleteError.NotFound(file, Some ex))
        | ex -> Error(FAErr.FileDeleteError.Unknown(Some ex))
    
    static member DeleteAsync file =
        task {
            try
                do! BlockingTask.Run(fun () -> File.Delete(file))
                return Ok()
            with
            | :? DirectoryNotFoundException as ex -> return Error(FAErr.FileDeleteError.NotFound(file, Some ex))
            | ex -> return Error(FAErr.FileDeleteError.Unknown(Some ex))
        }
