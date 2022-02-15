[<RequireQualifiedAccess>]
module System.IO.Directory

open System
open System.Collections.Generic
open System.IO
open System.Security
open FAUtils.Async
open FAUtils.ErrorManagement
open FSharp.Control

[<RequireQualifiedAccess>]
module FAErr =
    type DirectoryDeleteError =
        | NotFound of dirPath: string * ex: Exception option
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
    
    type EnumerateFilesWithBlockError<'BlockError when 'BlockError :> IError> =
        | DirectoryNotFound of ex: Exception option
        | IOError of ex: Exception option
        | PathTooLong of ex: Exception option
        | SecurityError of ex: Exception option
        | UnauthorizedAccess of ex: Exception option
        | BlockError of blockError: 'BlockError
        | Unknown of ex: Exception option
        
        interface IError with
            member this.Exception =
                match this with
                | DirectoryNotFound ex -> ex
                | IOError ex -> ex
                | PathTooLong ex -> ex
                | SecurityError ex -> ex
                | UnauthorizedAccess ex -> ex
                | BlockError err -> err.Exception
                | Unknown ex -> ex
                
            member this.ToString() =
                match this with
                | DirectoryNotFound _ -> "Directory is not found"
                | IOError _ -> "IO Error"
                | PathTooLong _ -> "Path too long"
                | SecurityError _ -> "Security error"
                | UnauthorizedAccess _ -> "Unauthorized access"
                | BlockError error -> error.ToString()
                | Unknown ex ->
                    match ex with
                    | Some ex -> ex.Message
                    | None -> "Unknown error"
    type EnumerateFilesError =
        | DirectoryNotFound of ex: Exception option
        | IOError of ex: Exception option
        | PathTooLong of ex: Exception option
        | SecurityError of ex: Exception option
        | UnauthorizedAccess of ex: Exception option
        | Unknown of ex: Exception option
        
        interface IError with
            member this.Exception =
                match this with
                | DirectoryNotFound ex -> ex
                | IOError ex -> ex
                | PathTooLong ex -> ex
                | SecurityError ex -> ex
                | UnauthorizedAccess ex -> ex
                | Unknown ex -> ex
                
            member this.ToString() =
                match this with
                | DirectoryNotFound _ -> "Directory is not found"
                | IOError _ -> "IO Error"
                | PathTooLong _ -> "Path too long"
                | SecurityError _ -> "Security error"
                | UnauthorizedAccess _ -> "Unauthorized access"
                | Unknown ex ->
                    match ex with
                    | Some ex -> ex.Message
                    | None -> "Unknown error"
                    
    type EnumerateDirectoriesWithBlockError<'BlockError when 'BlockError :> IError> =
        | DirectoryNotFound of ex: Exception option
        | IOError of ex: Exception option
        | PathTooLong of ex: Exception option
        | SecurityError of ex: Exception option
        | UnauthorizedAccess of ex: Exception option
        | BlockError of blockError: 'BlockError
        | Unknown of ex: Exception option
        
        interface IError with
            member this.Exception =
                match this with
                | DirectoryNotFound ex -> ex
                | IOError ex -> ex
                | PathTooLong ex -> ex
                | SecurityError ex -> ex
                | UnauthorizedAccess ex -> ex
                | BlockError err -> err.Exception
                | Unknown ex -> ex
                
            member this.ToString() =
                match this with
                | DirectoryNotFound _ -> "Directory is not found"
                | IOError _ -> "IO Error"
                | PathTooLong _ -> "Path too long"
                | SecurityError _ -> "Security error"
                | UnauthorizedAccess _ -> "Unauthorized access"
                | BlockError error -> error.ToString()
                | Unknown ex ->
                    match ex with
                    | Some ex -> ex.Message
                    | None -> "Unknown error"
        
    type EnumerateDirectoriesError =
        | DirectoryNotFound of ex: Exception option
        | IOError of ex: Exception option
        | PathTooLong of ex: Exception option
        | SecurityError of ex: Exception option
        | UnauthorizedAccess of ex: Exception option
        | Unknown of ex: Exception option
        
        interface IError with
            member this.Exception =
                match this with
                | DirectoryNotFound ex -> ex
                | IOError ex -> ex
                | PathTooLong ex -> ex
                | SecurityError ex -> ex
                | UnauthorizedAccess ex -> ex
                | Unknown ex -> ex
                
            member this.ToString() =
                match this with
                | DirectoryNotFound _ -> "Directory is not found"
                | IOError _ -> "IO Error"
                | PathTooLong _ -> "Path too long"
                | SecurityError _ -> "Security error"
                | UnauthorizedAccess _ -> "Unauthorized access"
                | Unknown ex ->
                    match ex with
                    | Some ex -> ex.Message
                    | None -> "Unknown error"
    
    type GetDirectoriesError<'BlockError when 'BlockError :> IError> =
        | DirectoryNotFound of ex: Exception option
        | IOError of ex: Exception option
        | PathTooLong of ex: Exception option
        | SecurityError of ex: Exception option
        | UnauthorizedAccess of ex: Exception option
        | Unknown of ex: Exception option
        
        interface IError with
            member this.Exception =
                match this with
                | DirectoryNotFound ex -> ex
                | IOError ex -> ex
                | PathTooLong ex -> ex
                | SecurityError ex -> ex
                | UnauthorizedAccess ex -> ex
                | Unknown ex -> ex
                
            member this.ToString() =
                match this with
                | DirectoryNotFound _ -> "Directory is not found"
                | IOError _ -> "IO Error"
                | PathTooLong _ -> "Path too long"
                | SecurityError _ -> "Security error"
                | UnauthorizedAccess _ -> "Unauthorized access"
                | Unknown ex ->
                    match ex with
                    | Some ex -> ex.Message
                    | None -> "Unknown error"

type FAEx =
    static member Delete dirPath =
        try
            Directory.Delete(dirPath)
            Ok()
        with
        | :? DirectoryNotFoundException as ex -> Error(FAErr.DirectoryDeleteError.NotFound(dirPath, Some ex))
        | ex -> Error(FAErr.DirectoryDeleteError.Unknown(Some ex))
    
    static member DeleteAsync dirPath =
        task {
            try
                do! BlockingTask.Run(fun () -> Directory.Delete(dirPath))
                return Ok()
            with
            | :? DirectoryNotFoundException as ex -> return Error(FAErr.DirectoryDeleteError.NotFound(dirPath, Some ex))
            | ex -> return Error(FAErr.DirectoryDeleteError.Unknown(Some ex))
        }
        
    static member private safeEnumerateFileBlock(block: unit -> 'Ok): Result<'Ok, FAErr.EnumerateFilesError> =
        try
            Ok(block())
        with
        | :? DirectoryNotFoundException as ex -> Error(FAErr.EnumerateFilesError.DirectoryNotFound(Some ex))
        | :? PathTooLongException as ex -> Error(FAErr.EnumerateFilesError.PathTooLong(Some ex))
        | :? IOException as ex -> Error(FAErr.EnumerateFilesError.IOError(Some ex))
        | :? SecurityException as ex -> Error(FAErr.EnumerateFilesError.SecurityError(Some ex))
        | :? UnauthorizedAccessException as ex -> Error(FAErr.EnumerateFilesError.UnauthorizedAccess(Some ex))
        | ex -> Error(FAErr.EnumerateFilesError.Unknown(Some ex))

    static member private enumerateNextFileWithBlock(
                                  enumerator: IEnumerator<string>,
                                  block: string -> Result<'BlockOk, 'BlockError>
                              ): Result<unit, FAErr.EnumerateFilesWithBlockError<'BlockError>> =
        
        match FAEx.safeEnumerateFileBlock(fun () -> enumerator.MoveNext()) with
         //enumeration ends when move next returns false
        | Ok false -> Ok()
        
        | Ok true ->
            match block(enumerator.Current) with
            | Ok _ -> FAEx.enumerateNextFileWithBlock(enumerator, block)
            | Error blockError -> Error(FAErr.EnumerateFilesWithBlockError.BlockError(blockError))
        | Error(FAErr.EnumerateFilesError.DirectoryNotFound(ex)) ->
            Error(FAErr.EnumerateFilesWithBlockError.DirectoryNotFound(ex))
        | Error(FAErr.EnumerateFilesError.IOError(ex)) ->
            Error(FAErr.EnumerateFilesWithBlockError.IOError(ex))
        | Error(FAErr.EnumerateFilesError.PathTooLong(ex)) ->
            Error(FAErr.EnumerateFilesWithBlockError.PathTooLong(ex))
        | Error(FAErr.EnumerateFilesError.SecurityError(ex)) ->
            Error(FAErr.EnumerateFilesWithBlockError.SecurityError(ex))
        | Error(FAErr.EnumerateFilesError.UnauthorizedAccess(ex)) ->
            Error(FAErr.EnumerateFilesWithBlockError.UnauthorizedAccess(ex))
        | Error(FAErr.EnumerateFilesError.Unknown(ex)) ->
            Error(FAErr.EnumerateFilesWithBlockError.Unknown(ex))

    static member EnumerateFilesWithBlock(srcPath, pattern, block: string -> Result<unit, 'BlockError>) =
        let enumeration = FAEx.safeEnumerateFileBlock(fun () -> Directory.EnumerateFiles(srcPath, pattern))
        
        match enumeration with
        | Ok enumeration ->
            FAEx.enumerateNextFileWithBlock(enumeration.GetEnumerator(), block)
        | Error(FAErr.EnumerateFilesError.DirectoryNotFound(ex)) ->
            Error(FAErr.EnumerateFilesWithBlockError.DirectoryNotFound(ex))
        | Error(FAErr.EnumerateFilesError.IOError(ex)) ->
            Error(FAErr.EnumerateFilesWithBlockError.IOError(ex))
        | Error(FAErr.EnumerateFilesError.PathTooLong(ex)) ->
            Error(FAErr.EnumerateFilesWithBlockError.PathTooLong(ex))
        | Error(FAErr.EnumerateFilesError.SecurityError(ex)) ->
            Error(FAErr.EnumerateFilesWithBlockError.SecurityError(ex))
        | Error(FAErr.EnumerateFilesError.UnauthorizedAccess(ex)) ->
            Error(FAErr.EnumerateFilesWithBlockError.UnauthorizedAccess(ex))
        | Error(FAErr.EnumerateFilesError.Unknown(ex)) ->
            Error(FAErr.EnumerateFilesWithBlockError.Unknown(ex))

    static member private safeEnumerateDirectoryBlock(block: unit -> 'Ok): Result<'Ok, FAErr.EnumerateDirectoriesError> =
        try
            Ok(block())
        with
        | :? DirectoryNotFoundException as ex -> Error(FAErr.EnumerateDirectoriesError.DirectoryNotFound(Some ex))
        | :? PathTooLongException as ex -> Error(FAErr.EnumerateDirectoriesError.PathTooLong(Some ex))
        | :? IOException as ex -> Error(FAErr.EnumerateDirectoriesError.IOError(Some ex))
        | :? SecurityException as ex -> Error(FAErr.EnumerateDirectoriesError.SecurityError(Some ex))
        | :? UnauthorizedAccessException as ex -> Error(FAErr.EnumerateDirectoriesError.UnauthorizedAccess(Some ex))
        | ex -> Error(FAErr.EnumerateDirectoriesError.Unknown(Some ex))

    static member private enumerateNextDirectoryWithBlock(enumerator: IEnumerator<string>,
                              block: string -> Result<'BlockOk, 'BlockError>): Result<unit, FAErr.EnumerateDirectoriesWithBlockError<'BlockError>> =
        
       
        match FAEx.safeEnumerateDirectoryBlock(fun () -> enumerator.MoveNext()) with
        //enumeration ends when move next returns false
        | Ok false -> Ok()
        
        | Ok true ->
            match block(enumerator.Current) with
            | Ok _ -> FAEx.enumerateNextDirectoryWithBlock(enumerator, block)
            | Error blockError -> Error(FAErr.EnumerateDirectoriesWithBlockError.BlockError(blockError))
        | Error(FAErr.EnumerateDirectoriesError.DirectoryNotFound(ex)) ->
            Error(FAErr.EnumerateDirectoriesWithBlockError.DirectoryNotFound(ex))
        | Error(FAErr.EnumerateDirectoriesError.IOError(ex)) ->
            Error(FAErr.EnumerateDirectoriesWithBlockError.IOError(ex))
        | Error(FAErr.EnumerateDirectoriesError.PathTooLong(ex)) ->
            Error(FAErr.EnumerateDirectoriesWithBlockError.PathTooLong(ex))
        | Error(FAErr.EnumerateDirectoriesError.SecurityError(ex)) ->
            Error(FAErr.EnumerateDirectoriesWithBlockError.SecurityError(ex))
        | Error(FAErr.EnumerateDirectoriesError.UnauthorizedAccess(ex)) ->
            Error(FAErr.EnumerateDirectoriesWithBlockError.UnauthorizedAccess(ex))
        | Error(FAErr.EnumerateDirectoriesError.Unknown(ex)) ->
            Error(FAErr.EnumerateDirectoriesWithBlockError.Unknown(ex))

    static member EnumerateDirectoriesWithBlock(srcPath, pattern, block: string -> Result<unit, 'BlockError>) =
        
        let enumeration = FAEx.safeEnumerateDirectoryBlock(fun () -> Directory.EnumerateDirectories(srcPath, pattern))
        
        match enumeration with
        | Ok enumeration ->
            FAEx.enumerateNextDirectoryWithBlock(enumeration.GetEnumerator(), block)
        | Error(FAErr.EnumerateDirectoriesError.DirectoryNotFound(ex)) ->
            Error(FAErr.EnumerateDirectoriesWithBlockError.DirectoryNotFound(ex))
        | Error(FAErr.EnumerateDirectoriesError.IOError(ex)) ->
            Error(FAErr.EnumerateDirectoriesWithBlockError.IOError(ex))
        | Error(FAErr.EnumerateDirectoriesError.PathTooLong(ex)) ->
            Error(FAErr.EnumerateDirectoriesWithBlockError.PathTooLong(ex))
        | Error(FAErr.EnumerateDirectoriesError.SecurityError(ex)) ->
            Error(FAErr.EnumerateDirectoriesWithBlockError.SecurityError(ex))
        | Error(FAErr.EnumerateDirectoriesError.UnauthorizedAccess(ex)) ->
            Error(FAErr.EnumerateDirectoriesWithBlockError.UnauthorizedAccess(ex))
        | Error(FAErr.EnumerateDirectoriesError.Unknown(ex)) ->
            Error(FAErr.EnumerateDirectoriesWithBlockError.Unknown(ex))
     
    static member EnumerateDirectories(srcPath, pattern) =
        seq {
            let enumeration = FAEx.safeEnumerateDirectoryBlock(fun () -> Directory.EnumerateDirectories(srcPath, pattern))
            
            match enumeration with
            | Ok enumeration ->
                let enumerator = enumeration.GetEnumerator()
                
                let mutable moveNextExists = true
                
                while moveNextExists do
                    match FAEx.safeEnumerateDirectoryBlock(fun () -> enumerator.MoveNext()) with
                    | Ok true -> yield Ok enumerator.Current
                    | Ok false -> moveNextExists <- false
                    | Error error ->
                        yield Error error
                        moveNextExists <- false
                
            | Error error -> yield Error error
        }
            
    static member EnumerateFiles(srcPath, pattern) =
        seq {
            let enumeration = FAEx.safeEnumerateFileBlock(fun () -> Directory.EnumerateFiles(srcPath, pattern))
            
            match enumeration with
            | Ok enumeration ->
                let enumerator = enumeration.GetEnumerator()
                
                let mutable moveNextExists = true
                
                while moveNextExists do
                    match FAEx.safeEnumerateFileBlock(fun () -> enumerator.MoveNext()) with
                    | Ok true -> yield Ok enumerator.Current
                    | Ok false -> moveNextExists <- false
                    | Error error ->
                        yield Error error
                        moveNextExists <- false
                
            | Error error -> yield Error error
        }

    static member EnumerateFilesAsync(srcPath, pattern) =
        seq {
            let mutable moveNextExists = true
            let mutable enumerator: IEnumerator<string> = null
            
            let enumeration =
                task {
                    let! enumeration =
                        BlockingTask.Run(fun () ->
                            FAEx.safeEnumerateFileBlock(fun () ->
                                Directory.EnumerateFiles(srcPath, pattern)
                            )
                        )
                                         
                    match enumeration with
                    | Ok enumeration ->
                        enumerator <- enumeration.GetEnumerator()
                        
                        let! moveNextRes =
                            BlockingTask.Run(fun () -> FAEx.safeEnumerateFileBlock(fun () -> enumerator.MoveNext()))
                        
                        match moveNextRes with
                        | Ok true -> return Ok(Some enumerator.Current)
                        | Ok false ->
                            moveNextExists <- false
                            return Ok None
                        | Error error ->
                            moveNextExists <- false
                            return Error error
                        
                    | Error error ->
                        moveNextExists <- false
                        return Error error
                }
            
            yield enumeration
            
            let mutable lastTaskExecuted = true
            
            while moveNextExists do
                if not lastTaskExecuted then
                    failwith "Previous task is not executed. Please, wait task before iterate next element"
                
                let enumeration =
                    task {
                        lastTaskExecuted <- true
                        
                        let! moveNextRes =
                                BlockingTask.Run(fun () -> FAEx.safeEnumerateFileBlock(fun () -> enumerator.MoveNext()))
                        
                        match moveNextRes with
                        | Ok true -> return Ok(Some enumerator.Current)
                        | Ok false ->
                            moveNextExists <- false
                            return Ok None
                        | Error error ->
                            moveNextExists <- false
                            return Error error
                    }
                
                lastTaskExecuted <- false
                yield enumeration
        }
    
    // Returns the names of subdirectories (including their paths) in the specified directory.
    static member GetDirectories(srcPath, pattern) =
        let mutable dirList = []
        
        let res = 
            FAEx.EnumerateDirectoriesWithBlock(
                srcPath,
                pattern,
                (fun file ->
                    dirList <- file :: dirList
                    Ok()
                )
            )
        
        match res with
        | Error(FAErr.EnumerateDirectoriesWithBlockError.DirectoryNotFound(ex)) ->
            Error(FAErr.GetDirectoriesError.DirectoryNotFound(ex))
        | Error(FAErr.EnumerateDirectoriesWithBlockError.IOError(ex)) ->
            Error(FAErr.GetDirectoriesError.IOError(ex))
        | Error(FAErr.EnumerateDirectoriesWithBlockError.PathTooLong(ex)) ->
            Error(FAErr.GetDirectoriesError.PathTooLong(ex))
        | Error(FAErr.EnumerateDirectoriesWithBlockError.SecurityError(ex)) ->
            Error(FAErr.GetDirectoriesError.SecurityError(ex))
        | Error(FAErr.EnumerateDirectoriesWithBlockError.UnauthorizedAccess(ex)) ->
            Error(FAErr.GetDirectoriesError.UnauthorizedAccess(ex))
        | Error(FAErr.EnumerateDirectoriesWithBlockError.Unknown(ex)) ->
            Error(FAErr.GetDirectoriesError.Unknown(ex))
        | Error(FAErr.EnumerateDirectoriesWithBlockError.BlockError _) ->
            failwith "Block error is not realized"
        | Ok _ -> Ok(dirList |> List.toArray)
