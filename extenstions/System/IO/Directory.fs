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
module FAEx =
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

    let public Delete dirPath =
        
        try
            Directory.Delete(dirPath)
            Ok()
        with
        | :? DirectoryNotFoundException as ex -> Error(NotFound(dirPath, Some ex))
        | ex -> Error(Unknown(Some ex))
    
    let public DeleteAsync dirPath =
        BlockingTask.Run(fun () -> Delete(dirPath))

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
        
    let private safeEnumerateFileBlock(block: unit -> 'Ok): Result<'Ok, EnumerateFilesError> =
        try
            Ok(block())
        with
        | :? DirectoryNotFoundException as ex -> Error(DirectoryNotFound(Some ex))
        | :? PathTooLongException as ex -> Error(PathTooLong(Some ex))
        | :? IOException as ex -> Error(IOError(Some ex))
        | :? SecurityException as ex -> Error(SecurityError(Some ex))
        | :? UnauthorizedAccessException as ex -> Error(UnauthorizedAccess(Some ex))
        | ex -> Error(Unknown(Some ex))

    let rec private enumerateNextFileWithBlock(enumerator: IEnumerator<string>,
                              block: string -> Result<'BlockOk, 'BlockError>): Result<unit, EnumerateFilesWithBlockError<'BlockError>> =
        
        match safeEnumerateFileBlock(fun () -> enumerator.MoveNext()) with
         //enumeration ends when move next returns false
        | Ok false -> Ok()
        
        | Ok true ->
            match block(enumerator.Current) with
            | Ok _ -> enumerateNextFileWithBlock(enumerator, block)
            | Error blockError -> Error(BlockError(blockError))
        | Error(EnumerateFilesError.DirectoryNotFound(ex)) ->
            Error(EnumerateFilesWithBlockError.DirectoryNotFound(ex))
        | Error(EnumerateFilesError.IOError(ex)) ->
            Error(EnumerateFilesWithBlockError.IOError(ex))
        | Error(EnumerateFilesError.PathTooLong(ex)) ->
            Error(EnumerateFilesWithBlockError.PathTooLong(ex))
        | Error(EnumerateFilesError.SecurityError(ex)) ->
            Error(EnumerateFilesWithBlockError.SecurityError(ex))
        | Error(EnumerateFilesError.UnauthorizedAccess(ex)) ->
            Error(EnumerateFilesWithBlockError.UnauthorizedAccess(ex))
        | Error(EnumerateFilesError.Unknown(ex)) ->
            Error(EnumerateFilesWithBlockError.Unknown(ex))

    let public EnumerateFilesWithBlock(srcPath, pattern, block: string -> Result<unit, 'BlockError>) =
        let enumeration = safeEnumerateFileBlock(fun () -> Directory.EnumerateFiles(srcPath, pattern))
        
        match enumeration with
        | Ok enumeration ->
            enumerateNextFileWithBlock(enumeration.GetEnumerator(), block)
        | Error(EnumerateFilesError.DirectoryNotFound(ex)) ->
            Error(EnumerateFilesWithBlockError.DirectoryNotFound(ex))
        | Error(EnumerateFilesError.IOError(ex)) ->
            Error(EnumerateFilesWithBlockError.IOError(ex))
        | Error(EnumerateFilesError.PathTooLong(ex)) ->
            Error(EnumerateFilesWithBlockError.PathTooLong(ex))
        | Error(EnumerateFilesError.SecurityError(ex)) ->
            Error(EnumerateFilesWithBlockError.SecurityError(ex))
        | Error(EnumerateFilesError.UnauthorizedAccess(ex)) ->
            Error(EnumerateFilesWithBlockError.UnauthorizedAccess(ex))
        | Error(EnumerateFilesError.Unknown(ex)) ->
            Error(EnumerateFilesWithBlockError.Unknown(ex))

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
        
    let private safeEnumerateDirectoryBlock(block: unit -> 'Ok): Result<'Ok, EnumerateDirectoriesError> =
        try
            Ok(block())
        with
        | :? DirectoryNotFoundException as ex -> Error(EnumerateDirectoriesError.DirectoryNotFound(Some ex))
        | :? PathTooLongException as ex -> Error(EnumerateDirectoriesError.PathTooLong(Some ex))
        | :? IOException as ex -> Error(EnumerateDirectoriesError.IOError(Some ex))
        | :? SecurityException as ex -> Error(EnumerateDirectoriesError.SecurityError(Some ex))
        | :? UnauthorizedAccessException as ex -> Error(EnumerateDirectoriesError.UnauthorizedAccess(Some ex))
        | ex -> Error(EnumerateDirectoriesError.Unknown(Some ex))

    let rec private enumerateNextDirectoryWithBlock(enumerator: IEnumerator<string>,
                              block: string -> Result<'BlockOk, 'BlockError>): Result<unit, EnumerateDirectoriesWithBlockError<'BlockError>> =
        
       
        match safeEnumerateDirectoryBlock(fun () -> enumerator.MoveNext()) with
        //enumeration ends when move next returns false
        | Ok false -> Ok()
        
        | Ok true ->
            match block(enumerator.Current) with
            | Ok _ -> enumerateNextDirectoryWithBlock(enumerator, block)
            | Error blockError -> Error(BlockError(blockError))
        | Error(EnumerateDirectoriesError.DirectoryNotFound(ex)) ->
            Error(EnumerateDirectoriesWithBlockError.DirectoryNotFound(ex))
        | Error(EnumerateDirectoriesError.IOError(ex)) ->
            Error(EnumerateDirectoriesWithBlockError.IOError(ex))
        | Error(EnumerateDirectoriesError.PathTooLong(ex)) ->
            Error(EnumerateDirectoriesWithBlockError.PathTooLong(ex))
        | Error(EnumerateDirectoriesError.SecurityError(ex)) ->
            Error(EnumerateDirectoriesWithBlockError.SecurityError(ex))
        | Error(EnumerateDirectoriesError.UnauthorizedAccess(ex)) ->
            Error(EnumerateDirectoriesWithBlockError.UnauthorizedAccess(ex))
        | Error(EnumerateDirectoriesError.Unknown(ex)) ->
            Error(EnumerateDirectoriesWithBlockError.Unknown(ex))

    let public EnumerateDirectoriesWithBlock(srcPath, pattern, block: string -> Result<unit, 'BlockError>) =
        
        let enumeration = safeEnumerateDirectoryBlock(fun () -> Directory.EnumerateDirectories(srcPath, pattern))
        
        match enumeration with
        | Ok enumeration ->
            enumerateNextDirectoryWithBlock(enumeration.GetEnumerator(), block)
        | Error(EnumerateDirectoriesError.DirectoryNotFound(ex)) ->
            Error(EnumerateDirectoriesWithBlockError.DirectoryNotFound(ex))
        | Error(EnumerateDirectoriesError.IOError(ex)) ->
            Error(EnumerateDirectoriesWithBlockError.IOError(ex))
        | Error(EnumerateDirectoriesError.PathTooLong(ex)) ->
            Error(EnumerateDirectoriesWithBlockError.PathTooLong(ex))
        | Error(EnumerateDirectoriesError.SecurityError(ex)) ->
            Error(EnumerateDirectoriesWithBlockError.SecurityError(ex))
        | Error(EnumerateDirectoriesError.UnauthorizedAccess(ex)) ->
            Error(EnumerateDirectoriesWithBlockError.UnauthorizedAccess(ex))
        | Error(EnumerateDirectoriesError.Unknown(ex)) ->
            Error(EnumerateDirectoriesWithBlockError.Unknown(ex))

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
                
    let public EnumerateDirectories(srcPath, pattern) =
        seq {
            let enumeration = safeEnumerateDirectoryBlock(fun () -> Directory.EnumerateDirectories(srcPath, pattern))
            
            match enumeration with
            | Ok enumeration ->
                let enumerator = enumeration.GetEnumerator()
                
                let mutable moveNextExists = true
                
                while moveNextExists do
                    match safeEnumerateDirectoryBlock(fun () -> enumerator.MoveNext()) with
                    | Ok true -> yield Ok enumerator.Current
                    | Ok false -> moveNextExists <- false
                    | Error error ->
                        yield Error error
                        moveNextExists <- false
                
            | Error error -> yield Error error
        }
            
    let public EnumerateFiles(srcPath, pattern) =
        seq {
            let enumeration = safeEnumerateFileBlock(fun () -> Directory.EnumerateFiles(srcPath, pattern))
            
            match enumeration with
            | Ok enumeration ->
                let enumerator = enumeration.GetEnumerator()
                
                let mutable moveNextExists = true
                
                while moveNextExists do
                    match safeEnumerateFileBlock(fun () -> enumerator.MoveNext()) with
                    | Ok true -> yield Ok enumerator.Current
                    | Ok false -> moveNextExists <- false
                    | Error error ->
                        yield Error error
                        moveNextExists <- false
                
            | Error error -> yield Error error
        }

    let public EnumerateFilesAsync(srcPath, pattern) =
        seq {
            let mutable moveNextExists = true
            let mutable enumerator: IEnumerator<string> = null
            
            let enumeration =
                task {
                    let! enumeration =
                        BlockingTask.Run(fun () ->
                            safeEnumerateFileBlock(fun () ->
                                Directory.EnumerateFiles(srcPath, pattern)
                            )
                        )
                                         
                    match enumeration with
                    | Ok enumeration ->
                        enumerator <- enumeration.GetEnumerator()
                        
                        let! moveNextRes =
                            BlockingTask.Run(fun () -> safeEnumerateFileBlock(fun () -> enumerator.MoveNext()))
                        
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
                                BlockingTask.Run(fun () -> safeEnumerateFileBlock(fun () -> enumerator.MoveNext()))
                        
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
    let public GetDirectories(srcPath, pattern) =
        let mutable dirList = []
        
        let res = 
            EnumerateDirectoriesWithBlock(
                srcPath,
                pattern,
                (fun file ->
                    dirList <- file :: dirList
                    Ok()
                )
            )
        
        match res with
        | Error(EnumerateDirectoriesWithBlockError.DirectoryNotFound(ex)) ->
            Error(GetDirectoriesError.DirectoryNotFound(ex))
        | Error(EnumerateDirectoriesWithBlockError.IOError(ex)) ->
            Error(GetDirectoriesError.IOError(ex))
        | Error(EnumerateDirectoriesWithBlockError.PathTooLong(ex)) ->
            Error(GetDirectoriesError.PathTooLong(ex))
        | Error(EnumerateDirectoriesWithBlockError.SecurityError(ex)) ->
            Error(GetDirectoriesError.SecurityError(ex))
        | Error(EnumerateDirectoriesWithBlockError.UnauthorizedAccess(ex)) ->
            Error(GetDirectoriesError.UnauthorizedAccess(ex))
        | Error(EnumerateDirectoriesWithBlockError.Unknown(ex)) ->
            Error(GetDirectoriesError.Unknown(ex))
        | Error(EnumerateDirectoriesWithBlockError.BlockError _) ->
            failwith "Block error is not realized"
        | Ok _ -> Ok(dirList |> List.toArray)
