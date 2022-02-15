module FAUtils.FileUtils
                
open System
open System.IO
open FAUtils
open FAUtils.ErrorManagement

type DeleteFilesInDirectoryError =
    | DirectoryNotFound of dirPath: string * ex: Exception option
    | FileNotFound of filePath: string * ex: Exception option
    | IOError of ex: Exception option
    | UnauthorizedAccess of path: string * ex: Exception option
    | PathTooLong of path: string * ex: Exception option
    | SecurityError of path: string * ex: Exception option
    | Unknown of ex: Exception option
     
    interface IError with
        member this.ToString() =
            match this with
            | DirectoryNotFound(dirPath, _) -> $"Directory {dirPath} is not found"
            | FileNotFound(filePath, _) -> $"File {filePath} is not found"
            | PathTooLong(path, _) -> $"Path {path} is too long"
            | SecurityError(path, _) -> $"Security error occured on {path}"
            | UnauthorizedAccess(path, _) -> $"Unauthorized access occured on {path}"
            | IOError _ -> $"Input Output error occured"
            | Unknown ex ->
                match ex with
                | Some ex -> ex.Message
                | None -> "Unknown error"
        
        member this.Exception with get() =
            match this with
            | DirectoryNotFound(_, ex) -> ex
            | FileNotFound(_, ex) -> ex
            | PathTooLong(_, ex) -> ex
            | SecurityError(_, ex) -> ex
            | UnauthorizedAccess(_, ex) -> ex
            | IOError ex -> ex
            | Unknown(ex) -> ex

let rec public DeleteFilesInDirectory(srcPath, pattern) =
    let enumerationFilesResult = Directory.FAEx.EnumerateFilesWithBlock(srcPath, pattern, (fun file ->
            File.FAEx.Delete(file)
        ))
    
    match enumerationFilesResult with
    | Error(Directory.FAErr.DirectoryNotFound(ex)) -> Error(DirectoryNotFound(srcPath, ex))
    | Error(Directory.FAErr.IOError(ex)) -> Error(IOError(ex))
    | Error(Directory.FAErr.PathTooLong(ex)) -> Error(PathTooLong(srcPath, ex))
    | Error(Directory.FAErr.SecurityError(ex)) -> Error(SecurityError(srcPath, ex))
    | Error(Directory.FAErr.UnauthorizedAccess(ex)) -> Error(UnauthorizedAccess(srcPath, ex))
    | Error(Directory.FAErr.EnumerateFilesWithBlockError.Unknown(ex)) -> Error(Unknown(ex))
    | Error(Directory.FAErr.BlockError(error)) -> 
        match error with
            | File.FAEx.FileDeleteError.NotFound(file, ex) -> Error(FileNotFound(file, ex))
            | File.FAEx.FileDeleteError.Unknown ex -> Error(Unknown(ex))
    | Ok _ -> Ok()

type DeleteDirectoriesInDirectoryError =
    | DirectoryNotFound of dirPath: string * ex: Exception option
    | FileNotFound of filePath: string * ex: Exception option
    | IOError of ex: Exception option
    | UnauthorizedAccess of path: string * ex: Exception option
    | PathTooLong of path: string * ex: Exception option
    | SecurityError of path: string * ex: Exception option
    | Unknown of ex: Exception option
     
    interface IError with
        member this.ToString() =
            match this with
            | DirectoryNotFound(dirPath, _) -> $"Directory {dirPath} is not found"
            | FileNotFound(filePath, _) -> $"File {filePath} is not found"
            | PathTooLong(path, _) -> $"Path {path} is too long"
            | SecurityError(path, _) -> $"Security error occured on {path}"
            | UnauthorizedAccess(path, _) -> $"Unauthorized access occured on {path}"
            | IOError _ -> $"Input Output error occured"
            | Unknown ex ->
                match ex with
                | Some ex -> ex.Message
                | None -> "Unknown error"
        
        member this.Exception with get() =
            match this with
            | DirectoryNotFound(_, ex) -> ex
            | FileNotFound(_, ex) -> ex
            | PathTooLong(_, ex) -> ex
            | SecurityError(_, ex) -> ex
            | UnauthorizedAccess(_, ex) -> ex
            | IOError ex -> ex
            | Unknown(ex) -> ex

let rec public DeleteDirectoriesInDirectory(srcPath, pattern, deleteFilesInDirectories, deleteSubDirs) =
    let enumerationDirectoriesResult = Directory.FAEx.EnumerateDirectoriesWithBlock(srcPath, pattern, (fun dir ->
            if deleteFilesInDirectories then
                match DeleteFilesInDirectory(dir, "*") with
                | Error(DeleteFilesInDirectoryError.DirectoryNotFound(srcPath, ex)) ->
                    Error(DeleteDirectoriesInDirectoryError.DirectoryNotFound(srcPath, ex))
                | Error(DeleteFilesInDirectoryError.IOError(ex)) ->
                    Error(DeleteDirectoriesInDirectoryError.IOError(ex))
                | Error(DeleteFilesInDirectoryError.PathTooLong(srcPath, ex)) ->
                    Error(DeleteDirectoriesInDirectoryError.PathTooLong(srcPath, ex))
                | Error(DeleteFilesInDirectoryError.SecurityError(srcPath, ex)) ->
                    Error(DeleteDirectoriesInDirectoryError.SecurityError(srcPath, ex))
                | Error(DeleteFilesInDirectoryError.UnauthorizedAccess(srcPath, ex)) ->
                    Error(DeleteDirectoriesInDirectoryError.UnauthorizedAccess(srcPath, ex))
                | Error(DeleteFilesInDirectoryError.FileNotFound(file, ex)) ->
                    Error(DeleteDirectoriesInDirectoryError.FileNotFound(file, ex))
                | Error(DeleteFilesInDirectoryError.Unknown ex) ->
                    Error(DeleteDirectoriesInDirectoryError.Unknown ex)
                | Ok _ -> 
            
                    if deleteSubDirs then
                        match DeleteDirectoriesInDirectory(dir, "*", deleteFilesInDirectories, deleteSubDirs) with
                        | Error error -> Error error
                        | Ok _ ->
                            
                            match Directory.FAEx.Delete(dir) with
                            | Error(Directory.FAErr.DirectoryDeleteError.NotFound(tempPath, ex)) ->
                                Error(DeleteDirectoriesInDirectoryError.DirectoryNotFound(tempPath, ex))
                            | Error(Directory.FAErr.DirectoryDeleteError.Unknown ex) ->
                                Error(DeleteDirectoriesInDirectoryError.Unknown ex)
                            | Ok _ ->
                                Ok()
                    else
                        Ok()
            else 
                Ok()
    ))
    
    match enumerationDirectoriesResult with
    | Error(Directory.FAErr.EnumerateDirectoriesWithBlockError.DirectoryNotFound(ex)) ->
        Error(DeleteDirectoriesInDirectoryError.DirectoryNotFound(srcPath, ex))
    | Error(Directory.FAErr.EnumerateDirectoriesWithBlockError.IOError(ex)) ->
        Error(DeleteDirectoriesInDirectoryError.IOError(ex))
    | Error(Directory.FAErr.EnumerateDirectoriesWithBlockError.PathTooLong(ex)) ->
        Error(DeleteDirectoriesInDirectoryError.PathTooLong(srcPath, ex))
    | Error(Directory.FAErr.EnumerateDirectoriesWithBlockError.SecurityError(ex)) ->
        Error(DeleteDirectoriesInDirectoryError.SecurityError(srcPath, ex))
    | Error(Directory.FAErr.EnumerateDirectoriesWithBlockError.UnauthorizedAccess(ex)) ->
        Error(DeleteDirectoriesInDirectoryError.UnauthorizedAccess(srcPath, ex))
    | Error(Directory.FAErr.EnumerateDirectoriesWithBlockError.Unknown(ex)) ->
        Error(DeleteDirectoriesInDirectoryError.Unknown(ex))
    | Error(Directory.FAErr.EnumerateDirectoriesWithBlockError.BlockError(error)) ->
        Error error
    | Ok _ -> Ok()

type DeleteEntriesFromDirectory =
    | DirectoryNotFound of dirPath: string * ex: Exception option
    | FileNotFound of filePath: string * ex: Exception option
    | IOError of ex: Exception option
    | UnauthorizedAccess of path: string * ex: Exception option
    | PathTooLong of path: string * ex: Exception option
    | SecurityError of path: string * ex: Exception option
    | Unknown of ex: Exception option
     
    interface IError with
        member this.ToString() =
            match this with
            | DirectoryNotFound(dirPath, _) -> $"Directory {dirPath} is not found"
            | FileNotFound(filePath, _) -> $"File {filePath} is not found"
            | PathTooLong(path, _) -> $"Path {path} is too long"
            | SecurityError(path, _) -> $"Security error occured on {path}"
            | UnauthorizedAccess(path, _) -> $"Unauthorized access occured on {path}"
            | IOError _ -> $"Input Output error occured"
            | Unknown ex ->
                match ex with
                | Some ex -> ex.Message
                | None -> "Unknown error"
        
        member this.Exception with get() =
            match this with
            | DirectoryNotFound(_, ex) -> ex
            | FileNotFound(_, ex) -> ex
            | PathTooLong(_, ex) -> ex
            | SecurityError(_, ex) -> ex
            | UnauthorizedAccess(_, ex) -> ex
            | IOError ex -> ex
            | Unknown(ex) -> ex

let public DeleteEntriesFromDirectory(srcPath, pattern) =
    match DeleteFilesInDirectory(srcPath, pattern) with
    | Error(DeleteFilesInDirectoryError.DirectoryNotFound(dirPath, ex)) ->
        Error(DirectoryNotFound(dirPath, ex))
    | Error(DeleteFilesInDirectoryError.IOError(ex)) ->
        Error(IOError(ex))
    | Error(DeleteFilesInDirectoryError.PathTooLong(srcPath, ex)) ->
        Error(PathTooLong(srcPath, ex))
    | Error(DeleteFilesInDirectoryError.SecurityError(srcPath, ex)) ->
        Error(SecurityError(srcPath, ex))
    | Error(DeleteFilesInDirectoryError.UnauthorizedAccess(srcPath, ex)) ->
        Error(UnauthorizedAccess(srcPath, ex))
    | Error(DeleteFilesInDirectoryError.FileNotFound(file, ex)) ->
        Error(FileNotFound(file, ex))
    | Error(DeleteFilesInDirectoryError.Unknown(ex)) ->
        Error(Unknown(ex))
    | Ok _ ->
        
        match DeleteDirectoriesInDirectory(srcPath, pattern, true, true) with
        | Error(DeleteDirectoriesInDirectoryError.DirectoryNotFound(dirPath, ex)) ->
            Error(DirectoryNotFound(dirPath, ex))
        | Error(DeleteDirectoriesInDirectoryError.IOError(ex)) ->
            Error(IOError(ex))
        | Error(DeleteDirectoriesInDirectoryError.PathTooLong(srcPath, ex)) ->
            Error(PathTooLong(srcPath, ex))
        | Error(DeleteDirectoriesInDirectoryError.SecurityError(srcPath, ex)) ->
            Error(SecurityError(srcPath, ex))
        | Error(DeleteDirectoriesInDirectoryError.UnauthorizedAccess(srcPath, ex)) ->
            Error(UnauthorizedAccess(srcPath, ex))
        | Error(DeleteDirectoriesInDirectoryError.FileNotFound(file, ex)) ->
            Error(FileNotFound(file, ex))
        | Error(DeleteDirectoriesInDirectoryError.Unknown(ex)) ->
            Error(Unknown(ex))
        | Ok _ ->
            
            Ok()

let public DeleteFilesInDirectoryCancellableAsync(srcPath, pattern, cancellationToken) =
    Async.BlockingTask.Run(fun () -> DeleteFilesInDirectory(srcPath, pattern), cancellationToken)

let public DeleteFilesInDirectoryAsync(srcPath, pattern) =
    Async.BlockingTask.Run(fun () -> DeleteFilesInDirectory(srcPath, pattern))

let public DeleteEntriesInDirectoryAsync(srcPath, pattern) =
    Async.BlockingTask.Run(fun () -> DeleteEntriesFromDirectory(srcPath, pattern))
