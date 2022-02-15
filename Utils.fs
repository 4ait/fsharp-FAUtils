module FAUtils.Utils
                
open System
open System.IO
open FAUtils.ErrorManagement

[<RequireQualifiedAccess>]
module FileUtils =
    
    [<RequireQualifiedAccess>]
    module Err =
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

type FileUtils =
    static member DeleteFilesInDirectory(srcPath, pattern) =
        let enumerationFilesResult = Directory.FAEx.EnumerateFilesWithBlock(srcPath, pattern, (fun file ->
                File.FAEx.Delete(file)
            ))
        
        match enumerationFilesResult with
        | Error(Directory.FAErr.DirectoryNotFound(ex)) ->
            Error(FileUtils.Err.DeleteFilesInDirectoryError.DirectoryNotFound(srcPath, ex))
        | Error(Directory.FAErr.IOError(ex)) ->
            Error(FileUtils.Err.DeleteFilesInDirectoryError.IOError(ex))
        | Error(Directory.FAErr.PathTooLong(ex)) ->
            Error(FileUtils.Err.DeleteFilesInDirectoryError.PathTooLong(srcPath, ex))
        | Error(Directory.FAErr.SecurityError(ex)) ->
            Error(FileUtils.Err.DeleteFilesInDirectoryError.SecurityError(srcPath, ex))
        | Error(Directory.FAErr.UnauthorizedAccess(ex)) ->
            Error(FileUtils.Err.DeleteFilesInDirectoryError.UnauthorizedAccess(srcPath, ex))
        | Error(Directory.FAErr.EnumerateFilesWithBlockError.Unknown(ex)) ->
            Error(FileUtils.Err.DeleteFilesInDirectoryError.Unknown(ex))
        | Error(Directory.FAErr.BlockError(error)) -> 
            match error with
                | File.FAErr.FileDeleteError.NotFound(file, ex) ->
                    Error(FileUtils.Err.DeleteFilesInDirectoryError.FileNotFound(file, ex))
                | File.FAErr.FileDeleteError.Unknown ex ->
                    Error(FileUtils.Err.DeleteFilesInDirectoryError.Unknown(ex))
        | Ok _ -> Ok()



    static member DeleteDirectoriesInDirectory(srcPath, pattern, deleteFilesInDirectories, deleteSubDirs) =
        let enumerationDirectoriesResult = Directory.FAEx.EnumerateDirectoriesWithBlock(srcPath, pattern, (fun dir ->
                if deleteFilesInDirectories then
                    match FileUtils.DeleteFilesInDirectory(dir, "*") with
                    | Error(FileUtils.Err.DeleteFilesInDirectoryError.DirectoryNotFound(srcPath, ex)) ->
                        Error(FileUtils.Err.DeleteDirectoriesInDirectoryError.DirectoryNotFound(srcPath, ex))
                    | Error(FileUtils.Err.DeleteFilesInDirectoryError.IOError(ex)) ->
                        Error(FileUtils.Err.DeleteDirectoriesInDirectoryError.IOError(ex))
                    | Error(FileUtils.Err.DeleteFilesInDirectoryError.PathTooLong(srcPath, ex)) ->
                        Error(FileUtils.Err.DeleteDirectoriesInDirectoryError.PathTooLong(srcPath, ex))
                    | Error(FileUtils.Err.DeleteFilesInDirectoryError.SecurityError(srcPath, ex)) ->
                        Error(FileUtils.Err.DeleteDirectoriesInDirectoryError.SecurityError(srcPath, ex))
                    | Error(FileUtils.Err.DeleteFilesInDirectoryError.UnauthorizedAccess(srcPath, ex)) ->
                        Error(FileUtils.Err.DeleteDirectoriesInDirectoryError.UnauthorizedAccess(srcPath, ex))
                    | Error(FileUtils.Err.DeleteFilesInDirectoryError.FileNotFound(file, ex)) ->
                        Error(FileUtils.Err.DeleteDirectoriesInDirectoryError.FileNotFound(file, ex))
                    | Error(FileUtils.Err.DeleteFilesInDirectoryError.Unknown ex) ->
                        Error(FileUtils.Err.DeleteDirectoriesInDirectoryError.Unknown ex)
                    | Ok _ -> 
                
                        if deleteSubDirs then
                            match FileUtils.DeleteDirectoriesInDirectory(dir, "*", deleteFilesInDirectories, deleteSubDirs) with
                            | Error error -> Error error
                            | Ok _ ->
                                
                                match Directory.FAEx.Delete(dir) with
                                | Error(Directory.FAErr.DirectoryDeleteError.NotFound(tempPath, ex)) ->
                                    Error(FileUtils.Err.DeleteDirectoriesInDirectoryError.DirectoryNotFound(tempPath, ex))
                                | Error(Directory.FAErr.DirectoryDeleteError.Unknown ex) ->
                                    Error(FileUtils.Err.DeleteDirectoriesInDirectoryError.Unknown ex)
                                | Ok _ ->
                                    Ok()
                        else
                            Ok()
                else 
                    Ok()
        ))
        
        match enumerationDirectoriesResult with
        | Error(Directory.FAErr.EnumerateDirectoriesWithBlockError.DirectoryNotFound(ex)) ->
            Error(FileUtils.Err.DeleteDirectoriesInDirectoryError.DirectoryNotFound(srcPath, ex))
        | Error(Directory.FAErr.EnumerateDirectoriesWithBlockError.IOError(ex)) ->
            Error(FileUtils.Err.DeleteDirectoriesInDirectoryError.IOError(ex))
        | Error(Directory.FAErr.EnumerateDirectoriesWithBlockError.PathTooLong(ex)) ->
            Error(FileUtils.Err.DeleteDirectoriesInDirectoryError.PathTooLong(srcPath, ex))
        | Error(Directory.FAErr.EnumerateDirectoriesWithBlockError.SecurityError(ex)) ->
            Error(FileUtils.Err.DeleteDirectoriesInDirectoryError.SecurityError(srcPath, ex))
        | Error(Directory.FAErr.EnumerateDirectoriesWithBlockError.UnauthorizedAccess(ex)) ->
            Error(FileUtils.Err.DeleteDirectoriesInDirectoryError.UnauthorizedAccess(srcPath, ex))
        | Error(Directory.FAErr.EnumerateDirectoriesWithBlockError.Unknown(ex)) ->
            Error(FileUtils.Err.DeleteDirectoriesInDirectoryError.Unknown(ex))
        | Error(Directory.FAErr.EnumerateDirectoriesWithBlockError.BlockError(error)) ->
            Error error
        | Ok _ -> Ok()



    static member DeleteEntriesFromDirectory(srcPath, pattern) =
        match FileUtils.DeleteFilesInDirectory(srcPath, pattern) with
        | Error(FileUtils.Err.DeleteFilesInDirectoryError.DirectoryNotFound(dirPath, ex)) ->
            Error(FileUtils.Err.DeleteDirectoriesInDirectoryError.DirectoryNotFound(dirPath, ex))
        | Error(FileUtils.Err.DeleteFilesInDirectoryError.IOError(ex)) ->
            Error(FileUtils.Err.DeleteDirectoriesInDirectoryError.IOError(ex))
        | Error(FileUtils.Err.DeleteFilesInDirectoryError.PathTooLong(srcPath, ex)) ->
            Error(FileUtils.Err.DeleteDirectoriesInDirectoryError.PathTooLong(srcPath, ex))
        | Error(FileUtils.Err.DeleteFilesInDirectoryError.SecurityError(srcPath, ex)) ->
            Error(FileUtils.Err.DeleteDirectoriesInDirectoryError.SecurityError(srcPath, ex))
        | Error(FileUtils.Err.DeleteFilesInDirectoryError.UnauthorizedAccess(srcPath, ex)) ->
            Error(FileUtils.Err.DeleteDirectoriesInDirectoryError.UnauthorizedAccess(srcPath, ex))
        | Error(FileUtils.Err.DeleteFilesInDirectoryError.FileNotFound(file, ex)) ->
            Error(FileUtils.Err.DeleteDirectoriesInDirectoryError.FileNotFound(file, ex))
        | Error(FileUtils.Err.DeleteFilesInDirectoryError.Unknown(ex)) ->
            Error(FileUtils.Err.DeleteDirectoriesInDirectoryError.Unknown(ex))
        | Ok _ ->
            
            match FileUtils.DeleteDirectoriesInDirectory(srcPath, pattern, true, true) with
            | Error(FileUtils.Err.DeleteDirectoriesInDirectoryError.DirectoryNotFound(dirPath, ex)) ->
                Error(FileUtils.Err.DeleteDirectoriesInDirectoryError.DirectoryNotFound(dirPath, ex))
            | Error(FileUtils.Err.DeleteDirectoriesInDirectoryError.IOError(ex)) ->
                Error(FileUtils.Err.DeleteDirectoriesInDirectoryError.IOError(ex))
            | Error(FileUtils.Err.DeleteDirectoriesInDirectoryError.PathTooLong(srcPath, ex)) ->
                Error(FileUtils.Err.DeleteDirectoriesInDirectoryError.PathTooLong(srcPath, ex))
            | Error(FileUtils.Err.DeleteDirectoriesInDirectoryError.SecurityError(srcPath, ex)) ->
                Error(FileUtils.Err.DeleteDirectoriesInDirectoryError.SecurityError(srcPath, ex))
            | Error(FileUtils.Err.DeleteDirectoriesInDirectoryError.UnauthorizedAccess(srcPath, ex)) ->
                Error(FileUtils.Err.DeleteDirectoriesInDirectoryError.UnauthorizedAccess(srcPath, ex))
            | Error(FileUtils.Err.DeleteDirectoriesInDirectoryError.FileNotFound(file, ex)) ->
                Error(FileUtils.Err.DeleteDirectoriesInDirectoryError.FileNotFound(file, ex))
            | Error(FileUtils.Err.DeleteDirectoriesInDirectoryError.Unknown(ex)) ->
                Error(FileUtils.Err.DeleteDirectoriesInDirectoryError.Unknown(ex))
            | Ok _ ->
                
                Ok()

    static member DeleteFilesInDirectoryAsync(srcPath, pattern) =
        task {
            let files = Directory.FAEx.EnumerateFilesAsync(srcPath, pattern)
               
            let mutable lastError = None
            
            let files =
                files
                |> Seq.takeWhile(fun _ -> lastError.IsNone)
            
            for file in files do
                let! file = file
            
                match file with
                | Ok file ->
                    match file with
                    | Some file ->
                        let! res = File.FAEx.DeleteAsync file
                        
                        match res with
                        | Error(File.FAErr.FileDeleteError.NotFound(file, ex)) ->
                            lastError <- Some(FileUtils.Err.DeleteFilesInDirectoryError.FileNotFound(file, ex))
                        | Error(File.FAErr.FileDeleteError.Unknown ex) ->
                            lastError <- Some(FileUtils.Err.DeleteFilesInDirectoryError.Unknown(ex))
                        | Ok _ -> ()
                    | None -> ()
                        
                | Error(Directory.FAErr.EnumerateFilesError.DirectoryNotFound(ex)) ->
                    lastError <-  Some(FileUtils.Err.DeleteFilesInDirectoryError.DirectoryNotFound(srcPath, ex))
                | Error(Directory.FAErr.EnumerateFilesError.IOError(ex)) ->
                    lastError <-  Some(FileUtils.Err.DeleteFilesInDirectoryError.IOError(ex))
                | Error(Directory.FAErr.EnumerateFilesError.PathTooLong(ex)) ->
                    lastError <-  Some(FileUtils.Err.DeleteFilesInDirectoryError.PathTooLong(srcPath, ex))
                | Error(Directory.FAErr.EnumerateFilesError.SecurityError(ex)) ->
                    lastError <-  Some(FileUtils.Err.DeleteFilesInDirectoryError.SecurityError(srcPath, ex))
                | Error(Directory.FAErr.EnumerateFilesError.UnauthorizedAccess(ex)) ->
                    lastError <-  Some(FileUtils.Err.DeleteFilesInDirectoryError.UnauthorizedAccess(srcPath, ex))
                | Error(Directory.FAErr.EnumerateFilesError.Unknown(ex)) ->
                    lastError <- Some(FileUtils.Err.DeleteFilesInDirectoryError.Unknown(ex))
            
            match lastError with
            | Some error -> return Error error
            | None -> return Ok()
        }
        
    static member DeleteFilesInDirectoryAsync(srcPath) =
        FileUtils.DeleteFilesInDirectoryAsync(srcPath, "*")
    
