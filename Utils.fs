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
    
    type ErrorConvertor =
        
        static member ToDeleteFilesInDirectoryError(res, srcPath) =
            match res with
            | Error(Directory.FAErr.DirectoryNotFound(ex)) ->
                Error(Err.DeleteFilesInDirectoryError.DirectoryNotFound(srcPath, ex))
            | Error(Directory.FAErr.IOError(ex)) ->
                Error(Err.DeleteFilesInDirectoryError.IOError(ex))
            | Error(Directory.FAErr.PathTooLong(ex)) ->
                Error(Err.DeleteFilesInDirectoryError.PathTooLong(srcPath, ex))
            | Error(Directory.FAErr.SecurityError(ex)) ->
                Error(Err.DeleteFilesInDirectoryError.SecurityError(srcPath, ex))
            | Error(Directory.FAErr.UnauthorizedAccess(ex)) ->
                Error(Err.DeleteFilesInDirectoryError.UnauthorizedAccess(srcPath, ex))
            | Error(Directory.FAErr.EnumerateFilesWithBlockError.Unknown(ex)) ->
                Error(Err.DeleteFilesInDirectoryError.Unknown(ex))
            | Error(Directory.FAErr.BlockError(error)) -> 
                match error with
                    | File.FAErr.FileDeleteError.NotFound(file, ex) ->
                        Error(Err.DeleteFilesInDirectoryError.FileNotFound(file, ex))
                    | File.FAErr.FileDeleteError.Unknown ex ->
                        Error(Err.DeleteFilesInDirectoryError.Unknown(ex))
            | Ok res -> Ok res
        
        static member ToDeleteDirectoriesInDirectoryError(res) =
            match res with
            | Error(Directory.FAErr.DirectoryDeleteError.NotFound(tempPath, ex)) ->
                Error(Err.DeleteDirectoriesInDirectoryError.DirectoryNotFound(tempPath, ex))
            | Error(Directory.FAErr.DirectoryDeleteError.Unknown ex) ->
                Error(Err.DeleteDirectoriesInDirectoryError.Unknown ex)
            | Ok res -> res
        
        static member ToDeleteDirectoriesInDirectoryError(error) =
            match error with
            | Error(Directory.FAErr.DirectoryDeleteError.NotFound(tempPath, ex)) ->
                Error(Err.DeleteDirectoriesInDirectoryError.DirectoryNotFound(tempPath, ex))
            | Error(Directory.FAErr.DirectoryDeleteError.Unknown ex) ->
                Error(Err.DeleteDirectoriesInDirectoryError.Unknown ex)
            | Ok res -> Ok res
        
        static member ToDeleteDirectoriesInDirectoryError(res, srcPath) =
            match res with
            | Error(Directory.FAErr.EnumerateDirectoriesWithBlockError.DirectoryNotFound(ex)) ->
                Error(Err.DeleteDirectoriesInDirectoryError.DirectoryNotFound(srcPath, ex))
            | Error(Directory.FAErr.EnumerateDirectoriesWithBlockError.IOError(ex)) ->
                Error(Err.DeleteDirectoriesInDirectoryError.IOError(ex))
            | Error(Directory.FAErr.EnumerateDirectoriesWithBlockError.PathTooLong(ex)) ->
                Error(Err.DeleteDirectoriesInDirectoryError.PathTooLong(srcPath, ex))
            | Error(Directory.FAErr.EnumerateDirectoriesWithBlockError.SecurityError(ex)) ->
                Error(Err.DeleteDirectoriesInDirectoryError.SecurityError(srcPath, ex))
            | Error(Directory.FAErr.EnumerateDirectoriesWithBlockError.UnauthorizedAccess(ex)) ->
                Error(Err.DeleteDirectoriesInDirectoryError.UnauthorizedAccess(srcPath, ex))
            | Error(Directory.FAErr.EnumerateDirectoriesWithBlockError.Unknown(ex)) ->
                Error(Err.DeleteDirectoriesInDirectoryError.Unknown(ex))
            | Error(Directory.FAErr.EnumerateDirectoriesWithBlockError.BlockError(error)) ->
                Error error
            | Ok res -> Ok res
        
        static member ToDeleteDirectoriesInDirectoryError(res) =
            match res with
            | Error(Err.DeleteFilesInDirectoryError.DirectoryNotFound(dirPath, ex)) ->
                Error(Err.DeleteDirectoriesInDirectoryError.DirectoryNotFound(dirPath, ex))
            | Error(Err.DeleteFilesInDirectoryError.IOError(ex)) ->
                Error(Err.DeleteDirectoriesInDirectoryError.IOError(ex))
            | Error(Err.DeleteFilesInDirectoryError.PathTooLong(srcPath, ex)) ->
                Error(Err.DeleteDirectoriesInDirectoryError.PathTooLong(srcPath, ex))
            | Error(Err.DeleteFilesInDirectoryError.SecurityError(srcPath, ex)) ->
                Error(Err.DeleteDirectoriesInDirectoryError.SecurityError(srcPath, ex))
            | Error(Err.DeleteFilesInDirectoryError.UnauthorizedAccess(srcPath, ex)) ->
                Error(Err.DeleteDirectoriesInDirectoryError.UnauthorizedAccess(srcPath, ex))
            | Error(Err.DeleteFilesInDirectoryError.FileNotFound(file, ex)) ->
                Error(Err.DeleteDirectoriesInDirectoryError.FileNotFound(file, ex))
            | Error(Err.DeleteFilesInDirectoryError.Unknown(ex)) ->
                Error(Err.DeleteDirectoriesInDirectoryError.Unknown(ex))
            | Ok res -> Ok res

        static member ToDeleteDirectoriesInDirectoryError(res) =
            match res with
            | Error(Err.DeleteDirectoriesInDirectoryError.DirectoryNotFound(dirPath, ex)) ->
                Error(Err.DeleteDirectoriesInDirectoryError.DirectoryNotFound(dirPath, ex))
            | Error(Err.DeleteDirectoriesInDirectoryError.IOError(ex)) ->
                Error(Err.DeleteDirectoriesInDirectoryError.IOError(ex))
            | Error(Err.DeleteDirectoriesInDirectoryError.PathTooLong(srcPath, ex)) ->
                Error(Err.DeleteDirectoriesInDirectoryError.PathTooLong(srcPath, ex))
            | Error(Err.DeleteDirectoriesInDirectoryError.SecurityError(srcPath, ex)) ->
                Error(Err.DeleteDirectoriesInDirectoryError.SecurityError(srcPath, ex))
            | Error(Err.DeleteDirectoriesInDirectoryError.UnauthorizedAccess(srcPath, ex)) ->
                Error(Err.DeleteDirectoriesInDirectoryError.UnauthorizedAccess(srcPath, ex))
            | Error(Err.DeleteDirectoriesInDirectoryError.FileNotFound(file, ex)) ->
                Error(Err.DeleteDirectoriesInDirectoryError.FileNotFound(file, ex))
            | Error(Err.DeleteDirectoriesInDirectoryError.Unknown(ex)) ->
                Error(Err.DeleteDirectoriesInDirectoryError.Unknown(ex))
            | Ok res -> Ok res
            

type FileUtils =
    static member DeleteFilesInDirectory(srcPath, pattern) =
        let enumerationFilesResult = Directory.FAEx.EnumerateFilesWithBlock(srcPath, pattern, (fun file ->
                File.FAEx.Delete(file)
            ))
        
        FileUtils.ErrorConvertor.ToDeleteFilesInDirectoryError(enumerationFilesResult, srcPath)


    static member DeleteDirectoriesInDirectory(srcPath, pattern, deleteFilesInDirectories, deleteSubDirs) =
        let enumerationDirectoriesResult = Directory.FAEx.EnumerateDirectoriesWithBlock(srcPath, pattern, (fun dir ->
                
                let deleteDirectory() =
                    let res = Directory.FAEx.Delete(dir)
                    
                    FileUtils.ErrorConvertor.ToDeleteDirectoriesInDirectoryError(res)
                
                if deleteFilesInDirectories then
                    let res = FileUtils.DeleteFilesInDirectory(dir, "*")
                    
                    match FileUtils.ErrorConvertor.ToDeleteDirectoriesInDirectoryError(res) with
                    | Error error -> Error error
                    | Ok _ -> 
                
                        if deleteSubDirs then
                            match FileUtils.DeleteDirectoriesInDirectory(dir, "*", deleteFilesInDirectories, deleteSubDirs) with
                            | Error error -> Error error
                            | Ok _ ->
                                deleteDirectory()
                        else
                            deleteDirectory()
                else 
                    deleteDirectory()
        ))
        
        FileUtils.ErrorConvertor.ToDeleteDirectoriesInDirectoryError(enumerationDirectoriesResult, srcPath)



    (*static member DeleteEntriesFromDirectory(srcPath, pattern) =
        let res = FileUtils.DeleteFilesInDirectory(srcPath, pattern)
        
        match FileUtils.ErrorConvertor.Convert(res) with
        | Error error -> Error error
        | Ok _ ->
            let res = FileUtils.DeleteDirectoriesInDirectory(srcPath, pattern, true, true)
            
            match FileUtils.ErrorConvertor.Convert(res) with
            | Error error -> Error error
            | Ok _ -> Ok()*)

    static member private safeDeleteFilesInDirectoryAsync(srcPath, files) =
        task {
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
                    lastError <- Some(FileUtils.Err.DeleteFilesInDirectoryError.DirectoryNotFound(srcPath, ex))
                | Error(Directory.FAErr.EnumerateFilesError.IOError(ex)) ->
                    lastError <- Some(FileUtils.Err.DeleteFilesInDirectoryError.IOError(ex))
                | Error(Directory.FAErr.EnumerateFilesError.PathTooLong(ex)) ->
                    lastError <- Some(FileUtils.Err.DeleteFilesInDirectoryError.PathTooLong(srcPath, ex))
                | Error(Directory.FAErr.EnumerateFilesError.SecurityError(ex)) ->
                    lastError <- Some(FileUtils.Err.DeleteFilesInDirectoryError.SecurityError(srcPath, ex))
                | Error(Directory.FAErr.EnumerateFilesError.UnauthorizedAccess(ex)) ->
                    lastError <- Some(FileUtils.Err.DeleteFilesInDirectoryError.UnauthorizedAccess(srcPath, ex))
                | Error(Directory.FAErr.EnumerateFilesError.Unknown(ex)) ->
                    lastError <- Some(FileUtils.Err.DeleteFilesInDirectoryError.Unknown(ex))
            
            match lastError with
            | Some error -> return Error error
            | None -> return Ok()
        }
    
    static member private safeDeleteDirectoriesInDirectoryAsync(srcPath,
                                                                directories,
                                                                deleteFilesInDirectories,
                                                                deleteSubDirs) =
        
        task {
            let mutable lastError = None
            
            let directories =
                directories
                |> Seq.takeWhile(fun _ -> lastError.IsNone)
            
            for directory in directories do
                let! directory = directory
            
                match directory with
                | Ok directory ->
                    match directory with
                    | Some directory ->
                        let deleteDirectoryAsync() =
                            task {
                                let! res = Directory.FAEx.DeleteAsync directory
                                                            
                                match res with
                                | Error(Directory.FAErr.DirectoryDeleteError.NotFound(file, ex)) ->
                                    lastError <- Some(FileUtils.Err.DeleteDirectoriesInDirectoryError.FileNotFound(file, ex))
                                | Error(Directory.FAErr.DirectoryDeleteError.Unknown ex) ->
                                    lastError <- Some(FileUtils.Err.DeleteDirectoriesInDirectoryError.Unknown(ex))
                                | Ok _ -> ()
                            }
                        
                        if deleteFilesInDirectories then
                            let! res = FileUtils.DeleteFilesInDirectoryAsync(directory)
                            match res with
                            | Error(FileUtils.Err.DeleteFilesInDirectoryError.DirectoryNotFound(dirPath, ex)) ->
                                lastError <- Some(FileUtils.Err.DeleteDirectoriesInDirectoryError.DirectoryNotFound(dirPath, ex))
                            | Error(FileUtils.Err.DeleteFilesInDirectoryError.IOError(ex)) ->
                                lastError <- Some(FileUtils.Err.DeleteDirectoriesInDirectoryError.IOError(ex))
                            | Error(FileUtils.Err.DeleteFilesInDirectoryError.PathTooLong(srcPath, ex)) ->
                                lastError <- Some(FileUtils.Err.DeleteDirectoriesInDirectoryError.PathTooLong(srcPath, ex))
                            | Error(FileUtils.Err.DeleteFilesInDirectoryError.SecurityError(srcPath, ex)) ->
                                lastError <- Some(FileUtils.Err.DeleteDirectoriesInDirectoryError.SecurityError(srcPath, ex))
                            | Error(FileUtils.Err.DeleteFilesInDirectoryError.UnauthorizedAccess(srcPath, ex)) ->
                                lastError <- Some(FileUtils.Err.DeleteDirectoriesInDirectoryError.UnauthorizedAccess(srcPath, ex))
                            | Error(FileUtils.Err.DeleteFilesInDirectoryError.FileNotFound(file, ex)) ->
                                lastError <- Some(FileUtils.Err.DeleteDirectoriesInDirectoryError.FileNotFound(file, ex))
                            | Error(FileUtils.Err.DeleteFilesInDirectoryError.Unknown(ex)) ->
                                lastError <- Some(FileUtils.Err.DeleteDirectoriesInDirectoryError.Unknown(ex))
                            | Ok _ -> 
                                
                                if deleteSubDirs then
                                    let! res = FileUtils.DeleteDirectoriesInDirectoryAsync(directory,
                                                                                           deleteFilesInDirectories,
                                                                                           deleteSubDirs)
                                    
                                    match res with
                                    | Error error -> lastError <- Some(error)
                                    | Ok _ ->
                                        do! deleteDirectoryAsync()
                                else
                                    do! deleteDirectoryAsync()
                        else
                            do! deleteDirectoryAsync()
                        
                    | None -> ()
                        
                | Error(Directory.FAErr.EnumerateDirectoriesError.DirectoryNotFound(ex)) ->
                    lastError <-  Some(FileUtils.Err.DeleteDirectoriesInDirectoryError.DirectoryNotFound(srcPath, ex))
                | Error(Directory.FAErr.EnumerateDirectoriesError.IOError(ex)) ->
                    lastError <-  Some(FileUtils.Err.DeleteDirectoriesInDirectoryError.IOError(ex))
                | Error(Directory.FAErr.EnumerateDirectoriesError.PathTooLong(ex)) ->
                    lastError <-  Some(FileUtils.Err.DeleteDirectoriesInDirectoryError.PathTooLong(srcPath, ex))
                | Error(Directory.FAErr.EnumerateDirectoriesError.SecurityError(ex)) ->
                    lastError <-  Some(FileUtils.Err.DeleteDirectoriesInDirectoryError.SecurityError(srcPath, ex))
                | Error(Directory.FAErr.EnumerateDirectoriesError.UnauthorizedAccess(ex)) ->
                    lastError <-  Some(FileUtils.Err.DeleteDirectoriesInDirectoryError.UnauthorizedAccess(srcPath, ex))
                | Error(Directory.FAErr.EnumerateDirectoriesError.Unknown(ex)) ->
                    lastError <- Some(FileUtils.Err.DeleteDirectoriesInDirectoryError.Unknown(ex))
            
            match lastError with
            | Some error -> return Error error
            | None -> return Ok()
        }
    
    static member DeleteFilesInDirectoryAsync(srcPath, pattern) =
       FileUtils.safeDeleteFilesInDirectoryAsync(srcPath,
                                                 Directory.FAEx.EnumerateFilesAsync(srcPath, pattern))
        
    static member DeleteFilesInDirectoryAsync(srcPath) =
       FileUtils.safeDeleteFilesInDirectoryAsync(srcPath, Directory.FAEx.EnumerateFilesAsync(srcPath))
    
    static member DeleteDirectoriesInDirectoryAsync(srcPath, pattern, deleteFilesInDirectories, deleteSubDirs) =
       FileUtils.safeDeleteDirectoriesInDirectoryAsync(srcPath,
                                                       Directory.FAEx.EnumerateDirectoriesAsync(srcPath, pattern),
                                                       deleteFilesInDirectories, deleteSubDirs)
        
    static member DeleteDirectoriesInDirectoryAsync(srcPath, deleteFilesInDirectories, deleteSubDirs) =
       FileUtils.safeDeleteDirectoriesInDirectoryAsync(srcPath,
                                                       Directory.FAEx.EnumerateDirectoriesAsync(srcPath),
                                                       deleteFilesInDirectories,
                                                       deleteSubDirs)
    
