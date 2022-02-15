[<RequireQualifiedAccess>]
module System.Diagnostics.Process

open System
open System.Linq
open System.Diagnostics
open FAUtils.ErrorManagement

[<RequireQualifiedAccess>]
module FAErr =
    
    type StartError =
        | InvalidOperation of ex: Exception option
        | Unknown of ex: Exception option
        
        interface IError with
            member this.Exception =
                match this with
                | InvalidOperation ex -> ex
                | Unknown(ex) -> ex
                
            member this.ToString() =
                match this with
                | InvalidOperation ex ->
                    match ex with
                    | Some ex -> ex.Message
                    | None -> "Operation is invalid"
                | Unknown ex ->
                    match ex with
                    | Some ex -> ex.Message
                    | None -> "Unknown error"
    
    type RunCommandError =
        | CommandEmpty of cmd: string
        | InvalidOperation of ex: Exception option
        | Unknown of ex: Exception option
        
        interface IError with
            member this.Exception =
                match this with
                | CommandEmpty _ -> None
                | InvalidOperation ex -> ex
                | Unknown(ex) -> ex
                
            member this.ToString() =
                match this with
                | CommandEmpty _ -> "Command is empty"
                | InvalidOperation ex ->
                    match ex with
                    | Some ex -> ex.Message
                    | None -> "Operation is invalid"
                | Unknown ex ->
                    match ex with
                    | Some ex -> ex.Message
                    | None -> "Unknown error"

type FAEx =
    static member Start(proc: ProcessStartInfo) =
       try
            Ok (proc |> Process.Start)
       with
       | :? InvalidOperationException as ex -> Error(FAErr.StartError.InvalidOperation(Some ex))
       | ex -> Error(FAErr.StartError.Unknown(Some(ex)))
    
    static member RunCommand(cmd: string, workingDir: string) =
       if cmd.Length = 0 then
           Error(FAErr.RunCommandError.CommandEmpty(cmd))
       else    
           let split = cmd.Split(" ", StringSplitOptions.RemoveEmptyEntries) |> Array.map (fun e -> e.Trim())
                         
           if split.Length = 0 then
               Error(FAErr.RunCommandError.CommandEmpty(cmd))
           else                
               let cmd = split.First()
               
               let args = split[1..] |> String.concat " "
               
               let proc = ProcessStartInfo
                            (
                                FileName = cmd,
                                Arguments = args,
                                UseShellExecute = true,
                                CreateNoWindow = true,
                                WorkingDirectory = workingDir
                            )
               
               match (proc |> FAEx.Start) with
               | Error(FAErr.StartError.InvalidOperation(ex)) -> Error(FAErr.RunCommandError.InvalidOperation(ex))
               | Error(FAErr.StartError.Unknown(ex)) -> Error(FAErr.RunCommandError.Unknown(ex))
               | Ok proc -> Ok proc
    static member RunCommandAsync(cmd, workingDir) =
        task {
            match FAEx.RunCommand(cmd, workingDir) with
            | Ok proc ->
                do! proc.WaitForExitAsync()
                return Ok proc
            | Error error -> return Error error
        }
        
    static member RunCommandAsync(cmd, workingDir, token) =
        task {
            match FAEx.RunCommand(cmd, workingDir) with
            | Ok proc ->
                do! proc.WaitForExitAsync(token)
                return Ok proc
            | Error error -> return Error error
        } 
       
