module System.Diagnostics.Process.FAEx

open System
open System.Linq
open System.Diagnostics
open FAUtils.ErrorManagement

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

let public RunCommand(cmd: string, workingDir: string) =
   if cmd.Length = 0 then
       Error(CommandEmpty(cmd))
   else    
       let split = cmd.Split(" ", StringSplitOptions.RemoveEmptyEntries) |> Array.map (fun e -> e.Trim())
                     
       if split.Length = 0 then
           Error(CommandEmpty(cmd))
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
           
           try
                Ok (proc |> Process.Start)
           with
           | :? InvalidOperationException as ex -> Error(InvalidOperation(Some ex))
           | ex -> Error(Unknown(Some(ex)))

let public RunCommandAsync(cmd, workingDir) =
    task {
        match RunCommand(cmd, workingDir) with
        | Ok proc ->
            do! proc.WaitForExitAsync()
            return Ok proc
        | Error error -> return Error error
    }
    
let public RunCommandCancellableAsync(cmd, workingDir, token) =
    task {
        match RunCommand(cmd, workingDir) with
        | Ok proc ->
            do! proc.WaitForExitAsync(token)
            return Ok proc
        | Error error -> return Error error
    }       
           
       
