module FAUtils.Async

open System.Threading
open System.Threading.Tasks

type BlockingTask<'TResult>(callback: unit -> 'TResult, ?token: CancellationToken) =
    
    static member Run(callback: unit -> 'TResult, ?token: CancellationToken) =
        let task = 
            match token with
            | Some token ->
                 new Task<'TResult>(callback, token, TaskCreationOptions.LongRunning)
            | None ->
                 new Task<'TResult>(callback, TaskCreationOptions.LongRunning)

        task.Start()
        task

    member this.Start() =
        match token with
            | Some token ->
                 BlockingTask.Run(callback, token)
            | None ->
                 BlockingTask.Run(callback)
        