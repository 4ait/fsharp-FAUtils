module FAUtils.Safe

open System.Threading.Tasks
open FAUtils.Async
open System.Collections.Generic


type SafeEnumeration =
    static member EnumerateWithError(
        enumGetter: unit -> Result<IEnumerable<'ResultEnum>, 'ErrorEnum>,
        moveNext: (unit -> bool) -> Result<bool, 'ErrorEnum>
        ) =
         seq {
            let enumeration = enumGetter()
            
            match enumeration with
            | Ok enumeration ->
                let enumerator = enumeration.GetEnumerator()
                
                let mutable moveNextExists = true
                
                while moveNextExists do
                    match moveNext(fun () -> enumerator.MoveNext()) with
                    | Ok true -> yield Ok enumerator.Current
                    | Ok false -> moveNextExists <- false
                    | Error error ->
                        yield Error error
                        moveNextExists <- false
                
            | Error error -> yield Error error
        }
    
    static member EnumerateWithErrorAsync(
            enumGetter: unit -> Task<Result<IEnumerable<'ResultEnum>, 'ErrorEnum>>,
            moveNext: (unit -> bool) -> Task<Result<bool, 'ErrorEnum>>
        ) =
        seq {
            let mutable moveNextExists = true
            let mutable enumerator: IEnumerator<'ResultEnum> = null
            
            let mutable lastTaskExecuted = false
            
            let enumeration =
                task {
                    let! enumeration = enumGetter()
                    
                    lastTaskExecuted <- true
                                         
                    match enumeration with
                    | Ok enumeration ->
                        enumerator <- enumeration.GetEnumerator()
                        
                        let! moveNextRes = moveNext(fun () -> enumerator.MoveNext())
                        
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
            
            while moveNextExists do
                if not lastTaskExecuted then
                    failwith "Previous task is not executed. Please, wait task before iterate next element"
                
                let enumeration =
                    task {
                        let! moveNextRes = moveNext(fun () -> enumerator.MoveNext())
                        
                        lastTaskExecuted <- true
                        
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

