


open System.IO
open System.Threading.Tasks
open FAUtils.ErrorManagement
open FAUtils.Utils

let dirEnum = Directory.FAEx.EnumerateFiles("C:/Windows")

for dir in dirEnum do
    let dir = dir |> IError.Expect
    printfn $"{dir}"

let task =
    task {
        let dirEnum = Directory.FAEx.EnumerateFilesAsync(@"C:/")

        for dir in dirEnum do
            let! dir = dir
            let dir = dir |> IError.Expect
            printfn $"{dir}"
    }
task.Wait()
