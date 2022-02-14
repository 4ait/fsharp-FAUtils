


open System.IO
open FAUtils.ErrorManagement

let dirEnum = Directory.FAEx.EnumerateFiles("C:/Windows", "*/*")

for dir in dirEnum do
    let dir = dir |> IError.Expect
    printfn $"{dir}"
    