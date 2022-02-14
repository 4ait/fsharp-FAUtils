


open System.IO
open FAUtils.ErrorManagement

Directory.FAEx.EnumerateDirectoriesWithBlock(
    "C:/",
    "Windows",
    (fun e ->
        match true with
        | true -> Ok()
        | false -> failwith ""
    )
    
) |> IError.Expect

let dirEnum = Directory.FAEx.EnumerateDirectories("C:/", "Windows")

for dir in dirEnum do
    let dir = dir |> IError.Expect
    printfn $"{dir}"
    