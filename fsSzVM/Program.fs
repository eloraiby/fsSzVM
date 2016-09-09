open System

open GCBase

let IntCell(i: int) =
    interface ICellType with

let testClean() =
    let mb = new MemoryBlock()

[<EntryPoint>]
let main argv = 
    printfn "%A" argv
    0 // return an integer exit code
