open System

open GCBase

let typeRegistry = Collections.Concurrent.ConcurrentDictionary<int<ty>, ICellType>()

let dispose (d: #IDisposable) = d.Dispose()

type IntCellType private (tyid) =
    
    static member Singleton = 
        let ct = IntCellType (typeRegistry.Count * 1<ty>)
        typeRegistry.AddOrUpdate(typeRegistry.Count * 1<ty>, ct, fun i ct -> ct)

    interface ICellType with
        member x.TypeID             = tyid
        member x.GetCellCount _     = 0
        member x.GetCellRef (_, _)  = failwith "no cells for int"
        member x.ReplaceCellRef (_, _, _) = failwith "cannot replace cells for int"
        member x.Destruct     _     = ()


let testCompacting() =
    let mb = new MemoryBlock(fun t -> typeRegistry.[t])

    for i in 0..128 do
        let h = mb.Alloc(IntCellType.Singleton.TypeID, 0<oref>)
        dispose h

    dispose mb
    printfn "success"

let testPinning() =
    let mb = new MemoryBlock(fun t -> typeRegistry.[t])

    for i in 0..16 do
        mb.Alloc (IntCellType.Singleton.TypeID, i * 1<oref>)
        |> dispose

    let h = mb.Alloc (IntCellType.Singleton.TypeID, 17 * 1<oref>)

    for i in 0..160 do
        mb.Alloc (IntCellType.Singleton.TypeID, i * 1<oref>)
        |> dispose

    dispose mb
    printfn "success"

[<EntryPoint>]
let main argv = 
    printfn "%A" argv
    testCompacting()
    testPinning()
    0 // return an integer exit code
