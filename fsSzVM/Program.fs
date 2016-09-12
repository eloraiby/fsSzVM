open System

open GCBase

let typeRegistry = Collections.Concurrent.ConcurrentDictionary<int<ty>, ICellType>()

let dispose (d: #IDisposable) = d.Dispose()

type IntCellType private (tyid) =

    static let singleton_ = lazy(
        let tyI = typeRegistry.Count * 1<ty>
        let ct = IntCellType tyI
        printfn "IntCellSingleton - %d" (tyI * 1</ty>)
        typeRegistry.AddOrUpdate(tyI, ct, fun i ct -> ct))


    static member Singleton = singleton_.Value

    interface ICellType with
        member x.TypeID             = tyid
        member x.GetCellCount _     = 0
        member x.GetCellRef (_, _)  = failwith "no cells for int"
        member x.ReplaceCellRef (_, _, _) = failwith "cannot replace cells for int"
        member x.Destruct     _     = ()

type Cons = { Head : int<cell>
              Tail : int<cell> }

type ConsCellType private (tyid) =
    static let cons = Collections.Generic.List<Cons>()

    static member Singleton = 
        let ct = ConsCellType (typeRegistry.Count * 1<ty>)
        typeRegistry.AddOrUpdate(typeRegistry.Count * 1<ty>, ct, fun i ct -> ct)

    static member make(h: int<cell>, t: int<cell>) =
        let i = cons.Count
        cons.Add { Head = h; Tail = t }
        i

    interface ICellType with
        member x.TypeID             = tyid
        member x.GetCellCount _     = 2
        member x.GetCellRef (o, i)  =
            let o = o * 1</oref>
            match i with
            | 0 -> cons.[o].Head
            | 1 -> cons.[o].Tail
            | _ -> failwith "invalid cons index"

        member x.ReplaceCellRef (o, i, c) =
            let o = o * 1</oref>
            match i with
            | 0 -> cons.[o] <- { cons.[o] with Head = c }
            | 1 -> cons.[o] <- { cons.[o] with Tail = c }
            | _ -> failwith "invalid cons index: cannot replace cons cells"
        member x.Destruct     _     = ()

let testCompacting() =
    let mb = new MemoryBlock(fun t -> typeRegistry.[t])

    for i in 0..128 do
        let h = mb.Alloc(IntCellType.Singleton.TypeID, i * 1<oref>)
        dispose h

    dispose mb
    printfn "success"

let testPinning() =
    let mb = new MemoryBlock(fun t -> typeRegistry.[t])

    for i in 0..mb.Size / 4 do
        mb.Alloc (IntCellType.Singleton.TypeID, i * 1<oref>)
        |> dispose

    let h = mb.Alloc (IntCellType.Singleton.TypeID, 1777 * 1<oref>)

    for i in 0..mb.Size * 4 do
        mb.Alloc (IntCellType.Singleton.TypeID, i * 1<oref>)
        |> dispose

    assert(h.IsPinned = true)
    assert(h.Ptr = 1777<oref>)
    assert(h.Ty = IntCellType.Singleton.TypeID)

    dispose mb
    printfn "success"

let testMoving() =
    let mb = new MemoryBlock(fun t -> typeRegistry.[t])
    for i in 0..mb.Size / 4 do
        mb.Alloc (IntCellType.Singleton.TypeID, i * 1<oref>)
        |> dispose

    let h = mb.Alloc (IntCellType.Singleton.TypeID, 1777 * 1<oref>)

    dispose mb
    printfn "success"

[<EntryPoint>]
let main argv = 
    printfn "%A" argv
    testCompacting()
    testPinning()
    0 // return an integer exit code
