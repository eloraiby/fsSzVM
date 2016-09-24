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

    static let singleton_ = lazy (
        let ct = ConsCellType (typeRegistry.Count * 1<ty>)
        typeRegistry.AddOrUpdate(typeRegistry.Count * 1<ty>, ct, fun i ct -> ct))

    static member Singleton = singleton_.Value


    static member make(h: int<cell>, t: int<cell>) =
        let i = cons.Count
        cons.Add { Head = h; Tail = t }
        i * 1<oref>

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

    let h1 = mb.Alloc (IntCellType.Singleton.TypeID, 1111<oref>)
    let h2 = mb.Alloc (IntCellType.Singleton.TypeID, 2222<oref>)
    let cid = ConsCellType.make (h1.CellIndex, h2.CellIndex)
    let c = mb.Alloc (ConsCellType.Singleton.TypeID, cid)

    dispose h1
    dispose h2

    for i in 0..mb.Size * 4 do
        mb.Alloc (IntCellType.Singleton.TypeID, i * 1<oref>)
        |> dispose
         
    assert(c.IsPinned = true)
    assert(c.Ty = ConsCellType.Singleton.TypeID)
    assert(typeRegistry.[c.Ty].GetCellCount(c.Ptr) = 2)
    let h1i = typeRegistry.[c.Ty].GetCellRef(c.Ptr, 0)
    let h2i = typeRegistry.[c.Ty].GetCellRef(c.Ptr, 1)
    let h1 = mb.[h1i * 1</cell>]
    let h2 = mb.[h2i * 1</cell>]
    assert(h1.Ty = IntCellType.Singleton.TypeID)
    assert(h2.Ty = IntCellType.Singleton.TypeID)
    assert(h1.Pinned = false)
    assert(h2.Pinned = false)
    assert(h1.Ptr = 1111<oref>)
    assert(h2.Ptr = 2222<oref>)

    let h3 = mb.Alloc (IntCellType.Singleton.TypeID, 3333<oref>)
    let cid2 = ConsCellType.make (c.CellIndex, h3.CellIndex)
    let c2 = mb.Alloc (ConsCellType.Singleton.TypeID, cid2)

    dispose c

    for i in 0..mb.Size * 4 do
        mb.Alloc (IntCellType.Singleton.TypeID, i * 1<oref>)
        |> dispose
    
    for i in 0..mb.Size  - 1 do
        if mb.[i].Ty > 0<ty>
        then printfn "%d - %d" i mb.[i].Ptr

    assert(c2.IsPinned = true)
    assert(c2.Ty = ConsCellType.Singleton.TypeID)
    assert(typeRegistry.[c2.Ty].GetCellCount(c2.Ptr) = 2)

    let ci = typeRegistry.[c2.Ty].GetCellRef(c2.Ptr, 0)
    let h3i = typeRegistry.[c2.Ty].GetCellRef(c2.Ptr, 1)
    let c = mb.[ci * 1</cell>]
    let h3 = mb.[h3i * 1</cell>]
    let h1i = typeRegistry.[c.Ty].GetCellRef(c.Ptr, 0)
    let h2i = typeRegistry.[c.Ty].GetCellRef(c.Ptr, 1)
    let h1 = mb.[h1i * 1</cell>]
    let h2 = mb.[h2i * 1</cell>]
    assert(h1.Ty = IntCellType.Singleton.TypeID)
    assert(h2.Ty = IntCellType.Singleton.TypeID)
    assert(h1.Pinned = false)
    assert(h2.Pinned = false)
    assert(h1.Ptr = 1111<oref>)
    assert(h2.Ptr = 2222<oref>)

    dispose c2
    dispose mb
    printfn "success"

let testMemExpand() =
    let mb = new MemoryBlock(fun t -> typeRegistry.[t])
    let arr = Array.init (mb.Size - 1) (fun i ->
        mb.Alloc (IntCellType.Singleton.TypeID, i * 4<oref>))

    let origSize = mb.Size
    for i in 0..mb.Size / 4 do
        mb.Alloc (IntCellType.Singleton.TypeID, i * 1<oref>)
        |> dispose

    let h1 = mb.Alloc (IntCellType.Singleton.TypeID, 1111<oref>)
    let h2 = mb.Alloc (IntCellType.Singleton.TypeID, 2222<oref>)
    let cid = ConsCellType.make (h1.CellIndex, h2.CellIndex)
    let c = mb.Alloc (ConsCellType.Singleton.TypeID, cid)

    dispose h1
    dispose h2

    for i in 0..mb.Size * 4 do
        mb.Alloc (IntCellType.Singleton.TypeID, i * 1<oref>)
        |> dispose
         
    assert(c.IsPinned = true)
    assert(c.Ty = ConsCellType.Singleton.TypeID)
    assert(typeRegistry.[c.Ty].GetCellCount(c.Ptr) = 2)
    let h1i = typeRegistry.[c.Ty].GetCellRef(c.Ptr, 0)
    let h2i = typeRegistry.[c.Ty].GetCellRef(c.Ptr, 1)
    let h1 = mb.[h1i * 1</cell>]
    let h2 = mb.[h2i * 1</cell>]
    assert(h1.Ty = IntCellType.Singleton.TypeID)
    assert(h2.Ty = IntCellType.Singleton.TypeID)
    assert(h1.Pinned = false)
    assert(h2.Pinned = false)
    assert(h1.Ptr = 1111<oref>)
    assert(h2.Ptr = 2222<oref>)

    let h3 = mb.Alloc (IntCellType.Singleton.TypeID, 3333<oref>)
    let cid2 = ConsCellType.make (c.CellIndex, h3.CellIndex)
    let c2 = mb.Alloc (ConsCellType.Singleton.TypeID, cid2)

    dispose c

    for i in 0..mb.Size * 4 do
        mb.Alloc (IntCellType.Singleton.TypeID, i * 1<oref>)
        |> dispose
    
    for i in 0..mb.Size  - 1 do
        if mb.[i].Ty > 0<ty>
        then printfn "%d - %d" i mb.[i].Ptr

    assert(c2.IsPinned = true)
    assert(c2.Ty = ConsCellType.Singleton.TypeID)
    assert(typeRegistry.[c2.Ty].GetCellCount(c2.Ptr) = 2)

    let ci = typeRegistry.[c2.Ty].GetCellRef(c2.Ptr, 0)
    let h3i = typeRegistry.[c2.Ty].GetCellRef(c2.Ptr, 1)
    let c = mb.[ci * 1</cell>]
    let h3 = mb.[h3i * 1</cell>]
    let h1i = typeRegistry.[c.Ty].GetCellRef(c.Ptr, 0)
    let h2i = typeRegistry.[c.Ty].GetCellRef(c.Ptr, 1)
    let h1 = mb.[h1i * 1</cell>]
    let h2 = mb.[h2i * 1</cell>]
    assert(h1.Ty = IntCellType.Singleton.TypeID)
    assert(h2.Ty = IntCellType.Singleton.TypeID)
    assert(h1.Pinned = false)
    assert(h2.Pinned = false)
    assert(h1.Ptr = 1111<oref>)
    assert(h2.Ptr = 2222<oref>)

    assert(mb.Size > origSize)
    dispose c2
    dispose mb
    printfn "success"


[<EntryPoint>]
let main argv = 
    printfn "%A" argv
    testCompacting ()
    testPinning ()
    testMoving ()
    testMemExpand ()
    0 // return an integer exit code
