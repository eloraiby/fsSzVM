//
// The sanity/assertions are kept in separate functions, these should be translated to another
// dependent type language with proof engine such as F*, isabelle
//
module GCBase

open System

type [<Measure>] cell
type [<Measure>] oref
type [<Measure>] ty
type [<Measure>] src
type [<Measure>] dst

type ICellType =
    abstract member TypeID         : int<ty>
    abstract member GetCellCount   : int<oref> -> int
    abstract member GetCellRef     : int<oref> * int -> int<cell>
    abstract member ReplaceCellRef : int<oref> * int * int<cell> -> unit  // replace object reference   

let [<Literal>] CT_FREE     = 0<ty>
let [<Literal>] CT_MOVED    = -1<ty>

type Cell = { Pinned: bool
              Ty    : int<ty>
              Ptr   : int<oref> }

with static member empty = { Pinned = false; Ty = CT_FREE; Ptr = 0<oref> }

type private DisposableArray<'T> (size: int, filler: int -> 'T) =
    let arr = Array.init size filler
    member x.Item with get i = arr.[i]
                    and set i value = arr.[i] <- value

    member x.Length = arr.Length

    static member init (size: int) (filler: int -> 'T) =
        new DisposableArray<'T> (size, filler)

    interface IDisposable with
        member x.Dispose() = ()

let private dispose (d: #IDisposable) = d.Dispose()

type Handle internal (arr: MemoryBlock, idx: int) =
    let sanityCheck() =
        assert(idx >= 0 && idx < arr.Size)
        assert(arr.[idx].Pinned = true)

    member x.Ptr    =
        sanityCheck()
        arr.[idx].Ptr
    
    member x.Ty     =
        sanityCheck()
        arr.[idx].Ty

    interface IDisposable with
        member x.Dispose() =
            sanityCheck()
            arr.[idx] <- { arr.[idx] with Pinned = false }

and MemoryBlock(getType: int<ty> -> ICellType) =
    let [<Literal>] INITIAL_PROCESS_SIZE = 64
    let mutable arr = DisposableArray.init INITIAL_PROCESS_SIZE (fun _ -> Cell.empty)
    let mutable index = 0

    let nextPinnedIndex (start: int<src>) =
        let mutable i = start * 1</src>
        while not arr.[i].Pinned && i < arr.Length do
            i <- i + 1
        match i with
        | x when x < arr.Length -> Some (i * 1<src>)
        | _                     -> None

    let nextFreeIndex (arr: DisposableArray<Cell>) (start: int<dst>) =
        let mutable i = start * 1</dst>
        while arr.[i].Ty <> CT_FREE do
            i <- i + 1
        match i with
        | x when x < arr.Length -> Some (i * 1<dst>)
        | _                     -> None

    // returns last dest position
    let rec moveObject (arrDest: DisposableArray<Cell>, fi: int<dst>, ob: int<src>) =
        let fiI = fi * 1</dst>
        let obI = ob * 1</src>
        match nextFreeIndex arrDest fi with
        | Some fi ->
            arrDest.[fiI] <- arr.[obI]
            arr.[obI]     <- { Pinned = false; Ty = CT_MOVED; Ptr = fiI * 1<oref> }
            moveReferencedObjects (arrDest, fi + 1<dst>, arrDest.[fiI])
        | None -> None

    // returns last dest position
    and moveReferencedObjects (arrDest: DisposableArray<Cell>, fi: int<dst>, ob: Cell) =
        let oty =getType ob.Ty
        let cnt = oty.GetCellCount ob.Ptr

        let rec loop (fi, i) =
            match i with
            | i when i < cnt ->
                let cid = oty.GetCellRef (ob.Ptr, i)
                match moveObject (arrDest, fi, cid * 1<src/cell>) with
                | None -> None
                | Some fi ->
                    // replace the reference
                    oty.ReplaceCellRef (ob.Ptr, i, arr.[cid * 1</cell>].Ptr * 1<cell/oref>)
                    loop (fi, i + 1)
            | _ -> Some fi

        match nextFreeIndex arrDest fi with
        | Some fi -> loop (fi, 0)
        | None    -> None

    // returns the last dest position
    let step (arrDest: DisposableArray<Cell>, fi: int<dst>, pi: int<src>) =
        match nextPinnedIndex pi with
        | None -> None
        | Some pi -> moveObject (arrDest, fi, pi)
        
    // compact the memory first
    let compact () =
        // start by copying the pinned objects in their place
        let arrDest =
            (fun i -> 
                match arr.[i].Pinned with
                | true  -> arr.[i]
                | false -> Cell.empty)
            |> DisposableArray.init arr.Length

        let rec loop (fi, pi) =
            match step (arrDest, fi, pi) with
            | None -> None
            | Some fi -> loop (fi, pi + 1<src>)

        loop (0<dst>, 0<src>), arrDest

    // even after GC, the handles are still valid
    member internal x.Item with get i = arr.[i]
                            and set i value = arr.[i] <- value
    member x.GC () =
        let freeIdx, newArr = compact()
        match freeIdx with
        | Some fi ->
            index <- fi * 1</dst>
            // claim arr here
            let old = arr
            arr <- newArr
            dispose old
            Some (arr.Length - index)
        | None -> // error!
            // new array should be claimed here (keep old array)
            dispose newArr
            None

    member x.Alloc(t, p) =
        if index < arr.Length
        then
            let i = index
            arr.[i] <- { Pinned = true; Ty = t; Ptr = p}
            index <- index + 1
            i
        else 0

    member x.Size = arr.Length

    interface IDisposable with
        member x.Dispose () = dispose arr

