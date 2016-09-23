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
    abstract member Destruct       : int<oref> -> unit  

let [<Literal>] CT_FREE     = -1<ty>
let [<Literal>] CT_MOVED    = -2<ty>

type Cell = { Pinned: bool
              Ty    : int<ty>
              Ptr   : int<oref> }

with static member empty = { Pinned = false; Ty = CT_FREE; Ptr = 0<oref> }

type private DisposableArray<'T> (size: int, filler: int -> 'T) =
    let arr = Array.init size filler
    member x.Item with get i = arr.[i]
                    and set i value = arr.[i] <- value

    member x.Length = arr.Length

    member private x.Arr = arr

    static member init (size: int) (filler: int -> 'T) = new DisposableArray<'T> (size, filler)

    static member iter (f: 'T -> unit) (da: DisposableArray<'T>) =
        da.Arr
        |> Array.iter f

    interface IDisposable with
        member x.Dispose() = ()

let private dispose (d: #IDisposable) = d.Dispose()

type MoveResult = { LastFreeIndex   : int<cell>
                    Destination     : int<dst> }

type Handle internal (arr: MemoryBlock, idx: int, t: int<ty>, p: int<oref>) =
    let sanityCheck() =
        assert(idx >= 0 && idx < arr.Size)
        assert(arr.[idx].Pinned = true)

    do arr.[idx] <- { Pinned = true; Ty = t; Ptr = p}

    member x.Ptr with get () =
                     sanityCheck()
                     arr.[idx].Ptr
                 and set v =
                     sanityCheck()
                     arr.[idx] <- { arr.[idx] with Ptr = v }
    
    member x.Ty     =
        sanityCheck()
        arr.[idx].Ty

    member x.IsPinned =
        sanityCheck()
        arr.[idx].Pinned

    // the cell index is invariant as long as this object lives (not disposed)
    member x.CellIndex  =
        sanityCheck()
        idx * 1<cell>

    interface IDisposable with
        member x.Dispose() =
            sanityCheck()
            arr.[idx] <- { arr.[idx] with Pinned = false }

and MemoryBlock(getType: int<ty> -> ICellType) =
    let [<Literal>] INITIAL_PROCESS_SIZE = 64
    let mutable arr = DisposableArray.init INITIAL_PROCESS_SIZE (fun _ -> Cell.empty)
    let mutable iterator = 0

    let nextPinnedIndex (start: int<src>) =
        let mutable i = start * 1</src>
        while i < arr.Length && not arr.[i].Pinned do
            i <- i + 1
        match i with
        | x when x < arr.Length -> Some (i * 1<src>)
        | _                     -> None

    let nextFreeIndex (arr: DisposableArray<Cell>) (start: int<dst>) =
        let mutable i = start * 1</dst>
        while i < start * 1</dst> + arr.Length && arr.[i % arr.Length].Ty <> CT_FREE do
            i <- i + 1
        match i with
        | x when x < start * 1</dst> + arr.Length -> Some (i % arr.Length * 1<dst>)
        | _                     -> None
       
    // move objects recursively
    let rec moveObject(destArr: DisposableArray<Cell>, lmr: MoveResult, ob: int<cell>) : MoveResult =
        let c = arr.[ob * 1</cell>]
        match c.Ty with
        | CT_FREE -> failwith "unexpected free"
        | CT_MOVED ->
            assert(c.Ptr <> -1<oref>)
            { lmr with MoveResult.Destination = c.Ptr * 1<dst/oref> }
        | ty -> // anything else, we have to digg deeper
            let oty = getType ty
            let rcnt = oty.GetCellCount c.Ptr
            let state =
                match c.Pinned with
                | true -> { LastFreeIndex = lmr.LastFreeIndex + 1<cell>; Destination = ob * 1<dst/cell> }
                | false ->  
                    let nextFree = nextFreeIndex destArr (lmr.LastFreeIndex * 1<dst/cell>)
                    match nextFree with
                    | Some nextFree ->
                        assert(nextFree <> -1<dst>)
                        destArr.[nextFree * 1</dst>] <- c
                        arr.[ob * 1</cell>] <- { Cell.Pinned = false; Cell.Ty = CT_MOVED; Cell.Ptr = nextFree * 1<oref/dst> }
                        { LastFreeIndex = nextFree * 1<cell/dst> + 1<cell>; Destination = nextFree }

                    | None -> failwith "unexpected error, no more free cells"

            let rec loop (state: MoveResult, i) =
                match i with
                | i when i < rcnt ->
                    let r = oty.GetCellRef(c.Ptr, i)
                    let res = moveObject (destArr, state, r) 
                    match res with
                    | state2 ->
                        assert(state2.Destination <> -1<dst>)
                        oty.ReplaceCellRef (c.Ptr, i, state2.Destination * 1<cell/dst>)
                        loop (state2, i + 1)
                    
                | _ -> state
                
            loop (state, 0)

    // compact the memory first
    let compact () =
        // start by copying the pinned objects in their place
        let arrDest =
            (fun i -> 
                match arr.[i].Pinned with
                | true  -> arr.[i]
                | false -> Cell.empty)
            |> DisposableArray.init arr.Length

        let rec loop (state, pi) =
            let pi = nextPinnedIndex pi
            match pi with
            | Some pi ->
                let state = moveObject (arrDest, state, pi * 1<cell/src>)
                loop (state, pi + 1<src>)
            | None    -> state.LastFreeIndex

        loop ({ LastFreeIndex = 0<cell>; Destination = -1<dst> }, 0<src>), arrDest

    // even after GC, the handles are still valid
    member internal x.Item with get i = arr.[i]
                            and set i value = arr.[i] <- value
    member x.GC () =
        let freeIdx, newArr = compact()
        match freeIdx with
        | fi when fi < arr.Length * 1<cell> ->
            iterator <- fi * 1</cell>
            // claim arr here
            let old = arr
            arr <- newArr
            // now destruct any remaining object
            old
            |> DisposableArray.iter (fun o ->
                match o.Ty with
                | CT_FREE | CT_MOVED -> ()
                | t -> (getType o.Ty).Destruct o.Ptr)

            dispose old
            Some (arr.Length - iterator)
        | fi when fi >= arr.Length * 1<cell> -> // error!
            // new array should be claimed here (keep old array)
            dispose newArr
            None

    member x.Alloc(t, p) =
        let i = nextFreeIndex arr (iterator * 1<dst>)
        match i with
        | Some i ->
            iterator <- i * 1</dst> + 1
            new Handle(x, i * 1</dst>, t, p)
        | None ->
            match x.GC() with
            | None -> failwith "not enough memory"
            | Some _ ->
                let i = nextFreeIndex arr (iterator * 1<dst>)
                match i with
                | Some i ->
                    iterator <- i * 1</dst> + 1
                    new Handle(x, i * 1</dst>, t, p)
                | None -> failwith "Something went terribly wrong: not enough memory after compacting and returning OK"


    member x.Size = arr.Length

    interface IDisposable with
        member x.Dispose () =
            // destruct all cells before disposal
            arr
            |> DisposableArray.iter(fun c ->
                match c.Ty with
                | CT_FREE | CT_MOVED -> ()
                | _ ->
                    c.Ptr
                    |> (getType c.Ty).Destruct)
            dispose arr

