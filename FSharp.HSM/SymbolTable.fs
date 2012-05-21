module internal FSharp.SymbolTables

[<Struct>]
type Symbol<'T> =
  val internal id : int
  internal new(id) = {id=id}

open System.Collections.Generic

type SymbolDictionary<'T, 'Value> =
  val private used : System.Collections.BitArray
  val private arr : 'Value []
  internal new(count) =
    { used = System.Collections.BitArray(count, false)
      arr = Array.zeroCreate count }
  member dict.Item
    with get(id: Symbol<'T>) =
      if dict.used.[id.id] then
        dict.arr.[id.id]
      else
        raise(KeyNotFoundException())
    and set (id: Symbol<'T>) x =
      dict.used.[id.id] <- true
      dict.arr.[id.id] <- x
  member dict.Remove (id: Symbol<'T>) =
    dict.used.[id.id] <- false
    dict.arr.[id.id] <- Unchecked.defaultof<_>
  member dict.TryFind (id: Symbol<'T>) =
    if dict.used.[id.id] then
      Some dict.arr.[id.id]
    else
      None

type SymbolTable<'T when 'T: equality>(symbols: 'T seq) =
  let toID =
    Seq.mapi (fun id symbol -> symbol, Symbol<'T> id) symbols
    |> dict
  let toSymbol = Array.zeroCreate toID.Count
  do
    for kv in toID do
      toSymbol.[kv.Value.id] <- kv.Key
  member table.GetSymbol(id: Symbol<'T>) =
    toSymbol.[id.id]
  member table.Get(symbol: 'T) =
    toID.[symbol]
  member table.Dictionary() : SymbolDictionary<'T, 'a> =
    SymbolDictionary toSymbol.Length