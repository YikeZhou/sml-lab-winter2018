functor MkRangeCount(structure OrdTable : ORD_TABLE) : RANGE_COUNT =
struct
  structure OrdTable = OrdTable
  open OrdTable

  (* Ordered table type: *)
  type 'a table = 'a table
  type 'a seq = 'a Seq.seq
  type point = Key.t * Key.t

  (* Use this to compare x- or y-values. *)
  val compareKey : (Key.t * Key.t) -> order = Key.compare

  (* Define this yourself *)
  type countTable = (Key.t table) table

  fun makeCountTable (S : point seq) : countTable =
    let
      fun sortPoint ((a, b) : point, (c, d) : point) : order = compareKey (a, c)
      val sortedS = Seq.sort sortPoint S
      fun inserting ((T : countTable, prev : Key.t table), (x, y) : point) =
        let
          val next = OrdTable.insert (fn (x, y) => y) (y, y) prev
          val temp = OrdTable.insert (fn (x, y) => y) (x, next) T
        in
          (temp, next)
        end
      val (finT, _) = Seq.iter inserting ((OrdTable.empty ()), (OrdTable.empty ())) sortedS
    in
      finT
    end

  fun count (T : countTable)
                   ((xLeft, yHi) : point, (xRght, yLo) : point) : int  =
    let
      val l = case OrdTable.previous T xLeft
                of (SOME (k', v)) => v
                 | (NONE) => OrdTable.empty()
      val r = case OrdTable.find T xRght
                of (SOME x) => x
                 | NONE => (case OrdTable.previous T xRght
                              of (SOME (k', v)) => v
                               | (NONE) => OrdTable.empty())
      val mid = OrdTable.erase (r, Set.fromSeq (range l))
      val ans = OrdTable.getRange mid (yLo, yHi)
    in
      OrdTable.size ans
    end
        
end