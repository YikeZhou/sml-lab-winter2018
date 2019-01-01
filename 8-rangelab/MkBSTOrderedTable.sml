functor MkBSTOrderedTable (structure Tree : BSTREE
                           structure Seq : SEQUENCE) : ORD_TABLE =
struct

  structure Table = MkBSTTable(structure Tree = Tree
                               structure Seq = Seq)

  (* Include all the functionalities of the standard Table *)
  open Table

  (* This is defined after "open" so it doesn't get overwritten *)
  structure Key = Tree.Key
  type key = Key.t

  (* Remember, type 'a table = 'a Tree.bst *)

  fun first (T : 'a table) : (key * 'a) option =
    case Tree.expose T
	  of NONE => NONE
	   | SOME {key, value, left, right} => case Tree.expose left
	                                         of NONE => SOME(key, value)
											  | SOME _ => first left

  fun last (T : 'a table) : (key * 'a) option =
    case Tree.expose T
	  of NONE => NONE
	   | SOME {key, value, left, right} => case Tree.expose right
	                                         of NONE => SOME(key, value)
											  | SOME _ => last right
		      
  fun previous (T : 'a table) (k : key) : (key * 'a) option =
    let
	  val (left, middle, right) = Tree.splitAt (T, k)
	in
	  last left
	end

  fun next (T : 'a table) (k : key) : (key * 'a) option =
    let
	  val (left, middle, right) = Tree.splitAt (T, k)
	in
	  first right
	end

  fun join (L : 'a table, R : 'a table) : 'a table =
    Tree.join(L, R)

  fun split (T : 'a table, k : key) : 'a table * 'a option * 'a table =
    Tree.splitAt(T, k)

  fun getRange (T : 'a table) (low : key, high : key) : 'a table =
    let
	  val (L1, M1, R1) = Tree.splitAt(T, low)
	  val (L2, M2, R2) = Tree.splitAt(R1, high)
	  fun f (k1, k2) = k2
	  val lowvalue = find T low
	  val highvalue = find T high
	in
	  case (lowvalue, highvalue)
	    of (NONE, NONE) => L2
		 | (NONE, SOME y) => insert f (low, y) L2
		 | (SOME x, NONE) => insert f (high, x) L2
		 | (SOME x, SOME y) => insert f (low, y) (insert f (high, x) L2)
	end

end
