functor MkBigNumAdd(structure U : BIGNUM_UTIL) : BIGNUM_ADD =
struct
  structure Util = U
  open Util
  open Seq

  infix 6 ++
  exception NotYetImplemented
  datatype carry = GEN | PROP | STOP

  fun x ++ y =
    let
      (* pass info through scan *)
	  (* return a sequence of carry *)
      fun tupleadd (left, right) : carry =
          if right = GEN then GEN
		  else if right = STOP then if left = GEN then PROP else STOP
		  else (* right = PROP *) if left = GEN then GEN else PROP
      (* combine middle and carry *)
      fun genbit (x, c) : bit = case c of
	                                 (STOP|PROP) => if x=PROP then ONE else ZERO
								   | GEN => if x=PROP then ZERO else ONE
      (* compute the initial sequence from x and y *)
      fun multi (place : int) : bit * bit = let
                                              val lenx = Seq.length x
                                              val leny = Seq.length y
                                            in
                                              if (lenx < leny) then if (place < length x) then (Seq.nth x place, Seq.nth y place)
                                                                                          else (ZERO, Seq.nth y place)
                                              (* lenx > leny *)else if (place < length y) then (Seq.nth x place, Seq.nth y place)
                                                                                          else (Seq.nth x place, ZERO)
                                            end
      fun bittocarry (x : bit, y : bit) : carry =
          case (x, y) of
               (ONE, ONE) => GEN
             | (ZERO, ZERO) => STOP
             | (_, _) => PROP
      val middle = map bittocarry (tabulate multi (Int.max (length x, length y)))
      (* scan to find each bit *)
      val (carries, flag) = Seq.scan tupleadd STOP middle
      val final = if ((Seq.length x) = 0 andalso (Seq.length y) = 0) then empty () else map2 genbit middle carries
      (* make sure xn is not zero *)
    in
      if (length final) = 0 then (empty () : bit seq)
      else if flag = GEN then Seq.append (final, (Seq.singleton ONE))
      else final
    end
    (* raise NotYetImplemented *)
    
  val add = op++
end
