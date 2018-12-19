functor MkBigNumMultiply(structure BNA : BIGNUM_ADD
                         structure BNS : BIGNUM_SUBTRACT
                         sharing BNA.Util = BNS.Util) : BIGNUM_MULTIPLY =
struct
  structure Util = BNA.Util
  open Util
  open Seq
  open Primitives

  infix 6 ++ --
  infix 7 **

  fun x ++ y = BNA.add (x, y)
  fun x -- y = BNS.sub (x, y)
  fun x ** y = 
   let
     (* compare and insert zeros *)
     fun generateZero ( m : int ) : bit seq = let fun f ( x : int ) : bit = ZERO
                                             in Seq.tabulate f m
                                             end

     val n = Int.max (Seq.length x, Seq.length y)
     val newy = if (length x > length y) then Seq.append (y, generateZero (Seq.length x - Seq.length y)) else y
     val newx = if (length y > length x) then Seq.append (x, generateZero (Seq.length y - Seq.length x)) else x
     (* split x and y into p and q, r and s
      * let A = pr, B = qs, C = ps + rq = ( p + q ) * ( r + s ) - A - B
      * then x ** y = A * 2^(low*2) + C * 2^low + B *)
	 val low = n div 2
	 fun deletezero (rawseq : bit seq)=
	    let
		  fun countzero ((flag : bool, i : int), x : bit) = if flag then if x=ZERO then (flag, i+1) else (false, i)
		                                                            else (flag, i)
		  val (_, num) = iter countzero (true, 0) (rev rawseq)
        in take (rawseq, (length rawseq)-num) end
     val p = deletezero(drop ( newx, n div 2 ))(* left (high) part length : n - n div 2 *)
     val q = deletezero(take ( newx, n div 2 ))(* right part length : n div 2 *)
     val r = deletezero(drop ( newy, n div 2 ))
     val s = deletezero(take ( newy, n div 2 ))
	 val zeroSequence1 = generateZero (low * 2)
	 val zeroSequence2 = generateZero low
   in
     case (length x, length y) of
		  (0, 0) => empty()
		| (0, _) => empty()
		| (_, 0) => empty()
		| (_, 1) => x
		| (1, _) => y
		| (_, _) => let
		              val (A, B, C) = par3 (fn _ => p**r, fn _ => q**s, fn _ => (p ++ q) ** (r ++ s))
					in
					  append(zeroSequence2, C -- A -- B) ++ B ++ append(zeroSequence1, A)
					end
   end

  val mul = op**
end
