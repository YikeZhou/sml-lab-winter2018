functor MkBigNumSubtract(structure BNA : BIGNUM_ADD) : BIGNUM_SUBTRACT =
struct
  structure Util = BNA.Util
  open Util
  open Seq

  exception NotYetImplemented
  infix 6 ++ --
  fun x ++ y = BNA.add (x, y)
  fun x -- y =
    let
      fun reverse (num : bit) : bit = if num = ONE then ZERO else ONE
	  fun hold (x : int) : bit = ONE
	  val n : int = (length x)-(length y)
	  val head : bit seq = tabulate hold n
      val revy = append ((map reverse y), head)
      val newy = revy ++ Seq.singleton (ONE)
	  val answithzero = take (x ++ newy, length x)
	  fun deletezero (rawseq : bit seq)=
	    let
		  fun countzero ((flag : bool, i : int), x : bit) = if flag then if x=ZERO then (flag, i+1) else (false, i)
		                                                            else (flag, i)
		  val (_, num) = iter countzero (true, 0) (rev rawseq)
        in take (rawseq, (length rawseq)-num) end
    in deletezero answithzero end
      
  val sub = op--
end
