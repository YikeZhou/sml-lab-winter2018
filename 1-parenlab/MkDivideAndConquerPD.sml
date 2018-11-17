functor MkDivideAndConquerPD (structure P : PAREN_PACKAGE) : PAREN_DIST =
struct
  structure P = P
  open Primitives
  open P
  open Seq
  open Option210


  fun parenDist (parens : paren seq) : int option =
      let
        fun pm s =
            case showt s
            (* max length of the sub-problem, unpaired cparen, unpaired oparen, length from left to last impaired cparen, length from first impaired oparen to right *)
             of EMPTY => (0, 0, 0, 0, 0)
              | ELT OPAREN => (0, 0, 1, 0, 1)
              | ELT CPAREN => (0, 1, 0, 1, 0)
              | NODE (ll, rr) =>
                let
                  val ((left, i, j, lefthead, lefttail), (right, k, l, righthead, righttail)) =
                    par (fn () => pm ll, fn () => pm rr)
                in
                  if (j=k)
                  then let val nextmax = Int.max (lefttail+righthead, Int.max (left, right))
                       in (nextmax, i, l, lefthead, righttail)
                       end
                  else if (j<k)
                  then let val nextmax = Int.max ((lefttail+righthead-k+j), Int.max (left, right))
                       in (nextmax, i+k-j, l, righthead+left+i+j, righttail)
                       end
                  else let val nextmax = Int.max ((lefttail+righthead-j+k), Int.max (left, right))
                       in (nextmax, i, l+j-k, lefthead, lefttail+right+k+l)
                       end
                end
        
        val (max, _, _, _, _) = pm parens
      in
        if max=0 then NONE
        else (SOME max)
      end
      
end
