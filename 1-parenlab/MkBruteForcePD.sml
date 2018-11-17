functor MkBruteForcePD (structure P : PAREN_PACKAGE) : PAREN_DIST =
struct
  structure P = P
  open P
  open Seq
  open Option210

  fun parenDist (parens : paren seq) : int option =
      let
        fun ismatched (subp : paren seq) =
            let
              fun fp (((NONE, x), _) | ((SOME 0, x), CPAREN)) = (NONE, x)
                | fp ((SOME c, x), CPAREN) = if ((c-1)=0) then (SOME (c-1), x+1)
                                             else (SOME (c-1), x)
                | fp ((SOME c, x), OPAREN) = (SOME (c+1), x)
            in
              iter fp (SOME 0, 0) subp = (SOME 0, 1)
            end

        fun maxp (start : int, l : int, maxlength : int option) : int option =
            if (start < 0) then maxlength
            else
              if ((start+l)>Seq.length parens) then maxp (start-1, 1, maxlength)
              else
                let
                  (* compare two int options, return the bigger one *)
                  fun cmp (SOME x, SOME y) : int option =
                      if (x>y) then (SOME x)
                      else (SOME y)
                    | cmp (NONE, SOME y) = if (y=0) then NONE else (SOME y)
                    | cmp (SOME x, NONE) = if (x=0) then NONE else (SOME x)
                    | cmp (_, _) = NONE
                  
                  val somemax = if ismatched (Seq.subseq parens (start, l)) then (SOME l)
                                else NONE
                in
                  maxp (start, l+1, cmp (somemax, maxlength))
                end
                  
      in
        maxp (((Seq.length parens)-1), 1, NONE)
      end

end
