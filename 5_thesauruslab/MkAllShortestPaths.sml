functor MkAllShortestPaths (Table : TABLE) : ALL_SHORTEST_PATHS =
struct
  open Table
  open Seq

  (* Table.key defines our vertex type *)
  type vertex = key
  type edge = vertex * vertex

  exception NYI
  (* You must define the following two types and
   * explain your decision here with comments.
   *)
  type graph = vertex seq table
  type asp = graph

  (* Task 2.1 *)
  fun makeGraph (E : edge seq) : graph =
    Table.collect E

  (* Task 2.2 *)
  fun numEdges (G : graph) : int =
    let
      fun add (sum : int, (v : vertex, s : vertex seq)) =
        sum + (Seq.length s)
    in
      Table.iter add 0 G
    end

  fun numVertices (G : graph) : int =
    let
      val A = Seq.reduce Set.union (Set.empty ()) (Table.range (Table.map Set.fromSeq G))
      val B = Table.domain G
    in
      Set.size (Set.union (A, B))
    end

  (* Task 2.3 *)
  fun outNeighbors (G : graph) (v : vertex) : vertex seq =
    case (Table.find G v)
      of (SOME x) => x
       | NONE => Seq.empty ()

  (* Task 2.4 *)
  fun makeASP (G : graph) (v : vertex) : asp =
    case (Table.find G v)
      of NONE => (Table.empty ())
       | (SOME f1) => let
                       (* X : visited edges seq, F : frontiers set, N : out-edges seq of F *)
                       val X = ref (Seq.empty ())
                       val Y = ref (Set.empty ())
                       val F = ref (Set.singleton v)
                       fun addParent (p : vertex, S : vertex seq) : edge seq = Seq.map (fn x => (x,p)) S
                       val GwithParent = Table.mapk addParent G (* key -> (vertex, its parent key) *)
                       val N = ref (case Table.find GwithParent v
                                      of (SOME x) => x
                                       | NONE => Seq.empty())
                     in
                       while ((Set.size (!F)) > 0) do 
                       (
                        X := Seq.append (!X, !N);
                        Y := Set.union (!Y, !F);
                        F := let
                               val temp = Table.reduce Set.union (Set.empty ()) (Table.map Set.fromSeq (Table.filterk (fn (k, _) => Set.find (!F) k) G))
                             in
                               Set.difference (temp, !Y)
                             end;
                        N := let
                               val rawSeq = Table.reduce Seq.append (Seq.empty ()) (Table.filterk (fn (k, _) => Set.find (!F) k) GwithParent)
                               fun filt (a, _) : bool = case (Set.find (Set.union (!F, !Y)) a)
                                                          of true => false
                                                           | false => true
                             in
                               Seq.filter filt rawSeq
                             end
                       );
                       (Table.insert (fn (x, y) => y) (v, Seq.empty()) (Table.collect (!X)))
                     end

  (* Task 2.5 *)
  fun report (A : asp) (v : vertex) : vertex seq seq =
    let
      fun addVtoAns (v : vertex, S : vertex seq seq) =
        Seq.map (fn x => Seq.append (Seq.singleton v, x)) S
      fun f (G : graph) (curV : vertex) (ans : vertex seq seq) =
        case (Table.find G curV)
          of NONE => Seq.empty () (* reach dead end *)
           | (SOME vertexSeq) => case (Seq.showl vertexSeq)
                                   of NIL => addVtoAns(curV, ans) (* reach the source *)
                                    | CONS(a, b) => let
                                                      val next : vertex seq seq = addVtoAns (curV, ans)
                                                    in
                                                      Seq.flatten (Seq.map (fn x => f G x next) vertexSeq)
                                                    end
    in
      case (Table.find A v)
        of NONE => Seq.empty()
        (* x : vertex seq, for each vertex in x (parent of v), map it into a vertex seq seq *)
         | (SOME x) => Seq.flatten (Seq.map (fn y => f A y (Seq.singleton (Seq.singleton v))) x)
    end
                      
end
