structure Tests =
struct

  val tests = List.map ArraySequence.% [
    [(1,1,2)],
    [(1,1,3),(2,1,4)],
    [(4,5,20),(1,3,5),(2,4,6),(8,7,11),(12,11,13),(10,10,14),(17,2,21)],
    [(20,10000,90000)],
    [(1,11,5), (2,6,7), (3,13,9), (12,7,16), (14,3,25),(19,18,22), (23,13,29), (24,4,28)],
    [],
    [(2,9,10),(3,7,15),(5,9,12),(16,7,24),(19,8,27)]
    
  ]

end