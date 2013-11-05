object Chapter2 {
  ///Ex2

  def isSorted[A](list:List[A], gt:(A, A) => Boolean):Boolean = {
    def _sorted(l:List[A]):Boolean = {
      if(l.length == 1) true
      else l match {
        case Nil => true
        case x :: xs  => gt(x, xs.head) && _sorted(xs)
      }
    }
    _sorted(list)
  }

  //Ex3
  def partial1[A,B,C](a:A, f:(A,B) => C): B => C = f(a, _)
}

