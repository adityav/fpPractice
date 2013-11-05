import scala.annotation.tailrec

object Chapter1 {
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

object Chapter4 {

  //Ex3 combines two Option values using a binary function.
  //If either Option value is None, then the return value is None too
  def map2[A,B,C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] = {
    for {
      aa <- a
      bb <- b
    } yield f(aa,bb)
  }

  //Ex6 Single pass. if any result of f applied on a is None, end result is None as well
  def traverse[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] = {

/*    def _t(l:List[A], acc: List[B]):Option[List[B]] = l match {
      case Nil => Some(acc.reverse)
      case x :: xs =>
        f(x).map( y =>  _t(xs, y :: acc)) getOrElse None
    }
    _t(a, List.empty[B])*/
    a.foldRight[Option[List[B]]](Some(List.empty[B]))((elem, acc) => map2(f(elem), acc)(_ :: _))
  }


}