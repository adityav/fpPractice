import scala.annotation.tailrec

/**
 * User: Aditya Vishwakarma
 * Date: 05/11/13
 * Time: 5:45 PM
 * Note: 
 */
object Chapter5 {
  trait Stream[+A] {
    def uncons: Option[(A, Stream[A])]
    def isEmpty: Boolean = uncons.isEmpty

    //Ex1 to List
//    def toList:List[A] = uncons match {
//      case Some(c) => c._1 :: c._2.toList
//      case None => Nil
//    }
//    def toList:List[A] = {
//      @tailrec
//      def _tl(s:Stream[A], acc:List[A]):List[A] = s.uncons match {
//        case Some((hd,tl)) => _tl(tl, hd :: acc)
//        case None => acc
//      }
//      _tl(this, List.empty[A])
//    }
    //Best version
    def toList:List[A] = {
      //Note list buffer. Not ArrayBuffer. ListBuffer to List is constant time.
      val buf = collection.mutable.ListBuffer.empty[A]
      @tailrec
      def _tl(s:Stream[A]):List[A] = s.uncons match {
        case Some((hd,tl)) =>
          buf += hd
          _tl(tl)
        case None => buf.toList
      }
      _tl(this)
    }

    //Ex2
    def take(n: Int): Stream[A] = if (n > 0) {
      uncons match {
        case Some((hd, tl)) => Stream.cons(hd, tl.take(n - 1))
        case None => Stream.empty[A]
      }
    } else Stream.empty[A]

    //Ex3
    def takeWhile(p: A => Boolean):Stream[A] = {
      def _tw(s:Stream[A]):Stream[A] = {
        uncons match {
          case Some((hd, tl)) => if(p(hd)) Stream.cons(hd, _tw(tl)) else Stream.empty[A]
          case None => Stream.empty[A]
        }
      }
      _tw(this)
    }
  }

  object Stream {
    def empty[A]: Stream[A] =
      new Stream[A] { def uncons = None }
    def cons[A](hd: => A, tl: => Stream[A]): Stream[A] =
      new Stream[A] {
        lazy val uncons = Some((hd, tl))
      }
    def apply[A](as: A*): Stream[A] =
      if (as.isEmpty) empty
      else cons(as.head, apply(as.tail: _*))
  }

}
