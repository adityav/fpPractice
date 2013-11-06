import scala.annotation.tailrec

/**
 * User: Aditya Vishwakarma
 * Date: 05/11/13
 * Time: 5:45 PM
 * Note: 
 */
object Chapter5 {
  trait Stream[+A] {
    import Stream._

    def uncons: Option[Cons[A]]
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
        case Some(c) =>
          buf += c.head
          _tl(c.tail)
        case None => buf.toList
      }
      _tl(this)
    }

    //Ex2
    def take(n: Int): Stream[A] = if (n > 0) {
      uncons match {
        case Some(c) => Stream.cons(c.head, c.tail.take(n - 1))
        case None => Stream.empty[A]
      }
    } else Stream.empty[A]

    //Ex3
    def takeWhile(p: A => Boolean):Stream[A] = {
      def _tw(s:Stream[A]):Stream[A] = {
        s.uncons match {
          case Some(c) if p(c.head) => Stream.cons(c.head, _tw(c.tail))
          case _ => Stream.empty[A]
        }
      }
      _tw(this)
    }

    //Ex4
    def forall(p: A => Boolean): Boolean = foldRight(true)((a, acc) => p(a) && acc)

    //Ex5 takeWhile using FoldRight
    def takeWhile2(p: A => Boolean): Stream[A] = {
      foldRight(empty[A])((a, acc) => if(p(a)) cons(a, acc) else empty)
    }
    //Ex6
    def map[B](f: A => B): Stream[B] = foldRight(empty[B])((h,t) => cons(f(h),t))
    def filter(f: A => Boolean):Stream[A] = foldRight(empty[A]){
      (h,t) =>
        if(f(h)) cons(h, t) else t
    }
    def append[B >: A](s: Stream[B]):Stream[B] = foldRight(s)((h,t) => cons(h,t))
    def flatMap[B](f: A => Stream[B]):Stream[B] = foldRight(empty[B])((h,t) => f(h) append t)


    //Misc
    def foldRight[B](z:B)(f:(A, => B) => B):B = uncons match {
      case Some(c) => f(c.head, c.tail.foldRight(z)(f))
      case _ => z
    }
    def exists(p: A => Boolean):Boolean = foldRight(false)((a, acc) => p(a) || acc)

    //Using unfold
    //Ex12
    def mapU[B](f: A => B):Stream[B] = unfold(this)(_.uncons match {
      case Some(c) => Some((f(c.head), c.tail))
      case _ => None
    })

    def takeU(n:Int) = unfold((n, this))(n => if(n._1 == 0) None else {
      n._2.uncons match {
        case Some(c) => Some(c.head, (n._1 - 1, c.tail))
        case _ => None
      }
    })
    def takeWhileU(p: A => Boolean) = unfold(this)( _.uncons match {
      case Some(c) if p(c.head) => Some(c.head, c.tail)
      case _ => None
    })

    def zip[B](s2:Stream[B]): Stream[(A,B)] = unfold((this, s2)){
      case (s1, s2) =>
        (s1.uncons, s2.uncons) match {
          case (Some(c1), Some(c2)) => Some((c1.head,c2.head), (c1.tail, c2.tail))
          case _ => None
        }
    }
    def zipAll[B](s2: Stream[B]):Stream[(Option[A], Option[B])] = {
      val a = this map (Some(_)) append constant(None)
      val b = s2 map (Some(_)) append constant(None)
      a.zip(b)
    }

    //Ex14
    def tails:Stream[Stream[A]] = unfold(this)(_.uncons match {
      case Some(c) => Some(c, c.tail)
      case _ => None
    }) append Stream.empty

    //Ex15
    def scanRight[B](z:B)(f:(A, => B) => B): Stream[B] =
    foldRight((z, Stream(z)))((a, p) => {
      val b = f(a, p._1)
      (b, cons(b, p._2))
    })._2

    def list5 = this.take(5).toList
  }

  object Empty extends Stream[Nothing] {
    val uncons = None
  }
  sealed abstract class Cons[+A] extends Stream[A] {
    def head: A
    def tail: Stream[A]
    val uncons = Some(this)
  }

  object Stream {
    def empty[A]: Stream[A] = Empty

    def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = new Cons[A] {
        lazy val head = hd
        lazy val tail = tl
      }
    def apply[A](as: A*): Stream[A] =
      if (as.isEmpty) empty
      else cons(as.head, apply(as.tail: _*))

    val ones:Stream[Int] = cons(1, ones)

    //ex7
    def constant[A](a:A): Stream[A] = new Cons[A] {
      lazy val head = a
      lazy val tail = this
    }
    //ex8 infinite stream of nos
    def from(n:Int):Stream[Int] = cons(n, from(n + 1))

    //ex9
    def fibs = {
      def go(a: Int, b: Int):Stream[Int] = cons(a, go(b, a + b))
      go(0,1)
    }

    //ex10
    def unfold[A,S](z:S)(f: S => Option[(A,S)]):Stream[A] = {
      f(z) match {
        case Some((h,t)) => cons(h, unfold(t)(f))
        case _ => empty[A]
      }
    }

    //Using Unfold
    //cons/ from/ fibs are easy

    //Ex13
    def startsWith[A](s:Stream[A], s2:Stream[A]):Boolean =
    s.zipAll(s2).takeWhile(_._2.isDefined).forall{
      case (Some(h), Some(h2)) if h == h2 => true
      case _ => false
    }


  }
}
