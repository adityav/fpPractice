/**
 * User: Aditya Vishwakarma
 * Date: 05/11/13
 * Time: 2:32 PM
 */
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

  //Either
  sealed trait Either[+E, +A] {

    def map[B](f: A => B): Either[E, B] = {
      this match {
        case Left(value) => Left(value)
        case Right(value) => Right(f(value))
      }
    }

    def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] =  this match {
      case Left(value) => Left(value)
      case Right(value) => f(value)
    }

    def orElse[EE >: E,B >: A](b: => Either[EE, B]): Either[EE, B] = this match {
      case Left(_) => b
      case Right(xb) => this
    }

    def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] =
      for {
      aa <- this
      bb <- b
    } yield f(aa,bb)
  }

  case class Left[+E](value: E) extends Either[E, Nothing]
  case class Right[+A](value: A) extends Either[Nothing, A]

  //Ex8 Sequence and traverse for either
  def traverseE[A,B,E](a:List[A])(f: A => Either[E,B]): Either[E,List[B]] = {
    a.foldRight[Either[E,List[B]]](Right(Nil))((x,y) => f(x).map2(y)(_ :: _))
  }

  def sequenceE[B,E](a:List[Either[E,B]]):Either[E,List[B]] = traverseE(a)(x => x)

}