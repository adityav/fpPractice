/**
 * User: Aditya Vishwakarma
 * Date: 14/11/13
 * Time: 9:27 PM
 * Note: 
 */

/**
 * Applicative functors
 * which define map2 and unit. Less powerful than monad, more composable.
 */
import Chapter11.{Monad, Functor}
object Chapter12 {

  trait Applicative[F[_]] extends Functor[F] {
    self =>
    def map2[A,B,C](fa: F[A], fb: F[B])(f: (A, B) => C): F[C] =
      apply(apply(unit(f.curried))(fa))(fb)

    def map3[A,B,C,D](fa: F[A], fb: F[B], fc:F[C])(f: (A,B,C) => D): F[D] =
      apply(apply(apply(unit(f.curried))(fa))(fb))(fc)

    def apply[A,B](fab: F[A => B])(fa: F[A]): F[B] = map2(fab,fa)(_(_))

    def unit[A](a: A): F[A]

    def map[A,B](a:F[A])(f: A => B):F[B] = apply[A,B](unit(f))(a)

    def sequence[A](fas: List[F[A]]): F[List[A]] = traverse(fas)(x => x)

    def traverse[A,B](as: List[A])(f: A => F[B]): F[List[B]] =
      (as :\ unit(List.empty[B]))((a,acc) => map2(f(a),acc)(_::_))

    def replicateM[A](n: Int, fa: F[A]): F[List[A]] = sequence(List.fill(n)(fa))

    def factor[A,B](fa: F[A], fb: F[B]): F[(A,B)] = map2(fa,fb)((_,_))

    def product[G[_]](G: Applicative[G]):
    Applicative[({type f[x] = (F[x], G[x])})#f] = new Applicative[({type f[x] = (F[x], G[x])})#f] {
      def unit[A](a: A): (F[A], G[A]) = (self.unit(a), G.unit(a))
      override def apply[A,B](fab: (F[A => B], G[A => B]))(fa: (F[A],G[A])): (F[B],G[B]) =
        (self.apply(fab._1)(fa._1),G.apply(fab._2)(fa._2))
    }
    def compose[G[_]](G: Applicative[G]): Applicative[({type f[x] = F[G[x]]})#f] =
    new Applicative[({type f[A] = F[G[A]]})#f] {
      def unit[A](a: A): F[G[A]] = self.unit(G.unit(a))
      override def map2[A,B,C](fa:F[G[A]],fb:F[G[B]])(f:(A,B) => C):F[G[C]] =
        self.map2(fa,fb)((ga,gb) => G.map2(ga,gb)(f))
    }
  }

  def eitherMonad[E]: Monad[({type f[x] = Either[E, x]})#f] = new Monad[({type f[x] = Either[E, x]})#f] {
    def unit[x](a:x):Either[E, x] = Right(a)

    def flatMap[x, B](ma: Either[E, x])(f: (x) => Either[E, B]): Either[E, B] = ma match {
      case Left(a) => Left(a)
      case Right(a) => f(a)
    }
  }

  sealed trait Validation[+E,+A]
  case class Failure[E](head:E, tail:Vector[E]) extends Validation[E,Nothing]
  case class Success[A](a:A) extends Validation[Nothing,A]

  def validationApplicative[E] = new Applicative[({type lambda[A] = Validation[E,A]})#lambda] {
    override def unit[A](a:A) = Success(a)
    override def map2[A,B,C](fa: Validation[E,A],
                    fb: Validation[E,B])(f: (A, B) => C): Validation[E,C] =
      (fa,fb) match {
        case (Success(a), Success(b)) => Success(f(a,b))
        case (Success(_), Failure(head,tail)) => Failure(head,tail)
        case (Failure(head,tail),Success(_)) => Failure(head,tail)
        case (Failure(lhead,ltail),Failure(rhead,rtail)) => Failure(rhead,rtail ++ (lhead +: ltail))
      }

  }
}
