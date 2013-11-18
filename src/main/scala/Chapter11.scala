import Chapter6.State

/**
 * User: Aditya Vishwakarma
 * Date: 09/11/13
 * Time: 9:14 PM
 * Note: 
 */
object Chapter11 {
  //Functors
  trait Functor[F[_]] {
    def map[A,B](fa: F[A])(f: A => B):F[B]

    def distribute[A,B](fa:F[(A,B)]):(F[A],F[B]) = (map(fa)(_._1), map(fa)(_._2))
  }

  val listFunctor = new Functor[List] {
    def map[A,B](fa:List[A])(f: A => B):List[B] = fa map f
  }

  //Monad
  trait Monad[M[_]] extends Functor[M] {
    def unit[A](a:A):M[A]
    def flatMap[A,B](ma:M[A])(f: A => M[B]):M[B]

    def map[A,B](ma:M[A])(f: A => B):M[B]= flatMap(ma)(x => unit(f(x)))
    def map2[A,B,C](ma:M[A], mb:M[B])(f:(A,B) => C):M[C] = flatMap(ma)(a => map(mb)(b => f(a,b)))

    def sequence[A](ms:List[M[A]]):M[List[A]] =
      (ms :\ unit(List.empty[A]))((e,l) => map2(e,l)(_ :: _))

    def traverse[A,B](ms:List[A])(f:A => M[B]):M[List[B]] =
      (ms :\ unit(List.empty[B]))((e,l) => map2(f(e),l)(_ :: _))

    def replicateM[A](n:Int, ma:M[A]):M[List[A]] = sequence(List.fill(n)(ma))

    def factor[A,B](ma:M[A], mb:M[B]):M[(A,B)] = map2(ma,mb)((_,_))
    def cofactor[A,B](e:Either[M[A],M[B]]):M[Either[A,B]] = e match {
      case Left(a) => map(a)(x => Left[A,B](x))
      case Right(b) => map(b)(x => Right[A,B](x))
    }
  }

  def stateMonad[S] = new Monad[({type lambda[A] = State[S,A]})#lambda] {
    def unit[A](a:A) = State(s => (s,a))
    def flatMap[A,B](ma:State[S,A])(f: A => State[S,B]):State[S,B] = ma.flatMap(f)
  }

  object Monad {
    val listMonad = new Monad[List] {
      def unit[A](a:A):List[A] = List(a)
      def flatMap[A,B](ma:List[A])(f: A => List[B]):List[B] = ma.flatMap(f)
    }
    val optionMonad = new Monad[Option] {
      def unit[A](a:A):Option[A] = Option(a)
      def flatMap[A,B](ma:Option[A])(f: A => Option[B]):Option[B] = ma.flatMap(f)
    }
  }

  case class Reader[R, A](run: R => A)
  object Reader {
    def readerMonad[R] = new Monad[({type f[x] = Reader[R,x]})#f] {
      def unit[A](a: A): Reader[R,A] = Reader(r => a)
      def flatMap[A,B](st: Reader[R,A])(f: A => Reader[R,B]): Reader[R,B] = Reader{
        r =>
          val a = st run r
          f(a) run r
      }
    }
  }

}
