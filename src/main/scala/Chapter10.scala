/**
 * User: Aditya Vishwakarma
 * Date: 07/11/13
 * Time: 10:32 PM
 * Note: 
 */
object Chapter10 {

  trait Monoid[A] {
    def op(a: A, b: A): A
    def zero: A
  }

  def dual[A](m:Monoid[A]) = new Monoid[A] {
    def op(a:A, b:A) = m.op(b,a)
    val zero = m.zero
  }

  //Ex5
  def wordsMonoid: Monoid[String] = new Monoid[String] {
    def op(a:String, b:String) = (a.trim + " " + b.trim).trim //outer trim for law to hold.
    def zero = ""
  }

  //Ex 6
  def concatenate[A](as: List[A], m: Monoid[A]): A =  (m.zero /: as)(m.op)

  //Ex 7
  def foldMap[A,B](as: List[A], m: Monoid[B])(f: A => B): B = concatenate(as map f, m)

  def foldMap_withFold[A,B](as: List[A], m: Monoid[B])(f: A => B): B = {
    (m.zero /: as)((b,a) => m.op(f(a), b))
  }

  def foldRight_withfoldMap[A,B](as:List[A])(z:B)(f: (A,B) => B):B = {
    foldMap(as, endoMonoid[B])(f.curried)(z)
  }
  def foldLeft_withfoldMap[A,B](as:List[A])(z:B)(f: (B,A) => B):B = {
    foldMap(as, dual(endoMonoid[B]))(a => b => f(b,a))(z)
  }

  //World count

  sealed trait WC
  case class Stub(chars: String) extends WC
  case class Part(lStub: String, words: Int, rStub: String) extends WC

  //Ex9
  val wcMonoid: Monoid[WC] = new Monoid[WC] {
    def op(l:WC, r:WC):WC = {
      (l,r) match {
        case (Stub(a), Stub(b)) => Stub(a + b)
        case (p@Part(_,_,rStub), Stub(b)) => p.copy(rStub = rStub + b)
        case (Stub(a), p@Part(lStub,_,_)) => p.copy(lStub = a + lStub)
        case (a:Part, b:Part) =>
          if((a.rStub + b.lStub).length > 0) {
          Part(a.lStub, a.words + b.words + 1, b.rStub)
          } else Part(a.lStub, a.words + b.words, b.rStub)
      }
    }
    val zero = Stub("")
  }

  //Ex10
  def wordCounter(s:String, wc:Monoid[WC]) = {
    def wChar(c:Char) = if(c.isWhitespace) Part("", 0, "") else Stub(c.toString)

    (wc.zero /: s)((w, c) => wc.op(w, wChar(c)))
  }

  //Trying a crazy homomophism
  def magicFunc[A,B](f:A => B)(m: Monoid[B]): List[A] => B = {
    l: List[A] =>
      (m.zero /: (l map f))(m.op)
  }

  //Ex11
  def foldMapV[A,B](v: IndexedSeq[A], m: Monoid[B])(f: A => B): B = {
    if(v.length == 0) m.zero
    else if(v.length == 1) f(v(0))
    else {
      val (left, right) = v.splitAt( v.length / 2)
      m.op(foldMapV(left, m)(f), foldMapV(right, m)(f))
    }
  }

  sealed trait Order
  case class UnOrdered(value: Int) extends Order
  case class Ascending(minVal: Int, maxVal: Int) extends Order
  case class Descending(minVal: Int, maxVal: Int) extends Order
  object NoOrder extends Order

  //Ex12 is list ordered
  /*def isOrdered(v: IndexedSeq[Int]): Boolean = {
    //Boolean determines ordering is acending(T), or descending (Min, Max)
    val intVectorOrdering = new Monoid[Order] {
      def op(a: Order, b: Order) = {
        (a,b) match {
          case (NoOrder, _) => NoOrder
          case (_, NoOrder) => NoOrder
          case (UnOrdered(lv), UnOrdered(rv)) if lv > rv => Descending(rv, lv)
          case (UnOrdered(lv), UnOrdered(rv)) if lv < rv => Ascending(rv, lv)
          case (UnOrdered(lv), UnOrdered(rv)) if lv < rv => UnOrdered(rv)

          case (Ascending(lmin, lMax), UnOrdered(rv)) if lMax <= rv => Ascending(lmin, rv)
          case (UnOrdered(rv), Ascending(lmin, lMax)) if rv <= lmin => Ascending(rv, lMax)

          case (Descending(lmin, lMax), UnOrdered(rv)) if lmin >= rv => Descending(lmin, rv)
          case (UnOrdered(rv), Descending(lmin, lMax)) if rv >= lMax => Descending(rv, lMax)

          case (_, _) => NoOrder
        }

      }
      val zero = UnOrdered
    }
    foldMapV(v, intVectorOrdering)(x => Some(UnOrdered(x))).isDefined
  }*/


  val stringMonoid = new Monoid[String] {
    def op(a:String,b:String)= a ++ b
    val zero = ""
  }
  def listMonoid[A] = new Monoid[List[A]] {
    def op(a:List[A],b:List[A])= a ++ b
    val zero = Nil
  }
  //Ex1
  val intAddition: Monoid[Int] = new Monoid[Int] {
    def op(a:Int,b:Int)= a + b
    val zero = 0
  }
  val intMultiplication: Monoid[Int] = new Monoid[Int] {
    def op(a:Int,b:Int)= a * b
    val zero = 1
  }
  val booleanOr: Monoid[Boolean] = new Monoid[Boolean] {
    def op(a:Boolean,b:Boolean)= a || b
    val zero = false
  }
  val booleanAnd: Monoid[Boolean] = new Monoid[Boolean] {
    def op(a:Boolean,b:Boolean)= a && b
    val zero = true
  }

  //Ex2
  def optionMonoid[A]: Monoid[Option[A]] = new Monoid[Option[A]] {
    def op(a:Option[A],b:Option[A])= a.orElse(b)
    val zero:Option[A] = None
  }
  //Ex3
  def endoMonoid[A]: Monoid[A => A] = new Monoid[A => A] {
    def op(a:A => A,b:A => A)= a andThen b
    val zero:A => A = id => id
  }

  //Foldables
  trait Foldable[F[_]] {

    def foldRight[A, B](as: F[A])(z:B)(f: (A, B) => B): B =
      foldMap(as)(f.curried)(endoMonoid[B])(z)

    def foldLeft[A, B](as: F[A])(z:B)(f: (B, A) => B): B =
      foldMap(as)(a => (b:B) => f(b,a))(dual(endoMonoid[B]))(z)

    def foldMap[A, B](as: F[A])(f: A => B)(mb: Monoid[B]): B =
      foldLeft(as)(mb.zero)((acc,e) => mb.op(acc,f(e)))

    def concatenate[A](as: F[A])(m: Monoid[A]): A =
      foldLeft(as)(m.zero)(m.op)

    def toList[A](as:F[A]):List[A] = foldRight(as)(List.empty[A])(_ :: _)
  }
  val foldableList = new Foldable[List] {
    override def foldRight[A, B](as: List[A])(z:B)(f: (A, B) => B): B = as.foldRight(z)(f)
    override def foldLeft[A, B](as: List[A])(z:B)(f: (B, A) => B): B = as.foldLeft(z)(f)
  }

  val foldableIndexSeq = new Foldable[IndexedSeq] {
    override def foldMap[A, B](as: IndexedSeq[A])(f: A => B)(mb: Monoid[B]): B = foldMapV(as, mb)(f)
  }
  val foldableOption = new Foldable[Option] {
    override def foldLeft[A, B](as:Option[A])(z:B)(f: (B, A) => B): B = as match {
      case Some(a) => f(z,a)
      case _ => z
    }
  }

  //Monoid Composition
  def productMonoid[A,B](a: Monoid[A], b: Monoid[B]): Monoid[(A,B)] = new Monoid[(A,B)] {
    def op(l:(A,B), r:(A,B)) =(a.op(l._1, r._1), b.op(l._2, r._2))
    val zero = (a.zero, b.zero)
  }

  def mapMergeMonoid[K, V](V: Monoid[V]): Monoid[Map[K, V]] =
    new Monoid[Map[K, V]] {
      def zero = Map.empty[K,V]

      def op(a: Map[K, V], b: Map[K, V]) =
        a.map {
          case (k, v) => (k, V.op(v, b.get(k) getOrElse V.zero))
        }
    }

  //Ex19
  def functionMonoid[A,B](b: Monoid[B]): Monoid[A => B] = new Monoid[A => B] {
    def op(l: A => B, r: A => B) = a => b.op(l(a), r(a))
    def zero = a => b.zero
  }

  //Frequency of elements in the bag
  def bag[A](bag:IndexedSeq[A]):Map[A, Int] =
    foldMapV(bag, mapMergeMonoid[A,Int](intAddition))(a => Map(a -> 1))


  //Ex20
  def frequencyMap(strings: IndexedSeq[String]): Map[String, Int] = bag(strings)



}
