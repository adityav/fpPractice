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
  def EndoMonoid[A]: Monoid[A => A] = new Monoid[A => A] {
    def op(a:A => A,b:A => A)= a andThen b
    val zero:A => A = id => id
  }

}
