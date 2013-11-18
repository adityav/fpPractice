import Functools.Monad
import scala.annotation.tailrec

/**
 * User: Aditya Vishwakarma
 * Date: 16/11/13
 * Time: 2:09 PM
 * Note: 
 */
object Chapter15 {

  sealed trait Process[I, O]  {
    import Process._

    def map[O2](f: O => O2):Process[I,O2] = this match {
      case Halt() => Halt()
      case Emit(head,tail) => Emit(head map f, tail map f)
      case Await(recv,fb) => Await(recv andThen(_ map f),fb map f)
    }

    def ++(p: => Process[I,O]):Process[I,O] = this match {
      case Halt() => p
      case Emit(head,tail) => Process.emitAll(head, tail ++ p)
      case Await(recv,fb) => Await(recv andThen(_ ++ p), fb ++ p)
    }


    //A sample driver from stream
    def apply(s:Stream[I]):Stream[O] = this match {
      case Halt() => Stream.empty[O]
      case Emit(head,tail) => head.toStream append tail(s)
      case Await(recv,finalizer) => s match {
        case h #:: t => recv(h)(t)
        case _ => finalizer(s)
      }
    }

    def flatMap[O2](f: O => Process[I,O2]): Process[I,O2] = this match {
      case Halt() => Halt()
      case Emit(h, t) =>
        if (h.isEmpty) t flatMap f
        else f(h.head) ++ Process.emitAll(h.tail, t).flatMap(f)
      case Await(recv,fb) =>
        Await(recv andThen (_ flatMap f), fb flatMap f)
    }
    //pipe operator
    def |>[O2](p2: Process[O,O2]): Process[I,O2] = p2 match {
      case Halt() => Halt()
      case Emit(h,t) => Emit(h,this |> t)
      case Await(recv,fb) => this match {
        case Halt() => Halt() |> fb
        case Emit(h,t) => t |> p2.feed(h)
        case Await(rec,gb) => Await(rec.andThen(_ |> p2),gb |> fb)
      }
    }

    def repeat:Process[I,O] = {
      def go(p:Process[I,O]):Process[I,O] = p match {
        case Halt() => go(this)
        case Emit(h,t) => Emit(h,go(t))
        case Await(recv,fb) => Await(recv andThen go, fb)
      }
      go(this)
    }

    def feed(s:Seq[I]):Process[I,O] = {
      @tailrec
      def go(in:Seq[I], state:Process[I,O]):Process[I,O] = {
        state match {
          case Halt() => Halt()
          case Await(recv,fb) => if(in.nonEmpty) go(in.tail,recv(in.head)) else state
          case Emit(h,t) => Emit(h, t.feed(in))
        }
      }
      go(s,this)
    }

    def zipWithIndex:Process[I,(O,Int)] = {
      Process.zip(this, Process.count)
    }
  }

  object Process {
    implicit def toMonadic[I,O](a: Process[I,O]) = monad[I].toMonadic(a)

    def Id[I]:Process[I,I] = lift(i => i)

    def emitAll[I,O](head: Seq[O],
                     tail: Process[I,O] = Halt[I,O]()): Process[I,O] =
      tail match {
        case Emit(h2, tl) => Emit(head ++ h2, tl)
        case _ => Emit(head, tail)
      }
    def emit[I,O](head: O,
                  tail: Process[I,O] = Halt[I,O]()): Process[I,O] =
      emitAll(Vector(head), tail)

    def lift[I,O](f: I => O):Process[I,O] = {
      Await(i => emit(f(i),lift(f)))
    }
    def filter[I](f: I => Boolean):Process[I,I] = {
      Await[I,I](i => if (f(i)) emit(i) else Halt()).repeat
    }

    def sum: Process[Double,Double] = {
      def go(acc: Double): Process[Double,Double] =
        Await((d: Double) => emit(d+acc, go(d+acc)))
      go(0.0)
    }
    //Ex2

    def take[I](n:Int):Process[I,I] = {
      if(n > 0) Await(i =>emit(i, take(n-1))) else Halt()
    }

    def drop[I](n:Int):Process[I,I] = {
      if(n > 0) Await(i => drop[I](n-1)) else Id
    }

    def takeWhile[I](f: I => Boolean):Process[I,I] = {
      Await(i => if(f(i)) emit(i, takeWhile(f)) else Halt())
    }
    def dropWhile[I](f: I => Boolean):Process[I,I] = {
      Await(i => if(f(i)) takeWhile(f) else Id)
    }

    //Ex3
    def count[I]:Process[I,Int] = {
      lift((i:I) => 1.0) |> sum |> lift(_.toInt)
    }
    //Ex4
    def mean: Process[Double,Double] = {
      def go(sum: Double, count: Double): Process[Double,Double] =
        Await((d: Double) => emit((sum+d) / (count+1), go(sum+d,count+1)))
      go(0.0, 0.0)
    }

    def loop[S,I,O](z: S)(f: (I,S) => (O,S)): Process[I,O] =
      Await((i: I) => f(i,z) match {
        case (o,s2) => emit(o, loop(s2)(f))
      })

    def sumL:Process[Double,Double] = loop(0.0)((in,acc) => (acc + in, acc + in))
    def countL[I]:Process[I,Int] = loop(0)((i,acc) => (acc+1,acc + 1))

    //End if either pa or pa ends
    def zip[I,O,O2](pa:Process[I,O], pb:Process[I,O2]):Process[I,(O,O2)] = (pa,pb) match {
      case (_,Halt()) => Halt()
      case (Halt(),_) => Halt()
      case (Emit(ha,ta), Emit(hb,tb)) =>
        //need to take care about different sizes of ha,hb
        val (zipped, lefta, leftb) = (ha.zip(hb), ha.drop(hb.length), hb.drop(ha.length))
        Emit(zipped, zip(
          if(lefta.isEmpty) ta else Emit(lefta,ta),
          if(leftb.isEmpty) tb else Emit(leftb,tb)
        ))
      case (Await(recva,fa), Await(recvb,fb)) => Await(i => zip(recva(i), recvb(i)),zip(fa,fb))
      case (Await(recv,fa), _) => Await(i => zip(recv(i),pb),zip(fa,pb))
      case (_,Await(recv,fb)) => Await(i => zip(pa,recv(i)),zip(pa,fb))
    }

    //Constant stream of truth values. If true once, rest of result are true always
    def exists[I](f: I => Boolean):Process[I,Boolean] = {
       loop(false)((i,acc) => (acc || f(i), acc || f(i)))
    }
    //Halt after finding true once
    def exists2[I](f: I => Boolean):Process[I,Boolean] = {
       Await(i => if(!f(i)) emit(false, exists2(f)) else emit(true, Halt()))
    }
    //Alternate to above
    def exists22[I](f: I => Boolean):Process[I,Boolean] = {
      def m1:Process[Boolean,Boolean] = Await[Boolean,Boolean](i => if(i) emit(true, Halt()) else emit(false,m1))
       exists(f) |> m1
    }


    def monad[I] = new Monad[({type f[O] = Process[I,O]})#f] {
      def unit[O](o: => O): Process[I,O] = emit(o)

      def flatMap[A, B](a: => Process[I,A])(f: (A) => Process[I,B]): Process[I,B] = a.flatMap(f)
    }
  }

  case class Emit[I, O](
                         head: Seq[O],
                         tail: Process[I, O] = Halt[I, O]())
    extends Process[I, O]

  case class Await[I, O](
                          recv: I => Process[I, O],
                          fallback: Process[I, O] = Halt[I, O]())
    extends Process[I, O]

  case class Halt[I, O]() extends Process[I, O]


}
