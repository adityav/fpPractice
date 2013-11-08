/**
 * User: Aditya Vishwakarma
 * Date: 06/11/13
 * Time: 3:34 PM
 * Note: 
 */
object Chapter6 {

  trait RNG  {
    def nextInt: (Int, RNG)
  }

  type Rand[+A] = RNG => (A, RNG)


  //A Stupid RNG

  object RNG {

    def simple(seed: Long): RNG = new RNG {
      def nextInt = {
        val seed2 = (seed*0x5DEECE66DL + 0xBL) &
          ((1L << 48) - 1)
        ((seed2 >>> 16).asInstanceOf[Int],
          simple(seed2))
      }
    }

    //Based on RAND. Older exercise at the end
    val int : Rand[Int] = _.nextInt
    def unit[A](a:A):Rand[A] = rng => (a, rng)
    def map[A,B](s:Rand[A])(f:A => B):Rand[B] = {
      rng =>
        val (e, r) = s(rng)
        (f(e), r)
    }

    //Ex5 positiveMax using map
    def positiveMax(n: Int): Rand[Int]  = map(positiveInt)(_ / n)

    val positiveInt = map(int)(i => if(i < 0) -(i + 1) else i)

    //Ex6 double
    val double: Rand[Double] = map(positiveInt)( _ / (Int.MaxValue.toDouble + 1))

    //Ex7 map2
    def map2[A,B,C](ra: Rand[A], rb:Rand[B])(f: (A, B) => C): Rand[C] = {
      rng =>
        val (e1, r1) = ra(rng)
        val (e2, r2) = rb(r1)
        (f(e1,e2), r2)
    }

    val intDouble: Rand[(Int, Double)] = map2(int, double)((_,_))
    val doubleInt: Rand[(Double, Int)] = map2(double, int)((_,_))

    //Ex8
//    def sequence[A](fs: List[Rand[A]]): Rand[List[A]] = {
//      rng =>
//        fs.foldRight((List.empty[A], rng))((e, r) => {
//          val (e1, r1) = e(r._2)
//          (e1 :: r._1, r1)
//        })
//    }
    def sequence[A](fs: List[Rand[A]]): Rand[List[A]] = {
      fs.foldRight(unit(List.empty[A]))((e, r) => map2(e,r)( _ :: _))
    }
    def ints(count: Int) = sequence(List.fill(count)(int))

    //Ex9
    def flatMap[A,B](f: Rand[A])(g: A => Rand[B]): Rand[B] = {
      rng =>
        val (e, r) = f(rng)
        g(e)(r)
    }

    def mapFM[A,B](s:Rand[A])(f:A => B):Rand[B] = flatMap(s)(e => unit(f(e)))
    def map2FM[A,B,C](ra: Rand[A], rb:Rand[B])(f: (A, B) => C): Rand[C] = {
      flatMap(ra)(e => mapFM(rb)(x => f(e,x)))
    }




    object Stupid {
      //Ex1
      def positiveInt(rng: RNG):(Int, RNG) = {
        val (i, r) = rng.nextInt
        (if(i < 0) -(i + 1) else i, r)
      }
      //Ex2
      def double(rng:RNG): (Double, RNG) = {
        val (i, r) = positiveInt(rng)
        (i / (Int.MaxValue.toDouble + 1), r)
      }

      //ex3
      def intDouble(rng: RNG): ((Int,Double), RNG) = {
        val (i, r) = rng.nextInt
        val (d, r2) = double(r)
        ((i,d), r2)
      }
      def doubleInt(rng: RNG): ((Double,Int), RNG) = {
        val ((i, d), r) = intDouble(rng)
        ((d, i), r)
      }
      def double3(rng: RNG): ((Double,Double,Double), RNG) = {
        val (d1, r1) = double(rng)
        val (d2, r2) = double(r1)
        val (d3, r3) = double(r2)
        ((d1, d2, d3), r3)
      }
      //ex4
      def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
        def tr(count: Int, r: RNG, xs: List[Int]):(List[Int], RNG) = {
          if(count == 0) (xs, r)
          else {
            val (i,r1) = r.nextInt
            tr(count -1, r1, i :: xs)
          }
        }
        tr(count, rng, List.empty[Int])
      }
    }
  }

  //State Action machine
  case class State[S, +A](run: S => (S, A)) {
    def flatMap[B](g: A => State[S,B]): State[S,B] = State(s => {
      val (s1, a1) = run(s)
      g(a1).run(s1)
    })

    def map[B](f:A => B):State[S,B] = flatMap(e => State.unit(f(e)))
    def map2[B,C](rb:State[S,B])(f: (A, B) => C): State[S,C] = {
      flatMap(e => rb.map(x => f(e,x)))
    }
  }
  object State {
    def unit[S, A](a:A): State[S,A] = State(s => (s, a))

    def sequence[S,A](fs: List[State[S,A]]): State[S,List[A]] = {
      fs.foldRight(unit[S,List[A]](List.empty[A]))((e, r) => e.map2(r)( _ :: _))
    }

    def get[S]: State[S,S] = State(s => (s,s))
    def set[S](s:S): State[S, Unit] = State(_ => (s, ()))

    def modify[S](f: S => S): State[S, Unit] = for {
      s <- get
      _ <- set(f(s))
    } yield ()

    sealed trait Input
    case object Coin extends Input
    case object Turn extends Input

    case class Machine(locked:Boolean, candies:Int, coins:Int)

    def simulateMachine(inputs:List[Input]):State[Machine,(Int,Int)] = {
      def turns(i:Input) = {
        s:Machine => (i,s) match {
          case (Coin, Machine(true, c, p )) if c > 0 => Machine(false, c, p + 1)
          case (Turn, Machine(false, c, p)) if c > 0 => Machine(true, c - 1, p)
          case (_, m) => m
        }
      }
      for {
        _ <- sequence(inputs.map(i => modify(turns(i))))
        s <- get

      } yield (s.coins, s.candies)
    }
  }

}
