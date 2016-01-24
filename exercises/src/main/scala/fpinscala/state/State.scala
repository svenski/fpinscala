
trait RNG {
  def nextInt: (Int, RNG) // Should generate a random `Int`. We'll later define other functions in terms of `nextInt`.
}

object RNG {
  // NB - this was called SimpleRNG in the book text

  case class Simple(seed: Long) extends RNG {
    def nextInt: (Int, RNG) = {
      val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL // `&` is bitwise AND. We use the current seed to generate a new seed.
      val nextRNG = Simple(newSeed) // The next state, which is an `RNG` instance created from the new seed.
      val n = (newSeed >>> 16).toInt // `>>>` is right binary shift with zero fill. The value `n` is our new pseudo-random integer.
      (n, nextRNG) // The return value is a tuple containing both a pseudo-random integer and the next `RNG` state.
    }
  }

  type Rand[+A] = RNG => (A, RNG)

  val int: Rand[Int] = _.nextInt

  def unit[A](a: A): Rand[A] =
    rng => (a, rng)

  def map[A,B](s: Rand[A])(f: A => B): Rand[B] =
    rng => {
      val (a, rng2) = s(rng)
      (f(a), rng2)
    }

  def nonNegativeInt(rng: RNG): (Int, RNG) = rng.nextInt match {
    case (Int.MinValue, nextRng) => nonNegativeInt(nextRng)
    case(int: Int, nextRng: RNG) => (math.abs(int), nextRng)
  }

  // Double between 0 and 1
  def double(rng: RNG): (Double, RNG) = nonNegativeInt(rng) match {
    case (Int.MaxValue, nRng) => double(nRng)
    case(int, nRng) => (int/Int.MaxValue.toDouble, nRng)
  }

  //
  def intDouble(rng: RNG): ((Int,Double), RNG) = {
    //rng.nextInt.map ( (int, rr) => (int, double(rr)))
    val (int, rr)  = rng.nextInt
    val (dd, rr2) = double(rr)

    ((int, dd), rr2)
  }

  def doubleInt(rng: RNG): ((Double,Int), RNG) = {
    val ((i,d),r) = intDouble(rng)
    ((d,i), r)
  }

  def double3(rng: RNG): ((Double,Double,Double), RNG) = {
    val (dd, r) = double(rng)
    val (dd2, r1) = double(r)
    val (dd3, r2) = double(r1)

    ((dd, dd2, dd3), r2)
  }

  def ints(count: Int)(rng: RNG): (List[Int], RNG) = {

    def go(count: Int, acc: List[Int])(rng: RNG) : (List[Int], RNG) = (count, rng.nextInt) match {
      case (0, _) => (acc, rng)
      case (x, (int, rr)) => go(x -1, int::acc)(rr)
    }

    go(count, Nil: List[Int])(rng)
  }

  def doubleUsingMap: Rand[Double] = {
    map(int)( _/(Int.MaxValue.toDouble +1))
  }

  def map2[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = {
    rng => {
      val (a, rng1) = ra(rng)
      val (b, rng2) = rb(rng1)
      (f(a,b), rng2)
    }
  }

  def both[A,B](ra: Rand[A], rb: Rand[B]): Rand[(A,B)] = 
    map2(ra,rb)((_, _))

  def intDouble2: Rand[(Int,Double)] = 
    both(int, doubleUsingMap)

  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] = {
    
    def go(in: List[Rand[A]], acc: Rand[List[A]]): Rand[List[A]] = in match {
      case h::t => go(t, map2(h, acc) ((a,b) => a::b))
      case Nil => acc
    }

    go(fs, rng => (Nil:List[A], rng))
  }

  // Opposite direction!
  def intsUsingSeq(n: Int): Rand[List[Int]] = sequence(List.fill(n)(int))

  def flatMap[A,B](f: Rand[A])(g: A => Rand[B]): Rand[B] = {
    rng => {
      val (a, r1) = f(rng)
      g(a)(r1)
    }
  }

  def nonNegativeLessThan(x: Int) : Rand[Int] = {

    def nn(x: Int)(in: Int): Rand[Int] = {
      rng => {
        val mod = in % x
        if (in + (x-1) - mod >= 0)
          (mod, rng)
        else nonNegativeLessThan(x)(rng)
      }
    }

    flatMap(int)(nn(x))
  }

  def mapUsingFlatMap[A, B](s: Rand[A])(f: A => B): Rand[B] = 
    flatMap(s)( x => rng => (f(x), rng))

  def doubleUsingMap2: Rand[Double] = {
    mapUsingFlatMap(int)( _/(Int.MaxValue.toDouble +1))
  }
}


case class State[S,+A](run: S => (A, S)) {
  def map[B](f: A => B): State[S, B] =
    sys.error("todo")
  def map2[B,C](sb: State[S, B])(f: (A, B) => C): State[S, C] =
    sys.error("todo")
  def flatMap[B](f: A => State[S, B]): State[S, B] =
    sys.error("todo")
}

sealed trait Input
case object Coin extends Input
case object Turn extends Input

case class Machine(locked: Boolean, candies: Int, coins: Int)

object State {
  type Rand[A] = State[RNG, A]
  def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] = ???
}
