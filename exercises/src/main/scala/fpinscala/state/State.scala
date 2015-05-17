package fpinscala.state


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

  def boolean(rng: RNG): (Boolean, RNG) =
    rng.nextInt match { case (i,rng2) => (i%2==0,rng2) }

  def unit[A](a: A): Rand[A] =
    rng => (a, rng)

  def map[A,B](s: Rand[A])(f: A => B): Rand[B] =
    rng => {
      val (a, rng2) = s(rng)
      (f(a), rng2)
    }

  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    val (v, nextRng) = rng.nextInt
    (if (v < 0) -(v + 1) else v, nextRng)
  }

  def nonNegativeEven: Rand[Int] =
    map(nonNegativeInt)(i => i - i % 2)

  // [0,1) のDouble型を返す
  def double(rng: RNG): (Double, RNG) = {
    val (v, nextRng) = nonNegativeInt(rng)
    (v / (Int.MaxValue.toDouble + 1), nextRng)
  }

  val doubleWithMap = map(nonNegativeInt)(_ / (Int.MaxValue.toDouble + 1))

  def intDouble(rng: RNG): ((Int,Double), RNG) = {
    val (v1, rng1) = rng.nextInt
    val (v2, rng2) = double(rng1)
    ((v1, v2), rng2)
  }

  def doubleInt(rng: RNG): ((Double,Int), RNG) = {
    val ((v1, v2), nextRng) = intDouble(rng)
    ((v2, v1), nextRng)
  }

  def double3(rng: RNG): ((Double,Double,Double), RNG) = {
    val (v1, rng1) = double(rng)
    val (v2, rng2) = double(rng1)
    val (v3, rng3) = double(rng2)
    ((v1, v2, v3), rng3)
  }

  def ints(count: Int)(rng: RNG): (List[Int], RNG) =
    if (count <= 0) (Nil, rng)
    else {
      val (v, rng1) = rng.nextInt
      val (ls, rng2) = ints(count - 1)(rng1)
      (v :: ls, rng2)
    }

  def map2[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = {
    (rng: RNG) =>
      val (a, rng1) = ra(rng)
      val (b, rng2) = rb(rng1)
      (f(a, b), rng2)
  }

  def both[A, B](ra: Rand[A], rb: Rand[B]): Rand[(A, B)] = map2(ra, rb)((_, _))

  val randIntDouble: Rand[(Int, Double)] = both(int, double)

  val randDoubleInt: Rand[(Double, Int)] = both(double, int)

  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] = fs match {
    case Nil => unit(Nil)
    case rng::rngs => map2(rng, sequence(rngs))(_ :: _)
  }

  def sequenceWithFoldRight[A](fs: List[Rand[A]]): Rand[List[A]] =
    fs.foldRight(unit(List.empty[A]))((rnd, z) => map2(rnd, z)(_ :: _))

  // List.fill(n)(x)
  def intsWithSequence(count: Int)(rng: RNG): (List[Int], RNG) =
    sequence(List.fill(count)(int))(rng)

  def flatMap[A,B](f: Rand[A])(g: A => Rand[B]): Rand[B] = { rng: RNG =>
    val (a, rng1) = f(rng)
    g(a)(rng1)
  }

  def nonNegativeLessThan(n: Int): Rand[Int] =
    flatMap(nonNegativeInt) { i =>
      val mod = i % n
      if (i + (n-1) - mod >= 0) unit(mod)
      else nonNegativeLessThan(n)
    }

  def mapWithFlatMap[A,B](s: Rand[A])(f: A => B): Rand[B] = flatMap(s)(v => unit(f(v)))

  def map2WithFlatMap[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
    flatMap(ra)(a => map(rb)(b => f(a, b)))
}

case class State[S,+A](run: S => (A, S)) {
  import State._

  def map[B](f: A => B): State[S, B] = flatMap(a => unit(f(a)))
  def map2[B,C](sb: State[S, B])(f: (A, B) => C): State[S, C] = flatMap(a => sb.map(b => f(a,b)))
  def flatMap[B](f: A => State[S, B]): State[S, B] = State { s =>
    val (a, s1) = run(s)
    f(a).run(s1)
  }
}

sealed trait Input
case object Coin extends Input
case object Turn extends Input

/**
 *
 * @param locked Turnによりロックがかかり、Coinによりロックが外れる
 * @param candies 残りスナックの個数
 * @param coins 投入済み金額
 */
case class Machine(locked: Boolean, candies: Int, coins: Int)

object State {
  type Rand[A] = State[RNG, A]

  def unit[S, A](a: A): State[S, A] = State(s => (a, s))

  def sequence[S, A](fs: List[State[S, A]]): State[S, List[A]] = fs.foldRight(unit[S, List[A]](Nil)) { (zs, acc) =>
    zs.map2(acc){ (s, ls) => s :: ls }
  }

  def modify[S](f: S => S): State[S, Unit] = for {
    s <- get
    _ <- set(f(s))
  } yield ()

  def get[S]: State[S, S] = State(s => (s, s))

  def set[S](s: S): State[S, Unit] = State(_ => ((), s))
}

object Machine {
  import State._

  // (coin, candies)を状態に持つ
  type MachineState = State[Machine, (Int, Int)]

  /**
   * 硬貨10枚、スナック5個の自動販売機から2個のスナックを購入すると、(12, 3)が返る
   * Machine.simulateMachine(List(Coin, Turn, Coin, Turn)).run(Machine(true, 5, 10)) --> (12, 3)
   * @param inputs
   * @return
   */
  def simulateMachine(inputs: List[Input]): MachineState = for {
    v <- sequence(inputs.map { input =>
      modify { machine: Machine =>
        action(input, machine)
      }
    })
    s <- get
  } yield (s.coins, s.candies)

  private val action: PartialFunction[(Input, Machine), Machine] = {
    case (Coin, machine@Machine(_, 0, _)) => machine
    case (Coin, machine@Machine(false, _, _)) => machine
    case (Turn, machine@Machine(true, _, _)) => machine
    case (Coin, Machine(true, candies, coins)) => Machine(false, candies, coins + 1)
    case (Turn, Machine(false, candies, coins)) => Machine(true, candies - 1, coins)
  }
}
