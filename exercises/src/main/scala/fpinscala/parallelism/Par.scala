package fpinscala.parallelism

import java.util.concurrent._

object Par {
  type Par[A] = ExecutorService => Future[A]
  
  def run[A](s: ExecutorService)(a: Par[A]): Future[A] = a(s)

  def unit[A](a: A): Par[A] = (es: ExecutorService) => UnitFuture(a) // `unit` is represented as a function that returns a `UnitFuture`, which is a simple implementation of `Future` that just wraps a constant value. It doesn't use the `ExecutorService` at all. It's always done and can't be cancelled. Its `get` method simply returns the value that we gave it.

  def lazyUnit[A](a: => A): Par[A] = fork(unit(a))

  private case class UnitFuture[A](get: A) extends Future[A] {
    def isDone = true 
    def get(timeout: Long, units: TimeUnit) = get 
    def isCancelled = false 
    def cancel(evenIfRunning: Boolean): Boolean = false 
  }

  private case class Wait2Future[A,B,C](a: Future[A], b: Future[B], f: (A, B) => C) extends Future[C] {
    override def isCancelled: Boolean = a.isCancelled || b.isCancelled
    override def get(): C = getValue(Long.MaxValue)
    override def get(timeout: Long, unit: TimeUnit): C = getValue(unit.toMillis(timeout))
    override def cancel(mayInterruptIfRunning: Boolean): Boolean =
      a.cancel(mayInterruptIfRunning) || b.cancel(mayInterruptIfRunning)
    override def isDone: Boolean = a.isDone && b.isDone

    private[this] def getValue(timeoutMills: Long): C = {
      val aStartTime = System.currentTimeMillis()
      val va = a.get(timeoutMills, TimeUnit.MILLISECONDS)
      val aFinishTime = System.currentTimeMillis()
      val vb = b.get(timeoutMills - (aFinishTime - aStartTime), TimeUnit.MILLISECONDS)
      f(va, vb)
    }
  }

  def map2[A,B,C](a: Par[A], b: Par[B])(f: (A,B) => C): Par[C] = // `map2` doesn't evaluate the call to `f` in a separate logical thread, in accord with our design choice of having `fork` be the sole function in the API for controlling parallelism. We can always do `fork(map2(a,b)(f))` if we want the evaluation of `f` to occur in a separate thread.
    (es: ExecutorService) => {
      val af = a(es)
      val bf = b(es)
      Wait2Future(af, bf, f)
    }

  def map3[A,B,C,D](a: Par[A], b: Par[B], c: Par[C])(f: (A,B,C) => D): Par[D] = {
    val g = (va:A, vb:B) => (vc:C) => f(va, vb, vc)
    map2(map2(a, b)(g(_, _)), c)(_(_))
  }

  def map4[A,B,C,D,E](a: Par[A], b: Par[B], c: Par[C], d:Par[D])(f: (A,B,C,D) => E): Par[E] = {
    val g = (va: A, vb: B, vc: C) => (vd: D) => f(va, vb, vc, vd)
    map2(map3(a, b, c)(g(_, _, _)), d)(_(_))
  }

  def map[A,B](pa: Par[A])(f: A => B): Par[B] =
    map2(pa, unit(()))((a,_) => f(a))

  def sortPar(parList: Par[List[Int]]) = map(parList)(_.sorted)

  def fork[A](a: => Par[A]): Par[A] = // This is the simplest and most natural implementation of `fork`, but there are some problems with it--for one, the outer `Callable` will block waiting for the "inner" task to complete. Since this blocking occupies a thread in our thread pool, or whatever resource backs the `ExecutorService`, this implies that we're losing out on some potential parallelism. Essentially, we're using two threads when one should suffice. This is a symptom of a more serious problem with the implementation, and we will discuss this later in the chapter.
    es => es.submit(new Callable[A] { 
      def call = a(es).get
    })

  scala.concurrent.ExecutionContext.global

  def asyncF[A,B](f: A => B): A => Par[B] = (a: A) => lazyUnit(f(a))

  def sequence[A](ps: List[Par[A]]): Par[List[A]] =
    ps.foldRight(Par.unit(List.empty[A])) { (pa, acc) =>
      map2(pa, acc)(_ :: _)
    }

  def sequenceForIndexedSeq[A](ps: IndexedSeq[Par[A]]): Par[IndexedSeq[A]] = {
    if (ps.isEmpty) Par.unit(Vector.empty)
    else if (ps.length == 1) map(ps.head)(Vector(_))
    else {
      val (a, b) = ps.splitAt(ps.length / 2)
      map2(fork(sequenceForIndexedSeq(a)), fork(sequenceForIndexedSeq(b)))(_ ++ _)
    }
  }

  def parMap[A,B](ps: List[A])(f: A => B): Par[List[B]] =
    sequence(ps.map(asyncF(f)))

  def parFilter[A](as: List[A])(f: A => Boolean): Par[List[A]] =
    map(sequence(as.map(asyncF(a => if (f(a)) List(a) else Nil))))(_.flatten)

  def parFoldLeft[A,B](l: List[A], z: B)(f: (A, B) => B): Par[B] = l match {
    case Nil => Par.unit(z)
    case a::as => parFoldLeft(as, f(a, z))(f)
  }

  def paragraphWordCount(ps: List[String]): Par[List[Int]] =
    parMap(ps)(_.split("\\s").length)

  def equal[A](e: ExecutorService)(p: Par[A], p2: Par[A]): Boolean =
    p(e).get == p2(e).get

  def delay[A](fa: => Par[A]): Par[A] = 
    es => fa(es)

  def choice[A](cond: Par[Boolean])(t: Par[A], f: Par[A]): Par[A] =
    es => 
      if (run(es)(cond).get) t(es) // Notice we are blocking on the result of `cond`.
      else f(es)

  /* Gives us infix syntax for `Par`. */
  implicit def toParOps[A](p: Par[A]): ParOps[A] = new ParOps(p)

  class ParOps[A](p: Par[A]) {


  }
}

object Examples {
  import Par._
  def sum(ints: IndexedSeq[Int]): Par[Int] = // `IndexedSeq` is a superclass of random-access sequences like `Vector` in the standard library. Unlike lists, these sequences provide an efficient `splitAt` method for dividing them into two parts at a particular index.
    if (ints.size <= 1)
      Par.unit(ints.headOption getOrElse 0) // `headOption` is a method defined on all collections in Scala. We saw this function in chapter 3.
    else { 
      val (l,r) = ints.splitAt(ints.length/2) // Divide the sequence in half using the `splitAt` function.
      Par.map2(Par.fork(sum(l)), Par.fork(sum(r)))(_ + _) // Recursively sum both halves and add the results together.
    }

}

// where map(y)(id) == y ...
// map(map(y)(g))(f) == map(y)(f compose g)
