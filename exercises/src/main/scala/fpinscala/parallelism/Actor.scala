package fpinscala.parallelism

import java.util.concurrent.atomic.{AtomicInteger, AtomicReference}
import java.util.concurrent.{Callable,ExecutorService}
import annotation.tailrec

/*
 * Implementation is taken from `scalaz` library, with only minor changes. See:
 *
 * https://github.com/scalaz/scalaz/blob/scalaz-seven/concurrent/src/main/scala/scalaz/concurrent/Actor.scala
 *
 * This code is copyright Andriy Plokhotnyuk, Runar Bjarnason, and other contributors,
 * and is licensed using 3-clause BSD, see LICENSE file at:
 *
 * https://github.com/scalaz/scalaz/blob/scalaz-seven/etc/LICENCE
 */

/**
 * Processes messages of type `A`, one at a time. Messages are submitted to
 * the actor with the method `!`. Processing is typically performed asynchronously,
 * this is controlled by the provided `strategy`.
 *
 * Memory consistency guarantee: when each message is processed by the `handler`, any memory that it
 * mutates is guaranteed to be visible by the `handler` when it processes the next message, even if
 * the `strategy` runs the invocations of `handler` on separate threads. This is achieved because
 * the `Actor` reads a volatile memory location before entering its event loop, and writes to the same
 * location before suspending.
 *
 * Implementation based on non-intrusive MPSC node-based queue, described by Dmitriy Vyukov:
 * [[http://www.1024cores.net/home/lock-free-algorithms/queues/non-intrusive-mpsc-node-based-queue]]
 *
 * @see scalaz.concurrent.Promise for a use case.
 *
 * @param handler  The message handler
 * @param onError  Exception handler, called if the message handler throws any `Throwable`.
 * @param strategy Execution strategy, for example, a strategy that is backed by an `ExecutorService`
 * @tparam A       The type of messages accepted by this actor.
 */
final class Actor[A](strategy: Strategy)(handler: A => Unit, onError: Throwable => Unit = throw(_)) {
  self =>

  // tailだけど1つのNodeに対するAtomicReference？？？
  // => あああ、NodeはAtomicReference[Node]継承してた...
  private val tail = new AtomicReference(new Node[A]())
  private val suspended = new AtomicInteger(1)
  private val head = new AtomicReference(tail.get)

  /** Alias for `apply` */
  def !(a: A) {
    val n = new Node(a)
    // headに値をsetする???メッセージボックスの後ろに新しい値を追加するとかになってる？？？？？
    // - 元のheadの値を取得して.lazySet(n)
    // - 元のheadにはnを入れる
    // すまん、分からん (AtomicReference分からん)
    head.getAndSet(n).lazySet(n)
    trySchedule()
  }

  /** Pass the message `a` to the mailbox of this actor */
  def apply(a: A) {
    this ! a
  }

  def contramap[B](f: B => A): Actor[B] =
    new Actor[B](strategy)((b: B) => (this ! f(b)), onError)

  private def trySchedule() {
    // suspended == 1なら 0にセットしつつ、schedule を実行
    // suspended == 0なら何もしない
    if (suspended.compareAndSet(1, 0)) schedule()
  }

  private def schedule() {
    // strategyにactを渡す
    // 名前渡しのため、別スレッド上で評価されるとかもstrategyの実装によっては起こりえる
    strategy(act())
  }

  private def act() {
    val t = tail.get
    // handle処理(メッセージ受け取って何かするとか)を実行
    // tailのNodeを初めに渡す
    val n = batchHandle(t, 1024)
    if (n ne t) {
      n.a = null.asInstanceOf[A]
      // batchHandleし終わった後だから、全てのmailboxのを処理し終わった的な
      // だからn.a = null(末尾)のものをtailにセットする
      tail.lazySet(n)
      schedule()
    } else {
      suspended.set(1)
      if (n.get ne null) trySchedule()
    }
  }

  @tailrec
  private def batchHandle(t: Node[A], i: Int): Node[A] = {
    val n = t.get
    // nullになるのは、次のreferenceが無い <=> 最後尾のNode(=AtomicReference[Node])ってこった
    if (n ne null) {
      // 値を持ってるNode
      try {
        handler(n.a)
      } catch {
        case ex: Throwable => onError(ex)
      }
      // 特にnは何も変えないでそのまま渡したり返したりする？
      // あああ、n=t.getだから、自分が持ってるNodeのReferenceを次に渡してるんだ
      if (i > 0) batchHandle(n, i - 1) else n
    } else t // nullを持つNodeなら、そのままNode返す
  }
}

// NodeもAtomicReferenceでNode持ってる
private class Node[A](var a: A = null.asInstanceOf[A]) extends AtomicReference[Node[A]]

object Actor {

  /** Create an `Actor` backed by the given `ExecutorService`. */
  def apply[A](es: ExecutorService)(handler: A => Unit, onError: Throwable => Unit = throw(_)): Actor[A] =
    new Actor(Strategy.fromExecutorService(es))(handler, onError)
}

/**
 * Provides a function for evaluating expressions, possibly asynchronously.
 * The `apply` function should typically begin evaluating its argument
 * immediately. The returned thunk can be used to block until the resulting `A`
 * is available.
 */
trait Strategy {
  def apply[A](a: => A): () => A
}

object Strategy {

  /**
   * We can create a `Strategy` from any `ExecutorService`. It's a little more
   * convenient than submitting `Callable` objects directly.
   */
  def fromExecutorService(es: ExecutorService): Strategy = new Strategy {
    def apply[A](a: => A): () => A = {
      val f = es.submit { new Callable[A] { def call = a} }
      () => f.get
    }
  }

  /**
   * A `Strategy` which begins executing its argument immediately in the calling thread.
   */
  def sequential: Strategy = new Strategy {
    def apply[A](a: => A): () => A = {
      val r = a
      () => r
    }
  }
}

