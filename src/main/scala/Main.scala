import u02.Modules.Person
import u03.Optionals.Optional

import scala.annotation.tailrec

object Sequences:

  enum Sequence[E]:
    case Cons(head: E, tail: Sequence[E])
    case Nil()

  object Sequence:

    def sum(l: Sequence[Int]): Int = l match
      case Cons(h, t) => h + sum(t)
      case _          => 0

    // Task 1, svolto da solo
    def take[A](l: Sequence[A])(n: Int): Sequence[A] = l match
      case Cons(h, t) if n > 0 => Cons(h, take(t)(n - 1))
      case _                   => Nil()

    def zip[A, B](first: Sequence[A], second: Sequence[B]): Sequence[(A, B)] = (first, second) match
      case (Cons(hf, tf), Cons(hs, ts)) => Cons((hf, hs), zip(tf, ts))
      case _                            => Nil()

    def concat[A](l1: Sequence[A], l2: Sequence[A]): Sequence[A] = (l1, l2) match
      case (Cons(h1, t1), _) => Cons(h1, concat(t1, l2))
      case (_, Cons(h2, t2)) => Cons(h2, t2)
      case _                 => Nil()

    def flatMap[A, B](l: Sequence[A])(mapper: A => Sequence[B]): Sequence[B] = l match
      case Cons(h, t) => concat(mapper(h), flatMap(t)(mapper))
      case _          => Nil()

    def map[A, B](l: Sequence[A])(mapper: A => B): Sequence[B] =
      flatMap(l)(a => Cons(mapper(a), Nil()))

    def filter[A](l1: Sequence[A])(pred: A => Boolean): Sequence[A] =
      flatMap(l1)({
        case a if pred(a) => Cons(a, Nil())
        case _            => Nil()
      })

    // Task 2, svolto da solo
    def min(l: Sequence[Int]): Optional[Int] = l match
      case Cons(h, t) => Optional.Just(Math.min(h, Optional.orElse(min(t), h)))
      case _          => Optional.Empty()

    // Task 3, svolto da solo
    def getCourses(s: Sequence[Person]): Sequence[String] =
      flatMap(s)({
        case Person.Teacher(_, c) => Sequence.Cons(c, Sequence.Nil())
        case _                    => Sequence.Nil()
      })

    // Task 4, svolto da solo
    @tailrec
    def foldLeft[A, B](l: Sequence[A])(v: B)(accumulator: (B, A) => B): B = l match
      case Cons(h, t) => foldLeft(t)(accumulator(v, h))(accumulator)
      case _          => v

    // Task 5, svolto da solo
    extension [A](l: Sequence[A])
      def takeExt(n: Int): Sequence[A] = l match
        case Cons(h, t) if n > 0 => Cons(h, t.takeExt(n - 1))
        case _                   => Nil()

      def zipExt[B](l2: Sequence[B]): Sequence[(A, B)] = (l, l2) match
        case (Cons(h1, t1), Cons(h2, t2)) => Cons((h1, h2), t1.zipExt(t2))
        case _                            => Nil()

      def concatExt(l2: Sequence[A]): Sequence[A] = (l, l2) match
        case (Cons(h1, t1), _) => Cons(h1, t1.concatExt(l2))
        case (_, Cons(h2, t2)) => Cons(h2, t2)
        case _                 => Nil()

      def flatMapExt[B](mapper: A => Sequence[B]): Sequence[B] = l match
        case Cons(h, t) => mapper(h).concatExt(t.flatMapExt(mapper))
        case _          => Nil()

      def mapExt[B](mapper: A => B): Sequence[B] =
        l.flatMapExt(a => Cons(mapper(a), Nil()))

      def filterExt(pred: A => Boolean): Sequence[A] =
        l.flatMapExt({
          case a if pred(a) => Cons(a, Nil())
          case _            => Nil()
        })

      def foldLeftExt[B](v: B)(accumulator: (B, A) => B): B = l match
        case Cons(h, t) => t.foldLeftExt(accumulator(v, h))(accumulator)
        case _          => v

    extension (l: Sequence[Int])
      def sumExt: Int = l match
        case Cons(h, t) => h + t.sumExt
        case _          => 0

      def minExt: Optional[Int] = l match
        case Cons(h, t) => Optional.Just(Math.min(h, Optional.orElse(t.minExt, h)))
        case _          => Optional.Empty()

    extension (l: Sequence[Person])
      def getCoursesExt: Sequence[String] =
        l.flatMapExt({
          case Person.Teacher(_, c) => Sequence.Cons(c, Sequence.Nil())
          case _                    => Sequence.Nil()
        })

object Streams:

  import Sequences.*

  enum Stream[A]:
    private case Empty()
    private case Cons(head: () => A, tail: () => Stream[A])

  object Stream:

    def empty[A](): Stream[A] = Empty()

    def cons[A](hd: => A, tl: => Stream[A]): Stream[A] =
      lazy val head = hd
      lazy val tail = tl
      Cons(() => head, () => tail)

    def toList[A](stream: Stream[A]): Sequence[A] = stream match
      case Cons(h, t) => Sequence.Cons(h(), toList(t()))
      case _          => Sequence.Nil()

    def map[A, B](stream: Stream[A])(f: A => B): Stream[B] = stream match
      case Cons(head, tail) => cons(f(head()), map(tail())(f))
      case _                => Empty()

    def filter[A](stream: Stream[A])(pred: A => Boolean): Stream[A] = stream match
      case Cons(head, tail) if pred(head()) => cons(head(), filter(tail())(pred))
      case Cons(head, tail)                 => filter(tail())(pred)
      case _                                => Empty()

    def take[A](stream: Stream[A])(n: Int): Stream[A] = (stream, n) match
      case (Cons(head, tail), n) if n > 0 => cons(head(), take(tail())(n - 1))
      case _                              => Empty()

    def iterate[A](init: => A)(next: A => A): Stream[A] =
      cons(init, iterate(next(init))(next))

    // Task 6, svolto da solo
    def takeWhile[A](stream: Stream[A])(pred: A => Boolean): Stream[A] = stream match
      case Cons(head, tail) if pred(head()) => cons(head(), takeWhile(tail())(pred))
      case _                                => Empty()

    // Task 7, svolto da solo
    def fill[A](n: Int)(k: A): Stream[A] = n match
      case n if n > 0 => cons(k, fill(n - 1)(k))
      case _          => Empty()

    // Task 8, svolto da solo
    def pell: Stream[Int] =
      def _pell(a: Int, b: Int): Stream[Int] =
        cons(a, _pell(b, 2 * b + a))
      _pell(0, 1)
