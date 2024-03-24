package u03

import u02.Modules.Person
import u03.Optionals.Optional

import scala.annotation.tailrec

object Sequences: // Essentially, generic linked lists

  enum Sequence[E]:
    case Cons(head: E, tail: Sequence[E])
    case Nil()

  object Sequence:

    def sum(l: Sequence[Int]): Int = l match
      case Cons(h, t) => h + sum(t)
      case _          => 0

//    def map[A, B](l: Sequence[A])(mapper: A => B): Sequence[B] = l match
//      case Cons(h, t) => Cons(mapper(h), map(t)(mapper))
//      case Nil()      => Nil()
//
//    def filter[A](l1: Sequence[A])(pred: A => Boolean): Sequence[A] = l1 match
//      case Cons(h, t) if pred(h) => Cons(h, filter(t)(pred))
//      case Cons(_, t)            => filter(t)(pred)
//      case Nil()                 => Nil()

    // Lab 03
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

    def min(l: Sequence[Int]): Optional[Int] = l match
      case Cons(h, t) => Optional.Just(Math.min(h, Optional.orElse(min(t), h)))
      case _          => Optional.Empty()

    def getCourses(s: Sequence[Person]): Sequence[String] =
      flatMap(s)({
        case Person.Teacher(_, c) => Sequence.Cons(c, Sequence.Nil())
        case _                    => Sequence.Nil()
      })

    @tailrec
    def foldLeft[A, B](l: Sequence[A])(v: B)(accumulator: (B, A) => B): B = l match
      case Cons(h, t) => foldLeft(t)(accumulator(v, h))(accumulator)
      case _          => v

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

@main def trySequences(): Unit =
  import Sequences.*
  val l = Sequence.Cons(10, Sequence.Cons(20, Sequence.Cons(30, Sequence.Nil())))
  println(Sequence.sum(l)) // 60

  import Sequence.*

  println(sum(map(filter(l)(_ >= 20))(_ + 1))) // 21+31 = 52
