import Sequences.Sequence
import Sequences.Sequence.*
import Streams.Stream
import org.junit.*
import org.junit.Assert.*
import u02.Modules.Person
import u03.Optionals.Optional

class SequenceTest:

  val l: Sequence[Int] = Cons(10, Cons(20, Cons(30, Nil())))

  @Test def testSum(): Unit =
    assertEquals(0, sum(Nil()))
    assertEquals(60, sum(l))

  @Test def testMap(): Unit =
    assertEquals(Cons(11, Cons(21, Cons(31, Nil()))), Sequence.map(l)(_ + 1))
    assertEquals(Cons("10", Cons("20", Cons("30", Nil()))), Sequence.map(l)(_ + ""))

  @Test def testFilter(): Unit =
    assertEquals(Cons(20, Cons(30, Nil())), filter(l)(_ >= 20))
    assertEquals(Cons(10, Cons(30, Nil())), filter(l)(_ != 20))

  @Test def testTake(): Unit =
    assertEquals(Cons(10, Cons(20, Nil())), take(l)(2))
    assertEquals(Cons(10, Cons(20, Cons(30, Nil()))), take(l)(3))
    assertEquals(Nil(), take(l)(0))
    assertEquals(Nil(), take(Nil())(2))

  @Test def testZip(): Unit =
    val l2: Sequence[String] = Cons("10", Cons("20", Cons("30", Nil())))
    val l3: Sequence[String] = Cons("10", Cons("20", Nil()))
    assertEquals(Cons((10, "10"), Cons((20, "20"), Cons((30, "30"), Nil()))), zip(l, l2))
    assertEquals(Cons((10, "10"), Cons((20, "20"), Nil())), zip(l, l3))
    assertEquals(Nil(), zip(l, Nil()))
    assertEquals(Nil(), zip(Nil(), l2))
    assertEquals(Nil(), zip(Nil(), Nil()))

  @Test def testConcat(): Unit =
    val l2: Sequence[Int] = Cons(40, Cons(50, Nil()))
    assertEquals(Cons(10, Cons(20, Cons(30, Cons(40, Cons(50, Nil()))))), concat(l, l2))
    assertEquals(Cons(40, Cons(50, Nil())), concat(Nil(), l2))
    assertEquals(Cons(40, Cons(50, Nil())), concat(l2, Nil()))

  @Test def testFlatMap(): Unit =
    assertEquals(Cons(11, Cons(21, Cons(31, Nil()))), flatMap(l)(v => Cons(v + 1, Nil())))
    assertEquals(
      Cons(11, Cons(12, Cons(21, Cons(22, Cons(31, Cons(32, Nil())))))),
      flatMap(l)(v => Cons(v + 1, Cons(v + 2, Nil())))
    )
    assertEquals(Nil(), flatMap(Nil())(v => Cons(v, Nil())))

  // Task 2, svolto da solo
  @Test def testMin(): Unit =
    assertEquals(Optional.Just(10), min(l))
    assertEquals(Optional.Just(1), min(Cons(1, Nil())))
    assertEquals(Optional.Empty(), min(Nil()))

  // Task 3, svolto da solo
  @Test def testGetCourses(): Unit =
    val l1 = Cons(
      Person.Student("marco", 2020),
      Cons(
        Person.Teacher("luca", "pps"),
        Cons(
          Person.Student("alberto", 2020),
          Cons(
            Person.Teacher("andrea", "pcd"),
            Cons(Person.Teacher("giovanni", "asw"), Cons(Person.Teacher("giorgio", "pps"), Nil()))
          )
        )
      )
    )
    assertEquals(Cons("pps", Cons("pcd", Cons("asw", Cons("pps", Nil())))), getCourses(l1))

  // Task 4, svolto da solo
  @Test def testFoldLeft(): Unit =
    val lst = Cons(3, Cons(7, Cons(1, Cons(5, Nil()))))
    assertEquals(-16, foldLeft(lst)(0)(_ - _))

class StreamTest:

  @Test def testIterate(): Unit =
    val str1 = Stream.iterate(0)(_ + 1)
    assertEquals(Cons(0, Cons(1, Cons(2, Cons(3, Nil())))), Stream.toList(Stream.take(str1)(4)))

  @Test def testMap(): Unit =
    val str1 = Stream.iterate(0)(_ + 1)
    val str2 = Stream.map(str1)(_ + 1)
    assertEquals(Cons(1, Cons(2, Cons(3, Cons(4, Nil())))), Stream.toList(Stream.take(str2)(4)))

  @Test def testFilter(): Unit =
    val str1 = Stream.iterate(0)(_ + 1)
    val str2 = Stream.filter(str1)(x => x % 2 == 1)
    assertEquals(Cons(1, Cons(3, Cons(5, Cons(7, Nil())))), Stream.toList(Stream.take(str2)(4)))

  // Task 6, svolto da solo
  @Test def testTakeWhile(): Unit =
    val str1 = Stream.iterate(0)(_ + 1)
    val str2 = Stream.takeWhile(str1)(_ < 5)
    assertEquals(Cons(0, Cons(1, Cons(2, Cons(3, Cons(4, Nil()))))), Stream.toList(str2))

  // Task 7, svolto da solo
  @Test def testFill(): Unit =
    val str1 = Stream.fill(3)("a")
    val str2 = Stream.fill(-1)("a")
    assertEquals(Cons("a", Cons("a", Cons("a", Nil()))), Stream.toList(str1))
    assertEquals(Nil(), Stream.toList(str2))

  // Task 8, svolto da solo
  @Test def testPell(): Unit =
    val pell: Stream[Int] = Stream.pell
    val str1              = Stream.take(pell)(5)
    assertEquals(Cons(0, Cons(1, Cons(2, Cons(5, Cons(12, Nil()))))), Stream.toList(str1))
