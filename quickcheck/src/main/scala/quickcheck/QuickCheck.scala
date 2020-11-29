package quickcheck

import org.scalacheck._
import Arbitrary._
import Gen._
import Prop._

import scala.annotation.tailrec

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {

  lazy val genHeap: Gen[H] = oneOf(
    const(empty),
    for {
      v <- arbitrary[Int]
      h <- genHeap
    } yield insert(v, h)

  )
  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

  property("min1") = forAll { a: Int =>
    val h = insert(a, empty)
    findMin(h) == a
  }

  property("gen1") = forAll { (h: H) =>
    val m = if (isEmpty(h)) 0 else findMin(h)
    findMin(insert(m, h)) == m
  }

  property("gen2") = forAll { (h: H) =>
    findMin(insert(0, insert(1, empty))) == 0
  }

  property("gen3") = forAll { (h: H) =>
    isEmpty(deleteMin(insert(5, empty)))
  }

  property("gen4") = forAll { (h1: H, h2: H) =>
    val m1 = if (isEmpty(h1)) 0 else findMin(h1)
    val m2 = if (isEmpty(h2)) 0 else findMin(h2)
    val melded = meld(h1, h2)

    val m = if (isEmpty(melded)) 0 else findMin(melded)
    if (isEmpty(h1)) m == m2 else if (isEmpty(h2)) m == m1 else m == math.min(m1, m2)
  }

  property("gen5") = forAll { (h: H) =>
    @tailrec
    def checkOrder(h: H, minSoFar: A): Boolean = {
      if (isEmpty(h)) {
        true
      } else {
        val min = findMin(h)
        if (ord.lt(min, minSoFar)) {
          false
        } else {
          checkOrder(deleteMin(h), min)
        }
      }
    }
    if (!isEmpty(h)) {
      checkOrder(h, findMin(h))
    } else {
      true
    }
  }

  property("case1") = forAll { (h: H) =>
    val res = List(-1, 2, 4, 8, 12)
    val he = insert(12, insert(4, insert(8, insert(2, insert(-1, empty)))))
    def toList(h: H): List[A] = {
      if (isEmpty(h)) {
        List()
      } else {
        findMin(h) :: toList(deleteMin(h))
      }
    }
    toList(he) == res
  }
}
