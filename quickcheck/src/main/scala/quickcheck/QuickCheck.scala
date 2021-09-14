package quickcheck

import org.scalacheck.*
import Arbitrary.*
import Gen.*
import Prop.forAll

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap:

  lazy val genHeap: Gen[H] = oneOf(
      const(this.empty),
      for {
        x <- arbitrary[Int]
        g <- genHeap
      } yield this.insert(x, g)
    )

  given Arbitrary[H] = Arbitrary(genHeap)

  property("gen1") = forAll { (h: H) =>
    val m = if isEmpty(h) then 0 else findMin(h)
    findMin(insert(m, h)) == m
  }

  property("minOfTwo") = forAll { (a: Int, b: Int) =>
    val h = insert(b, insert(a, empty))
    if (a<b) findMin(h) == a else findMin(h) == b
  }

  property("emptyAfterDeleteOnlyElement") = forAll { (a: Int) =>
    val h = insert(a, empty)
    isEmpty(deleteMin(h))
  }

  property("stillSortedAfterRemove") = forAll { (h: H) =>

    def checkRemoveMin(heap: H): Boolean = {
      if (isEmpty(heap))
        true
      else {
        val nextHeap = deleteMin(heap)
        if (isEmpty(nextHeap)) {
          true
        } else {
          val currentMin = findMin(heap)
          (currentMin <= findMin(nextHeap)) && checkRemoveMin(nextHeap)
        }
      }
    }
    checkRemoveMin(h)
  }

  property("minimumOfMelted") = forAll { (h1: H, h2: H) =>

    if (isEmpty(h1) && isEmpty(h2)) true
    else {
      val expectedMin = (h1, h2) match {
        case (x, Nil) => findMin(x)
        case (Nil, y) => findMin(y)
        case (x, y) => if (findMin(x) < findMin(y)) findMin(x) else findMin(y)
      }
      findMin(meld(h1, h2)) == expectedMin
    }
  }

  property("valueCanBeFound") = forAll { (h1: H, a: Int) =>

    val h2 = insert(a, h1)

    def findA(h: H): Boolean =
      if(isEmpty(h)) false
      else findMin(h) == a || findA(deleteMin(h))

    findA(h2)
  }




