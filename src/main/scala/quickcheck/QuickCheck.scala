package quickcheck

import common._

import org.scalacheck._
import Arbitrary._
import Gen._
import Prop._

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {

  property("min1") = forAll { a: Int =>
    val h = insert(a, empty)
    findMin(h) == a
  }

  /**
   * If you insert any two elements into an empty heap, find the
   * minimum of the resulting heap should get the smallest of the
   * the two elements back.
   */
  property("insertTwo") = forAll { (n: Int, m: Int) =>
    val h1 = insert(n, empty)
    val h2 = insert(m, h1)
    if (n <= m) 
      findMin(h2) == n
    else
      findMin(h2) == m
  }

  /**
   * If you insert an element into an empty heap, then delete the
   * minimum, the resulting heap should be empty.
   */
  property("insertAndDelete") = forAll { n: Int =>
    val h = insert(n, empty)
    deleteMin(h) == empty
  }

  lazy val genHeap: Gen[H] = ???

  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

}
