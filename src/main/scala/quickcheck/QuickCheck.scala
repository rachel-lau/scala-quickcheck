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
    val min = findMin(h2)
    if (n <= m) 
      min == n
    else
      min == m
  }

  /**
   * n=2147483647 m=1118895033 k=1
   * h1=List(Node(2147483647,0,List()))
   * h2=List(Node(1118895033,1,List(Node(2147483647,0,List()))))
   * h3=List(Node(1,0,List()), Node(1118895033,1,List(Node(2147483647,0,List()))))
   */
  property("insertThree") = forAll { (n: Int, m: Int, k: Int) =>
    val h1 = insert(n, empty)
    val h2 = insert(m, h1)
    val h3 = insert(k, h2)
    findMin(h3)== List(n, m, k).min
  }

  property("insertThreeAndDelete") = forAll { (n: Int, m: Int, k: Int) =>
    val h1 = insert(n, empty)
    val h2 = insert(m, h1)
    val h3 = insert(k, h2)
    val max = List(n, m, k).max
    deleteMin(deleteMin(h3)) == insert(max, empty)
  }

  /**
   * If you insert an element into an empty heap, then delete the
   * minimum, the resulting heap should be empty.
   */
  property("insertAndDelete") = forAll { n: Int =>
    val h = insert(n, empty)
    deleteMin(h) == empty
  }

  /**
   * Find a minimum of the melding of any two heaps should return a
   * minimum of one or the other.
   */
  property("findMinMeld") = forAll { (n: Int, m: Int) =>
    val h1 = insert(n, empty)
    val h2 = insert(m, empty)
    val h3 = meld(h1, h2)
    val min = findMin(h3)
    if (n <= m)
      min == n
    else
      min == m
  }

  lazy val genHeap: Gen[H] = ???

  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

}
