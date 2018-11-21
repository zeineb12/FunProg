package quickcheck

import common._

import org.scalacheck._
import Arbitrary._
import Gen._
import Prop._

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {

  lazy val genHeap: Gen[H] = oneOf(
  const(empty),
  for {
    k <- arbitrary[Int]
    m <- oneOf(const(empty), genHeap)
  } yield insert(k, m)
)

  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

  property("gen1") = forAll { h: H =>
    val m = if (isEmpty(h)) 0 else findMin(h)
    findMin(insert(m, h)) == m
  }
  /*Check that after adding a single element to an 
  empty heap, findMin would return the added element*/
  property("min1") = forAll { a: Int =>
    val h = insert(a, empty)
    findMin(h) == a
  }

  /*Check that after adding two element to an 
  empty heap, findMin would return the smaller one*/
  property("min2") = forAll { (a: Int, b: Int) =>
      val h = insert(b,insert(a, empty))
      if(a<b) findMin(h) == a
      else findMin(h) == b
  }

  /*Check that adding a single element to an empty heap,
  then deleting it would give an empty heap again*/
  property("emptyHeap") = forAll { a: Int =>
    val h = insert(a, empty)
    isEmpty(deleteMin(h))
  }

  /*Given any heap, get a sorted sequence of elements when continually finding 
  and deleting minima*/
  property("sortedSeq") = forAll { h: H =>
    def isSorted(h: H): Boolean =
      if (isEmpty(h)) true
      else {
        val m = findMin(h)
        val h2 = deleteMin(h)
        isEmpty(h2) || (m <= findMin(h2) && isSorted(h2))
      }
    isSorted(h)
  }

  /*Finding a minimum of the melding of any two heaps should 
  return a minimum of one or the other.*/
  property("minMergeHeap") = forAll { (h1: H, h2: H) =>
    findMin(meld(h1,h2))==findMin(h1) || findMin(meld(h1,h2))==findMin(h2)
  }

}
