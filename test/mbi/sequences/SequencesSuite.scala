package mbi.sequences

import org.scalatest.FlatSpec
import org.scalatest.matchers.Matchers
import scala.util.Random

/**
 * @author Marek Lewandowski <marek.m.lewandowski@gmail.com>
 * @since 1/3/14
 */
class SequencesSuite extends FlatSpec with Matchers {
  "A 1+1" should "be equal to 2" in {
    assert(1 + 1 === 2)
  }

  "Recursive function like F" should "does not have memoization" in {

    var evaluationCounter = 0

    def F(i: Int, j: Int): Int = {
      if (i == 0 || j == 0) return 0
      val f1 = F(i - 1, j - 1) + {
        evaluationCounter = evaluationCounter + 1
        Random.nextInt(5)
      }
      val f2 = F(i - 1, j) + {
        evaluationCounter = evaluationCounter + 1
        Random.nextInt(5)
      }
      val f3 = F(i, j - 1) + {
        evaluationCounter = evaluationCounter + 1
        Random.nextInt(5)
      }

      (f1 :: f2 :: f3 :: Nil).reduce(_ max _)
    }

    F(3, 2)
    assert(evaluationCounter == 3 + 3 + 3 + 3 + 3 + 3 + 3 + 3 + 3 + 3 + 3 + 3)
    println(s"evaluationCounter $evaluationCounter")

  }

  // TODO this test fails and it looks like Stream cannot be used in this way, maybe there is some clever solution but I don't know how to do it with Stream
  // Stream would give memoization for free.
  "Recursive function like F with stream could provide memoization" should "but it does not" in {
    var evaluationCounter = 0

    def F(i: Int, j: Int): Stream[Int] = {
      if (i == 0 || j == 0) return Stream(0)
      val f1 = F(i - 1, j - 1).head + {
        evaluationCounter = evaluationCounter + 1
        Random.nextInt(5)
      }
      val f2 = F(i - 1, j).head + {
        evaluationCounter = evaluationCounter + 1
        Random.nextInt(5)
      }
      val f3 = F(i, j - 1).head + {
        evaluationCounter = evaluationCounter + 1
        Random.nextInt(5)
      }

      Stream((f1 :: f2 :: f3 :: Nil).reduce(_ max _))
    }

    F(3, 2)
    println(s"evaluationCounter $evaluationCounter")
    assert(evaluationCounter != 3 + 3 + 3 + 3 + 3 + 3)
  }

  "Formatting sequences" should "work" in {
    import sequences._
    val s1: DNASeq = "AG"
    val s2: DNASeq = "CTAAG"
    val s3: DNASeq = "AGTT"
    val expected = ("---AG".toList, "CTAAG".toList, "AG-TT".toList)

    val moves = (doMove, doMove, doMove) ::
      (doMove, doMove, doMove) ::
      (noMove, doMove, noMove) ::
      (noMove, doMove, doMove) ::
      (noMove, doMove, doMove) ::
      Nil

    val formatted = Sequences.formatSequences(s1, s2, s3, moves)
    assert(formatted === expected)
  }
}
