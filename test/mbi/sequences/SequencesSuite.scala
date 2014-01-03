package mbi.sequences

import org.scalatest.FlatSpec
import org.scalatest.matchers.Matchers
import scala.util.Random
import nw.io.SimilarityMatrixReader
import nw.structures.Alphabet

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

//  "Formatting sequences" should "work" in {
//    import sequences._
//    val s1: DNASeq = Alphabet("AG")
//    val s2: DNASeq = Alphabet("CTAAG")
//    val s3: DNASeq = Alphabet("AGTT")
//    val expected = ("---AG".toList, "CTAAG".toList, "AG-TT".toList)
//
//    val moves = (doMove, doMove, doMove) ::
//      (doMove, doMove, doMove) ::
//      (noMove, doMove, noMove) ::
//      (noMove, doMove, doMove) ::
//      (noMove, doMove, doMove) ::
//      Nil
//
//    val formatted = Sequences.formatSequences(s1, s2, s3, moves)
//    assert(formatted === expected)
//  }

  "Iterative NeedlemanWunsch" should "give same results as recursive NeedlemanWunsch" in {
    println("RUNNING NOW Iterative NeedlemanWunsch")
    val similarityMatrixStr = """10 -1 -3 -4 -5
                                |-1 7 -5 -3 -5
                                |-3 -5 9 0 -5
                                |-4 -3 0 8 -5
                                |-5 -5 -5 -5 0""".stripMargin

    val s1f = """>ENA|X74510|X74510.1 M.musculus ANC1 mRNA for adenine nucleotide carrier
                |GCTGTCGACGGATTCGGGGTGGCGGTGC""".stripMargin

    val s2f = """>ENA|X99953|X99953.1 X.laevis mRNA for P2Y8 nucleotide receptor
                |CGAAAA""".stripMargin

    val s3f = """>ENA|Z11978|Z11978.1 L.tarentolae gene for pyridine nucleotide linked dehydrogenase.
                |CTCGAGGGCGGCGGCGGTGG""".stripMargin

    val s1 = App.createSequenceFromLines(s1f.lines)
    val s2 = App.createSequenceFromLines(s2f.lines)
    val s3 = App.createSequenceFromLines(s3f.lines)
    val similarityMatrix = SimilarityMatrixReader.read(similarityMatrixStr.lines)
    assert(Sequences.iterativeNeedlemanWunsch(s1, s2, s3, similarityMatrix) == Sequences.recursiveNeedlemanWunsch(s1, s2, s3, similarityMatrix))
  }
}
