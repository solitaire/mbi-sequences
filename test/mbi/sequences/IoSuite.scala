package mbi.sequences

import org.scalatest.FlatSpec
import org.scalatest.matchers.Matchers
import mbi.sequences.io.SimilarityMatrixReader
import mbi.sequences.structures.SimilarityMatrix
import mbi.sequences.structures.Alphabet._

/**
 * @author Anna Stępień
 * @since 1/3/14
 */
class IoSuite extends FlatSpec with Matchers {
  "SimilarityMatrixReader" should "read matrix values" in {
    val similarityMatrixStr = """10 -1 -3 -4 -5
                                |-1 7 -5 -3 -5
                                |-3 -5 9 0 -5
                                |-4 -3 0 8 -5
                                |-5 -5 -5 -5 0""".stripMargin

    val similarityMatrix = SimilarityMatrixReader.read(similarityMatrixStr.lines)

    assert(similarityMatrix.matrix === new SimilarityMatrix(Map(
      (A, Map((A, 10), (G, -1), (C, -3), (T, -4), (GAP, -5))),
      (G, Map((A, -1), (G,  7), (C, -5), (T, -3), (GAP, -5))),
      (C, Map((A, -3), (G, -5), (C,  9), (T,  0), (GAP, -5))),
      (T, Map((A, -4), (G, -3), (C,  0), (T,  8), (GAP, -5))),
      (GAP, Map((A, -5), (G, -5), (C, -5), (T, -5), (GAP, 0))))).matrix)
  }
}
