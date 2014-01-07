package mbi.sequences

import org.scalatest.FlatSpec
import org.scalatest.matchers.Matchers
import mbi.sequences.structures.SimilarityMatrix
import mbi.sequences.structures.Alphabet._

/**
 * @author Anna Stępień
 * @since 1/3/14
 */
class StructuresSuite extends FlatSpec with Matchers {
  private val defaultSimilarityMatrix = new SimilarityMatrix(Map(
    (A, Map((A, 10), (G, -1), (C, -3), (T, -4), (GAP, -5))),
    (G, Map((A, -1), (G,  7), (C, -5), (T, -3), (GAP, -5))),
    (C, Map((A, -3), (G, -5), (C,  9), (T,  0), (GAP, -5))),
    (T, Map((A, -4), (G, -3), (C,  0), (T,  8), (GAP, -5))),
    (GAP, Map((A, -5), (G, -5), (C, -5), (T, -5), (GAP, 0)))))

  "SimilarityMatrix" should "return correct values" in {
    assert(defaultSimilarityMatrix.get(A, A, A) == 30)
    assert(defaultSimilarityMatrix.get(G, G, G) == 21)
    assert(defaultSimilarityMatrix.get(C, C, C) == 27)
    assert(defaultSimilarityMatrix.get(T, T, T) == 24)
    assert(defaultSimilarityMatrix.get(GAP, GAP, GAP) == 0)

    assert(defaultSimilarityMatrix.get(A, C, GAP) == -13)
    assert(defaultSimilarityMatrix.get(A, GAP, GAP) == -10)
    assert(defaultSimilarityMatrix.get(C, G, T) == -8)
    assert(defaultSimilarityMatrix.get(G, T, T) == 2)
  }
}
