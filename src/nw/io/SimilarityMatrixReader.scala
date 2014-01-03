package nw.io

import scala.collection.mutable.MutableList
import nw.structures.SimilarityMatrix
import nw.structures.Alphabet

object SimilarityMatrixReader {

  /**
   * Expects format
   *
   * x x x x x
   * x x x x x
   * x x x x x
   * x x x x x
   * x x x x x
   *
   * which is interpreted as matrix
   *
   * A G C T GAP
   * A x x x x x
   * G x x x x x
   * C x x x x x
   * T x x x x x
   * GAP x x x x x
   * @param lines
   * @return
   */

  def read(lines: Iterator[String]) = {
    val values = MutableList[Map[Alphabet.Value, Int]]()
    lines.foreach(line => {
      line.split(" ") match {
        case Array(a, g, c, t, gap) => {
          values += Map(
            (Alphabet.A, a.toInt),
            (Alphabet.G, g.toInt),
            (Alphabet.C, c.toInt),
            (Alphabet.T, t.toInt),
            (Alphabet.GAP, gap.toInt))
        }
        case _ => //TODO: handle error
      }
    })
    new SimilarityMatrix(Alphabet.values.toList zip values toMap)
  }
}