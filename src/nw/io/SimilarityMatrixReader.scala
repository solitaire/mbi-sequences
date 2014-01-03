package nw.io

import scala.collection.mutable.MutableList
import nw.structures.SimilarityMatrix
import nw.structures.Alphabet

object SimilarityMatrixReader {
  
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