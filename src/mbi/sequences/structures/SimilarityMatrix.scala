package mbi.sequences.structures

/**
 * @author Anna Stępień
 * @since 1/3/14
 */
class SimilarityMatrix(map: Map[Alphabet.Value, Map[Alphabet.Value, Int]]) {
  def matrix = map
  def get(item: (Alphabet.Value, Alphabet.Value, Alphabet.Value)) =
    matrix.get(item._1).get(item._2) + matrix.get(item._1).get(item._3) + matrix.get(item._2).get(item._3)

  def gapCost = matrix.get(Alphabet.A).get(Alphabet.GAP)
}