package nw.structures

class SimilarityMatrix(map: Map[Alphabet.Value, Map[Alphabet.Value, Int]]) {
  def matrix = map
  def get(item: (Alphabet.Value, Alphabet.Value, Alphabet.Value)) =
    matrix.get(item._1).get(item._2) + matrix.get(item._1).get(item._3) + matrix.get(item._2).get(item._3)
}