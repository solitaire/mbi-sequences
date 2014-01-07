package mbi.sequences.structures

/**
 * @author Anna Stępień
 * @author Marek Lewandowski <marek.m.lewandowski@gmail.com>
 * @since 1/3/14
 */
object Alphabet extends Enumeration {
  val A, G, C, T, GAP = Value

  val mapping = Map('A' -> A, 'G' -> G, 'C' -> C, 'T' -> T, '-' -> GAP)
  val reverseMapping = Map(A -> 'A', G -> 'G', C -> 'C', T -> 'T', GAP -> '-')

  def apply(opt: Option[Char]): Value = opt.map(c => mapping(c)).getOrElse(GAP)

  def apply(s: String): List[Value] = s.toList.map(c => apply(Some(c)))

  def print(l : Alphabet.Value): Char = reverseMapping(l)
}