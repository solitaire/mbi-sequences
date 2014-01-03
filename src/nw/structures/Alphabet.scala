package nw.structures

object Alphabet extends Enumeration {
	val A, G, C, T, GAP = Value

  val mapping = Map('A' -> A, 'G' -> G, 'C' -> C, 'T' -> T, '-' -> GAP)

  def apply(opt: Option[Char]): Value = opt.map(c => mapping(c)).getOrElse(GAP)
}