package mbi.sequences

import mbi.sequences.sequences.{Moves, SimilarityMatrix, DNASeq}

/**
 * @author Marek Lewandowski <marek.lewandowski@icompass.pl>
 * @since 1/3/14
 */

package object sequences {
  type DNASeq = Seq[Char]
  // TODO
  type SimilarityMatrix = AnyRef

  sealed trait MoveType
  case object Horizonal extends MoveType
  case object Vertical extends MoveType
  case object Diagonal extends MoveType

  type Moves = List[MoveType]

}

class Sequences {


}

object Sequences {

  def NeedlemanWunsch(s1: DNASeq, s2: DNASeq, s3: DNASeq, sm: SimilarityMatrix): Moves = ???
}
