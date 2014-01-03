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

  /**
   * (doMove, doMove, doMove) means F(i-1, j-1, k-1) which means sequences are similar
   *
   * There is one illegal move (noMove, noMove, noMove)
   */
  type Move = Boolean
  val doMove: Move = true
  val noMove: Move = false
  type MoveType = (Boolean, Boolean, Boolean)

  type Moves = List[MoveType]

}

class Sequences {


}

object Sequences {

  def NeedlemanWunsch(s1: DNASeq, s2: DNASeq, s3: DNASeq, sm: SimilarityMatrix): Moves = {

    /**
     * Head of the Moves is the move corresponding to F(I, J, K) - sequences length. Last element in moves is for F(0,0,0)
     * @param i
     * @param j
     * @param k
     * @param acc
     * @return
     */
    def F(i: Int, j: Int, k: Int, acc: Moves): (Int, Moves) = {
      import sequences._
      val f: (Int, Moves) = F(i - 1, j - 1, k - 1, acc)
      val f1: (Int, Moves) = F(i - 1, j - 1, k, acc)
      val f2: (Int, Moves) = F(i - 1, j, k - 1, acc)
      val f3: (Int, Moves) = F(i, j - 1, k - 1, acc)
      val f4: (Int, Moves) = F(i - 1, j, k, acc)
      val f5: (Int, Moves) = F(i, j - 1, k, acc)
      val f6: (Int, Moves) = F(i, j, k - 1, acc)

      val maxAndMove = ((f._1 + e(s = true, t = true, u = true), (doMove, doMove, doMove)) ::
        (f1._1 + e(s = true, t = true, u = false), (doMove, doMove, noMove)) ::
        (f2._1 + e(s = true, t = false, u = true), (doMove, noMove, doMove)) ::
        (f3._1 + e(s = false, t = true, u = true), (noMove, doMove, doMove)) ::
        (f4._1 + e(s = true, t = false, u = false), (doMove, noMove, noMove)) ::
        (f5._1 + e(s = false, t = true, u = false), (noMove, doMove, noMove)) ::
        (f6._1 + e(s = false, t = false, u = true), (noMove, noMove, doMove)) :: Nil).reduce((t1, t2) => if (t1._1 > t2._1) t1 else t2)

      (maxAndMove._1, maxAndMove._2 :: acc)
    }

    def e(s: Boolean, t: Boolean, u: Boolean): Int = ???


  }
}
