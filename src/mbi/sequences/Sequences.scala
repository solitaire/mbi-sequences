package mbi.sequences

import mbi.sequences.sequences.{MoveType, Moves, DNASeq}
import nw.structures.{Alphabet, SimilarityMatrix}
import scala.collection.mutable

/**
 * @author Marek Lewandowski <marek.m.lewandowski@gmail.com>
 * @since 1/3/14
 */

package object sequences {
  type DNASeq = Seq[Alphabet.Value]

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

  def NeedlemanWunsch(s: DNASeq, t: DNASeq, u: DNASeq, sm: SimilarityMatrix): (DNASeq, DNASeq, DNASeq, Int) = {

    var alignments: mutable.Map[(Int, Int, Int), (Int, Moves)] = mutable.Map()

    def getOrPut(i: Int, j: Int, k: Int, f: => () => (Int, Moves)): (Int, Moves) = {

      def getFromMatrix(i: Int, j: Int, k: Int): Option[(Int, Moves)] = alignments.get((i, j, k))

      def putToMatrix(i: Int, j: Int, k: Int, data: (Int, Moves)) = alignments += (((i, j, k), data))

      getFromMatrix(i, j, k) match {
        case Some(data) => data
        case None => {
          val f1: (Int, Moves) = f()
          putToMatrix(i, j, k, f1)
          f1
        }
      }
    }



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
      val f: (Int, Moves) = getOrPut(i - 1, j - 1, k - 1, () => F(i - 1, j - 1, k - 1, acc))
      val f1: (Int, Moves) = getOrPut(i - 1, j - 1, k, () => F(i - 1, j - 1, k, acc))
      val f2: (Int, Moves) = getOrPut(i - 1, j, k - 1, () => F(i - 1, j, k - 1, acc))
      val f3: (Int, Moves) = getOrPut(i, j - 1, k - 1, () => F(i, j - 1, k - 1, acc))
      val f4: (Int, Moves) = getOrPut(i - 1, j, k, () => F(i - 1, j, k, acc))
      val f5: (Int, Moves) = getOrPut(i, j - 1, k, () => F(i, j - 1, k, acc))
      val f6: (Int, Moves) = getOrPut(i, j, k - 1, () => F(i, j, k - 1, acc))

      val maxAndMove = ((f._1 + e(Some(s(i)), Some(t(j)), Some(u(k))), (doMove, doMove, doMove)) ::
        (f1._1 + e(Some(s(i)), Some(t(j)), None), (doMove, doMove, noMove)) ::
        (f2._1 + e(Some(s(i)), None, Some(u(k))), (doMove, noMove, doMove)) ::
        (f3._1 + e(None, Some(t(j)), Some(u(k))), (noMove, doMove, doMove)) ::
        (f4._1 + e(Some(s(i)), None, None), (doMove, noMove, noMove)) ::
        (f5._1 + e(None, Some(t(j)), None), (noMove, doMove, noMove)) ::
        (f6._1 + e(None, None, Some(u(k))), (noMove, noMove, doMove)) :: Nil).reduce((t1, t2) => if (t1._1 > t2._1) t1 else t2)

      (maxAndMove._1, maxAndMove._2 :: acc)
    }

    def e(s: Option[Alphabet.Value], t: Option[Alphabet.Value], u: Option[Alphabet.Value]): Int = sm.get((s.getOrElse(Alphabet.GAP), t.getOrElse(Alphabet.GAP), u.getOrElse(Alphabet.GAP)))

    val f: (Int, Moves) = F(s.length - 1, t.length - 1, u.length - 1, List())
    val formatted: (DNASeq, DNASeq, DNASeq) = formatSequences(s, t, u, f._2)
    (formatted._1, formatted._2, formatted._3, f._1)
  }

  def formatSequences(s: DNASeq, t: DNASeq, u: DNASeq, m: Moves): (DNASeq, DNASeq, DNASeq) = {
    def formatSeq(seq: DNASeq, f: MoveType => Boolean): DNASeq = {
      m.map(f).map(v => if (v) None else Some(Alphabet.GAP)).foldRight((List[Alphabet.Value](), seq))((charOpt, listWithSeq) =>
        if (charOpt.isDefined) (listWithSeq._1.:+(charOpt.get), listWithSeq._2)
        else (listWithSeq._1.:+(listWithSeq._2.head), listWithSeq._2.tail))._1
    }
    (formatSeq(s, _._1), formatSeq(t, _._2), formatSeq(u, _._3))
  }
}
