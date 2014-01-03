package mbi.sequences

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

  import sequences._

  def marginalCosts(i: Int, j: Int, k: Int, sm: SimilarityMatrix) = {
    if (i == 0 && j == 0 && k == 0) (0, Nil)
    else if (i == 0 && j == 0) (k * sm.gapCost, (noMove, noMove, doMove) :: Nil)
    else if (i == 0 && k == 0) (j * sm.gapCost, (noMove, doMove, noMove) :: Nil)
    else if (j == 0 && k == 0) (i * sm.gapCost, (doMove, noMove, noMove) :: Nil)
    else if (i == 0) ((j + k) * sm.gapCost, (noMove, doMove, doMove) :: Nil)
    else if (j == 0) ((i + k) * sm.gapCost, (doMove, noMove, doMove) :: Nil)
    else if (k == 0) ((i + j) * sm.gapCost, (doMove, doMove, noMove) :: Nil)
    else throw new Error("unexpected")
  }

  def recursiveNeedlemanWunsch(s: DNASeq, t: DNASeq, u: DNASeq, sm: SimilarityMatrix): (DNASeq, DNASeq, DNASeq, Int) = {
    implicit val smm = sm
    var alignments: mutable.Map[(Int, Int, Int), (Int, Moves)] = mutable.Map()

    def getOrPut(i: Int, j: Int, k: Int, f: => () => (Int, Moves)): (Int, Moves) = {

      def getFromMatrix(i: Int, j: Int, k: Int): Option[(Int, Moves)] = alignments.get((i, j, k))

      def putToMatrix(i: Int, j: Int, k: Int, data: (Int, Moves)) = alignments += (((i, j, k), data))
      if (i == 0 || j == 0 || k == 0) {
        marginalCosts(i, j, k, sm)
      }
      else getFromMatrix(i, j, k) match {
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

      if (i == 0 && j == 0 && k == 0) (0, acc)
      else {
        val f: (Int, Moves) = getOrPut(i - 1, j - 1, k - 1, () => F(i - 1, j - 1, k - 1, acc))
        val f1: (Int, Moves) = getOrPut(i - 1, j - 1, k, () => F(i - 1, j - 1, k, acc))
        val f2: (Int, Moves) = getOrPut(i - 1, j, k - 1, () => F(i - 1, j, k - 1, acc))
        val f3: (Int, Moves) = getOrPut(i, j - 1, k - 1, () => F(i, j - 1, k - 1, acc))
        val f4: (Int, Moves) = getOrPut(i - 1, j, k, () => F(i - 1, j, k, acc))
        val f5: (Int, Moves) = getOrPut(i, j - 1, k, () => F(i, j - 1, k, acc))
        val f6: (Int, Moves) = getOrPut(i, j, k - 1, () => F(i, j, k - 1, acc))

        val maxAndMove = ((f, f._1 + e(Some(s(i)), Some(t(j)), Some(u(k))), (doMove, doMove, doMove)) ::
          (f1, f1._1 + e(Some(s(i)), Some(t(j)), None), (doMove, doMove, noMove)) ::
          (f2, f2._1 + e(Some(s(i)), None, Some(u(k))), (doMove, noMove, doMove)) ::
          (f3, f3._1 + e(None, Some(t(j)), Some(u(k))), (noMove, doMove, doMove)) ::
          (f4, f4._1 + e(Some(s(i)), None, None), (doMove, noMove, noMove)) ::
          (f5, f5._1 + e(None, Some(t(j)), None), (noMove, doMove, noMove)) ::
          (f6, f6._1 + e(None, None, Some(u(k))), (noMove, noMove, doMove)) :: Nil).reduce((t1, t2) => if (t1._2 > t2._2) t1 else t2)

        (maxAndMove._2, maxAndMove._3 :: maxAndMove._1._2)
      }
    }



    val f: (Int, Moves) = F(s.length - 1, t.length - 1, u.length - 1, List())
    val formatted: (DNASeq, DNASeq, DNASeq) = formatSequences(s, t, u, f._2)
    (formatted._1, formatted._2, formatted._3, f._1)
  }

  def e(s: Option[Alphabet.Value], t: Option[Alphabet.Value], u: Option[Alphabet.Value])(implicit sm: SimilarityMatrix): Int = sm.get((s.getOrElse(Alphabet.GAP), t.getOrElse(Alphabet.GAP), u.getOrElse(Alphabet.GAP)))

  def iterativeNeedlemanWunsch(s: DNASeq, t: DNASeq, u: DNASeq, sm: SimilarityMatrix): (DNASeq, DNASeq, DNASeq, Int) = {
    var alignments: mutable.Map[(Int, Int, Int), Int] = mutable.Map()
    implicit val smm = sm

    def get(i: Int, j: Int, k: Int) = {

      if (i == 0 || j == 0 || k == 0) {
        marginalCosts(i, j, k, sm)._1
      }
      else if(!alignments.contains(i,j,k)) {
        println(s"does not constain ($i, $j, $k)")
        0
      }
      else alignments(i, j, k)
    }

    def F(i: Int, j: Int, k: Int) = {

      if (i == 0 || j == 0 || k == 0) {
        marginalCosts(i, j, k, sm)._1
      }
      else {
        val costs = (get(i - 1, j - 1, k - 1) + e(Some(s(i)), Some(t(j)), Some(u(k)))) ::
          (get(i - 1, j - 1, k) + e(Some(s(i)), Some(t(j)), None)) ::
          (get(i - 1, j, k - 1) + e(Some(s(i)), None, Some(u(k)))) ::
          (get(i, j - 1, k - 1) + e(None, Some(t(j)), Some(u(k)))) ::
          (get(i - 1, j, k) + e(Some(s(i)), None, None)) ::
          (get(i, j - 1, k) + e(None, Some(t(j)), None)) ::
          (get(i, j, k - 1) + e(None, None, Some(u(k)))) :: Nil
        costs.reduce(_ max _)
      }
    }

    var moves: Moves = List()
    for {
      i <- 0 until s.length
      j <- 0 until t.length
      k <- 0 until u.length
    } {
      val f = F(i + 1, j + 1, k + 1)
      alignments += (((i + 1, j + 1, k + 1), f))
      val f1 = F(i + 1, j + 1, k)
      alignments += (((i + 1, j + 1, k), f1))
      val f2 = F(i + 1, j, k + 1)
      alignments += (((i + 1, j, k + 1), f2))
      val f3 = F(i, j + 1, k + 1)
      alignments += (((i, j + 1, k + 1), f3))
      val f4 = F(i + 1, j, k)
      alignments += (((i + 1, j, k), f4))
      val f5 = F(i, j + 1, k)
      alignments += (((i, j + 1, k), f5))
      val f6 = F(i, j, k + 1)
      alignments += (((i, j, k + 1), f6))

      val costsWithMoves = (f, (doMove, doMove, doMove)) ::(f1, (doMove, doMove, noMove)) ::(f2, (doMove, noMove, doMove)) ::
        (f3, (noMove, doMove, doMove)) ::(f4, (doMove, noMove, noMove)) ::(f5, (noMove, doMove, noMove)) ::(f6, (noMove, noMove, doMove)) :: Nil

      val bestCostWithMove: (Int, (sequences.Move, sequences.Move, sequences.Move)) = costsWithMoves.reduce((fm1, fm2) => if (fm1._1 > fm2._1) fm1 else fm2)

      moves = bestCostWithMove._2 :: moves
    }
    val formatted: (DNASeq, DNASeq, DNASeq) = formatSequences(s, t, u, moves)
    (formatted._1, formatted._2, formatted._3, alignments((s.length, t.length, u.length)))

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
