package mbi.sequences

import scala.collection.mutable
import mbi.sequences.structures.{SimilarityMatrix, Alphabet}

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

object Sequences {

  import sequences._

  def NeedlemanWunsch(s: DNASeq, t: DNASeq, u: DNASeq, sm: SimilarityMatrix, recursive: Boolean): (DNASeq, DNASeq, DNASeq, Int) = {

    if (recursive) {
      val results: (Int, sequences.Moves, mutable.Map[(Int, Int, Int), (Int, sequences.Moves)]) = recursiveNeedlemanWunsch(s, t, u, sm)
      val formatted: (DNASeq, DNASeq, DNASeq) = formatSequences(s, t, u, results._2)
      (formatted._1, formatted._2, formatted._3, results._1)
    } else {
      val results: (Int, List[sequences.MoveType], mutable.Map[Position, BestAlignmentWithMoveType]) = iterativeNeedlemanWunsch(s, t, u, sm)
      val formatted: (DNASeq, DNASeq, DNASeq) = formatSequences(s, t, u, results._2)
      (formatted._1, formatted._2, formatted._3, results._1)
    }
  }

  private def marginalCosts(i: Int, j: Int, k: Int, sm: SimilarityMatrix) = {
    def repeatMove(times : Int, move: MoveType) = { for(i <- 0 until times) yield move }.toList
    if (i == 0 && j == 0 && k == 0) (0, Nil)
    else if (i == 0 && j == 0) (2 * k * sm.gapCost, repeatMove(k, (noMove, noMove, doMove)))
    else if (i == 0 && k == 0) (2 * j * sm.gapCost, repeatMove(j, (noMove, doMove, noMove)))
    else if (j == 0 && k == 0) (2 * i * sm.gapCost, repeatMove(i, (doMove, noMove, noMove)))
    else if (i == 0) ((k + j) * sm.gapCost, repeatMove(j + k, (noMove, doMove, doMove)))
    else if (j == 0) ((i + k) * sm.gapCost, repeatMove(i + k, (doMove, noMove, doMove)))
    else if (k == 0) ((i + j) * sm.gapCost, repeatMove(i+j, (doMove, doMove, noMove)))
    else throw new Error("unexpected")
  }

  private def e(s: Option[Alphabet.Value], t: Option[Alphabet.Value], u: Option[Alphabet.Value])(implicit sm: SimilarityMatrix): Int = sm.get((s.getOrElse(Alphabet.GAP), t.getOrElse(Alphabet.GAP), u.getOrElse(Alphabet.GAP)))

  protected[sequences] def recursiveNeedlemanWunsch(s: DNASeq, t: DNASeq, u: DNASeq, sm: SimilarityMatrix) = {
    implicit val smm = sm
    var alignments: mutable.Map[(Int, Int, Int), (Int, Moves)] = mutable.Map()
    def getOrPut(i: Int, j: Int, k: Int, f: => () => (Int, Moves)): (Int, Moves) = {

      def getFromMatrix(i: Int, j: Int, k: Int): Option[(Int, Moves)] = alignments.get((i, j, k))

      def putToMatrix(i: Int, j: Int, k: Int, data: (Int, Moves)) = alignments += (((i, j, k), data))

      if (i == 0 || j == 0 || k == 0) {
        val costs: (Int, List[(sequences.Move, sequences.Move, sequences.Move)]) = marginalCosts(i, j, k, sm)
        alignments += (((i, j, k), costs))
        costs
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
     *
     **/
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

        val maxAndMove = ((f, f._1 + e(Some(s(i - 1)), Some(t(j - 1)), Some(u(k - 1))), (doMove, doMove, doMove)) ::
          (f1, f1._1 + e(Some(s(i - 1)), Some(t(j - 1)), None), (doMove, doMove, noMove)) ::
          (f2, f2._1 + e(Some(s(i - 1)), None, Some(u(k - 1))), (doMove, noMove, doMove)) ::
          (f3, f3._1 + e(None, Some(t(j - 1)), Some(u(k - 1))), (noMove, doMove, doMove)) ::
          (f4, f4._1 + e(Some(s(i - 1)), None, None), (doMove, noMove, noMove)) ::
          (f5, f5._1 + e(None, Some(t(j - 1)), None), (noMove, doMove, noMove)) ::
          (f6, f6._1 + e(None, None, Some(u(k - 1))), (noMove, noMove, doMove)) :: Nil).reduce((t1, t2) => if (t1._2 >= t2._2) t1 else t2)

        (maxAndMove._2, maxAndMove._3 :: maxAndMove._1._2)
      }
    }
    val f: (Int, sequences.Moves) = F(s.length, t.length, u.length, List())
    alignments += (((s.length, t.length, u.length), f))
    (f._1, f._2, alignments)
  }

  case class Position(i: Int, j: Int, k: Int)

  case class BestAlignmentWithMoveType(alignment: Int, m: Option[MoveType])

  protected[sequences] def iterativeNeedlemanWunsch(s: DNASeq, t: DNASeq, u: DNASeq, sm: SimilarityMatrix) = {


    case class PerformMove(di: Int, dj: Int, dk: Int) {
      private def getMove(i: Int) = if (i == 1) doMove else noMove

      def performOn(i: Int, j: Int, k: Int) = (i - di, j - dj, k - dk)

      def performOn(p: Position) = Position(p.i - di, p.j - dj, p.k - dk)

      val moveType: MoveType = (getMove(di), getMove(dj), getMove(dk))
    }

    def calculateE(s: DNASeq, t: DNASeq, u: DNASeq)(p: Position, pm: PerformMove): Int = {
      val si = if (pm.moveType._1) Some(s(p.i - 1)) else None
      val tj = if (pm.moveType._2) Some(t(p.j - 1)) else None
      val uk = if (pm.moveType._3) Some(u(p.k - 1)) else None
      e(si, tj, uk)(sm)
    }

    def isMarginal(p: Position) = p.i == 0 || p.j == 0 || p.k == 0

    val allowedMoves = {
      val m = (1, 1, 1) ::(1, 1, 0) ::(1, 0, 1) ::(0, 1, 1) ::(1, 0, 0) ::(0, 1, 0) ::(0, 0, 1) :: Nil
      m.map(t => PerformMove(t._1, t._2, t._3)).map(pm => (pm, pm.moveType))
    }
    val moveTypeToPerformMove = allowedMoves.map(_.swap).toMap

    var alignments: mutable.Map[Position, BestAlignmentWithMoveType] = mutable.Map()
    def F(p: Position): BestAlignmentWithMoveType = {
      alignments.get(p) match {
        case Some(d) => d
        case None => if (isMarginal(p)) {
          val costs: (Int, List[(sequences.Move, sequences.Move, sequences.Move)]) = marginalCosts(p.i, p.j, p.k, sm)
          val bawmt = BestAlignmentWithMoveType(costs._1, costs._2.headOption)
          alignments += ((p, bawmt)) // side effect
          bawmt
        }
        else {
          val posToCalculateWithRelativeMoveAndEFunc: List[(Position, sequences.MoveType, Int)] = allowedMoves.map(tuple => {
            val pos: Position = tuple._1.performOn(p)
            val eValue = calculateE(s, t, u)(p, tuple._1)
            (pos, tuple._2, eValue)
          })

          val bestAlignmentForPosition = posToCalculateWithRelativeMoveAndEFunc.map(posAndMoveTypeAndE => {
            val bestForPos: BestAlignmentWithMoveType = F(posAndMoveTypeAndE._1)
            BestAlignmentWithMoveType(bestForPos.alignment + posAndMoveTypeAndE._3, Some(posAndMoveTypeAndE._2))
          }).reduce((bm1, bm2) => if (bm1.alignment >= bm2.alignment) bm1 else bm2 )

          alignments += ((p, bestAlignmentForPosition)) // side effect
          bestAlignmentForPosition
        }
      }
    }

    for {
      i <- 0 to s.length
      j <- 0 to t.length
      k <- 0 to u.length
    } {
      F(Position(i, j, k))
    }
    
    var position: Position = Position(s.length, t.length, u.length)
    var moves: Moves = List()
    var nextBestAlignment = alignments(position)
    while(nextBestAlignment.m.isDefined) {
      val move: sequences.MoveType = nextBestAlignment.m.get
      moves = move :: moves
      position = moveTypeToPerformMove(move).performOn(position)
      nextBestAlignment = alignments(position)
    }
    (alignments(Position(s.length, t.length, u.length)).alignment, moves.reverse, alignments)
  }


  protected[sequences] def formatSequences(s: DNASeq, t: DNASeq, u: DNASeq, m: Moves): (DNASeq, DNASeq, DNASeq) = {
    def formatSeq(seq: DNASeq, f: MoveType => Boolean): DNASeq = {
      m.map(f).map(v => if (v) None else Some(Alphabet.GAP)).foldRight((List[Alphabet.Value](), seq))((charOpt, listWithSeq) =>
        if (charOpt.isDefined) (listWithSeq._1.:+(charOpt.get), listWithSeq._2)
        else (listWithSeq._1.:+(listWithSeq._2.head), listWithSeq._2.tail))._1
    }
    (formatSeq(s, _._1), formatSeq(t, _._2), formatSeq(u, _._3))
  }
}
