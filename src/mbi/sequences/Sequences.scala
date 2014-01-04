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

object Sequences {

  import sequences._

  def NeedlemanWunsch(s: DNASeq, t: DNASeq, u: DNASeq, sm: SimilarityMatrix, recursive: Boolean): (DNASeq, DNASeq, DNASeq, Int) = {

    if (recursive) {
      val results: (Int, sequences.Moves, mutable.Map[(Int, Int, Int), (Int, sequences.Moves)]) = recursiveNeedlemanWunsch(s, t, u, sm)
      val formatted: (DNASeq, DNASeq, DNASeq) = formatSequences(s, t, u, results._2)
      (formatted._1, formatted._2, formatted._3, results._1)
    } else {
      val results: (Int, sequences.Moves) = iterativeNeedlemanWunschVersionTwo(s, t, u, sm)
      val formatted: (DNASeq, DNASeq, DNASeq) = formatSequences(s, t, u, results._2)
      (formatted._1, formatted._2, formatted._3, results._1)
    }
  }

  private def marginalCosts(i: Int, j: Int, k: Int, sm: SimilarityMatrix) = {
    if (i == 0 && j == 0 && k == 0) (0, Nil)
    else if (i == 0 && j == 0) (k * sm.gapCost, (noMove, noMove, doMove) :: Nil)
    else if (i == 0 && k == 0) (j * sm.gapCost, (noMove, doMove, noMove) :: Nil)
    else if (j == 0 && k == 0) (i * sm.gapCost, (doMove, noMove, noMove) :: Nil)
    else if (i == 0) ((j + k) * sm.gapCost, (noMove, doMove, doMove) :: Nil)
    else if (j == 0) ((i + k) * sm.gapCost, (doMove, noMove, doMove) :: Nil)
    else if (k == 0) ((i + j) * sm.gapCost, (doMove, doMove, noMove) :: Nil)
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

        val maxAndMove = ((f, f._1 + e(Some(s(i - 1)), Some(t(j - 1)), Some(u(k - 1))), (doMove, doMove, doMove)) ::
          (f1, f1._1 + e(Some(s(i - 1)), Some(t(j - 1)), None), (doMove, doMove, noMove)) ::
          (f2, f2._1 + e(Some(s(i - 1)), None, Some(u(k - 1))), (doMove, noMove, doMove)) ::
          (f3, f3._1 + e(None, Some(t(j - 1)), Some(u(k - 1))), (noMove, doMove, doMove)) ::
          (f4, f4._1 + e(Some(s(i - 1)), None, None), (doMove, noMove, noMove)) ::
          (f5, f5._1 + e(None, Some(t(j - 1)), None), (noMove, doMove, noMove)) ::
          (f6, f6._1 + e(None, None, Some(u(k - 1))), (noMove, noMove, doMove)) :: Nil).reduce((t1, t2) => if (t1._2 >= t2._2) t1 else t2)

        val moves = maxAndMove._3
        val ii = if (moves._1) i - 1 else i
        val jj = if (moves._2) j - 1 else j
        val kk = if (moves._3) k - 1 else k
        //        println(s"($ii, $jj, $kk)")
        (maxAndMove._2, maxAndMove._3 :: maxAndMove._1._2)
      }
    }
    val f: (Int, sequences.Moves) = F(s.length, t.length, u.length, List())
    alignments += (((s.length, t.length, u.length), f))
    (f._1, f._2, alignments)
  }

  protected[sequences] def iterativeNeedlemanWunschVersionTwo(s: DNASeq, t: DNASeq, u: DNASeq, sm: SimilarityMatrix) = {

    case class Position(i: Int, j: Int, k: Int)

    case class BestAlignmentWithMoveType(alignment: Int, m: Option[MoveType])

    case class PerformMove(di: Int, dj: Int, dk: Int) {
      private def getMove(i: Int) = if (i == 1) doMove else noMove

      def performOn(i: Int, j: Int, k: Int) = (i - di, j - dj, k - dk)

      def performOn(p: Position) = Position(p.i - di, p.j - dj, p.k - dk)

      val moveType: MoveType = (getMove(di), getMove(dj), getMove(dk))
    }

    def calculateE(s: DNASeq, t: DNASeq, u: DNASeq)(p: Position, pm: PerformMove): Int = {
      val si = if (pm.moveType._1) Some(s(p.i - 1)) else None
      val tj = if (pm.moveType._2) Some(s(p.j - 1)) else None
      val uk = if (pm.moveType._3) Some(s(p.k - 1)) else None
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
    (alignments(Position(s.length, t.length, u.length)).alignment, moves)
  }

  protected[sequences] def iterativeNeedlemanWunsch(s: DNASeq, t: DNASeq, u: DNASeq, sm: SimilarityMatrix) = {
    var alignments: mutable.Map[(Int, Int, Int), Int] = mutable.Map()
    implicit val smm = sm

    def get(i: Int, j: Int, k: Int) = {

      if (i == 0 || j == 0 || k == 0) {
        marginalCosts(i, j, k, sm)._1
      }
      else if (!alignments.contains(i, j, k)) {
        throw new Error(s"alignments do not contain ($i, $j, $k)")
      }
      else alignments(i, j, k)
    }

    def F(i: Int, j: Int, k: Int) = {
      if (i == 0 && j == 0 && k == 0) 0
      else if (i == 0 || j == 0 || k == 0) {
        marginalCosts(i, j, k, sm)._1
      }
      else
        alignments.get((i, j, k)) match {
          case Some(cost) => cost
          case None => {
            val costs = (get(i - 1, j - 1, k - 1) +
              e(Some(s(i - 1)), Some(t(j - 1)), Some(u(k - 1)))) ::
              (get(i - 1, j - 1, k) +
                e(Some(s(i - 1)), Some(t(j - 1)), None)) ::
              (get(i - 1, j, k - 1) +
                e(Some(s(i - 1)), None, Some(u(k - 1)))) ::
              (get(i, j - 1, k - 1) +
                e(None, Some(t(j - 1)), Some(u(k - 1)))) ::
              (get(i - 1, j, k) +
                e(Some(s(i - 1)), None, None)) ::
              (get(i, j - 1, k) +
                e(None, Some(t(j - 1)), None)) ::
              (get(i, j, k - 1) +
                e(None, None, Some(u(k - 1)))) :: Nil
            val max: Int = costs.reduce(_ max _)
            alignments += (((i, j, k), max))
            max
          }
        }
    }

    for {
      i <- 0 to s.length
      j <- 0 to t.length
      k <- 0 to u.length
    } {
      F(i, j, k)
    }

    val (recursiveCost, recursiveMoves, recursiveAlignment): (Int, sequences.Moves, mutable.Map[(Int, Int, Int), (Int, sequences.Moves)]) = recursiveNeedlemanWunsch(s, t, u, sm)

    var i = 0
    var j = 0
    var k = 0
    // Head move is for (0,0,0), last is for (s.length, t.length, u.length)
    var moves: Moves = List()
    var totalCost = 0
    while (!(i == s.length && j == t.length && k == u.length)) {

      var costsWithMoves: mutable.MutableList[(Int, (sequences.Move, sequences.Move, sequences.Move))] = mutable.MutableList()

      if (i + 1 <= s.length && j + 1 <= t.length && k + 1 <= u.length) {
        val f1 = (F(i + 1, j + 1, k + 1), (doMove, doMove, doMove))
        costsWithMoves += f1

        assert(recursiveAlignment((i + 1, j + 1, k + 1))._1 == f1._1)

      }
      if (i + 1 <= s.length && j + 1 <= t.length) {


        val f2 = (F(i + 1, j + 1, k), (doMove, doMove, noMove))
        costsWithMoves += f2

        assert(recursiveAlignment((i + 1, j + 1, k))._1 == f2._1)

      }
      if (i + 1 <= s.length && k + 1 <= u.length) {

        val f3 = (F(i + 1, j, k + 1), (doMove, noMove, doMove))
        costsWithMoves += f3

        assert(recursiveAlignment((i + 1, j, k + 1))._1 == f3._1)

      }
      if (j + 1 <= t.length && k + 1 <= u.length) {

        val f4 = (F(i, j + 1, k + 1), (noMove, doMove, doMove))
        costsWithMoves += f4

        assert(recursiveAlignment((i, j + 1, k + 1))._1 == f4._1)
      }
      if (i + 1 <= s.length) {

        val f5 = (F(i + 1, j, k), (doMove, noMove, noMove))
        costsWithMoves += f5
        assert(recursiveAlignment((i + 1, j, k))._1 == f5._1)


      }
      if (j + 1 <= t.length) {

        val f6 = (F(i, j + 1, k), (noMove, doMove, noMove))
        costsWithMoves += f6

        assert(recursiveAlignment((i, j + 1, k))._1 == f6._1)

      }
      if (k + 1 <= u.length) {
        val f7 = (F(i, j, k + 1), (noMove, noMove, doMove))
        costsWithMoves += f7

        assert(recursiveAlignment((i, j, k + 1))._1 == f7._1)

      }
      // old
      //

      // There are duplicates
      //      require(costsWithMoves.map(_._1).toSet.size == costsWithMoves.size)

      val bestCostWithMove: (Int, (sequences.Move, sequences.Move, sequences.Move)) = costsWithMoves.reduce((fm1, fm2) => if (fm1._1 >= fm2._1) fm1 else fm2)
      println(s"($i, $j, $k)")
      val move: (sequences.Move, sequences.Move, sequences.Move) = bestCostWithMove._2
      moves = move :: moves

      println(s"recursiveMoves $recursiveMoves")
      println(s"recursiveMovesInSameOrderAsIterative $recursiveMoves")
      println("Recursive Move" + recursiveMoves(moves.size - 1))
      println("Iterative Move" + move)
      val recursiveMove: sequences.MoveType = recursiveMoves(moves.size - 1)
      require(recursiveMove == move, s"Comparing move #${moves.size - 1} Recursive move: $recursiveMove iterative move: $move All recursive moves $recursiveMoves")

      totalCost += bestCostWithMove._1
      if (move._1) i = i + 1
      if (move._2) j = j + 1
      if (move._3) k = k + 1

    }
    val cost: Int = get(s.length, t.length, u.length)
    println(s"accumulated cost via moves: $totalCost cost of 'right down corner' $cost")
    //    require(totalCost == cost)
    (cost, moves, alignments)
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
