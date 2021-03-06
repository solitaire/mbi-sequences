package mbi.sequences

import scala.collection.mutable

import mbi.sequences.io.SimilarityMatrixReader
import mbi.sequences.sequences.DNASeq
import org.scalatest.FlatSpec
import org.scalatest.matchers.Matchers

/**
 * @author Marek Lewandowski <marek.m.lewandowski@gmail.com>
 * @author Anna Stępień
 * @since 1/3/14
 */
class SequencesSuite extends FlatSpec with Matchers {

  val similarityMatrixStr = """10 -1 -3 -4 -5
                              |-1 7 -5 -3 -5
                              |-3 -5 9 0 -5
                              |-4 -3 0 8 -5
                              |-5 -5 -5 -5 0""".stripMargin

  def checkAlignments(s1: DNASeq, s2: DNASeq, s3: DNASeq)(it: scala.collection.mutable.Map[(Int,Int,Int), Int])(rec: scala.collection.mutable.Map[(Int, Int, Int), Int]) = {
    for {
      i <- 0 to s1.length
      j <- 0 to s2.length
      k <- 0 to s3.length
    } {
      if(it.contains((i,j,k)) && rec.contains((i,j,k))) {
        assert(it((i,j,k)) === rec((i,j,k)), Some(s"Alignments matrix should be the same for key ($j, $j, $k)"))
      }
    }
  }

  "Iterative and Recursive NeedlemanWunsch" should "work on different sequences and give same results" in {
    val s1f = """>ENA|X74510|X74510.1 M.musculus ANC1 mRNA for adenine nucleotide carrier
                |TGCA""".stripMargin

    val s2f = """>ENA|X99953|X99953.1 X.laevis mRNA for P2Y8 nucleotide receptor
                |TGGTGC""".stripMargin

    val s3f = """>ENA|Z11978|Z11978.1 L.tarentolae gene for pyridine nucleotide linked dehydrogenase.
                |TGCA""".stripMargin

    val s1 = App.createSequenceFromLines(s1f.lines)
    val s2 = App.createSequenceFromLines(s2f.lines)
    val s3 = App.createSequenceFromLines(s3f.lines)
    val similarityMatrix = SimilarityMatrixReader.read(similarityMatrixStr.lines)
    val iterative = Sequences.iterativeNeedlemanWunsch(s1, s2, s3, similarityMatrix)
    val recursive = Sequences.recursiveNeedlemanWunsch(s1, s2, s3, similarityMatrix)

    val map: mutable.Map[(Int, Int, Int), Int] = iterative._3.map(t => ((t._1.i, t._1.j, t._1.k), t._2.alignment))
    checkAlignments(s1, s2, s3)(map)(recursive._3.map( t => (t._1, t._2._1)  ))
    assert(iterative._2.size === recursive._2.size, Some("there should be same number of moves"))
    assert(iterative._1 === recursive._1, Some("Best alignment should be the same"))
    assert(iterative._2 === recursive._2, Some("Moves should be the same"))
  }

  it should "produce same sequences on the output given identical sequences" in {
    val sameSeqs = List("AAC", "AAAC", "ACAC", "AGAGAC", "ATTATTC")

    sameSeqs.foreach(s => {
      val seq = App.createSequenceFromLines(s.lines)
      val similarityMatrix = SimilarityMatrixReader.read(similarityMatrixStr.lines)
      val iterative = Sequences.iterativeNeedlemanWunsch(seq, seq, seq, similarityMatrix)
      val map: mutable.Map[(Int, Int, Int), Int] = iterative._3.map(t => ((t._1.i, t._1.j, t._1.k), t._2.alignment))
      val recursive = Sequences.recursiveNeedlemanWunsch(seq, seq, seq, similarityMatrix)
      checkAlignments(seq, seq ,seq)(map)(recursive._3.map( t => (t._1, t._2._1)  ))
      assert(iterative._2.size === recursive._2.size, Some("there should be same number of moves"))
      assert(iterative._1 === recursive._1, Some("Best alignment should be the same"))
      assert(iterative._2 === recursive._2, Some("Moves should be the same"))

      val (seq1, seq2, seq3, a1) = Sequences.NeedlemanWunsch(seq, seq, seq, similarityMatrix, recursive = true)
      assert(seq1 == seq)
      assert(seq2 == seq)
      assert(seq3 == seq)
      assert(a1 == (seq1, seq2, seq3).zipped.foldLeft(0)( _ + similarityMatrix.get(_)))
      val (seq1i, seq2i, seq3i, a2) = Sequences.NeedlemanWunsch(seq, seq, seq, similarityMatrix, recursive = false)
      assert(seq1i == seq)
      assert(seq2i == seq)
      assert(seq3i == seq)
      assert(a2 == (seq1i, seq2i, seq3i).zipped.foldLeft(0)( _ + similarityMatrix.get(_)))
    })
  }

  "Alignment cost" should "match value computed using similarity matrix" in {
    val s1f = """GGAGAATAGGAATAGGAATGGGTGAATAGATTGAAAGATAGAATAAGTCGTATTTAACTAACTCCCAATCCACCATTCTCAATTCCTCCAACATACTCG""".stripMargin

    val s2f = """GGAGAATAGGAATAGGGTGAATAGATTGAAAGATAGAATAAGTCGTATTTAACTAACTCCCAATCCACCAAATTCCTCCAACATACTCG""".stripMargin

    val s3f = """GGAGAATAGGAATAGGAATGGGTGAATAGATTGAAAGATAGAATAAGTCGTATTTAACTAACTCCCAATCCACCATTCTCAATTCCTCCAACAT""".stripMargin

    val s1 = App.createSequenceFromLines(s1f.lines)
    val s2 = App.createSequenceFromLines(s2f.lines)
    val s3 = App.createSequenceFromLines(s3f.lines)
    val similarityMatrix = SimilarityMatrixReader.read(similarityMatrixStr.lines)

    val (seq1, seq2, seq3, a1) = Sequences.NeedlemanWunsch(s1, s2, s3, similarityMatrix, recursive = true)
    val (seq1i, seq2i, seq3i, a2) = Sequences.NeedlemanWunsch(s1, s2, s3, similarityMatrix, recursive = false)

    assert(a1 == (seq1, seq2, seq3).zipped.foldLeft(0)( _ + similarityMatrix.get(_)))
    assert(a2 == (seq1i, seq2i, seq3i).zipped.foldLeft(0)( _ + similarityMatrix.get(_)))
  }

}
