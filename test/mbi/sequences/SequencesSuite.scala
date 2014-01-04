package mbi.sequences

import org.scalatest.FlatSpec
import org.scalatest.matchers.Matchers
import scala.util.Random
import nw.io.SimilarityMatrixReader
import nw.structures.Alphabet

/**
 * @author Marek Lewandowski <marek.m.lewandowski@gmail.com>
 * @since 1/3/14
 */
class SequencesSuite extends FlatSpec with Matchers {

  "Iterative NeedlemanWunsch" should "give same results as recursive NeedlemanWunsch" in {
    println("RUNNING NOW Iterative NeedlemanWunsch")
    val similarityMatrixStr = """10 -1 -3 -4 -5
                                |-1 7 -5 -3 -5
                                |-3 -5 9 0 -5
                                |-4 -3 0 8 -5
                                |-5 -5 -5 -5 0""".stripMargin

    val s1f = """>ENA|X74510|X74510.1 M.musculus ANC1 mRNA for adenine nucleotide carrier
                |GCTGTCGACGGATTCGGGGTGGCGGTGC""".stripMargin

    val s2f = """>ENA|X99953|X99953.1 X.laevis mRNA for P2Y8 nucleotide receptor
                |CGAAAA""".stripMargin

    val s3f = """>ENA|Z11978|Z11978.1 L.tarentolae gene for pyridine nucleotide linked dehydrogenase.
                |CTCGAGGGCGGCGGCGGTGG""".stripMargin

    val s1 = App.createSequenceFromLines(s1f.lines)
    val s2 = App.createSequenceFromLines(s2f.lines)
    val s3 = App.createSequenceFromLines(s3f.lines)
    println(s"s1: ${s1.size}, s2: ${s2.size}, s3: ${s3.size}}")
    val similarityMatrix = SimilarityMatrixReader.read(similarityMatrixStr.lines)
    val iterative: (Int, _root_.mbi.sequences.sequences.Moves) = Sequences.iterativeNeedlemanWunsch(s1, s2, s3, similarityMatrix)
    val recursive: (Int, _root_.mbi.sequences.sequences.Moves) = Sequences.recursiveNeedlemanWunsch(s1, s2, s3, similarityMatrix)
    assert(iterative._2.size === recursive._2.size, Some("Moves size should be the same"))
    assert(iterative._1 === recursive._1, Some("Best alignment should be the same"))
    assert(iterative._2 === recursive._2, Some("Moves should be the same"))

  }

  "Iterative NeedlemanWunsch" should "calculate same cost" in {
    val similarityMatrixStr = """10 -1 -3 -4 -5
                                |-1 7 -5 -3 -5
                                |-3 -5 9 0 -5
                                |-4 -3 0 8 -5
                                |-5 -5 -5 -5 0""".stripMargin

    val s1f = """>ENA|X74510|X74510.1 M.musculus ANC1 mRNA for adenine nucleotide carrier
                |GCT""".stripMargin

    val s2f = """>ENA|X99953|X99953.1 X.laevis mRNA for P2Y8 nucleotide receptor
                |CGA""".stripMargin

    val s3f = """>ENA|Z11978|Z11978.1 L.tarentolae gene for pyridine nucleotide linked dehydrogenase.
                |CT""".stripMargin

    val s1 = App.createSequenceFromLines(s1f.lines)
    val s2 = App.createSequenceFromLines(s2f.lines)
    val s3 = App.createSequenceFromLines(s3f.lines)
    val similarityMatrix = SimilarityMatrixReader.read(similarityMatrixStr.lines)
    val iterative: (Int, _root_.mbi.sequences.sequences.Moves) = Sequences.iterativeNeedlemanWunsch(s1, s2, s3, similarityMatrix)
    val recursive: (Int, _root_.mbi.sequences.sequences.Moves) = Sequences.recursiveNeedlemanWunsch(s1, s2, s3, similarityMatrix)
    println("interative moves size "+ iterative._2.size )
    println("recursive moves size "+ recursive._2.size )
    println("iteractive moves" + iterative._2 )
    println("recursive moves" + recursive._2 )
    assert(iterative._2.size === recursive._2.size, Some("there should be same number of moves"))
    println("interative alignment "+ iterative._1)
    println("recursive alignment "+ recursive._1)
    assert(iterative._1 === recursive._1, Some("Best alignment should be the same"))
  }
}
