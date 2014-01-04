package mbi.sequences

import org.scalatest.FlatSpec
import org.scalatest.matchers.Matchers
import nw.io.SimilarityMatrixReader
import mbi.sequences.sequences.{DNASeq, Moves}

/**
 * @author Marek Lewandowski <marek.m.lewandowski@gmail.com>
 * @since 1/3/14
 */
class SequencesSuite extends FlatSpec with Matchers {

  val similarityMatrixStr = """10 -1 -3 -4 -5
                              |-1 7 -5 -3 -5
                              |-3 -5 9 0 -5
                              |-4 -3 0 8 -5
                              |-5 -5 -5 -5 0""".stripMargin

//  "Iterative NeedlemanWunsch" should "give same results as recursive NeedlemanWunsch" in {
//    println("RUNNING NOW Iterative NeedlemanWunsch")
//
//    val s1f = """>ENA|X74510|X74510.1 M.musculus ANC1 mRNA for adenine nucleotide carrier
//                |GCTGTCGACGGATTCGGGGTGGCGGTGC""".stripMargin
//
//    val s2f = """>ENA|X99953|X99953.1 X.laevis mRNA for P2Y8 nucleotide receptor
//                |CGAAAA""".stripMargin
//
//    val s3f = """>ENA|Z11978|Z11978.1 L.tarentolae gene for pyridine nucleotide linked dehydrogenase.
//                |CTCGAGGGCGGCGGCGGTGG""".stripMargin
//
//    val s1 = App.createSequenceFromLines(s1f.lines)
//    val s2 = App.createSequenceFromLines(s2f.lines)
//    val s3 = App.createSequenceFromLines(s3f.lines)
//    println(s"s1: ${s1.size}, s2: ${s2.size}, s3: ${s3.size}}")
//    val similarityMatrix = SimilarityMatrixReader.read(similarityMatrixStr.lines)
//    val iterative = Sequences.iterativeNeedlemanWunsch(s1, s2, s3, similarityMatrix)
//    val recursive = Sequences.recursiveNeedlemanWunsch(s1, s2, s3, similarityMatrix)
//
//    // Check alignments matrix
//    val iteAlignments = iterative._3
//    val recAlignments = recursive._3
//
//    checkAlignments(s1,s2,s3)(iteAlignments)(recAlignments)
//    println("iteractive moves" + iterative._2 )
//    println("recursive moves" + recursive._2 )
//    assert(iterative._2.size === recursive._2.size, Some("Moves size should be the same"))
//    assert(iterative._1 === recursive._1, Some("Best alignment should be the same"))
//    assert(iterative._2 === recursive._2, Some("Moves should be the same"))
//
//  }

  def checkAlignments(s1: DNASeq, s2: DNASeq, s3: DNASeq)(it: scala.collection.mutable.Map[(Int,Int,Int), Int])(rec: scala.collection.mutable.Map[(Int, Int, Int), (Int, Moves)]) = {
    for {
      i <- 0 to s1.length
      j <- 0 to s2.length
      k <- 0 to s3.length
    } {
      if(it.contains((i,j,k)) && rec.contains((i,j,k))) {
        assert(it((i,j,k)) === rec((i,j,k))._1, Some("Alignments matrix should be the same"))
      }
    }
  }

//  "Iterative NeedlemanWunsch" should "calculate same cost" in {
//    val s1f = """>ENA|X74510|X74510.1 M.musculus ANC1 mRNA for adenine nucleotide carrier
//                |GCT""".stripMargin
//
//    val s2f = """>ENA|X99953|X99953.1 X.laevis mRNA for P2Y8 nucleotide receptor
//                |CGA""".stripMargin
//
//    val s3f = """>ENA|Z11978|Z11978.1 L.tarentolae gene for pyridine nucleotide linked dehydrogenase.
//                |CT""".stripMargin
//
//    val s1 = App.createSequenceFromLines(s1f.lines)
//    val s2 = App.createSequenceFromLines(s2f.lines)
//    val s3 = App.createSequenceFromLines(s3f.lines)
//    val similarityMatrix = SimilarityMatrixReader.read(similarityMatrixStr.lines)
//    println("Sequences with different size")
//    println("ITERATIVE")
//    val iterative = Sequences.iterativeNeedlemanWunsch(s1, s2, s3, similarityMatrix)
//    println("RECURSIVE")
//    val recursive = Sequences.recursiveNeedlemanWunsch(s1, s2, s3, similarityMatrix)
//
//    checkAlignments(s1,s2,s3)(iterative._3)(recursive._3)
//    println("RECURSIVE MOVES print")
//    print(recursive._2)
//    println("Iterative MOVES print")
//    print(iterative._2)
//
//    println(iterative._3)
//    println(recursive._3.map( (  t  => (t._1, t._2._1) )  ))
//
//
//
//    println("interative moves size "+ iterative._2.size )
//    println("recursive moves size "+ recursive._2.size )
//    println("iteractive moves" + iterative._2 )
//    println("recursive moves" + recursive._2 )
//    assert(iterative._2.size === recursive._2.size, Some("there should be same number of moves"))
//    println("interative alignment "+ iterative._1)
//    println("recursive alignment "+ recursive._1)
//    assert(iterative._1 === recursive._1, Some("Best alignment should be the same"))
//    assert(iterative._2 === recursive._2, Some("Moves should be the same"))
//  }

  "A three same sequences" should "be the same on the output" in {
    val sameSeqs = List("AAC", "AAAC", "ACAC", "AGAGAC", "ATTATTC")

    sameSeqs.foreach(s => {
      val seq = App.createSequenceFromLines(s.lines)
      val similarityMatrix = SimilarityMatrixReader.read(similarityMatrixStr.lines)
      val iterative = Sequences.iterativeNeedlemanWunschVersionTwo(seq, seq, seq, similarityMatrix)
      val recursive = Sequences.recursiveNeedlemanWunsch(seq, seq, seq, similarityMatrix)
      assert(iterative._2.size === recursive._2.size, Some("there should be same number of moves"))
      assert(iterative._1 === recursive._1, Some("Best alignment should be the same"))
      assert(iterative._2 === recursive._2, Some("Moves should be the same"))
    })
  }

  "Second version of NeedlemanWunsch" should "finally work as intended" in {
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
    println("Sequences with different size")
    println("ITERATIVE")
    val iterative = Sequences.iterativeNeedlemanWunschVersionTwo(s1, s2, s3, similarityMatrix)
    println("RECURSIVE")
    val recursive = Sequences.recursiveNeedlemanWunsch(s1, s2, s3, similarityMatrix)

//    checkAlignments(s1,s2,s3)(iterative._3)(recursive._3)
    println("RECURSIVE MOVES print")
    print(recursive._2)
    println("Iterative MOVES print")
    print(iterative._2)

//    println(iterative._3)
//    println(recursive._3.map( (  t  => (t._1, t._2._1) )  ))



    println("interative moves size "+ iterative._2.size )
    println("recursive moves size "+ recursive._2.size )
    println("iteractive moves" + iterative._2 )
    println("recursive moves" + recursive._2 )
    assert(iterative._2.size === recursive._2.size, Some("there should be same number of moves"))
    println("interative alignment "+ iterative._1)
    println("recursive alignment "+ recursive._1)
    assert(iterative._1 === recursive._1, Some("Best alignment should be the same"))
    assert(iterative._2 === recursive._2, Some("Moves should be the same"))
  }


  def print(ms: Moves) {
    var i = 0
    var j = 0
    var k = 0
    println(s"($i, $j, $k)")
    ms.reverse.foreach(m => {
      if (m._1) i = i + 1
      if (m._2) j = j + 1
      if (m._3) k = k + 1
      println(s"($i, $j, $k)")
    })
  }
}
