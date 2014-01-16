package mbi.sequences

import org.scalatest.FlatSpec
import org.scalatest.matchers.Matchers
import mbi.sequences.structures.Alphabet._
import mbi.sequences.io.SimilarityMatrixReader
import mbi.sequences.structures.SimilarityMatrix

/**
 * @author Marek Lewandowski <marek.m.lewandowski@gmail.com>
 * @author Anna Stępień
 * @since 1/3/14
 */
class AppSuite extends FlatSpec with Matchers {

  "App" should "convert files to list of symbols in alphabet" in {

    val input = """GCA
                  |AA
                  |CC
                  |TA""".stripMargin
    val sequenceOfLetters: List[Value] = App.createSequenceFromLines(input.lines)

    assert(sequenceOfLetters === List(G, C, A, A, A, C, C, T, A))
  }

  it should "convert default file into sequence of alphabet letters which does not have gaps" in {
    val input = """|GCAGTCAGTTGGTCTGGGCACTGCAGCAGGCTCGGCTCTGTCCCAGCACTTGTCTGGGAG
                   |AAAAGTGGTGTTACTCACCCAGGGAGAGTCTCTCTTTCTACCTTCCTTCTTTCTCGATCT
                   |CCTTGTGTGCTTTTGTGTTTCTTTATTTCTTTTCCTTTTTTTTCTTTTTTTTTTTTTTGT
                   |TACTTAATTATATTCCTAATCCTGGATGAAGTTGCTGGATTCTGCAGCACAAGTCTTCAT
                   |GAACAAGCAGCACCGCTCAGAGATTTCACGGCATTCAAAGGTCACAGAACTGCCACTATG
                   |GTTAAATGTCTTGTTTAATGGTTGAGAGGTGTGGCGAAATCTTGTTTGAGAACCCCGATC
                   |AGAATGCCAAATGTGTTTGCATGCTGGGAGATATACGACTAAGGGGTCAGACGGGGGTTC
                   |GTGCTGAACGCCGTGGCTCCTACCCATTCATTGACTTCCGCCTACTTAACAGTACAACAT
                   |ACTCAGGGGAGATTGGCACCAAGAAAAAGGTGAAAAGACTATTAAGCTTTCAAAGATACT""".stripMargin

    assert(App.createSequenceFromLines(input.lines).forall(_ != GAP))
  }

  it should "convert fasta file into sequence of alphabet letters which does not have gaps" in {
    val input = """>ENA|AB038040|AB038040.1 Homo sapiens HSPDE7B mRNA for cyclic nucleotide phosphodiesterase 7B, complete cds.
                  |GCAGTCAGTTGGTCTGGGCACTGCAGCAGGCTCGGCTCTGTCCCAGCACTTGTCTGGGAG
                  |AAAAGTGGTGTTACTCACCCAGGGAGAGTCTCTCTTTCTACCTTCCTTCTTTCTCGATCT
                  |CCTTGTGTGCTTTTGTGTTTCTTTATTTCTTTTCCTTTTTTTTCTTTTTTTTTTTTTTGT
                  |TACTTAATTATATTCCTAATCCTGGATGAAGTTGCTGGATTCTGCAGCACAAGTCTTCAT
                  |GAACAAGCAGCACCGCTCAGAGATTTCACGGCATTCAAAGGTCACAGAACTGCCACTATG
                  |GTTAAATGTCTTGTTTAATGGTTGAGAGGTGTGGCGAAATCTTGTTTGAGAACCCCGATC
                  |AGAATGCCAAATGTGTTTGCATGCTGGGAGATATACGACTAAGGGGTCAGACGGGGGTTC
                  |GTGCTGAACGCCGTGGCTCCTACCCATTCATTGACTTCCGCCTACTTAACAGTACAACAT
                  |ACTCAGGGGAGATTGGCACCAAGAAAAAGGTGAAAAGACTATTAAGCTTTCAAAGATACT""".stripMargin

    assert(App.createSequenceFromLines(input.lines).forall(_ != GAP))
  }

  it should "read sequence" in {
    val input = """>ENA|X99953|X99953.1 X.laevis mRNA for P2Y8 nucleotide receptor
                |TGGTGC""".stripMargin

    val sequenceOfLetters: List[Value] = App.createSequenceFromLines(input.lines)
    assert(sequenceOfLetters === List(T,G,G,T,G,C))
  }

  "SimilarityMatrix" should "be read correctly" in {
    val similarityMatrixStr = """10 -1 -3 -4 -5
                                |-1 7 -5 -3 -5
                                |-3 -5 9 0 -5
                                |-4 -3 0 8 -5
                                |-5 -5 -5 -5 0""".stripMargin

    val similarityMatrix = SimilarityMatrixReader.read(similarityMatrixStr.lines)

    val expected = new SimilarityMatrix(Map(
      (A, Map((A, 10), (G, -1), (C, -3), (T, -4), (GAP, -5))),
      (G, Map((A, -1), (G,  7), (C, -5), (T, -3), (GAP, -5))),
      (C, Map((A, -3), (G, -5), (C,  9), (T,  0), (GAP, -5))),
      (T, Map((A, -4), (G, -3), (C,  0), (T,  8), (GAP, -5))),
      (GAP, Map((A, -5), (G, -5), (C, -5), (T, -5), (GAP, 0)))))

    assert(similarityMatrix === expected)
  }
}
