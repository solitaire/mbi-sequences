package mbi.sequences

import org.scalatest.FlatSpec
import org.scalatest.matchers.Matchers
import nw.structures.Alphabet
import Alphabet._

/**
 * @author Marek Lewandowski <marek.m.lewandowski@gmail.com>
 * @since 1/3/14
 */
class AppSuite extends FlatSpec with Matchers {

  "App" should "convert files to list of symbols in alphabet" in {

    val input = """GCA
                  |AA
                  |CC
                  |TA""".stripMargin
    val sequenceOfLetters: List[Alphabet.Value] = App.createSequenceFromLines(input.lines)

    assert(sequenceOfLetters === List(G, C, A, A, A, C, C, T, A))
  }

  it should "convert real file into sequence of alphabet letters which does not have gaps" in {
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
}
