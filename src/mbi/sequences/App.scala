package mbi.sequences

import scala.io.Source
import nw.structures.Alphabet

/**
 * @author Marek Lewandowski <marek.lewandowski@icompass.pl>
 * @since 1/3/14
 */
object App extends scala.App {
  println("mbi.sequences application has run")
  val usage =
    """ Usage:
      | -seq <path to first sequence> <path to second sequence> <path to third sequence> -sm <path to similarity matrix>
      |
      | Example
      | -seq data/s1 data/s2 data/s3 -sm data/similarity-matrix >> results
    """.stripMargin

  def nextOption(optionsMap: Map[String, Any], remainingArgs: List[String]): Map[String, Any] = {
    remainingArgs match {
      case "-seq" :: p1 :: p2 :: p3 :: tail => nextOption(optionsMap ++ Map("-seq" -> List(p1, p2, p3)), tail)
      case "-sm" :: p :: tail => nextOption(optionsMap ++ Map("-sm" -> p), tail)
      case rest => optionsMap
    }
  }

  def createSequenceFromLines(lines: Iterator[String]) = {
    val HeaderOfSequence = """>.*$""".r
    var seq = List[Alphabet.Value]()
    for (line <- lines) line match {
      case HeaderOfSequence() => ()
      case _ => seq = seq ++ Alphabet(line)
    }
    seq
  }

  if (args.length < 6) println(usage)
  else {
    val optionsMap: Map[String, Any] = nextOption(Map.empty, args.toList)
    if (!(optionsMap.contains("-seq") && optionsMap("-seq").asInstanceOf[List[String]].size == 3)) println(
      """
        |Wrong usage of -seq
        |
        |example: -seq sequence1 ../some/dir/seq2 data/seq3
      """.stripMargin)

    if (!(optionsMap.contains("-sm") && optionsMap("-sm").asInstanceOf[String].size > 0)) println(
      """
        |Wrong usage of -sm
        |
        |example: -sm path/to/similarity-matrix
      """.stripMargin)

    println(optionsMap)

    val sequences = {
      for {
        path <- optionsMap("-seq").asInstanceOf[List[String]]
      } yield {
        val lines: Iterator[String] = Source.fromFile(path).getLines()
        createSequenceFromLines(lines)
      }
    }
  }

}
