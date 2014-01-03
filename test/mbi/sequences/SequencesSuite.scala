package mbi.sequences

import org.scalatest.FlatSpec
import org.scalatest.matchers.Matchers

/**
 * @author Marek Lewandowski <marek.lewandowski@icompass.pl>
 * @since 1/3/14
 */
class SequencesSuite extends FlatSpec with Matchers {
  "A 1+1" should "be equal to 2" in {
    assert(1 + 1 === 2)
  }
}
