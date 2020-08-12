package let

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class EvaluatorSpec extends AnyFlatSpec with Matchers {
  "Let" should "return 10 when input is just 10" in {
    val input = "10"
    val result = "10"
    Evaluator.let(input) shouldEqual result
  }
  "Let" should "return false when input of zero is not zero" in {
    val input = "zero?(3)"
    val result = "false"
    Evaluator.let(input) shouldEqual result
  }
  "Let" should "return value of an arithmetic result" in {
    val input = "let x = 6 in x"
    val result = "6"
    Evaluator.let(input) shouldEqual result
  }

  "Let" should "return correct value after some assignments and a diff" in {
    val input = "let z = 1 in let x = 2 in let y = -(x, 1) in let x = 4 in -(z, -(z, y))"
    val result = "1"
    Evaluator.let(input) shouldEqual result
  }

  "Let" should "return correct value after a proc is defined and applied twice" in {
    val input = "let f = proc (x) -(x, 11) in (f (f 77))"
    val result = "55"
    val environment = Environment.empty.updated("x", Values.IntegerValue(10))
    Evaluator.let(input,environment) shouldEqual result
  }
}
