package let

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class EvaluatorSpec extends AnyFlatSpec with Matchers {
  "Continuation-Passing" should "return 10 when input is just 10" in {
    val input = "10"
    val result = "10"
    Evaluator.continuationPassing(input) shouldEqual result
  }
  "Continuation-Passing" should "return false when input of zero is not zero" in {
    val input = "zero?(3)"
    val result = "false"
    Evaluator.continuationPassing(input) shouldEqual result
  }
  "Continuation-Passing" should "return value of an arithmetic result" in {
    val input = "let x = 6 in x"
    val result = "6"
    Evaluator.continuationPassing(input) shouldEqual result
  }

  "Continuation-Passing" should "return correct value after some assignments and a diff" in {
    val input = "let z = 1 in let x = 2 in let y = -(x, 1) in let x = 4 in -(z, -(z, y))"
    val result = "1"
    Evaluator.continuationPassing(input) shouldEqual result
  }

  "Continuation-Passing" should "return correct value after a proc is defined and applied twice" in {
    val input = "let f = proc (x) -(x, 11) in (f (f 77))"
    val result = "55"
    val environment = Environment.extend("x", ExpVal.IntegerValue(10), Environment.empty)
    Evaluator.continuationPassing(input,environment) shouldEqual result
  }

  "Continuation-Passing" should "test if then" in {
    val input = "if zero?(x) then 0 else x"
    val result = "0"
    val environment = Environment.extend("x", ExpVal.IntegerValue(0), Environment.empty)
    Evaluator.continuationPassing(input,environment) shouldEqual result
  }

  "Continuation-Passing" should "test if else" in {
    val input = "if zero?(x) then 0 else x"
    val result = "10"
    val environment = Environment.extend("x", ExpVal.IntegerValue(10), Environment.empty)
    Evaluator.continuationPassing(input,environment) shouldEqual result
  }

  "Continuation-Passing" should "return correct value after a letrec operation is applied" in {
    val input = "letrec double(x) = if zero?(x) then 0 else -((double -(x,1)), -(0,2)) in (double 6)"
    val result = "12"
    Evaluator.continuationPassing(input,Environment.empty) shouldEqual result
  }
}
