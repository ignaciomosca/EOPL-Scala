package let

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class EvaluatorSpec extends AnyFlatSpec with Matchers {
  "Mutable-Pairs" should "return 10 when input is just 10" in {
    val input = "10"
    val result = "10"
    Evaluator.mutablePairs(input) shouldEqual result
  }
  "Mutable-Pairs" should "return false when input of zero is not zero" in {
    val input = "zero?(3)"
    val result = "false"
    Evaluator.mutablePairs(input) shouldEqual result
  }
  "Mutable-Pairs" should "return value of an arithmetic result" in {
    val input = "let x = 6 in x"
    val result = "6"
    Evaluator.mutablePairs(input) shouldEqual result
  }

  "Mutable-Pairs" should "return correct value after some assignments and a diff" in {
    val input = "let z = 1 in let x = 2 in let y = -(x, 1) in let x = 4 in -(z, -(z, y))"
    val result = "1"
    Evaluator.mutablePairs(input) shouldEqual result
  }

  "Mutable-Pairs" should "return correct value after a proc is defined and applied twice" in {
    val input = "let f = proc (x) -(x, 11) in (f (f 77))"
    val result = "55"
    val env = Environment(Environment.empty.bindings.updated("x", 10))
    val environment = Environment.extend("x", 0, env)
    Evaluator.mutablePairs(input,environment) shouldEqual result
  }

  "Mutable-Pairs" should "return 0" in {
    val input = "let y = 0 in let x = y in let dummy = set y = 11 in x"
    val result = "0"
    Evaluator.mutablePairs(input) shouldEqual result
  }

  "Mutable-Pairs" should "return 11" in {
    val input =
      """let x = 22
        | in let f =
        |      proc (z)
        |        let zz = -(z, x)
        |        in zz
        |    in -((f 66), (f 55)) "
        |""".stripMargin
    val result = "11"
    Evaluator.mutablePairs(input) shouldEqual result
  }

  "Mutable-Pairs" should "begin-end program should return 0" in {
    val input =
      """let g = proc (dummy)
        |              let counter = 0
        |               in
        |                begin
        |                  set counter = -(counter, -(0, 1));
        |                  counter
        |                end
        |    in let a = (g 11)
        |       in let b = (g 11)
        |          in -(a, b)
        |""".stripMargin
    val result = "0"
    Evaluator.mutablePairs(input) shouldEqual result
  }

  "Mutable-Pairs" should "work with mutable pairs and return 1" in {
    val input ="getleft(pair(1, 2))"
    val result = "1"
    Evaluator.mutablePairs(input) shouldEqual result
  }

  "Mutable-Pairs" should "work with mutable pairs and return 2" in {
    val input ="getright(pair(1, 2))"
    val result = "2"
    Evaluator.mutablePairs(input) shouldEqual result
  }

  "Mutable-Pairs" should "work with setright mutable pairs and return 2" in {
    val input ="setright(pair(5, 6), 7)"
    val result = "pair(5, 7)"
    Evaluator.mutablePairs(input) shouldEqual result
  }





  
}
