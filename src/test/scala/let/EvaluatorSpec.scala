package let

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class EvaluatorSpec extends AnyFlatSpec with Matchers {
  "Implicit-Refs" should "return 10 when input is just 10" in {
    val input = "10"
    val result = "10"
    Evaluator.let(input) shouldEqual result
  }
  "Implicit-Refs" should "return false when input of zero is not zero" in {
    val input = "zero?(3)"
    val result = "false"
    Evaluator.let(input) shouldEqual result
  }
  "Implicit-Refs" should "return value of an arithmetic result" in {
    val input = "let x = 6 in x"
    val result = "6"
    Evaluator.let(input) shouldEqual result
  }

  "Implicit-Refs" should "return correct value after some assignments and a diff" in {
    val input = "let z = 1 in let x = 2 in let y = -(x, 1) in let x = 4 in -(z, -(z, y))"
    val result = "1"
    Evaluator.let(input) shouldEqual result
  }

  "Implicit-Refs" should "return correct value after a proc is defined and applied twice" in {
    val input = "let f = proc (x) -(x, 11) in (f (f 77))"
    val result = "55"
    val env = Environment(Environment.empty.bindings.updated("x", 10))
    val environment = Environment.extend("x", 0, env)
    Evaluator.let(input,environment) shouldEqual result
  }

  "Implicit-Refs" should "return 0" in {
    val input = "let y = 0 in let x = y in let dummy = set y = 11 in x"
    val result = "0"
    Evaluator.let(input) shouldEqual result
  }

  "Implicit-Refs" should "return 11" in {
    val input =
      """let x = 22
        | in let f =
        |      proc (z)
        |        let zz = -(z, x)
        |        in zz
        |    in -((f 66), (f 55)) "
        |""".stripMargin
    val result = "11"
    Evaluator.let(input) shouldEqual result
  }

  "Implicit-Refs" should "begin-end program should return 0" in {
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
    Evaluator.let(input) shouldEqual result
  }





  
}
