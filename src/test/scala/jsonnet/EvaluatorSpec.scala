package jsonnet

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class EvaluatorSpec extends AnyFlatSpec with Matchers {
  "Jsonnet" should "return json of correctly defined input" in {
    val input = """local greeting = "Hello ";
       local person = function (name) {
         "name": name,
         "welcome": greeting + name + "!"
       };
       {
         "person1": person("Alice"),
         "person2": person("Bob"),
         "person3": person("Charlie")
       }"""
    val result = """{"person1": {"name": "Alice", "welcome": "Hello Alice!"}, "person2": {"name": "Bob", "welcome": "Hello Bob!"}, "person3": {"name": "Charlie", "welcome": "Hello Charlie!"}}"""
    Evaluator.jsonnet(input) shouldEqual result
  }
}
