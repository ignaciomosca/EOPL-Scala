package let

import let.Environment.Environment
import let.Values.{BooleanValue, IntegerValue}


object Evaluator {

  def evaluate(expression: Expression, env: Environment): Values = expression match {
    case Expression.ConstExpr(value) => value
    case Expression.VarExpr(value) => Environment.applyForce(env, value)
    case Expression.LetExpr(id, exp, body) =>
      val evaluated = evaluate(exp, env)
      val updatedEnvironment = Environment.extend(id,evaluated,env)
      evaluate(body, updatedEnvironment)
    case Expression.DiffExpr(exp1, exp2) =>
      val eval1 = evaluate(exp1, env)
      val eval2 = evaluate(exp2, env)
      Values.IntegerValue(toNum(eval1) - toNum(eval2))
    case Expression.ZeroExpr(exp) =>
      val evalExpr = evaluate(exp, env)
      Values.BooleanValue(toNum(evalExpr) == 0)
    case Expression.CondExpr(ifExp, thenExp, elseExp) =>
      val ifExpr = evaluate(ifExp, env)
      if(toBool(ifExpr)) evaluate(thenExp, env) else evaluate(elseExp, env)
  }

  def let(input: String): String = {
    serialize(evaluate(fastparse.parse(input, Parser.expr(_)).get.value, Environment.empty))
  }

  def serialize(v: Values): String = v match {
    case IntegerValue(value) => value.toString
    case BooleanValue(value) => value.toString
  }

  def toNum(exprValue: Values):Integer = exprValue match {
    case IntegerValue(value) => value
    case BooleanValue(_) => throw new Exception(s"Expected a number: ${exprValue.toString}")
  }

  def toBool(exprValue: Values):Boolean = exprValue match {
    case BooleanValue(value) => value
    case IntegerValue(_) => throw new Exception(s"Expected a boolean: ${exprValue.toString}")
  }

}
