package let

import let.Environment.Environment
import let.Values.{BooleanValue, IntegerValue}


object Evaluator {

  def ev(expression: Expression, env: Environment): Values = expression match {
    case Expression.ConstExpr(value) => value
    case Expression.VarExpr(value) => Environment.applyForce(env, value)
    case Expression.LetExpr(id, exp, body) =>
      val evaluated = ev(exp, env)
      val updatedEnvironment = Environment.extend(id,evaluated,env)
      ev(body, updatedEnvironment)
    case Expression.DiffExpr(exp1, exp2) =>
      val eval1 = ev(exp1, env)
      val eval2 = ev(exp2, env)
      Values.IntegerValue(toNum(eval1) - toNum(eval2))
    case Expression.ZeroExpr(exp) =>
      val evalExpr = ev(exp, env)
      Values.BooleanValue(toNum(evalExpr) == 0)
    case Expression.CondExpr(ifExp, thenExp, elseExp) =>
      val ifExpr = ev(ifExp, env)
      if(toBool(ifExpr)) ev(thenExp, env) else ev(elseExp, env)
  }

  def let(input: String): String = {
    serialize(ev(fastparse.parse(input, Parser.expr(_)).get.value, Environment.empty))
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
