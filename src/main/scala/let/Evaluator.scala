package let

import let.Environment.Environment
import let.Values.{BooleanValue, EmptyList, IntegerValue}


object Evaluator {

  def evaluateList(value: List[Expression], env: Environment): Values = value.isEmpty match {
    case true => EmptyList
    case false if value.size == 1 => evaluate(value.head,env)
    case false if value.size > 1 => evaluate(value.foldLeft(value.head)((acc, e) => Expression.Plus(acc, e)),env)
  }

  def evaluateBinaryOp(op: BinOp, v1: Values, v2: Values, env: Environment): Values = (op,v1,v2) match {
    case (BinOp.Add, IntegerValue(val1), IntegerValue(val2)) => IntegerValue(val1+val2)
    case (BinOp.Sub, IntegerValue(val1), IntegerValue(val2)) => IntegerValue(val1-val2)
    case (BinOp.Mul, IntegerValue(val1), IntegerValue(val2)) => IntegerValue(val1*val2)
    case (BinOp.Div, IntegerValue(val1), IntegerValue(val2)) => IntegerValue(val1/val2)
    case (BinOp.Gt, IntegerValue(val1), IntegerValue(val2)) => BooleanValue(val1 > val2)
    case (BinOp.Lt, IntegerValue(val1), IntegerValue(val2)) => BooleanValue(val1 < val2)
    case (BinOp.Eq, IntegerValue(val1), IntegerValue(val2)) => BooleanValue(val1==val2)
    case _ => EmptyList
  }

  def evaluate(expr: Expression, env: Environment): Values = expr match {
    case Expression.ConstExpr(value) => value
    case Expression.VarExpr(value) => Environment.applyForce(env, value)
    case Expression.LetExpr(name, exp, body) =>
      val evaluated = evaluate(exp, env)
      val updatedEnvironment = Environment.extend(name,evaluated,env)
      evaluate(body, updatedEnvironment)
    case Expression.BinOpExpr(op, exp1, exp2) => evaluateBinaryOp(op, evaluate(exp1, env),evaluate(exp2, env),env)
    case Expression.EmptyListExpr => EmptyList
    case Expression.ListExpr(value) => evaluateList(value, env)
    case Expression.CondExpr(ifExp,thenExp,elseExp) =>
      val evalIf = evaluate(ifExp, env)
      evalIf match { // if the types don't match it evaluates to false
        case IntegerValue(_) => evaluate(elseExp, env)
        case Values.BooleanValue(value) if value=>evaluate(thenExp, env)
        case Values.BooleanValue(_) =>evaluate(elseExp, env)
        case Values.ExprList(_) =>evaluate(elseExp, env)
        case Values.EmptyList =>evaluate(elseExp, env)
      }
    case Expression.Call(expr, args) => EmptyList
    case Expression.Plus(left, right) => EmptyList
  }

  def serialize(v: Values): String = v match {
    case IntegerValue(value) => value.toString
    case BooleanValue(value) =>value.toString
    case Values.ExprList(value) if value.nonEmpty =>s"""[${value.map(serialize).mkString(",")}]"""
    case Values.ExprList(value) if value.isEmpty => "[]"
    case Values.EmptyList =>"[]"
  }

  def let(input: String): String = {
    serialize(evaluate(fastparse.parse(input, Parser.expr(_)).get.value, Environment.empty))
  }

}
