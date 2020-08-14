package let

import let.Values.{BooleanValue, IntegerValue, ProcedureValue}

object Evaluator {

  def evaluate(expression: Expression, env: Environment): Values =
    expression match {
      case Expression.ConstExpr(value) => value
      case Expression.VarExpr(value) => Environment.apply(env, value, ProcedureValue(Procedure(value, expression, env)))
      case Expression.LetExpr(id, exp, body) =>
        val evaluated = evaluate(exp, env)
        val updatedEnvironment = Environment.extend(id, evaluated, env)
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
        if (toBool(ifExpr)) evaluate(thenExp, env) else evaluate(elseExp, env)
      case Expression.ProcExpr(name, exp) =>
        Values.ProcedureValue(Procedure(name, exp, env))
      case Expression.CallExpr(exp1, exp2) =>
        val procVal = evaluate(exp1, env)
        val args = evaluate(exp2, env)
        applyProc(toProcedure(procVal), args)
      case Expression.LetrecExpr(pname, bvar, pbody, letrecBody) => evaluate(letrecBody, Environment.extendRec(pname, bvar, pbody, env))
    }

  def let(
      input: String,
      environment: Environment = Environment.empty
  ): String = {
    serialize(
      evaluate(fastparse.parse(input, Parser.expr(_)).get.value, environment)
    )
  }

  def serialize(v: Values): String =
    v match {
      case IntegerValue(value) => value.toString
      case BooleanValue(value) => value.toString
      case ProcedureValue(value) => value.toString
    }

  def toNum(exprValue: Values): Integer =
    exprValue match {
      case IntegerValue(value) => value
      case _ => throw new Exception(s"Expected a number: ${exprValue.toString}")
    }

  def toBool(exprValue: Values): Boolean =
    exprValue match {
      case BooleanValue(value) => value
      case _ => throw new Exception(s"Expected a boolean: ${exprValue.toString}")
    }

  def toProcedure(exprValue: Values): Procedure =
    exprValue match {
      case Values.ProcedureValue(value) => value
      case _                            => throw new Exception(s"Expected a proc: ${exprValue.toString}")
    }

  def applyProc(proc: Procedure, value: Values): Values =
    evaluate(proc.exp, Environment.extend(proc.name, value, proc.environment))

}
