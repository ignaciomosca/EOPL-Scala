package let

import let.Values.{BooleanValue, IntegerValue, ProcedureValue}

object Evaluator {

  def evaluate(expression: Expression, env: Environment, store: Store[Values]): (Values, Store[Values]) =
    expression match {
      case Expression.ConstExpr(value) => (value, store)
      case Expression.VarExpr(value) =>
        (store.deRef(Environment.apply(env, value)), store)
      case Expression.LetExpr(id, exp, body) =>
        val (value, updatedStore1) = evaluate(exp, env, store)
        val (ref, updatedStore2) = updatedStore1.newRef(value)
        val updatedEnvironment = Environment.extend(id, ref, env)
        evaluate(body, updatedEnvironment, updatedStore2)
      case Expression.DiffExpr(exp1, exp2) =>
        val (eval1, updatedStore1) = evaluate(exp1, env, store)
        val (eval2, updatedStore2) = evaluate(exp2, env, updatedStore1)
        (Values.IntegerValue(toNum(eval1) - toNum(eval2)), updatedStore2)
      case Expression.ZeroExpr(exp) =>
        val (evalExpr, updatedStore) = evaluate(exp, env, store)
        (Values.BooleanValue(toNum(evalExpr) == 0), updatedStore)
      case Expression.CondExpr(ifExp, thenExp, elseExp) =>
        val (ifExpr, updatedStore) = evaluate(ifExp, env, store)
        if (toBool(ifExpr)) evaluate(thenExp, env, updatedStore) else evaluate(elseExp, env, updatedStore)
      case Expression.ProcExpr(name, exp) =>
        (Values.ProcedureValue(Procedure(name, exp, env)), store)
      case Expression.CallExpr(exp1, exp2) =>
        val (procVal, updatedStore1) = evaluate(exp1, env, store)
        val (args, updatedStore2) = evaluate(exp2, env, updatedStore1)
        applyProc(toProcedure(procVal), args, updatedStore2)
      case Expression.LetrecExpr(recProcs, letrecBody) =>
        val (updatedEnv, updatedStore) = Environment.extendRec(recProcs, env, store)
        evaluate(letrecBody, updatedEnv, updatedStore)
      case Expression.BeginExpr(exps) => evaluateList(exps, env, store)
      case Expression.AssignExpr(name, exp) =>
        val (value, updatedStore1) = evaluate(exp, env, store)
        val updatedStore2 = updatedStore1.setRef(Environment.apply(env, name), value)
        (value,updatedStore2)
    }

  def evaluateList(exps: List[Expression], env: Environment, store: Store[Values]): (Values, Store[Values]) = exps match {
    case head::Nil => evaluate(head, env, store)
    case head::tail =>
      val (_, updatedStore) = evaluate(head, env, store)
      evaluateList(tail, env, updatedStore)
  }

  def let(input: String, environment: Environment = Environment.empty): String = {
    serialize(
      evaluate(fastparse.parse(input, Parser.expr(_)).get.value, environment, Store[Values](List()))
    )
  }

  def serialize(v: (Values, Store[Values])): String =
    v match {
      case (IntegerValue(value), _) => value.toString
      case (BooleanValue(value), _) => value.toString
      case (ProcedureValue(value), _) => value.toString
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
      case _ => throw new Exception(s"Expected a proc: ${exprValue.toString}")
    }

  def applyProc(proc: Procedure, value: Values, store: Store[Values]): (Values , Store[Values]) = {
    val (refValue, updatedStore) = store.newRef(value)
    val appliedProcedureResult = Environment.extend(proc.name, refValue, proc.environment)
    evaluate(proc.exp, appliedProcedureResult, updatedStore)
  }

}
