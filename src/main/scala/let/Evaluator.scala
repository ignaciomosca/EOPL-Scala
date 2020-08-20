package let

import let.Values.{BooleanValue, IntegerValue, ProcedureValue, RefValue}

object Evaluator {



  def evaluate(expression: Expression, env: Environment, store: Store[Values]): (Values, Store[Values]) =
    expression match {
      case Expression.ConstExpr(value) => (value, store)
      case Expression.VarExpr(value) => (Environment.apply(env, value, ProcedureValue(Procedure(value, expression, env))), store)
      case Expression.LetExpr(id, exp, body) =>
        val (value, updatedStore) = evaluate(exp, env, store)
        val updatedEnvironment = Environment.extend(id, value, env)
        evaluate(body, updatedEnvironment, updatedStore)
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
      case Expression.LetrecExpr(pname, bvar, pbody, letrecBody) => evaluate(letrecBody, Environment.extendRec(pname, bvar, pbody, env), store)
      case Expression.NewRefExpr(exp) =>
        val (value, updatedStore1) = evaluate(exp, env, store)
        val (ref, updatedStore2) = updatedStore1.newRef(value)
        (RefValue(ref), updatedStore2)
      case Expression.DeRefExpr(exp) =>
        val (refVal, updatedStore) = evaluate(exp, env, store)
        val value = updatedStore.deRef(toRef(refVal))
        (value,updatedStore)
      case Expression.SetRefExpr(refExpr, exp) =>
        val (ref, updatedStore1) = evaluate(refExpr, env, store)
        val (value, updatedStore2) = evaluate(exp, env, updatedStore1)
        val updatedStore3 = updatedStore2.setRef(toRef(ref), value)
        (value, updatedStore3)
    }

  def let(
      input: String,
      environment: Environment = Environment.empty
  ): String = {
    serialize(
      evaluate(fastparse.parse(input, Parser.expr(_)).get.value, environment, Store[Values](List()))
    )
  }

  def serialize(v: (Values, Store[Values])): String =
    v match {
      case (IntegerValue(value), _) => value.toString
      case (BooleanValue(value), _) => value.toString
      case (ProcedureValue(value), _) => value.toString
      case (RefValue(value), _) => value.toString
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

  def toRef(refVal: Values): Ref = refVal match {
    case RefValue(ref) =>ref
    case _ => throw new Exception(s"${refVal.toString} is not a ref")
  }

  def applyProc(proc: Procedure, value: Values, store: Store[Values]): (Values , Store[Values]) =
    evaluate(proc.exp, Environment.extend(proc.name, value, proc.environment), store)

}
