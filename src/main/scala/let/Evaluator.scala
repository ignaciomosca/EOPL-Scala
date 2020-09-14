package let

import let.Expression._
import let.ExpVal._

object Evaluator {

  private def applyContinuation(cont: Continuation, value: ExpVal): ExpVal =
    cont match {
      case EndCont => value
      case ZeroCont(nextCont) =>
        applyContinuation(nextCont, BooleanValue(toNum(value) == 0))
      case LetCont(variable, body, env, nextCont) =>
        evaluate(body, ExtendEnv(variable, value, env), nextCont)
      case CondCont(thenExp, elseExp, env, nextCont) =>
        if (toBool(value)) {
          evaluate(thenExp, env, nextCont)
        } else {
          evaluate(elseExp, env, nextCont)
        }
      case DiffCont(expr, env, nextCont) =>
        evaluate(expr, env, DiffAuxCont(value, nextCont))
      case DiffAuxCont(firstValue, nextCont) =>
        applyContinuation(
          nextCont,
          IntegerValue(toNum(firstValue) - toNum(value))
        )
      case CallExprCont(arg, env, nextCont) =>
        evaluate(arg, env, CallExprAuxCont(value, nextCont))
      case CallExprAuxCont(proc, nextCont) =>
        applyProc(toProcedure(proc), value, nextCont)
    }

  private def evaluate(
      expression: Expression,
      env: Environment,
      cont: Continuation
  ): ExpVal =
    expression match {
      case ConstExpr(value) => applyContinuation(cont, value)
      case VarExpr(value) =>
        applyContinuation(cont, Environment.apply(env, value))
      case LetExpr(id, exp, body) =>
        evaluate(exp, env, LetCont(id, body, env, cont))
      case DiffExpr(exp1, exp2) =>
        evaluate(exp1, env, DiffCont(exp2, env, cont))
      case ZeroExpr(exp) => evaluate(exp, env, ZeroCont(cont))
      case CondExpr(ifExp, thenExp, elseExp) =>
        evaluate(ifExp, env, CondCont(thenExp, elseExp, env, cont))
      case ProcExpr(name, exp) =>
        applyContinuation(
          cont,
          ExpVal.ProcedureValue(Procedure(name, exp, env))
        )
      case CallExpr(exp1, exp2) =>
        evaluate(exp1, env, CallExprCont(exp2, env, cont))
      case LetrecExpr(pname, bvar, pbody, letrecBody) =>
        evaluate(
          letrecBody,
          Environment.extendRec(pname, bvar, pbody, env),
          cont
        )
    }

  def continuationPassing(
      input: String,
      environment: Environment = Environment.empty
  ): String = {
    serialize(
      evaluate(
        fastparse.parse(input, Parser.expr(_)).get.value,
        environment,
        EndCont
      )
    )
  }

  private def serialize(v: ExpVal): String =
    v match {
      case IntegerValue(value)   => value.toString
      case BooleanValue(value)   => value.toString
      case ProcedureValue(value) => value.toString
    }

  private def toNum(exprValue: ExpVal): Integer =
    exprValue match {
      case IntegerValue(value) => value
      case _                   => throw new Exception(s"Expected a number: ${exprValue.toString}")
    }

  private def toBool(exprValue: ExpVal): Boolean =
    exprValue match {
      case BooleanValue(value) => value
      case _ =>
        throw new Exception(s"Expected a boolean: ${exprValue.toString}")
    }

  private def toProcedure(exprValue: ExpVal): Procedure =
    exprValue match {
      case ExpVal.ProcedureValue(value) => value
      case _                            => throw new Exception(s"Expected a proc: ${exprValue.toString}")
    }

  private def applyProc(
      proc: Procedure,
      value: ExpVal,
      cont: Continuation
  ): ExpVal =
    evaluate(
      proc.exp,
      Environment.extend(proc.name, value, proc.environment),
      cont
    )

}
