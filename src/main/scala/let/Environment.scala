package let

import let.ExpVal.ProcedureValue

sealed trait Environment
case object Empty extends Environment
case class ExtendEnv(idType: String, valueType: ExpVal, environment: Environment) extends Environment
case class ExtendEnvRec(pname: String, bvar: String, pbody: Expression, environment: Environment) extends Environment

object Environment {

  def empty: Environment = Empty

  def extend(element: String, value: ExpVal, environment: Environment): Environment = {
    ExtendEnv(element, value, environment)
  }

  def extendRec(pname: String, bvar: String,
                pbody: Expression, environment: Environment): Environment = {
    ExtendEnvRec(pname, bvar, pbody, environment)
  }

  def apply(env: Environment, variable: String): ExpVal =
    env match {
      case Empty => throw new Exception(s"$variable not found")
      case ExtendEnv(var1, val1, environment) =>
        if (variable == var1) val1
        else apply(environment, variable)
      case ExtendEnvRec(fname, arg, body, env1) =>
        if (variable == fname) {
          ProcedureValue(Procedure(arg, body, env))
        } else apply(env1, variable)
    }

}
