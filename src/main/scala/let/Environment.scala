package let

import let.Values.ProcedureValue

trait Environment
case object Empty extends Environment
case class Bind(idType: String, valueType: Values, environment: Environment)
    extends Environment
case class RecBind(
    pname: String,
    bvar: String,
    pbody: Expression,
    environment: Environment
) extends Environment

object Environment {

  def empty: Environment = Empty

  def extend(
      element: String,
      value: Values,
      environment: Environment
  ): Environment = {
    Bind(element, value, environment)
  }

  def extendRec(
      pname: String,
      bvar: String,
      pbody: Expression,
      environment: Environment
  ): Environment = RecBind(pname, bvar, pbody, environment)

  def apply(env: Environment, variable: String, value: Values): Values =
    env match {
      case Empty => throw new Exception(s"$variable not found")
      case Bind(idType, valueType, environment) =>
        if (variable == idType) valueType
        else apply(environment, variable, valueType)
      case RecBind(pname, bvar, pbody, environment) =>
        if (variable == pname) {
          ProcedureValue(Procedure(bvar, pbody, env))
        } else apply(environment, variable, value)
    }

}
