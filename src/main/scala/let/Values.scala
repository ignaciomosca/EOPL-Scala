package let

import let.Environment.Environment

sealed trait Values

case class Procedure(name: String, exp: Expression, environment: Environment)

object Values {

  case class IntegerValue(value: Integer) extends Values

  case class BooleanValue(value: Boolean) extends Values

  case class ProcedureValue(value: Procedure) extends Values


}