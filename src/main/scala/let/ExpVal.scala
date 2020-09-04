package let

case class Procedure(name: String, exp: Expression, environment: Environment)

sealed trait ExpVal
object ExpVal {
  case class IntegerValue(value: Integer) extends ExpVal
  case class BooleanValue(value: Boolean) extends ExpVal
  case class ProcedureValue(value: Procedure) extends ExpVal
}
