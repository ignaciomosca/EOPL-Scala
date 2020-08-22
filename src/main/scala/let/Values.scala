package let

sealed trait Values

case class Procedure(name: String, exp: Expression, environment: Environment)
case class MutPair(lRef: Values, rRef: Values)

object Values {

  case class IntegerValue(value: Integer) extends Values

  case class BooleanValue(value: Boolean) extends Values

  case class ProcedureValue(value: Procedure) extends Values

  case class PairValue(value: MutPair) extends Values

  case class RefValue(ref: let.Ref) extends Values

}