package let

sealed trait Values

object Values {

  case class IntegerValue(value: Integer) extends Values

  case class BooleanValue(value: Boolean) extends Values

}