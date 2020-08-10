package let

import fastparse._
import let._

object Parser {

  val reservedWords: List[String] = List("let*" , "let" , "in" , "if" , "then" , "else" , "zero?" , "minus" , "equal?" , "greater?" , "less?" , "cons" , "car" , "cdr" , "emptyList" , "list" , "cond" , "end")

  val binOpsMap: Map[String, BinOp] = Map("+" -> BinOp.Add, "-" -> BinOp.Sub, "*" -> BinOp.Mul, "/" -> BinOp.Div,
    "equal?" -> BinOp.Eq, "greater?" -> BinOp.Gt, "less?" -> BinOp.Le, "cons" -> BinOp.Cons)

  val unaryOpsMap: Map[String, UnaryOp] = Map("car" -> UnaryOp.Car, "cdr" -> UnaryOp.Cdr, "minus" -> UnaryOp.Minus, "zero?" -> UnaryOp.IsZero)


}