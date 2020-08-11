package let

import fastparse.MultiLineWhitespace._
import fastparse._

object Parser {

  def expr[_: P]: P[Expression] = P(str | let | const | zero | ifExp | diff)

  def const[_: P] = P(num).map(Expression.ConstExpr)

  def str[_: P] = P( str0 ).map(Expression.VarExpr)

  def str0[_: P] = P( "\"" ~~/ CharsWhile(_ != '"', 0).! ~~ "\"" )

  def num[_: P] = P( CharsWhileIn("0-9").! ).map(s => Values.IntegerValue(s.toInt))

  def zero[_: P] = P("zero?" ~/ "(" ~ expr ~ ")").map(Expression.ZeroExpr)

  def ifExp[_: P] = P("if" ~/ expr ~ "then" ~ expr ~ "else" ~ expr).map(Expression.CondExpr.tupled)

  def let[_: P] = P( "let" ~/ str0 ~ "=" ~ expr ~ ";" ~ expr ).map(Expression.LetExpr.tupled)

  def diff[_: P] = P( "-" ~/ "(" ~ expr ~ "," ~ expr ~ ")" ).map(Expression.DiffExpr.tupled)

}