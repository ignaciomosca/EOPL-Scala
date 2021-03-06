package let

import fastparse.MultiLineWhitespace._
import fastparse._

object Parser {

  def expr[_: P]: P[Expression] =
    P(const | diff | zero | ifExp | letRec | let | proc | call | varExpr)

  def const[_: P] = P(num).map(Expression.ConstExpr)

  def varExpr[_: P] = P(str0).map(Expression.VarExpr)

  def str0[_: P] = P(CharIn("a-zA-Z_") ~~ CharsWhileIn("a-zA-Z0-9_", 0)).!

  def num[_: P] = P(CharsWhileIn("0-9").!).map(s => ExpVal.IntegerValue(s.toInt))

  def zero[_: P] = P("zero?" ~/ "(" ~ expr ~ ")").map(Expression.ZeroExpr)

  def ifExp[_: P] = P("if" ~/ expr ~ "then" ~ expr ~ "else" ~ expr).map(Expression.CondExpr.tupled)

  def let[_: P] = P("let" ~/ str0 ~/ "=" ~/ expr ~/ "in" ~/ expr).map(Expression.LetExpr.tupled)

  def diff[_: P] = P("-" ~/ "(" ~ expr ~ "," ~ expr ~ ")").map(Expression.DiffExpr.tupled)

  def proc[_: P] = P("proc" ~/ "(" ~ str0 ~ ")" ~ expr).map(Expression.ProcExpr.tupled)

  def call[_: P] = P("(" ~/ expr ~/ expr ~/ ")").map(Expression.CallExpr.tupled)

  def letRec[_: P] = P("letrec" ~/ str0 ~ "(" ~/ str0 ~/  ")" ~/ "=" ~ expr ~/ "in" ~ expr).map(Expression.LetrecExpr.tupled)

}
