package let

import fastparse.MultiLineWhitespace._
import fastparse._

object Parser {

  def expr[_: P]: P[Expression] =
    P(const | diff | zero | pair | left | right | setleft | setright | begin | assign | ifExp | letRec | let | proc | call | varExpr)

  def const[_: P] = P(num).map(Expression.ConstExpr)

  def varExpr[_: P] = P(str0).map(Expression.VarExpr)

  def str0[_: P] = P(CharIn("a-zA-Z_") ~~ CharsWhileIn("a-zA-Z0-9_", 0)).!

  def num[_: P] = P(CharsWhileIn("0-9").!).map(s => Values.IntegerValue(s.toInt))

  def zero[_: P] = P("zero?" ~/ "(" ~ expr ~ ")").map(Expression.ZeroExpr)

  def ifExp[_: P] = P("if" ~/ expr ~ "then" ~ expr ~ "else" ~ expr).map(Expression.CondExpr.tupled)

  def let[_: P] = P("let" ~/ str0 ~/ "=" ~/ expr ~/ "in" ~/ expr).map(Expression.LetExpr.tupled)

  def diff[_: P] = P("-" ~/ "(" ~ expr ~ "," ~ expr ~ ")").map(Expression.DiffExpr.tupled)

  def proc[_: P] = P("proc" ~/ "(" ~ str0 ~ ")" ~/ expr).map(Expression.ProcExpr.tupled)

  def call[_: P] = P("(" ~/ expr ~/ expr ~/ ")").map(Expression.CallExpr.tupled)

  def letRec[_: P] = P("letrec" ~/ letRecAux.rep(1, ",") ~/ "in" ~ expr).map { case (value, expression) => Expression.LetrecExpr(value.toList, expression) }

  def letRecAux[_: P]:P[RecProc] = P(str0 ~/ "(" ~/ str0 ~/  ")" ~/ "=" ~ expr)

  def begin[_: P] = P("begin" ~/ expr.rep(1, ";") ~/ "end").map(list => Expression.BeginExpr(list.toList))

  def assign[_: P] = P("set" ~/ str0 ~/ "=" ~/ expr).map(Expression.AssignExpr.tupled)

  def pair[_: P] = P("pair" ~/ "(" ~/ expr ~/ "," ~/ expr ~/   ")").map(Expression.NewPairExpr.tupled)

  def right[_: P] = P("getright" ~/ "(" ~/ expr ~/ ")").map(Expression.GetRightPairExpr)

  def left[_: P] = P("getleft" ~/ "(" ~/ expr ~/ ")").map(Expression.GetLeftPairExpr)

  def setright[_: P] = P("setright" ~/ "(" ~/ expr ~/ "," ~/ expr ~/   ")").map(Expression.SetRightPairExpr.tupled)

  def setleft[_: P] = P("setleft" ~/ "(" ~/ expr ~/ "," ~/ expr ~/   ")").map(Expression.SetLeftPairExpr.tupled)

}
