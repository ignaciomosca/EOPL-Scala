package let

import fastparse.MultiLineWhitespace._
import fastparse._


object Parser {

  def expr[_: P]: P[Expression] = P(prefixExpr ~ callExpr.rep ).map{
    case (e, items) => items.foldLeft(e)(Expression.Plus)
  }
  def prefixExpr[_: P]: P[Expression] = P( callExpr ~ call.rep ).map{
    case (e, items) => items.foldLeft(e)(Expression.Call(_, _))
  }

  def callExpr[_: P]= P(str | let | list | binariyOperators)


  def str[_: P] = P( str0 ).map(Expression.VarExpr)

  def str0[_: P] = P( "\"" ~~/ CharsWhile(_ != '"', 0).! ~~ "\"" )

  def list[_: P] = P( "[" ~/ (str0).rep(0, ",") ~ "]" ).map {
    list => Expression.ListExpr(list.map(Expression.VarExpr).toList)
  }

  // Let

  def let[_: P] = P( "let" ~/ str0 ~ "=" ~ expr ~ ";" ~ expr )
    .map(Expression.LetExpr.tupled)

  // Call

  def call[_: P] = P( "(" ~/ expr.rep(0, ",") ~ ")" )

  // Binary operators

  def binariyOperators[_:P] = P(gt | lt | eq | add | substract | multiply | divide)

  def gt[_: P] = P(prefixExpr ~ ">" ~ prefixExpr ).map {
    case (e1, e2) => Expression.BinOpExpr(BinOp.Gt, e1,e2)
  }

  def lt[_: P] = P(prefixExpr ~ "<" ~ prefixExpr ).map {
    case (e1, e2) => Expression.BinOpExpr(BinOp.Lt, e1,e2)
  }

  def eq[_: P] = P(prefixExpr ~ "==" ~ prefixExpr ).map {
    case (e1, e2) => Expression.BinOpExpr(BinOp.Eq, e1,e2)
  }

  def add[_: P] = P(prefixExpr ~ "+" ~ prefixExpr ).map {
    case (e1, e2) => Expression.BinOpExpr(BinOp.Add, e1,e2)
  }

  def substract[_: P] = P(prefixExpr ~ "-" ~ prefixExpr ).map {
    case (e1, e2) => Expression.BinOpExpr(BinOp.Sub, e1,e2)
  }

  def multiply[_: P] = P(prefixExpr ~ "*" ~ prefixExpr ).map {
    case (e1, e2) => Expression.BinOpExpr(BinOp.Mul, e1,e2)
  }

  def divide[_: P] = P(prefixExpr ~ "/" ~ prefixExpr ).map {
    case (e1, e2) => Expression.BinOpExpr(BinOp.Div, e1,e2)
  }

}