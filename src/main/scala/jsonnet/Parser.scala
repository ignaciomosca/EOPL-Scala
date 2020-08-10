package jsonnet

import fastparse._
import MultiLineWhitespace._

object Parser {
  import fastparse._, MultiLineWhitespace._
  def expr[_: P]: P[Expr] = P( prefixExpr ~ plus.rep ).map{
    case (e, items) => items.foldLeft(e)(Expr.Plus)
  }
  def prefixExpr[_: P]: P[Expr] = P( callExpr ~ call.rep ).map{
    case (e, items) => items.foldLeft(e)(Expr.Call(_, _))
  }
  def callExpr[_: P] = P(bool | num | str | dict | local | func | ident )

  def num[_: P] = P( CharsWhileIn("0-9").! ).map(s => Expr.Num(s.toInt))
  def bool[_: P] = P( "true" | "false").!.map(s => Expr.Bool(s.toBoolean))
  def str[_: P] = P( str0 ).map(Expr.Str)
  def str0[_: P] = P( "\"" ~~/ CharsWhile(_ != '"', 0).! ~~ "\"" )
  def ident[_: P] = P( ident0 ).map(Expr.Ident)
  def ident0[_: P] = P( CharIn("a-zA-Z_") ~~ CharsWhileIn("a-zA-Z0-9_", 0) ).!

  def dict[_: P] = P( "{" ~/ (str0 ~ ":" ~/ expr).rep(0, ",") ~ "}" )
    .map(kvs => Expr.Dict(kvs.toMap))
  def local[_: P] = P( "local" ~/ ident0 ~ "=" ~ expr ~ ";" ~ expr )
    .map(Expr.Local.tupled)
  def func[_: P] = P( "function" ~/ "(" ~ ident0.rep(0, ",") ~ ")" ~ expr )
    .map(Expr.Func.tupled)

  def plus[_: P] = P( "+" ~ prefixExpr )
  def call[_: P] = P( "(" ~/ expr.rep(0, ",") ~ ")" )
}