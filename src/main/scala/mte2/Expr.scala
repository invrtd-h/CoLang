package mte2

import scala.util.matching.Regex
import scala.util.parsing.combinator.*

sealed trait Expr

case class IntE(value: BigInt) extends Expr

case class Add(left: Expr, right: Expr) extends Expr

case class Sub(left: Expr, right: Expr) extends Expr

case class ParsingError(msg: String) extends Exception

object Expr extends RegexParsers {
  def apply(str: String): Expr = parseAll(e, str).getOrElse(error(""))

  private def error(msg: String): Nothing = throw ParsingError(msg)

  override def skipWhitespace = true
  override val whiteSpace: Regex = "[ \n\t\r\f]+".r

  private def wrapR[T](e: Parser[T]): Parser[T] = "(" ~> e <~ ")"
  private def wrapC[T](e: Parser[T]): Parser[T] = "{" ~> e <~ "}"
  private def wrapS[T](e: Parser[T]): Parser[T] = "[" ~> e <~ "]"

  private lazy val n: Parser[BigInt] = "-?[0-9]+".r ^^ BigInt.apply

  private lazy val e: Parser[Expr] = {
    e1 ~ rep(("+" | "-") ~ e1) ^^ {
      case e1 ~ es => es.foldLeft(e1) {
        case (l, "+" ~ r) => Add(l, r)
        case (l, _ ~ r) => Sub(l, r)
      }
    }
  }

  private lazy val e1: Parser[Expr] = n ^^ IntE.apply
}

def pret(expr: Expr): BigInt = expr match {
  case IntE(value) => value
  case Add(left, right) => pret(left) + pret(right)
  case Sub(left, right) => pret(left) - pret(right)
}

@main
def main(): Unit = {
  println(pret(Expr("43")))
  println(pret(Expr("4+3")))
  println(pret(Expr("4 + 3")))
}