package mte.sugar

import scala.annotation.{tailrec, targetName, unused}
import mte._

def exprToFn(expr: Expr): Expr = Fun(AnonFn1, Vector(), expr)

def newScope(expr: Expr): Expr = App(exprToFn(expr), Vector())

def vecToSeq(vec: Vector[Expr]): Expr = {
  @tailrec
  def help(vec: Vector[Expr], ret: Expr): Expr = {
    if (vec.nonEmpty) {
      help(vec.tail, Seqn(ret, vec.head))
    } else {
      ret
    }
  }

  if (vec.length == 1) {
    vec.head
  } else {
    help(vec.tail, vec.head)
  }
}

def newFor(iterName: String, initExpr: Expr, condExpr: Expr, manipulationExpr: Expr, inExpr: Expr): Expr = {
  BoxDef(StringID(iterName), initExpr, WhileN0(condExpr, Seqn(
    inExpr,
    manipulationExpr
  )))
}

def newSimpleFor(iterName: String, lbdInclusive: Expr, ubdExclusive: Expr, inExpr: Expr): Expr = newFor(
  iterName = iterName,
  initExpr = lbdInclusive,
  condExpr = ops.makeGtExpr(ubdExclusive, Id(StringID(iterName))),
  manipulationExpr = SetBox(Id(StringID(iterName)), ops.makeAddExpr(Id(StringID(iterName)), Num(1))),
  inExpr = inExpr
)
