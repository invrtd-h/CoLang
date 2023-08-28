package mte.sugar

import mte.expr._
import mte.mtetype._
import mte.ids.{AnonFn1, StringID}
import mte.ops.{makeGtExpr, makeAddExpr}

import scala.annotation.tailrec

def exprToFn(expr: Expr): Expr = Fun(AnonFn1, Vector(), Vector(), VarT(), expr)

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

def newFor(iterName: String, iterT: TypeInfo, initExpr: Expr, condExpr: Expr, manipulationExpr: Expr, inExpr: Expr): Expr = {
  BoxDef(StringID(iterName), iterT, initExpr, WhileN0(condExpr, Seqn(
    inExpr,
    manipulationExpr
  )))
}

def newSimpleFor(iterName: String, lbdInclusive: Expr, ubdExclusive: Expr, inExpr: Expr): Expr = newFor(
  iterName = iterName,
  iterT = NumT,
  initExpr = lbdInclusive,
  condExpr = makeGtExpr(ubdExclusive, Id(StringID(iterName))),
  manipulationExpr = BoxSet(Id(StringID(iterName)), makeAddExpr(Id(StringID(iterName)), Num(1))),
  inExpr = inExpr
)
