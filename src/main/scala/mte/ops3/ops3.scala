package mte.ops3

import mte.*
import mte.expr.{Expr, TernaryOp}
import mte.value.{FOV, HMapV, NumV, VecV}

def makeTernaryIfExpr(cond: Expr, yes: Expr, no: Expr): Expr = {
  def ternaryIf(cond: => FOV, yes: => FOV, no: => FOV): Either[String, FOV] =
    if (cond.isTruthy) Right(yes) else Right(no)

  TernaryOp(cond, yes, no, ternaryIf, "if")
}

def makeUpdatedExpr(vecE: Expr, idxE: Expr, dataE: Expr): Expr = {
  def updated(vec: => FOV, idx: => FOV, data: => FOV): Either[String, FOV] = vec match {
    case VecV(vec) => idx match {
      case NumV(idx) =>
        if (idx < 0 || idx >= vec.length)
          Left(s"얘! 지금 idx=$idx 이게 길이 ${vec.length}짜리 한줄서기에 접근이 되겠니??")
        else
          Right(VecV(vec.updated(idx.toInt, data)))
      case _ => Left(s"얘! 지금 한줄서기 인덱스가 $idx 이게 숫자로 보이니??")
    }
    case HMapV(hMap) => Right(HMapV(hMap.updated(idx, data)))
    case _ => Left(s"얘! 지금 한줄서기 인덱스 접근 문법(mte=$vec, index=$idx)에서 $vec 이게 한줄서기로 보이냐??")
  }

  TernaryOp(vecE, idxE, dataE, updated, "updated")
}