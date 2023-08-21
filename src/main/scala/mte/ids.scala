package mte

import scala.annotation.unused

@unused
def 함수호출: VarID = FnCallOp

@unused
val 나: Expr = Id(ThisKW)

/**
 * 프로그램 내부의 변수명
 */
sealed trait VarID

private case class StringID(id: String) extends VarID {
  override def toString: String = id
}

private case object ThisKW extends VarID

private case object AnonFn1 extends VarID

private case class AnonArg(argIdx: Int) extends VarID

private case object FnCallOp extends VarID

case class VarIDImplicit(id: VarID)

implicit class VarIDImplicitFromVarID(id: VarID) extends VarIDImplicit(id)

implicit class VarIDImplicitFromStr(id: String) extends VarIDImplicit(StringID(id))