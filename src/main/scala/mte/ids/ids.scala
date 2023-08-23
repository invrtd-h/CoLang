package mte.ids

import mte.expr._

import scala.annotation.unused

/**
 * 프로그램 내부의 변수명
 */
private[mte] sealed trait VarID

private[mte] case class StringID(id: String) extends VarID {
  override def toString: String = id
}

private[mte] case object ThisKW extends VarID

private[mte] case object AnonFn1 extends VarID

private[mte] case class AnonArg(argIdx: Int) extends VarID

private[mte] case object FnCallOp extends VarID