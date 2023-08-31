package mte.expr

import mte.ops.Op
import mte.value.{FOV, Value}
import mte.mtetype.TypeExpr
import mte.ids._
import mte.utility.strcut._

import scala.annotation.{targetName, unused}

private[mte] sealed trait Expr

@unused
private[mte] case class Num(data: BigInt) extends Expr {
  @unused
  @targetName("plusPlus")
  def ++(rhs: Num): Num = Num(data * 10 + rhs.data)

  override def toString: String = data.toString
}

private[mte] case class StrE(data: String) extends Expr

private[mte] case class BinaryOp(lhs: Expr, rhs: Expr, op: Op) extends Expr {
  override def toString: String =
    s"<${op.name}>($lhs, $rhs)"
}

private[mte] case class TernaryOp(x: Expr, y: Expr, z: Expr,
                             op: (=> FOV, => FOV, => FOV) => Either[String, FOV],
                             opName: String) extends Expr {
  override def toString: String =
    s"<$opName>($x, $y, $z)"
}

private[mte] case class Id(name: VarID) extends Expr {
  override def toString: String = s"\"$name\""
}

private[mte] object Id {
  def apply(id: String): Id = {
    Id(StringID(id))
  }
}

private[mte] case class ValDef(id: StringID, t: TypeExpr, initExpr: Expr, next: Expr) extends Expr {
  override def toString: String =
    s"ValDef(변수명:$id, 타입:$t, 초기치:$initExpr, 계속:$next)"
}

private[mte] case class Fun(funName: VarID,
                            argName: Vector[VarID],
                            argsT: Vector[TypeExpr],
                            retT: TypeExpr,
                            fExpr: Expr) extends Expr

private[mte] case class App(fnExpr: Expr, argExpr: Vector[Expr]) extends Expr {
  override def toString: String = s"App($fnExpr, ${argExpr.cut})"
}

private[mte] case class Tuple(data: Vector[Expr], types: Vector[TypeExpr]) extends Expr

private[mte] val unitE: Expr = Tuple(Vector(), Vector())

private[mte] case class Seqn(lhs: Expr, rhs: Expr) extends Expr

private[mte] case class WhileN0(cond: Expr, exprIn: Expr) extends Expr

private[mte] case class Proj(obj: Expr, id: VarID) extends Expr

private[mte] case class BoxDef(id: StringID, t: TypeExpr, initExpr: Expr, next: Expr) extends Expr

private[mte] case class BoxSet(box: Expr, setExpr: Expr) extends Expr

private[mte] case class Vec(data: Vector[Expr], t: TypeExpr) extends Expr

private[mte] case class HMap(data: Map[Expr, Expr], kT: TypeExpr, vT: TypeExpr) extends Expr

private[mte] case class ClassDef(memberName: Vector[StringID],
                            methods: Map[VarID, Expr],
                            typeName: StringID,
                            next: Expr) extends Expr {
  override def toString: String =
    s"ClassDef(이름:$typeName, 인자:[${memberName.cut}], 메서드:[${methods.cut}], 계속:$next)"
}

private[mte] sealed trait Cmd