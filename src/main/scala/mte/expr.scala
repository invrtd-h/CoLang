package mte

import scala.annotation.{targetName, unused}

import utility.mteName

private sealed trait Expr

@unused
private case class Num(data: BigInt) extends Expr {
  @unused
  @targetName("plusPlus")
  def ++(rhs: Num): Num = Num(data * 10 + rhs.data)

  override def toString: String = data.toString
}

private case class UnitE() extends Expr {
  override def toString: String = "unit"
}

private case class BinaryOp(lhs: Expr,
                            rhs: Expr,
                            name: String,
                            op: (=> Value, => Value) => Either[String, Value]) extends Expr {
  override def toString: String =
    s"<$name>($lhs, $rhs)"
}

private case class TernaryOp(x: Expr, y: Expr, z: Expr,
                             op: (=> Value, => Value, => Value) => Either[String, Value],
                             opName: String) extends Expr {
  override def toString: String =
    s"<$opName>($x, $y, $z)"
}

private case class Id(name: VarID) extends Expr {
  override def toString: String = s"\"$name\""
}

private object Id {
  def apply(id: String): Id = {
    Id(StringID(id))
  }
}

private case class ValDef(valName: StringID, initExpr: Expr, next: Expr) extends Expr {
  override def toString: String =
    s"ValDef(변수명:$valName, 초기치:$initExpr, 계속:$next)"
}

private case class Fun(funName: VarID, argName: Vector[VarID], fExpr: Expr) extends Expr

private case class App(fnExpr: Expr, argExpr: Vector[Expr]) extends Expr {
  override def toString: String = s"App($fnExpr, ${argExpr.toString.drop(7).dropRight(1)})"
}

private case class Seqn(lhs: Expr, rhs: Expr) extends Expr

private case class WhileN0(cond: Expr, exprIn: Expr) extends Expr

private case class Proj(obj: Expr, id: VarID) extends Expr

private case class BoxDef(id: StringID, initExpr: Expr, next: Expr) extends Expr

private case class SetBox(box: Expr, setExpr: Expr) extends Expr

private case class Vec(data: Vector[Expr]) extends Expr

private case class HMap(data: Map[Expr, Expr]) extends Expr

private case class ClassDef(memberName: Vector[StringID],
                            methods: Map[VarID, Expr],
                            typeName: StringID,
                            next: Expr) extends Expr {
  override def toString: String =
    s"ClassDef(이름:$typeName, 인자:[${mteName(memberName)}], 메서드:[${mteName(methods)}], 계속:$next)"
}

private case class BuiltinFnE2E(fn: Expr => Expr, arg: Expr, opName: String) extends Expr {
  override def toString: String = s"BF<$opName>($arg)"
}
