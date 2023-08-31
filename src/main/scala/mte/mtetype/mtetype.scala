package mte.mtetype

import mte.ids._
import mte.error._

import scala.annotation.{targetName, unused}
import scala.util.{Try, Success, Failure}

private[mte] sealed trait TypeExpr

private[mte] case object NumTE extends TypeExpr {
  override def toString: String = "유리계수"
}

private[mte] case object StrTE extends TypeExpr {
  override def toString: String = "글"
}

private[mte] case class ArrowTE(args: Vector[TypeExpr], ret: TypeExpr) extends TypeExpr

private[mte] case class TupleTE(types: Vector[TypeExpr]) extends TypeExpr

private[mte] val unitTE = TupleTE(Vector())

private[mte] case class VecTE(t: TypeExpr) extends TypeExpr {
  override def toString: String = s"한줄서기[$t]"
}

private[mte] case class HMapTE(k: TypeExpr, v: TypeExpr) extends TypeExpr {
  override def toString: String = s"뭉탱이[$k -> $v]"
}

private[mte] case class ObjTE(members: Map[StringID, TypeExpr],
                              typeName: StringID) extends TypeExpr {
  override def toString: String = s"코괴물($typeName)"
}

private[mte] case class ForAllTE(args: Vector[StringID], retT: TypeExpr) extends TypeExpr

private[mte] case class IdTE(id: StringID) extends TypeExpr

private[mte] case class VarTE(t: Option[TypeExpr] = None) extends TypeExpr

@unused
private[mte] sealed trait Type {
  @targetName("w")
  def :>(rhs: Type): Boolean = isSubtype(this, rhs)

  @targetName("ww")
  def :>!(rhs: Type): Try[Unit] = Try {
    assert(this :> rhs, s"얘! 여기 지금 $rhs 이게 behave-as $this(으)로 보이니??")
    Success(())
  }

  @targetName("assertEq")
  def ==!(rhs: Type): Try[Unit] = Try {
    assert(this == rhs, s"얘! 여기 지금 $rhs 이게 equal-to $this(으)로 보이니??")
    Success(())
  }
}

private[mte] case object NumT extends Type

private[mte] case object StrT extends Type

private[mte] case class ArrowT(args: Vector[Type], ret: Type) extends Type

private[mte] case class TupleT(types: Vector[Type]) extends Type

private[mte] case class VecT(t: Type) extends Type

private[mte] case class HMapT(k: Type, v: Type) extends Type

private[mte] case class ObjT(members: Map[StringID, Type],
                             methods: Map[VarID, Type],
                             name: StringID) extends Type

def unitT: Type = TupleT(Vector())

private[mte] object ArrowT {
  @targetName("typeUnapply")
  def :<-(t: Type): Try[(Vector[Type], Type)] = trySubtract(t)

  def trySubtract(t: Type): Try[(Vector[Type], Type)] = t match {
    case ArrowT(args, ret) => Success((args, ret))
    case _ => Failure(MteTypeNotMatchExc(t, "함수"))
  }
}

private[mte] object VecT {
  @targetName("typeUnapply")
  def :<-(t: Type): Try[Type] = trySubtract(t)

  def trySubtract(t: Type): Try[Type] = t match {
    case VecT(v) => Success(v)
    case _ => Failure(MteTypeNotMatchExc(t, "한줄서기"))
  }
}

private[mte] object HMapT {
  @targetName("typeUnapply")
  def :<-(t: Type): Try[(Type, Type)] = trySubtract(t)

  def trySubtract(t: Type): Try[(Type, Type)] = t match {
    case HMapT(k, v) => Success((k, v))
    case _ => Failure(MteTypeNotMatchExc(t, "뭉탱이"))
  }
}

private def isSubtype(supertype: Type, subtype: Type): Boolean = supertype match {
  case NumT => subtype == NumT
  case StrT => subtype == StrT
  case ArrowT(args1, ret1) => subtype match {
    case ArrowT(args2, ret2) =>
      args2.zip(args1).map(isSubtype).reduce(_ && _) && isSubtype(ret1, ret2)
    case _ => false
  }
  case TupleT(types1) => subtype match {
    case TupleT(types2) =>
      types1.zip(types2).map(isSubtype).reduce(_ && _)
    case _ => false
  }
  case VecT(t1) => subtype match {
    case VecT(t2) => isSubtype(t1, t2)
    case _ => false
  }
  case HMapT(k1, v1) => subtype match {
    case HMapT(k2, v2) => k1 == k2 && isSubtype(v1, v2)
    case _ => false
  }
  case ObjT(members, methods, _) => members.map((k1, v1) => methods.get(k1) match {
    case Some(v2) => isSubtype(v1, v2)
    case None => false
  }).reduce(_ && _)
}



/**
 * 타입 환경
 * @param vars 변수 이름과 그 타입을 묶는 맵
 * @param typeIds 정의된 타입의 이름과 그 인스턴스를 묶는 맵
 */
private[mte] case class TEnv(vars: Map[VarID, Type], typeIds: Map[VarID, Type])

