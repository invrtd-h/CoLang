package mte.mtetype

import mte.ids._
import mte.error._

import scala.annotation.{targetName, unused}
import scala.util.{Try, Success, Failure}

private[mte] sealed trait TypeInfo

private[mte] case object NumT extends TypeInfo {
  override def toString: String = "유리계수"
}

private[mte] case object StrT extends TypeInfo {
  override def toString: String = "글"
}

private[mte] case class ArrowT(args: Vector[TypeInfo], ret: TypeInfo) extends TypeInfo

private[mte] case class TupleT(types: Vector[TypeInfo]) extends TypeInfo

private[mte] val unitT = TupleT(Vector())

private[mte] case class VecT(t: TypeInfo) extends TypeInfo {
  override def toString: String = s"한줄서기[$t]"
}

private[mte] case class HMapT(k: TypeInfo, v: TypeInfo) extends TypeInfo {
  override def toString: String = s"뭉탱이[$k -> $v]"
}

private[mte] case class ObjT(members: Map[StringID, TypeInfo],
                             typeName: StringID) extends TypeInfo {
  override def toString: String = s"코괴물($typeName)"
}

private[mte] case class ForAllT(args: Vector[StringID], retT: TypeInfo) extends TypeInfo

private[mte] case class IdT(id: StringID) extends TypeInfo

private[mte] case class VarT(t: Option[TypeInfo] = None) extends TypeInfo

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

private[mte] case object NumTV extends Type

private[mte] case object StrTV extends Type

private[mte] case class ArrowTV(args: Vector[Type], ret: Type) extends Type

private[mte] case class TupleTV(types: Vector[Type]) extends Type

private[mte] case class VecTV(t: Type) extends Type

private[mte] case class HMapTV(k: Type, v: Type) extends Type

private[mte] case class ObjTV(members: Map[StringID, Type],
                              methods: Map[VarID, Type],
                              name: StringID) extends Type

def unitTV: Type = TupleTV(Vector())

private[mte] object ArrowTV {
  @targetName("typeUnapply")
  def :<-(t: Type): Try[(Vector[Type], Type)] = trySubtract(t)

  def trySubtract(t: Type): Try[(Vector[Type], Type)] = t match {
    case ArrowTV(args, ret) => Success((args, ret))
    case _ => Failure(MteTypeNonMatchExc(t, "함수"))
  }
}

private[mte] object VecTV {
  @targetName("typeUnapply")
  def :<-(t: Type): Try[Type] = trySubtract(t)
  
  def trySubtract(t: Type): Try[Type] = t match {
    case VecTV(v) => Success(v)
    case _ => Failure(MteTypeNonMatchExc(t, "한줄서기"))
  }
}

private[mte] object HMapTV {
  @targetName("typeUnapply")
  def :<-(t: Type): Try[(Type, Type)] = trySubtract(t)
  
  def trySubtract(t: Type): Try[(Type, Type)] = t match {
    case HMapTV(k, v) => Success((k, v))
    case _ => Failure(MteTypeNonMatchExc(t, "뭉탱이"))
  }
}

private def isSubtype(supertype: Type, subtype: Type): Boolean = supertype match {
  case NumTV => subtype == NumTV
  case StrTV => subtype == StrTV
  case ArrowTV(args1, ret1) => subtype match {
    case ArrowTV(args2, ret2) =>
      args2.zip(args1).map(isSubtype).reduce(_ && _) && isSubtype(ret1, ret2)
    case _ => false
  }
  case TupleTV(types1) => subtype match {
    case TupleTV(types2) =>
      types1.zip(types2).map(isSubtype).reduce(_ && _)
    case _ => false
  }
  case VecTV(t1) => subtype match {
    case VecTV(t2) => isSubtype(t1, t2)
    case _ => false
  }
  case HMapTV(k1, v1) => subtype match {
    case HMapTV(k2, v2) => k1 == k2 && isSubtype(v1, v2)
    case _ => false
  }
  case ObjTV(members, methods, _) => members.map((k1, v1) => methods.get(k1) match {
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

