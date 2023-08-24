package mte.mtetype

import mte.ids.StringID
import mte.error._

import scala.annotation.{targetName, unused}
import scala.util.{Try, Success, Failure}

private[mte] sealed trait Type {
  @targetName("w")
  def :>(rhs: Type): Boolean = isSubtypeOf(this, rhs)

  @targetName("ww")
  def :>!(rhs: Type): Try[Unit] =
    assertSubtype(this, rhs)
}

private[mte] case object UnitT extends Type

private[mte] case object NumT extends Type {
  override def toString: String = "유리계수"
}

private[mte] case object StrT extends Type {
  override def toString: String = "글"
}

private[mte] case class ArrowT(args: Vector[Type], ret: Type) extends Type

private[mte] case class VecT(t: Type) extends Type {
  override def toString: String = s"한줄서기[$t]"
}

private[mte] case class HMapT(k: Type, v: Type) extends Type {
  override def toString: String = s"뭉탱이[$k -> $v]"
}

private[mte] case class ObjT(members: Map[StringID, Type],
                             typeName: String) extends Type {
  override def toString: String = s"코괴물($typeName)"
}

private[mte] case class IdT(id: String) extends Type

private[mte] case class VarT(t: Option[Type] = None) extends Type

private def isSubtypeOf(supertype: Type, subtype: Type): Boolean = {
  def unwind(t: Type): Type = t match {
    case VarT(t) => t match {
      case Some(t) => t
      case None => throw MteTypeUnsolvedException("")
    }
    case _ => t
  }

  val superT = unwind(supertype)
  val subT = unwind(subtype)

  superT match {
    case UnitT => ???
    case NumT => subT match {
      case NumT => true
      case _ => false
    }
    case ArrowT(a, t) => subT match {
      case ArrowT(b, u) =>
        a.zip(b).map((l, r) => isSubtypeOf(r, l)).reduce(_ && _) && isSubtypeOf(t, u)
      case _ => false
    }
    case VecT(t) => subT match {
      case VecT(u) => isSubtypeOf(t, u)
      case _ => false
    }
    case HMapT(kl, vl) => subT match {
      case HMapT(kr, vr) => kl == kr && isSubtypeOf(vl, vr)
      case _ => false
    }
    case IdT(id) => ???
    case _ => throw MteUnexpectedErr("")
  }
}


/**
 * 타입 환경
 * @param vars 변수 이름과 그 타입을 묶는 맵
 * @param typeIds 정의된 타입의 이름과 그 인스턴스를 묶는 맵
 */
private[mte] case class TEnv(vars: Map[String, Type], typeIds: Map[String, Type])

private[mte] def assertSubtype(supertype: Type, subtype: Type): Try[Unit] = {
  assert(supertype :> subtype, s"얘! 여기 지금 $subtype 이게 $supertype(으)로 보이니??")
  Success(())
}