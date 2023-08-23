package mte.mtetype

import mte.ids.StringID
import mte.error._

import scala.annotation.{targetName, unused}

private[mte] sealed trait Type {
  @targetName("w")
  def :>(rhs: Type): Boolean = isSubtypeOf(this, rhs)
}

private[mte] case object UnitT extends Type

private[mte] case object TopT extends Type

private[mte] case object BottomT extends Type

private[mte] case object NumT extends Type {
  override def toString: String = "유리계수"
}

private[mte] case class ArrowT(args: Vector[Type], ret: Type) extends Type

private[mte] case class VecT(t: Type) extends Type {
  override def toString: String = s"한줄서기($t)"
}

private[mte] case class HMapT(k: Type, v: Type) extends Type

private[mte] case class IdT(id: String) extends Type

private[mte] case class VarT(t: Option[Type] = None) extends Type

private def isSubtypeOf(supertype: Type, subtype: Type): Boolean = supertype match {
  case UnitT => ???
  case TopT => true
  case BottomT => subtype match {
    case BottomT => true
    case _ => false
  }
  case NumT => subtype match {
    case NumT => true
    case BottomT => true
    case _ => false
  }
  case ArrowT(a, t) => subtype match {
    case ArrowT(b, u) =>
      a.zip(b).map((l, r) => isSubtypeOf(r, l)).reduce(_ && _) && isSubtypeOf(t, u)
    case BottomT => true
    case _ => false
  }
  case VecT(t) => subtype match {
    case VecT(u) => isSubtypeOf(t, u)
    case BottomT => true
    case _ => false
  }
  case HMapT(kl, vl) => subtype match {
    case HMapT(kr, vr) => kl == kr && isSubtypeOf(vl, vr)
    case BottomT => true
    case _ => false
  }
  case IdT(id) => ???
  case VarT(_) => throw MteUnexpectedErr("")
}


/**
 * 타입 환경
 * @param vars 변수 이름과 그 타입을 묶는 맵
 * @param typeIds 정의된 타입의 이름과 그 인스턴스를 묶는 맵
 */
private[mte] case class TEnv(vars: Map[String, Type], typeIds: Map[String, Type])


/**
 * 프로그래머가 사용할 타입 노테이션을 정의한다
 */
private[mte] sealed trait TypeNotation {
  def getType: Type

  @unused
  @targetName("colonColon")
  def |:(id: String): (StringID, Type) = (StringID(id), getType)

  @unused
  @targetName("w")
  def >>:(argTypes: TypeNotation*): TypeNotation = ArrowTNotation(argTypes.toVector, this)
}

@unused
case object 스킵 extends TypeNotation {
  override def getType: Type = VarT(None)
}

@unused
case object 유리계수 extends TypeNotation {
  override def getType: Type = NumT
}

private case class ArrowTNotation(args: Vector[TypeNotation], ret: TypeNotation) extends TypeNotation {
  override def getType: Type = ArrowT(args.map(x => x.getType), ret.getType)
}