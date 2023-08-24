package mte.value

import mte.error._
import mte.expr.{Expr, App, Id}
import mte.ids.{VarID, StringID, ThisKW}
import mte.pret.pret
import mte.utility.strcut._

private[mte] type Env = Map[VarID, Value]

private[mte] sealed trait Value {
  def isTruthy: Boolean = this match {
    case NumV(data) => data != 0
    case BoxV(_, _) => throw MteUnexpectedErr(
      "얘! CoLang 개발자가 구현 과정에서 unboxing 를! 생략한 것 같단다! 개발자에게 문의하렴"
    )
    case _ => throw MteRuntimeErr(
      s"얘! 지금 네 눈에 $this 이게 참/거짓을 따질 수 있는 값으로 보이니??"
    )
  }

  def toFOV: FOV = this match {
    case fov: FOV => fov
    case nfov: NFOV => nfov.decay
  }
}

private[mte] sealed trait FOV extends Value

private[mte] sealed trait NFOV extends Value {
  def decay: FOV
}

private[mte] case class NumV(data: BigInt) extends FOV {
  override def toString: String = "%s".format(data)
}

private[mte] case class StrV(data: String) extends FOV {
  override def toString: String = data
}

private[mte] case class CloV(argName: Vector[VarID], fExpr: Expr, var fEnv: Env) extends FOV {
  override def toString: String = s"함수(인자:[${argName.cut}])"

  def call(arg: Vector[Value]): Value = {
    if (argName.length != arg.length) {
      throw MteSyntaxErr(
        s"얘! 지금 돈 ${arg.length} 원에 함수 ${this}를 불러달라고 한 거니?? 단가가 안 맞잖아 임마!!"
      )
    }
    pret(fExpr, fEnv ++ argName.zip(arg).toMap)
  }
}

private[mte] case class TupleV(data: Vector[Value]) extends FOV

private[mte] val unitV: Value = TupleV(Vector())

private[mte] case class VecV(data: Vector[Value]) extends FOV {
  override def toString: String = s"한줄서기(${data.cut})"
}

private[mte] case class HMapV(data: Map[Value, Value]) extends FOV {
  override def toString: String = data.toString()
}

private[mte] case class ClassV(memberNames: Vector[StringID], 
                               methods: Map[VarID, Value], 
                               typeName: StringID, 
                               var cEnv: Env) extends NFOV {
  def makeMethodOf(obj: ObjV, methodName: VarID): CloV = methods.get(methodName) match {
    case Some(value) => value match {
      case fn@CloV(_, _, _) =>
        fn.fEnv ++= Map(ThisKW -> obj, typeName -> this)
        fn.fEnv ++= obj.data
        fn
      case _ => throw MteRuntimeErr(
        s"얘! 메소드 정의($value)가 틀려먹었단다! 고통스럽게 죽도록 해요~"
      )
    }
    case None => throw MteRuntimeErr(
      s"얘! 지금 코괴물 ${typeName}의 코객체 ${obj}가 ${methodName}를 알겠니??"
    )
  }

  def construct(memberValues: Vector[Value]): ObjV = if (memberNames.length == memberValues.length) {
    ObjV(memberNames.zip(memberValues).toMap, supertype = this)
  } else throw MteRuntimeErr(
    s"얘! 지금 멤버 ${memberNames.length}개짜리 코객체 만드는 데 값을 ${memberValues.length}개 줘서 되겠니??"
  )

  override def decay: FOV = CloV(memberNames, App(Id(typeName), memberNames.map(x => Id(x))), cEnv)

  override def toString: String = s"코괴물(${typeName.id})"
}

private[mte] case class ObjV(data: Map[StringID, Value], 
                             supertype: ClassV) extends FOV {
  override def toString: String =
    s"${supertype.typeName.id}(${data.cut})"
}

/**
 * @param data 데이터의 변경 과정을 기록해놓음
 * @param addr 현재 박스가 데이터의 총 변경 과정 중 몇 번째 원소에 해당하는 값을 갖고 있는지
 */
private[mte] case class BoxV(var data: Vector[Value], var addr: Int) extends NFOV {
  def get: Value = data(addr)

  def set(newData: Value): Unit = {
    addr = data.length
    data = data.appended(newData)
  }

  override def decay: FOV = this.get match {
    case fov: FOV => fov
    case nfov: NFOV => nfov.decay
  }

  override def toString: String = s"BoxV($get)"
}

private[mte] def makeNewBox(value: Value): BoxV = BoxV(Vector(value), 0)