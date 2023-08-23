package mte.ops

import scala.annotation.{tailrec, targetName, unused}
import scala.util.Right
import mte.{expr, *}
import mte.expr.{BinaryOp, Expr, unitE}
import mte.mtetype.{HMapT, NumT, Type, UnitT, VecT}
import mte.value.{CloV, HMapV, NumV, Value, VecV, unitV}

type OpFn = (=> Value, => Value) => Either[String, Value]
type TypeOpFn = (Type, Type) => Either[String, Type]

private[mte] case class Op(op: OpFn, typeOp: TypeOpFn, name: String) {
  def calculate(lhs: => Value, rhs: => Value): Either[String, Value] =
    op(lhs, rhs)

  def calculateType(lhs: Type, rhs: Type): Either[String, Type] =
    typeOp(lhs, rhs)

  override def toString: String = s"Op<$name>"
}

private def liftBinaryOp(op: (BigInt, BigInt) => BigInt): OpFn = {
  def ret(lhs: => Value, rhs: => Value): Either[String, Value] = {
    lhs match {
      case NumV(dataL) => rhs match {
        case NumV(dataR) => Right(NumV(op(dataL, dataR)))
        case _ => Left(s"얘! 여기 지금 $rhs 이게 숫자로 보이니??")
      }
      case _ => Left(s"얘! 여기 지금 $lhs 이게 숫자로 보이니??")
    }
  }

  ret
}

private def liftUnaryOp(op: BigInt => BigInt): OpFn = {
  def ret(lhs: => Value, @unused rhs: => Value): Either[String, Value] = lhs match {
    case NumV(data) => Right(NumV(op(data)))
    case _ => Left(s"얘! 여기 지금 $lhs 이게 숫자로 보이니??")
  }

  ret
}

private def binaryTypeOp: TypeOpFn = {
  def ret(t: Type, u: Type): Either[String, Type] = if (NumT :> t) {
    if (NumT :> u) Right(NumT) else typeNotMatch(NumT, u)
  } else typeNotMatch(NumT, t)

  ret
}

def makeAddExpr(lhs: Expr, rhs: Expr): Expr =
  BinaryOp(lhs, rhs, Op(liftBinaryOp(_ + _), binaryTypeOp, "+"))
def makeSubExpr(lhs: Expr, rhs: Expr): Expr =
  BinaryOp(lhs, rhs, Op(liftBinaryOp(_ - _), binaryTypeOp, "-"))
def makeMulExpr(lhs: Expr, rhs: Expr): Expr =
  BinaryOp(lhs, rhs, Op(liftBinaryOp(_ * _), binaryTypeOp, "*"))
def makeDivExpr(lhs: Expr, rhs: Expr): Expr =
  BinaryOp(lhs, rhs, Op(liftBinaryOp(_ / _), binaryTypeOp, "/"))
def makeGtExpr(lhs: Expr, rhs: Expr): Expr =
  BinaryOp(lhs, rhs, Op(liftBinaryOp(utility.gtInt), binaryTypeOp, ">"))
def makeGeExpr(lhs: Expr, rhs: Expr): Expr =
  BinaryOp(lhs, rhs, Op(liftBinaryOp(utility.geInt), binaryTypeOp, ">="))
def makeLogNotExpr(lhs: Expr): Expr =
  BinaryOp(lhs, unitE, Op(liftUnaryOp(utility.logNot), binaryTypeOp, "!"))
def makeRemainderExpr(lhs: Expr, rhs: Expr): Expr =
  BinaryOp(lhs, rhs, Op(liftBinaryOp(_ % _), binaryTypeOp, "%"))

private[mte] def makeSqrtExpr(lhs: Expr): Expr = {
  def sqrt(value: => Value, @unused x: => Value): Either[String, Value] = value match {
    case NumV(data) => Right(NumV(math.sqrt(data.doubleValue).floor.toLong))
    case _ => Left(s"얘! 지금 $value 이게 숫자로 보이니??")
  }

  def sqrtT(t: Type, @unused x: Type): Either[String, Type] =
    if (NumT :> t) Right(NumT) else typeNotMatch(NumT, t)

  BinaryOp(lhs, unitE, Op(sqrt, sqrtT, "sqrt"))
}

def makePrintExpr(x: Expr, template: String): Expr = {
  def ret(x: Value, @unused y: Value): Either[String, Value] = {
    print(template.format(x))
    Right(x)
  }

  def retT(t: Type, @unused y: Type): Either[String, Type] = Right(t)

  BinaryOp(x, unitE, Op(ret, retT, "print"))
}

def makeAssertExpr(value: Expr): Expr = {
  def myAssert(value: => Value, @unused y: => Value): Either[String, Value] = value match {
    case NumV(data) =>
      if (data != 0)
        Right(unitV)
      else
        throw error.MteAssertionFailedException(s"얘! 지금 네 눈에 $value 이게 truthy한 값이 되겠니??")
    case _ => Left(s"얘! 지금 네 눈에 $value 이게 true/false가 되는 타입이 되겠니??")
  }

  def myAssertT(t: Type, @unused u: Type): Either[String, Type] = t match {
    case NumT => Right(UnitT)
    case _ => Left(s"얘! 지금 네 눈에 $value 이게 true/false가 되는 타입이 되겠니??")
  }

  BinaryOp(value, unitE, Op(myAssert, myAssertT, "assert"))
}

def makeAccessExpr(lhs: Expr, rhs: Expr): Expr = {
  def access(container: => Value, idx: => Value): Either[String, Value] = container match {
    case VecV(data) => idx match {
      case NumV(n) =>
        if (n < 0 || n >= data.length)
          Left(s"얘! 지금 idx=$n 이게 길이 ${data.length}짜리 한줄서기에 접근이 되겠니??")
        else
          Right(data(n.toInt))
      case _ => Left(s"얘! 지금 한줄서기 인덱스가 $idx 이게 숫자로 보이니??")
    }
    case HMapV(data) => data.get(idx) match {
      case Some(value) => Right(value)
      case None => Left(s"얘! ${data}는 $idx 이런 거 몰라 임마!!")
    }
    case _ => Left(s"얘! 지금 인덱스 접근 문법(mte=$container, index=$idx)에서 $container 이게 컨테이너가 되겠니??")
  }

  def accessT(t: Type, u: Type): Either[String, Type] = t match {
    case VecT(t) => u match {
      case NumT => Right(t)
      case _ => Left(s"얘! 지금 한줄서기 인덱스가 $u 이게 유리계수 타입으로 보이니??")
    }
    case HMapT(k, v) => if (k :> u) Right(v) else Left(s"얘! 지금 ${u}가 ${k}인 걸로 보이니??")
    case _ => Left(s"얘! 지금 $t 이게 컨테이너가 되겠니??")
  }

  BinaryOp(lhs, rhs, Op(access, accessT, "access"))
}

def makeVecFillExpr(sizeE: Expr, initE: Expr): Expr = {
  def vecFill(size: => Value, init: => Value): Either[String, Value] = size match {
    case NumV(data) => Right(VecV(Vector.fill(data.toInt) {
      init
    }))
    case _ => Left(s"얘! 지금 나보고 ${size}개의 원소를 가진 벡터를 만들어 달라고 하면 어쩌자는 거니!")
  }

  def vecFillT(t: Type, u: Type): Either[String, Type] = if (NumT :> t) Right(VecT(u)) else typeNotMatch(NumT, t)

  BinaryOp(sizeE, initE, Op(vecFill, vecFillT, "vecFill"))
}

def makeVecIotaExpr(lbdInclusive: Expr, ubdExclusive: Expr): Expr = {
  def vecIota(lbdInclusive: => Value, ubdExclusive: => Value): Either[String, Value] = {
    lbdInclusive match {
      case NumV(l) => ubdExclusive match {
        case NumV(u) => Right(VecV((l.toInt until u.toInt).map(value => NumV(value)).toVector))
        case _ => Left(s"얘! 지금 $ubdExclusive 이게 숫자로 보이니??")
      }
      case _ => Left(s"얘! 지금 $lbdInclusive 이게 숫자로 보이니??")
    }
  }

  def vecIotaT(t: Type, u: Type): Either[String, Type] = ???

  BinaryOp(lbdInclusive, ubdExclusive, Op(vecIota, vecIotaT, "vecIota"))
}

def makeExtExpr(lhs: Expr, rhs: Expr): Expr = {
  def ext(lhs: => Value, rhs: => Value): Either[String, Value] = lhs match {
    case VecV(lData) => rhs match {
      case VecV(rData) => Right(VecV(lData ++ rData))
      case _ => Left(s"얘! 지금 한줄서기 연결 문법(lhs=$lhs, rhs=$rhs)에서 $rhs 이게 한줄서기로 보이냐??")
    }
    case HMapV(lData) => rhs match {
      case HMapV(rData) => Right(HMapV(lData ++ rData))
      case _ => Left(s"얘! 지금 뭉탱이 연결 문법(lhs=$lhs, rhs=$rhs)에서 $rhs 이게 뭉탱이로 보이냐??")
    }
    case _ => Left(s"얘! 지금 한줄서기 연결 문법(lhs=$lhs, rhs=$rhs)에서 $lhs 이게 컨테이너로 보이냐??")
  }

  def extT(t: Type, u: Type): Either[String, Type] = ???

  BinaryOp(lhs, rhs, Op(ext, extT, "ext"))
}

def makeSizeExpr(lhs: Expr, rhs: Expr): Expr = {
  def size(vec: => Value, @unused x: => Value): Either[String, Value] = vec match {
    case VecV(data) => Right(NumV(data.length))
    case HMapV(data) => Right(NumV(data.size))
    case _ => Left(s"얘! 지금 $vec 이게 컨테이너로 보이냐??")
  }

  def sizeT(t: Type, u: Type): Either[String, Type] = ???

  BinaryOp(lhs, rhs, Op(size, sizeT, "size"))
}

def makeVecDropRightExpr(vecE: Expr, dropNumE: Expr): Expr = {
  def vecDropRight(vec: => Value, dropNum: => Value): Either[String, Value] = vec match {
    case VecV(vec) => dropNum match {
      case NumV(num) =>
        if (vec.length < num) {
          Left(s"얘! 지금 원소 ${vec.length}짜리 한줄서기에서 ${num}개짜리 원소를 없애겠다는 게 말이 되니??")
        } else {
          Right(VecV(vec.dropRight(num.toInt)))
        }
      case _ => Left(s"얘! 지금 $dropNum 이게 숫자로 보이니??")
    }
    case _ => Left(s"얘! 지금 $vec 이게 한줄서기로 보이냐??")
  }

  def vecDropRightT(t: Type, u: Type): Either[String, Type] = ???

  BinaryOp(vecE, dropNumE, Op(vecDropRight, vecDropRightT, "vecDropRight"))
}

def makeVecFilterExpr(vec: Expr, fn: Expr): Expr = {
  def vecFilter(vec: => Value, fn: => Value): Either[String, Value] = vec match {
    case VecV(vec) => fn match {
      case fnV@CloV(_, _, _) => Right(VecV(vec.filter(value => fnV.call(Vector(value)).isTruthy)))
      case _ => Left(s"얘! 지금 $fn 이게 함수로 보이니??")
    }
    case _ => Left(s"얘! 지금 $vec 이게 벡터로 보이니??")
  }

  def vecFilterT(t: Type, u: Type): Either[String, Type] = ???

  BinaryOp(vec, fn, Op(vecFilter, vecFilterT, "vecFilter"))
}

def makeVecRejectExpr(vec: Expr, fn: Expr): Expr = {
  def vecReject(vec: => Value, fn: => Value): Either[String, Value] = vec match {
    case VecV(vec) => fn match {
      case fnV@CloV(_, _, _) => Right(VecV(vec.filterNot(value => fnV.call(Vector(value)).isTruthy)))
      case _ => Left(s"얘! 지금 $fn 이게 함수로 보이니??")
    }
    case _ => Left(s"얘! 지금 $vec 이게 벡터로 보이니??")
  }

  def vecRejectT(t: Type, u: Type): Either[String, Type] = ???

  BinaryOp(vec, fn, Op(vecReject, vecRejectT, "vecReject"))
}

def makeVecMapExpr(vec: Expr, fn: Expr): Expr = {
  def vecMap(vec: => Value, fn: => Value): Either[String, Value] = vec match {
    case VecV(vec) => fn match {
      case fnV@CloV(_, _, _) => Right(VecV(vec.map(x => fnV.call(Vector(x)))))
      case _ => Left(s"얘! 지금 $fn 이게 함수로 보이니??")
    }
    case _ => Left(s"얘! 지금 $vec 이게 벡터로 보이니??")
  }

  def vecMapT(t: Type, u: Type): Either[String, Type] = ???

  BinaryOp(vec, fn, Op(vecMap, vecMapT, "vecMap"))
}

def makeHMapContainsExpr(hMap: Expr, v: Expr): Expr = {
  def hMapContains(hMap: => Value, value: => Value): Either[String, Value] = hMap match {
    case HMapV(hMap) => Right(NumV(if (hMap.contains(value)) 1 else 0))
    case _ => Left(s"얘! 지금 $hMap 이게 뭉탱이로 보이니??")
  }

  def hMapContainsT(t: Type, u: Type): Either[String, Type] = ???

  BinaryOp(hMap, v, Op(hMapContains, hMapContainsT, "hMapContains"))
}

private def typeNotMatch(supertype: Type, subtype: Type): Either[String, Type] =
  Left(s"얘! 여기 지금 $subtype 이게 ${supertype}(으)로 보이니??")