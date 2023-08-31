package mte.ops

import scala.annotation.{tailrec, targetName, unused}
import scala.util.Right
import scala.util.{Try, Success, Failure}

import mte.expr.{BinaryOp, Expr, unitE}
import mte.mtetype.{ArrowT, HMapT, NumT, Type, VecT, unitT}
import mte.value.{CloV, FOV, HMapV, NumV, Value, VecV, unitV}
import mte.utility.{gtInt, geInt, logNot, AssertLen}
import mte.error.{mteAssert, MteAssertionFailedException, MteTypeNotMatchExc, MteArgNumIncorrectExc}

type OpFn = (=> FOV, => FOV) => Either[String, FOV]
type TypeOpFn = (Type, Type) => Try[Type]

private[mte] case class Op(op: OpFn, typeOp: TypeOpFn, name: String) {
  def calculate(lhs: => FOV, rhs: => FOV): Either[String, FOV] =
    op(lhs, rhs)

  def calculateType(lhs: Type, rhs: Type): Try[Type] =
    typeOp(lhs, rhs)

  override def toString: String = s"Op<$name>"
}

private def liftBinaryOp(op: (BigInt, BigInt) => BigInt): OpFn = {
  def ret(lhs: => FOV, rhs: => FOV): Either[String, FOV] = {
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
  def ret(lhs: => FOV, @unused rhs: => FOV): Either[String, FOV] = lhs match {
    case NumV(data) => Right(NumV(op(data)))
    case _ => Left(s"얘! 여기 지금 $lhs 이게 숫자로 보이니??")
  }

  ret
}

private def binaryTypeOp: TypeOpFn = {
  def ret(t: Type, u: Type): Try[Type] = for {
    _ <- NumT :>! t
    _ <- NumT :>! u
  } yield NumT

  ret
}

private[mte] def makeAddExpr(lhs: Expr, rhs: Expr): Expr =
  BinaryOp(lhs, rhs, Op(liftBinaryOp(_ + _), binaryTypeOp, "+"))
private[mte] def makeSubExpr(lhs: Expr, rhs: Expr): Expr =
  BinaryOp(lhs, rhs, Op(liftBinaryOp(_ - _), binaryTypeOp, "-"))
private[mte] def makeMulExpr(lhs: Expr, rhs: Expr): Expr =
  BinaryOp(lhs, rhs, Op(liftBinaryOp(_ * _), binaryTypeOp, "*"))
private[mte] def makeDivExpr(lhs: Expr, rhs: Expr): Expr =
  BinaryOp(lhs, rhs, Op(liftBinaryOp(_ / _), binaryTypeOp, "/"))
private[mte] def makeGtExpr(lhs: Expr, rhs: Expr): Expr =
  BinaryOp(lhs, rhs, Op(liftBinaryOp(gtInt), binaryTypeOp, ">"))
private[mte] def makeGeExpr(lhs: Expr, rhs: Expr): Expr =
  BinaryOp(lhs, rhs, Op(liftBinaryOp(geInt), binaryTypeOp, ">="))
private[mte] def makeLogNotExpr(lhs: Expr): Expr =
  BinaryOp(lhs, unitE, Op(liftUnaryOp(logNot), binaryTypeOp, "!"))
private[mte] def makeRemainderExpr(lhs: Expr, rhs: Expr): Expr =
  BinaryOp(lhs, rhs, Op(liftBinaryOp(_ % _), binaryTypeOp, "%"))

private[mte] def makeSqrtExpr(lhs: Expr): Expr = {
  def sqrt(value: => FOV, @unused x: => FOV): Either[String, FOV] = value match {
    case NumV(data) => Right(NumV(math.sqrt(data.doubleValue).floor.toLong))
    case _ => Left(s"얘! 지금 $value 이게 숫자로 보이니??")
  }

  def sqrtT(t: Type, @unused x: Type): Try[Type] =
    for {_ <- NumT :>! t} yield NumT

  BinaryOp(lhs, unitE, Op(sqrt, sqrtT, "sqrt"))
}

private[mte] def makePrintExpr(x: Expr, template: String): Expr = {
  def ret(x: FOV, @unused y: FOV): Either[String, FOV] = {
    print(template.format(x))
    Right(x)
  }

  def retT(t: Type, @unused y: Type): Try[Type] = Success(t)

  BinaryOp(x, unitE, Op(ret, retT, "print"))
}

private[mte] def makeReadIntExpr: Expr = {
  def ret(@unused x: FOV, @unused y: FOV): Either[String, FOV] = Right(NumV(scala.io.StdIn.readInt))

  def retT(@unused x: Type, @unused y: Type): Try[Type] = Success(NumT)

  BinaryOp(unitE, unitE, Op(ret, retT, "readInt"))
}

private[mte] def makeAssertExpr(value: Expr): Expr = {
  def myAssert(value: => FOV, @unused y: => FOV): Either[String, FOV] = value match {
    case NumV(data) =>
      if (data != 0)
        Right(unitV)
      else
        throw MteAssertionFailedException(s"얘! 지금 네 눈에 $value 이게 truthy한 값이 되겠니??")
    case _ => Left(s"얘! 지금 네 눈에 $value 이게 true/false가 되는 타입이 되겠니??")
  }

  def myAssertT(t: Type, @unused u: Type): Try[Type] =
    for (_ <- NumT :>! t) yield unitT

  BinaryOp(value, unitE, Op(myAssert, myAssertT, "assert"))
}

private[mte] def makeAccessExpr(lhs: Expr, rhs: Expr): Expr = {
  def access(container: => FOV, idx: => FOV): Either[String, FOV] = container match {
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

  def accessT1(vecT: Type, idxT: Type): Try[Type] = for {
    v <- VecT.trySubtract(vecT)
    _ <- NumT :>! idxT
  } yield v

  def accessT2(hMapT: Type, keyT: Type): Try[Type] = for {
    (k, v) <- HMapT.trySubtract(hMapT)
    _ <- k ==! keyT
  } yield v

  def accessT(t: Type, u: Type): Try[Type] = accessT1(t, u) match {
    case Failure(_) => accessT2(t, u) match {
      case Failure(_) => Failure(MteTypeNotMatchExc(t, "컨테이너"))
      case Success(value) => Success(value)
    }
    case Success(value) => Success(value)
  }

  BinaryOp(lhs, rhs, Op(access, accessT, "access"))
}

private[mte] def makeVecFillExpr(sizeE: Expr, initE: Expr): Expr = {
  def vecFill(size: => FOV, init: => FOV): Either[String, FOV] = size match {
    case NumV(data) => Right(VecV(Vector.fill(data.toInt) {
      init
    }))
    case _ => Left(s"얘! 지금 나보고 ${size}개의 원소를 가진 벡터를 만들어 달라고 하면 어쩌자는 거니!")
  }

  def vecFillT(t: Type, u: Type): Try[Type] =
    for (_ <- NumT :>! t) yield VecT(u)

  BinaryOp(sizeE, initE, Op(vecFill, vecFillT, "vecFill"))
}

private[mte] def makeVecIotaExpr(lbdInclusive: Expr, ubdExclusive: Expr): Expr = {
  def vecIota(lbdInclusive: => FOV, ubdExclusive: => FOV): Either[String, FOV] = {
    lbdInclusive match {
      case NumV(l) => ubdExclusive match {
        case NumV(u) => Right(VecV((l.toInt until u.toInt).map(value => NumV(value)).toVector))
        case _ => Left(s"얘! 지금 $ubdExclusive 이게 숫자로 보이니??")
      }
      case _ => Left(s"얘! 지금 $lbdInclusive 이게 숫자로 보이니??")
    }
  }

  def vecIotaT(t: Type, u: Type): Try[Type] =
    for {_ <- NumT :>! t; _ <- NumT :>! u} yield VecT(NumT)

  BinaryOp(lbdInclusive, ubdExclusive, Op(vecIota, vecIotaT, "vecIota"))
}

private[mte] def makeExtExpr(lhs: Expr, rhs: Expr): Expr = {
  def ext(lhs: => FOV, rhs: => FOV): Either[String, FOV] = lhs match {
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

  def extT(vecT1: Type, vecT2: Type): Try[Type] = for {
    t1 <- VecT.trySubtract(vecT1)
    t2 <- VecT.trySubtract(vecT2)
    _ <- t1 :>! t2
  } yield vecT1

  BinaryOp(lhs, rhs, Op(ext, extT, "ext"))
}

def makeSizeExpr(lhs: Expr, rhs: Expr): Expr = {
  def size(vec: => FOV, @unused x: => FOV): Either[String, FOV] = vec match {
    case VecV(data) => Right(NumV(data.length))
    case HMapV(data) => Right(NumV(data.size))
    case _ => Left(s"얘! 지금 $vec 이게 컨테이너로 보이냐??")
  }

  def sizeT(vecT: Type, numT: Type): Try[Type] = for {
    _ <- VecT.trySubtract(vecT)
    _ <- NumT :>! numT
  } yield NumT

  BinaryOp(lhs, rhs, Op(size, sizeT, "size"))
}

def makeVecDropRightExpr(vecE: Expr, dropNumE: Expr): Expr = {
  def vecDropRight(vec: => FOV, dropNum: => FOV): Either[String, FOV] = vec match {
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

  def vecDropRightT(vecT: Type, numT: Type): Try[Type] = for {
    _ <- VecT.trySubtract(vecT)
    _ <- NumT :>! numT
  } yield vecT

  BinaryOp(vecE, dropNumE, Op(vecDropRight, vecDropRightT, "vecDropRight"))
}

private def vecFilterT(vecT: Type, fnT: Type): Try[Type] = for {
  t <- VecT.trySubtract(vecT)
  (argTs, retT) <- ArrowT.trySubtract(fnT)
  _ <- mteAssert(argTs.length == 1, MteArgNumIncorrectExc(fnT, 1))
  _ <- argTs(0) :>! t
  _ <- NumT :>! retT
} yield vecT

def makeVecFilterExpr(vec: Expr, fn: Expr): Expr = {
  def vecFilter(vec: => FOV, fn: => FOV): Either[String, FOV] = vec match {
    case VecV(vec) => fn match {
      case fnV@CloV(_, _, _) => Right(VecV(vec.filter(value => fnV.call(Vector(value)).isTruthy)))
      case _ => Left(s"얘! 지금 $fn 이게 함수로 보이니??")
    }
    case _ => Left(s"얘! 지금 $vec 이게 벡터로 보이니??")
  }

  BinaryOp(vec, fn, Op(vecFilter, vecFilterT, "vecFilter"))
}

def makeVecRejectExpr(vec: Expr, fn: Expr): Expr = {
  def vecReject(vec: => FOV, fn: => FOV): Either[String, FOV] = vec match {
    case VecV(vec) => fn match {
      case fnV@CloV(_, _, _) => Right(VecV(vec.filterNot(value => fnV.call(Vector(value)).isTruthy)))
      case _ => Left(s"얘! 지금 $fn 이게 함수로 보이니??")
    }
    case _ => Left(s"얘! 지금 $vec 이게 벡터로 보이니??")
  }

  BinaryOp(vec, fn, Op(vecReject, vecFilterT, "vecReject"))
}

def makeVecMapExpr(vec: Expr, fn: Expr): Expr = {
  def vecMap(vec: => FOV, fn: => FOV): Either[String, FOV] = vec match {
    case VecV(vec) => fn match {
      case fnV@CloV(_, _, _) => Right(VecV(vec.map(x => fnV.call(Vector(x)).toFOV)))
      case _ => Left(s"얘! 지금 $fn 이게 함수로 보이니??")
    }
    case _ => Left(s"얘! 지금 $vec 이게 벡터로 보이니??")
  }

  def vecMapT(vecT: Type, fnT: Type): Try[Type] = for {
    t <- VecT.trySubtract(vecT)
    (argTs, retT) <- ArrowT.trySubtract(fnT)
    _ <- mteAssert(argTs.length == 1, MteArgNumIncorrectExc(fnT, 1))
    _ <- argTs(0) :>! t
  } yield VecT(retT)

  BinaryOp(vec, fn, Op(vecMap, vecMapT, "vecMap"))
}

def makeHMapContainsExpr(hMap: Expr, v: Expr): Expr = {
  def hMapContains(hMap: => FOV, value: => FOV): Either[String, FOV] = hMap match {
    case HMapV(hMap) => Right(NumV(if (hMap.contains(value)) 1 else 0))
    case _ => Left(s"얘! 지금 $hMap 이게 뭉탱이로 보이니??")
  }

  def hMapContainsT(hMapT: Type, keyT: Type): Try[Type] = for {
    (k, _) <- HMapT :<- hMapT
    _ <- k ==! keyT
  } yield NumT

  BinaryOp(hMap, v, Op(hMapContains, hMapContainsT, "hMapContains"))
}


private def joinTryFns(fns: Vector[TypeOpFn], exception: Throwable): TypeOpFn = {
  @tailrec
  def joinTryFnsHelp(t: Type, u: Type, fns: Vector[TypeOpFn]): Try[Type] = Try {
    fns.head(t, u)
  } match {
    case Failure(exc) => if (fns.tail.isEmpty) Failure(exc) else joinTryFnsHelp(t, u, fns.tail)
    case Success(value) => value
  }

  def ret(t: Type, u: Type): Try[Type] = joinTryFnsHelp(t, u, fns)

  ret
}