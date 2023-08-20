package mte {
  import scala.annotation.{tailrec, targetName, unused}
  import scala.language.implicitConversions
  import scala.util.Random
  import scala.util.chaining.*
  import utility.Piper

  sealed trait Expr

  sealed trait Value {
    def isTruthy: Boolean = this match {
      case NumV(data) => data != 0
      case BoxV(_, _) => throw error.MteUnexpectedErr(
        "얘! CoLang 개발자가 구현 과정에서 unboxing 를! 생략한 것 같단다! 개발자에게 문의하렴"
      )
      case _ => throw error.MteRuntimeErr(
        s"얘! 지금 네 눈에 $this 이게 참/거짓을 따질 수 있는 값으로 보이니??"
      )
    }

    def toFOV: FOV = this match {
      case fov: FOV => fov
      case nfov: NFOV => nfov.decay
    }
  }

  sealed trait FOV extends Value
  sealed trait NFOV extends Value {
    def decay: FOV
  }

  sealed trait Type
  case object NumT extends Type

  sealed trait VarID
  case class StringID(id: String) extends VarID {
    override def toString: String = id
  }
  private case object ThisKW extends VarID
  private case object AnonFn1 extends VarID
  private case class AnonArg(argIdx: Int) extends VarID

  case class VarIDImplicit(id: VarID)
  implicit class VarIDImplicitFromVarID(id: VarID) extends VarIDImplicit(id)
  implicit class VarIDImplicitFromStr(id: String) extends VarIDImplicit(StringID(id))

  type Env = Map[VarID, Value]
  type Addr = Int

  // Expressions
  case class Num(data: BigInt) extends Expr {
    @unused
    @targetName("plusPlus")
    def ++(rhs: Num): Num = Num(data * 10 + rhs.data)

    override def toString: String = data.toString
  }

  case class NewValue(value: FOV) extends Expr

  case class UnitE() extends Expr {
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

  private case class ValDef(valName: StringID, initExpr: Expr, next: Expr) extends Expr

  private case class Fun(funName: VarID, argName: Vector[VarID], fExpr: Expr) extends Expr

  private case class App(fnExpr: Expr, argExpr: Vector[Expr]) extends Expr {
    override def toString: String = s"App($fnExpr, ${argExpr.toString.drop(7).dropRight(1)})"
  }

  private case class Seq(lhs: Expr, rhs: Expr) extends Expr

  private case class WhileN0(cond: Expr, exprIn: Expr) extends Expr

  private case class Proj(obj: Expr, id: VarID) extends Expr

  private case class BoxDef(id: StringID, initExpr: Expr, next: Expr) extends Expr

  private case class SetBox(box: Expr, setExpr: Expr) extends Expr

  private case class Vec(data: Vector[Expr]) extends Expr

  private case class HMap(data: Map[Expr, Expr]) extends Expr

  private case class ClassDef(memberName: Vector[StringID],
                              methods: Map[VarID, Expr],
                              typeName: StringID,
                              next: Expr) extends Expr

  private case class BuiltinFnE2E(fn: Expr => Expr, arg: Expr, opName: String) extends Expr {
    override def toString: String = s"BF<$opName>($arg)"
  }


  // Values
  case class UnitV() extends FOV {
    override def toString: String = "UnitV"
  }

  private case class NumV(data: BigInt) extends FOV {
    override def toString: String = "%s".format(data)
  }

  private case class StrV(data: String) extends FOV {
    override def toString: String = data
  }

  private case class CloV(argName: Vector[VarID], fExpr: Expr, var fEnv: Env) extends FOV {
    override def toString: String = s"함수(인자:[${argName.toString.drop(7).dropRight(1)}], 본문:[$fExpr])"

    def call(arg: Vector[Value]): Value = {
      if (argName.length != arg.length) {
        throw error.MteSyntaxErr(
          s"얘! 지금 돈 ${arg.length} 원에 함수 ${this}를 불러달라고 한 거니?? 단가가 안 맞잖아 임마!!"
        )
      }
      pret(fExpr, fEnv ++ argName.zip(arg).toMap)
    }
  }

  private case class VecV(data: Vector[Value]) extends FOV {
    override def toString: String = data.toString()
  }

  private case class HMapV(data: Map[Value, Value]) extends FOV {
    override def toString: String = data.toString()
  }

  private case class ClassV(memberName: Vector[StringID],
                            methods: Map[VarID, Value],
                            typeName: StringID,
                            cEnv: Env) extends NFOV {
    def makeMethodOf(obj: ObjV, methodName: VarID): Value = methods.get(methodName) match {
      case Some(value) => value match {
        case fn@CloV(_, _, _) =>
          fn.fEnv += (ThisKW -> obj)
          fn.fEnv += (typeName -> this)
          fn
        case _ => throw error.MteRuntimeErr(
          s"얘! 메소드 정의($value)가 틀려먹었단다! 고통스럽게 죽도록 해요~"
        )
      }
      case None => throw error.MteRuntimeErr(
        s"얘! 지금 코괴물 ${typeName}의 코객체 ${obj}가 ${methodName}를 알겠니??"
      )
    }

    def construct(memberValues: Vector[Value]): ObjV = if (memberName.length == memberValues.length) {
      ObjV(memberName.zip(memberValues).toMap, supertype=this)
    } else throw error.MteRuntimeErr(
      s"얘! 지금 멤버 ${memberName.length}개짜리 코객체 만드는 데 값을 ${memberValues.length}개 줘서 되겠니??"
    )

    override def decay: FOV = ???

    override def toString: String = s"코괴물(${typeName.id})"
  }

  private case class ObjV(data: Map[StringID, Value],
                          supertype: ClassV) extends FOV {
    override def toString: String =
      s"${supertype.typeName.id}(${data.toString().drop(4).dropRight(1)})"
  }

  /**
   * @param data 데이터의 변경 과정을 기록해놓음
   * @param addr 현재 박스가 데이터의 총 변경 과정 중 몇 번째 원소에 해당하는 값을 갖고 있는지
   */
  case class BoxV(var data: Vector[Value], var addr: Addr) extends NFOV {
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

  def makeNewBox(value: Value): BoxV = BoxV(Vector(value), 0)

  def pret(expr: Expr, env: Env): Value = {
    def validateID(id: VarID): Unit = id match {
      case StringID(id) => if (id.contains("킹") && id.contains("갓")) {
        throw error.MteRuntimeErr("내가 킹하고 갓하고 함부로 막 붙이지 말라 그랬지!!")
      }
      case _ =>
    }

    def fnCall(fnExpr: Expr, argExpr: Vector[Expr]): Value = pret(fnExpr, env) match {
      case CloV(argName, fExpr, fEnv) =>
        if (argName.length != argExpr.length) {
          throw error.MteSyntaxErr()
        }
        val argV: Vector[Value] = argExpr.map(x => pret(x, env).toFOV)
        pret(fExpr, fEnv ++ argName.zip(argV).toMap)
      case typeV@ClassV(_, _, _, _) => typeV.construct(argExpr.map(x => pret(x, env)))
      case err@_ => throw error.MteRuntimeErr(
        s"얘! 지금 $err 이게 함수로 보이니?"
      )
    }

    expr match {
      case Num(data) => NumV(data)
      case NewValue(value) => value
      case UnitE() => unitV
      case BinaryOp(lhs, rhs, _, op) => op(pret(lhs, env).toFOV, pret(rhs, env).toFOV) match {
        case Left(err) => throw error.MteRuntimeErr(err + s"\nexpr: $expr")
        case Right(value) => value
      }
      case TernaryOp(x, y, z, op, _) => op(pret(x, env).toFOV, pret(y, env).toFOV, pret(z, env).toFOV) match {
        case Left(err) => throw error.MteRuntimeErr(err + s"\nexpr: $expr")
        case Right(value) => value
      }
      case Id(name) => env.get(name) match {
        case Some(value) => value match {
          case NumV(num) => name match {
            case StringID(id) => NumV(num - 3000 * id.count(_ == '코'))
            case _ => NumV(num)
          }
          case value@_ => value
        }
        case _ => throw error.MteRuntimeErr(
          s"얘! 컴파일쟁이($env)들은 $name 이런 거 잘 몰라 임마!"
        )
      }
      case ValDef(valName, initExpr, next) =>
        validateID(valName)
        val initV: Value = pret(initExpr, env).toFOV
        pret(next, env + (valName -> initV))
      case Fun(funName, argName, fExpr) =>
        val ret: CloV = CloV(argName, fExpr, env)
        ret.fEnv += (funName -> ret)
        ret
      case App(fnExpr, argExpr) => fnCall(fnExpr, argExpr)
      case Seq(lhs, rhs) =>
        pret(lhs, env); pret(rhs, env)
      case WhileN0(cond, exprIn) =>
        val check = (condExpr: Expr) => {
          pret(condExpr, env).toFOV match {
            case NumV(data) => data
            case err@_ => throw error.MteRuntimeErr(
              s"얘! 지금 $err 이게 조건문 안에 들어갈 수 있겠니?? 죽여벌랑"
            )
          }
        }
        var condVal = check(cond)
        while (condVal != 0) {
          fnCall(sugar.exprToFn(exprIn), Vector())
          condVal = check(cond)
        }
        unitV
      case Proj(obj, id) => pret(obj, env).toFOV match {
        case obj@ObjV(data, supertype) => id match
          case id@StringID(_) => data.get(id) match
            case Some(value) => value
            case None => supertype.makeMethodOf(obj, id)
          case _ => supertype.makeMethodOf(obj, id)
        case _ => throw error.MteRuntimeErr(
          s"얘! 지금 네 눈에 $obj 이게 객체로 보이니??"
        )
      }
      case BoxDef(id, initExpr, next) =>
        validateID(id)
        val initV: Value = pret(initExpr, env).toFOV
        pret(next, env + (id -> makeNewBox(initV)))
      case SetBox(ref, setExpr) =>
        val setVal = pret(setExpr, env).toFOV
        pret(ref, env) match {
          case box@BoxV(_, _) =>
            box.set(setVal)
            setVal
          case err@_ => throw error.MteRuntimeErr(
            s"얘! 지금 네 눈에 $err (${ref}를 실행했다 맨이야) 이게 NFT로 보이니? env=$env"
          )
        }
      case Vec(data) => VecV(data.map(x => pret(x, env)))
      case HMap(data) => HMapV(data.map((key, value) => (pret(key, env), pret(value, env))))
      case ClassDef(memberName, methods, typeName, next) =>
        if (env.contains(typeName)) throw error.MteRuntimeErr(
          s"얘! 이미 사용하고 있는 변수명은 코괴물 이름이 되지 아내!"
        )
        pret(next, env + (typeName -> ClassV(
          memberName, methods.map((key, value) => (key, pret(value, env))), typeName, env
        )))
      case BuiltinFnE2E(fn, arg, _) => pret(fn(arg), env)
    }
  }

  val unitE: UnitE = UnitE()
  val unitV: UnitV = UnitV()

  package ops {
    import java.lang.Package

    private def liftBinaryOp(op: (BigInt, BigInt) => BigInt): (=> Value, => Value) => Either[String, Value] = {
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

    private def liftUnaryOp(op: BigInt => BigInt): (=> Value, => Value) => Either[String, Value] = {
      def ret(lhs: => Value, @unused rhs: => Value): Either[String, Value] = lhs match {
        case NumV(data) => Right(NumV(op(data)))
        case _ => Left(s"얘! 여기 지금 $lhs 이게 숫자로 보이니??")
      }

      ret
    }

    def makeAddExpr(lhs: Expr, rhs: Expr): Expr = BinaryOp(lhs, rhs, "add", liftBinaryOp(_ + _))
    def makeSubExpr(lhs: Expr, rhs: Expr): Expr = BinaryOp(lhs, rhs, "sub", liftBinaryOp(_ - _))
    def makeMulExpr(lhs: Expr, rhs: Expr): Expr = BinaryOp(lhs, rhs, "mul", liftBinaryOp(_ * _))
    def makeDivExpr(lhs: Expr, rhs: Expr): Expr = BinaryOp(lhs, rhs, "div", liftBinaryOp(_ / _))
    def makeGtExpr(lhs: Expr, rhs: Expr): Expr = BinaryOp(lhs, rhs, "gt", liftBinaryOp(utility.gtInt))
    def makeGeExpr(lhs: Expr, rhs: Expr): Expr = BinaryOp(lhs, rhs, "ge", liftBinaryOp(utility.geInt))
    def makeLogNotExpr(lhs: Expr): Expr = BinaryOp(lhs, unitE, "logNot", liftUnaryOp(utility.logNot))
    def makeRemainderExpr(lhs: Expr, rhs: Expr): Expr = BinaryOp(lhs, rhs, "%", liftBinaryOp(_ % _))

    def makeSqrtExpr(lhs: Expr): Expr = {
      def sqrt(value: => Value, @unused x: => Value): Either[String, Value] = value match {
        case NumV(data) => Right(NumV(math.sqrt(data.doubleValue).floor.toLong))
        case _ => Left(s"얘! 지금 $value 이게 숫자로 보이니??")
      }

      BinaryOp(lhs, unitE, "sqrt", sqrt)
    }

    def makePrintExpr(x: Expr, template: String): Expr = {
      def ret(x: Value, @unused y: Value): Either[String, Value] = {
        print(template.format(x))
        Right(x)
      }

      BinaryOp(x, unitE, "print", ret)
    }

    def makeAssertExpr(value: Expr): Expr = {
      def myAssert(value: => Value, @unused y: => Value): Either[String, Value] = value match {
        case NumV(data) =>
          if (data != 0)
            Right(unitV)
          else
            Left(s"얘! 지금 네 눈에 $value 이게 truthy한 값이 되겠니??")
        case _ => Left(s"얘! 지금 네 눈에 $value 이게 true/false가 되는 타입이 되겠니??")
      }

      BinaryOp(value, unitE, "assert", myAssert)
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
          case None => Left(s"얘! 뭉탱이($data)는 $idx 이런 거 몰라 임마!!")
        }
        case _ => Left(s"얘! 지금 인덱스 접근 문법(mte=$container, index=$idx)에서 $container 이게 컨테이너가 되겠니??")
      }

      BinaryOp(lhs, rhs, "access", access)
    }

    def makeVecFillExpr(sizeE: Expr, initE: Expr): Expr = {
      def vecFill(size: => Value, init: => Value): Either[String, Value] = size match {
        case NumV(data) => Right(VecV(Vector.fill(data.toInt) {
          init
        }))
        case _ => Left(s"얘! 지금 나보고 ${size}개의 원소를 가진 벡터를 만들어 달라고 하면 어쩌자는 거니!")
      }

      BinaryOp(sizeE, initE, "vecFill", vecFill)
    }

    def makeVecIotaExpr(lbdInclusive: Expr, ubdExclusive: Expr): Expr = {
      def vecIota(lbdInclusive: => Value, ubdExclusive: => Value): Either[String, Value] = {
        lbdInclusive match {
          case NumV(l) => ubdExclusive match {
            case NumV(u) => Right(VecV((l.toInt to u.toInt).map(value => NumV(value)).toVector))
            case _ => Left(s"얘! 지금 $ubdExclusive 이게 숫자로 보이니??")
          }
          case _ => Left(s"얘! 지금 $lbdInclusive 이게 숫자로 보이니??")
        }
      }

      BinaryOp(lbdInclusive, ubdExclusive, "vecIota", vecIota)
    }

    def makeExtExpr(lhs: Expr, rhs: Expr): Expr = {
      def extension(lhs: => Value, rhs: => Value): Either[String, Value] = lhs match {
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

      BinaryOp(lhs, rhs, "ext", extension)
    }

    def makeSizeExpr(lhs: Expr, rhs: Expr): Expr = {
      def containerSize(vec: => Value, @unused x: => Value): Either[String, Value] = vec match {
        case VecV(data) => Right(NumV(data.length))
        case HMapV(data) => Right(NumV(data.size))
        case _ => Left(s"얘! 지금 $vec 이게 컨테이너로 보이냐??")
      }

      BinaryOp(lhs, rhs, "size", containerSize)
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

      BinaryOp(vecE, dropNumE, "vecDropRight", vecDropRight)
    }

    def makeVecFilterExpr(vec: Expr, fn: Expr): Expr = {
      def vecFilter(vec: => Value, fn: => Value): Either[String, Value] = vec match {
        case VecV(vec) => fn match {
          case fnV@CloV(_, _, _) => Right(VecV(vec.filter(value => fnV.call(Vector(value)).isTruthy)))
          case _ => Left(s"얘! 지금 $fn 이게 함수로 보이니??")
        }
        case _ => Left(s"얘! 지금 $vec 이게 벡터로 보이니??")
      }

      BinaryOp(vec, fn, "vecFilter", vecFilter)
    }

    def makeVecRejectExpr(vec: Expr, fn: Expr): Expr = {
      def vecReject(vec: => Value, fn: => Value): Either[String, Value] = vec match {
        case VecV(vec) => fn match {
          case fnV@CloV(_, _, _) => Right(VecV(vec.filterNot(value => fnV.call(Vector(value)).isTruthy)))
          case _ => Left(s"얘! 지금 $fn 이게 함수로 보이니??")
        }
        case _ => Left(s"얘! 지금 $vec 이게 벡터로 보이니??")
      }

      BinaryOp(vec, fn, "vecReject", vecReject)
    }

    def makeVecMapExpr(vec: Expr, fn: Expr): Expr = {
      def vecMap(vec: => Value, fn: => Value): Either[String, Value] = vec match {
        case VecV(vec) => fn match {
          case fnV@CloV(_, _, _) => Right(VecV(vec.map(x => fnV.call(Vector(x)))))
          case _ => Left(s"얘! 지금 $fn 이게 함수로 보이니??")
        }
        case _ => Left(s"얘! 지금 $vec 이게 벡터로 보이니??")
      }

      BinaryOp(vec, fn, "vecMap", vecMap)
    }
  }

  package ops3 {
    def makeTernaryIfExpr(cond: Expr, yes: Expr, no: Expr): Expr = {
      def ternaryIf(cond: => Value, yes: => Value, no: => Value): Either[String, Value] =
        if (cond.isTruthy) Right(yes) else Right(no)

      TernaryOp(cond, yes, no, ternaryIf, "If")
    }

    def makeVecUpdatedExpr(vecE: Expr, idxE: Expr, dataE: Expr): Expr = {
      def vecUpdated(vec: => Value, idx: => Value, data: => Value): Either[String, Value] = vec match {
        case VecV(vec) => idx match {
          case NumV(idx) =>
            if (idx < 0 || idx >= vec.length)
              Left(s"얘! 지금 idx=$idx 이게 길이 ${vec.length}짜리 한줄서기에 접근이 되겠니??")
            else
              Right(VecV(vec.updated(idx.toInt, data)))
          case _ => Left(s"얘! 지금 한줄서기 인덱스가 $idx 이게 숫자로 보이니??")
        }
        case _ => Left(s"얘! 지금 한줄서기 인덱스 접근 문법(mte=$vec, index=$idx)에서 $vec 이게 한줄서기로 보이냐??")
      }

      TernaryOp(vecE, idxE, dataE, vecUpdated, "vecUpdate")
    }
  }

  package sugar {
    def exprToFn(expr: Expr): Expr = Fun(AnonFn1, Vector(), expr)

    def newScope(expr: Expr): Expr = App(exprToFn(expr), Vector())

    def vecToSeq(vec: Vector[Expr]): Expr = {
      @tailrec
      def help(vec: Vector[Expr], ret: Expr): Expr = {
        if (vec.nonEmpty) {
          help(vec.tail, Seq(ret, vec.head))
        } else {
          ret
        }
      }

      if (vec.length == 1) {
        vec.head
      } else {
        help(vec.tail, vec.head)
      }
    }

    def newFor(iterName: String, initExpr: Expr, condExpr: Expr, manipulationExpr: Expr, inExpr: Expr): Expr = {
      BoxDef(StringID(iterName), initExpr, WhileN0(condExpr, Seq(
        inExpr,
        manipulationExpr
      )))
    }

    def newSimpleFor(iterName: String, lbdInclusive: Expr, ubdExclusive: Expr, inExpr: Expr): Expr = newFor(
      iterName = iterName,
      initExpr = lbdInclusive,
      condExpr = ops.makeGtExpr(ubdExclusive, Id(StringID(iterName))),
      manipulationExpr = SetBox(Id(StringID(iterName)), ops.makeAddExpr(Id(StringID(iterName)), Num(1))),
      inExpr = inExpr
    )

  }

  /*
  (characters not shown below)
  * / %
  + -
  :
  < >
  = !
  &
  ^
  |
  (all letters, $, _)
  */

  sealed trait CodeFragment
  case class CodeFragmentGeneral(expr: Expr) extends CodeFragment
  case class ValDefFragment(name: StringID, expr: Expr) extends CodeFragment
  case class BoxDefFragment(name: StringID, expr: Expr) extends CodeFragment
  case class ClassDefFragment(memberName: Vector[StringID],
                              methods: Map[VarID, Expr],
                              id: StringID) extends CodeFragment

  def joinFragments(fragments: Vector[CodeFragment]): Expr = {
    if (fragments.length == 1) {
      fragments.head match {
        case CodeFragmentGeneral(expr) => expr
        case ValDefFragment(name, expr) => ValDef(name, expr, unitE)
        case BoxDefFragment(name, expr) => BoxDef(name, expr, unitE)
        case ClassDefFragment(memberName, methods, id) => ClassDef(memberName, methods, id, unitE)
      }
    } else {
      fragments.head match
        case CodeFragmentGeneral(expr) => Seq(expr, joinFragments(fragments.tail))
        case ValDefFragment(name, expr) => ValDef(name, expr, joinFragments(fragments.tail))
        case BoxDefFragment(name, expr) => BoxDef(name, expr, joinFragments(fragments.tail))
        case ClassDefFragment(memberName, methods, id) =>
          ClassDef(memberName, methods, id, joinFragments(fragments.tail))
    }
  }

  implicit def exprToFragment(expr: Expr): CodeFragment = CodeFragmentGeneral(expr)
  implicit def strToId(id: String): Expr = Id(id)
  implicit def intToNum(num: Int): Expr = Num(num)
  implicit def toIdFrag(name: String): CodeFragment = CodeFragmentGeneral(Id(name))
  implicit def toNumFrag(num: Int): CodeFragment = CodeFragmentGeneral(Num(num))

  class ExprOps(lhs: Expr) {
    @unused def 배(rhs: Expr): Expr = ops.makeAddExpr(lhs, rhs)

    @unused def 코(rhs: Expr): Expr = ops.makeSubExpr(lhs, rhs)

    @unused def 조이고(rhs: Expr): Expr = ops.makeMulExpr(lhs, rhs)

    @unused def 법회(rhs: Expr): Expr = ops.makeDivExpr(lhs, rhs)

    @unused def 릴(rhs: Expr): Expr = App(rhs, Vector(lhs))

    @unused def 리액션(template: String): Expr = ops.makePrintExpr(lhs, template)

    @unused def 케바바바밥줘(rhs: Expr): Expr = Seq(lhs, rhs)

    @unused def 꼽표(@unused rhs: EndState3): Expr = ops.makeLogNotExpr(lhs)

    @unused def 코가커요(rhs: Expr): Expr = ops.makeRemainderExpr(lhs, rhs)

    @unused def 반제곱(@unused rhs: EndState6): Expr = ops.makeSqrtExpr(lhs)
  }

  implicit class ExprOpsExtByExpr(lhs: Expr) extends ExprOps(lhs)
  implicit class ExprOpsExtByStr(lhs: String) extends ExprOps(Id(lhs))
  implicit class ExprOpsExtByNum(lhs: Int) extends ExprOps(Num(lhs))

  private class EndState
  private case class EndState1() extends EndState
  private case class EndState2() extends EndState
  private case class EndState3() extends EndState
  private case class EndState4() extends EndState
  private case class EndState5() extends EndState
  private case class EndState6() extends EndState
  private case class EndState7() extends EndState


  @unused val 뭉: Num = Num(1)
  @unused val 탱: Num = Num(0)
  @unused val 뭉뭉: Num = Num(3)
  @unused val 뭉탱: Num = Num(2)
  @unused val 뭉뭉뭉: Num = Num(7)
  @unused val 뭉뭉탱: Num = Num(6)
  @unused val 뭉탱뭉: Num = Num(5)
  @unused val 뭉탱탱: Num = Num(4)
  @unused val 뭉뭉뭉뭉: Num = Num(15)
  @unused val 뭉뭉뭉탱: Num = Num(14)
  @unused val 뭉뭉탱뭉: Num = Num(13)
  @unused val 뭉뭉탱탱: Num = Num(12)
  @unused val 뭉탱뭉뭉: Num = Num(11)
  @unused val 뭉탱뭉탱: Num = Num(10)
  @unused val 뭉탱탱뭉: Num = Num(9)
  @unused val 뭉탱탱탱: Num = Num(8)

  @unused val 스키비야: UnitE = unitE
  @unused val 스킵이야: UnitE = unitE

  @unused val 나: Expr = Id(ThisKW)

  case object 춘잣 {
    @targetName("fact")
    @unused
    def !(frag: CodeFragment*): Value = pret(joinFragments(frag.toVector), Map())
  }

  @unused
  @targetName("notFact")
  val ~! = EndState1()

  @unused
  @targetName("quesQues")
  val ?? = EndState2()

  @unused
  @targetName("eqQuesQues")
  val =?? = EndState5()

  @unused
  @targetName("bitNotBitNot")
  val ~~ = EndState3()

  @unused
  val 게이조이고 = EndState4()

  @unused
  val 방어부스터 = EndState6()

  @unused
  @targetName("factFact")
  val !! = EndState7()

  /**
   * 곱셈 연산이다 맨이야.
   * 문법: (lhs) 화면을 확대하셨군요!! (rhs)
   * @param lhs lhs
   */
  case class MulBuilder(lhs: Expr) {
    import 확대하셨군요.RhsBuilder

    /**
     * 곱셈 연산이다 맨이야.
     * 문법: (lhs) 화면을 확대하셨군요!! (rhs)
     * @param rhs rhs
     */
    @unused
    def 화면을(rhs: RhsBuilder): Expr = ops.makeMulExpr(lhs, rhs.rhs)
  }

  implicit class MulBuilderFromExpr(lhs: Expr) extends MulBuilder(lhs)
  implicit class MulBuilderFromId(id: String) extends MulBuilder(Id(id))
  implicit class MulBuilderFromInt(n: Int) extends MulBuilder(Num(n))

  /**
   * 곱셈 연산이다 맨이야.
   * 문법: (lhs) 화면을 확대하셨군요!! (rhs)
   */
  case object 확대하셨군요 {
    @unused
    @targetName("factFact")
    def !!(rhs: Expr): RhsBuilder = RhsBuilder(rhs)

    case class RhsBuilder(rhs: Expr)
  }

  /**
   * 주어진 조건이 유링게슝하면 앞부분을, 유링게슝하지 않으면 뒷부분을 실행한단다. 뒷부분은 안돼 임마!!
   * 문법: (유링게슝한?) (expr) {expr} 안유링게슝 {expr}
   */
  @unused
  case object 유링게슝한 {
    @targetName("question")
    @unused
    def ?(condExpr: Expr)(trueExprs: CodeFragment*): Ifn0Builder =
      Ifn0Builder(condExpr, joinFragments(trueExprs.toVector))

    case class Ifn0Builder(condExpr: Expr, trueExpr: Expr) {
      @unused
      def 안유링게슝(falseExprs: CodeFragment*): Expr =
        ops3.makeTernaryIfExpr(condExpr, trueExpr, joinFragments(falseExprs.toVector))
    }
  }

  /**
   * 얘! 변수 사용이 잘 안 되니? 이걸로 변수를 정의해 보렴!
   * 문법: 아니 자기가 (id)라는 사람인데 {expr}을 했대
   */
  @unused val 아니: ValBuilderInit = ValBuilderInit()
  /**
   * 얘! 변수 사용이 잘 안 되니? 이걸로 변수를 정의해 보렴!
   * 문법: 아니세상에 자기가 (id)라는 사람인데 {expr}을 했대
   */
  @unused val 아니세상에: ValBuilderInit = ValBuilderInit()

  case class ValBuilderInit() {
    @unused
    def 자기가(name: String): ValBuilder =
      ValBuilder(StringID(name))

    case class ValBuilder(name: StringID) {
      @unused
      def 라는사람인데(expr: Expr): ValBuilder2 =
        ValBuilder2(name, expr)

      @unused
      def 이라는사람인데(expr: Expr): ValBuilder2 =
        ValBuilder2(name, expr)
    }

    case class ValBuilder2(name: StringID, expr: Expr) {
      @unused
      def 를(@unused h: 했대.type): CodeFragment =
        ValDefFragment(name, expr)

      @unused
      def 을(@unused h: 했대.type): CodeFragment =
        ValDefFragment(name, expr)

      @unused
      def 발행(@unused x: NFT.type): DefBoxBuilder = DefBoxBuilder(name, expr)

      case class DefBoxBuilder(name: StringID, expr: Expr) {
        @unused
        def 를(@unused h: 했대.type): CodeFragment = BoxDefFragment(name, expr)

        @unused
        def 을(@unused h: 했대.type): CodeFragment = BoxDefFragment(name, expr)
      }
    }
  }

  case object 했대

  /**
   * 함수를 정의한단다. 함수 리터럴이라 다른 함수에 인자로 넘겨줄 수도 있고 정의한 즉시 쓸 수도 있어요~
   * 람다 함수 만들 때 쓰는 가벼운 표현을 하나 더 만들 계획이다 맨이야
   * 문법: 아~! (("fnName", "argName") / ("argName"))은/는 (expr)이/가 참 좋구나~!
   */
  case object 아 {
    @unused
    @targetName("notFact")
    def ~!(funName: String, argName: String*): FunBuilder =
      FunBuilder(StringID(funName), argName.toVector.map(StringID.apply))

    case class FunBuilder(funName: StringID, argName: Vector[VarID]) {
      @unused
      def 는(fExpr: CodeFragment*): FunBuilder2 =
        FunBuilder2(funName, argName, joinFragments(fExpr.toVector))

      @unused
      def 은(fExpr: CodeFragment*): FunBuilder2 =
        FunBuilder2(funName, argName, joinFragments(fExpr.toVector))
    }

    case class FunBuilder2(funName: StringID, argName: Vector[VarID], fExpr: Expr) {
      @unused
      def 이(@unused x: 참.type): FunBuilder3 =
        FunBuilder3(funName, argName, fExpr)

      @unused
      def 가(@unused x: 참.type): FunBuilder3 =
        FunBuilder3(funName, argName, fExpr)
    }

    case class FunBuilder3(funName: StringID, argName: Vector[VarID], fExpr: Expr) {
      @unused
      def 좋구나(@unused x: EndState): CodeFragment =
        ValDefFragment(funName, Fun(funName, argName, fExpr))
    }
  }

  case object 참

  class LambdaBuilder(argIds: Vector[StringID]) {
    @unused
    def 은(fnExpr: Expr): LambdaBuilder2 = LambdaBuilder2(argIds, fnExpr)

    @unused
    def 은(fnExprs: Vector[Expr]): LambdaBuilder2 =
      LambdaBuilder2(argIds, sugar.vecToSeq(fnExprs))

    @unused
    def 는(fnExpr: Expr): LambdaBuilder2 = LambdaBuilder2(argIds, fnExpr)

    @unused
    def 는(fnExprs: Vector[Expr]): LambdaBuilder2 =
      LambdaBuilder2(argIds, sugar.vecToSeq(fnExprs))

    class LambdaBuilder2(argIds: Vector[StringID], fnExpr: Expr) {
      @unused
      def 다(@unused joyGo: EndState4): Expr = Fun(AnonFn1, argIds, fnExpr)

      @unused
      def 이다(@unused joyGo: EndState4): Expr = Fun(AnonFn1, argIds, fnExpr)
    }
  }

  implicit class LambdaBuilderFromVecStr(argIds: Vector[Expr]) extends LambdaBuilder(argIds.map {
    case Id(StringID(id)) => StringID(id)
    case _ => throw error.MteSyntaxErr(s"얘! 지금 $argIds 이게 변수명으로 보이니?!?!")
  })
  implicit class LambdaBuilderFromStr(argName: String) extends LambdaBuilder(Vector(StringID(argName)))

  /**
   * 케인인님 함수호출 한판해요
   * 문법: (f)아/야 (arg) 먹어라??
   * @param f 호출할 함수
   */
  case class AppBuilder(f: Expr) {
    @unused
    def 아(args: Expr*): AppBuilder2 = AppBuilder2(f, args.toVector)

    @unused
    def 야(args: Expr*): AppBuilder2 = AppBuilder2(f, args.toVector)

    case class AppBuilder2(f: Expr, args: Vector[Expr]) {
      def 먹어라(@unused x: EndState2): Expr = App(f, args)
    }
  }

  implicit class AppBuilderFromExpr(f: Expr) extends AppBuilder(f)
  implicit class AppBuilderFromId(id: String) extends AppBuilder(Id(id))
  implicit class AppBuilderFromInt(n: Int) extends AppBuilder(Num(n))

  /**
   * 케인님이 조건문이 거짓이 되기 전까지 강제연결을 해 주신단다!
   * 문법: 강제연결 (cond) {expr}
   * @param cond 조건문
   * @param exprIn 조건문이 참인 동안 실행할 명령
   */
  @unused
  def 강제연결(cond: Expr)(exprIn: Expr*): Expr = {
    WhileN0(cond, sugar.vecToSeq(exprIn.toVector))
  }

  /**
   * 케인인님이 11수의 경험을 살려 해당 문장을 원하는 만큼 실행시켜 준단다!
   * 문법: ((정수)수) (iterName) {expr}
   */
  @unused
  case class SimpleForBuilder(n: Expr) {
    @unused
    def 수(iterName: String)(forExpr: Expr): Expr =
      sugar.newSimpleFor(iterName, Num(0), n, forExpr)

    @unused
    def 수(forExpr: Expr): Expr =
      sugar.newSimpleFor("", Num(0), n, forExpr)
  }

  implicit class SimpleForBuilderFromInt(n: Int) extends SimpleForBuilder(Num(n))
  implicit class SimpleForBuilderFromExpr(expr: Expr) extends SimpleForBuilder(expr)
  implicit class SimpleForBuilderFromId(id: String) extends SimpleForBuilder(Id(id))

  case class RunningForBuilder(lhs: Expr) {
    @unused
    def 달려가(rhs: Expr)(iterName: String)(forExpr: CodeFragment*): Expr =
      sugar.newSimpleFor(iterName, lhs, rhs, joinFragments(forExpr.toVector))
  }

  implicit class RunningForBuilderFromExpr(expr: Expr) extends RunningForBuilder(expr)
  implicit class RunningForBuilderFromId(id: String) extends RunningForBuilder(Id(id))
  implicit class RunningForBuilderFromInt(n: Int) extends RunningForBuilder(Num(n))

  /**
   * 도네 금액이 케인인님의 마음에 들지 판단한단다.
   * 문법: (lhs) 돈 (rhs) 원에??/=??
   * @param lhs lhs
   */
  case class GtBuilder(lhs: Expr) {
    @unused
    def 돈(rhs: Expr): GtBuilder2 =
      GtBuilder2(lhs, rhs)

    case class GtBuilder2(lhs: Expr, rhs: Expr) {
      @unused
      def 원에(@unused x: EndState2): Expr = ops.makeGtExpr(lhs, rhs)

      @unused
      def 원에(@unused x: EndState5): Expr = ops.makeGeExpr(lhs, rhs)
    }
  }

  implicit class GtBuilderFromExpr(lhs: Expr) extends GtBuilder(lhs)
  implicit class GtBuilderFromString(lhs: String) extends GtBuilder(Id(lhs))
  implicit class GtBuilderFromInt(lhs: Int) extends GtBuilder(Num(lhs))

  /**
   * 새 스코프를 생성한단다.
   * 문법: 박스 아저씨 {expr}
   */
  @unused
  case object 박스 {
    @unused
    def 아저씨(expr: Expr): Expr =
      sugar.newScope(expr)
  }

  /**
   * Assertion을 하도록 해요~
   * 문법: 정품 맞어 {}
   */
  @unused
  case object 정품 {
    @unused
    def 맞어(expr: Expr*): Expr =
      sugar.vecToSeq(expr.map(ops.makeAssertExpr).toVector)
  }


  case object NFT

  case class SetBoxBuilder(box: Expr) {
    @unused def 게(@unused x: 그런.type): SetBoxBuilder2 = SetBoxBuilder2(box)
    @unused def 니게(@unused x: 그런.type): SetBoxBuilder2 = SetBoxBuilder2(box)

    case class SetBoxBuilder2(ptr: Expr) {
      @unused def 사람이(setExpr: Expr): SetBoxBuilder3 = SetBoxBuilder3(ptr, setExpr)
    }

    case class SetBoxBuilder3(ptr: Expr, setExpr: Expr) {
      @unused def 일(@unused x: 순없는지.type): Expr = SetBox(ptr, setExpr)

      @unused def 힐(@unused x: 순없는지.type): Expr = SetBox(ptr, setExpr)
    }
  }

  implicit class PtrSetBuilderFromExpr(box: Expr) extends SetBoxBuilder(box)
  implicit class PtrSetBuilderFromStr(name: String) extends SetBoxBuilder(Id(name))
  implicit class PtrSetBuilderFromInt(num: Int) extends SetBoxBuilder(Num(num))

  @unused case object 그런
  @unused case object 순없는지

  case class ArgListBuilder(argList: Vector[String]) {
    @unused
    @targetName("plusPlusPlus")
    def +++(nextArgId: String): Vector[String] = argList :+ nextArgId
  }

  implicit class ArgListBuilderFromVecStr(argList: Vector[String]) extends ArgListBuilder(argList)
  implicit class ArgListBuilderFromStr(firstArg: String) extends ArgListBuilder(Vector(firstArg))

  /**
   * 다변수 함수에 인자를 전달할 때 쓰는 문법을 만들었어요~
   * 문법: fnExpr 묶음!!(arg1, arg2, ...)
   */
  @unused
  object 묶음 {
    @unused
    @targetName("factFact")
    def !!(args: Expr*): Vector[Expr] = {
      var ret: Vector[Expr] = Vector()
      for (arg <- args) ret = ret :+ arg
      ret
    }

    @unused
    @targetName("factFactFactFact")
    def !!!!(args: Expr*): Expr = sugar.vecToSeq(args.toVector)
  }

  /**
   * 한줄서기 벡터를 만드는 문법이에요~
   * @param args 벡터를 구성하는 원소들 (variadic)
   * @return 벡터에 해당하는 표현식
   * @return 벡터에 해당하는 표현식
   */
  @unused
  def 한줄서기(args: Expr*): Expr = {
    var ret: Vector[Expr] = Vector()
    for (arg <- args) ret = ret :+ arg
    Vec(ret)
  }

  /**
   * 같은 원소가 여러 개 있는 벡터를 만들고 싶으면 왕한줄서기 문법을 사용하세요~
   * @param size 벡터의 크기
   * @param init 벡터의 초기치
   * @return 벡터에 해당하는 표현식
   */
  @unused
  def 왕한줄서기(size: Expr, init: Expr): Expr = ops.makeVecFillExpr(size, init)

  @unused
  case object 뭐 {
    @unused def 드시냐고(ubdExclusive: Expr): IotaBuilder = IotaBuilder(Num(0), ubdExclusive)

    @unused def 드시냐고(lbdInclusive: Expr, ubdExclusive: Expr): IotaBuilder =
      IotaBuilder(lbdInclusive, ubdExclusive)

    case class IotaBuilder(lbdInclusive: Expr, ubdExclusive: Expr) {
      @unused
      def 번째(@unused x: 물어봅니다.type): Expr = ops.makeVecIotaExpr(lbdInclusive, ubdExclusive)
    }
  }

  case object 물어봅니다

  case class VecOpsBuilder(lhs: Expr) {
    /**
     * 문법: lhs 조이는 rhs
     * @param rhs 연결할 벡터
     * @return 연결된 벡터를 나타내는 표현식
     */
    @unused
    def 조이는(rhs: Expr): Expr = ops.makeExtExpr(lhs, rhs)

    /**
     * 문법: vec 즐기면서가자 fn
     * @param fn vec의 각 원소에 적용할 함수
     * @return container map을 나타내는 표현식
     */
    @unused
    def 즐기면서가자(fn: Expr): Expr = ops.makeVecMapExpr(lhs, fn)

    /**
     * 문법: vec 특수한 fn
     * @param fn 필터 함수
     * @return container filter를 나타내는 표현식
     */
    @unused
    def 특수한(fn: Expr): Expr = ops.makeVecFilterExpr(lhs, fn)

    /**
     * 문법: vec 씻구 fn
     * @param fn 필터 함수
     * @return container filter(reject)를 나타내는 표현식
     */
    @unused
    def 씻구(fn: Expr): Expr = ops.makeVecRejectExpr(lhs, fn)

    import 임마.VecAccessRhsBuilder

    /**
     * 문법: (container) 갖고와 임마!! (idx)
     * @param accIdxWrapper idx
     * @return vector access expression
     */
    @unused
    def 갖고와(accIdxWrapper: VecAccessRhsBuilder): Expr = ops.makeAccessExpr(lhs, accIdxWrapper.accIdx)

    import 한판마안.VecUpdateRhsBuilder


    /**
     * 문법: (container) 갖고와 (idx) 할게 한판마안~~!! (val)
     * @param idxE idx
     * @return a fragment of vector update expression
     */
    @unused
    def 갖고와(idxE: Expr): VecUpdateBuilder = VecUpdateBuilder(lhs, idxE)

    case class VecUpdateBuilder(vecE: Expr, idxE: Expr) {
      @unused
      def 할게(rhs: VecUpdateRhsBuilder): Expr = ops3.makeVecUpdatedExpr(vecE, idxE, rhs.updateE)
    }

    /**
     * 케인님이 ㄸㄸㅆ를 통해 한줄서기에서 가장 오른쪽에 있는 n개의 원소를 제거해 주실 거예요~
     * 문법: vec ㄸㄸㅆ num
     * @param rhs num
     * @return dropRight을 나타내는 표현식
     */
    @unused
    def ㄸㄸㅆ(rhs: Expr): Expr = ops.makeVecDropRightExpr(lhs, rhs)
  }

  case object 임마 {
    @unused
    @targetName("factFact")
    def !!(accIdx: Expr): VecAccessRhsBuilder = VecAccessRhsBuilder(accIdx)

    case class VecAccessRhsBuilder(accIdx: Expr)
  }

  case object 한판마안 {
    @unused
    @targetName("notNotFactFact")
    def ~~!!(updateE: Expr): VecUpdateRhsBuilder = VecUpdateRhsBuilder(updateE)

    case class VecUpdateRhsBuilder(updateE: Expr)
  }

  implicit class VecOpsBuilderFromExpr(lhs: Expr) extends VecOpsBuilder(lhs)
  implicit class VecOpsBuilderFromId(id: String) extends VecOpsBuilder(Id(id))
  implicit class VecOpsBuilderFromInt(num: Int) extends VecOpsBuilder(Num(num))



  /**
   * 랜덤이 필요하면 윷놀이 를! 아침까지 조이도록 해요~
   */
  @unused
  val 윷놀이: Expr = Num(utility.randBetween(1, 6))

  /**
   * stdin에서 정수를 읽어온단다.
   * 정수 말고 다른 문자는 이따 쉬는시간에 감사하다고 할게~
   */
  @unused
  val 개입: Expr = BuiltinFnE2E(builtin.readInt, UnitE(), "readInt")

  /**
   * 나는! 나는! 나는! stdout으로 출력을 했다!
   * 문법: 리액션 (expr) // 리액션 (expr, template)
   * @param expr 출력할 표현식
   * @param template 템플릿 스트링
   * @return 프로그램 트리
   */
  @unused
  def 리액션(expr: Expr, template: String = "%s"): Expr = ops.makePrintExpr(expr, template)

  /**
   * 새로운 클래스를 정의하는 문법이다 맨이야~
   * 문법: "id" 하는 플레이보이 예전에 (members) 같은데 이제 (method definition) 하는갑지 돈좀버냐?? 개노잼 노라라??
   * @param id 새로운 타입의 이름
   */
  case class TypeEBuilder(id: StringID) {
    /**
     * 새로운 클래스를 정의하는 문법이다 맨이야~
     * 문법: "id" 하는 플레이보이 예전에 (members) 같은데 이제 (method definition) 하는갑지 돈좀버냐?? 개노잼 노라라??
     */
    @unused
    def 하는(@unused x: 플레이보이.type): TypeEBuilder2 = TypeEBuilder2(id)

    case class TypeEBuilder2(id: StringID) {
      @unused
      def 예전에(memberName: String*): TypeEBuilder3 = TypeEBuilder3(id, memberName.toVector.map(StringID.apply))
    }

    case class TypeEBuilder3(id: StringID, memberName: Vector[StringID]) {
      @unused
      def 하더놈(@unused x: 같은데.type): TypeEBuilder4 = TypeEBuilder4(id, memberName)
    }

    case class TypeEBuilder4(id: StringID, memberName: Vector[StringID]) {
      @unused
      def 이제(methods: TypeEMethodBuilderFinal*): TypeEBuilder5 = {
        val methodVec = methods.toVector
        val keys = methodVec.map(x => x.methodName)
        val values = methodVec.map(x => Fun(x.methodName, x.arg, x.fExpr))
        TypeEBuilder5(id, memberName, keys.zip(values).toMap)
      }

      @unused
      def 이제(@unused x: UnitE): TypeEBuilder5 = TypeEBuilder5(id, memberName, Map())
    }

    case class TypeEBuilder5(id: StringID, memberName: Vector[StringID], methods: Map[VarID, Expr]) {
      import 돈좀버냐.Help
      @unused
      def 하는갑지(@unused x: Help.type): TypeEBuilder6 = TypeEBuilder6(id, memberName, methods)
    }

    case class TypeEBuilder6(id: StringID, memberName: Vector[StringID], methods: Map[VarID, Expr]) {
      @unused
      def 노라라(@unused x: EndState2): ClassDefFragment = ClassDefFragment(memberName, methods, id)
    }
  }

  implicit def typeEBuilderDecay(tuple: (String, Expr)): (VarID, Expr) = (StringID(tuple(0)), tuple(1))

  case object 플레이보이
  case object 같은데
  case object 돈좀버냐 {
    @unused
    @targetName("quesQues")
    def ??(@unused x: 개노잼.type): Help.type = Help

    case object Help
  }

  case object 개노잼

  implicit class TypeEBuilderFromId(id: String) extends TypeEBuilder(StringID(id))

  case class TypeEMethodBuilder1(methodName: VarID) {
    @unused
    def 중에는(@unused x: EndState7): TypeEMethodBuilder2 = TypeEMethodBuilder2(methodName)
    case class TypeEMethodBuilder2(methodName: VarID) {
      @unused
      def 아무리(arg: VarIDImplicit*): TypeEMethodBuilder3 = TypeEMethodBuilder3(methodName, arg.toVector.map(x => x.id))
    }

    case class TypeEMethodBuilder3(methodName: VarID, arg: Vector[VarID]) {
      @unused
      def 라도(fExpr: Expr): TypeEMethodBuilder4 = TypeEMethodBuilder4(methodName, arg, fExpr)

      @unused
      def 이라도(fExpr: Expr): TypeEMethodBuilder4 = TypeEMethodBuilder4(methodName, arg, fExpr)
    }

    case class TypeEMethodBuilder4(methodName: VarID, arg: Vector[VarID], fExpr: Expr) {
      @unused
      def 할(@unused x: 수가.type): TypeEMethodBuilder5 =
        TypeEMethodBuilder5(methodName, arg, fExpr)
    }

    case class TypeEMethodBuilder5(methodName: VarID, arg: Vector[VarID], fExpr: Expr) {
      @unused
      def 없단다(@unused x: EndState7): TypeEMethodBuilderFinal =
        TypeEMethodBuilderFinal(methodName, arg, fExpr)
    }
  }

  case object 수가

  private case class TypeEMethodBuilderFinal(methodName: VarID, arg: Vector[VarID], fExpr: Expr)

  implicit def methodToFragment(x: TypeEMethodBuilderFinal): CodeFragment = x.methodName match {
    case id@StringID(_) => ValDefFragment(id, Fun(id, x.arg, x.fExpr))
    case _ => throw mte.error.MteSyntaxErr(
      s"얘! 지금 메서드가 아닌 함수 이름 꼬라지(${x.methodName}) 이게 뭐니!!"
    )
  }


  implicit class TypeEMethodBuilderFromString(id: String) extends TypeEMethodBuilder1(StringID(id))
  implicit class TypeEMethodBuilderFromID(id: VarID) extends TypeEMethodBuilder1(id)

  case class ProjBuilder(obj: Expr) {
    @unused
    def 의(memberId: VarID): ProjBuilder2 = ProjBuilder2(obj, memberId)

    @unused
    def 의(memberId: String): ProjBuilder2 = ProjBuilder2(obj, StringID(memberId))

    case class ProjBuilder2(obj: Expr, memberId: VarID) {
      @unused
      def 감동님(@unused x: 사랑해.type): Expr = Proj(obj, memberId)
    }
  }

  case object 사랑해

  implicit class ProjBuilderFromExpr(expr: Expr) extends ProjBuilder(expr)
  implicit class ProjBuilderFromId(id: String) extends ProjBuilder(Id(id))
  implicit class ProjBuilderFromInt(n: Int) extends ProjBuilder(Num(n))



  package builtin {
    def readInt(@unused expr: Expr = UnitE()): Expr =
      Num(scala.io.StdIn.readInt)
  }

  package utility {
    implicit class Piper[F](val value: F) {
      @unused
      @targetName("pipe")
      def |>[G](f: F => G): G = f(value)
    }

    /**
     *
     */
    private val rand: Random = new Random()

    def randBetween(lbdInclusive: Int, ubdExclusive: Int): Int =
      rand.between(lbdInclusive, ubdExclusive)

    @unused
    def leInt(lhs: BigInt, rhs: BigInt): Int =
      if (lhs <= rhs) 1 else 0

    /**
     * 얘! 내가 이런 함수까지 일일이 다 주석을 달아야 되니? 귀찮아!
     * @param lhs 아 귀찮아~~~!!!!!
     * @param rhs 아 귀찮아~~~!!!!!
     * @return lhs > rhs
     */
    def gtInt(lhs: BigInt, rhs: BigInt): BigInt =
      if (lhs > rhs) 1 else 0

    def geInt(lhs: BigInt, rhs: BigInt): BigInt =
      if (lhs >= rhs) 1 else 0

    def logNot(lhs: BigInt): BigInt =
      if (lhs == 0) 1 else 0

    def maxInt(lhs: BigInt, rhs: BigInt): BigInt =
      if (lhs > rhs) lhs else rhs
  }

  package error {
    /**
     * MTELang의 런타임 에러는 이 클래스가 담당할 거예요~
     * @param message message
     * @param cause cause
     */
    final case class MteRuntimeErr(private val message: String = "",
                                   private val cause: Throwable = None.orNull)
      extends Exception(message, cause)

    final case class MteSyntaxErr(private val message: String = "",
                            private val cause: Throwable = None.orNull)
      extends Exception(message, cause)

    final case class MteUnexpectedErr(private val message: String = "",
                                  private val cause: Throwable = None.orNull)
      extends Exception(message, cause)


  }

  package predef {

  }
}