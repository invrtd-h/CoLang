package mte {
  import scala.annotation.{tailrec, targetName, unused}
  import scala.language.implicitConversions
  import scala.util.Random
  import scala.util.chaining.*
  import utility.Piper

  @unused
  sealed trait Expr {
    @targetName("plus")
    def +(rhs: Expr): Expr = ops.makeAddExpr(this, rhs)
  }

  sealed trait Value

  type Env = Map[String, Value]
  type Addr = Int
  type Sto = Map[Addr, Value]

  // Expressions
  @unused
  case class Num(data: BigInt) extends Expr {
    @unused
    @targetName("plusPlus")
    def ++(rhs: Num): Num = Num(data * 10 + rhs.data)
  }

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
                               op: (Value, Value, Value) => Either[String, Value],
                               name: String) extends Expr {
    override def toString: String =
      s"<$name>($x, $y, $z)"
  }

  private case class Id(name: String) extends Expr

  private case class ValDef(valName: String, initExpr: Expr) extends Expr

  private case class Fun(funName: String, argName: String, fExpr: Expr) extends Expr

  private case class App(fnExpr: Expr, argExpr: Expr) extends Expr

  private case class Seq(lhs: Expr, rhs: Expr) extends Expr

  private case class IfN0(cond: Expr, exprTrue: Expr, exprFalse: Expr) extends Expr

  private case class WhileN0(cond: Expr, exprIn: Expr) extends Expr

  private case class NewBox(initExpr: Expr) extends Expr

  private case class SetBox(box: Expr, setExpr: Expr) extends Expr

  private case class Vec(data: Vector[Expr]) extends Expr

  private case class HMap(data: Map[Expr, Expr]) extends Expr

  private case class Try(exprTry: Expr) extends Expr

  private case class BuiltinFnE2E(fn: Expr => Expr, arg: Expr, name: String) extends Expr {
    override def toString: String = s"BF<$name>($arg)"
  }


  // Values
  case class UnitV() extends Value {
    override def toString: String = "UnitV"
  }

  private case class NumV(data: BigInt) extends Value {
    override def toString: String = "%s".format(data)
  }

  private case class CloV(argName: String, fExpr: Expr, var fEnv: Env) extends Value {
    override def toString: String = s"CloV($argName, $fExpr)"
  }

  private case class VecV(data: Vector[Value]) extends Value {
    override def toString: String = data.toString()
  }

  private case class HMapV(data: Map[Value, Value]) extends Value {
    override def toString: String = data.toString()
  }

  private case class TypeV(memberName: Vector[String],
                           methods: Map[String, Value],
                           typeName: String) extends Value {
    def makeMethodOf(obj: ObjV, methodName: String): Value = methods.get(methodName) match {
      case Some(value) => value match {
        case fn@CloV(_, _, _) =>
          fn.fEnv += ("%reserved_self%" -> obj)
          fn
        case _ => throw error.MteRuntimeErr(
          s"얘! 메소드 정의($value)가 틀려먹었단다! 고통스럽게 죽도록 해요~"
        )
      }
      case None => throw error.MteRuntimeErr(
        s"얘! 지금 클래스 ${typeName}의 인스턴스 ${obj}가 ${methodName}를 알겠니??"
      )
    }

    def construct(memberValues: Vector[Value]): ObjV = if (memberName.length == memberValues.length) {
      ObjV(memberName.zip(memberValues).toMap, supertype=this)
    } else throw error.MteRuntimeErr(
      s"얘! 지금 멤버 ${memberName.length}개짜리 코객체 만드는 데 값을 ${memberValues.length}개 줘서 되겠니??"
    )
  }

  private case class ObjV(data: Map[String, Value],
                          supertype: TypeV) extends Value

  /**
   * 가비지 컬렉션이 더 잘 되는 새로운 박스를 준비했어요~~ 버그가 있는지 테스트 중이다 맨이야
   * @param data 데이터의 변경 과정을 기록해놓음
   * @param addr 현재 박스가 데이터의 총 변경 과정 중 몇 번째 원소에 해당하는 값을 갖고 있는지
   */
  case class BoxV(var data: Vector[Value], var addr: Addr) extends Value {
    def get: Value = data(addr)

    def set(newData: Value): Unit = {
      addr = data.length
      data = data.appended(newData)
    }
  }

  def makeNewBox(value: Value): BoxV = BoxV(Vector(value), 0)

  @unused
  case class Process(var pSto: Sto, private var nextAddr: Addr = 0) {
    @unused
    def pret(expr: Expr): Value = {
      val func = ProcessFn(this, Map())
      func.pret(expr)
    }

    def giveNextAddr: Addr = {
      nextAddr += 1
      nextAddr
    }
  }

  case class ProcessFn(parentProcess: Process, var pEnv: Env) {

    @unused
    @tailrec
    private def unbox(value: Value): Value = value match {
      case box@BoxV(_, _) => unbox(box.get)
      case _ => value
    }

    private def fnCall(fnExpr: Expr, argExpr: Expr): Value = pret(fnExpr) match {
      case CloV(argName, fExpr, fEnv) =>
        val argV: Value = pret(argExpr)
        val newFn: ProcessFn = ProcessFn(parentProcess, fEnv + (argName -> argV))
        newFn.pret(fExpr)
      case err@_ => throw error.MteRuntimeErr(
        s"얘! 지금 $err 이게 함수로 보이니?"
      )
    }

    def pret(expr: Expr): Value = {
      expr match {
        case Num(data) => NumV(data)
        case UnitE() => unitV
        case BinaryOp(lhs, rhs, _, op) => op(pret(lhs) |> unbox, pret(rhs) |> unbox) match {
          case Left(err) => throw error.MteRuntimeErr(err + s"\nexpr: $expr")
          case Right(value) => value
        }
        case TernaryOp(x, y, z, op, _) => op(pret(x) |> unbox, pret(y) |> unbox, pret(z) |> unbox) match {
          case Left(err) => throw error.MteRuntimeErr(err + s"\nexpr: $expr")
          case Right(value) => value
        }
        case Id(name) => pEnv.get(name) match {
          case Some(value) => value match {
            case NumV(num) => NumV(num - 3000 * name.count(_ == '코'))
            case value@_ => value
          }
          case _ => throw error.MteRuntimeErr(
            s"얘! 컴파일쟁이($pEnv)들은 $name 이런 거 잘 몰라 임마!"
          )
        }
        case ValDef(valName, initExpr) =>
          if (valName.contains("킹") && valName.contains("갓")) {
            throw error.MteRuntimeErr("내가 킹하고 갓하고 함부로 막 붙이지 말라 그랬지!!")
          }
          val initV: Value = pret(initExpr)
          pEnv += (valName -> initV)
          initV
        case Fun(funName, argName, fExpr) =>
          val ret: CloV = CloV(argName, fExpr, pEnv)
          ret.fEnv += (funName -> ret)
          ret
        case App(fnExpr, argExpr) => fnCall(fnExpr, argExpr)
        case Seq(lhs, rhs) =>
          pret(lhs); pret(rhs)
        case IfN0(cond, exprTrue, exprFalse) =>
          pret(cond) |> unbox match {
            case NumV(data) =>
              if (data != 0)
                fnCall(sugarbuilder.exprToFn(exprTrue), unitE)
              else
                fnCall(sugarbuilder.exprToFn(exprFalse), unitE)
            case err@_ => throw error.MteRuntimeErr(
              s"얘! 지금 $err 이게 조건문 안에 들어갈 수 있겠니?? 죽여벌랑"
            )
          }
        case WhileN0(cond, exprIn) =>
          val check = (condExpr: Expr) => {
            pret(condExpr) |> unbox match {
              case NumV(data) => data
              case err@_ => throw error.MteRuntimeErr(
                s"얘! 지금 $err 이게 조건문 안에 들어갈 수 있겠니?? 죽여벌랑"
              )
            }
          }
          var condVal = check(cond)
          while (condVal != 0) {
            fnCall(sugarbuilder.exprToFn(exprIn), unitE)
            condVal = check(cond)
          }
          unitV
        case NewBox(initExpr) =>
          val initV: Value = pret(initExpr) |> unbox
          makeNewBox(initV)
        case SetBox(ref, setExpr) =>
          val setVal = pret(setExpr) |> unbox
          pret(ref) match {
            case box@BoxV(_, _) =>
              box.set(setVal)
              setVal
            case err@_ => throw error.MteRuntimeErr(
              s"얘! 지금 네 눈에 $err (${ref}를 실행했다 맨이야) 이게 NFT로 보이니? env=$pEnv, sto=${
                parentProcess.pSto.toVector.sortBy((addr: Addr, _) => addr)
              }"
            )
          }
        case Vec(data) => VecV(data.map(pret))
        case HMap(data) => HMapV(data.map((key, value) => (pret(key), pret(value))))
        case Try(exprTry) =>
          try {
            pret(exprTry)
            NumV(1)
          } catch {
            case _: error.MteRuntimeErr => NumV(0)
          }
        case BuiltinFnE2E(fn, arg, _) => pret(fn(arg))
      }
    }
  }

  val unitE: UnitE = UnitE()
  val unitV: UnitV = UnitV()

  package ops {
    import java.lang.Package

    def liftBinaryOp(op: (BigInt, BigInt) => BigInt): (=> Value, => Value) => Either[String, Value] = {
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

    def liftUnaryOp(op: BigInt => BigInt): (=> Value, => Value) => Either[String, Value] = {
      def ret(lhs: => Value, @unused rhs: => Value): Either[String, Value] = lhs match {
        case NumV(data) => Right(NumV(op(data)))
        case _ => Left(s"얘! 여기 지금 $lhs 이게 숫자로 보이니??")
      }

      ret
    }

    val valSub = liftBinaryOp(_ - _)
    val valMul = liftBinaryOp(_ * _)
    val valDiv = liftBinaryOp(_ / _)
    val valRemainder = liftBinaryOp(_ % _)

    def makeAddExpr(lhs: Expr, rhs: Expr): Expr = BinaryOp(lhs, rhs, "add", liftBinaryOp(_ + _))
    def makeSubExpr(lhs: Expr, rhs: Expr): Expr = BinaryOp(lhs, rhs, "sub", valSub)
    def makeMulExpr(lhs: Expr, rhs: Expr): Expr = BinaryOp(lhs, rhs, "mul", valMul)
    def makeDivExpr(lhs: Expr, rhs: Expr): Expr = BinaryOp(lhs, rhs, "div", valDiv)
    def makeGtExpr(lhs: Expr, rhs: Expr): Expr = BinaryOp(lhs, rhs, "gt", liftBinaryOp(utility.gtInt))
    def makeGeExpr(lhs: Expr, rhs: Expr): Expr = BinaryOp(lhs, rhs, "ge", liftBinaryOp(utility.geInt))
    def makeLogNotExpr(lhs: Expr): Expr = BinaryOp(lhs, unitE, "logNot", liftUnaryOp(utility.logNot))
    def makeRemainderExpr(lhs: Expr, rhs: Expr): Expr = BinaryOp(lhs, rhs, "%", valRemainder)

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
      case _ => Left(s"얘! 지금 한줄서기 인덱스 접근 문법(mte=$container, index=$idx)에서 $container 이게 컨테이너가 되겠니??")
    }

    def makeAccessExpr(lhs: Expr, rhs: Expr): Expr = BinaryOp(lhs, rhs, "access", access)

    def vecAppend(lhs: => Value, rhs: => Value): Either[String, Value] = lhs match {
      case VecV(lData) => Right(VecV(lData :+ rhs))
      case _ => Left(s"얘! 지금 한줄서기 원소 추가 문법(lhs=$lhs, rhs=$rhs)에서 $lhs 여기다 뭘 넣겠다는 거니??")
    }

    def makeVecAppendExpr(lhs: Expr, rhs: Expr): Expr = BinaryOp(lhs, rhs, "VecAppend", vecAppend)

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

    def makeVecFillExpr(sizeE: Expr, initE: Expr): Expr = {
      def vecFill(size: => Value, init: => Value): Either[String, Value] = size match {
        case NumV(data) => Right(VecV(Vector.fill(data.toInt) {
          init
        }))
        case _ => Left(s"얘! 지금 나보고 ${size}개의 원소를 가진 벡터를 만들어 달라고 하면 어쩌자는 거니!")
      }

      BinaryOp(sizeE, initE, "vecFill", vecFill)
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
  }

  package ops3 {
    def ternaryIf(cond: => Value, yes: => Value, no: => Value): Either[String, Value] = cond match {
      case NumV(data) => if (data != 0) Right(yes) else Right(no)
      case _ => Left(s"얘! 지금 네 눈에 $cond 이게 boolean 취급할 수 있는 값처럼 보이니?? 죽여벌랑")
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

  package sugarbuilder {
    def exprToFn(expr: Expr): Expr = Fun("", "_", expr)

    def newScope(expr: Expr): Expr = App(exprToFn(expr), unitE)

    def seqs(expressions: Expr*): Expr = {
      @tailrec
      def help(expressions: Vector[Expr], ret: Expr): Expr = {
        if (expressions.nonEmpty) {
          help(expressions.tail, Seq(ret, expressions.head))
        } else {
          ret
        }
      }

      if (expressions.length == 1) {
        expressions.head
      } else {
        help(expressions.tail.toVector, expressions.head)
      }
    }

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
      newScope(
        Seq(
          ValDef(iterName, initExpr),
          WhileN0(condExpr, Seq(
            inExpr,
            manipulationExpr
          ))
        )
      )
    }

    def newSimpleFor(iterName: String, lbdInclusive: Expr, ubdExclusive: Expr, inExpr: Expr): Expr = newFor(
      iterName = iterName,
      initExpr = NewBox(lbdInclusive),
      condExpr = ops.makeGtExpr(ubdExclusive, Id(iterName)),
      manipulationExpr = SetBox(Id(iterName), Id(iterName) + 1),
      inExpr = inExpr
    )

    def newMultivariableFn(fnName: String, argName: Vector[String], fnExpr: Expr): Expr = {
      def help(fnName: String, argName: Vector[String], fnExpr: Expr, depth: Int = 0): Expr =
        if (argName.length <= 1)
          Fun(fnName, argName(0), fnExpr)
        else
          Fun(fnName, argName.head, help(fnName + s"inner$depth", argName.tail, fnExpr, depth + 1))

      help(fnName, argName, fnExpr)
    }

    def newMultivariableLambda(argName: Vector[String], fnExpr: Expr): Expr = {
      if (argName.length <= 1)
        Fun("", argName(0), fnExpr)
      else
        Fun("", argName.head, newMultivariableLambda(argName.tail, fnExpr))
    }

    @tailrec
    def newMultivariableApp(fnExpr: Expr, args: Vector[Expr]): Expr = {
      if (args.length < 1) {
        throw error.MteSyntaxErr(
          "얘! 인자 리스트에 아무것도 없단다!"
        )
      } else if (args.length == 1) {
        App(fnExpr, args(0))
      } else {
        newMultivariableApp(App(fnExpr, args(0)), args.tail)
      }
    }
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

  implicit def toId(name: String): Expr = Id(name)
  implicit def toNum(num: Int): Expr = Num(num)

  class ExprOps(lhs: Expr) {
    @unused def 배(rhs: Expr): Expr = ops.makeAddExpr(lhs, rhs)

    @unused def 코(rhs: Expr): Expr = ops.makeSubExpr(lhs, rhs)

    @unused def 조이고(rhs: Expr): Expr = ops.makeMulExpr(lhs, rhs)

    @unused def 법회(rhs: Expr): Expr = ops.makeDivExpr(lhs, rhs)

    @unused def 릴(rhs: Expr): Expr = App(rhs, lhs)

    @unused def 리액션(template: String): Expr = ops.makePrintExpr(lhs, template)

    @unused def 케바바바밥줘(rhs: Expr): Expr = Seq(lhs, rhs)

    @unused def 꼽표(@unused rhs: EndState3): Expr = ops.makeLogNotExpr(lhs)

    @unused def 코가커요(rhs: Expr): Expr = ops.makeRemainderExpr(lhs, rhs)

    @unused def 반제곱(@unused rhs: EndState6): Expr = ops.makeSqrtExpr(lhs)
  }

  @unused
  @targetName("bitNotBitNot")
  val ~~ = EndState3()

  @unused
  val 게이조이고 = EndState4()

  @unused
  val 방어부스터 = EndState6()

  implicit class ExprOpsExtByExpr(lhs: Expr) extends ExprOps(lhs)
  implicit class ExprOpsExtByStr(lhs: String) extends ExprOps(Id(lhs))
  implicit class ExprOpsExtByNum(lhs: Int) extends ExprOps(Num(lhs))

  class EndState
  case class EndState1() extends EndState
  case class EndState2() extends EndState
  case class EndState3() extends EndState
  case class EndState4() extends EndState
  case class EndState5() extends EndState
  case class EndState6() extends EndState


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

  @unused val 나: Expr = Id("%reserved_self%")

  def makeNewProgram: Program = {
    val process: Process = Process(Map())
    val program: Program = Program(process, ProcessFn(process, Map()))
    program
  }

  def 춘잣: ProgramBuilder = ProgramBuilder(makeNewProgram)

  case class ProgramBuilder(program: Program) {
    @targetName("fact")
    @unused
    def ! (expr: Expr*) : ProgramBuilder = factHelper(expr.toVector)

    @unused
    def 케바바바밥줘(expr: Expr*): ProgramBuilder = factHelper(expr.toVector)

    private def factHelper(expr: Vector[Expr]): ProgramBuilder = {
      val sequencedExpr = sugarbuilder.vecToSeq(expr)
      val result: Value = program.mainFn.pret(sequencedExpr)
      this
    }
  }
  case class Program(process: Process, mainFn: ProcessFn)

  @unused
  @targetName("notFact")
  val ~! = EndState1()

  @unused
  @targetName("quesQues")
  val ?? = EndState2()

  @unused
  @targetName("eqQuesQues")
  val =?? = EndState5()

  // 를! "x"
  @unused
  case object 를 {
    @targetName("fact")
    @unused
    def !(valName: String): Expr = Id(valName)
  }

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
    def ?(condExpr: Expr)(trueExprs: Expr*): Ifn0Builder =
      Ifn0Builder(condExpr, sugarbuilder.vecToSeq(trueExprs.toVector))

    case class Ifn0Builder(condExpr: Expr, trueExpr: Expr) {
      @unused
      def 안유링게슝(falseExprs: Expr*): Expr =
        IfN0(condExpr, trueExpr, sugarbuilder.vecToSeq(falseExprs.toVector))
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
      ValBuilder(name)

    case class ValBuilder(name: String) {
      @unused
      def 라는사람인데(expr: Expr): ValBuilder2 =
        ValBuilder2(name, expr)

      @unused
      def 이라는사람인데(expr: Expr): ValBuilder2 =
        ValBuilder2(name, expr)
    }

    case class ValBuilder2(name: String, expr: Expr) {
      @unused
      def 를(@unused h: 했대.type): Expr =
        ValDef(name, expr)

      @unused
      def 을(@unused h: 했대.type): Expr =
        ValDef(name, expr)
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
    def ~!(funName: String, argName: String): FunBuilder =
      FunBuilder(funName, argName)

    @unused
    @targetName("notFact")
    def ~!(argName: String): FunBuilder =
      FunBuilder("", argName)

    case class FunBuilder(funName: String, argName: String) {
      @unused
      def 는(fExpr: Expr): FunBuilder2 =
        FunBuilder2(funName, argName, fExpr)

      @unused
      def 은(fExpr: Expr): FunBuilder2 =
        FunBuilder2(funName, argName, fExpr)
    }

    case class FunBuilder2(funName: String, argName: String, fExpr: Expr) {
      @unused
      def 이(@unused x: 참.type): FunBuilder3 =
        FunBuilder3(funName, argName, fExpr)

      @unused
      def 가(@unused x: 참.type): FunBuilder3 =
        FunBuilder3(funName, argName, fExpr)
    }

    case class FunBuilder3(funName: String, argName: String, fExpr: Expr) {
      @unused
      def 좋구나(@unused x: EndState): Expr =
        Seq(ValDef(funName, Fun(funName, argName, fExpr)), Id(funName))
    }
  }

  case object 참

  class MultiLambdaBuilder(argIds: Vector[String]) {
    @unused
    def 은(fnExpr: Expr): MultiLambdaBuilder2 = MultiLambdaBuilder2(argIds, fnExpr)

    @unused
    def 는(fnExpr: Expr): MultiLambdaBuilder2 = MultiLambdaBuilder2(argIds, fnExpr)

    class MultiLambdaBuilder2(argIds: Vector[String], fnExpr: Expr) {
      @unused
      def 다(@unused joyGo: EndState4): Expr = sugarbuilder.newMultivariableLambda(argIds, fnExpr)

      @unused
      def 이다(@unused joyGo: EndState4): Expr = sugarbuilder.newMultivariableLambda(argIds, fnExpr)
    }
  }

  def vecIdToVecString(vec: Vector[Expr]): Vector[String] = vec.map {
    case Id(id) => id
    case _ => throw error.MteSyntaxErr(s"얘! 지금 $vec 이게 변수명으로 보이니?!?!")
  }

  implicit class MultiLambdaBuilderFromVecStr(argIds: Vector[Expr]) extends MultiLambdaBuilder(vecIdToVecString(argIds))
  implicit class MultiLambdaBuilderFromStr(argName: String) extends MultiLambdaBuilder(Vector(argName))

  /**
   * 케인인님 함수호출 한판해요
   * 문법: (f)아/야 (arg) 먹어라??
   * @param f 호출할 함수
   */
  case class AppBuilder(f: Expr) {
    @unused
    def 아(arg: Expr): AppBuilder2 = AppBuilder2(f, Vector(arg))

    @unused
    def 아(args: Vector[Expr]): AppBuilder2 = AppBuilder2(f, args)

    @unused
    def 야(arg: Expr): AppBuilder2 = AppBuilder2(f, Vector(arg))

    @unused
    def 야(args: Vector[Expr]): AppBuilder2 = AppBuilder2(f, args)

    case class AppBuilder2(f: Expr, args: Vector[Expr]) {
      def 먹어라(@unused x: EndState2): Expr =
        sugarbuilder.newMultivariableApp(f, args)
    }
  }

  implicit class AppBuilderFromExpr(f: Expr) extends AppBuilder(f)
  implicit class AppBuilderFromId(id: String) extends AppBuilder(Id(id))
  implicit class AppBuilderFromInt(n: Int) extends AppBuilder(Num(n))

  /**
   * 케인님이 조건문이 거짓이 되기 전까지 강제연결을 해 주신단다!
   * 문법: 강제연결 (cond) {expr}결
   * @param cond 조건문
   * @param exprIn 조건문이 참인 동안 실행할 명령
   */
  @unused
  def 강제연결(cond: Expr)(exprIn: Expr*): Expr = {
    WhileN0(cond, sugarbuilder.vecToSeq(exprIn.toVector))
  }

  /**
   * 케인인님이 11수의 경험을 살려 해당 문장을 원하는 만큼 실행시켜 준단다!
   * 문법: ((정수)수) (iterName) {expr}
   */
  @unused
  case class SimpleForBuilder(n: Expr) {
    @unused
    def 수(iterName: String)(forExpr: Expr): Expr =
      sugarbuilder.newSimpleFor(iterName, 0, n, forExpr)

    @unused
    def 수(forExpr: Expr): Expr =
      sugarbuilder.newSimpleFor("", 0, n, forExpr)
  }

  implicit class SimpleForBuilderFromInt(n: Int) extends SimpleForBuilder(Num(n))
  implicit class SimpleForBuilderFromExpr(expr: Expr) extends SimpleForBuilder(expr)
  implicit class SimpleForBuilderFromId(id: String) extends SimpleForBuilder(Id(id))

  case class RunningForBuilder(lhs: Expr) {
    @unused
    def 달려가(rhs: Expr)(iterName: String)(forExpr: Expr): Expr =
      sugarbuilder.newSimpleFor(iterName, lhs, rhs, forExpr)
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
      sugarbuilder.newScope(expr)
  }

  /**
   * 내부 식이 문법적으로 올바른 식이면 1, 아니면 0을 리턴한단다.
   * 문법: 주제넘은? {expr}
   */
  @unused
  case object 주제넘은 {
    @unused
    @targetName("question")
    def ? (expr: Expr): Expr =
      Try(expr)
  }

  /**
   * Assertion을 하도록 해요~
   * 문법: 정품 맞어 {}
   */
  @unused
  case object 정품 {
    @unused
    def 맞어(expr: Expr*): Expr =
      sugarbuilder.vecToSeq(expr.map(ops.makeAssertExpr).toVector)
  }

  @unused
  case class BoxBuilder(expr: Expr) {
    @unused
    def 발행(@unused nft: NFT.type): Expr = NewBox(expr)

    @unused
    @targetName("rrArrow")
    def -->(@unused nft: NFT.type): Expr = NewBox(expr)
  }

  implicit class BoxBuilderFromExpr(expr: Expr) extends BoxBuilder(expr)
  implicit class BoxBuilderFromID(name: String) extends BoxBuilder(Id(name))
  implicit class BoxBuilderFromNumber(num: Int) extends BoxBuilder(Num(num))

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
    def !!!!(args: Expr*): Expr = sugarbuilder.vecToSeq(args.toVector)
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
    def 즐기면서가자(fn: Expr): Expr = ???

    /**
     * 문법: vec 특수한 fn
     * @param fn 필터 함수
     * @return container filter를 나타내는 표현식
     */
    @unused
    def 특수한(fn: Expr): Expr = ???

    /**
     * 문법: vec 씻구 fn
     * @param fn 필터 함수
     * @return container filter(reject)를 나타내는 표현식
     */
    @unused
    def 씻구(fn: Expr): Expr = ???

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

  package builtin {
    def readInt(@unused expr: Expr = UnitE()): Expr =
      Num(scala.io.StdIn.readInt)

    def write(value: Value, template: String): Value = {
      print(template.format(value))
      value
    }

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

    def randomStringGen(n: Int): String = {
      var str: String = ""
      for (_ <- 0 to n) {
        str += rand.nextPrintableChar()
      }
      str
    }


    /**
     * 프로그램이 내부적으로 사용하는 변수 이름은 이 함수가 부여해 준단다.
     * @return 변수 이름
     */
    def randomNameGen(): String =
      s"%rsvd%_${randomStringGen(21)}%"

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

    def makeKillFn(msg: String): Expr => Expr = {
      def ret(unitE: Expr): Expr = {
        assert(false, msg)
        unitE
      }

      ret
    }

    def pow(x: Long, power: Long): Long = {
      var x_ = x
      var pw = power
      var ret = x_
      while (pw > 0) {
        if (pw % 2 == 1) ret = ret * x_
        x_ = x_ * x_
        pw /= 2
      }
      ret
    }
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
  }

  package stl {

  }
}