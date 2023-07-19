package mte {
  import scala.annotation.{tailrec, targetName, unused}
  import scala.language.implicitConversions
  import scala.util.Random
  import scala.util.chaining.*
  import utility.Piper

  @unused
  sealed trait Expr {
    @targetName("plus")
    def +(rhs: Expr): Expr = BinaryOp(this, rhs, "plus", ops.valAdd)

    @targetName("minus")
    def -(rhs: Expr): Expr = BinaryOp(this, rhs, "minus", ops.valSub)
  }

  sealed trait Value

  type Env = Map[String, Value]
  type Addr = Int
  type Sto = Map[Addr, Value]

  // Expressions
  @unused
  case class Num(data: BigInt) extends Expr

  case class UnitE() extends Expr

  private case class BinaryOp(lhs: Expr, rhs: Expr, name: String, op: (=> Value, => Value) => Value) extends Expr {
    override def toString: String =
      s"BO<$name>($lhs, $rhs)"
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

  private case class Try(exprTry: Expr) extends Expr

  private case class BuiltinFnE2E(fn: Expr => Expr, arg: Expr, name: String) extends Expr {
    override def toString: String = s"BF<$name>($arg)"
  }

  private case class BuiltinFnV2V(fn: Value => Value, arg: Expr, name: String) extends Expr {
    override def toString: String = s"BFV<$name>($arg)"
  }


  // Values
  private case class UnitV() extends Value {
    override def toString: String = "UnitV"
  }

  private case class NumV(data: BigInt) extends Value {
    override def toString: String = "%s".format(data)
  }

  private case class CloV(argName: String, fExpr: Expr, var fEnv: Env) extends Value {
    override def toString: String = s"CloV($argName, $fExpr)"
  }

  private case class Vec(data: Vector[Value]) extends Value

  private case class BoxV(addr: Addr) extends Value {
    /**
     * 자~ 가짜 주소를 만들었어요~
     * 가짜 주소는 진짜 주소처럼 이렇게 잘 돼 있지 않아 딱 한줄서기 그런 게 아니고
     * 동인천역 앞에 동인천역 앞에 쯥 그냥 뭉탱이로 있단 말이야
     * @return 가짜 주소를 16진수로 변환한 문자열
     */
    override def toString: String = {
      val newAddr = {
        val newAddr = (utility.pow(913, addr + 20) * 998244353) >>> 12
        newAddr - (newAddr % 8)
      }
      f"*[0x$newAddr%16x]"
    }
  }

  package ops {
    def bigIntOpToValueOp(op: (BigInt, BigInt) => BigInt): (=> Value, => Value) => Value = {
      def ret(lhs: => Value, rhs: => Value): Value = {
        lhs match {
          case NumV(dataL) => rhs match {
            case NumV(dataR) => NumV(op(dataL, dataR))
            case _ => throw error.MteRuntimeErr(
              s"얘! 여기 지금 $rhs 이게 숫자로 보이니??"
            )
          }
          case _ => throw error.MteRuntimeErr(
            s"얘! 여기 지금 $lhs 이게 숫자로 보이니??"
          )
        }
      }

      ret
    }
    
    val valAdd    = bigIntOpToValueOp(_ + _)
    val valSub    = bigIntOpToValueOp(_ - _)
    val valMul    = bigIntOpToValueOp(_ * _)
    val valDiv    = bigIntOpToValueOp(_ / _)
    val valGt     = bigIntOpToValueOp(utility.gtInt)
    val valLogNot = bigIntOpToValueOp(utility.logNot)
  }

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

    @tailrec
    private def unbox(value: Value): Value = value match {
      case BoxV(addr) => parentProcess.pSto.get(addr) match {
        case Some(value) => unbox(value)
        case None => throw error.MteRuntimeErr(
          s"얘! 컴파일쟁이(${parentProcess.pSto})들은 $addr 이런 거 잘 몰라 임마!"
        )
      }
      case _ => value
    }

    private def fnCall(fnExpr: Expr, argExpr: Expr): Value = pret(fnExpr) match {
      case CloV(argName, fExpr, fEnv) =>
        val argV: Value = pret(argExpr) |> unbox
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
        case BinaryOp(lhs, rhs, _, op) => op(pret(lhs) |> unbox, pret(rhs) |> unbox)
        case Id(name) => pEnv.get(name) match {
          case Some(value) => value
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
          val addr: Addr = parentProcess.giveNextAddr
          parentProcess.pSto += (addr -> initV)
          BoxV(addr)
        case SetBox(ref, setExpr) =>
          val setVal = pret(setExpr) |> unbox
          pret(ref) match {
            case BoxV(addr) =>
              parentProcess.pSto += (addr -> setVal)
              setVal
            case err@_ => throw error.MteRuntimeErr(
              s"얘! 지금 네 눈에 $err 이게 포인터로 보이니? env=$pEnv, sto=${parentProcess.pSto}"
            )
          }
        case Try(exprTry) =>
          try {
            pret(exprTry)
            NumV(1)
          } catch {
            case _: error.MteRuntimeErr => NumV(0)
          }
        case BuiltinFnE2E(fn, arg, _) => pret(fn(arg))
        case BuiltinFnV2V(fn, arg, _) => fn(pret(arg))
      }
    }
  }

  val unitE: UnitE = UnitE()
  val unitV: UnitV = UnitV()

  package sugarbuilder {
    def exprToFn(expr: Expr): Expr = Fun("", "_", expr)

    def newScope(expr: Expr): Expr = App(exprToFn(expr), unitE)

    def makePrintExpr(expr: Expr, template: String = "%s"): Expr =
      BuiltinFnV2V(v => builtin.write(v, template), expr, "print")

    def newFor(iterName: String, initExpr: Expr, condExpr: Expr, manipulationExpr: Expr, inExpr: Expr): Expr = {
      Seq(
        ValDef(iterName, NewBox(initExpr)),
        WhileN0(condExpr, Seq(
          inExpr,
          manipulationExpr
        ))
      )
    }

    def newSimpleFor(iterName: String, lbdInclusive: Expr, ubdExclusive: Expr, inExpr: Expr): Expr = newFor(
      iterName = iterName,
      initExpr = lbdInclusive,
      condExpr = BinaryOp(ubdExclusive, Id(iterName), "Gt", ops.valGt),
      manipulationExpr = SetBox(Id(iterName), Id(iterName) + 1),
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

  implicit def toId(name: String): Expr = Id(name)
  implicit def toNum(num: Int): Expr = Num(num)

  implicit class NumExpansion(data: Num) {
    @unused def 뭉: Num = Num(2 * data.data + 1)
    @unused def 탱: Num = Num(2 * data.data)
    @unused def 뭉뭉: Num = Num(4 * data.data + 3)
    @unused def 뭉탱: Num = Num(4 * data.data + 2)

    @unused
    def 탱뭉: Num = Num(4 * data.data + 1)

    @unused
    def 탱탱: Num = Num(4 * data.data)

    @unused
    def 뭉뭉뭉: Num = Num(8 * data.data + 7)

    @unused
    def 뭉뭉탱: Num = Num(8 * data.data + 6)

    @unused
    def 뭉탱뭉: Num = Num(8 * data.data + 5)

    @unused
    def 뭉탱탱: Num = Num(8 * data.data + 4)

    @unused
    def 탱뭉뭉: Num = Num(8 * data.data + 3)

    @unused
    def 탱뭉탱: Num = Num(8 * data.data + 2)

    @unused
    def 탱탱뭉: Num = Num(8 * data.data + 1)

    @unused
    def 탱탱탱: Num = Num(8 * data.data)
  }

  class ExprOps(lhs: Expr) {
    @unused def 배(rhs: Expr): Expr = BinaryOp(lhs, rhs, "Add", ops.valAdd)

    @unused def 코(rhs: Expr): Expr = BinaryOp(lhs, rhs, "Sub", ops.valSub)

    @unused def 조이고(rhs: Expr): Expr = BinaryOp(lhs, rhs, "Mul", ops.valMul)

    @unused def 법회(rhs: Expr): Expr = BinaryOp(lhs, rhs, "Div", ops.valDiv)

    @unused def 릴(rhs: Expr): Expr = App(rhs, lhs)

    @unused def 리액션(template: String): Expr = sugarbuilder.makePrintExpr(lhs, template)

    @unused def 케바바바밥줘(rhs: Expr): Expr = Seq(lhs, rhs)

    @unused def 꼽표(@unused rhs: EndState3): Expr = BinaryOp(lhs, Num(0), "LogNot", ops.valLogNot)
  }

  @unused
  @targetName("bitNotBitNot")
  val ~~ = EndState3()

  @unused
  val 게이조이고 = EndState4()

  implicit class ExprOpsExtByExpr(lhs: Expr) extends ExprOps(lhs)
  implicit class ExprOpsExtByStr(lhs: String) extends ExprOps(Id(lhs))
  implicit class ExprOpsExtByNum(lhs: Int) extends ExprOps(Num(lhs))

  class EndState
  class EndState1 extends EndState
  class EndState2 extends EndState
  class EndState3 extends EndState
  class EndState4 extends EndState


  @unused val 뭉: Num = Num(1)
  @unused val 탱: Num = Num(0)
  @unused val 뭉뭉: Num = Num(3)
  @unused val 뭉탱: Num = Num(2)
  @unused val 뭉뭉뭉: Num = Num(7)
  @unused val 뭉뭉탱: Num = Num(6)
  @unused val 뭉탱뭉: Num = Num(5)
  @unused val 뭉탱탱: Num = Num(4)

  @unused val 스키비야: UnitE = unitE
  @unused val 스킵이야: UnitE = unitE

  def 춘잣: ProgramBuilder = {
    val process: Process = Process(Map())
    val program: Program = Program(process, ProcessFn(process, Map()))
    ProgramBuilder(program)
  }

  case class ProgramBuilder(program: Program) {
    @targetName("fact")
    @unused
    def ! (expr: Expr) : ProgramBuilder = factHelper(expr)

    @unused
    def 케바바바밥줘(expr: Expr): ProgramBuilder = factHelper(expr)

    private def factHelper(expr: Expr): ProgramBuilder = {
      val result: Value = program.mainFn.pret(expr)
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

  // 를! "x"
  @unused
  case object 를 {
    @targetName("fact")
    @unused
    def !(valName: String): Expr = Id(valName)
  }

  /**
   * 주어진 조건이 유링게슝하면 앞부분을, 유링게슝하지 않으면 뒷부분을 실행한단다. 뒷부분은 안돼 임마!
   * 문법: (유링게슝한?) (expr) {expr} 안유링게슝 {expr}
   */
  @unused
  case object 유링게슝한 {
    @targetName("question")
    @unused
    def ?(condExpr: Expr)(trueExpr: Expr): Ifn0Builder =
      Ifn0Builder(condExpr, trueExpr)

    case class Ifn0Builder(condExpr: Expr, trueExpr: Expr) {
      @unused
      def 안유링게슝(falseExpr: Expr): Expr = IfN0(condExpr, trueExpr, falseExpr)
    }
  }

  /**
   * 얘! 변수 사용이 잘 안 되니? 이걸로 변수를 정의해 보렴!
   * 문법: 아니 자기가 (id)라는 사람인데 {expr}을 했대
   */
  @unused val 아니: ValBuilderInit = ValBuilderInit()
  /**
   * 얘! 변수 사용이 잘 안 되니? 이걸로 변수를 정의해 보렴!
   * 문법: 아니 자기가 (id)라는 사람인데 {expr}을 했대
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

  implicit class LambdaBuilder(argId: String) {
    @unused
    def 는(fnExpr: Expr): LambdaBuilder2 = LambdaBuilder2(argId, fnExpr)

    @unused
    def 은(fnExpr: Expr): LambdaBuilder2 = LambdaBuilder2(argId, fnExpr)

    class LambdaBuilder2(argId: String, fnExpr: Expr) {
      @unused
      def 다(@unused joyGo: EndState4): Expr = Fun("", argId, fnExpr)

      @unused
      def 이다(@unused joyGo: EndState4): Expr = Fun("", argId, fnExpr)
    }
  }



  /**
   * 케인인님 함수호출 한판해요
   * 문법: (f)아/야 (arg) 먹어라??
   * @param f 호출할 함수
   */
  implicit class AppBuilder1(f: Expr) {
    def 아(arg: Expr): AppBuilder2 =
      AppBuilder2(f, arg)

    @unused
    def 야(arg: Expr): AppBuilder2 =
      AppBuilder2(f, arg)
  }

  implicit class AppBuilder0(name: String) {
    def 아(arg: Expr): AppBuilder2 =
      AppBuilder2(Id(name), arg)

    def 야(arg: Expr): AppBuilder2 =
      AppBuilder2(Id(name), arg)
  }

  case class AppBuilder2(f: Expr, arg: Expr) {
    def 먹어라(@unused x: EndState2): Expr =
      App(f, arg)
  }

  /**
   * 케인님이 조건문이 거짓이 되기 전까지 강제연결을 해 주신단다!
   * 문법: 강제연결 (cond) {expr}
   * @param cond 조건문
   * @param exprIn 조건문이 참인 동안 실행할 명령
   */
  @unused
  def 강제연결(cond: Expr)(exprIn: Expr): Expr = {
    WhileN0(cond, exprIn)
  }

  /**
   * 케인인님이 11수의 경험을 살려 해당 문장을 원하는 만큼 실행시켜 준단다!
   * 문법: ((정수)수) (iterName) {expr}
   */
  @unused
  class SimpleForBuilder(n: Expr) {
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

  /**
   * 도네 금액이 케인인님의 마음에 들지 판단한단다.
   * 문법: (lhs) 돈 (rhs) 원에??
   * @param lhs lhs
   */
  @unused
  implicit class GtBuilder(lhs: Expr) {
    @unused
    def 돈(rhs: Expr): GtBuilder2 =
      GtBuilder2(lhs, rhs)
  }

  @unused
  case class GtBuilder2(lhs: Expr, rhs: Expr) {
    @unused
    def 원에(@unused x: EndState2): Expr =
      BinaryOp(lhs, rhs, "Gt", ops.valGt)
  }

  @unused
  implicit class GtBuilder0(lhs: String) {
    @unused
    def 돈(rhs: Expr): GtBuilder2 =
      GtBuilder2(Id(lhs), rhs)
  }

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
    def 맞어(expr: Expr): Expr = {
      IfN0(expr, unitE, BuiltinFnE2E(utility.makeKillFn(
        s"얘! $expr 이게 truthy 한 값이 되겠니??"
      ), unitE, "kill"))
    }
  }

  @unused
  implicit class BoxBuilder(expr: Expr) {
    @unused
    def 발행(@unused nft: NFT.type): Expr = NewBox(expr)
  }

  @unused
  implicit class BoxBuilderFromID(name: String) {
    @unused
    def 발행(@unused nft: NFT.type): Expr = NewBox(Id(name))
  }

  @unused
  implicit class BoxBuilderFromNumber(num: Int) {
    @unused
    def 발행(@unused nft: NFT.type): Expr = NewBox(Num(num))
  }

  case object NFT

  class SetBoxBuilder(box: Expr) {
    @unused def 게(@unused x: 그런.type): SetBoxBuilder2 = SetBoxBuilder2(box)
    @unused def 니게(@unused x: 그런.type): SetBoxBuilder2 = SetBoxBuilder2(box)
  }

  implicit class PtrSetBuilderFromExpr(box: Expr) extends SetBoxBuilder(box)

  implicit class PtrSetBuilderFromStr(name: String) extends SetBoxBuilder(Id(name))

  case class SetBoxBuilder2(ptr: Expr) {
    @unused def 사람이(setExpr: Expr): SetBoxBuilder3 = SetBoxBuilder3(ptr, setExpr)
  }

  case class SetBoxBuilder3(ptr: Expr, setExpr: Expr) {
    @unused def 일(@unused x: 순없는지.type): Expr = SetBox(ptr, setExpr)

    @unused def 힐(@unused x: 순없는지.type): Expr = SetBox(ptr, setExpr)
  }

  @unused case object 그런
  @unused case object 순없는지

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
  def 리액션(expr: Expr, template: String = "%s"): Expr = sugarbuilder.makePrintExpr(expr, template)

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
      for (i <- 0 to n) {
        str += rand.nextPrintableChar()
      }
      str
    }


    /**
     * 프로그램이 내부적으로 사용하는 변수 이름은 이 함수가 부여해 준단다.
     * @return 변수 이름
     */
    def randomNameGen(): String =
      s"%rsvd%_${randomStringGen(21)}%_%"

    def randBetween(lbdInclusive: Int, ubdExclusive: Int): Int =
      rand.between(lbdInclusive, ubdExclusive)

    @unused
    def leInt(lhs: BigInt, rhs: BigInt): Int =
      if (lhs <= rhs) 1 else 0

    /**
     * 얘! 내가 이런 함수까지 일일이 다 주석을 달아야 되니? 귀찮아!
     * @param lhs 아 귀찮아~~~!!!!!
     * @param rhs 아 귀찮아~~~!!!!!
     * @return
     */
    def gtInt(lhs: BigInt, rhs: BigInt): BigInt =
      if (lhs > rhs) 1 else 0
      
    def logNot(lhs: BigInt, @unused rhs: BigInt): BigInt =
      if (lhs == 0) 1 else 0

    def makeKillFn(msg: String): Expr => Expr = {
      def ret(unitE: Expr): Expr =
        assert(false, msg)
        unitE

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
  }
}