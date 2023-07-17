package mte {
  import scala.annotation.targetName
  import scala.annotation.unused
  import scala.language.implicitConversions
  import scala.util.Random
  import scala.util.chaining.*

  @unused
  sealed trait Expr {
    @unused
    def 배(rhs: Expr): Expr = BinaryOp(this, rhs, "Add", ops.valAdd)

    @unused
    def 코(rhs: Expr): Expr = BinaryOp(this, rhs, "Sub", ops.valSub)

    @unused
    def 조이고(rhs: Expr): Expr = BinaryOp(this, rhs, "Mul", ops.valMul)

    @unused
    def 법회(rhs: Expr): Expr = BinaryOp(this, rhs, "Div", ops.valDiv)

    @unused
    def 릴(rhs: Expr): Expr = App(rhs, this)

    @unused
    def 리액션(template: String): Expr = sugarbuilder.makePrintExpr(this, template)

    @unused
    def 케바바바밥줘(rhs: Expr): Expr = Seq(this, rhs)

    @targetName("plus")
    def +(rhs: Expr): Expr = BinaryOp(this, rhs, "plus", ops.valAdd)

    @targetName("minus")
    def -(rhs: Expr): Expr = BinaryOp(this, rhs, "minus", ops.valSub)
  }

  sealed trait Value

  type Env = Map[String, Addr]
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

  private case class Ptr(expr: Expr) extends Expr

  private case class SetPtr(ptr: Expr, setExpr: Expr) extends Expr

  private case class Try(exprTry: Expr) extends Expr

  private case class BuiltinFnE2E(fn: Expr => Expr, arg: Expr, name: String) extends Expr {
    override def toString: String = s"BF<$name>($arg)"
  }

  private case class BuiltinFnV2V(fn: Value => Value, arg: Expr) extends Expr


  // Values
  private case class UnitV() extends Value {
    override def toString: String = ""
  }

  private case class NumV(data: BigInt) extends Value {
    override def toString: String = "%s".format(data)
  }

  private case class CloV(argName: String, fExpr: Expr, var fEnv: Env) extends Value {
    override def toString: String = s"CloV($argName, $fExpr)"
  }

  private case class PtrV(addr: Addr) extends Value {
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


    val valAdd = bigIntOpToValueOp(_ + _)
    val valSub = bigIntOpToValueOp(_ - _)
    val valMul = bigIntOpToValueOp(_ * _)
    val valDiv = bigIntOpToValueOp(_ / _)
    val valGt  = bigIntOpToValueOp(utility.gtInt)
  }

  @unused
  case class Process(var pSto: Sto, private var nextAddr: Addr = 0) {
    @unused
    def pret(expr: Expr): Value = {
      val func = ProcessFunc(this, Map())
      func.pret(expr)
    }

    def giveNextAddr: Addr = {
      nextAddr += 1
      nextAddr
    }
  }

  case class ProcessFunc(parentProcess: Process, var pEnv: Env) {
    def pret(expr: Expr): Value = {
      expr match {
        case Num(data) => NumV(data)
        case UnitE() => UnitV()
        case BinaryOp(lhs, rhs, _, op) => op(pret(lhs), pret(rhs))
        case Id(name) => pEnv.get(name) match {
          case Some(addr) => parentProcess.pSto.get(addr) match {
            case Some(value) => value
            case _ => throw error.MteRuntimeErr(
              s"얘! 컴파일쟁이(${parentProcess.pSto})들은 주소값 $addr 이런 거 잘 몰라 임마!"
            )
          }
          case _ => throw error.MteRuntimeErr(
            s"얘! 컴파일쟁이($pEnv)들은 $name 이런 거 잘 몰라 임마!"
          )
        }
        case ValDef(valName, initExpr) =>
          if (valName.contains("킹") || valName.contains("갓")) {
            throw error.MteRuntimeErr("내가 킹하고 갓하고 함부로 막 붙이지 말라 그랬지!!")
          }
          val initV: Value = pret(initExpr)
          val addr: Addr = parentProcess.giveNextAddr
          pEnv += (valName -> addr)
          parentProcess.pSto += (addr -> initV)
          UnitV()
        case Fun(funName, argName, fExpr) =>
          val ret: CloV = CloV(argName, fExpr, pEnv)
          val addr: Addr = parentProcess.giveNextAddr
          parentProcess.pSto += (addr -> ret)
          ret.fEnv += (funName -> addr)
          ret
        case App(fnExpr, argExpr) => pret(fnExpr) match {
          case CloV(argName, fExpr, fEnv) =>
            val argV: Value = pret(argExpr)
            val addr: Addr = parentProcess.giveNextAddr
            parentProcess.pSto += (addr -> argV)
            val newFunc: ProcessFunc = ProcessFunc(parentProcess, fEnv + (argName -> addr))
            newFunc.pret(fExpr)
          case err@_ => throw error.MteRuntimeErr(
            s"얘! 지금 $err 이게 함수로 보이니?"
          )
        }
        case Seq(lhs, rhs) =>
          pret(lhs); pret(rhs)
        case IfN0(cond, exprTrue, exprFalse) =>
          pret(cond) match {
            case NumV(data) =>
              if (data != 0) pret(exprTrue) else pret(exprFalse)
            case err@_ => throw error.MteRuntimeErr(
              s"얘! 지금 $err 이게 조건문 안에 들어갈 수 있겠니?? 죽여벌랑"
            )
          }
        case WhileN0(cond, exprIn) =>
          val act = (condExpr: Expr) => {
            pret(condExpr) match {
              case NumV(data) => data
              case err@_ => throw error.MteRuntimeErr(
                s"얘! 지금 $err 이게 조건문 안에 들어갈 수 있겠니?? 죽여벌랑"
              )
            }
          }
          var condVal = act(cond)
          while (condVal != 0) {
            pret(exprIn)
            condVal = act(cond)
          }
          UnitV()
        case Ptr(expr) => expr match {
          case Id(id) => pEnv.get(id) match {
            case Some(addr) => PtrV(addr)
            case None => throw error.MteRuntimeErr(
              s"얘! 컴파일쟁이($pEnv)들은 $id 이런 거 잘 몰라 임마!"
            )
          }
          case _ => throw error.MteRuntimeErr(
            s"얘! $expr 지금 이게 lvalue가 되겠니??"
          )
        }
        case SetPtr(ptr, setExpr) =>
          val setVal = pret(setExpr)
          pret(ptr) match {
            case PtrV(addr) =>
              parentProcess.pSto += (addr -> setVal)
              setVal
            case err@_ => throw error.MteRuntimeErr(
              s"얘! 지금 네 눈에 $err 이게 포인터로 보이니?"
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
        case BuiltinFnV2V(fn, arg) => fn(pret(arg))
      }
    }
  }

  package sugarbuilder {
    def makeNewScope(expr: Expr): Expr =
      App(Fun(utility.randomNameGen(), utility.randomNameGen(), expr), UnitE())

    def makePrintExpr(expr: Expr, template: String = "%s"): Expr =
      BuiltinFnV2V(v => builtin.write(v, template), expr)

    def makeScopedWhile(condExpr: Expr, inExpr: Expr): Expr = makeNewScope(WhileN0(condExpr, inExpr))


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

  /**
   * 옛날 버전에서는 를! 함수를 사용해서 명시적으로 변수에 들어 있는 값을
   * 꺼내와야 했다 맨이야! 이제는 그럴 필요가 없다맨
   * @param name 변수명
   * @return 그 아이디에 저장된 값
   */
  implicit def toId(name: String): Expr = Id(name)
  implicit def toNum(num: Int): Expr = Num(num)

  implicit class NumExpansion(data: Num) {
    @unused
    def 뭉: Num = Num(2 * data.data + 1)

    @unused
    def 탱: Num = Num(2 * data.data)

    @unused
    def 뭉뭉: Num = Num(4 * data.data + 3)

    @unused
    def 뭉탱: Num = Num(4 * data.data + 2)

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

  @unused val 뭉: Num = Num(1)
  @unused val 탱: Num = Num(0)
  @unused val 뭉뭉: Num = Num(3)
  @unused val 뭉탱: Num = Num(2)
  @unused val 뭉뭉뭉: Num = Num(7)
  @unused val 뭉뭉탱: Num = Num(6)
  @unused val 뭉탱뭉: Num = Num(5)
  @unused val 뭉탱탱: Num = Num(4)

  @unused val 스키비야: UnitE = UnitE()
  @unused val 스킵이야: UnitE = UnitE()

  def 춘잣: ProgramBuilder =
    ProgramBuilder(Process(Map()))

  case class ProgramBuilder(var process: Process) {
    @targetName("fact")
    def ! : Program =
      Program(process, ProcessFunc(process, Map()))
  }
  case class Program(process: Process, mainFn: ProcessFunc) {
    def apply(expr: Expr): Program = {
      val result: Value = mainFn.pret(expr)
      this
    }

    @unused
    def 케바바바밥줘: Expr => Program = apply
  }

  case class EndState()

  @targetName("endState")
  @unused
  val ~! = EndState()

  case class EndState2()

  @targetName("endState2")
  @unused
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
   * 함수를 정의한단다. 함수 리터럴이라 다른 함수에 인자로 넘겨줄 수도 있고 정의한 즉시 쓸 수도 있단다.
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
      FunBuilder(utility.randomNameGen(), argName)

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
  implicit class BasicForBuilder(n: Int) {
    @unused
    def 수(iterName: String)(forExpr: Expr): Expr =
      sugarbuilder.makeNewScope(
        Seq(ValDef(iterName, Num(0)), WhileN0(
          Id(iterName) - Num(n),
          Seq(forExpr, ValDef(iterName, Id(iterName) + Num(1)))
        ))
      )
  }

  implicit class ForBuilder(time: Expr) {
    @unused
    def 수(iterName: String)(forExpr: Expr): Expr =
      sugarbuilder.makeNewScope(
        Seq(ValDef(iterName, Num(0)), WhileN0(
          Id(iterName) - time,
          Seq(forExpr, ValDef(iterName, Id(iterName) + Num(1)))
        ))
      )
  }

  implicit class IdForBuilder(id: String) {
    @unused
    def 수(iterName: String)(forExpr: Expr): Expr =
      sugarbuilder.makeNewScope(
        Seq(ValDef(iterName, Num(0)), WhileN0(
          Id(iterName) - Id(id),
          Seq(forExpr, ValDef(iterName, Id(iterName) + Num(1)))
        ))
      )
  }

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
      sugarbuilder.makeNewScope(expr)
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
      IfN0(expr, UnitE(), BuiltinFnE2E(utility.makeKillFn(
        s"얘! $expr 이게 truthy 한 값이 되겠니??"
      ), UnitE(), "kill"))
    }
  }

  implicit class PtrBuilder(expr: Expr) {
    @unused
    def 발행(@unused nft: NFT.type): Expr = Ptr(expr)
  }

  implicit class PtrBuilderFromStr(name: String) {
    @unused
    def 발행(@unused nft: NFT.type): Expr = Ptr(Id(name))
  }

  case object NFT
  
  implicit class PtrSetBuilder(ptr: Expr) {
    @unused def 게(@unused x: 그런.type): PtrSetBuilder2 = PtrSetBuilder2(ptr)
    
    @unused def 니게(@unused x: 그런.type): PtrSetBuilder2 = PtrSetBuilder2(ptr)
    
  }
  
  implicit class PtrSetBuilderFromStr(name: String) {
    @unused def 게(@unused x: 그런.type): PtrSetBuilder2 = PtrSetBuilder2(Id(name))

    @unused def 니게(@unused x: 그런.type): PtrSetBuilder2 = PtrSetBuilder2(Id(name))
  }

  case class PtrSetBuilder2(ptr: Expr) {
    @unused def 사람이(setExpr: Expr): PtrSetBuilder3 = PtrSetBuilder3(ptr, setExpr)
  }

  case class PtrSetBuilder3(ptr: Expr, setExpr: Expr) {
    @unused def 일(@unused x: 순없는지.type): Expr = SetPtr(ptr, setExpr)

    @unused def 힐(@unused x: 순없는지.type): Expr = SetPtr(ptr, setExpr)
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
    /**
     *
     */
    private val rand: Random = new Random()

    /**
     * 프로그램이 내부적으로 사용하는 변수 이름은 이 함수가 부여해 준단다.
     * @return 변수 이름
     */
    def randomNameGen(): String =
      s"%reserved%_${rand.nextString(21)}%_%"

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
    @unused
    def gtInt(lhs: BigInt, rhs: BigInt): Int =
      if (lhs > rhs) 1 else 0

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