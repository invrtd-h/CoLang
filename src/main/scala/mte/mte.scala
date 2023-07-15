package mte {
  import scala.annotation.targetName
  import scala.annotation.unused
  import scala.language.implicitConversions
  import scala.util.Random
  import scala.util.chaining._

  @unused
  private sealed trait Expr {
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
    def 게이(template: String): Expr = Print(this, template)

    @unused
    def 케바바바밥줘(rhs: Expr): Expr = Seq(this, rhs)

    @unused
    def 겸상(rhs: Expr): Expr = Pair(this, rhs)

    @unused
    @targetName("plus")
    def +(rhs: Expr): Expr = BinaryOp(this, rhs, "plus", ops.valAdd)

    @unused
    @targetName("minus")
    def -(rhs: Expr): Expr = BinaryOp(this, rhs, "minus", ops.valSub)
  }

  private sealed trait Value

  private type Env = Map[String, Value]
  private type Addr = Int
  private type Sto = Map[Addr, Value]

  // Expressions
  @unused
  private case class Num(data: BigInt) extends Expr {
    @unused
    def 뭉: Num = Num(2 * data + 1)

    @unused
    def 탱: Num = Num(2 * data)

    @unused
    def 뭉뭉: Num = Num(4 * data + 3)

    @unused
    def 뭉탱: Num = Num(4 * data + 2)

    @unused
    def 탱뭉: Num = Num(4 * data + 1)

    @unused
    def 탱탱: Num = Num(4 * data)

    @unused
    def 뭉뭉뭉: Num = Num(8 * data + 7)

    @unused
    def 뭉뭉탱: Num = Num(8 * data + 6)

    @unused
    def 뭉탱뭉: Num = Num(8 * data + 5)

    @unused
    def 뭉탱탱: Num = Num(8 * data + 4)

    @unused
    def 탱뭉뭉: Num = Num(8 * data + 3)

    @unused
    def 탱뭉탱: Num = Num(8 * data + 2)

    @unused
    def 탱탱뭉: Num = Num(8 * data + 1)

    @unused
    def 탱탱탱: Num = Num(8 * data)
  }

  private case class BinaryOp(lhs: Expr, rhs: Expr, name: String, op: (=> Value, => Value) => Value) extends Expr {
    override def toString: String =
      s"BO[$name]($lhs, $rhs)"
  }

  private case class UnaryOp(x: Expr, op: (=> Value) => Value) extends Expr

  private case class Id(name: String) extends Expr

  private case class ValDef(valName: String, initExpr: Expr) extends Expr

  private case class Fun(funName: String, argName: String, fExpr: Expr) extends Expr

  private case class App(fnExpr: Expr, argExpr: Expr) extends Expr

  private case class BuiltinCode(fn: () => Expr) extends Expr

  private case class Seq(lhs: Expr, rhs: Expr) extends Expr

  private case class IfN0(cond: Expr, exprTrue: Expr, exprFalse: Expr) extends Expr

  private case class WhileN0(cond: Expr, exprIn: Expr) extends Expr

  private case class Print(exprPrint: Expr, template: String) extends Expr

  private case class UnitE() extends Expr

  private case class Pair(firstExpr: Expr, secondExpr: Expr) extends Expr

  private case class NewBox(initExpr: Expr) extends Expr
  private case class OpenBox(box: Expr) extends Expr
  private case class SetBox(box: Expr, assignExpr: Expr) extends Expr

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

  private case class PairV(first: Value, second: Value) extends Value

  private case class BoxV(addr: Addr) extends Value

  package ops {
    def bigIntOpToValueOp(op: (BigInt, BigInt) => BigInt): (=> Value, => Value) => Value =
      def ret(lhs: => Value, rhs: => Value): Value = {
        lhs match {
          case NumV(dataL) => rhs match {
            case NumV(dataR) => NumV(op(dataL, dataR))
            case _ => throw Exception(
              s"얘! 여기 지금 $rhs 이게 숫자로 보이니??"
            )
          }
          case _ => throw Exception(
            s"얘! 여기 지금 $lhs 이게 숫자로 보이니??"
          )
        }
      }
      ret

    val valAdd = bigIntOpToValueOp(_ + _)
    val valSub = bigIntOpToValueOp(_ - _)
    val valMul = bigIntOpToValueOp(_ * _)
    val valDiv = bigIntOpToValueOp(_ / _)
    val valGt = bigIntOpToValueOp(utility.gtInt)
  }

  @unused
  case class Process(var pSto: Sto) {
    @unused
    def pret(expr: Expr): Value =
      val func = ProcessFunc(this, Map())
      func.pret(expr)
  }

  case class ProcessFunc(parentProcess: Process, var pEnv: Env) {
    def pret(expr: Expr): Value = {
      expr match {
        case Num(data) => NumV(data)
        case BinaryOp(lhs, rhs, _, op) => op(pret(lhs), pret(rhs))
        case UnaryOp(x, op) => op(pret(x))
        case Id(name) => pEnv.get(name) match {
          case Some(value) => value
          case _ => throw Exception(
            s"얘! 네 눈에 \"$name\"이(가) $pEnv 에 있는 걸로 보이니?"
          )
        }
        case ValDef(valName, initExpr) =>
          pEnv += (valName -> pret(initExpr))
          UnitV()
        case Fun(funName, argName, fExpr) =>
          val ret: CloV = CloV(argName, fExpr, pEnv)
          ret.fEnv += (funName -> ret)
          ret
        case App(fnExpr, argExpr) => pret(fnExpr) match {
          case CloV(argName, fExpr, fEnv) =>
            val argV: Value = pret(argExpr)
            val newFunc: ProcessFunc = ProcessFunc(this.parentProcess, fEnv + (argName -> argV))
            newFunc.pret(fExpr)
          case err@_ => throw Exception(
            s"얘! 지금 $err 이게 함수로 보이니?"
          )
        }
        case BuiltinCode(fn) =>
          pret(fn())
        case Seq(lhs, rhs) =>
          pret(lhs); pret(rhs)
        case IfN0(cond, exprTrue, exprFalse) =>
          pret(cond) match {
            case NumV(data) =>
              if (data != 0) pret(exprTrue) else pret(exprFalse)
            case err@_ => throw Exception(
              s"얘! 지금 $err 이게 조건문 안에 들어갈 수 있겠니?? 죽여벌랑"
            )
          }
        case WhileN0(cond, exprIn) =>
          val act = (condExpr: Expr) => {
            pret(condExpr) match {
              case NumV(data) => data
              case err@_ => throw Exception(
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
        case Print(exprPrint, template) =>
          val value = pret(exprPrint)
          print(template.format(value))
          value
        case UnitE() => UnitV()
        case Pair(firstExpr, secondExpr) =>
          PairV(pret(firstExpr), pret(secondExpr))
        case NewBox(initExpr) =>
          val addr: Addr = parentProcess.pSto.keys.maxOption.getOrElse(0) + 1
          parentProcess.pSto += (addr -> pret(initExpr))
          BoxV(addr)
        case OpenBox(box) =>
          pret(box) match {
            case BoxV(addr) => parentProcess.pSto(addr)
            case err@_ => throw Exception(
              s"얘! 지금 $err 이게 빡스로 보이니? 죽여벌랑"
            )
          }
        case SetBox(box, assignExpr) =>
          pret(box) match {
            case BoxV(addr) =>
              val toAssign: Value = pret(assignExpr)
              parentProcess.pSto += (addr -> toAssign)
              toAssign
            case err@_ => throw Exception(
              s"얘! 지금 $err 이게 빡스로 보이니? 죽여벌랑"
            )
          }
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

  /**
   * 옛날 버전에서는 를! 함수를 사용해서 명시적으로 변수에 들어 있는 값을
   * 꺼내와야 했다 맨이야! 이제는 그럴 필요가 없다맨
   * @param name 변수명
   * @return 그 아이디에 저장된 값
   */
  implicit def toId(name: String): Expr = Id(name)

  @unused
  val 뭉: Num = Num(1)
  @unused
  val 탱: Num = Num(0)
  @unused
  val 뭉뭉: Num = Num(3)
  @unused
  val 뭉탱: Num = Num(2)
  @unused
  val 뭉뭉뭉: Num = Num(7)
  @unused
  val 뭉뭉탱: Num = Num(6)
  @unused
  val 뭉탱뭉: Num = Num(5)
  @unused
  val 뭉탱탱: Num = Num(4)

  @unused
  val 스키비야: UnitE = UnitE()
  @unused
  val 스킵이야: UnitE = UnitE()

  def 춘잣 =
    ProgramBuilder(Process(Map()))

  private case class ProgramBuilder(var process: Process) {
    @targetName("fact")
    @unused
    def ! : Program =
      Program(process, ProcessFunc(process, Map()))
  }
  private case class Program(process: Process, mainFn: ProcessFunc) {
    def apply(expr: Expr): Program = {
      val result: Value = mainFn.pret(expr)
      println(result)
      println(mainFn.pEnv)
      this
    }

    @unused
    def 케바바바밥줘: Expr => Program = apply
  }

  private class EndState

  @targetName("endState")
  @unused
  val ~! = EndState()

  private class EndState2

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
  @unused
  case object 아니 {
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
  implicit class BasicForHelper(n: Int) {
    @unused
    def 수(iterName: String)(forExpr: Expr): Expr =
      makeNewScope(
        Seq(ValDef(iterName, Num(0)), WhileN0(
          Id(iterName) - Num(n),
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

  // scope 생성
  // 문법은 아직 모르겠다 맨이야
  @unused
  def makeNewScope(expr: Expr): Expr =
    App(Fun(utility.randomNameGen(), utility.randomNameGen(), expr), Num(0))

  // box 생성
  // 박스 아저씨 ()
  @unused
  case object 박스 {
    @unused
    def 아저씨(expr: Expr): Expr =
      NewBox(expr)
  }

  package stl {
    def readInt(): Expr =
      Num(scala.io.StdIn.readInt)
  }

  /**
   * stdin에서 정수를 읽어온단다.
   * 정수 말고 다른 문자는 이따 쉬는시간에 감사하다고 할게~
   */
  @unused
  val 개입: Expr = BuiltinCode(stl.readInt)

  package utility {
    // 구현에 쓸모 있는 잡탱이들을 모아놓은 라이브러리란다!

    /**
     *
     */
    private val rand: Random = new Random()

    /**
     * 프로그램이 내부적으로 사용하는 변수 이름은 이 함수가 부여해 준단다.
     * @return 변수 이름
     */
    @unused
    def randomNameGen(): String =
      s"%reserved%_${rand.nextLong()}%_%${rand.nextLong()}"

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
  }
}