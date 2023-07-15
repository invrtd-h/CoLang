package mte {
  import scala.annotation.targetName
  import scala.annotation.unused
  import scala.language.implicitConversions
  import scala.util.Random

  @unused
  private[mte] sealed trait Expr {
    @unused
    def 배(rhs: Expr): Expr = BinaryOp(this, rhs, "Add", Ops.add.apply)

    @unused
    def 코(rhs: Expr): Expr = BinaryOp(this, rhs, "Sub", Ops.sub.apply)

    @unused
    def 조이고(rhs: Expr): Expr = BinaryOp(this, rhs, "Mul", Ops.mul.apply)

    @unused
    def 법회(rhs: Expr): Expr = BinaryOp(this, rhs, "Div", Ops.div.apply)

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
    def +(rhs: Expr): Expr = BinaryOp(this, rhs, "plus", Ops.add.apply)

    @unused
    @targetName("minus")
    def -(rhs: Expr): Expr = BinaryOp(this, rhs, "minus", Ops.sub.apply)
  }

  private[mte] sealed trait Value

  private[mte] type Env = Map[String, Value]
  private[mte] type Addr = Int
  private[mte] type Sto = Map[Addr, Value]

  // Expressions
  @unused
  private[mte] case class Num(data: BigInt) extends Expr {
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

  private[mte] case class BinaryOp(lhs: Expr, rhs: Expr, name: String, op: (=> Value, => Value) => Value) extends Expr {
    override def toString: String =
      s"BO[$name]($lhs, $rhs)"
  }

  private[mte] case class UnaryOp(x: Expr, op: (=> Value) => Value) extends Expr

  private[mte] case class Id(name: String) extends Expr

  private[mte] case class ValDef(valName: String, initExpr: Expr) extends Expr

  private[mte] case class Fun(funName: String, argName: String, fExpr: Expr) extends Expr

  private[mte] case class App(fnExpr: Expr, argExpr: Expr) extends Expr
  private[mte] case class AppBuiltin(fn: Value => Value, arg: Expr) extends Expr

  private[mte] case class Seq(lhs: Expr, rhs: Expr) extends Expr

  private[mte] case class IfN0(cond: Expr, exprTrue: Expr, exprFalse: Expr) extends Expr

  private[mte] case class WhileN0(cond: Expr, exprIn: Expr) extends Expr

  private[mte] case class Print(exprPrint: Expr, template: String) extends Expr
  
  private[mte] case class UnitE() extends Expr
  
  private[mte] case class Pair(firstExpr: Expr, secondExpr: Expr) extends Expr
  
  private[mte] case class NewBox(initExpr: Expr) extends Expr

  // Values
  private[mte] case class UnitV() extends Value {
    override def toString: String = ""
  }

  private[mte] case class NumV(data: BigInt) extends Value {
    override def toString: String = "%s".format(data)
  }

  private[mte] case class CloV(argName: String, fExpr: Expr, var fEnv: Env) extends Value {
    override def toString: String = s"CloV($argName, $fExpr)"
  }

  private[mte] case class PairV(first: Value, second: Value) extends Value
  
  private[mte] case class BoxV(addr: Addr) extends Value

  private[mte] case object Ops {
    @unused
    case class UnaryOpNum(op: BigInt => BigInt, name: String) {
      def apply(x: => Value): Value = x match {
        case NumV(data) => NumV(this.op(data))
        case _ => throw Exception(s"${this.name} failed ### x=$x")
      }
    }

    @unused
    case class BinaryOpNum(op: (BigInt, BigInt) => BigInt, name: String) {
      def apply(lhs: => Value, rhs: => Value): Value = {
        lhs match {
          case NumV(dataL) => rhs match {
            case NumV(dataR) => NumV(this.op(dataL, dataR))
            case _ => throw Exception(
              s"얘! ${this.name} 여기 지금 $rhs 이게 숫자로 보이니??"
            )
          }
          case _ => throw Exception(
            s"얘! ${this.name} 여기 지금 $lhs 이게 숫자로 보이니??"
          )
        }
      }
    }

    val add: BinaryOpNum = BinaryOpNum(_ + _, "Addition")
    val sub: BinaryOpNum = BinaryOpNum(_ - _, "Subtraction")
    val mul: BinaryOpNum = BinaryOpNum(_ * _, "Multiplication")
    val div: BinaryOpNum = BinaryOpNum(_ / _, "Division")
    val le: BinaryOpNum = BinaryOpNum(mteUtil.leInt, "Le")
    val gt: BinaryOpNum = BinaryOpNum(mteUtil.gtInt, "Gt")
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
        case AppBuiltin(fn, arg) =>
          fn(pret(arg))
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

  private[mte] case class ProgramBuilder(var process: Process) {
    @targetName("fact")
    def ! : Program =
      Program(process, ProcessFunc(process, Map()))
  }
  case class Program(process: Process, mainFn: ProcessFunc) {
    def apply(expr: Expr): Program = {
      val result: Value = mainFn.pret(expr)
      println(result)
      this
    }

    def 케바바바밥줘: Expr => Program = apply
  }

  private[mte] class EndState

  @targetName("endState")
  @unused
  val ~! = EndState()

  private[mte] class EndState2

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

  // 유링게슝한.? (val) {stmt} 안유링게슝 {stmt}
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

  // ValDef
  // 아니 자기가 (x) 라는사람인데 () 를 했대
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

  // Fun
  // 아~! ()는 ()이 참 좋구나~!

  case object 아 {
    @unused
    @targetName("notFact")
    def ~!(funName: String, argName: String): FunBuilder =
      FunBuilder(funName, argName)

    @unused
    @targetName("notFact")
    def ~!(argName: String): FunBuilder =
      FunBuilder(mteUtil.randomNameGen(), argName)

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

  // App
  // ()아 () 먹어라??
  implicit class AppBuilder1(f: Expr) {
    def 아(arg: Expr): AppBuilder2 =
      AppBuilder2(f, arg)
  }

  case class AppBuilder2(f: Expr, arg: Expr) {
    def 먹어라(@unused x: EndState2): Expr =
      App(f, arg)
  }

  implicit class AppBuilder0(name: String) {
    def 아(arg: Expr): AppBuilder2 =
      AppBuilder2(Id(name), arg)
  }

  // (11수) (i) {}
  implicit class BasicForHelper(n: Int) {
    @unused
    def 수(iterName: String)(forExpr: Expr): Expr =
      Seq(ValDef(iterName, Num(0)), WhileN0(
        Id(iterName) - Num(n),
        Seq(forExpr, ValDef(iterName, Id(iterName) + Num(1)))
      ))
  }

  // gt
  // () 돈 ()원에??
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
      BinaryOp(lhs, rhs, "Gt", Ops.gt.apply)
  }

  @unused
  implicit class GtBuilder0(lhs: String) {
    @unused
    def 돈(rhs: Expr): GtBuilder2 =
      GtBuilder2(Id(lhs), rhs)
  }

  // semantic box 생성
  // 박스 아저씨 {}
  def makeNewScope(expr: Expr): Expr =
    App(Fun(mteUtil.randomNameGen(), mteUtil.randomNameGen(), expr), Num(0))

  @unused
  case object 박스 {
    @unused
    def 아저씨(expr: Expr): Expr =
      makeNewScope(expr)
  }

  // utility package
  package mteUtil {
    val rand: Random = new Random()
    @unused
    def randomNameGen(): String =
      s"%reserved%_${rand.nextLong()}%_%${rand.nextLong()}"

    def leInt(lhs: BigInt, rhs: BigInt): Int =
      if (lhs <= rhs) 1 else 0

    def gtInt(lhs: BigInt, rhs: BigInt): Int =
      if (lhs > rhs) 1 else 0
  }
}