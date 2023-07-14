package mte

import scala.annotation.targetName
import scala.annotation.unused
import scala.language.implicitConversions
import scala.util.Random

@unused
sealed trait Expr {
  @unused
  def 배(rhs: Expr): Expr = BinaryOp(this, rhs, Ops.add.apply)
  @unused
  def 코(rhs: Expr): Expr = BinaryOp(this, rhs, Ops.sub.apply)
  @unused
  def 조이고(rhs: Expr): Expr = BinaryOp(this, rhs, Ops.mul.apply)
  @unused
  def 법회(rhs: Expr): Expr = BinaryOp(this, rhs, Ops.div.apply)
  @unused
  def 릴(rhs: Expr): Expr = App(rhs, this)
  @unused
  def 게이(template: String): Expr = Print(this, template)
  @unused
  def 케바바바밥줘(rhs: Expr): Expr = Seq(this, rhs)

  @unused
  @targetName("plus")
  def +(rhs: Expr): Expr = BinaryOp(this, rhs, Ops.add.apply)
  @unused
  @targetName("minus")
  def -(rhs: Expr): Expr = BinaryOp(this, rhs, Ops.sub.apply)
}
sealed trait Value
type Env = Map[String, Value]

// Exprs
@unused
case class Num(data: BigInt) extends Expr {
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
case class BinaryOp(lhs: Expr, rhs: Expr, op: (=> Value, => Value) => Value) extends Expr {
  override def toString: String =
    s"BinaryOp($lhs, $rhs)"
}
case class UnaryOp(x: Expr, op: (=> Value) => Value) extends Expr
case class Id(name: String) extends Expr
case class ValDef(valName: String, initExpr: Expr) extends Expr
case class Fun(funName: String, argName: String, fExpr: Expr) extends Expr
case class App(fnExpr: Expr, argExpr: Expr) extends Expr
case class Seq(lhs: Expr, rhs: Expr) extends Expr
case class IfN0(cond: Expr, exprTrue: Expr, exprFalse: Expr) extends Expr
case class WhileN0(cond: Expr, exprIn: Expr) extends Expr
case class Print(exprPrint: Expr, template: String) extends Expr

// Values
case object unitV extends Value {
  override def toString: String = ""
}
case class NumV(data: BigInt) extends Value {
  override def toString: String = "%s".format(data)
}
case class CloV(argName: String, fExpr: Expr, var fEnv: Env) extends Value {
  override def toString: String = s"CloV($argName, $fExpr)"
}

case object Ops {
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
          case _ => throw Exception(s"${this.name} failed ### rhs=$rhs")
        }
        case _ => throw Exception(s"${this.name} failed ### lhs=$lhs")
      }
    }
  }

  val add: BinaryOpNum = BinaryOpNum(_ + _, "Addition")
  val sub: BinaryOpNum = BinaryOpNum(_ - _, "Subtraction")
  val mul: BinaryOpNum = BinaryOpNum(_ * _, "Multiplication")
  val div: BinaryOpNum = BinaryOpNum(_ / _, "Division")
}

@unused
case class Program(var pEnv: Env) {
  def pret(expr: Expr): Value = {
    expr match {
      case Num(data) => NumV(data)
      case BinaryOp(lhs, rhs, op) => op(pret(lhs), pret(rhs))
      case UnaryOp(x, op) => op(pret(x))
      case Id(name) => pEnv.get(name) match {
        case Some(value) => value
        case _ => throw Exception(
          s"얘! 네 눈에 \"$name\"이(가) $pEnv 에 있는 걸로 보이니?"
        )
      }
      case ValDef(valName, initExpr) =>
        pEnv += (valName -> pret(initExpr))
        unitV
      case Fun(funName, argName, fExpr) =>
        val ret: CloV = CloV(argName, fExpr, pEnv)
        ret.fEnv += (funName -> ret)
        ret
      case App(fnExpr, argExpr) => pret(fnExpr) match {
        case CloV(argName, fExpr, fEnv) =>
          val argV: Value = pret(argExpr)
          val program: Program = Program(fEnv + (argName -> argV))
          program.pret(fExpr)
        case err@_ => throw Exception(
          s"얘! 지금 $err 이게 함수로 보이니?"
        )
      }
      case Seq(lhs, rhs) =>
        pret(lhs); pret(rhs)
      case IfN0(cond, exprTrue, exprFalse) =>
        pret(cond) match {
          case NumV(data) =>
            if (data != 0) pret(exprTrue) else pret(exprFalse)
          case _ => throw Exception("err")
        }
      case WhileN0(cond, exprIn) =>
        var condVal = pret(cond) match {
          case NumV(data) => data
          case _ => throw Exception("err")
        }
        while (condVal != 0) {
          pret(exprIn)
          condVal = pret(cond) match {
            case NumV(data) => data
            case _ => throw Exception("err")
          }
        }
        unitV
      case Print(exprPrint, template) =>
        val value = pret(exprPrint)
        print(template.format(value))
        value
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

def 춘잣 =
  new StartProgram(Program(Map()))

class StartProgram(var program: Program) {
  @targetName("fact")
  def ! : Helper =
    Helper(this)
  case class Helper(sp: StartProgram) {
    def apply(expr: Expr): Helper = {
      val result: Value = sp.program.pret(expr)
      println(result)
      this
    }

    def 케바바바밥줘: Expr => Helper = apply
  }
}

class EndState
@targetName("endState")
@unused
val ~! = EndState()

class EndState2
@targetName("endState2")
@unused
val ?? = EndState2()

// 를! "x"
case object 를 {
  @targetName("fact")
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
case object 아니 {
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
    FunBuilder(utils.randomNameGen(), argName)
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
  def 수(iterName: String)(forExpr: Expr): Expr =
    Seq(ValDef(iterName, Num(0)), WhileN0(
      Id(iterName) - Num(n),
      Seq(forExpr, ValDef(iterName, Id(iterName) + Num(1)))
    ))
}

// semantic box 생성
// 박스 아저씨 {}

val rand: Random = new Random()

package utils {
  @unused
  def randomNameGen(): String =
    s"%reserved%_${rand.nextLong()}%_%${rand.nextLong()}"
}