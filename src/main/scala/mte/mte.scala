package mte

import mte.expr.{App, BinaryOp, BoxDef, BoxSet, BuiltinFnE2E, ClassDef, Expr, Fun, HMap, Id, Num, Proj, Seqn, TernaryOp, UnitE, ValDef, Vec, WhileN0}
import mte.ids.{AnonArg, AnonFn1, FnCallOp, StringID, ThisKW, VarID}
import mte.mtetype.{ArrowT, Type, VarT}
import mte.value.{BoxV, UnitV, Value}
import mte.pret.run

import scala.annotation.{tailrec, targetName, unused}
import scala.language.implicitConversions
import scala.util.Random
import scala.util.chaining.*
import utility.Piper

import scala.collection.immutable.Vector

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

case class ValDefFragment(name: StringID, t: Type, expr: Expr) extends CodeFragment

case class BoxDefFragment(name: StringID, t: Type, expr: Expr) extends CodeFragment

case class ClassDefFragment(memberName: Vector[StringID],
                            methods: Map[VarID, Expr],
                            id: StringID) extends CodeFragment

def joinFragments(fragments: Vector[CodeFragment]): Expr = {
  if (fragments.length == 1) {
    fragments.head match {
      case CodeFragmentGeneral(expr) => expr
      case ValDefFragment(name, t, expr) => ValDef(name, t, expr, UnitE())
      case BoxDefFragment(name, t, expr) => BoxDef(name, t, expr, UnitE())
      case ClassDefFragment(memberName, methods, id) => ClassDef(memberName, methods, id, UnitE())
    }
  } else {
    fragments.head match
      case CodeFragmentGeneral(expr) => Seqn(expr, joinFragments(fragments.tail))
      case ValDefFragment(name, t, expr) => mte.expr.ValDef(name, t, expr, joinFragments(fragments.tail))
      case BoxDefFragment(name, t, expr) => mte.expr.BoxDef(name, t, expr, joinFragments(fragments.tail))
      case ClassDefFragment(memberName, methods, id) =>
        expr.ClassDef(memberName, methods, id, joinFragments(fragments.tail))
  }
}

type VarInfo = (StringID, Type)

implicit def strToVarInfo(id: String): (StringID, Type) = (StringID(id), VarT())

implicit def exprToFragment(expr: Expr): CodeFragment = CodeFragmentGeneral(expr)
implicit def strToId(id: String): Expr = Id(id)
implicit def varIDToId(id: VarID): Expr = Id(id)
implicit def intToNum(num: Int): Expr = Num(num)
implicit def toIdFrag(name: String): CodeFragment = CodeFragmentGeneral(Id(name))
implicit def varIDToIDFrag(id: VarID): CodeFragment = CodeFragmentGeneral(expr.Id(id))
implicit def toNumFrag(num: Int): CodeFragment = CodeFragmentGeneral(Num(num))

class ExprOps(lhs: Expr) {
  @unused def 배(rhs: Expr): Expr = ops.makeAddExpr(lhs, rhs)

  @unused def 코(rhs: Expr): Expr = ops.makeSubExpr(lhs, rhs)

  @unused def 조이고(rhs: Expr): Expr = ops.makeMulExpr(lhs, rhs)

  @unused def 법회(rhs: Expr): Expr = ops.makeDivExpr(lhs, rhs)

  @unused def 릴(rhs: Expr): Expr = App(rhs, Vector(lhs))

  @unused def 리액션(template: String): Expr = ops.makePrintExpr(lhs, template)

  @unused def 케바바바밥줘(rhs: Expr): Expr = Seqn(lhs, rhs)

  @unused def 꼽표(@unused rhs: EndState3): Expr = ops.makeLogNotExpr(lhs)

  @unused def 코가커요(rhs: Expr): Expr = ops.makeRemainderExpr(lhs, rhs)

  @unused def 반제곱(@unused rhs: EndState6): Expr = ops.makeSqrtExpr(lhs)
}

implicit class ExprOpsExtByExpr(lhs: Expr) extends ExprOps(lhs)

implicit class ExprOpsExtByStr(lhs: String) extends ExprOps(Id(lhs))

implicit class ExprOpsExtByVarID(lhs: VarID) extends ExprOps(expr.Id(lhs))

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

@unused val 스키비야: UnitE = UnitE()
@unused val 스킵이야: UnitE = UnitE()

@unused
def 함수호출: VarID = FnCallOp

@unused
val 나: Expr = Id(ThisKW)

val 아이고난1 = AnonArg(1)
val 아이고난2 = AnonArg(2)
val 아이고난3 = AnonArg(3)
val 아이고난4 = AnonArg(4)
val 아이고난5 = AnonArg(5)
val 아이고난6 = AnonArg(6)
val 아이고난7 = AnonArg(7)

case class VarIDImplicit(id: VarID)

implicit class VarIDImplicitFromVarID(id: VarID) extends VarIDImplicit(id)

implicit class VarIDImplicitFromStr(id: String) extends VarIDImplicit(StringID(id))

case object 춘잣 {
  @targetName("fact")
  @unused
  def !(frag: CodeFragment*): Value = run(joinFragments(frag.toVector))
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
 *
 * @param lhs lhs
 */
case class MulBuilder(lhs: Expr) {

  import 확대하셨군요.RhsBuilder

  /**
   * 곱셈 연산이다 맨이야.
   * 문법: (lhs) 화면을 확대하셨군요!! (rhs)
   *
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
 * 문법: 아니세상에 자기가 (id)라는 사람인데 (expr)을 했대
 */
case object 아니세상에 {
  @unused
  def 자기가(info: (StringID, Type)): ValBuilder = {
    val (name, t) = info
    ValBuilder(name, t)
  }

  case class ValBuilder(name: StringID, t: Type) {
    @unused
    def 라는사람인데(expr: Expr): ValBuilder2 =
      ValBuilder2(name, t, expr)

    @unused
    def 이라는사람인데(expr: Expr): ValBuilder2 =
      ValBuilder2(name, t, expr)
  }

  case class ValBuilder2(name: StringID, t: Type, expr: Expr) {
    @unused
    def 를(@unused h: 했대.type): CodeFragment =
      ValDefFragment(name, t, expr)

    @unused
    def 을(@unused h: 했대.type): CodeFragment =
      ValDefFragment(name, t, expr)

    @unused
    def 발행(@unused x: NFT.type): DefBoxBuilder = DefBoxBuilder(name, t, expr)

    case class DefBoxBuilder(name: StringID, t: Type, expr: Expr) {
      @unused
      def 를(@unused h: 했대.type): CodeFragment = BoxDefFragment(name, t, expr)

      @unused
      def 을(@unused h: 했대.type): CodeFragment = BoxDefFragment(name, t, expr)
    }
  }
}

case object 했대

case class LambdaBuilder(argsID: Vector[StringID], argsT: Vector[Type]) {
  @unused
  def 은(fnExpr: CodeFragment*): LambdaBuilder2 = LambdaBuilder2(argsID, argsT, joinFragments(fnExpr.toVector))

  @unused
  def 는(fnExpr: CodeFragment*): LambdaBuilder2 = LambdaBuilder2(argsID, argsT, joinFragments(fnExpr.toVector))

  class LambdaBuilder2(argsID: Vector[StringID], argsT: Vector[Type], fnExpr: Expr) {
    @unused
    def 다(@unused joyGo: EndState4): Expr = Fun(AnonFn1, argsID, argsT, VarT(), fnExpr)

    @unused
    def 이다(@unused joyGo: EndState4): Expr = expr.Fun(AnonFn1, argsID, argsT, VarT(), fnExpr)
  }
}

implicit class LambdaBuilderFromVecStr(args: Vector[(StringID, Type)])
  extends LambdaBuilder(args.map((x, _) => x), args.map((_, y) => y))

implicit class LambdaBuilderFromStr(argName: String) extends LambdaBuilder(Vector(StringID(argName)), Vector(VarT()))

@unused
case object 아이고난 {
  @unused
  @targetName("factFact")
  def !!(expr: Expr): Expr = {
    def max(x: Int, y: Int): Int = math.max(x, y)

    def analysis(expr: Expr): Int = expr match {
      case BinaryOp(lhs, rhs, _) => max(analysis(lhs), analysis(rhs))
      case TernaryOp(x, y, z, _, _) => max(analysis(x), max(analysis(y), analysis(z)))
      case Id(name) => name match {
        case AnonArg(argIdx) => argIdx
        case _ => 0
      }
      case ValDef(_, _, initExpr, next) => max(analysis(initExpr), analysis(next))
      case Fun(_, _, _, _, fExpr) => analysis(fExpr)
      case App(fnExpr, argExpr) => max(analysis(fnExpr), argExpr.map(analysis).reduce(max))
      case Seqn(lhs, rhs) => max(analysis(lhs), analysis(rhs))
      case WhileN0(cond, exprIn) => max(analysis(cond), analysis(exprIn))
      case Proj(obj, _) => analysis(obj)
      case BoxDef(_, _, initExpr, next) => max(analysis(initExpr), analysis(next))
      case BoxSet(box, setExpr) => max(analysis(box), analysis(setExpr))
      case Vec(data, _) => data.map(analysis).reduce(max)
      case HMap(data, _, _) => data.map((key, value) => max(analysis(key), analysis(value))).reduce(max)
      case ClassDef(_, methods, _, next) => max(methods.values.map(analysis).reduce(max), analysis(next))
      case BuiltinFnE2E(_, arg, _) => analysis(arg)
      case _ => 0
    }

    val argNames = Vector(아이고난1, 아이고난2, 아이고난3, 아이고난4, 아이고난5, 아이고난6, 아이고난7)
    val n = analysis(expr)
    mte.expr.Fun(AnonFn1, argNames.take(n), Vector.fill(n)(VarT()), VarT(), expr)
  }
}

/**
 * 케인인님 함수호출 한판해요
 * 문법: (f)아/야 (arg) 먹어라??
 *
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
 *
 * @param cond   조건문
 * @param exprIn 조건문이 참인 동안 실행할 명령
 */
@unused
def 강제연결(cond: Expr)(exprIn: Expr*): Expr = {
  expr.WhileN0(cond, sugar.vecToSeq(exprIn.toVector))
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
 *
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
    @unused def 일(@unused x: 순없는지.type): Expr = BoxSet(ptr, setExpr)

    @unused def 힐(@unused x: 순없는지.type): Expr = BoxSet(ptr, setExpr)
  }
}

implicit class PtrSetBuilderFromExpr(box: Expr) extends SetBoxBuilder(box)

implicit class PtrSetBuilderFromStr(name: String) extends SetBoxBuilder(Id(name))

implicit class PtrSetBuilderFromInt(num: Int) extends SetBoxBuilder(Num(num))

@unused case object 그런

@unused case object 순없는지

@unused
object 묶음 {
  @unused
  @targetName("factFact")
  def !!(args: (StringID, Type)*): Vector[(StringID, Type)] = args.toVector
}

/**
 * 한줄서기 벡터를 만드는 문법이에요~
 *
 * @param args 벡터를 구성하는 원소들 (variadic)
 * @return 벡터에 해당하는 표현식
 * @return 벡터에 해당하는 표현식
 */
@unused
def 한줄서기(args: Expr*): Expr = {
  var ret: Vector[Expr] = Vector()
  for (arg <- args) ret = ret :+ arg
  Vec(ret, VarT())
}

/**
 * 같은 원소가 여러 개 있는 벡터를 만들고 싶으면 왕한줄서기 문법을 사용하세요~
 *
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
   *
   * @param rhs 연결할 벡터
   * @return 연결된 벡터를 나타내는 표현식
   */
  @unused
  def 조이는(rhs: Expr): Expr = ops.makeExtExpr(lhs, rhs)

  /**
   * 문법: vec 즐기면서가자 fn
   *
   * @param fn vec의 각 원소에 적용할 함수
   * @return container map을 나타내는 표현식
   */
  @unused
  def 즐기면서가자(fn: Expr): Expr = ops.makeVecMapExpr(lhs, fn)

  /**
   * 문법: vec 특수한 fn
   *
   * @param fn 필터 함수
   * @return container filter를 나타내는 표현식
   */
  @unused
  def 특수한(fn: Expr): Expr = ops.makeVecFilterExpr(lhs, fn)

  /**
   * 문법: vec 씻구 fn
   *
   * @param fn 필터 함수
   * @return container filter(reject)를 나타내는 표현식
   */
  @unused
  def 씻구(fn: Expr): Expr = ops.makeVecRejectExpr(lhs, fn)

  import 임마.VecAccessRhsBuilder

  /**
   * 문법: (container) 갖고와 임마!! (idx)
   *
   * @param accIdxWrapper idx
   * @return vector access expression
   */
  @unused
  def 갖고와(accIdxWrapper: VecAccessRhsBuilder): Expr = ops.makeAccessExpr(lhs, accIdxWrapper.accIdx)

  import 한판마안.VecUpdateRhsBuilder


  /**
   * 문법: (container) 갖고와 (idx) 할게 한판마안~~!! (val)
   *
   * @param idxE idx
   * @return a fragment of vector update expression
   */
  @unused
  def 갖고와(idxE: Expr): VecUpdateBuilder = VecUpdateBuilder(lhs, idxE)

  case class VecUpdateBuilder(vecE: Expr, idxE: Expr) {
    @unused
    def 할게(rhs: VecUpdateRhsBuilder): Expr = ops3.makeUpdatedExpr(vecE, idxE, rhs.updateE)
  }

  /**
   * 케인님이 ㄸㄸㅆ를 통해 한줄서기에서 가장 오른쪽에 있는 n개의 원소를 제거해 주실 거예요~
   * 문법: vec ㄸㄸㅆ num
   *
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

@unused
def 뭉탱이(args: (Expr, Expr)*): Expr = HMap(args.toMap, VarT(), VarT())

@unused
case object 겸상 {
  @unused
  @targetName("quesQues")
  def ??(hMap: Expr, v: Expr): Expr = ops.makeHMapContainsExpr(hMap, v)
}

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
val 개입: Expr = BuiltinFnE2E(readInt, UnitE(), "readInt")

/**
 * 나는! 나는! 나는! stdout으로 출력을 했다!
 * 문법: 리액션 (expr) // 리액션 (expr, template)
 *
 * @param expr     출력할 표현식
 * @param template 템플릿 스트링
 * @return 프로그램 트리
 */
@unused
def 리액션(expr: Expr, template: String = "%s"): Expr = ops.makePrintExpr(expr, template)

/**
 * 새로운 클래스를 정의하는 문법이다 맨이야~
 * 문법: "id" 하는 플레이보이 예전에 (members) 같은데 이제 (method definition) 하는갑지 돈좀버냐?? 개노잼 노라라??
 *
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
      val values = methodVec.map(x => expr.Fun(x.methodName, x.args, x.argsT, x.retT, x.fExpr))
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

case class TypeEMethodBuilder1(methodName: VarID, retT: Type) {
  @unused
  def 중에는(@unused x: EndState7): TypeEMethodBuilder2 = TypeEMethodBuilder2(methodName, retT)

  case class TypeEMethodBuilder2(methodName: VarID, retT: Type) {
    @unused
    def 아무리(arg: (StringID, Type)*): TypeEMethodBuilder3 =
      val argv = arg.toVector
      TypeEMethodBuilder3(methodName, argv.map(x => x(0)), argv.map(x => x(1)), retT)
  }

  case class TypeEMethodBuilder3(methodName: VarID, args: Vector[VarID], argsT: Vector[Type], retT: Type) {
    @unused
    def 라도(fragments: CodeFragment*): TypeEMethodBuilder4 =
      TypeEMethodBuilder4(methodName, args, argsT, retT, joinFragments(fragments.toVector))

    @unused
    def 이라도(fragments: CodeFragment*): TypeEMethodBuilder4 =
      TypeEMethodBuilder4(methodName, args, argsT, retT, joinFragments(fragments.toVector))
  }

  case class TypeEMethodBuilder4(methodName: VarID, args: Vector[VarID], argsT: Vector[Type], retT: Type, fExpr: Expr) {
    @unused
    def 할(@unused x: 수가.type): TypeEMethodBuilder5 =
      TypeEMethodBuilder5(methodName, args, argsT, retT, fExpr)
  }

  case class TypeEMethodBuilder5(methodName: VarID, args: Vector[VarID], argsT: Vector[Type], retT: Type, fExpr: Expr) {
    @unused
    def 없단다(@unused x: EndState7): TypeEMethodBuilderFinal =
      TypeEMethodBuilderFinal(methodName, args, argsT, retT, fExpr)
  }
}

case object 수가

private case class TypeEMethodBuilderFinal(methodName: VarID, args: Vector[VarID], argsT: Vector[Type], retT: Type, fExpr: Expr)

implicit def methodToFragment(x: TypeEMethodBuilderFinal): CodeFragment = x.methodName match {
  case id@StringID(_) => ValDefFragment(id, ArrowT(x.argsT, x.retT), expr.Fun(id, x.args, x.argsT, x.retT, x.fExpr))
  case _ => throw mte.error.MteSyntaxErr(
    s"얘! 지금 메서드가 아닌 함수 이름 꼬라지(${x.methodName}) 이게 뭐니!!"
  )
}


implicit class TypeEMethodBuilderFromString(id: String) extends TypeEMethodBuilder1(StringID(id), VarT())

implicit class TypeEMethodBuilderFromID(id: VarID) extends TypeEMethodBuilder1(id, VarT())

case class ProjBuilder(obj: Expr) {
  @unused
  def 의(memberId: VarID): ProjBuilder2 = ProjBuilder2(obj, memberId)

  @unused
  def 의(memberId: String): ProjBuilder2 = ProjBuilder2(obj, StringID(memberId))

  case class ProjBuilder2(obj: Expr, memberId: VarID) {
    @unused
    def 감동님(@unused x: 사랑해.type): Expr = expr.Proj(obj, memberId)
  }
}

case object 사랑해

implicit class ProjBuilderFromExpr(expr: Expr) extends ProjBuilder(expr)

implicit class ProjBuilderFromId(id: String) extends ProjBuilder(Id(id))

implicit class ProjBuilderFromInt(n: Int) extends ProjBuilder(Num(n))

def readInt(@unused expr: Expr = UnitE()): Expr =
  Num(scala.io.StdIn.readInt)