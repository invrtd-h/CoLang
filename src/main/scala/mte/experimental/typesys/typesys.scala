package mte.experimental.typesys

import scala.annotation.targetName

private sealed trait Expr

private sealed trait Value

private sealed trait TypeExpr

private sealed trait Type

type Env = Map[String, Value]

case class TEnv(vars: Map[String, Type]) {
  def pushedVar(var0: (String, Type)): TEnv =
    this.copy(vars = this.vars + var0)
}

case class Num(n: Int) extends Expr
case class Id(id: String) extends Expr
case class Add(l: Expr, r: Expr) extends Expr
case class Fn(argId: String, argTE: TypeExpr, fnState: Expr) extends Expr
case class App(fn: Expr, arg: Expr) extends Expr

case class NumV(n: Int) extends Value
case class CloV(fn: Expr, arg: String, fEnv: Env) extends Value

case object NumTE extends TypeExpr
case class ArrowTE(argTE: TypeExpr, retTE: TypeExpr) extends TypeExpr

case object NumT extends Type
case class ArrowT(argT: Type, retT: Type) extends Type

implicit class TypeRelations(t: Type) {
  @targetName("assertEq")
  def ==!(u: Type): Type = {
    assert(t == u, s"$t != $u")
    t
  }
}

def typePret(typeExpr: TypeExpr, tEnv: TEnv): Type = typeExpr match {
  case NumTE => NumT
  case ArrowTE(argTE, retTE) => ArrowT(typePret(argTE, tEnv), typePret(retTE, tEnv))
}

def typeCheck(expr: Expr, tEnv: TEnv): Type = expr match {
  case Num(_) => NumT
  case Id(id) => tEnv.vars(id)
  case Add(l, r) =>
    typeCheck(l, tEnv) ==! NumT
    typeCheck(r, tEnv) ==! NumT
    NumT
  case Fn(argId, argTE, fnState) =>
    val argT: Type = typePret(argTE, tEnv)
    ArrowT(argT, typeCheck(fnState, tEnv.pushedVar(argId -> argT)))
  case App(fn, arg) => typeCheck(fn, tEnv) match {
    case ArrowT(argT, retT) =>
      typeCheck(arg, tEnv) ==! argT
      retT
    case _ => throw Exception()
  }
}

def pret(expr: Expr, env: Env): Value = expr match {
  case Num(n) => NumV(n)
  case Id(id) => env(id)
  case Add(l, r) => pret(l, env) match {
    case NumV(nl) => pret(r, env) match {
      case NumV(nr) => NumV(nl + nr)
      case _ => throw Exception()
    }
    case _ => throw Exception()
  }
  case Fn(argId, _, fnState) => CloV(fnState, argId, env)
  case App(fn, arg) => pret(fn, env) match {
    case CloV(fn, argId, fEnv) =>
      val argV: Value = pret(arg, env)
      pret(fn, fEnv + (argId -> argV))
    case _ => throw Exception()
  }
}

def run(expr: Expr): Value = {
  typeCheck(expr, TEnv(Map()))
  pret(expr, Map())
}

def test(expr: Expr): Unit = {
  try {
    println(run(expr))
  } catch {
    case e: Exception => println(e)
    case e: AssertionError => println(e)
  }
}

def code1: Expr = {
  val f1 = Fn("x", NumTE, Fn("y", NumTE, Add(Id("x"), Id("y"))))
  App(App(f1, Num(1)), Num(2))
}

def code2: Expr = {
  val f2 = Fn("x", NumTE, Add(Id("x"), Num(1)))
  App(f2, f2)
}

@main
def main(): Unit = {
  test(code1)
  test(code2)
}