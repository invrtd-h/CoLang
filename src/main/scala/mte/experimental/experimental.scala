package mte.experimental

import scala.util.control.TailCalls.*
import mte.ids._
import mte.expr._
import mte.value._
import mte.error._

type ContFn = Value => TailRec[Value]

private class Cont[R, U](data: (U => R) => R) {
  def applyCont(f: U => R): R = data(f)

  def map[U2](fn: U => U2): Cont[R, U2] = Cont(
    (f: U2 => R) => applyCont(fn.andThen(f))
  )

  def flatMap[U2](fn: U => Cont[R, U2]): Cont[R, U2] = Cont(
    (f: U2 => R) => applyCont(u => fn(u).applyCont(f))
  )
}

def toCont[R, U](u: U): Cont[R, U] = Cont((f: U => R) => f(u))

def addCPS(a: Int, b: Int): Cont[Int, Int] = toCont(a + b)
def subCPS(a: Int, b: Int): Cont[Int, Int] = toCont(a - b)
def mulCPS(a: Int, b: Int): Cont[Int, Int] = toCont(a * b)

@main
def main(): Unit = {
  val ret = for {
    v1 <- addCPS(1, 2)
    v2 <- subCPS(7, 4)
    v3 <- mulCPS(v1, v2)
  } yield v3
  println(ret.applyCont(x => x))
}

private def toCPS[T, U, R](f: T => U, c: Cont[R, U]): T => Cont[R, U] =
  t => Cont(uToR => uToR(f(t)))

def pret(expr: Expr, env: Env, cont: ContFn): TailRec[Value] = expr match {
  case Num(data) => tailcall(cont(NumV(data)))
  case StrE(data) => tailcall(cont(StrV(data)))
  case BinaryOp(lhs, rhs, op) => tailcall(
    pret(lhs, env, x => tailcall(
      pret(rhs, env, y => tailcall(
        cont(op.calculate(x.toFOV, y.toFOV) match {
          case Left(value) => throw MteRuntimeErr(value)
          case Right(value) => value
        })
      ))
    ))
  )
  case TernaryOp(x, y, z, op, _) => tailcall(
    pret(x, env, xx => tailcall(
      pret(y, env, yy => tailcall(
        pret(z, env, zz => tailcall(
          cont(op(xx.toFOV, yy.toFOV, zz.toFOV) match {
            case Left(value) => throw MteRuntimeErr(value)
            case Right(value) => value
          })
        ))
      ))
    ))
  )
  case Id(name) => tailcall(cont(env(name)))
  case ValDef(id, _, initExpr, next) => tailcall(
    pret(initExpr, env, initV => tailcall(
      pret(next, env + (id -> initV), cont)
    ))
  )
  case Fun(funName, argName, _, _, fExpr) => tailcall {
    val ret = CloV(argName, fExpr, env)
    ret.fEnv += (funName -> ret)
    cont(ret)
  }
  case App(fnExpr, argExpr) => ???
  case Seqn(lhs, rhs) => tailcall(
    pret(lhs, env, * => tailcall(
      pret(rhs, env, cont)
    ))
  )
  case WhileN0(cond, exprIn) => ???
  case Proj(obj, id) => ???
  case BoxDef(id, t, initExpr, next) => ???
  case BoxSet(box, setExpr) => ???
  case Vec(data, t) => ???
  case HMap(data, k, v) => ???
  case ClassDef(memberName, methods, typeName, next) => ???
  case _ => ???
}

private[mte] def run(expr: Expr): Value = pret(expr, Map(), x => done(x)).result