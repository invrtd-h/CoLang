package mte.experimental

import scala.util.control.TailCalls.*
import mte.ids._
import mte.expr._
import mte.value._
import mte.error._

type Cont = Value => TailRec[Value]

def pret(expr: Expr, env: Env, cont: Cont): TailRec[Value] = expr match {
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
  case Seqn(lhs, rhs) => ???
  case WhileN0(cond, exprIn) => ???
  case Proj(obj, id) => ???
  case BoxDef(id, t, initExpr, next) => ???
  case BoxSet(box, setExpr) => ???
  case Vec(data, t) => ???
  case HMap(data, k, v) => ???
  case ClassDef(memberName, methods, typeName, next) => ???
  case _ => ???
}

def run(expr: Expr): Value = pret(expr, Map(), x => done(x)).result