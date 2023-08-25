package mte.pret

import mte.expr._
import mte.value._
import mte.ids._
import mte.mtetype._
import mte.error._

/**
 * 타입체크를 한 뒤 코드를 실행한다 맨이야
 *
 * @param expr 실행할 프로그램 트리
 * @return 프로그램의 결과
 */
private[mte] def run(expr: Expr): Value = {
  staticCheck(expr, TEnv(Map(), Map()))
  pret(expr, Map())
}

private def staticCheck(expr: Expr, tEnv: TEnv): Type = {
  NumT
}

private[mte] def pret(expr: Expr, env: Env): Value = {
  def validateID(id: VarID): Unit = id match {
    case StringID(id) => if (id.contains("킹") && id.contains("갓")) {
      throw MteRuntimeErr("내가 킹하고 갓하고 함부로 막 붙이지 말라 그랬지!!")
    }
    case _ =>
  }

  def fnCall(fnExpr: Expr, argExprs: Vector[Expr]): Value = pret(fnExpr, env) match {
    case CloV(argName, fExpr, fEnv) =>
      if (argName.length != argExprs.length) {
        throw MteSyntaxErr()
      }
      val argV: Vector[Value] = argExprs.map(x => pret(x, env).toFOV)
      pret(fExpr, fEnv ++ argName.zip(argV).toMap)
    case typeV@ClassV(_, _, _, _) => typeV.construct(argExprs.map(x => pret(x, env)))
    case objV@ObjV(_, supertype) => supertype.makeMethodOf(objV, FnCallOp).call(
      argExprs.map(x => pret(x, env).toFOV)
    )
    case err@_ => throw MteRuntimeErr(
      s"얘! 지금 $err 이게 함수로 보이니?"
    )
  }

  expr match {
    case Num(data) => NumV(data)
    case StrE(data) => StrV(data)
    case BinaryOp(lhs, rhs, op) => op.calculate(pret(lhs, env).toFOV, pret(rhs, env).toFOV) match {
      case Left(err) => throw MteRuntimeErr(err + s"\nexpr: $expr")
      case Right(value) => value
    }
    case TernaryOp(x, y, z, op, _) => op(pret(x, env).toFOV, pret(y, env).toFOV, pret(z, env).toFOV) match {
      case Left(err) => throw MteRuntimeErr(err + s"\nexpr: $expr")
      case Right(value) => value
    }
    case Id(name) => env.get(name) match {
      case Some(value) => value match {
        case NumV(num) => name match {
          case StringID(id) => NumV(num - 3000 * id.count(_ == '코'))
          case _ => NumV(num)
        }
        case value@_ => value
      }
      case _ => throw MteRuntimeErr(
        s"얘! 컴파일쟁이($env)들은 $name 이런 거 잘 몰라 임마!"
      )
    }
    case ValDef(valName, _, initExpr, next) =>
      validateID(valName)
      val initV: Value = pret(initExpr, env).toFOV
      pret(next, env + (valName -> initV))
    case Fun(funName, argName, _, _, fExpr) =>
      val ret: CloV = CloV(argName, fExpr, env)
      ret.fEnv += (funName -> ret)
      ret
    case App(fnExpr, argExpr) => fnCall(fnExpr, argExpr)
    case Tuple(data, _) => TupleV(data.map(x => pret(x, env)))
    case Seqn(lhs, rhs) =>
      pret(lhs, env); pret(rhs, env)
    case WhileN0(cond, exprIn) =>
      val check = (condExpr: Expr) => {
        pret(condExpr, env).toFOV match {
          case NumV(data) => data
          case err@_ => throw MteRuntimeErr(
            s"얘! 지금 $err 이게 조건문 안에 들어갈 수 있겠니?? 죽여벌랑"
          )
        }
      }
      var condVal = check(cond)
      while (condVal != 0) {
        pret(exprIn, env)
        condVal = check(cond)
      }
      unitV
    case Proj(obj, id) => pret(obj, env).toFOV match {
      case obj@ObjV(data, supertype) => id match
        case id@StringID(_) => data.get(id) match
          case Some(value) => value
          case None => supertype.makeMethodOf(obj, id)
        case _ => supertype.makeMethodOf(obj, id)
      case _ => throw MteRuntimeErr(
        s"얘! 지금 네 눈에 $obj 이게 객체로 보이니??"
      )
    }
    case BoxDef(id, _, initExpr, next) =>
      validateID(id)
      val initV: Value = pret(initExpr, env).toFOV
      pret(next, env + (id -> makeNewBox(initV)))
    case BoxSet(ref, setExpr) =>
      val setVal = pret(setExpr, env).toFOV
      pret(ref, env) match {
        case box@BoxV(_, _) =>
          box.set(setVal)
          setVal
        case err@_ => throw MteRuntimeErr(
          s"얘! 지금 네 눈에 $err (${ref}를 실행했다 맨이야) 이게 NFT로 보이니? env=$env"
        )
      }
    case Vec(data, _) => VecV(data.map(x => pret(x, env).toFOV))
    case HMap(data, _, _) => HMapV(data.map((key, value) => (pret(key, env).toFOV, pret(value, env).toFOV)))
    case ClassDef(memberName, methods, typeName, next) =>
      if (env.contains(typeName)) throw MteRuntimeErr(
        s"얘! 이미 사용하고 있는 변수명은 코괴물 이름이 되지 아내!"
      )
      val newClass = ClassV(
        memberName, methods.map((key, value) => (key, pret(value, env))), typeName, env
      )
      newClass.cEnv += (typeName -> newClass)
      pret(next, env + (typeName -> newClass))
  }
}