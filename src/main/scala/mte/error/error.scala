package mte.error

import mte.mtetype.Type
import scala.util.{Try, Success, Failure}

def mteAssert(b: Boolean, exception: Exception): Try[Unit] =
  if (b) Success(()) else Failure(exception)

/**
 * MTELang의 런타임 에러는 이 클래스가 담당할 거예요~
 *
 * @param message message
 * @param cause   cause
 */
final case class MteRuntimeErr(private val message: String = "",
                               private val cause: Throwable = None.orNull)
  extends Exception(message, cause)

final case class MteSyntaxErr(private val message: String = "",
                              private val cause: Throwable = None.orNull)
  extends Exception(message, cause)

final case class MteUnexpectedErr(private val message: String = "",
                                  private val cause: Throwable = None.orNull)
  extends Exception(message, cause)

class MteExc(message: String) extends Exception(message) {
  def this(message: String, cause: Throwable) = {
    this(message)
    initCause(cause)
  }

  def this(cause: Throwable) = {
    this(Option(cause).map(_.toString).orNull, cause)
  }

  def this() = this(null: String)
}

class MteRuntimeExc(msg: String) extends MteExc(msg)

class MteSyntaxExc(msg: String) extends MteExc(msg)

class MteTypeExc(msg: String) extends MteExc(msg)

case class MteAssertionFailedException(msg: String) extends MteRuntimeExc(msg)

case class MteTypeUnsolvedException(msg: String) extends MteSyntaxExc(msg)

case class MteTypeNonMatchExc(t: Type, s: String)
  extends MteTypeExc(s"얘! 여기 지금 $t 이게 $s 분류의 타입으로 보이니??")

case class MteArgNumIncorrectExc(t: Type, argNum: Int)
  extends MteTypeExc(s"얘! 지금 어딜 감히 인수 ${argNum}개에 $t 타입 함수를 불러달라는 거니??")