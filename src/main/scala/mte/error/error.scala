package mte.error

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

class MteRuntimeException(message: String) extends Exception(message) {
  def this(message: String, cause: Throwable) = {
    this(message)
    initCause(cause)
  }

  def this(cause: Throwable) = {
    this(Option(cause).map(_.toString).orNull, cause)
  }

  def this() = this(null: String)
}

case class MteAssertionFailedException(msg: String) extends MteRuntimeException(msg)

