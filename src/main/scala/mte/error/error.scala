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

