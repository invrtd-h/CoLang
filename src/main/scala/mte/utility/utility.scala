package mte.utility

import scala.annotation.{tailrec, targetName, unused}
import scala.util.Random

import mte._

implicit class Piper[F](val value: F) {
  @unused
  @targetName("pipe")
  def |>[G](f: F => G): G = f(value)
}

package strcut {
  case class StrCut(s: String, l: Int, r: Int) {
    def cut: String = {
      val t: String = s.dropRight(r)
      t.substring(l)
    }
  }

  implicit class StrCutFromVec[T](s: Vector[T]) extends StrCut(s.toString(), 7, 1)

  implicit class StrCutFromMap[K, V](s: Map[K, V]) extends StrCut(s.toString(), 4, 1)
}

/**
 *
 */
private val rand: Random = new Random()

def randBetween(lbdInclusive: Int, ubdExclusive: Int): Int =
  rand.between(lbdInclusive, ubdExclusive)

@unused
def leInt(lhs: BigInt, rhs: BigInt): Int =
  if (lhs <= rhs) 1 else 0

/**
 * 얘! 내가 이런 함수까지 일일이 다 주석을 달아야 되니? 귀찮아!
 *
 * @param lhs 아 귀찮아~~~!!!!!
 * @param rhs 아 귀찮아~~~!!!!!
 * @return lhs > rhs
 */
def gtInt(lhs: BigInt, rhs: BigInt): BigInt =
  if (lhs > rhs) 1 else 0

def geInt(lhs: BigInt, rhs: BigInt): BigInt =
  if (lhs >= rhs) 1 else 0

def logNot(lhs: BigInt): BigInt =
  if (lhs == 0) 1 else 0

@unused
def maxInt(lhs: BigInt, rhs: BigInt): BigInt =
  if (lhs > rhs) lhs else rhs