package mte.utility

import scala.annotation.{tailrec, targetName, unused}
import scala.util.Random

import mte._

implicit class Piper[F](val value: F) {
  @unused
  @targetName("pipe")
  def |>[G](f: F => G): G = f(value)
}

def mteName[T](vec: Vector[T]): String = vec.toString.drop(7).dropRight(1)
def mteName[T, U](map: Map[T, U]): String = map.toString.drop(4).dropRight(1)

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

def maxInt(lhs: BigInt, rhs: BigInt): BigInt =
  if (lhs > rhs) lhs else rhs