import stainless.annotation._
import stainless.collection._
import stainless.math._
import stainless.lang._


// Interface with convenient types
@library @extern
object LeftPad {
  def leftPad(str: String, filler: Char, n: Int): String = stainlessListToString(LeftPadProofs.leftPad(stringToStainlessList(str), filler, BigInt(n)))

  private def stringToStainlessList(s: String): List[Char] = List.fromScala(s.toList)

  private def stainlessListToString(l: List[Char]): String = l.toScala(l).mkString
}


// Proofs using Stainless types
object LeftPadProofs {

  def leftPad[T](str: List[T], filler: T, n: BigInt): List[T] = {
    List.fill(padSize(str, n))(filler) ++ str
  }

  def padSize[T](str: List[T], n: BigInt): BigInt = max(0, n - str.length)

  // Proofs of properties
  def leftPad_length[T](str: List[T], filler: T, n: BigInt): Boolean = {
    leftPad(str, filler, n).length == max(n, str.length)
  }.holds

  def leftPad_suffix[T](str: List[T], filler: T, @induct n: BigInt): Boolean = {
    require(n >= 0)
    leftPad(str, filler, n).drop(padSize(str, n)) == str
  }.holds

  def leftPad_prefix[T](str: List[T], filler: T, @induct n: BigInt): Boolean = {
    require(n >= 0)
    leftPad(str, filler, n).take(padSize(str, n)) == List.fill(padSize(str, n))(filler)
  }.holds

  def leftPad_n_smaller[T](str: List[T], filler: T, n: BigInt): Boolean = {
    require(n <= str.length)
    leftPad(str, filler, n) == str
  }.holds
}
