

case class Platter(p: Int) {
  // bits VUTS
  def operator(): Int = this.p >>> 28

  // bits 876
  def registerA() = (this.p >> 6) & 0x7
  // bits 543
  def registerB() = (this.p >> 3) & 0x7
  // bits 210
  def registerC() = this.p & 0x7

  // bits RQP
  def specialRegA() = (this.p >> 25) & 0x7
  //bits O to 0
  def specialValue()= this.p & 0x01ffffff
}

object Platter {
  // Conditional Move
  // from B to A unless C==0
  def operator0(r: IndexedSeq[Int], p: Platter): IndexedSeq[Int] = {
    if (r(p.registerC) != 0) {
      r.updated(p.registerA, r(p.registerB))
    } else r
  }

  // Array Index
  // ???
  def operator1() = ???
  def operator2() = ???

  // Addition
  // A = B + C mod 2^32
  def operator3(r: IndexedSeq[Int], p: Platter): IndexedSeq[Int] = {
    r.updated(p.registerA, r(p.registerB) + r(p.registerC))
  }

  // multiplication
  def operator4(r: IndexedSeq[Int], p: Platter) = {
    r.updated(p.registerA, r(p.registerB) * r(p.registerC))
  }
  // division
  def operator5(r: IndexedSeq[Int], p: Platter) = {
    r.updated(p.registerA, r(p.registerB) / r(p.registerC))
  }
  // not-and
  def operator6(r: IndexedSeq[Int], p: Platter) = {
    r.updated(p.registerA, ~(r(p.registerB) & r(p.registerC)))
  }
  def operator7() = ???
  def operator8() = ???
  def operator9() = ???
  def operator10() = ???
  def operator11() = ???
  def operator12() = ???

  def operator13(r: IndexedSeq[Int], p: Platter): IndexedSeq[Int] = {
    r.updated(p.specialRegA(), p.specialValue())
  }
}

trait CreatePlatter {
  def createStandardPlatter(operator: Int, registerA: Int, registerB: Int, registerC: Int): Platter = {
    val trimmedOp = operator & 0x0f
    val trimmedA = registerA & 0x07
    val trimmedB = registerB & 0x07
    val trimmedC = registerC & 0x07

    Platter((trimmedOp << 28) + (trimmedA << 6) + (trimmedB << 3) + trimmedC)
  }
  def createSpecialPlatter(operator: Int, registerA: Int, value: Int): Platter = {
    val trimmedOp = operator & 0x0f
    //if (trimmedOp != 13)
    val trimmedA = registerA & 0x07
    val trimmedValue = value & 0x01ffffff

    Platter((trimmedOp << 28) + (trimmedA << 25) + trimmedValue)
  }
}