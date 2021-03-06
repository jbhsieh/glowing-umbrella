

case class platter(p: Int) {
  // bits VUTS
  def operator(): Int = this.p >>> 28

  // bits 876
  def getRegisterAIndex() = (this.p >> 6) & 0x7
  // bits 543
  def getRegisterBIndex() = (this.p >> 3) & 0x7
  // bits 210
  def getRegisterCIndex() = this.p & 0x7

  // bits RQP
  def getSpecialRegA() = (this.p >> 25) & 0x7
  //bits O to 0
  def getSpecialValue()= this.p & 0x01ffffff
}

object platter {
  def operator1() = ???
  def operator2() = ???
  def operator3() = ???
}