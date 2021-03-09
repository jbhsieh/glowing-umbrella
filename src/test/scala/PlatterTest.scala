import org.scalatest.flatspec.AnyFlatSpec

class PlatterTest extends AnyFlatSpec with CreatePlatter {
  behavior of "platter"

  it should "be able extract an operator" in {
    assert(Platter(0xa0000000).operator == 0x0a)
    assert(Platter(0xc0000000).operator == 0x0c)
    assert(Platter(0x7111ffff).operator == 0x07)
    assert(Platter(0x21ffffff).operator == 0x02)
  }

  it should "be able extract registers for standard operators" in {
    assert(Platter(0x000001c0).registerA == 0x07)
    assert(Platter(0x00000038).registerB == 0x07)
    assert(Platter(0x0000000f).registerC == 0x07)
  }

  it should "be able to extract fields for special operators" in {
    assert(Platter(0xf8000000).specialRegA().toBinaryString == "100")
    assert(Platter(0xaf120000).specialValue().toHexString == "1120000")
  }

  it should "support operation0 - Conditional Move" in {
    val startR: IndexedSeq[Int] = IndexedSeq.range(0, 8)
    val moveSuccess = createStandardPlatter(0, 2, 4, 6)
    val moveFail = createStandardPlatter(0, 2, 4, 0)
    assert(Platter.operator0(startR, moveSuccess) == Vector(0, 1, 4, 3, 4, 5, 6, 7))
    assert(Platter.operator0(startR, moveFail) == Vector(0, 1, 2, 3, 4, 5, 6, 7))
  }
  it should "support operation3 - Addition" in {
    val startR = IndexedSeq.range(0, 8)
    val addSimple = createStandardPlatter(3, 1, 3, 5)
    assert(Platter.operator3(startR, addSimple) == Vector(0, 8, 2, 3, 4, 5, 6, 7))
  }
  it should "support overflow addition" in pendingUntilFixed{
    val startRollover = Vector(0, 0x7fff0000, 0x000f1234, 0, 0, 0, 0, 0)
    val addRollover = createStandardPlatter(3, 0, 1, 2)
    assert(Platter.operator3(startRollover, addRollover) == Vector(0, 0x7fff0000, 0x000f1234, 0, 0, 0, 0, 0))
  }

  it should "support operation4 - Multiplication" in {
    val startR = IndexedSeq.range(0, 8)
    val multSimple = createStandardPlatter(4, 1, 3, 5)
    assert(Platter.operator4(startR, multSimple) == Vector(0, 15, 2, 3, 4, 5, 6, 7))
  }
  it should "support overflow multiplication" in pendingUntilFixed{
    val startRollover = Vector(0, 0x7fff0000, 0x000f1234, 0, 0, 0, 0, 0)
    val multRollover = createStandardPlatter(4, 0, 1, 2)
    assert(Platter.operator4(startRollover, multRollover) == Vector(0, 0x7fff0000, 0x000f1234, 0, 0, 0, 0, 0))
  }
  it should "support operation5 - Division" in {
    val startR = IndexedSeq.range(0, 8)
    val divSimple = createStandardPlatter(5, 1, 6, 3)
    assert(Platter.operator5(startR, divSimple) == Vector(0, 2, 2, 3, 4, 5, 6, 7))
  }
  it should "support overflow division" in pendingUntilFixed{
    val startRollover = Vector(0, 0x7fff0000, 0x000f1234, 0, 0, 0, 0, 0)
    val divRollover = createStandardPlatter(5, 0, 1, 2)
    assert(Platter.operator5(startRollover, divRollover) == Vector(0, 0x7fff0000, 0x000f1234, 0, 0, 0, 0, 0))
  }
  it should "support operation6 - NAND" in {
    val startR = Vector(0,0,0xffff0000,0x00ffff00,0,0,0,0)
    val nandSimple = createStandardPlatter(6, 1, 2, 3)
    assert(Platter.operator6(startR, nandSimple) == Vector(0,0xff00ffff,0xffff0000,0x00ffff00,0,0,0,0))
  }
    it should "support operation13 - Orthography" in {
    val zeros = IndexedSeq.fill(8)(0)
    val special = createSpecialPlatter(13, 7, 0x70ffff)
    assert(Platter.operator13(zeros, special) == Vector(0,0,0,0,0,0,0,7405567))
  }
}
