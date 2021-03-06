import org.scalatest.flatspec.AnyFlatSpec

class platterTest extends AnyFlatSpec {
  behavior of "platter"

  it should "be able extract an operator" in {
    assert(platter(0xa0000000).operator == 0x0a)
    assert(platter(0xc0000000).operator == 0x0c)
    assert(platter(0x7111ffff).operator == 0x07)
    assert(platter(0x21ffffff).operator == 0x02)
  }

  it should "be able extract registers for standard operators" in {
    assert(platter(0x000001c0).getRegisterAIndex == 0x07)
    assert(platter(0x00000038).getRegisterBIndex == 0x07)
    assert(platter(0x0000000f).getRegisterCIndex == 0x07)
  }

  it should "be able to extract fields for special operators" in {
    assert(platter(0xf8000000).getSpecialRegA().toBinaryString == "100")
    assert(platter(0xaf120000).getSpecialValue().toHexString == "1120000")
  }
}
