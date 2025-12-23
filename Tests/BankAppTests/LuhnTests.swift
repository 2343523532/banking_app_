import XCTest
@testable import BankAppCore

final class LuhnTests: XCTestCase {
    func testChecksumDigit() {
        // Known valid Luhn sequences
        // 7992739871 -> last digit 3
        XCTAssertEqual(Luhn.checksumDigit(forPartial: "7992739871"), 3)

        // Simple case: 1 -> double -> 2 -> sum 2 -> mod 2 -> 10-2=8
        XCTAssertEqual(Luhn.checksumDigit(forPartial: "1"), 8)
    }

    func testIsValid() {
        XCTAssertTrue(Luhn.isValid("79927398713"))
        XCTAssertFalse(Luhn.isValid("79927398710")) // Invalid checksum
        XCTAssertFalse(Luhn.isValid("1")) // Too short
        XCTAssertTrue(Luhn.isValid("18")) // 1 -> 2, sum 2+8=10, 0 mod 10 == 0
    }

    func testGenerateCardNumber() {
        let prefix = "400000"
        let generated = Luhn.generateCardNumber(prefix: prefix)
        XCTAssertNotNil(generated)
        XCTAssertTrue(generated!.hasPrefix(prefix))
        XCTAssertEqual(generated!.count, 16)
        XCTAssertTrue(Luhn.isValid(generated!))
    }
}
