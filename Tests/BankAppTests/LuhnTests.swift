import XCTest
@testable import BankAppCore

final class LuhnTests: XCTestCase {
    func testChecksumDigit() {
        XCTAssertEqual(Luhn.checksumDigit(forPartial: "7992739871"), 3)
        XCTAssertEqual(Luhn.checksumDigit(forPartial: "1"), 8)
    }

    func testIsValid() {
        XCTAssertTrue(Luhn.isValid("79927398713"))
        XCTAssertFalse(Luhn.isValid("79927398710"))
        XCTAssertFalse(Luhn.isValid("1"))
        XCTAssertTrue(Luhn.isValid("18"))
    }

    func testGenerateCardNumber() {
        let prefix = "400000"
        let generated = Luhn.generateCardNumber(prefix: prefix)
        XCTAssertNotNil(generated)
        XCTAssertTrue(generated!.hasPrefix(prefix))
        XCTAssertEqual(generated!.count, 16)
        XCTAssertTrue(Luhn.isValid(generated!))
    }

    func testNormalizeDigits() {
        XCTAssertEqual(Luhn.normalizedDigits(from: "4000 1234-5678 9010"), "4000123456789010")
        XCTAssertEqual(Luhn.normalizedDigits(from: "abc"), "")
    }

    func testMaskedNumber() {
        XCTAssertEqual(Luhn.masked("4000123412341234"), "************1234")
        XCTAssertEqual(Luhn.masked("4000-1234", visibleSuffixCount: 2), "******34")
        XCTAssertEqual(Luhn.masked("40001234", visibleSuffixCount: 0), "********")
    }

    func testCardBrandDetection() {
        XCTAssertEqual(Luhn.cardBrand(for: "4111111111111111"), "Visa")
        XCTAssertEqual(Luhn.cardBrand(for: "5555555555554444"), "Mastercard")
        XCTAssertEqual(Luhn.cardBrand(for: "2223003122003222"), "Mastercard")
        XCTAssertEqual(Luhn.cardBrand(for: "378282246310005"), "American Express")
        XCTAssertEqual(Luhn.cardBrand(for: "6011111111111117"), "Discover")
        XCTAssertEqual(Luhn.cardBrand(for: "999999"), "Unknown")
    }
}
