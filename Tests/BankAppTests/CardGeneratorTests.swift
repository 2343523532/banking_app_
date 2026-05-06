import XCTest
@testable import BankAppCore

final class CardGeneratorTests: XCTestCase {
    func testSanitizesPrefixForInteractiveEntry() {
        XCTAssertEqual(CardGenerator.sanitizedPrefix(" 4000-00abc9999999"), "400000999999")
    }

    func testValidationMessageRequiresSixDigitBin() {
        XCTAssertNil(CardGenerator.validationMessage(forPrefix: "400000"))
        XCTAssertEqual(CardGenerator.validationMessage(forPrefix: "4000"), "BIN prefixes are typically 6 digits long.")
        XCTAssertEqual(CardGenerator.validationMessage(forPrefix: "40A000"), "BIN prefix must contain digits only.")
    }

    func testMakeVisaSandboxCardAddsMetadataAndValidNumber() throws {
        let card = try CardGenerator.makeCard(
            options: CardGenerationOptions(
                holderName: "  Test Holder  ",
                binPrefix: CardBrand.visa.defaultTestPrefix,
                brand: .visa,
                balanceRange: 100...100,
                metadata: ["sourceType": "unit-test"]
            )
        )

        XCTAssertEqual(card.displayName, "Test Holder")
        XCTAssertEqual(card.prefix, "400000")
        XCTAssertEqual(card.number.count, 16)
        XCTAssertEqual(card.cvv.count, 3)
        XCTAssertEqual(card.balance, 100)
        XCTAssertTrue(card.isLikelyValid)
        XCTAssertEqual(card.brandName, CardBrand.visa.rawValue)
        XCTAssertEqual(card.metadata?["environment"], "sandbox")
        XCTAssertEqual(card.metadata?["sourceType"], "unit-test")
    }

    func testMakeAmericanExpressUsesFifteenDigitsAndFourDigitCVV() throws {
        let card = try CardGenerator.makeCard(
            options: CardGenerationOptions(
                holderName: "Amex Tester",
                binPrefix: CardBrand.americanExpress.defaultTestPrefix,
                brand: .americanExpress
            )
        )

        XCTAssertEqual(card.number.count, 15)
        XCTAssertEqual(card.cvv.count, 4)
        XCTAssertTrue(card.isLikelyValid)
        XCTAssertEqual(card.brandName, CardBrand.americanExpress.rawValue)
    }

    func testRejectsUnsafeOrUnsupportedInput() {
        XCTAssertThrowsError(try CardGenerator.makeCard(options: CardGenerationOptions(holderName: "", binPrefix: "400000"))) { error in
            XCTAssertEqual(error as? CardGenerationError, .emptyHolderName)
        }
        XCTAssertThrowsError(try CardGenerator.makeCard(options: CardGenerationOptions(holderName: "A", binPrefix: "4000AB"))) { error in
            XCTAssertEqual(error as? CardGenerationError, .invalidPrefix)
        }
    }
}
