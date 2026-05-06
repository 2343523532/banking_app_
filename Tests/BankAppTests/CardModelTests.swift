import XCTest
@testable import BankAppCore

final class CardModelTests: XCTestCase {
    func testMaskedNumber() {
        let card = Card(holderName: "A", prefix: "400000", number: "4000001234567890", expiryMonth: 12, expiryYear: 2030, cvv: "123", balance: 0)
        XCTAssertEqual(card.maskedNumber, "************7890")
    }

    func testBrandAndDisplayNameFallbacks() {
        let card = Card(holderName: "   ", prefix: "400000", number: "4111111111111111", expiryMonth: 12, expiryYear: 2030, cvv: "123", balance: 0)
        XCTAssertEqual(card.displayName, "Unnamed Holder")
        XCTAssertEqual(card.brandName, "Visa")
    }

    func testValidityAndExpiry() {
        let validNumber = Luhn.generateCardNumber(prefix: "400000")!
        let card = Card(holderName: "A", prefix: "400000", number: validNumber, expiryMonth: 1, expiryYear: 2020, cvv: "123", balance: 0)
        XCTAssertTrue(card.isLikelyValid)

        let calendar = Calendar(identifier: .gregorian)
        let ref = calendar.date(from: DateComponents(year: 2026, month: 1, day: 1))!
        XCTAssertTrue(card.isExpired(referenceDate: ref, calendar: calendar))
    }

    func testDepositAndWithdraw() {
        var card = Card(holderName: "A", prefix: "400000", number: "4000001234567899", expiryMonth: 12, expiryYear: 2030, cvv: "123", balance: 100)

        XCTAssertTrue(card.deposit(50))
        XCTAssertEqual(card.balance, 150)
        XCTAssertFalse(card.deposit(0))

        XCTAssertTrue(card.withdraw(40))
        XCTAssertEqual(card.balance, 110)
        XCTAssertFalse(card.withdraw(200))
        XCTAssertFalse(card.withdraw(-1))
    }
}
