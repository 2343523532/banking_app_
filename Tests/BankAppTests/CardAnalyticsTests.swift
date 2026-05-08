import XCTest
@testable import BankAppCore

final class CardAnalyticsTests: XCTestCase {
    private let calendar = Calendar(identifier: .gregorian)

    func testCardHealthStatusPrioritizesInvalidNumbers() {
        let card = Card(holderName: "A", prefix: "400000", number: "4000000000000000", expiryMonth: 12, expiryYear: 2030, cvv: "123", balance: 100)
        let referenceDate = calendar.date(from: DateComponents(year: 2026, month: 5, day: 8))!

        XCTAssertEqual(card.healthStatus(referenceDate: referenceDate, calendar: calendar), .invalidNumber)
    }

    func testCardHealthStatusDetectsExpiredAndExpiringSoonCards() {
        let expiredNumber = Luhn.generateCardNumber(prefix: "400000")!
        let soonNumber = Luhn.generateCardNumber(prefix: "555555")!
        let activeNumber = Luhn.generateCardNumber(prefix: "601111")!
        let referenceDate = calendar.date(from: DateComponents(year: 2026, month: 5, day: 8))!

        let expired = Card(holderName: "Expired", prefix: "400000", number: expiredNumber, expiryMonth: 4, expiryYear: 2026, cvv: "123", balance: 100)
        let soon = Card(holderName: "Soon", prefix: "555555", number: soonNumber, expiryMonth: 6, expiryYear: 2026, cvv: "123", balance: 200)
        let active = Card(holderName: "Active", prefix: "601111", number: activeNumber, expiryMonth: 12, expiryYear: 2027, cvv: "123", balance: 300)

        XCTAssertEqual(expired.healthStatus(referenceDate: referenceDate, calendar: calendar), .expired)
        XCTAssertEqual(soon.healthStatus(referenceDate: referenceDate, calendar: calendar), .expiringSoon)
        XCTAssertEqual(active.healthStatus(referenceDate: referenceDate, calendar: calendar), .active)
    }

    func testPortfolioSummaryCalculatesBalancesCountsAndAttentionItems() {
        let referenceDate = calendar.date(from: DateComponents(year: 2026, month: 5, day: 8))!
        let active = Card(holderName: "Active", prefix: "400000", number: Luhn.generateCardNumber(prefix: "400000")!, expiryMonth: 12, expiryYear: 2027, cvv: "123", balance: 300)
        let expiringSoon = Card(holderName: "Soon", prefix: "555555", number: Luhn.generateCardNumber(prefix: "555555")!, expiryMonth: 6, expiryYear: 2026, cvv: "123", balance: 200)
        let invalid = Card(holderName: "Invalid", prefix: "999999", number: "9999999999999999", expiryMonth: 12, expiryYear: 2030, cvv: "123", balance: 100)

        let summary = CardPortfolioSummary(cards: [active, expiringSoon, invalid], referenceDate: referenceDate, calendar: calendar)

        XCTAssertEqual(summary.totalCards, 3)
        XCTAssertEqual(summary.activeCards, 1)
        XCTAssertEqual(summary.expiringSoonCards, 1)
        XCTAssertEqual(summary.invalidCards, 1)
        XCTAssertEqual(summary.attentionCount, 2)
        XCTAssertEqual(summary.totalBalance, 600)
        XCTAssertEqual(summary.averageBalance, 200)
        XCTAssertEqual(summary.highestBalanceCard?.holderName, "Active")
        XCTAssertEqual(summary.brandCounts["Visa"], 1)
    }
}
