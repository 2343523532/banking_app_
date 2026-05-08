import Foundation

public enum CardHealthStatus: String, Codable, Equatable {
    case active = "Active"
    case expiringSoon = "Expiring Soon"
    case expired = "Expired"
    case invalidNumber = "Invalid Number"
}

public struct CardPortfolioSummary {
    public let totalCards: Int
    public let activeCards: Int
    public let expiredCards: Int
    public let expiringSoonCards: Int
    public let invalidCards: Int
    public let totalBalance: Decimal
    public let averageBalance: Decimal?
    public let highestBalanceCard: Card?
    public let brandCounts: [String: Int]

    public var attentionCount: Int {
        expiredCards + expiringSoonCards + invalidCards
    }

    public init(cards: [Card], referenceDate: Date = Date(), calendar: Calendar = .current) {
        totalCards = cards.count
        totalBalance = cards.reduce(Decimal.zero) { $0 + $1.balance }
        averageBalance = cards.isEmpty ? nil : totalBalance / Decimal(cards.count)
        highestBalanceCard = cards.max { $0.balance < $1.balance }
        brandCounts = Dictionary(grouping: cards, by: { $0.brandName }).mapValues(\.count)

        var active = 0
        var expired = 0
        var expiringSoon = 0
        var invalid = 0

        for card in cards {
            switch card.healthStatus(referenceDate: referenceDate, calendar: calendar) {
            case .active:
                active += 1
            case .expiringSoon:
                expiringSoon += 1
            case .expired:
                expired += 1
            case .invalidNumber:
                invalid += 1
            }
        }

        activeCards = active
        expiredCards = expired
        expiringSoonCards = expiringSoon
        invalidCards = invalid
    }

    public var brandSummary: String {
        brandCounts
            .map { "\($0.key): \($0.value)" }
            .sorted()
            .joined(separator: " • ")
    }
}

public extension Card {
    func healthStatus(referenceDate: Date = Date(), calendar: Calendar = .current, soonThresholdMonths: Int = 2) -> CardHealthStatus {
        guard isLikelyValid else { return .invalidNumber }
        guard !isExpired(referenceDate: referenceDate, calendar: calendar) else { return .expired }
        guard let expiryDate = calendar.date(from: DateComponents(year: expiryYear, month: expiryMonth, day: 1)),
              let thresholdDate = calendar.date(byAdding: .month, value: soonThresholdMonths, to: referenceDate) else {
            return .expired
        }
        return expiryDate <= thresholdDate ? .expiringSoon : .active
    }
}
