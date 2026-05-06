import Foundation

public enum CardBrand: String, CaseIterable, Codable, Identifiable {
    case visa = "Visa"
    case mastercard = "Mastercard"
    case americanExpress = "American Express"
    case discover = "Discover"
    case unknown = "Unknown"

    public var id: String { rawValue }

    public var defaultTestPrefix: String {
        switch self {
        case .visa:
            return "400000"
        case .mastercard:
            return "555555"
        case .americanExpress:
            return "378282"
        case .discover:
            return "601111"
        case .unknown:
            return "999999"
        }
    }

    public var cardNumberLength: Int {
        switch self {
        case .americanExpress:
            return 15
        default:
            return 16
        }
    }

    public static var selectableTestBrands: [CardBrand] {
        [.visa, .mastercard, .americanExpress, .discover]
    }
}

public struct CardGenerationOptions: Equatable {
    public let holderName: String
    public let binPrefix: String
    public let brand: CardBrand?
    public let balanceRange: ClosedRange<Int>
    public let metadata: [String: String]

    public init(holderName: String, binPrefix: String, brand: CardBrand? = nil, balanceRange: ClosedRange<Int> = 1_000_000...9_999_999, metadata: [String: String] = [:]) {
        self.holderName = holderName
        self.binPrefix = binPrefix
        self.brand = brand
        self.balanceRange = balanceRange
        self.metadata = metadata
    }
}

public enum CardGenerationError: LocalizedError, Equatable {
    case emptyHolderName
    case invalidPrefix
    case unsupportedLength
    case invalidBalanceRange

    public var errorDescription: String? {
        switch self {
        case .emptyHolderName:
            return "Enter a card holder name."
        case .invalidPrefix:
            return "BIN prefix must contain 1 to 12 digits."
        case .unsupportedLength:
            return "BIN prefix is too long for the selected brand."
        case .invalidBalanceRange:
            return "Balance range must contain positive values."
        }
    }
}

public enum CardGenerator {
    public static let testDataNotice = "Demo test data only — generated numbers are for local development and must not be used as real payment credentials."

    public static func sanitizedHolderName(_ holderName: String) -> String {
        holderName.trimmingCharacters(in: .whitespacesAndNewlines)
    }

    public static func sanitizedPrefix(_ prefix: String) -> String {
        String(prefix.filter { $0.isNumber }.prefix(12))
    }

    public static func validationMessage(forPrefix prefix: String, expectedLength: Int = 6) -> String? {
        if prefix.isEmpty {
            return "Enter a BIN prefix to get started."
        }
        if !prefix.allSatisfy({ $0.isNumber }) {
            return "BIN prefix must contain digits only."
        }
        if prefix.count != expectedLength {
            return "BIN prefixes are typically \(expectedLength) digits long."
        }
        return nil
    }

    public static func makeCard(options: CardGenerationOptions, calendar: Calendar = .current, now: Date = Date()) throws -> Card {
        let holder = sanitizedHolderName(options.holderName)
        guard !holder.isEmpty else { throw CardGenerationError.emptyHolderName }

        let prefix = sanitizedPrefix(options.binPrefix)
        guard !prefix.isEmpty, prefix == options.binPrefix, (1...12).contains(prefix.count) else {
            throw CardGenerationError.invalidPrefix
        }

        let brand = options.brand ?? CardBrand(rawValue: Luhn.cardBrand(for: prefix)) ?? .unknown
        let length = brand.cardNumberLength
        guard prefix.count < length else { throw CardGenerationError.unsupportedLength }
        guard options.balanceRange.lowerBound > 0, options.balanceRange.lowerBound <= options.balanceRange.upperBound else {
            throw CardGenerationError.invalidBalanceRange
        }

        guard let cardNumber = Luhn.generateCardNumber(prefix: prefix, length: length) else {
            throw CardGenerationError.unsupportedLength
        }

        let expiryMonth = Int.random(in: 1...12)
        let currentYear = calendar.component(.year, from: now)
        let expiryYear = currentYear + Int.random(in: 1...5)
        let cvvWidth = brand == .americanExpress ? 4 : 3
        let cvvUpperBound = Int(pow(10.0, Double(cvvWidth))) - 1
        let cvv = String(format: "%0\(cvvWidth)d", Int.random(in: 0...cvvUpperBound))
        let balance = Decimal(Int.random(in: options.balanceRange))

        var metadata = options.metadata
        metadata["brand"] = brand.rawValue
        metadata["environment"] = "sandbox"
        metadata["notice"] = testDataNotice

        return Card(holderName: holder, prefix: prefix, number: cardNumber, expiryMonth: expiryMonth, expiryYear: expiryYear, cvv: cvv, balance: balance, metadata: metadata)
    }
}
