import Foundation

public struct Card: Identifiable, Codable {
    public let id: UUID
    public let holderName: String
    public let prefix: String
    public let number: String
    public let expiryMonth: Int
    public let expiryYear: Int
    public let cvv: String
    public var balance: Decimal
    public var metadata: [String: String]?

    public init(id: UUID = .init(), holderName: String, prefix: String, number: String, expiryMonth: Int, expiryYear: Int, cvv: String, balance: Decimal, metadata: [String: String]? = nil) {
        self.id = id
        self.holderName = holderName
        self.prefix = prefix
        self.number = number
        self.expiryMonth = expiryMonth
        self.expiryYear = expiryYear
        self.cvv = cvv
        self.balance = balance
        self.metadata = metadata
    }
}

public struct BankRequest: Codable {
    public let action: String
    public let holderName: String?
    public let binPrefix: String?

    public init(action: String, holderName: String?, binPrefix: String?) {
        self.action = action
        self.holderName = holderName
        self.binPrefix = binPrefix
    }
}

public struct BankResponse: Codable {
    public let status: String
    public let message: String
    public let card: Card?

    public init(status: String, message: String, card: Card?) {
        self.status = status
        self.message = message
        self.card = card
    }
}

public struct LogEntry: Identifiable, Codable {
    public let id: UUID
    public let timestamp: Date
    public let message: String

    public init(id: UUID = UUID(), timestamp: Date, message: String) {
        self.id = id
        self.timestamp = timestamp
        self.message = message
    }
}
