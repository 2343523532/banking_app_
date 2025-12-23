import Foundation
import Combine

// Helper to produce a "millions" balance for demo cards
public func randomMillionsBalance() -> Decimal {
    let millions = Int.random(in: 1_000_000...9_999_999)
    return Decimal(millions)
}

public final class CardStore: ObservableObject {
    @Published public private(set) var cards: [Card] = [] {
        didSet {
            guard !isLoading else { return }
            persistence.save(cards: cards)
        }
    }

    private let persistence = PersistenceController.shared
    private var isLoading = false

    public init() {
        isLoading = true
        cards = CardStore.uniqueCards(from: persistence.loadCards())
        isLoading = false
    }

    @discardableResult
    public func add(_ card: Card) -> Bool {
        guard !cards.contains(where: { $0.number == card.number }) else { return false }
        cards.append(card)
        return true
    }

    public func remove(at offsets: IndexSet) {
        cards.remove(atOffsets: offsets)
    }

    public func removeAll() {
        cards.removeAll()
    }

    private static func uniqueCards(from cards: [Card]) -> [Card] {
        var seenNumbers = Set<String>()
        return cards.filter { card in
            let inserted = seenNumbers.insert(card.number).inserted
            return inserted
        }
    }
}
