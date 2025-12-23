import XCTest
@testable import BankAppCore

final class PersistenceTests: XCTestCase {
    // Note: Testing persistence involving FileManager usually requires mocking or isolated temp directories.
    // For this simple test, we will verify the logic of serialization/deserialization
    // by using the PersistenceController but checking if we can save and load back.

    // WARNING: This test runs against the actual file system in the sandbox.
    // In a real production app we would inject a temporary URL.

    func testSaveAndLoadCards() {
        let controller = PersistenceController.shared
        let originalCards = controller.loadCards()

        let newCard = Card(holderName: "Test User", prefix: "123", number: "123456", expiryMonth: 1, expiryYear: 2025, cvv: "123", balance: 100)

        controller.save(cards: [newCard])

        // Allow some time for async save
        let expectation = XCTestExpectation(description: "Save")
        DispatchQueue.global().asyncAfter(deadline: .now() + 1.0) {
            let loaded = controller.loadCards()
            if loaded.contains(where: { $0.id == newCard.id }) {
                expectation.fulfill()
            }
            // Restore original
            controller.save(cards: originalCards)
        }

        wait(for: [expectation], timeout: 2.0)
    }
}
