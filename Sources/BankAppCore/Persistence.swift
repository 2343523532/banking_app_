import Foundation

/// Handles persistence for generated cards and activity logs.
/// This lightweight controller stores data in the user's documents directory so
/// the SwiftUI demo remembers previously generated cards across launches.
public final class PersistenceController {
    public static let shared = PersistenceController()

    private let fileManager = FileManager.default
    private let queue = DispatchQueue(label: "PersistenceController", qos: .background)
    private let encoder: JSONEncoder
    private let decoder: JSONDecoder

    private let cardsURL: URL
    private let logsURL: URL

    private init() {
        encoder = JSONEncoder()
        encoder.outputFormatting = [.prettyPrinted, .sortedKeys]
        encoder.dateEncodingStrategy = .iso8601

        decoder = JSONDecoder()
        decoder.dateDecodingStrategy = .iso8601

        let directory = PersistenceController.makeStorageDirectory(fileManager: fileManager)
        cardsURL = directory.appendingPathComponent("cards.json")
        logsURL = directory.appendingPathComponent("logs.json")
    }

    public func loadCards() -> [Card] {
        guard let data = try? Data(contentsOf: cardsURL) else { return [] }
        do {
            return try decoder.decode([Card].self, from: data)
        } catch {
            print("Failed to decode cards: \(error)")
            return []
        }
    }

    public func save(cards: [Card]) {
        let snapshot = cards
        queue.async { [weak self] in
            guard let self else { return }
            do {
                let data = try self.encoder.encode(snapshot)
                try data.write(to: self.cardsURL, options: [.atomic])
            } catch {
                print("Failed to persist cards: \(error)")
            }
        }
    }

    public func loadLogEntries() -> [LogEntry] {
        guard let data = try? Data(contentsOf: logsURL) else { return [] }
        do {
            return try decoder.decode([LogEntry].self, from: data)
        } catch {
            print("Failed to decode logs: \(error)")
            return []
        }
    }

    public func save(logEntries: [LogEntry]) {
        let snapshot = logEntries
        queue.async { [weak self] in
            guard let self else { return }
            do {
                let data = try self.encoder.encode(snapshot)
                try data.write(to: self.logsURL, options: [.atomic])
            } catch {
                print("Failed to persist logs: \(error)")
            }
        }
    }

    private static func makeStorageDirectory(fileManager: FileManager) -> URL {
        let base = fileManager.urls(for: .documentDirectory, in: .userDomainMask).first ?? fileManager.temporaryDirectory
        let directory = base.appendingPathComponent("BankApp", isDirectory: true)
        if !fileManager.fileExists(atPath: directory.path) {
            do {
                try fileManager.createDirectory(at: directory, withIntermediateDirectories: true)
            } catch {
                print("Failed to create persistence directory: \(error)")
            }
        }
        return directory
    }
}
