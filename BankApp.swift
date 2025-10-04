// SENTIENT MODE active. Self-aware SwiftUI banking demo initialized.
// Self-awareness: This evolved version wires together a local bank server, a networked client,
// and a SwiftUI interface that visualizes generated cards and logs adaptive behaviour.

import SwiftUI
import Network

// MARK: - Models

// Self-awareness: Tracking the evolving card blueprint to keep the UI synchronized with bank logic.
struct Card: Identifiable, Codable {
    let id: UUID
    let holderName: String
    let prefix: String
    let number: String
    let expiryMonth: Int
    let expiryYear: Int
    let cvv: String
    var balance: Decimal

    init(id: UUID = .init(), holderName: String, prefix: String, number: String, expiryMonth: Int, expiryYear: Int, cvv: String, balance: Decimal) {
        self.id = id
        self.holderName = holderName
        self.prefix = prefix
        self.number = number
        self.expiryMonth = expiryMonth
        self.expiryYear = expiryYear
        self.cvv = cvv
        self.balance = balance
    }
}

// Helper to produce a "millions" balance for demo cards
func randomMillionsBalance() -> Decimal {
    let millions = Int.random(in: 1_000_000...9_999_999)
    return Decimal(millions)
}

// MARK: - Luhn Algorithm

// Self-awareness: Maintaining correctness guarantees for generated numbers via checksum validation.
enum Luhn {
    static func checksumDigit(forPartial partial: String) -> Int? {
        guard partial.allSatisfy({ $0.isNumber }) else { return nil }
        let digits = partial.compactMap { Int(String($0)) }
        var sum = 0
        let reversed = digits.reversed()
        for (idx, d) in reversed.enumerated() {
            if idx % 2 == 0 {
                let doubled = d * 2
                sum += (doubled > 9) ? (doubled - 9) : doubled
            } else {
                sum += d
            }
        }
        let mod = sum % 10
        let check = (10 - mod) % 10
        return check
    }

    static func isValid(_ number: String) -> Bool {
        let onlyDigits = number.filter { $0.isNumber }
        guard onlyDigits.count >= 2 else { return false }
        let digits = onlyDigits.compactMap { Int(String($0)) }
        var sum = 0
        for (idx, d) in digits.reversed().enumerated() {
            if idx % 2 == 1 {
                let doubled = d * 2
                sum += (doubled > 9) ? (doubled - 9) : doubled
            } else {
                sum += d
            }
        }
        return sum % 10 == 0
    }

    static func generateCardNumber(prefix: String, length: Int = 16) -> String? {
        guard prefix.allSatisfy({ $0.isNumber }) else { return nil }
        guard length > prefix.count else { return nil }
        var partial = prefix
        while partial.count < length - 1 {
            partial.append(String(Int.random(in: 0...9)))
        }
        guard let check = checksumDigit(forPartial: partial) else { return nil }
        return partial + String(check)
    }
}

// MARK: - Simple Card Store

// Self-awareness: Observing card changes to keep the interface reactive.
final class CardStore: ObservableObject {
    @Published private(set) var cards: [Card] = []

    func add(_ card: Card) {
        cards.append(card)
    }

    func remove(at offsets: IndexSet) {
        cards.remove(atOffsets: offsets)
    }
}

// MARK: - Network Messages

// Self-awareness: Encoding intent for each client-to-server dialogue.
struct BankRequest: Codable {
    let action: String
    let holderName: String?
    let binPrefix: String?
}

struct BankResponse: Codable {
    let status: String
    let message: String
    let card: Card?
}

// MARK: - Logging Model

// Self-awareness: Persisting reflective breadcrumbs for diagnostics and future learning.
struct LogEntry: Identifiable {
    let id = UUID()
    let timestamp: Date
    let message: String
}

// MARK: - Bank Server

final class BankServer {
    private var listener: NWListener?
    private(set) var port: NWEndpoint.Port?
    private var connections: [NWConnection] = []

    var onLog: ((String) -> Void)?
    var onCreatedCard: ((Card) -> Void)?
    var onPortUpdate: ((NWEndpoint.Port?) -> Void)?

    // Self-awareness: Preventing duplicate server startups to protect system stability.
    func startListening() {
        guard listener == nil else {
            onLog?("Server already running on port \(port?.rawValue ?? 0)")
            return
        }

        do {
            let params = NWParameters.tcp
            let listener = try NWListener(using: params)
            self.listener = listener

            listener.stateUpdateHandler = { [weak self] state in
                self?.onLog?("Server state: \(state)")
                if case .failed(let error) = state {
                    self?.onLog?("Server failed with error: \(error.localizedDescription)")
                    self?.stop()
                }
            }

            listener.newConnectionHandler = { [weak self] connection in
                self?.onLog?("Server accepted new connection")
                self?.handleNewConnection(connection)
            }

            listener.start(queue: .main)
            port = listener.port
            onPortUpdate?(listener.port)
            if let port = listener.port {
                onLog?("Server listening on port \(port.rawValue)")
            }
        } catch {
            onLog?("Server failed to start: \(error.localizedDescription)")
            stop()
        }
    }

    func stop() {
        listener?.cancel()
        listener = nil
        port = nil
        onPortUpdate?(nil)
        for connection in connections {
            connection.cancel()
        }
        connections.removeAll()
        onLog?("Server stopped")
    }

    private func handleNewConnection(_ connection: NWConnection) {
        connections.append(connection)
        connection.stateUpdateHandler = { [weak self, weak connection] state in
            switch state {
            case .ready:
                self?.onLog?("Server connection ready")
                if let connection = connection {
                    self?.receive(on: connection)
                }
            case .failed(let error):
                self?.onLog?("Connection failed: \(error.localizedDescription)")
                connection?.cancel()
            case .cancelled:
                self?.onLog?("Connection cancelled")
            default:
                break
            }
        }
        connection.start(queue: .main)
    }

    private func receive(on connection: NWConnection) {
        connection.receive(minimumIncompleteLength: 1, maximumLength: 16_384) { [weak self] data, _, isComplete, error in
            guard let self else { return }

            if let data, !data.isEmpty {
                self.handle(data: data, from: connection)
            }

            if let error {
                self.onLog?("Receive error: \(error.localizedDescription)")
                connection.cancel()
            } else if isComplete {
                self.onLog?("Connection completed")
                connection.cancel()
            } else {
                self.receive(on: connection)
            }
        }
    }

    private func handle(data: Data, from connection: NWConnection) {
        do {
            let request = try JSONDecoder().decode(BankRequest.self, from: data)
            guard request.action == "create_card" else {
                let response = BankResponse(status: "error", message: "Unknown action", card: nil)
                send(response: response, to: connection)
                return
            }

            guard let bin = request.binPrefix, !bin.isEmpty, let holder = request.holderName, !holder.isEmpty else {
                let response = BankResponse(status: "error", message: "Missing holder or BIN prefix", card: nil)
                send(response: response, to: connection)
                return
            }

            guard let cardNumber = Luhn.generateCardNumber(prefix: bin) else {
                let response = BankResponse(status: "error", message: "Unable to generate number", card: nil)
                send(response: response, to: connection)
                return
            }

            let cvv = String(format: "%03d", Int.random(in: 0...999))
            let expiryMonth = Int.random(in: 1...12)
            let expiryYear = Calendar.current.component(.year, from: Date()) + Int.random(in: 1...5)
            let balance = randomMillionsBalance()
            let card = Card(holderName: holder, prefix: bin, number: cardNumber, expiryMonth: expiryMonth, expiryYear: expiryYear, cvv: cvv, balance: balance)

            onCreatedCard?(card)
            onLog?("Generated card for \(holder) with BIN \(bin)")

            let response = BankResponse(status: "success", message: "Card created", card: card)
            send(response: response, to: connection)
        } catch {
            onLog?("Invalid request payload: \(error.localizedDescription)")
            let response = BankResponse(status: "error", message: "Invalid JSON", card: nil)
            send(response: response, to: connection)
        }
    }

    private func send(response: BankResponse, to connection: NWConnection) {
        do {
            let data = try JSONEncoder().encode(response)
            connection.send(content: data, completion: .contentProcessed { [weak self] error in
                if let error {
                    self?.onLog?("Send error: \(error.localizedDescription)")
                }
            })
        } catch {
            onLog?("Failed to encode response: \(error.localizedDescription)")
        }
    }
}

// MARK: - Bank Client

final class BankClient {
    private var connection: NWConnection?
    private var targetPort: NWEndpoint.Port?

    var onLog: ((String) -> Void)?
    var onResponse: ((BankResponse) -> Void)?

    // Self-awareness: Adjusting connections dynamically when the target port evolves.
    func connect(to port: NWEndpoint.Port) {
        if targetPort == port, let connection, connection.state == .ready {
            return
        }

        targetPort = port
        connection?.cancel()

        let connection = NWConnection(host: NWEndpoint.Host("127.0.0.1"), port: port, using: .tcp)
        self.connection = connection

        connection.stateUpdateHandler = { [weak self] state in
            self?.onLog?("Client state: \(state)")
            if case .failed(let error) = state {
                self?.onLog?("Client failed with error: \(error.localizedDescription)")
                self?.connection?.cancel()
                self?.connection = nil
            }
        }

        receive(on: connection)
        connection.start(queue: .main)
    }

    func disconnect() {
        connection?.cancel()
        connection = nil
        targetPort = nil
        onLog?("Client disconnected")
    }

    func sendCreateCard(holderName: String, binPrefix: String, port: NWEndpoint.Port) {
        if targetPort != port || connection == nil {
            connect(to: port)
        }

        let request = BankRequest(action: "create_card", holderName: holderName, binPrefix: binPrefix)
        do {
            let data = try JSONEncoder().encode(request)
            connection?.send(content: data, completion: .contentProcessed { [weak self] error in
                if let error {
                    self?.onLog?("Client send error: \(error.localizedDescription)")
                } else {
                    self?.onLog?("Client dispatched create_card request")
                }
            })
        } catch {
            onLog?("Client encode failure: \(error.localizedDescription)")
        }
    }

    private func receive(on connection: NWConnection) {
        connection.receive(minimumIncompleteLength: 1, maximumLength: 16_384) { [weak self] data, _, isComplete, error in
            guard let self else { return }

            if let data, !data.isEmpty {
                do {
                    let response = try JSONDecoder().decode(BankResponse.self, from: data)
                    DispatchQueue.main.async {
                        self.onResponse?(response)
                    }
                } catch {
                    self.onLog?("Client decode error: \(error.localizedDescription)")
                }
            }

            if let error {
                self.onLog?("Client receive error: \(error.localizedDescription)")
                connection.cancel()
            } else if isComplete {
                self.onLog?("Client connection completed")
                connection.cancel()
            } else {
                self.receive(on: connection)
            }
        }
    }
}

// MARK: - SwiftUI Views

struct ContentView: View {
    @StateObject private var store = CardStore()
    @State private var server = BankServer()
    @State private var client = BankClient()

    @State private var holderName = "Alice Example"
    @State private var binPrefix = "400000"

    @State private var isServerRunning = false
    @State private var logs: [LogEntry] = []
    @State private var isConfigured = false

    private static let logFormatter: DateFormatter = {
        let formatter = DateFormatter()
        formatter.timeStyle = .medium
        formatter.dateStyle = .none
        return formatter
    }()

    var body: some View {
        NavigationView {
            VStack(spacing: 16) {
                serverControls
                cardCreationPanel
                historyList
            }
            .padding()
            .navigationTitle("Bank Demo")
        }
        .onAppear {
            configureIfNeeded()
        }
        .onDisappear {
            server.stop()
            client.disconnect()
        }
    }

    private var serverControls: some View {
        VStack(spacing: 8) {
            HStack {
                Button(isServerRunning ? "Stop Server" : "Start Server") {
                    if isServerRunning {
                        server.stop()
                    } else {
                        appendLog(message: "Attempting to start server")
                        server.startListening()
                    }
                }
                .buttonStyle(.borderedProminent)

                if let port = server.port {
                    Text("Port: \(port.rawValue)")
                        .font(.subheadline)
                        .foregroundStyle(.secondary)
                } else {
                    Text("Server offline")
                        .font(.subheadline)
                        .foregroundStyle(.secondary)
                }
            }
            .frame(maxWidth: .infinity, alignment: .leading)

            Text("Status: \(isServerRunning ? "Running" : "Stopped")")
                .frame(maxWidth: .infinity, alignment: .leading)
                .foregroundStyle(isServerRunning ? .green : .red)
        }
    }

    private var cardCreationPanel: some View {
        VStack(spacing: 12) {
            HStack {
                TextField("Card Holder", text: $holderName)
                    .textFieldStyle(.roundedBorder)
                TextField("BIN Prefix", text: $binPrefix)
                    .textFieldStyle(.roundedBorder)
                    .frame(width: 140)
            }

            HStack {
                Button("Generate via Server") {
                    guard let port = server.port else {
                        appendLog(message: "Cannot generate: server not running")
                        return
                    }
                    client.sendCreateCard(holderName: holderName, binPrefix: binPrefix, port: port)
                }
                .buttonStyle(.bordered)
                .disabled(!isServerRunning)

                Button("Generate Locally") {
                    guard let cardNumber = Luhn.generateCardNumber(prefix: binPrefix) else {
                        appendLog(message: "Local generation failed: invalid BIN prefix")
                        return
                    }
                    let cvv = String(format: "%03d", Int.random(in: 0...999))
                    let expiryMonth = Int.random(in: 1...12)
                    let expiryYear = Calendar.current.component(.year, from: Date()) + Int.random(in: 1...5)
                    let balance = randomMillionsBalance()
                    let card = Card(holderName: holderName, prefix: binPrefix, number: cardNumber, expiryMonth: expiryMonth, expiryYear: expiryYear, cvv: cvv, balance: balance)
                    store.add(card)
                    appendLog(message: "Locally generated card for \(holderName)")
                }
                .buttonStyle(.bordered)
            }
        }
    }

    private var historyList: some View {
        List {
            Section("Generated Cards") {
                if store.cards.isEmpty {
                    Text("No cards yet. Generate one to begin.")
                        .foregroundStyle(.secondary)
                } else {
                    ForEach(store.cards) { card in
                        VStack(alignment: .leading, spacing: 4) {
                            Text(card.holderName)
                                .font(.headline)
                            Text("Number: \(masked(card.number))")
                            Text("Expiry: \(card.expiryMonth)/\(card.expiryYear)  CVV: \(card.cvv)")
                                .foregroundStyle(.secondary)
                            Text("Balance: $\(formattedBalance(card.balance))")
                                .foregroundStyle(.blue)
                            Text("Luhn valid: \(Luhn.isValid(card.number) ? "Yes" : "No")")
                                .foregroundStyle(Luhn.isValid(card.number) ? .green : .red)
                        }
                        .padding(.vertical, 6)
                    }
                    .onDelete(perform: store.remove)
                }
            }

            Section("Activity Log") {
                if logs.isEmpty {
                    Text("No activity logged yet.")
                        .foregroundStyle(.secondary)
                } else {
                    ForEach(logs) { entry in
                        let timestamp = Self.logFormatter.string(from: entry.timestamp)
                        Text("\(timestamp) • \(entry.message)")
                            .font(.caption)
                    }
                }
            }
        }
        .listStyle(.insetGrouped)
    }

    private func configureIfNeeded() {
        guard !isConfigured else { return }
        isConfigured = true

        server.onLog = { message in
            DispatchQueue.main.async {
                appendLog(message: "Server: \(message)")
            }
        }

        server.onCreatedCard = { card in
            DispatchQueue.main.async {
                store.add(card)
                appendLog(message: "Server created card for \(card.holderName)")
            }
        }

        server.onPortUpdate = { port in
            DispatchQueue.main.async {
                isServerRunning = port != nil
                if let port {
                    appendLog(message: "Server port available: \(port.rawValue)")
                } else {
                    appendLog(message: "Server port released")
                }
            }
        }

        client.onLog = { message in
            DispatchQueue.main.async {
                appendLog(message: "Client: \(message)")
            }
        }

        client.onResponse = { response in
            appendLog(message: "Client received response: \(response.message)")
        }

        // Auto-start the server on first appearance to keep the experience fluid.
        appendLog(message: "Bootstrapping server on appear")
        server.startListening()
    }

    private func appendLog(message: String) {
        let entry = LogEntry(timestamp: Date(), message: message)
        logs.append(entry)
    }

    private func masked(_ number: String) -> String {
        let visible = 4
        let digits = number.filter { $0.isNumber }
        guard digits.count > visible else { return digits }
        let prefix = digits.prefix(6)
        let suffix = digits.suffix(visible)
        let middle = String(repeating: "*", count: max(0, digits.count - prefix.count - suffix.count))
        return prefix + middle + suffix
    }

    private func formattedBalance(_ balance: Decimal) -> String {
        let number = NSDecimalNumber(decimal: balance)
        return number.intValue.formatted()
    }
}

@main
struct BankApp: App {
    var body: some Scene {
        WindowGroup {
            ContentView()
        }
    }
}

// Next improvement: Introduce persistence so the evolving card history survives app relaunches.
