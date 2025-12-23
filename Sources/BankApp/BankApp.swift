import SwiftUI
import Network
import BankAppCore

struct ContentView: View {
    private enum ClearAction: String, Identifiable {
        case cards
        case logs

        var id: String { rawValue }

        var confirmationMessage: String {
            switch self {
            case .cards:
                return "This removes all saved cards from the history."
            case .logs:
                return "This clears the recorded activity log."
            }
        }
    }

    @StateObject private var store = CardStore()
    @State private var server = BankServer()
    @State private var client = BankClient()

    @State private var holderName = "Alice Example"
    @State private var binPrefix = "400000"

    @State private var isServerRunning = false
    @State private var logs: [LogEntry] = PersistenceController.shared.loadLogEntries()
    @State private var isConfigured = false
    @State private var pendingClearAction: ClearAction?

    private let persistence = PersistenceController.shared
    private let logLimit = 200

    private var binValidationMessage: String? {
        if binPrefix.isEmpty {
            return "Enter a BIN prefix to get started."
        }
        if !binPrefix.allSatisfy({ $0.isNumber }) {
            return "BIN prefix must contain digits only."
        }
        if binPrefix.count != 6 {
            return "BIN prefixes are typically 6 digits long."
        }
        return nil
    }

    private var isBinValid: Bool { binValidationMessage == nil }

    private static let logFormatter: DateFormatter = {
        let formatter = DateFormatter()
        formatter.timeStyle = .medium
        formatter.dateStyle = .none
        return formatter
    }()

    private static let currencyFormatter: NumberFormatter = {
        let formatter = NumberFormatter()
        formatter.numberStyle = .currency
        formatter.maximumFractionDigits = 2
        formatter.minimumFractionDigits = 0
        return formatter
    }()

    var body: some View {
        NavigationStack {
            VStack(spacing: 16) {
                ViewThatFits {
                    HStack(alignment: .top, spacing: 20) {
                        serverControls
                        Divider()
                        cardCreationPanel
                    }
                    VStack(spacing: 16) {
                        serverControls
                        cardCreationPanel
                    }
                }
                historyList
            }
            .padding()
            .navigationTitle("Bank Demo")
            .toolbar {
                ToolbarItem(placement: .navigationBarTrailing) {
                    Menu {
                        Button("Clear Saved Cards", role: .destructive) {
                            pendingClearAction = .cards
                        }
                        Button("Clear Activity Log", role: .destructive) {
                            pendingClearAction = .logs
                        }

                        if !store.cards.isEmpty {
                            ShareLink(item: store.cards.generateCSV(), preview: SharePreview("Export Cards", image: Image(systemName: "creditcard"))) {
                                Label("Export CSV", systemImage: "square.and.arrow.up")
                            }
                        }
                    } label: {
                        Label("More options", systemImage: "ellipsis.circle")
                    }
                }
            }
        }
        .onAppear {
            configureIfNeeded()
        }
        .onDisappear {
            server.stop()
            client.disconnect()
        }
        .confirmationDialog("Are you sure?", item: $pendingClearAction) { action in
            Button("Clear", role: .destructive) {
                performClearAction(action)
            }
        } message: { action in
            Text(action.confirmationMessage)
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
                    .onChange(of: binPrefix) { newValue in
                        let filtered = newValue.filter { $0.isNumber }
                        let limited = String(filtered.prefix(6))
                        if limited != newValue {
                            binPrefix = limited
                        }
                    }
            }

            if let message = binValidationMessage {
                Text(message)
                    .font(.footnote)
                    .foregroundStyle(.red)
                    .frame(maxWidth: .infinity, alignment: .leading)
            }

            HStack {
                Button("Generate via Server") {
                    guard isBinValid else {
                        appendLog(message: "Cannot generate: BIN prefix invalid")
                        return
                    }
                    guard let port = server.port else {
                        appendLog(message: "Cannot generate: server not running")
                        return
                    }
                    client.sendCreateCard(holderName: holderName, binPrefix: binPrefix, port: port)
                }
                .buttonStyle(.bordered)
                .disabled(!isServerRunning || !isBinValid)

                Button("Generate Locally") {
                    guard isBinValid else {
                        appendLog(message: "Local generation failed: invalid BIN prefix")
                        return
                    }
                    guard let cardNumber = Luhn.generateCardNumber(prefix: binPrefix) else {
                        appendLog(message: "Local generation failed: invalid BIN prefix")
                        return
                    }
                    let cvv = String(format: "%03d", Int.random(in: 0...999))
                    let expiryMonth = Int.random(in: 1...12)
                    let expiryYear = Calendar.current.component(.year, from: Date()) + Int.random(in: 1...5)
                    let balance = randomMillionsBalance()
                    let card = Card(holderName: holderName, prefix: binPrefix, number: cardNumber, expiryMonth: expiryMonth, expiryYear: expiryYear, cvv: cvv, balance: balance)
                    if store.add(card) {
                        appendLog(message: "Locally generated card for \(holderName)")
                    } else {
                        appendLog(message: "Skipped local generation: duplicate card number")
                    }
                }
                .buttonStyle(.bordered)
                .disabled(!isBinValid)
            }
        }
    }

    private var historyList: some View {
        List {
            if !store.cards.isEmpty {
                Section("Summary") {
                    summaryRow(title: "Cards on file", value: "\(store.cards.count)")
                    summaryRow(title: "Total balance", value: formattedBalance(totalBalance()))
                    if let average = averageBalance() {
                        summaryRow(title: "Average balance", value: formattedBalance(average))
                    }
                }
            }

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
                            Text("Balance: \(formattedBalance(card.balance))")
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
                if store.add(card) {
                    appendLog(message: "Server created card for \(card.holderName)")
                } else {
                    appendLog(message: "Server duplicate ignored for \(card.holderName)")
                }
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

        if !store.cards.isEmpty {
            appendLog(message: "Restored \(store.cards.count) saved card(s) from storage")
        }
        if !logs.isEmpty {
            appendLog(message: "Resumed with \(logs.count) historical log entr\(logs.count == 1 ? "y" : "ies")")
        }
        appendLog(message: "Bootstrapping server on appear")
        server.startListening()
    }

    private func appendLog(message: String) {
        let entry = LogEntry(timestamp: Date(), message: message)
        logs.append(entry)
        trimLogsIfNeeded()
        persistence.save(logEntries: logs)
    }

    private func clearActivityLog() {
        logs.removeAll()
        persistence.save(logEntries: logs)
    }

    private func performClearAction(_ action: ClearAction) {
        switch action {
        case .cards:
            store.removeAll()
            appendLog(message: "Cleared saved card history")
        case .logs:
            clearActivityLog()
        }
        pendingClearAction = nil
    }

    private func totalBalance() -> Decimal {
        store.cards.reduce(Decimal.zero) { $0 + $1.balance }
    }

    private func averageBalance() -> Decimal? {
        guard !store.cards.isEmpty else { return nil }
        let total = totalBalance()
        let count = Decimal(store.cards.count)
        return total / count
    }

    private func trimLogsIfNeeded() {
        guard logs.count > logLimit else { return }
        let overflow = logs.count - logLimit
        logs.removeFirst(overflow)
    }

    private func summaryRow(title: String, value: String) -> some View {
        HStack {
            Text(title)
            Spacer()
            Text(value)
                .fontWeight(.semibold)
        }
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
        return Self.currencyFormatter.string(from: number) ?? number.stringValue
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
