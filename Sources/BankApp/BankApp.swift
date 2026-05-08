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
    @State private var selectedBrand: CardBrand = .visa
    @State private var binPrefix = CardBrand.visa.defaultTestPrefix
    @State private var searchText = ""

    @State private var isServerRunning = false
    @State private var logs: [LogEntry] = PersistenceController.shared.loadLogEntries()
    @State private var isConfigured = false
    @State private var pendingClearAction: ClearAction?

    private let persistence = PersistenceController.shared
    private let logLimit = 200

    private var binValidationMessage: String? {
        CardGenerator.validationMessage(forPrefix: binPrefix)
    }

    private var isBinValid: Bool { binValidationMessage == nil }

    private var filteredCards: [Card] {
        let query = searchText.trimmingCharacters(in: .whitespacesAndNewlines).lowercased()
        guard !query.isEmpty else { return store.cards }
        return store.cards.filter { card in
            card.displayName.lowercased().contains(query)
                || card.maskedNumber.contains(query)
                || card.brandName.lowercased().contains(query)
                || card.healthStatus().rawValue.lowercased().contains(query)
        }
    }

    private var portfolioSummary: CardPortfolioSummary {
        CardPortfolioSummary(cards: store.cards)
    }

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
            Text(CardGenerator.testDataNotice)
                .font(.footnote)
                .foregroundStyle(.secondary)
                .frame(maxWidth: .infinity, alignment: .leading)

            Picker("Test Brand", selection: $selectedBrand) {
                ForEach(CardBrand.selectableTestBrands) { brand in
                    Text(brand.rawValue).tag(brand)
                }
            }
            .pickerStyle(.segmented)
            .onChange(of: selectedBrand) { newBrand in
                binPrefix = newBrand.defaultTestPrefix
            }

            HStack {
                TextField("Card Holder", text: $holderName)
                    .textFieldStyle(.roundedBorder)
                TextField("BIN Prefix", text: $binPrefix)
                    .textFieldStyle(.roundedBorder)
                    .frame(width: 140)
                    .onChange(of: binPrefix) { newValue in
                        let limited = String(CardGenerator.sanitizedPrefix(newValue).prefix(6))
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
                    do {
                        let card = try CardGenerator.makeCard(
                            options: CardGenerationOptions(
                                holderName: holderName,
                                binPrefix: binPrefix,
                                brand: selectedBrand,
                                metadata: ["sourceType": "local"]
                            )
                        )
                        if store.add(card) {
                            appendLog(message: "Locally generated \(selectedBrand.rawValue) sandbox card for \(card.displayName)")
                        } else {
                            appendLog(message: "Skipped local generation: duplicate card number")
                        }
                    } catch {
                        appendLog(message: "Local generation failed: \(error.localizedDescription)")
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
                    summaryRow(title: "Cards on file", value: "\(portfolioSummary.totalCards)")
                    summaryRow(title: "Active cards", value: "\(portfolioSummary.activeCards)")
                    if portfolioSummary.attentionCount > 0 {
                        summaryRow(title: "Needs attention", value: "\(portfolioSummary.attentionCount)")
                    }
                    summaryRow(title: "Total balance", value: formattedBalance(portfolioSummary.totalBalance))
                    if let average = portfolioSummary.averageBalance {
                        summaryRow(title: "Average balance", value: formattedBalance(average))
                    }
                    if let highest = portfolioSummary.highestBalanceCard {
                        summaryRow(title: "Highest balance", value: "\(highest.displayName) • \(formattedBalance(highest.balance))")
                    }
                    summaryRow(title: "Brands", value: portfolioSummary.brandSummary)
                }
            }

            Section("Generated Cards") {
                if store.cards.isEmpty {
                    Text("No cards yet. Generate one to begin.")
                        .foregroundStyle(.secondary)
                } else {
                    TextField("Search holder, brand, or masked number", text: $searchText)
                        .textFieldStyle(.roundedBorder)

                    if filteredCards.isEmpty {
                        Text("No cards match your search.")
                            .foregroundStyle(.secondary)
                    }

                    ForEach(filteredCards) { card in
                        VStack(alignment: .leading, spacing: 4) {
                            Text(card.displayName)
                                .font(.headline)
                            HStack(spacing: 8) {
                                Text("Brand: \(card.brandName)")
                                    .foregroundStyle(.secondary)
                                Text(card.healthStatus().rawValue)
                                    .font(.caption)
                                    .fontWeight(.semibold)
                                    .padding(.horizontal, 8)
                                    .padding(.vertical, 3)
                                    .background(statusColor(for: card.healthStatus()).opacity(0.15))
                                    .foregroundStyle(statusColor(for: card.healthStatus()))
                                    .clipShape(Capsule())
                            }
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
                    .onDelete(perform: removeFilteredCards)
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

    private func removeFilteredCards(at offsets: IndexSet) {
        let idsToRemove = offsets.compactMap { offset in
            filteredCards.indices.contains(offset) ? filteredCards[offset].id : nil
        }
        store.removeCards(withIDs: Set(idsToRemove))
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

    private func statusColor(for status: CardHealthStatus) -> Color {
        switch status {
        case .active:
            return .green
        case .expiringSoon:
            return .orange
        case .expired, .invalidNumber:
            return .red
        }
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
