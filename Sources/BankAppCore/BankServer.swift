import Foundation
import Network

public final class BankServer {
    private var listener: NWListener?
    public private(set) var port: NWEndpoint.Port?
    private var connections: [NWConnection] = []

    public var onLog: ((String) -> Void)?
    public var onCreatedCard: ((Card) -> Void)?
    public var onPortUpdate: ((NWEndpoint.Port?) -> Void)?

    public init() {}

    public func startListening() {
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

    public func stop() {
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

            var metadata: [String: String] = [:]
            if let endpoint = connection.endpoint {
                metadata["source"] = "\(endpoint)"
            }
            if let currentPath = connection.currentPath {
                metadata["interface"] = "\(currentPath.availableInterfaces.first?.name ?? "unknown")"
            }

            let card = Card(holderName: holder, prefix: bin, number: cardNumber, expiryMonth: expiryMonth, expiryYear: expiryYear, cvv: cvv, balance: balance, metadata: metadata)

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
