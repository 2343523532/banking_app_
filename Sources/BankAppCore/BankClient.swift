import Foundation

#if canImport(Network)
import Network

public final class BankClient {
    private var connection: NWConnection?
    private var targetPort: NWEndpoint.Port?

    public var onLog: ((String) -> Void)?
    public var onResponse: ((BankResponse) -> Void)?

    public init() {}

    public func connect(to port: NWEndpoint.Port) {
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

    public func disconnect() {
        connection?.cancel()
        connection = nil
        targetPort = nil
        onLog?("Client disconnected")
    }

    public func sendCreateCard(holderName: String, binPrefix: String, port: NWEndpoint.Port) {
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

#else

public final class BankClient {
    public var onLog: ((String) -> Void)?
    public var onResponse: ((BankResponse) -> Void)?

    public init() {}

    public func connect(to port: UInt16) {
        onLog?("Client networking is unavailable on this platform (port: \(port)).")
    }

    public func disconnect() {
        onLog?("Client disconnected")
    }

    public func sendCreateCard(holderName: String, binPrefix: String, port: UInt16) {
        onLog?("Client cannot send request because Network framework is unavailable (port: \(port)).")
    }
}

#endif
