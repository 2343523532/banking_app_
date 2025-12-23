# Banking App Demo

This repository contains a SwiftUI banking application demo, structured as a Swift Package for modularity and testability. It demonstrates how a lightweight banking client can talk to a local TCP server, generate credit-card like records, and visualise the activity.

## Features

- **SwiftUI Dashboard**: Interactive controls to start/stop the server, generate cards, and view history.
- **Networking**: TCP `BankServer` and `BankClient` demonstrating custom protocol handling.
- **Luhn Validation**: Validates card numbers using the Luhn algorithm.
- **Persistence**: JSON-backed persistence for cards and logs.
- **Metadata Tracking**: Captures and persists connection metadata (source IP, interface) for generated cards.
- **CSV Export**: Export your card list to CSV for external use.
- **Adaptive Layout**: Responsive UI that adapts to different screen sizes.

## Project Structure

The project is organized as a Swift Package with the following targets:

- **BankAppCore**: A library containing the business logic, models, networking, and persistence layer.
- **BankApp**: The executable target containing the SwiftUI application and views.
- **BankAppTests**: Unit tests for the core logic.

```
.
├── Sources
│   ├── BankApp         # SwiftUI App and Views
│   └── BankAppCore     # Models, Server, Client, Persistence, Luhn
├── Tests
│   └── BankAppTests    # Unit tests
├── Package.swift       # Swift Package Manager configuration
└── README.md
```

## Getting Started

### Requirements
- macOS 13.0+ or iOS 16.0+
- Swift 5.9+

### Running the App
You can open this project in Xcode by opening the `Package.swift` file, or run it via command line if supported by your environment.

To run the app:
1. Open the project in Xcode.
2. Select the `BankApp` scheme.
3. Run (Cmd+R).

### Running Tests
To run the unit tests:
```bash
swift test
```

## Usage

1. **Server**: Toggle "Start Server" to begin listening for connections.
2. **Generate Cards**:
   - **Locally**: Generates a card immediately on the device.
   - **Via Server**: Sends a request to the local TCP server, demonstrating the client-server loop.
3. **Export**: Tap the menu icon (top right) -> "Export CSV" to share your card list.
4. **Clear Data**: Use the menu to clear cards or logs.

## License

MIT License
