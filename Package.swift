// swift-tools-version: 5.9
import PackageDescription

let package = Package(
    name: "BankApp",
    platforms: [
        .macOS(.v13),
        .iOS(.v16)
    ],
    products: [
        .executable(name: "BankApp", targets: ["BankApp"]),
        .library(name: "BankAppCore", targets: ["BankAppCore"])
    ],
    targets: [
        .target(
            name: "BankAppCore",
            dependencies: []
        ),
        .executableTarget(
            name: "BankApp",
            dependencies: ["BankAppCore"]
        ),
        .testTarget(
            name: "BankAppTests",
            dependencies: ["BankAppCore"]
        )
    ]
)
