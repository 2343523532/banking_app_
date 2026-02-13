// swift-tools-version: 5.9
import PackageDescription

var products: [Product] = [
    .library(name: "BankAppCore", targets: ["BankAppCore"])
]

var targets: [Target] = [
    .target(
        name: "BankAppCore",
        dependencies: []
    ),
    .testTarget(
        name: "BankAppTests",
        dependencies: ["BankAppCore"]
    )
]

#if os(macOS)
products.insert(.executable(name: "BankApp", targets: ["BankApp"]), at: 0)
targets.insert(
    .executableTarget(
        name: "BankApp",
        dependencies: ["BankAppCore"]
    ),
    at: 1
)
#endif

let package = Package(
    name: "BankApp",
    platforms: [
        .macOS(.v13),
        .iOS(.v16)
    ],
    products: products,
    targets: targets
)
