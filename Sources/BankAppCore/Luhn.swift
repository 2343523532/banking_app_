public enum Luhn {
    public static func normalizedDigits(from input: String) -> String {
        input.filter { $0.isNumber }
    }

    public static func checksumDigit(forPartial partial: String) -> Int? {
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

    public static func isValid(_ number: String) -> Bool {
        let onlyDigits = normalizedDigits(from: number)
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

    public static func generateCardNumber(prefix: String, length: Int = 16) -> String? {
        guard prefix.allSatisfy({ $0.isNumber }) else { return nil }
        guard length > prefix.count else { return nil }
        var partial = prefix
        while partial.count < length - 1 {
            partial.append(String(Int.random(in: 0...9)))
        }
        guard let check = checksumDigit(forPartial: partial) else { return nil }
        return partial + String(check)
    }

    public static func masked(_ number: String, visibleSuffixCount: Int = 4, maskCharacter: Character = "*") -> String {
        let digits = normalizedDigits(from: number)
        guard !digits.isEmpty else { return "" }
        guard visibleSuffixCount > 0 else { return String(repeating: maskCharacter, count: digits.count) }

        let suffix = String(digits.suffix(visibleSuffixCount))
        let maskedCount = max(0, digits.count - visibleSuffixCount)
        let prefixMask = String(repeating: maskCharacter, count: maskedCount)
        return prefixMask + suffix
    }

    public static func cardBrand(for number: String) -> String {
        let digits = normalizedDigits(from: number)

        if digits.hasPrefix("4") {
            return "Visa"
        }

        if digits.count >= 2, let firstTwo = Int(digits.prefix(2)), (51...55).contains(firstTwo) {
            return "Mastercard"
        }

        if digits.count >= 4, let firstFour = Int(digits.prefix(4)), (2221...2720).contains(firstFour) {
            return "Mastercard"
        }

        if digits.hasPrefix("34") || digits.hasPrefix("37") {
            return "American Express"
        }

        if digits.hasPrefix("6011") || digits.hasPrefix("65") {
            return "Discover"
        }

        return "Unknown"
    }
}
