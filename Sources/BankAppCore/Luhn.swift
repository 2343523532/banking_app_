public enum Luhn {
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
}
