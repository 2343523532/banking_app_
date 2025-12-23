import Foundation

extension Array where Element == Card {
    public func generateCSV() -> String {
        var csv = "ID,Holder Name,Number,Expiry,CVV,Balance,Metadata\n"
        for card in self {
            let expiry = "\(card.expiryMonth)/\(card.expiryYear)"

            // Serialize metadata to JSON string for the CSV field, handling escaping
            let metadataString: String
            if let metadata = card.metadata, !metadata.isEmpty {
                 if let data = try? JSONEncoder().encode(metadata),
                    let json = String(data: data, encoding: .utf8) {
                     metadataString = json.replacingOccurrences(of: "\"", with: "\"\"")
                 } else {
                     metadataString = ""
                 }
            } else {
                metadataString = ""
            }

            let row = "\"\(card.id)\",\"\(card.holderName)\",\"\(card.number)\",\"\(expiry)\",\"\(card.cvv)\",\"\(card.balance)\",\"\(metadataString)\"\n"
            csv.append(row)
        }
        return csv
    }
}
