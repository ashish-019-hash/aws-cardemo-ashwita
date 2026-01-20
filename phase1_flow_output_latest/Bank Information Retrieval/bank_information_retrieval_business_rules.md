# Business Rules Extraction

**Extracted From**: Bank Information Retrieval Capability (Scala Application - Open Bank Project)
**Analysis Date**: 2026-01-20
**Analyst**: Expert Scala Business Logic Analyst
**Migration Target**: Go Application

## Executive Summary
- Total Business Rules Extracted: 7
- API Endpoints Analyzed: 2
- Rule Categories:
  - Calculations: 0
  - Decisions: 2
  - Thresholds: 0
  - Aggregations: 1
  - Workflows: 2
  - Transformations: 2

---

## Business Rules Catalog

### BR-001: Bank List Retrieval

**Category**: AGGREGATION

**Description**: The system aggregates and returns a complete list of all banks supported on the platform with their essential information including identifiers, names, logos, and websites.

**Source**: 
- File: code/api/v4_0_0/APIMethods400.scala
- Class/Object: APIMethods400
- Method: getBanks
- Lines: Referenced in user story

**Business Logic**:
1. Receive request for all supported banks on the platform
2. Retrieve all bank records from the data store
3. Transform each bank record into a standardized response format containing id, short_name, full_name, logo, website, and bank_routings
4. Return the aggregated list of banks to the requester

**Scala Implementation**:
```scala
// Conceptual implementation based on user story
def getBanks(): Box[List[Bank]] = {
  NewStyle.function.getBanks()
}

def createBanksJson(banks: List[Bank]): BanksJson400 = {
  BanksJson400(banks.map(bank => createBankJSON400(bank, None)))
}
```

**Variables**:
- **Input**: None (retrieves all banks)
- **Output**: List of bank objects with id, short_name, full_name, logo, website, bank_routings
- **Constants**: None

**Business Conditions**: N/A - Returns all supported banks without filtering

**Business Impact**: 
Enables third-party developers and fintech applications to discover all available banks on the platform, which is essential for bank selection workflows and integration planning.

**API Endpoints Using This Rule**:
- [GET /obp/v4.0.0/banks] - Retrieve list of all supported banks

**Related Test Cases**:
- Test cases validating complete bank list retrieval
- Test cases verifying response structure contains all required fields

**Migration Notes for Go**:
- Implement as a simple GET handler returning a slice of Bank structs
- Use Go's encoding/json for response serialization
- Consider implementing response caching for high-volume scenarios

**Example Scenarios**:
```
Scenario 1: Retrieve all banks
Input: GET /obp/v4.0.0/banks
Processing: Query all bank records, transform to JSON response
Output: {"banks": [{"id": "bank-id-001", "short_name": "Example Bank", ...}, ...]}

Scenario 2: Empty platform (no banks configured)
Input: GET /obp/v4.0.0/banks
Processing: Query returns empty list
Output: {"banks": []}
```

---

### BR-002: Single Bank Retrieval by Identifier

**Category**: DECISION

**Description**: The system retrieves detailed information for a specific bank when provided with a valid bank identifier, including additional bank attributes not available in the list view.

**Source**: 
- File: code/api/v4_0_0/APIMethods400.scala
- Class/Object: APIMethods400
- Method: getBank
- Lines: Referenced in user story

**Business Logic**:
1. Receive request with a specific bank identifier (BANK_ID)
2. Validate that the bank identifier exists in the system
3. If bank exists, retrieve the bank record along with its attributes
4. Transform the bank record into a detailed response format including attributes
5. If bank does not exist, return appropriate error response (HTTP 404)

**Scala Implementation**:
```scala
// Conceptual implementation based on user story
def getBank(bankId: BankId): Box[Bank] = {
  NewStyle.function.getBank(bankId)
}

def getBankAttributesByBank(bankId: BankId): Box[List[BankAttribute]] = {
  NewStyle.function.getBankAttributesByBank(bankId)
}

def createBankJSON400(bank: Bank, attributes: Option[List[BankAttribute]]): BankJson400 = {
  BankJson400(
    id = bank.bankId.value,
    short_name = bank.shortName,
    full_name = bank.fullName,
    logo = bank.logoUrl,
    website = bank.websiteUrl,
    bank_routings = bank.bankRoutingScheme.map(r => BankRoutingJsonV121(r.scheme, r.address)),
    attributes = attributes.map(_.map(a => BankAttributeBankResponseJsonV400(a.name, a.value)))
  )
}
```

**Variables**:
- **Input**: BANK_ID (String) - Unique identifier of the bank
- **Output**: Bank object with id, short_name, full_name, logo, website, bank_routings, and attributes
- **Constants**: None

**Business Conditions**:
| Condition | Business Meaning | Values/Thresholds |
|-----------|------------------|-------------------|
| Bank exists | Bank is supported on platform | Valid BANK_ID in system |
| Bank not found | Bank is not supported or invalid ID | Returns HTTP 404 |

**Business Impact**: 
Enables applications to retrieve comprehensive details about a specific bank for display purposes, integration configuration, and accessing bank-specific attributes.

**API Endpoints Using This Rule**:
- [GET /obp/v4.0.0/banks/BANK_ID] - Retrieve specific bank details

**Related Test Cases**:
- Test cases for successful bank retrieval with valid ID
- Test cases for HTTP 404 response with invalid/non-existent bank ID
- Test cases verifying attributes are included in single bank response

**Migration Notes for Go**:
- Implement path parameter extraction for BANK_ID
- Use Go's http.StatusNotFound for bank not found scenarios
- Consider using a repository pattern for bank data access

**Example Scenarios**:
```
Scenario 1: Valid bank retrieval
Input: GET /obp/v4.0.0/banks/bank-id-001
Processing: Lookup bank by ID, retrieve attributes, transform to JSON
Output: {"id": "bank-id-001", "short_name": "Example Bank", ..., "attributes": [{"name": "COUNTRY", "value": "US"}]}

Scenario 2: Invalid bank ID
Input: GET /obp/v4.0.0/banks/invalid-bank-id
Processing: Lookup fails, bank not found
Output: HTTP 404 - BankNotFound error response
```

---

### BR-003: Bank Information Completeness Requirement

**Category**: TRANSFORMATION

**Description**: All bank information responses must include the complete set of required fields: unique identifier, short name, full name, logo URL, website URL, and bank routing information.

**Source**: 
- File: code/api/v4_0_0/JSONFactory4.0.0.scala
- Class/Object: JSONFactory400
- Method: createBankJSON400
- Lines: Referenced in user story

**Business Logic**:
1. For each bank record, extract all required business fields
2. Transform bank identifier to string format for API response
3. Include short_name for display in compact UI elements
4. Include full_name for legal/formal display purposes
5. Include logo URL for visual representation in applications
6. Include website URL for user reference and navigation
7. Include bank_routings list with scheme and address pairs

**Scala Implementation**:
```scala
case class BankJson400(
  id: String,
  short_name: String,
  full_name: String,
  logo: String,
  website: String,
  bank_routings: List[BankRoutingJsonV121],
  attributes: Option[List[BankAttributeBankResponseJsonV400]]
)
```

**Variables**:
- **Input**: Bank domain object from data store
- **Output**: BankJson400 response object with all required fields
- **Constants**: None

**Business Conditions**: N/A - All fields are required for every bank response

**Business Impact**: 
Ensures third-party applications receive consistent and complete bank information for proper display and integration, enabling bank selection workflows and user interface rendering.

**API Endpoints Using This Rule**:
- [GET /obp/v4.0.0/banks] - All banks in list include complete information
- [GET /obp/v4.0.0/banks/BANK_ID] - Single bank includes complete information plus attributes

**Related Test Cases**:
- Test cases validating all required fields are present in response
- Test cases verifying field data types and formats

**Migration Notes for Go**:
- Define a Bank struct with all required fields using json tags
- Use pointer types or omitempty for optional fields like attributes
- Implement proper null handling using stringOrNull equivalent

**Example Scenarios**:
```
Scenario 1: Complete bank information
Input: Bank record from database
Processing: Map all fields to response structure
Output: {
  "id": "bank-id-001",
  "short_name": "Example Bank",
  "full_name": "Example Bank Corporation",
  "logo": "https://example.com/logo.png",
  "website": "https://www.examplebank.com",
  "bank_routings": [{"scheme": "OBP", "address": "bank-id-001"}],
  "attributes": null
}
```

---

### BR-004: Bank Routing Information Structure

**Category**: TRANSFORMATION

**Description**: Bank routing information must be provided as a list of scheme-address pairs, supporting multiple routing schemes (OBP, BIC, etc.) for each bank to enable proper transaction routing.

**Source**: 
- File: code/api/v4_0_0/JSONFactory4.0.0.scala
- Class/Object: BankRoutingJsonV121
- Method: N/A (case class definition)
- Lines: Referenced in user story

**Business Logic**:
1. Each bank can have multiple routing identifiers
2. Each routing identifier consists of a scheme (type) and address (value)
3. Common schemes include OBP (Open Bank Project internal), BIC (Bank Identifier Code)
4. Routing information enables proper identification of banks in different contexts

**Scala Implementation**:
```scala
case class BankRoutingJsonV121(
  scheme: String,
  address: String
)

// Example usage in bank response
bank_routings = List(
  BankRoutingJsonV121("OBP", "bank-id-001"),
  BankRoutingJsonV121("BIC", "EXBKUS33XXX")
)
```

**Variables**:
- **Input**: Bank routing records associated with the bank
- **Output**: List of BankRoutingJsonV121 objects with scheme and address
- **Constants**: 
  - Common schemes: "OBP", "BIC"

**Business Conditions**:
| Condition | Business Meaning | Values/Thresholds |
|-----------|------------------|-------------------|
| scheme = "OBP" | Internal platform identifier | Platform-specific bank ID |
| scheme = "BIC" | SWIFT Bank Identifier Code | 8 or 11 character BIC code |

**Business Impact**: 
Enables applications to identify banks using different routing schemes for various use cases such as internal platform operations (OBP) or international transfers (BIC/SWIFT).

**API Endpoints Using This Rule**:
- [GET /obp/v4.0.0/banks] - Bank routings included for each bank
- [GET /obp/v4.0.0/banks/BANK_ID] - Bank routings included for specific bank

**Related Test Cases**:
- Test cases validating bank_routings structure
- Test cases verifying multiple routing schemes per bank

**Migration Notes for Go**:
- Define BankRouting struct with Scheme and Address fields
- Use slice of BankRouting for the bank_routings field
- Ensure proper JSON serialization with lowercase field names

**Example Scenarios**:
```
Scenario 1: Bank with multiple routing schemes
Input: Bank with OBP and BIC identifiers
Processing: Transform routing records to JSON array
Output: "bank_routings": [
  {"scheme": "OBP", "address": "bank-id-001"},
  {"scheme": "BIC", "address": "EXBKUS33XXX"}
]

Scenario 2: Bank with single routing scheme
Input: Bank with only OBP identifier
Processing: Transform single routing record
Output: "bank_routings": [{"scheme": "OBP", "address": "bank-id-002"}]
```

---

### BR-005: Platform Scope Filtering

**Category**: DECISION

**Description**: Only banks that are actively supported on the platform should be returned in retrieval operations. Banks not configured or supported on the platform must be excluded from results.

**Source**: 
- File: code/bankconnectors/Connector.scala
- Class/Object: Connector
- Method: getBanks, getBank
- Lines: Referenced in user story

**Business Logic**:
1. The platform maintains a registry of supported banks
2. When retrieving banks, only return those in the supported registry
3. When retrieving a specific bank, verify it is in the supported registry
4. Unsupported or unconfigured banks are not accessible via the API

**Scala Implementation**:
```scala
// Conceptual implementation
def getBanks(): Box[List[Bank]] = {
  // Returns only banks configured and supported on the platform
  Connector.connector.vend.getBanks()
}

def getBank(bankId: BankId): Box[Bank] = {
  // Returns bank only if it exists and is supported
  Connector.connector.vend.getBank(bankId)
}
```

**Variables**:
- **Input**: Platform bank registry/configuration
- **Output**: Filtered list of supported banks or single supported bank
- **Constants**: None

**Business Conditions**:
| Condition | Business Meaning | Values/Thresholds |
|-----------|------------------|-------------------|
| Bank in registry | Bank is supported on platform | Included in results |
| Bank not in registry | Bank is not supported | Excluded/Not Found |

**Business Impact**: 
Ensures API consumers only see and interact with banks that are properly configured and supported on the platform, preventing errors from attempting to use unsupported banks.

**API Endpoints Using This Rule**:
- [GET /obp/v4.0.0/banks] - Returns only supported banks
- [GET /obp/v4.0.0/banks/BANK_ID] - Returns bank only if supported

**Related Test Cases**:
- Test cases verifying only supported banks are returned
- Test cases for unsupported bank ID returning 404

**Migration Notes for Go**:
- Implement bank registry/repository with supported bank filtering
- Use database queries that filter by active/supported status
- Return appropriate error for unsupported bank requests

**Example Scenarios**:
```
Scenario 1: Platform with 3 supported banks
Input: GET /obp/v4.0.0/banks
Processing: Query supported bank registry
Output: {"banks": [bank1, bank2, bank3]} - Only supported banks

Scenario 2: Request for unsupported bank
Input: GET /obp/v4.0.0/banks/unsupported-bank
Processing: Bank not in supported registry
Output: HTTP 404 - Bank not found
```

---

### BR-006: Real-time Information Access

**Category**: WORKFLOW

**Description**: Bank information retrieval must support real-time access patterns with low latency to meet high-volume usage requirements of third-party applications.

**Source**: 
- File: Capability Description
- Class/Object: N/A
- Method: N/A
- Lines: N/A (Non-functional business requirement)

**Business Logic**:
1. Bank information requests must be processed synchronously
2. Response times must be optimized for real-time access
3. System must handle high volume of concurrent requests
4. Consider caching strategies for frequently accessed data

**Scala Implementation**:
```scala
// Conceptual implementation with caching consideration
def getBanks(): Box[List[Bank]] = {
  // Real-time synchronous retrieval
  // May use caching for performance optimization
  NewStyle.function.getBanks()
}
```

**Variables**:
- **Input**: Bank retrieval request
- **Output**: Bank information response
- **Constants**: 
  - Frequency: Real-time
  - Volume: High

**Business Conditions**: N/A - Performance requirement

**Business Impact**: 
Ensures third-party applications can provide responsive user experiences when displaying bank information, supporting real-time bank selection workflows and UI rendering.

**API Endpoints Using This Rule**:
- [GET /obp/v4.0.0/banks] - Must respond in real-time
- [GET /obp/v4.0.0/banks/BANK_ID] - Must respond in real-time

**Related Test Cases**:
- Performance test cases for response time validation
- Load test cases for high-volume concurrent access

**Migration Notes for Go**:
- Implement efficient database queries with proper indexing
- Consider implementing response caching (e.g., Redis, in-memory cache)
- Use Go's concurrency features for handling high-volume requests
- Implement connection pooling for database access

**Example Scenarios**:
```
Scenario 1: High-volume concurrent requests
Input: 1000 concurrent GET /obp/v4.0.0/banks requests
Processing: Efficient query execution, possibly cached responses
Output: All requests return within acceptable latency threshold

Scenario 2: Real-time bank selection workflow
Input: User initiates bank selection in third-party app
Processing: Immediate API call, real-time response
Output: Bank list displayed to user without noticeable delay
```

---

### BR-007: Bank Attributes for Detailed View

**Category**: WORKFLOW

**Description**: When retrieving a specific bank by ID, the system must include additional bank attributes (custom metadata) that are not available in the list view, providing comprehensive bank details for integration purposes.

**Source**: 
- File: code/api/v4_0_0/APIMethods400.scala
- Class/Object: APIMethods400
- Method: getBank
- Lines: Referenced in user story

**Business Logic**:
1. Bank attributes are optional custom metadata associated with each bank
2. Attributes are only retrieved and included for single bank detail requests
3. List view (GET /banks) does not include attributes for performance reasons
4. Attributes provide additional context like country, currency, or custom configurations

**Scala Implementation**:
```scala
// Single bank retrieval includes attributes
def getBank(bankId: BankId): Box[BankJson400] = {
  for {
    bank <- NewStyle.function.getBank(bankId)
    attributes <- NewStyle.function.getBankAttributesByBank(bankId)
  } yield JSONFactory400.createBankJSON400(bank, Some(attributes))
}

// List retrieval excludes attributes
def getBanks(): Box[BanksJson400] = {
  for {
    banks <- NewStyle.function.getBanks()
  } yield JSONFactory400.createBanksJson(banks) // attributes = None
}
```

**Variables**:
- **Input**: BANK_ID for single bank retrieval
- **Output**: Bank object with attributes field populated (for single bank) or null (for list)
- **Constants**: None

**Business Conditions**:
| Condition | Business Meaning | Values/Thresholds |
|-----------|------------------|-------------------|
| Single bank request | Include attributes | attributes field populated |
| List banks request | Exclude attributes | attributes field is null |

**Business Impact**: 
Provides detailed bank metadata for applications that need comprehensive bank information for integration configuration, while maintaining performance for list operations.

**API Endpoints Using This Rule**:
- [GET /obp/v4.0.0/banks] - attributes field is null
- [GET /obp/v4.0.0/banks/BANK_ID] - attributes field contains bank-specific metadata

**Related Test Cases**:
- Test cases verifying attributes are present in single bank response
- Test cases verifying attributes are null/absent in list response

**Migration Notes for Go**:
- Use pointer type or omitempty for attributes field
- Implement separate attribute retrieval for single bank endpoint
- Consider lazy loading pattern for attributes

**Example Scenarios**:
```
Scenario 1: Single bank with attributes
Input: GET /obp/v4.0.0/banks/bank-id-001
Processing: Retrieve bank and associated attributes
Output: {..., "attributes": [{"name": "COUNTRY", "value": "US"}, {"name": "CURRENCY", "value": "USD"}]}

Scenario 2: Bank list without attributes
Input: GET /obp/v4.0.0/banks
Processing: Retrieve banks without attribute lookup
Output: {"banks": [{..., "attributes": null}, {..., "attributes": null}]}
```

---

## API Endpoint Coverage

| Endpoint | HTTP Method | Business Rules | Rule IDs |
|----------|-------------|----------------|----------|
| /obp/v4.0.0/banks | GET | Bank list retrieval, completeness, routing, platform scope, real-time access | BR-001, BR-003, BR-004, BR-005, BR-006 |
| /obp/v4.0.0/banks/BANK_ID | GET | Single bank retrieval, completeness, routing, platform scope, real-time access, attributes | BR-002, BR-003, BR-004, BR-005, BR-006, BR-007 |

---

## Migration Validation Matrix

| Rule ID | Test Case Reference | Go Implementation Status | Validation Status |
|---------|---------------------|--------------------------|-------------------|
| BR-001 | TestGetAllBanks | Pending | Pending |
| BR-002 | TestGetBankById, TestBankNotFound | Pending | Pending |
| BR-003 | TestBankResponseFields | Pending | Pending |
| BR-004 | TestBankRoutingStructure | Pending | Pending |
| BR-005 | TestPlatformScopeFiltering | Pending | Pending |
| BR-006 | TestRealTimePerformance | Pending | Pending |
| BR-007 | TestBankAttributes | Pending | Pending |

---

## Notes and Assumptions

1. **Source Code Reference**: Business rules are extracted based on the user story content which references Scala implementation classes (APIMethods400, JSONFactory400, NewStyle.function, Connector). Actual line numbers would need to be verified against the source code.

2. **Authentication**: The user story indicates authentication is required (OAuth/API key) but authentication logic is considered technical implementation and not extracted as a business rule.

3. **Error Handling**: HTTP 404 for bank not found is documented as part of BR-002 decision logic. Other error codes (400 for malformed requests) are validation/technical concerns.

4. **Caching Strategy**: Real-time access with high volume (BR-006) suggests caching should be implemented, but specific caching rules are implementation details to be determined during Go migration.

5. **Attributes Schema**: The exact schema of bank attributes is flexible (name-value pairs). Specific attribute names like COUNTRY, CURRENCY are examples from the user story.

6. **Pagination**: The user story notes mention considering pagination for the list endpoint if bank count grows. This is a future enhancement consideration, not a current business rule.
