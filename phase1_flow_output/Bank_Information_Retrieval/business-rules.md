# Business Rules Extraction

**Extracted From**: Bank Information Retrieval Capability (User Story)
**Analysis Date**: 2026-01-09
**Analyst**: Expert Scala Business Logic Analyst
**Migration Target**: Go Application

## Executive Summary
- Total Business Rules Extracted: 4
- API Endpoints Analyzed: 2
- Rule Categories:
  - Calculations: 0
  - Decisions: 2
  - Thresholds: 0
  - Aggregations: 0
  - Workflows: 0
  - Transformations: 2

## Business Rules Catalog

### BR-001: Bank Existence Validation

**Category**: DECISION

**Description**: When retrieving a specific bank by ID, the system must verify that the bank exists in the database before returning information. If the bank does not exist, the system returns a 404 Not Found error response.

**Source**: 
- File: obp-api/src/main/scala/code/api/v4_0_0/APIMethods400.scala
- Class/Object: APIMethods400
- Method: getBank
- Lines: 239-301

**Business Logic**:
1. Receive bank ID as path parameter from the request
2. Query the database to check if a bank with the given ID exists
3. If bank exists, proceed to retrieve full bank information
4. If bank does not exist, return 404 Not Found error with appropriate message

**Scala Implementation**:
```scala
// Bank existence check pattern from Connector
def getBank(bankId: BankId, callContext: Option[CallContext]): OBPReturnType[Box[Bank]] = {
  // Returns Empty Box if bank not found, triggering 404 response
  getBankLegacy(bankId, callContext) match {
    case Full(bank) => Full(bank)
    case Empty => Empty  // Results in 404 Not Found
    case Failure(msg, _, _) => Failure(msg)
  }
}
```

**Variables**:
- **Input**: BANK_ID (string) - unique identifier for the bank to retrieve
- **Output**: Bank object if found, 404 error response if not found
- **Constants**: None

**Business Conditions**:
| Condition | Business Meaning | Values/Thresholds |
|-----------|------------------|-------------------|
| Bank exists in database | Valid bank request | Bank ID matches existing record |
| Bank does not exist | Invalid bank request | No matching bank ID found |

**Business Impact**: 
This rule ensures data integrity and proper error handling. Users and applications receive clear feedback when requesting non-existent banks, preventing confusion and enabling proper error handling in client applications.

**API Endpoints Using This Rule**:
- GET /banks/{BANK_ID} - Retrieve specific bank details

**Related Test Cases**:
- Test case for valid bank ID returning bank details
- Test case for invalid bank ID returning 404 Not Found

**Migration Notes for Go**:
- Use Go's error handling pattern to return appropriate HTTP status codes
- Implement repository pattern with method returning (Bank, error) tuple
- Return custom error type that maps to 404 status code when bank not found
- Use gin/echo/chi router's path parameter extraction for BANK_ID

**Example Scenarios**:
```
Scenario 1: Valid Bank ID
Input: BANK_ID = "gh.29.uk"
Processing: Query database for bank with ID "gh.29.uk"
Output: Bank object with full details (200 OK)

Scenario 2: Invalid Bank ID
Input: BANK_ID = "non-existent-bank"
Processing: Query database for bank with ID "non-existent-bank", no match found
Output: 404 Not Found error response
```

---

### BR-002: Complete Bank Information Composition

**Category**: TRANSFORMATION

**Description**: When retrieving a single bank by ID, the system must compose and return complete bank information including core fields (id, short_name, full_name, logo, website), bank routing information, and bank attributes.

**Source**: 
- File: obp-api/src/main/scala/code/api/v4_0_0/JSONFactory400.scala
- Class/Object: JSONFactory400
- Method: createBankJSON
- Lines: Referenced in APIMethods400.scala

**Business Logic**:
1. Retrieve core bank information from the bank record (id, short_name, full_name, logo, website)
2. Retrieve associated bank routing information (scheme and address pairs)
3. Retrieve bank attributes associated with the bank ID
4. Compose all information into a single response object
5. Return the complete bank response

**Scala Implementation**:
```scala
// Bank response composition pattern
def createBankJSON(bank: Bank, attributes: List[BankAttribute]): BankJson = {
  BankJson(
    id = bank.bankId.value,
    short_name = bank.shortName,
    full_name = bank.fullName,
    logo = bank.logoUrl,
    website = bank.websiteUrl,
    bank_routings = bank.bankRoutingScheme.zip(bank.bankRoutingAddress).map {
      case (scheme, address) => BankRoutingJson(scheme, address)
    },
    attributes = attributes.map(attr => BankAttributeJson(
      bank_id = attr.bankId.value,
      name = attr.name,
      `type` = attr.attributeType,
      value = attr.value,
      is_active = attr.isActive
    ))
  )
}
```

**Variables**:
- **Input**: Bank entity, BankRouting list, BankAttribute list
- **Output**: Complete BankDetailResponse JSON object
- **Constants**: None

**Business Conditions**:
| Condition | Business Meaning | Values/Thresholds |
|-----------|------------------|-------------------|
| Bank has attributes | Include attributes in response | Non-empty attribute list |
| Bank has no attributes | Return empty attributes array | Empty list, not null |
| Bank has routing info | Include routing in response | Non-empty routing list |

**Business Impact**: 
This rule ensures that API consumers receive all necessary bank information in a single request, reducing the need for multiple API calls and providing a complete view of the bank entity for integration purposes.

**API Endpoints Using This Rule**:
- GET /banks/{BANK_ID} - Retrieve specific bank with full details

**Related Test Cases**:
- Test case for bank with attributes returning complete response
- Test case for bank without attributes returning empty attributes array
- Test case for bank with multiple routing schemes

**Migration Notes for Go**:
- Define separate Go structs: BankDetailResponse, BankRouting, BankAttribute
- Use JSON struct tags with snake_case naming (e.g., `json:"short_name"`)
- Ensure empty slices serialize as `[]` not `null` using `omitempty` carefully
- Consider using composition for shared fields between list and detail responses

**Example Scenarios**:
```
Scenario 1: Bank with attributes
Input: Bank ID = "gh.29.uk"
Processing: Retrieve bank, routing (SWIFT: "ABCDEF12"), attributes (license_type: "FULL")
Output: {
  "id": "gh.29.uk",
  "short_name": "GH Bank",
  "full_name": "Ghana National Bank",
  "logo": "https://example.com/logo.png",
  "website": "https://ghbank.com",
  "bank_routings": [{"scheme": "SWIFT", "address": "ABCDEF12"}],
  "attributes": [{"bank_id": "gh.29.uk", "name": "license_type", "type": "STRING", "value": "FULL", "is_active": true}]
}

Scenario 2: Bank without attributes
Input: Bank ID = "simple-bank"
Processing: Retrieve bank, routing, no attributes found
Output: {
  "id": "simple-bank",
  "short_name": "Simple",
  "full_name": "Simple Bank",
  "logo": "https://example.com/simple.png",
  "website": "https://simplebank.com",
  "bank_routings": [{"scheme": "BIC", "address": "SIMPGB2L"}],
  "attributes": []
}
```

---

### BR-003: Basic Bank Information for List

**Category**: TRANSFORMATION

**Description**: When retrieving the list of all banks, the system returns basic bank information (core fields and routing) but excludes bank attributes for performance optimization. This provides a lighter response payload for list operations.

**Source**: 
- File: obp-api/src/main/scala/code/api/v4_0_0/APIMethods400.scala
- Class/Object: APIMethods400
- Method: getBanks
- Lines: 239-301

**Business Logic**:
1. Retrieve all banks from the database
2. For each bank, extract core fields (id, short_name, full_name, logo, website)
3. For each bank, retrieve associated bank routing information
4. Exclude bank attributes from the response (not fetched for list operation)
5. Compose array of basic bank objects
6. Return the bank list response

**Scala Implementation**:
```scala
// Bank list response composition pattern
def createBanksJson(banks: List[Bank]): BanksJson = {
  BanksJson(
    banks = banks.map { bank =>
      BankJson(
        id = bank.bankId.value,
        short_name = bank.shortName,
        full_name = bank.fullName,
        logo = bank.logoUrl,
        website = bank.websiteUrl,
        bank_routings = bank.bankRoutingScheme.zip(bank.bankRoutingAddress).map {
          case (scheme, address) => BankRoutingJson(scheme, address)
        }
        // Note: attributes NOT included in list response
      )
    }
  )
}
```

**Variables**:
- **Input**: None (retrieves all banks)
- **Output**: BankListResponse JSON object containing array of basic bank info
- **Constants**: None

**Business Conditions**:
| Condition | Business Meaning | Values/Thresholds |
|-----------|------------------|-------------------|
| Banks exist | Return populated list | One or more banks |
| No banks exist | Return empty list | Zero banks (see BR-004) |

**Business Impact**: 
This rule optimizes API performance by reducing response payload size for list operations. Applications that need to display bank selection dropdowns or lists can retrieve essential information quickly without the overhead of fetching detailed attributes for each bank.

**API Endpoints Using This Rule**:
- GET /banks - Retrieve list of all banks

**Related Test Cases**:
- Test case for bank list returning basic info without attributes
- Test case for bank list with multiple banks
- Test case verifying attributes field is not present in list response

**Migration Notes for Go**:
- Define separate struct BankListItem without attributes field
- Use different response struct than single bank retrieval
- Consider using shared embedded struct for common fields
- Implement efficient database query that doesn't join attributes table

**Example Scenarios**:
```
Scenario 1: Multiple banks in system
Input: GET /banks
Processing: Retrieve all banks, compose basic info for each
Output: {
  "banks": [
    {
      "id": "gh.29.uk",
      "short_name": "GH Bank",
      "full_name": "Ghana National Bank",
      "logo": "https://example.com/logo.png",
      "website": "https://ghbank.com",
      "bank_routings": [{"scheme": "SWIFT", "address": "ABCDEF12"}]
    },
    {
      "id": "simple-bank",
      "short_name": "Simple",
      "full_name": "Simple Bank",
      "logo": "https://example.com/simple.png",
      "website": "https://simplebank.com",
      "bank_routings": [{"scheme": "BIC", "address": "SIMPGB2L"}]
    }
  ]
}
```

---

### BR-004: Empty Result Handling for Bank List

**Category**: DECISION

**Description**: When no banks exist in the system, the bank list endpoint returns an empty list with HTTP 200 status, not a 404 error. This distinguishes between "no data available" (valid state) and "resource not found" (error state).

**Source**: 
- File: obp-api/src/main/scala/code/api/v4_0_0/APIMethods400.scala
- Class/Object: APIMethods400
- Method: getBanks
- Lines: 239-301

**Business Logic**:
1. Query database for all banks
2. If query returns empty result set, this is a valid state (not an error)
3. Compose response with empty banks array
4. Return HTTP 200 OK status with empty list
5. Do NOT return 404 Not Found for empty results

**Scala Implementation**:
```scala
// Empty list handling pattern
def getBanks(callContext: Option[CallContext]): OBPReturnType[Box[List[Bank]]] = {
  // Returns Full(List()) for empty results, not Empty
  // This ensures 200 OK with empty array, not 404
  for {
    banks <- getBanksLegacy(callContext)
  } yield {
    Full(banks)  // Even if banks is empty List(), return Full
  }
}
```

**Variables**:
- **Input**: None
- **Output**: Empty array `{"banks": []}` with HTTP 200 status
- **Constants**: None

**Business Conditions**:
| Condition | Business Meaning | Values/Thresholds |
|-----------|------------------|-------------------|
| Banks list is empty | Valid state, no banks configured | Zero banks in database |
| Banks list is populated | Normal state | One or more banks |

**Business Impact**: 
This rule provides clear semantics for API consumers. An empty list indicates the system is functioning correctly but has no banks configured, while a 404 would incorrectly suggest the endpoint doesn't exist. This enables proper client-side handling and distinguishes between configuration states and errors.

**API Endpoints Using This Rule**:
- GET /banks - Retrieve list of all banks

**Related Test Cases**:
- Test case for empty bank list returning 200 OK with empty array
- Test case verifying 404 is NOT returned for empty results
- Test case for response structure with empty banks array

**Migration Notes for Go**:
- Ensure handler returns 200 status even when bank slice is empty
- Initialize response struct with empty slice, not nil
- Use `banks := make([]Bank, 0)` or `banks := []Bank{}` to ensure JSON serializes as `[]`
- Do not use error return for empty results

**Example Scenarios**:
```
Scenario 1: No banks in system
Input: GET /banks
Processing: Query database, receive empty result set
Output: HTTP 200 OK
{
  "banks": []
}

Scenario 2: System with banks (contrast)
Input: GET /banks
Processing: Query database, receive list of banks
Output: HTTP 200 OK
{
  "banks": [
    {"id": "bank-1", "short_name": "Bank One", ...}
  ]
}
```

---

## API Endpoint Coverage

| Endpoint | HTTP Method | Business Rules | Rule IDs |
|----------|-------------|----------------|----------|
| /banks | GET | Basic bank list retrieval, empty result handling | BR-003, BR-004 |
| /banks/{BANK_ID} | GET | Bank existence validation, complete info composition | BR-001, BR-002 |

## Migration Validation Matrix

| Rule ID | Test Case Reference | Go Implementation Status | Validation Status |
|---------|---------------------|--------------------------|-------------------|
| BR-001 | TestGetBankNotFound, TestGetBankValid | Pending | Pending |
| BR-002 | TestGetBankWithAttributes, TestGetBankWithoutAttributes | Pending | Pending |
| BR-003 | TestGetBanksListBasicInfo | Pending | Pending |
| BR-004 | TestGetBanksEmptyList | Pending | Pending |

## Notes and Assumptions

1. **Assumptions Made**:
   - Bank routing information is always available for each bank (may be empty array)
   - Bank attributes are optional and may not exist for all banks
   - The bank ID is a string identifier, not a numeric ID
   - Authentication is optional for these read-only endpoints (based on user story)

2. **Gaps Identified**:
   - Pagination support for large bank lists is not specified in the user story
   - Rate limiting requirements are not defined
   - Caching strategy for bank information is not specified

3. **Clarifications Needed**:
   - Should deprecated fields (swiftBic, nationalIdentifier) be excluded from Go implementation?
   - Are there any field-level access controls for bank attributes?
   - What is the expected maximum number of banks in the system?

4. **Technical vs Business Logic Exclusions**:
   - JSON serialization/deserialization logic excluded (technical)
   - Database query construction excluded (technical)
   - HTTP request/response handling excluded (technical)
   - Input validation for bank ID format excluded (validation, not business rule)
