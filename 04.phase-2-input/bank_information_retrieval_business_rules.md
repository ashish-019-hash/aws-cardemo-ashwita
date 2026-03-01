# Business Rules Extraction

**Extracted From**: Bank Information Retrieval User Story
**Analysis Date**: December 01, 2025
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

### BR-001: Bank Existence Validation for Single Bank Retrieval

**Category**: DECISION

**Description**: When retrieving details for a specific bank, the system must verify that the bank identifier exists in the system before returning bank information. If the bank does not exist, an appropriate error response must be returned.

**Source**: 
- File: BankService.scala (inferred from user story)
- Class/Object: BankService
- Method: getBankById
- Lines: N/A (derived from user story)

**Business Logic**:
1. Receive bank identifier (BANK_ID) from the request path
2. Query the bank repository to check if bank exists
3. If bank exists, proceed to retrieve complete bank details
4. If bank does not exist, return 404 Not Found error response

**Variables**:
- **Input**: bankId (string) - Unique identifier for the bank
- **Output**: Bank details object or error response
- **Constants**: None

**Business Conditions**:
| Condition | Business Meaning | Values/Thresholds |
|-----------|------------------|-------------------|
| Bank exists in system | Bank is registered and available | Bank record found in repository |
| Bank does not exist | Invalid or unknown bank identifier | No bank record found |

**Business Impact**: 
This rule ensures data integrity and proper error handling when clients request information for non-existent banks. It prevents system errors and provides clear feedback to API consumers about invalid bank identifiers.

**API Endpoints Using This Rule**:
- GET /banks/BANK_ID - Retrieve single bank details

**Related Test Cases**:
- Test case for valid bank ID returning bank details
- Test case for invalid bank ID returning 404 error

**Migration Notes for Go**:
- Implement using Go's error handling pattern with explicit error returns
- Use appropriate HTTP status codes (200 for success, 404 for not found)
- Consider using a custom error type for bank not found scenarios

**Example Scenarios**:
```
Scenario 1: Valid bank identifier
Input: bankId = "bank-001"
Processing: Query repository for bank-001, bank exists
Output: Bank details with status 200 OK

Scenario 2: Invalid bank identifier
Input: bankId = "invalid-bank"
Processing: Query repository for invalid-bank, bank not found
Output: Error response with status 404 Not Found
```

---

### BR-002: Complete Bank Information Composition for Single Bank Retrieval

**Category**: TRANSFORMATION

**Description**: When retrieving a single bank by ID, the system must compose and return complete bank information including all core fields (id, short_name, full_name, logo, website), bank routing information, and bank attributes.

**Source**: 
- File: BankService.scala (inferred from user story)
- Class/Object: BankService, BankAttributeService
- Method: getBankById, getBankAttributes
- Lines: N/A (derived from user story)

**Business Logic**:
1. Retrieve core bank details from BankRepository (id, short_name, full_name, logo, website)
2. Retrieve bank routing information associated with the bank
3. Retrieve bank attributes from BankAttributeService
4. Compose complete bank response object with all retrieved data
5. If bank has no attributes, return empty attributes array (not null)

**Variables**:
- **Input**: bankId (string) - Unique identifier for the bank
- **Output**: Complete bank object containing:
  - id: string
  - short_name: string
  - full_name: string
  - logo: string (URL)
  - website: string (URL)
  - bank_routings: array of {scheme, address}
  - attributes: array of {bank_id, name, type, value, is_active}
- **Constants**: None

**Business Conditions**:
| Condition | Business Meaning | Values/Thresholds |
|-----------|------------------|-------------------|
| Bank has attributes | Bank has operational parameters defined | Non-empty attributes array |
| Bank has no attributes | Bank exists but no attributes configured | Empty attributes array [] |

**Business Impact**: 
This rule ensures that API consumers receive comprehensive bank information in a single request, enabling them to display complete bank details including operational parameters. This supports customer-facing applications that need to show full bank information.

**API Endpoints Using This Rule**:
- GET /banks/BANK_ID - Retrieve single bank details with attributes

**Related Test Cases**:
- Test case for bank with attributes returning complete data
- Test case for bank without attributes returning empty attributes array

**Migration Notes for Go**:
- Use Go structs to model the bank response with all nested fields
- Implement proper JSON marshaling for the response structure
- Handle nil/empty attributes gracefully by returning empty slice instead of nil

**Example Scenarios**:
```
Scenario 1: Bank with attributes
Input: bankId = "bank-001"
Processing: Retrieve bank details, routing info, and 3 attributes
Output: {
  "id": "bank-001",
  "short_name": "ABC",
  "full_name": "ABC Bank",
  "logo": "https://example.com/logo.png",
  "website": "https://abcbank.com",
  "bank_routings": [{"scheme": "SWIFT", "address": "ABCDEF12"}],
  "attributes": [
    {"bank_id": "bank-001", "name": "param1", "type": "string", "value": "value1", "is_active": true}
  ]
}

Scenario 2: Bank without attributes
Input: bankId = "bank-002"
Processing: Retrieve bank details, routing info, no attributes found
Output: {
  "id": "bank-002",
  "short_name": "XYZ",
  "full_name": "XYZ Bank",
  "logo": "https://example.com/xyz-logo.png",
  "website": "https://xyzbank.com",
  "bank_routings": [{"scheme": "IBAN", "address": "DE89370400440532013000"}],
  "attributes": []
}
```

---

### BR-003: Basic Bank Information Composition for Bank List Retrieval

**Category**: TRANSFORMATION

**Description**: When retrieving the list of all banks, the system must return basic bank information for each bank including core fields (id, short_name, full_name, logo, website) and bank routing information, but explicitly exclude bank attributes for performance optimization.

**Source**: 
- File: BankService.scala (inferred from user story)
- Class/Object: BankService
- Method: getAllBanks
- Lines: N/A (derived from user story)

**Business Logic**:
1. Retrieve all banks from BankRepository
2. For each bank, include core fields (id, short_name, full_name, logo, website)
3. For each bank, include bank routing information
4. Explicitly exclude bank attributes from the response
5. Return list of banks (empty list if no banks exist)

**Variables**:
- **Input**: None (no parameters required)
- **Output**: Banks list object containing array of bank objects, each with:
  - id: string
  - short_name: string
  - full_name: string
  - logo: string (URL)
  - website: string (URL)
  - bank_routings: array of {scheme, address}
- **Constants**: None

**Business Conditions**:
| Condition | Business Meaning | Values/Thresholds |
|-----------|------------------|-------------------|
| Banks exist in system | At least one bank is registered | Non-empty banks array |
| No banks exist | No banks registered in system | Empty banks array [] |

**Business Impact**: 
This rule optimizes API performance by excluding detailed attribute data when listing all banks. This is a business decision to balance information completeness with system performance, as attributes are typically only needed when viewing a specific bank's details.

**API Endpoints Using This Rule**:
- GET /banks - Retrieve all banks list

**Related Test Cases**:
- Test case for retrieving list of banks with basic information
- Test case for empty bank list returning empty array (not 404)

**Migration Notes for Go**:
- Use a separate response struct for bank list that excludes attributes field
- Ensure empty list returns as empty JSON array [], not null
- Consider pagination for large bank lists in future iterations

**Example Scenarios**:
```
Scenario 1: Multiple banks exist
Input: None
Processing: Retrieve all banks, compose basic info for each
Output: {
  "banks": [
    {
      "id": "bank-001",
      "short_name": "ABC",
      "full_name": "ABC Bank",
      "logo": "https://example.com/logo.png",
      "website": "https://abcbank.com",
      "bank_routings": [{"scheme": "SWIFT", "address": "ABCDEF12"}]
    },
    {
      "id": "bank-002",
      "short_name": "XYZ",
      "full_name": "XYZ Bank",
      "logo": "https://example.com/xyz-logo.png",
      "website": "https://xyzbank.com",
      "bank_routings": [{"scheme": "IBAN", "address": "DE89370400440532013000"}]
    }
  ]
}

Scenario 2: No banks exist
Input: None
Processing: Query repository, no banks found
Output: {
  "banks": []
}
```

---

### BR-004: Empty Result Handling for Bank List

**Category**: DECISION

**Description**: When no banks exist in the system, the bank list endpoint must return an empty list with HTTP 200 status, not a 404 error. This distinguishes between "no data available" (valid state) and "resource not found" (error state).

**Source**: 
- File: BankService.scala (inferred from user story)
- Class/Object: BankService
- Method: getAllBanks
- Lines: N/A (derived from user story)

**Business Logic**:
1. Query bank repository for all banks
2. If no banks found, return empty banks array with HTTP 200 OK
3. Do not return 404 Not Found for empty results

**Variables**:
- **Input**: None
- **Output**: Empty banks list object: {"banks": []}
- **Constants**: None

**Business Conditions**:
| Condition | Business Meaning | Values/Thresholds |
|-----------|------------------|-------------------|
| No banks in system | Valid empty state | Return 200 with empty array |
| Banks exist | Normal operation | Return 200 with populated array |

**Business Impact**: 
This rule ensures consistent API behavior and proper semantic meaning of HTTP status codes. An empty list is a valid business state (e.g., new system with no banks configured), while 404 should be reserved for truly missing resources (e.g., specific bank ID not found).

**API Endpoints Using This Rule**:
- GET /banks - Retrieve all banks list

**Related Test Cases**:
- Test case for empty bank list returning 200 with empty array

**Migration Notes for Go**:
- Ensure the handler returns 200 OK even when the bank slice is empty
- Initialize the response slice to avoid nil pointer issues
- Use json.Marshal with empty slice to produce "[]" not "null"

**Example Scenarios**:
```
Scenario 1: No banks in system
Input: None
Processing: Query repository, no banks found
Output: HTTP 200 OK with body {"banks": []}

Scenario 2: Contrast with single bank not found
Input: bankId = "non-existent"
Processing: Query repository for specific bank, not found
Output: HTTP 404 Not Found (different endpoint, different rule BR-001)
```

---

## API Endpoint Coverage

| Endpoint | HTTP Method | Business Rules | Rule IDs |
|----------|-------------|----------------|----------|
| /banks | GET | Basic bank info composition, Empty result handling | BR-003, BR-004 |
| /banks/BANK_ID | GET | Bank existence validation, Complete bank info composition | BR-001, BR-002 |

## Migration Validation Matrix

| Rule ID | Test Case Reference | Go Implementation Status | Validation Status |
|---------|---------------------|--------------------------|-------------------|
| BR-001 | TestGetBankById_NotFound | Pending | Pending |
| BR-002 | TestGetBankById_WithAttributes | Pending | Pending |
| BR-003 | TestGetAllBanks_BasicInfo | Pending | Pending |
| BR-004 | TestGetAllBanks_EmptyList | Pending | Pending |

## Notes and Assumptions

1. **Source Code Assumption**: Since the user story does not include actual Scala source code, business rules were derived from the documented business logic, acceptance criteria, and technical context in the user story.

2. **Service Layer Inference**: The BankService and BankAttributeService classes are inferred from the user story's technical context section.

3. **Authentication Exclusion**: Authentication and authorization logic is mentioned as a dependency but not extracted as a business rule since it's a cross-cutting concern handled separately.

4. **Validation Rules Exclusion**: Input validation rules (e.g., "bank ID must be non-empty") are excluded as they are technical validations, not business rules per the extraction guidelines.

5. **Deprecated Fields**: The user story mentions potential deprecated fields (swiftBic, nationalIdentifier) as questions for SME. These are not included in the business rules pending clarification.

6. **Performance Consideration**: The decision to exclude attributes from the bank list endpoint (BR-003) is documented as a business decision for performance optimization, which is a valid business rule affecting API design.
