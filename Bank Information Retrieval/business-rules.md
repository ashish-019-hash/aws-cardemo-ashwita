# Business Rules Extraction

**Extracted From**: Bank Information Retrieval Capability (OBP-API Scala Application)
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

### BR-001: Bank Existence Verification

**Category**: DECISION

**Description**: When retrieving a specific bank by its identifier, the system must verify that the bank exists in the platform before returning bank information. If the bank does not exist, the system must return a "not found" response.

**Source**: 
- File: APIMethods400.scala
- Class/Object: APIMethods400
- Method: getBank
- Lines: 259-301

**Business Logic**:
1. Receive bank identifier (BANK_ID) from the request path
2. Query the bank repository to check if a bank with the given identifier exists
3. If bank exists, proceed to retrieve and return bank information
4. If bank does not exist, return HTTP 404 Not Found error response

**Variables**:
- **Input**: BANK_ID (string) - The unique identifier of the bank to retrieve
- **Output**: Bank information object OR error response
- **Constants**: None

**Business Conditions**:
| Condition | Business Meaning | Values/Thresholds |
|-----------|------------------|-------------------|
| Bank exists in system | Bank is registered on the platform | Bank record found in repository |
| Bank does not exist | Bank is not registered on the platform | No bank record found |

**Business Impact**: 
This rule ensures data integrity and prevents the system from returning invalid or non-existent bank information. It protects API consumers from receiving misleading data and maintains trust in the platform's bank directory.

**API Endpoints Using This Rule**:
- GET /banks/BANK_ID - Retrieve single bank details

**Related Test Cases**:
- Test case for retrieving existing bank (should return 200 OK with bank data)
- Test case for retrieving non-existent bank (should return 404 Not Found)

**Migration Notes for Go**:
- Implement bank existence check before returning bank data
- Use appropriate Go error handling patterns (return error, nil pattern)
- Map Scala Option/Either patterns to Go's explicit error returns

**Example Scenarios**:
```
Scenario 1: Bank exists
Input: BANK_ID = "gh.29.uk"
Processing: Query bank repository for "gh.29.uk"
Output: Bank information object with id, name, logo, website, routings, attributes

Scenario 2: Bank does not exist
Input: BANK_ID = "invalid-bank-id"
Processing: Query bank repository for "invalid-bank-id" - not found
Output: HTTP 404 Not Found error response
```

---

### BR-002: Bank List Empty Result Handling

**Category**: DECISION

**Description**: When retrieving the list of all banks and no banks exist in the system, the system must return an empty list with a successful HTTP 200 status code, not a 404 error. This distinguishes between "no banks exist" (valid empty state) and "resource not found" (error state).

**Source**: 
- File: APIMethods400.scala
- Class/Object: APIMethods400
- Method: getBanks
- Lines: 259-301

**Business Logic**:
1. Query the bank repository for all banks
2. If banks exist, return the list of banks with HTTP 200 OK
3. If no banks exist (empty result), return an empty array [] with HTTP 200 OK
4. Never return 404 for an empty bank list

**Variables**:
- **Input**: None (list all banks operation)
- **Output**: Array of bank objects (may be empty) with HTTP 200 status
- **Constants**: None

**Business Conditions**:
| Condition | Business Meaning | Values/Thresholds |
|-----------|------------------|-------------------|
| Banks exist | Platform has registered banks | Bank count > 0 |
| No banks exist | Platform has no registered banks | Bank count = 0 |

**Business Impact**: 
This rule ensures consistent API behavior and proper semantic meaning of HTTP status codes. An empty list is a valid business state (platform with no banks yet), while 404 indicates a resource addressing error. This distinction is critical for API consumers to handle responses correctly.

**API Endpoints Using This Rule**:
- GET /banks - Retrieve all banks

**Related Test Cases**:
- Test case for retrieving banks when banks exist (should return 200 OK with bank array)
- Test case for retrieving banks when no banks exist (should return 200 OK with empty array)

**Migration Notes for Go**:
- Return empty slice []Bank{} instead of nil when no banks found
- Ensure JSON serialization produces [] not null for empty slices
- Use json:",omitempty" tag carefully to avoid omitting empty arrays

**Example Scenarios**:
```
Scenario 1: Banks exist in system
Input: GET /banks
Processing: Query returns 3 banks
Output: HTTP 200 OK with {"banks": [{...}, {...}, {...}]}

Scenario 2: No banks in system
Input: GET /banks
Processing: Query returns 0 banks
Output: HTTP 200 OK with {"banks": []}
```

---

### BR-003: Bank Information Response Composition for List Retrieval

**Category**: TRANSFORMATION

**Description**: When retrieving the list of all banks, the system must return basic bank information for each bank including: bank ID, short name, full name, logo URL, website URL, and bank routing information. Detailed bank attributes are excluded from the list response for performance optimization.

**Source**: 
- File: JSONFactory400.scala
- Class/Object: JSONFactory400
- Method: createBanksJson
- Lines: Referenced in APIMethods400.scala

**Business Logic**:
1. For each bank in the system, compose a response object containing:
   - id: The unique bank identifier
   - short_name: The abbreviated bank name
   - full_name: The complete official bank name
   - logo: URL to the bank's logo image
   - website: URL to the bank's official website
   - bank_routings: Array of routing information (scheme and address pairs)
2. Exclude detailed bank attributes from the list response
3. Return the composed list of bank objects

**Variables**:
- **Input**: List of Bank entities from repository
- **Output**: JSON array of bank objects with basic information
- **Constants**: None

**Business Conditions**:
| Condition | Business Meaning | Values/Thresholds |
|-----------|------------------|-------------------|
| List retrieval | User wants overview of all banks | GET /banks endpoint |
| Attributes excluded | Performance optimization for list view | No attributes in response |

**Business Impact**: 
This rule defines the data contract for the bank list API. By excluding detailed attributes from the list response, the system optimizes performance for scenarios where consumers need a quick overview of available banks without the overhead of loading all attribute data.

**API Endpoints Using This Rule**:
- GET /banks - Retrieve all banks

**Related Test Cases**:
- Test case verifying bank list response contains required fields (id, short_name, full_name, logo, website, bank_routings)
- Test case verifying bank list response does NOT contain attributes field

**Migration Notes for Go**:
- Define separate Go structs for list response (BankListItem) vs single bank response (BankDetail)
- Use struct tags to control JSON field names
- Consider using a mapper function to transform Bank entity to BankListItem

**Example Scenarios**:
```
Scenario 1: Bank list response composition
Input: Bank entity with all fields including attributes
Processing: Extract only basic fields, exclude attributes
Output: 
{
  "id": "gh.29.uk",
  "short_name": "Bank of UK",
  "full_name": "The Bank of the United Kingdom",
  "logo": "https://example.com/logo.png",
  "website": "https://www.bankofuk.com",
  "bank_routings": [
    {"scheme": "BIC", "address": "BKUKGB2L"}
  ]
}
```

---

### BR-004: Bank Information Response Composition for Single Bank Retrieval

**Category**: TRANSFORMATION

**Description**: When retrieving a specific bank by ID, the system must return complete bank information including all basic fields (id, short name, full name, logo, website, bank routings) PLUS detailed bank attributes. If the bank has no attributes, an empty attributes array must be returned (not null).

**Source**: 
- File: JSONFactory400.scala
- Class/Object: JSONFactory400
- Method: createBankJSON400
- Lines: Referenced in APIMethods400.scala

**Business Logic**:
1. Retrieve the bank entity by ID
2. Retrieve all attributes associated with the bank
3. Compose a response object containing:
   - id: The unique bank identifier
   - short_name: The abbreviated bank name
   - full_name: The complete official bank name
   - logo: URL to the bank's logo image
   - website: URL to the bank's official website
   - bank_routings: Array of routing information (scheme and address pairs)
   - attributes: Array of bank attribute objects (bank_id, name, type, value, is_active)
4. If bank has no attributes, return empty array [] for attributes field (not null)

**Variables**:
- **Input**: BANK_ID (string) - The unique identifier of the bank
- **Output**: JSON object with complete bank information including attributes
- **Constants**: None

**Business Conditions**:
| Condition | Business Meaning | Values/Thresholds |
|-----------|------------------|-------------------|
| Single bank retrieval | User wants complete details for one bank | GET /banks/BANK_ID endpoint |
| Attributes included | Full information for detailed view | Attributes array in response |
| No attributes exist | Bank has no custom attributes configured | Empty array [], not null |

**Business Impact**: 
This rule defines the complete data contract for single bank retrieval. By including attributes only in the single bank response, the system provides detailed information when a consumer specifically requests it, while keeping the list endpoint lightweight. The empty array handling ensures consistent response structure regardless of attribute presence.

**API Endpoints Using This Rule**:
- GET /banks/BANK_ID - Retrieve single bank details

**Related Test Cases**:
- Test case verifying single bank response contains all required fields including attributes
- Test case verifying single bank response returns empty attributes array (not null) when bank has no attributes
- Test case verifying attribute objects contain bank_id, name, type, value, is_active fields

**Migration Notes for Go**:
- Define BankDetail struct with Attributes field as []BankAttribute (not *[]BankAttribute)
- Initialize Attributes to empty slice if nil to ensure [] in JSON output
- Use json:",omitempty" carefully - may need custom marshaler to always include attributes field

**Example Scenarios**:
```
Scenario 1: Bank with attributes
Input: BANK_ID = "gh.29.uk"
Processing: Retrieve bank and its 2 attributes
Output: 
{
  "id": "gh.29.uk",
  "short_name": "Bank of UK",
  "full_name": "The Bank of the United Kingdom",
  "logo": "https://example.com/logo.png",
  "website": "https://www.bankofuk.com",
  "bank_routings": [{"scheme": "BIC", "address": "BKUKGB2L"}],
  "attributes": [
    {"bank_id": "gh.29.uk", "name": "OVERDRAFT_LIMIT", "type": "INTEGER", "value": "5000", "is_active": true},
    {"bank_id": "gh.29.uk", "name": "SUPPORTS_SEPA", "type": "BOOLEAN", "value": "true", "is_active": true}
  ]
}

Scenario 2: Bank without attributes
Input: BANK_ID = "new.bank.id"
Processing: Retrieve bank, no attributes found
Output: 
{
  "id": "new.bank.id",
  "short_name": "New Bank",
  "full_name": "The New Bank",
  "logo": "https://example.com/newlogo.png",
  "website": "https://www.newbank.com",
  "bank_routings": [],
  "attributes": []
}
```

---

## API Endpoint Coverage

| Endpoint | HTTP Method | Business Rules | Rule IDs |
|----------|-------------|----------------|----------|
| /banks | GET | Empty list handling, Response composition for list | BR-002, BR-003 |
| /banks/BANK_ID | GET | Bank existence verification, Response composition for single bank | BR-001, BR-004 |

## Migration Validation Matrix

| Rule ID | Test Case Reference | Go Implementation Status | Validation Status |
|---------|---------------------|--------------------------|-------------------|
| BR-001 | TestGetBankNotFound, TestGetBankSuccess | Pending | Pending |
| BR-002 | TestGetBanksEmpty, TestGetBanksSuccess | Pending | Pending |
| BR-003 | TestBankListResponseFields | Pending | Pending |
| BR-004 | TestSingleBankResponseFields, TestBankWithNoAttributes | Pending | Pending |

## Notes and Assumptions

1. **Scope Limitation**: This extraction focuses only on the Bank Information Retrieval capability as described in the user story. No CREATE, UPDATE, or DELETE operations are included as they are not part of this capability's description.

2. **Authentication Assumption**: The user story mentions authentication service as an upstream dependency. Access control rules for bank information retrieval are assumed to be handled at a higher level and are not extracted as business rules here.

3. **Routing Information**: Bank routing information (scheme and address) is included in both list and single bank responses. The structure of routing information is assumed to be consistent across all banks.

4. **Attribute Structure**: Bank attributes follow a consistent structure with bank_id, name, type, value, and is_active fields. The type field indicates the data type of the value (e.g., INTEGER, BOOLEAN, STRING).

5. **Performance Consideration**: The decision to exclude attributes from the list response is a business-driven performance optimization, not just a technical choice. This is documented as a business rule (BR-003) because it affects the API contract and consumer expectations.

6. **Deprecated Fields**: The user story mentions questions about deprecated fields (swiftBic, nationalIdentifier). These are not included in the business rules as they are flagged as questions for SME and not confirmed as part of the current response structure.
