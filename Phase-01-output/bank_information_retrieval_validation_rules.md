# Validation Rules

**Extracted From:** Bank Information Retrieval User Story  
**User Story:** Bank Information Retrieval  
**Analysis Date:** December 01, 2025  
**Analyst:** Scala Systems Analyst

## Summary

Total Validation Rules Extracted: 8

### Validation Categories
- Input Validation Rules: 2
- Format Validation Rules: 0
- Business Constraint Rules: 3
- Length/Boundary Rules: 0
- Cross-Field Validation Rules: 0
- Response Validation Rules: 3

---

## Category: Input Validation

### Rule VR-001: Bank Identifier Required Validation

**Field/Entity:** BANK_ID (path parameter)

**Validation Type:** Required Field Validation

**Rule Description:**
Bank identifier must be provided and non-empty when retrieving single bank details via GET /banks/BANK_ID endpoint.

**Validation Logic:**

- **Condition:** When a request is made to GET /banks/BANK_ID endpoint
- **Check:** Validate that BANK_ID path parameter is provided and is not empty
- **Valid Criteria:** 
  - BANK_ID is present in the URL path
  - BANK_ID is not an empty string
- **Invalid Criteria:**
  - BANK_ID is missing from the URL path
  - BANK_ID is an empty string
- **Action on Success:** Proceed with bank lookup operation
- **Action on Failure:** Return error response indicating missing or empty bank identifier

**Error Handling:**

- **Error Message:** `Bank identifier must be provided and non-empty`
- **Error Code:** `N/A (to be defined during implementation)`
- **HTTP Status Code:** `400 Bad Request`

**Related Entities:**
- Bank entity (id field)
- BankService (retrieval operations)
- BankRepository (data access)

**User Story Context:**
This validation ensures that the bank identifier is provided when attempting to retrieve specific bank details. Without a valid identifier, the system cannot locate the requested bank record.

**Dependencies:**
- None (standalone validation)

---

### Rule VR-002: Bank Identifier Existence Validation

**Field/Entity:** BANK_ID (path parameter)

**Validation Type:** Entity Existence Validation

**Rule Description:**
Bank identifier must exist in the system when retrieving single bank details. If the bank does not exist, return a 404 Not Found error.

**Validation Logic:**

- **Condition:** When a request is made to GET /banks/BANK_ID endpoint with a valid (non-empty) BANK_ID
- **Check:** Validate that a bank with the given BANK_ID exists in the database
- **Valid Criteria:** 
  - Bank record with matching BANK_ID exists in the system
- **Invalid Criteria:**
  - No bank record found with the given BANK_ID
- **Action on Success:** Return bank details with attributes
- **Action on Failure:** Return 404 Not Found error response

**Error Handling:**

- **Error Message:** `Bank not found` or `Bank with identifier [BANK_ID] does not exist`
- **Error Code:** `N/A (to be defined during implementation)`
- **HTTP Status Code:** `404 Not Found`

**Related Entities:**
- Bank entity
- BankService (retrieval operations)
- BankRepository (data access)

**User Story Context:**
This validation ensures that the system returns an appropriate error when a user attempts to retrieve details for a bank that does not exist, providing clear feedback rather than returning empty or null data.

**Dependencies:**
- VR-001: Bank Identifier Required Validation (must pass first)

---

## Category: Business Constraint Validation

### Rule VR-003: Valid Bank Identifier Business Rule

**Field/Entity:** BANK_ID

**Validation Type:** Business Rule Validation

**Rule Description:**
Bank identifier must exist in the system to retrieve single bank details (GET /banks/BANK_ID). This is a business constraint ensuring data integrity.

**Validation Logic:**

- **Condition:** When retrieving single bank details via GET /banks/BANK_ID
- **Check:** Verify that the bank identifier corresponds to an existing bank entity
- **Valid Criteria:** 
  - Bank with the specified identifier exists in the database
- **Invalid Criteria:**
  - Bank with the specified identifier does not exist
- **Action on Success:** Proceed with returning complete bank information including attributes
- **Action on Failure:** Return appropriate error indicating bank not found

**Error Handling:**

- **Error Message:** `Bank does not exist`
- **Error Code:** `N/A (to be defined during implementation)`
- **HTTP Status Code:** `404 Not Found`

**Related Entities:**
- Bank entity
- BankService
- BankRepository

**User Story Context:**
This business rule ensures that only valid, existing banks can have their details retrieved, maintaining data consistency and preventing invalid data access attempts.

**Dependencies:**
- VR-001: Bank Identifier Required Validation

---

### Rule VR-004: Complete Information for Single Bank

**Field/Entity:** Bank entity (response)

**Validation Type:** Response Completeness Validation

**Rule Description:**
Retrieved single bank details must include all specified fields: name (short and full), logo, website, routing information, and attributes.

**Validation Logic:**

- **Condition:** When returning response for GET /banks/BANK_ID endpoint
- **Check:** Ensure response contains all required fields
- **Valid Criteria:** 
  Response must include:
  - id (string)
  - short_name (string)
  - full_name (string)
  - logo (string - URL)
  - website (string - URL)
  - bank_routings (array of objects with scheme and address)
  - attributes (array of objects with bank_id, name, type, value, is_active)
- **Invalid Criteria:**
  - Any of the required fields is missing from the response
- **Action on Success:** Return complete bank details response
- **Action on Failure:** System error - incomplete data configuration

**Error Handling:**

- **Error Message:** `N/A (internal validation)`
- **Error Code:** `N/A`
- **HTTP Status Code:** `200 OK` (for successful retrieval)

**Related Entities:**
- Bank entity (all fields)
- BankAttribute entity
- BankRouting entity

**User Story Context:**
This validation ensures that when a user retrieves single bank details, they receive complete information including operational parameters (attributes) for comprehensive bank information display.

**Dependencies:**
- VR-002: Bank Identifier Existence Validation

---

### Rule VR-005: Basic Information for Bank List

**Field/Entity:** Banks collection (response)

**Validation Type:** Response Completeness Validation

**Rule Description:**
Retrieved bank list must include basic fields for each bank (name, logo, website, routing) but NOT attributes. Attributes are excluded from list responses for performance reasons.

**Validation Logic:**

- **Condition:** When returning response for GET /banks endpoint
- **Check:** Ensure each bank in the response contains basic fields but excludes attributes
- **Valid Criteria:** 
  Each bank in response must include:
  - id (string)
  - short_name (string)
  - full_name (string)
  - logo (string - URL)
  - website (string - URL)
  - bank_routings (array of objects with scheme and address)
  
  Each bank must NOT include:
  - attributes field
- **Invalid Criteria:**
  - Missing any of the basic required fields
  - Including attributes in list response
- **Action on Success:** Return list of banks with basic information
- **Action on Failure:** System error - incorrect response structure

**Error Handling:**

- **Error Message:** `N/A (internal validation)`
- **Error Code:** `N/A`
- **HTTP Status Code:** `200 OK` (for successful retrieval)

**Related Entities:**
- Bank entity (basic fields only)
- BankRouting entity

**User Story Context:**
This validation ensures that the bank list endpoint returns only essential information for performance optimization, while the single bank endpoint provides complete details including attributes.

**Dependencies:**
- None

---

## Category: Response Validation

### Rule VR-006: Empty Bank List Handling

**Field/Entity:** Banks collection (response)

**Validation Type:** Response Handling Validation

**Rule Description:**
GET /banks endpoint should return an empty list if no banks exist in the system, NOT a 404 error. This ensures consistent API behavior.

**Validation Logic:**

- **Condition:** When processing GET /banks request and no banks exist in the system
- **Check:** Verify response behavior when bank collection is empty
- **Valid Criteria:** 
  - Return HTTP 200 OK with empty banks array: `{"banks": []}`
- **Invalid Criteria:**
  - Returning 404 Not Found when no banks exist
  - Returning null or undefined instead of empty array
- **Action on Success:** Return 200 OK with empty banks array
- **Action on Failure:** Incorrect error response

**Error Handling:**

- **Error Message:** `N/A (no error for empty list)`
- **Error Code:** `N/A`
- **HTTP Status Code:** `200 OK` (always for GET /banks, even if empty)

**Related Entities:**
- Banks collection

**User Story Context:**
This validation ensures that the API returns a consistent response structure even when no banks are configured in the system, allowing client applications to handle the response uniformly.

**Dependencies:**
- None

---

### Rule VR-007: Empty Attributes Array Handling

**Field/Entity:** Bank attributes (response)

**Validation Type:** Response Handling Validation

**Rule Description:**
When a bank exists but has no attributes configured, the system should return an empty attributes array rather than null or omitting the field.

**Validation Logic:**

- **Condition:** When retrieving single bank details via GET /banks/BANK_ID and the bank has no attributes
- **Check:** Verify that attributes field is present as an empty array
- **Valid Criteria:** 
  - Return attributes as empty array: `"attributes": []`
- **Invalid Criteria:**
  - Returning null for attributes
  - Omitting the attributes field entirely
  - Returning error when bank has no attributes
- **Action on Success:** Return bank details with empty attributes array
- **Action on Failure:** Inconsistent response structure

**Error Handling:**

- **Error Message:** `N/A (no error for empty attributes)`
- **Error Code:** `N/A`
- **HTTP Status Code:** `200 OK`

**Related Entities:**
- Bank entity
- BankAttribute entity

**User Story Context:**
This validation ensures consistent API response structure, allowing client applications to always expect an attributes array in single bank responses, simplifying client-side data handling.

**Dependencies:**
- VR-002: Bank Identifier Existence Validation

---

### Rule VR-008: HTTP Status Code Validation

**Field/Entity:** API Response

**Validation Type:** Response Status Validation

**Rule Description:**
The API must return appropriate HTTP status codes: 200 for successful retrieval, 404 for bank not found (single bank retrieval only).

**Validation Logic:**

- **Condition:** When processing any bank retrieval request
- **Check:** Verify correct HTTP status code is returned based on operation result
- **Valid Criteria:** 
  - GET /banks: Always return 200 OK (even if empty list)
  - GET /banks/BANK_ID with existing bank: Return 200 OK
  - GET /banks/BANK_ID with non-existing bank: Return 404 Not Found
- **Invalid Criteria:**
  - Returning incorrect status codes for the operation result
  - Returning 404 for empty bank list
  - Returning 200 for non-existing bank
- **Action on Success:** Return appropriate status code with response body
- **Action on Failure:** Incorrect HTTP semantics

**Error Handling:**

- **Error Message:** Varies based on scenario
- **Error Code:** `N/A`
- **HTTP Status Code:** `200 OK` or `404 Not Found` (as appropriate)

**Related Entities:**
- All bank retrieval endpoints

**User Story Context:**
This validation ensures the API follows RESTful conventions and HTTP semantics, providing clear and predictable responses to client applications.

**Dependencies:**
- VR-002: Bank Identifier Existence Validation (for 404 scenarios)

---

## Validation Rules Summary Table

| Rule ID | Rule Name | Field/Entity | Validation Type | HTTP Status on Failure |
|---------|-----------|--------------|-----------------|------------------------|
| VR-001 | Bank Identifier Required | BANK_ID | Required Field | 400 |
| VR-002 | Bank Identifier Existence | BANK_ID | Entity Existence | 404 |
| VR-003 | Valid Bank Identifier Business Rule | BANK_ID | Business Rule | 404 |
| VR-004 | Complete Information for Single Bank | Bank (response) | Response Completeness | N/A |
| VR-005 | Basic Information for Bank List | Banks (response) | Response Completeness | N/A |
| VR-006 | Empty Bank List Handling | Banks collection | Response Handling | N/A |
| VR-007 | Empty Attributes Array Handling | Bank attributes | Response Handling | N/A |
| VR-008 | HTTP Status Code Validation | API Response | Response Status | N/A |

---

## Notes

1. **No Scala Code References**: Since this extraction is based solely on the user story document content, no actual Scala code snippets or file references are included. During implementation, these validation rules should be mapped to appropriate service and repository methods.

2. **Error Codes**: Specific error codes (e.g., OBP-XXXXX format) should be defined during implementation based on the application's error code conventions.

3. **Authentication**: The user story mentions dependency on "Authentication service (to verify user permissions for viewing bank details)" but does not specify validation rules for authentication. Authentication validation rules should be extracted from the relevant authentication user story.

4. **Deprecated Fields**: The user story raises a question about deprecated fields (swiftBic, nationalIdentifier). If these fields are to be included, additional validation rules may be needed.

5. **Bank Routing Information**: The user story confirms that bank routing information should be included in responses. No specific validation rules for routing data format are specified in the user story.
