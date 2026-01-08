# Validation Rules

**Extracted From:** Open Bank Project API (OBP-API)  
**User Story:** Bank Information Retrieval  
**Analysis Date:** 2026-01-08  
**Analyst:** Scala Systems Analyst

## Summary

Total Validation Rules Extracted: 8

### Validation Categories
- Input Validation Rules: 3
- Format Validation Rules: 2
- Business Constraint Rules: 2
- Length/Boundary Rules: 1
- Cross-Field Validation Rules: 0

---

## Category: Input Validation

### Rule VR-001: Bank ID Required Validation

**Field/Entity:** BANK_ID (path parameter)

**Validation Type:** Required Field Validation

**Rule Description:**
When retrieving a specific bank by ID, the Bank ID must be provided as a non-empty string in the request path parameter.

**Validation Logic:**

- **Condition:** When a request is made to `GET /obp/v5.1.0/banks/{BANK_ID}` endpoint
- **Check:** Validate that BANK_ID path parameter is present and not empty
- **Valid Criteria:** 
  - BANK_ID is provided in the URL path
  - BANK_ID is a non-empty string
- **Invalid Criteria:**
  - BANK_ID is missing from the path
  - BANK_ID is an empty string
- **Action on Success:** Proceed with bank lookup operation
- **Action on Failure:** Return error response with 400 Bad Request status code

**Error Handling:**

- **Error Message:** `Bank ID must be provided and cannot be empty`
- **Error Code:** `OBP-30001` (Invalid Bank ID)
- **HTTP Status Code:** `400 Bad Request`

**Implementation:**

- **Location:** Bank API Controller / Bank Service
- **Method/Function:** `getBankById(bankId: String)`

**Related Entities:**
- Bank entity (id field)
- All downstream operations that require bank_id

**User Story Context:**
This validation ensures that when retrieving detailed information for a specific bank, a valid bank identifier is provided. The user story states "Bank ID must be a valid, non-empty string when retrieving a specific bank."

**Dependencies:**
- None (standalone validation)

---

### Rule VR-002: Bank Existence Validation

**Field/Entity:** BANK_ID

**Validation Type:** Entity Existence Validation

**Rule Description:**
The system must validate that the requested bank exists in the database before returning bank information.

**Validation Logic:**

- **Condition:** When a request is made to retrieve a specific bank by ID
- **Check:** Query the bank repository to verify the bank with the given ID exists
- **Valid Criteria:** 
  - Bank with the specified BANK_ID exists in the system
  - Bank is active/supported on the platform
- **Invalid Criteria:**
  - No bank found with the specified BANK_ID
  - Bank exists but is not active/supported
- **Action on Success:** Return bank information with HTTP 200 status
- **Action on Failure:** Return HTTP 404 Not Found error response

**Error Handling:**

- **Error Message:** `Bank not found. Please specify a valid value for BANK_ID.`
- **Error Code:** `OBP-30001` (Bank Not Found)
- **HTTP Status Code:** `404 Not Found`

**Implementation:**

- **Location:** Bank Service / Bank Repository
- **Method/Function:** `getBankById(bankId: String): Box[Bank]`

**Related Entities:**
- Bank entity
- Bank repository

**User Story Context:**
The user story acceptance criteria states "The system shall return an appropriate error response (404 Not Found) when a requested bank does not exist." This validation ensures proper error handling for non-existent banks.

**Dependencies:**
- VR-001: Bank ID Required Validation (must pass first)

---

### Rule VR-003: Empty Bank List Response Validation

**Field/Entity:** Banks list response

**Validation Type:** Response Format Validation

**Rule Description:**
When no banks exist in the system, the API must return an empty list with HTTP 200 status, not a 404 error.

**Validation Logic:**

- **Condition:** When a request is made to `GET /obp/v5.1.0/banks` and no banks exist
- **Check:** Verify the response format when bank list is empty
- **Valid Criteria:** 
  - Return HTTP 200 OK status
  - Return JSON response with empty banks array: `{"banks": []}`
- **Invalid Criteria:**
  - Returning HTTP 404 Not Found for empty list
  - Returning null instead of empty array
- **Action on Success:** Return empty array with HTTP 200
- **Action on Failure:** N/A (this is a response format rule)

**Error Handling:**

- **Error Message:** N/A (no error - valid empty response)
- **Error Code:** N/A
- **HTTP Status Code:** `200 OK`

**Implementation:**

- **Location:** Bank API Controller
- **Method/Function:** `getBanks(): List[Bank]`

**Related Entities:**
- Banks list endpoint response

**User Story Context:**
The user story explicitly states "The system shall return an empty list with HTTP 200 status when no banks exist in the system (not 404)." This ensures consistent API behavior.

**Dependencies:**
- None (standalone validation)

---

## Category: Format Validation

### Rule VR-004: Logo URL Format Validation

**Field/Entity:** logo (Bank entity field)

**Validation Type:** URL Format Validation

**Rule Description:**
Bank logo URLs must be valid, properly formatted, and accessible URLs when provided in the response.

**Validation Logic:**

- **Condition:** When bank information is returned containing a logo field
- **Check:** Validate that the logo field contains a valid URL format
- **Valid Criteria:** 
  - URL follows standard URL format (http:// or https://)
  - URL is properly encoded
  - URL points to an accessible image resource
- **Invalid Criteria:**
  - Malformed URL string
  - Invalid URL scheme
  - URL with invalid characters
- **Action on Success:** Include logo URL in response
- **Action on Failure:** Return empty string or null for logo field (graceful handling)

**Error Handling:**

- **Error Message:** N/A (graceful handling - invalid URLs should be filtered/cleaned)
- **Error Code:** N/A
- **HTTP Status Code:** N/A (data quality issue, not request error)

**Implementation:**

- **Location:** Bank Service / Bank Data Mapper
- **Method/Function:** `validateLogoUrl(url: String): Option[String]`

**Related Entities:**
- Bank entity (logo field)
- CDN or storage system for bank logo assets

**User Story Context:**
The user story data validations section states "Logo URLs should be valid, accessible URLs when provided." This ensures that logo URLs returned to clients are usable for displaying bank branding.

**Dependencies:**
- None (standalone validation)

---

### Rule VR-005: Website URL Format Validation

**Field/Entity:** website (Bank entity field)

**Validation Type:** URL Format Validation

**Rule Description:**
Bank website URLs must be valid, properly formatted URLs when provided in the response.

**Validation Logic:**

- **Condition:** When bank information is returned containing a website field
- **Check:** Validate that the website field contains a valid URL format
- **Valid Criteria:** 
  - URL follows standard URL format (http:// or https://)
  - URL is properly encoded
  - URL contains valid domain name
- **Invalid Criteria:**
  - Malformed URL string
  - Invalid URL scheme
  - URL with invalid characters
  - Missing protocol prefix
- **Action on Success:** Include website URL in response
- **Action on Failure:** Return empty string or null for website field (graceful handling)

**Error Handling:**

- **Error Message:** N/A (graceful handling - invalid URLs should be filtered/cleaned)
- **Error Code:** N/A
- **HTTP Status Code:** N/A (data quality issue, not request error)

**Implementation:**

- **Location:** Bank Service / Bank Data Mapper
- **Method/Function:** `validateWebsiteUrl(url: String): Option[String]`

**Related Entities:**
- Bank entity (website field)

**User Story Context:**
The user story data validations section states "Website URLs should be valid, properly formatted URLs." This ensures that website URLs returned to clients are usable for linking to bank websites.

**Dependencies:**
- None (standalone validation)

---

## Category: Business Constraint Validation

### Rule VR-006: Active/Supported Bank Filter Validation

**Field/Entity:** Bank entity (status/active flag)

**Validation Type:** Business Rule Validation

**Rule Description:**
Only banks that are supported/active on the platform should be returned in retrieval results.

**Validation Logic:**

- **Condition:** When retrieving bank list or specific bank information
- **Check:** Filter results to include only active/supported banks
- **Valid Criteria:** 
  - Bank has active status
  - Bank is configured as supported on the platform
- **Invalid Criteria:**
  - Bank is inactive or disabled
  - Bank is not supported on the platform instance
- **Action on Success:** Include bank in response
- **Action on Failure:** Exclude bank from response (for list) or return 404 (for specific bank)

**Error Handling:**

- **Error Message:** `Bank not found` (if specific inactive bank requested)
- **Error Code:** `OBP-30001`
- **HTTP Status Code:** `404 Not Found` (for specific bank request)

**Implementation:**

- **Location:** Bank Repository / Bank Service
- **Method/Function:** `getActiveBanks(): List[Bank]`, `getBankById(bankId: String): Box[Bank]`

**Related Entities:**
- Bank entity (active/status field)
- Bank configuration

**User Story Context:**
The user story business rules state "Only banks that are supported/active on the platform should be returned in the retrieval results." This ensures that deprecated or inactive banks are not exposed to API consumers.

**Dependencies:**
- None (standalone validation)

---

### Rule VR-007: Response Schema Conformance Validation

**Field/Entity:** Bank response object

**Validation Type:** Schema Validation

**Rule Description:**
Response data must conform to the expected schema with all required fields populated.

**Validation Logic:**

- **Condition:** When bank information is serialized for API response
- **Check:** Validate that all required fields are present and properly typed
- **Valid Criteria:** 
  - id field is present and non-null (string)
  - short_name field is present (string)
  - full_name field is present (string)
  - logo field is present (string, can be empty)
  - website field is present (string, can be empty)
  - bank_routings field is present (array, can be empty)
  - attributes field is present for single bank response (array, can be empty)
- **Invalid Criteria:**
  - Missing required fields
  - Null values for required fields
  - Incorrect data types
- **Action on Success:** Return properly formatted JSON response
- **Action on Failure:** Log error and return sanitized response or error

**Error Handling:**

- **Error Message:** `Internal server error` (if schema validation fails)
- **Error Code:** `OBP-50000`
- **HTTP Status Code:** `500 Internal Server Error`

**Implementation:**

- **Location:** Bank API Controller / JSON Serializer
- **Method/Function:** `toJson(bank: Bank): JValue`

**Related Entities:**
- Bank entity
- BankJson response class
- BanksJson response class (for list)

**User Story Context:**
The user story data validations section states "Response data should conform to the expected schema with all required fields populated." This ensures API consumers receive consistent, well-formed responses.

**Dependencies:**
- None (standalone validation)

---

## Category: Length/Boundary Validation

### Rule VR-008: Bank Attributes Empty Array Validation

**Field/Entity:** attributes (Bank entity field)

**Validation Type:** Null Safety Validation

**Rule Description:**
Bank attributes array should be an empty array (not null) when a bank has no attributes.

**Validation Logic:**

- **Condition:** When returning single bank information with attributes
- **Check:** Ensure attributes field is never null
- **Valid Criteria:** 
  - attributes field is an empty array `[]` when no attributes exist
  - attributes field contains array of attribute objects when attributes exist
- **Invalid Criteria:**
  - attributes field is null
  - attributes field is omitted from response
- **Action on Success:** Return attributes as array (empty or populated)
- **Action on Failure:** Convert null to empty array before response

**Error Handling:**

- **Error Message:** N/A (handled internally)
- **Error Code:** N/A
- **HTTP Status Code:** N/A

**Implementation:**

- **Location:** Bank Service / Bank Data Mapper
- **Method/Function:** `getBankAttributes(bankId: String): List[BankAttribute]`

**Related Entities:**
- Bank entity
- BankAttribute entity

**User Story Context:**
The user story data validations section states "Bank attributes array should be empty array (not null) when bank has no attributes." This ensures consistent JSON structure for API consumers.

**Dependencies:**
- None (standalone validation)

---

## Quality Checklist Verification

- [x] All validation functions relevant to the user story are documented
- [x] All error messages are captured with exact text where applicable
- [x] Error codes are documented where applicable
- [x] Required vs. optional fields are clearly marked
- [x] Business constraint validations are included
- [x] User story context is explained for each rule
- [x] Valid and invalid criteria are clearly stated
- [x] HTTP status codes are documented where applicable
- [x] Dependencies between validations are noted

---

## Notes

- The user story focuses on READ/RETRIEVAL operations only, so validation rules are primarily related to input validation and response formatting
- No create, update, or delete validations are in scope based on the user story
- URL validations (VR-004, VR-005) are data quality validations that should be handled gracefully rather than causing request failures
- The distinction between list endpoint (excludes attributes) and single bank endpoint (includes attributes) is a design decision documented in VR-007 and VR-008
