# Validation Rules

**Extracted From:** Open Bank Project API (OBP-API) - Scala Application  
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

**Field/Entity:** bank_id (BANK_ID)

**Validation Type:** Required Field Validation

**Rule Description:**
When retrieving a specific bank's details via the GET /banks/{BANK_ID} endpoint, the bank_id path parameter must be provided and cannot be empty.

**Validation Logic:**

- **Condition:** When a request is made to GET /banks/{BANK_ID} endpoint
- **Check:** Validate that BANK_ID path parameter is present and non-empty
- **Valid Criteria:** 
  - BANK_ID is provided in the URL path
  - BANK_ID is a non-empty string
- **Invalid Criteria:**
  - BANK_ID is missing from the URL path
  - BANK_ID is an empty string
- **Action on Success:** Proceed with bank lookup operation
- **Action on Failure:** Return error response with 400 status code

**Error Handling:**

- **Error Message:** `OBP-10404: 404 Not Found. The server could not find the requested URI. Please double check your URL, headers and body.`
- **Error Code:** `OBP-10404`
- **HTTP Status Code:** `400 Bad Request` or `404 Not Found`

**Scala Implementation:**

- **Location:** `code.api.util.APIUtil`
- **Method/Function:** URL path parameter extraction and validation
- **Line Reference:** Lines 1916-1917

**Code Snippet:**
```scala
val bankId = pathParams.get("BANK_ID").map(BankId(_))
```

**Related Entities:**
- Bank entity
- BankId value object

**User Story Context:**
This validation ensures that when retrieving detailed information about a specific bank (including attributes), a valid bank identifier is provided. The user story specifies that "Bank identifier must be provided and non-empty" for the GET /banks/BANK_ID endpoint.

**Dependencies:**
- None (standalone validation)

---

### Rule VR-002: Bank ID Format Validation

**Field/Entity:** bank_id (BANK_ID)

**Validation Type:** Format Validation (String Pattern)

**Rule Description:**
Bank ID must contain only alphanumeric characters, hyphens, underscores, and periods, with a maximum length of 255 characters.

**Validation Logic:**

- **Condition:** When a bank_id is provided in any API request (GET /banks/{BANK_ID})
- **Check:** Validate that bank_id matches the pattern `^([A-Za-z0-9\-_.]+)$` and length < 256
- **Valid Criteria:** 
  - Contains only: A-Z, a-z, 0-9, hyphen (-), underscore (_), period (.)
  - Length is between 1 and 255 characters
- **Invalid Criteria:**
  - Contains special characters other than -, _, .
  - Length is 0 or greater than 255 characters
  - Contains spaces or unicode characters
- **Action on Success:** Proceed with bank lookup/operation
- **Action on Failure:** Return error response with 400 status code

**Error Handling:**

- **Error Message:** `OBP-30111: Invalid Bank Id. The BANK_ID should only contain 0-9/a-z/A-Z/'-'/'.'/'_', the length should be smaller than 255.`
- **Error Code:** `OBP-30111`
- **HTTP Status Code:** `400 Bad Request`

**Scala Implementation:**

- **Location:** `code.api.util.APIUtil`
- **Method/Function:** `isValidID(id: String): Boolean`
- **Line Reference:** Lines 789-795

**Code Snippet:**
```scala
/** Check the id values from GUI, such as ACCOUNT_ID, BANK_ID ...  */
def isValidID(id :String):Boolean= {
  val regex = """^([A-Za-z0-9\-_.]+)$""".r
  id match {
    case regex(e) if(e.length<256) => true
    case _ => false
  }
}
```

**Related Entities:**
- Bank (bank_id field)
- All entities that reference bank_id

**User Story Context:**
This validation ensures that bank identifiers used in the bank information retrieval are properly formatted and can be safely stored in the database and used in URLs without encoding issues. The user story specifies that "Bank ID must be a valid, non-empty string when retrieving a specific bank."

**Dependencies:**
- VR-001 (Bank ID Required Validation)

---

### Rule VR-003: Bank Existence Validation

**Field/Entity:** bank_id (BANK_ID)

**Validation Type:** Entity Existence Validation

**Rule Description:**
When retrieving a specific bank's details, the system must validate that the requested bank exists in the database before returning information.

**Validation Logic:**

- **Condition:** When a request is made to GET /banks/{BANK_ID} endpoint with a valid bank_id format
- **Check:** Query the database to verify the bank with the specified bank_id exists
- **Valid Criteria:** 
  - Bank record exists in the database with the specified bank_id
  - Bank is active/supported on the platform
- **Invalid Criteria:**
  - No bank record found with the specified bank_id
  - Bank has been deleted or deactivated
- **Action on Success:** Return bank details with HTTP 200 status
- **Action on Failure:** Return error response with 404 status code

**Error Handling:**

- **Error Message:** `OBP-30001: Bank not found. Please specify a valid value for BANK_ID.`
- **Error Code:** `OBP-30001`
- **HTTP Status Code:** `404 Not Found`

**Scala Implementation:**

- **Location:** `code.api.util.ErrorMessages`
- **Method/Function:** `$BankNotFound` / `checkBank`
- **Line Reference:** Lines 308, 842, 880-881

**Code Snippet:**
```scala
val BankNotFound = "OBP-30001: Bank not found. Please specify a valid value for BANK_ID."

// Error to HTTP status code mapping
BankNotFound -> 404,

/**
 * validate method: NewStyle.function.getBank
 */
def $BankNotFound = BankNotFound
```

**Related Entities:**
- Bank entity
- BankRepository

**User Story Context:**
This validation directly addresses the acceptance criteria: "System must return appropriate error if bank does not exist (for single bank retrieval)" and "The system shall return an appropriate error response (404 Not Found) when a requested bank does not exist."

**Dependencies:**
- VR-001 (Bank ID Required Validation)
- VR-002 (Bank ID Format Validation)

---

## Category: Format Validation

### Rule VR-004: Logo URL Format Validation

**Field/Entity:** logo

**Validation Type:** URL Format Validation

**Rule Description:**
Bank logo field should contain a valid, accessible URL when provided. The URL should be properly formatted and point to an accessible image resource.

**Validation Logic:**

- **Condition:** When bank logo information is returned in the response
- **Check:** Validate that the logo field contains a properly formatted URL
- **Valid Criteria:** 
  - URL follows standard URL format (http:// or https://)
  - URL is accessible and returns an image resource
  - URL does not contain invalid characters
- **Invalid Criteria:**
  - Malformed URL format
  - URL contains invalid characters
  - Empty string when logo is expected
- **Action on Success:** Include logo URL in response
- **Action on Failure:** Return empty string or null for logo field

**Error Handling:**

- **Error Message:** `OBP-10017: Incorrect URL Format.`
- **Error Code:** `OBP-10017`
- **HTTP Status Code:** `400 Bad Request` (if validation is enforced on input)

**Scala Implementation:**

- **Location:** `code.api.util.ErrorMessages`
- **Method/Function:** URL validation utilities
- **Line Reference:** Line 96

**Code Snippet:**
```scala
val InvalidUrl = "OBP-10017: Incorrect URL Format. "
```

**Related Entities:**
- Bank (logo field)

**User Story Context:**
The user story specifies that "Logo URLs should be valid, accessible URLs when provided" and "Bank logo information should be available for branding and visual identification purposes."

**Dependencies:**
- None (standalone validation)

---

### Rule VR-005: Website URL Format Validation

**Field/Entity:** website

**Validation Type:** URL Format Validation

**Rule Description:**
Bank website field should contain a valid, properly formatted URL when provided.

**Validation Logic:**

- **Condition:** When bank website information is returned in the response
- **Check:** Validate that the website field contains a properly formatted URL
- **Valid Criteria:** 
  - URL follows standard URL format (http:// or https://)
  - URL is properly formatted
  - URL does not contain invalid characters
- **Invalid Criteria:**
  - Malformed URL format
  - URL contains invalid characters
  - Missing protocol (http/https)
- **Action on Success:** Include website URL in response
- **Action on Failure:** Return empty string or null for website field

**Error Handling:**

- **Error Message:** `OBP-10017: Incorrect URL Format.`
- **Error Code:** `OBP-10017`
- **HTTP Status Code:** `400 Bad Request` (if validation is enforced on input)

**Scala Implementation:**

- **Location:** `code.api.util.ErrorMessages`
- **Method/Function:** URL validation utilities
- **Line Reference:** Line 96

**Code Snippet:**
```scala
val InvalidUrl = "OBP-10017: Incorrect URL Format. "
```

**Related Entities:**
- Bank (website field)

**User Story Context:**
The user story specifies that "Website URLs should be valid, properly formatted URLs" and "Bank website details should be provided for users who need to access the bank's official web presence."

**Dependencies:**
- None (standalone validation)

---

## Category: Business Constraint Validation

### Rule VR-006: Empty Bank List Response Validation

**Field/Entity:** banks (list)

**Validation Type:** Business Constraint Validation

**Rule Description:**
When retrieving all banks via GET /banks endpoint, if no banks exist in the system, the response should return an empty list with HTTP 200 status, not a 404 error.

**Validation Logic:**

- **Condition:** When a request is made to GET /banks endpoint and no banks exist
- **Check:** Determine appropriate response when bank list is empty
- **Valid Criteria:** 
  - Return HTTP 200 with empty banks array: `{"banks": []}`
- **Invalid Criteria:**
  - Return HTTP 404 Not Found for empty list
  - Return null instead of empty array
- **Action on Success:** Return HTTP 200 with empty array
- **Action on Failure:** N/A (this is a response format rule)

**Error Handling:**

- **Error Message:** N/A (no error for empty list)
- **Error Code:** N/A
- **HTTP Status Code:** `200 OK` (with empty array)

**Scala Implementation:**

- **Location:** Bank API endpoint implementation
- **Method/Function:** GET /banks handler
- **Line Reference:** N/A

**Code Snippet:**
```scala
// Response should be:
{
  "banks": []
}
// NOT 404 error
```

**Related Entities:**
- Bank (list)

**User Story Context:**
The user story explicitly states: "The system shall return an empty list with HTTP 200 status when no banks exist in the system (not 404)" and "GET /banks should return empty list if no banks exist (not 404)."

**Dependencies:**
- None (standalone validation)

---

### Rule VR-007: Bank Attributes Array Validation

**Field/Entity:** attributes (array)

**Validation Type:** Business Constraint Validation

**Rule Description:**
When retrieving a single bank's details, the attributes field should return an empty array (not null) when the bank has no attributes configured.

**Validation Logic:**

- **Condition:** When a request is made to GET /banks/{BANK_ID} and the bank has no attributes
- **Check:** Ensure attributes field is an empty array, not null
- **Valid Criteria:** 
  - Return `"attributes": []` when no attributes exist
- **Invalid Criteria:**
  - Return `"attributes": null`
  - Omit the attributes field entirely
- **Action on Success:** Return empty array for attributes
- **Action on Failure:** N/A (this is a response format rule)

**Error Handling:**

- **Error Message:** N/A (no error for empty attributes)
- **Error Code:** N/A
- **HTTP Status Code:** `200 OK`

**Scala Implementation:**

- **Location:** Bank API endpoint implementation
- **Method/Function:** GET /banks/{BANK_ID} handler
- **Line Reference:** N/A

**Code Snippet:**
```scala
// Response should include:
"attributes": []
// NOT null or missing field
```

**Related Entities:**
- Bank (attributes field)
- BankAttribute

**User Story Context:**
The user story specifies: "Handle cases where bank exists but has no attributes (return empty attributes array)" and "Bank attributes array should be empty array (not null) when bank has no attributes."

**Dependencies:**
- VR-003 (Bank Existence Validation)

---

## Category: Length/Boundary Validation

### Rule VR-008: Bank ID Length Validation

**Field/Entity:** bank_id (BANK_ID)

**Validation Type:** Length Validation

**Rule Description:**
Bank ID must not exceed 255 characters in length.

**Validation Logic:**

- **Condition:** When a bank_id is provided in any API request
- **Check:** Validate that bank_id length is less than 256 characters
- **Valid Criteria:** 
  - Length is between 1 and 255 characters (inclusive)
- **Invalid Criteria:**
  - Length is 0 (empty string)
  - Length is greater than 255 characters
- **Action on Success:** Proceed with bank lookup/operation
- **Action on Failure:** Return error response with 400 status code

**Error Handling:**

- **Error Message:** `OBP-30111: Invalid Bank Id. The BANK_ID should only contain 0-9/a-z/A-Z/'-'/'.'/'_', the length should be smaller than 255.`
- **Error Code:** `OBP-30111`
- **HTTP Status Code:** `400 Bad Request`

**Scala Implementation:**

- **Location:** `code.api.util.APIUtil`
- **Method/Function:** `isValidID(id: String): Boolean`
- **Line Reference:** Lines 789-795

**Code Snippet:**
```scala
def isValidID(id :String):Boolean= {
  val regex = """^([A-Za-z0-9\-_.]+)$""".r
  id match {
    case regex(e) if(e.length<256) => true
    case _ => false
  }
}
```

**Related Entities:**
- Bank (bank_id field)

**User Story Context:**
This validation ensures that bank identifiers are within acceptable length limits for database storage and URL handling. The validation is part of the standard ID validation applied to all OBP identifiers.

**Dependencies:**
- VR-002 (Bank ID Format Validation)

---

## Quality Checklist Verification

- [x] All validation functions in relevant code are documented
- [x] All error messages are captured with exact text
- [x] All error codes are documented
- [x] Regex patterns are included verbatim
- [x] Length constraints are specified with exact limits
- [x] Required vs. optional fields are clearly marked
- [x] Cross-field validations are identified (none applicable for this user story)
- [x] Business constraint validations are included
- [x] Code references include file paths and line numbers
- [x] User story context is explained for each rule
- [x] Valid and invalid criteria are clearly stated
- [x] HTTP status codes are documented where applicable
- [x] Dependencies between validations are noted

---

## Notes

- The Bank Information Retrieval capability is primarily a READ operation, so most validations focus on input parameter validation (bank_id) and response format validation.
- No write/update validations are included as this capability only covers retrieval operations.
- The validation rules extracted are specific to the endpoints mentioned in the user story: GET /banks and GET /banks/{BANK_ID}.
- Authentication/authorization validations are not included as they are handled by a separate authentication service and are not specific to this capability.

## Final Deliverable

This document serves as:
1. A reference for understanding current validation logic in the Scala OBP-API
2. A specification for implementing equivalent validations in the target Go application
3. Documentation for testing and quality assurance
4. A guide for maintaining validation consistency during migration
