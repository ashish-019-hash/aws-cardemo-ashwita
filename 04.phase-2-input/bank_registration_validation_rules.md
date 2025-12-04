# Validation Rules

**Extracted From:** Bank Registration and Configuration User Story  
**User Story:** Bank Registration and Configuration  
**Analysis Date:** 2025-11-25  
**Analyst:** Scala Systems Analyst

## Summary

Total Validation Rules Extracted: 15

### Validation Categories
- Input Validation Rules: 6
- Format Validation Rules: 3
- Business Constraint Rules: 3
- Length/Boundary Rules: 2
- Cross-Field Validation Rules: 1

---

## Category: Input Validation (Required Fields)

### Rule VR-001: Bank ID Required Validation

**Field/Entity:** bankId

**Validation Type:** Required Field Validation

**Rule Description:**
Bank ID must be provided when creating a new Bank entity. This field serves as the primary identifier for the bank in the system.

**Validation Logic:**

- **Condition:** When a POST request is made to `/api/banks` to create a new Bank entity
- **Check:** Validate that bankId field is present and not null/empty in the request body
- **Valid Criteria:** 
  - bankId is present in the request
  - bankId is not null
  - bankId is not an empty string
- **Invalid Criteria:**
  - bankId is missing from the request
  - bankId is null
  - bankId is an empty string
- **Action on Success:** Proceed with bank creation process
- **Action on Failure:** Return error response with 400 status code

**Error Handling:**

- **Error Message:** `Bank ID is required and cannot be empty`
- **Error Code:** `BANK-VAL-001`
- **HTTP Status Code:** `400 Bad Request`

**Implementation:**

- **Location:** `BankService`
- **Method/Function:** `createBank()`
- **Endpoint:** `POST /api/banks`

**Related Entities:**
- Bank (bankId field)

**User Story Context:**
This validation ensures that every Bank entity has a unique identifier as specified in the acceptance criteria: "System must allow creating new Bank entities with identification details."

**Dependencies:**
- None (standalone validation)

---

### Rule VR-002: Bank Code Required Validation

**Field/Entity:** bankCode

**Validation Type:** Required Field Validation

**Rule Description:**
Bank Code must be provided when creating a new Bank entity. This code is used for internal bank identification and routing purposes.

**Validation Logic:**

- **Condition:** When a POST request is made to `/api/banks` to create a new Bank entity
- **Check:** Validate that bankCode field is present and not null/empty in the request body
- **Valid Criteria:** 
  - bankCode is present in the request
  - bankCode is not null
  - bankCode is not an empty string
- **Invalid Criteria:**
  - bankCode is missing from the request
  - bankCode is null
  - bankCode is an empty string
- **Action on Success:** Proceed with bank creation process
- **Action on Failure:** Return error response with 400 status code

**Error Handling:**

- **Error Message:** `Bank Code is required and cannot be empty`
- **Error Code:** `BANK-VAL-002`
- **HTTP Status Code:** `400 Bad Request`

**Implementation:**

- **Location:** `BankService`
- **Method/Function:** `createBank()`
- **Endpoint:** `POST /api/banks`

**Related Entities:**
- Bank (bankCode field)

**User Story Context:**
This validation ensures that every Bank entity has proper identification as specified in the technical context: "Bank identification (ID, code, name)."

**Dependencies:**
- None (standalone validation)

---

### Rule VR-003: Bank Name Required Validation

**Field/Entity:** bankName

**Validation Type:** Required Field Validation

**Rule Description:**
Bank Name must be provided when creating a new Bank entity. This is the display name of the bank used in user interfaces and reports.

**Validation Logic:**

- **Condition:** When a POST request is made to `/api/banks` to create a new Bank entity
- **Check:** Validate that bankName field is present and not null/empty in the request body
- **Valid Criteria:** 
  - bankName is present in the request
  - bankName is not null
  - bankName is not an empty string
- **Invalid Criteria:**
  - bankName is missing from the request
  - bankName is null
  - bankName is an empty string
- **Action on Success:** Proceed with bank creation process
- **Action on Failure:** Return error response with 400 status code

**Error Handling:**

- **Error Message:** `Bank Name is required and cannot be empty`
- **Error Code:** `BANK-VAL-003`
- **HTTP Status Code:** `400 Bad Request`

**Implementation:**

- **Location:** `BankService`
- **Method/Function:** `createBank()`
- **Endpoint:** `POST /api/banks`

**Related Entities:**
- Bank (bankName field)

**User Story Context:**
This validation ensures that every Bank entity has a human-readable name as specified in the technical context: "Bank identification (ID, code, name)."

**Dependencies:**
- None (standalone validation)

---

### Rule VR-004: Branding Information Required Validation

**Field/Entity:** branding (logo, colors)

**Validation Type:** Required Field Validation

**Rule Description:**
Branding information including logo and colors must be provided when creating a new Bank entity. This ensures consistent visual identity for the bank.

**Validation Logic:**

- **Condition:** When a POST request is made to `/api/banks` to create a new Bank entity
- **Check:** Validate that branding object is present with logo and colors fields
- **Valid Criteria:** 
  - branding object is present in the request
  - branding.logo is not null/empty
  - branding.colors is not null/empty
- **Invalid Criteria:**
  - branding object is missing from the request
  - branding.logo is null or empty
  - branding.colors is null or empty
- **Action on Success:** Proceed with bank creation process
- **Action on Failure:** Return error response with 400 status code

**Error Handling:**

- **Error Message:** `Branding information (logo and colors) is required`
- **Error Code:** `BANK-VAL-004`
- **HTTP Status Code:** `400 Bad Request`

**Implementation:**

- **Location:** `BankService`
- **Method/Function:** `createBank()`
- **Endpoint:** `POST /api/banks`

**Related Entities:**
- Bank (branding field)
- BankBranding (logo, colors fields)

**User Story Context:**
This validation ensures that branding elements are provided as specified in the acceptance criteria: "System must allow managing (updating) Bank branding elements" and data validations: "Branding information must be provided."

**Dependencies:**
- None (standalone validation)

---

### Rule VR-005: Operational Parameters Required Validation

**Field/Entity:** operationalParams (businessHours, limits, currencies)

**Validation Type:** Required Field Validation

**Rule Description:**
Operational parameters including business hours, limits, and currencies must be provided when creating a new Bank entity.

**Validation Logic:**

- **Condition:** When a POST request is made to `/api/banks` to create a new Bank entity
- **Check:** Validate that operationalParams object is present with businessHours, limits, and currencies fields
- **Valid Criteria:** 
  - operationalParams object is present in the request
  - operationalParams.businessHours is not null/empty
  - operationalParams.limits is not null
  - operationalParams.currencies is not null and not empty array
- **Invalid Criteria:**
  - operationalParams object is missing from the request
  - operationalParams.businessHours is null or empty
  - operationalParams.limits is null
  - operationalParams.currencies is null or empty array
- **Action on Success:** Proceed with bank creation process
- **Action on Failure:** Return error response with 400 status code

**Error Handling:**

- **Error Message:** `Operational parameters (businessHours, limits, currencies) are required`
- **Error Code:** `BANK-VAL-005`
- **HTTP Status Code:** `400 Bad Request`

**Implementation:**

- **Location:** `BankService`
- **Method/Function:** `createBank()`
- **Endpoint:** `POST /api/banks`

**Related Entities:**
- Bank (operationalParams field)
- BankOperationalParams (businessHours, limits, currencies fields)

**User Story Context:**
This validation ensures that operational parameters are provided as specified in the acceptance criteria: "System must allow managing (updating) Bank operational parameters" and data validations: "Operational parameters must be provided."

**Dependencies:**
- None (standalone validation)

---

### Rule VR-006: Bank ID Path Parameter Required for Update

**Field/Entity:** bankId (path parameter)

**Validation Type:** Required Field Validation

**Rule Description:**
Bank ID must be provided as a path parameter when updating an existing Bank entity.

**Validation Logic:**

- **Condition:** When a PUT request is made to `/api/banks/{bankId}` to update a Bank entity
- **Check:** Validate that bankId path parameter is present and not empty
- **Valid Criteria:** 
  - bankId path parameter is present
  - bankId is not an empty string
- **Invalid Criteria:**
  - bankId path parameter is missing
  - bankId is an empty string
- **Action on Success:** Proceed with bank lookup and update process
- **Action on Failure:** Return error response with 400 status code

**Error Handling:**

- **Error Message:** `Bank ID path parameter is required`
- **Error Code:** `BANK-VAL-006`
- **HTTP Status Code:** `400 Bad Request`

**Implementation:**

- **Location:** `BankService`
- **Method/Function:** `updateBank()`
- **Endpoint:** `PUT /api/banks/{bankId}`

**Related Entities:**
- Bank (bankId field)

**User Story Context:**
This validation ensures that the correct bank is identified for update operations as specified in the acceptance criteria: "System must allow managing (updating) Bank identification information."

**Dependencies:**
- None (standalone validation)

---

## Category: Business Constraint Validation

### Rule VR-007: Bank ID Uniqueness Validation

**Field/Entity:** bankId

**Validation Type:** Business Constraint Validation (Uniqueness)

**Rule Description:**
Each Bank entity must have a unique Bank ID. No two banks can share the same identifier in the system.

**Validation Logic:**

- **Condition:** When a POST request is made to `/api/banks` to create a new Bank entity
- **Check:** Query the database to verify that no existing bank has the same bankId
- **Valid Criteria:** 
  - No existing bank record found with the provided bankId
- **Invalid Criteria:**
  - An existing bank record is found with the same bankId
- **Action on Success:** Proceed with bank creation process
- **Action on Failure:** Return error response with 409 status code

**Error Handling:**

- **Error Message:** `Bank with ID '{bankId}' already exists`
- **Error Code:** `BANK-VAL-007`
- **HTTP Status Code:** `409 Conflict`

**Implementation:**

- **Location:** `BankService`
- **Method/Function:** `createBank()`
- **Repository Method:** `BankRepository.findByBankId()`
- **Endpoint:** `POST /api/banks`

**Related Entities:**
- Bank (bankId field)

**User Story Context:**
This validation enforces the business rule: "Unique Identification: Each Bank entity must have unique identification" as specified in the Business Rules section.

**Dependencies:**
- VR-001 (Bank ID Required Validation) - bankId must be present before uniqueness check

---

### Rule VR-008: Bank Code Uniqueness Validation

**Field/Entity:** bankCode

**Validation Type:** Business Constraint Validation (Uniqueness)

**Rule Description:**
Each Bank entity must have a unique Bank Code. No two banks can share the same code in the system.

**Validation Logic:**

- **Condition:** When a POST request is made to `/api/banks` to create a new Bank entity
- **Check:** Query the database to verify that no existing bank has the same bankCode
- **Valid Criteria:** 
  - No existing bank record found with the provided bankCode
- **Invalid Criteria:**
  - An existing bank record is found with the same bankCode
- **Action on Success:** Proceed with bank creation process
- **Action on Failure:** Return error response with 409 status code

**Error Handling:**

- **Error Message:** `Bank with Code '{bankCode}' already exists`
- **Error Code:** `BANK-VAL-008`
- **HTTP Status Code:** `409 Conflict`

**Implementation:**

- **Location:** `BankService`
- **Method/Function:** `createBank()`
- **Repository Method:** `BankRepository.findByBankCode()`
- **Endpoint:** `POST /api/banks`

**Related Entities:**
- Bank (bankCode field)

**User Story Context:**
This validation enforces the business rule: "Unique Identification: Each Bank entity must have unique identification" as specified in the Business Rules section. Bank code is part of the identification.

**Dependencies:**
- VR-002 (Bank Code Required Validation) - bankCode must be present before uniqueness check

---

### Rule VR-009: Bank Existence Validation for Update

**Field/Entity:** bankId

**Validation Type:** Business Constraint Validation (Entity Existence)

**Rule Description:**
Only existing Bank entities can be updated. The system must verify that a bank with the given ID exists before allowing updates.

**Validation Logic:**

- **Condition:** When a PUT request is made to `/api/banks/{bankId}` to update a Bank entity
- **Check:** Query the database to verify that a bank with the given bankId exists
- **Valid Criteria:** 
  - A bank record is found with the provided bankId
- **Invalid Criteria:**
  - No bank record is found with the provided bankId
- **Action on Success:** Proceed with bank update process
- **Action on Failure:** Return error response with 404 status code

**Error Handling:**

- **Error Message:** `Bank with ID '{bankId}' not found`
- **Error Code:** `BANK-VAL-009`
- **HTTP Status Code:** `404 Not Found`

**Implementation:**

- **Location:** `BankService`
- **Method/Function:** `updateBank()`
- **Repository Method:** `BankRepository.findByBankId()`
- **Endpoint:** `PUT /api/banks/{bankId}`

**Related Entities:**
- Bank (bankId field)

**User Story Context:**
This validation enforces the business rule: "Valid Updates: Only existing Bank entities can be managed/updated" as specified in the Business Rules section.

**Dependencies:**
- VR-006 (Bank ID Path Parameter Required) - bankId must be present before existence check

---

## Category: Format Validation

### Rule VR-010: Bank ID Format Validation

**Field/Entity:** bankId

**Validation Type:** Format Validation (String Pattern)

**Rule Description:**
Bank ID must contain only alphanumeric characters, hyphens, and underscores. No special characters or spaces are allowed.

**Validation Logic:**

- **Condition:** When bankId is provided in any API request (create or update)
- **Check:** Validate that bankId matches the pattern `^[A-Za-z0-9_-]+$`
- **Valid Criteria:** 
  - Contains only: A-Z, a-z, 0-9, hyphen (-), underscore (_)
  - No spaces or special characters
- **Invalid Criteria:**
  - Contains special characters other than - and _
  - Contains spaces
  - Contains unicode or non-ASCII characters
- **Action on Success:** Proceed with processing
- **Action on Failure:** Return error response with 400 status code

**Error Handling:**

- **Error Message:** `Bank ID must contain only alphanumeric characters, hyphens, and underscores`
- **Error Code:** `BANK-VAL-010`
- **HTTP Status Code:** `400 Bad Request`

**Implementation:**

- **Location:** `BankService` or `BankValidator`
- **Method/Function:** `validateBankIdFormat()`
- **Endpoint:** `POST /api/banks`, `PUT /api/banks/{bankId}`

**Code Pattern:**
```go
func validateBankIdFormat(bankId string) error {
    regex := regexp.MustCompile(`^[A-Za-z0-9_-]+$`)
    if !regex.MatchString(bankId) {
        return errors.New("Bank ID must contain only alphanumeric characters, hyphens, and underscores")
    }
    return nil
}
```

**Related Entities:**
- Bank (bankId field)

**User Story Context:**
This validation ensures that bank identifiers are properly formatted and can be safely used in URLs and database queries without encoding issues.

**Dependencies:**
- VR-001 (Bank ID Required Validation) - bankId must be present before format check

---

### Rule VR-011: Bank Code Format Validation

**Field/Entity:** bankCode

**Validation Type:** Format Validation (String Pattern)

**Rule Description:**
Bank Code must be uppercase alphanumeric characters only, typically following banking industry standards.

**Validation Logic:**

- **Condition:** When bankCode is provided in a create request
- **Check:** Validate that bankCode matches the pattern `^[A-Z0-9]+$`
- **Valid Criteria:** 
  - Contains only: A-Z (uppercase), 0-9
  - No lowercase letters, spaces, or special characters
- **Invalid Criteria:**
  - Contains lowercase letters
  - Contains special characters
  - Contains spaces
- **Action on Success:** Proceed with processing
- **Action on Failure:** Return error response with 400 status code

**Error Handling:**

- **Error Message:** `Bank Code must contain only uppercase alphanumeric characters`
- **Error Code:** `BANK-VAL-011`
- **HTTP Status Code:** `400 Bad Request`

**Implementation:**

- **Location:** `BankService` or `BankValidator`
- **Method/Function:** `validateBankCodeFormat()`
- **Endpoint:** `POST /api/banks`

**Code Pattern:**
```go
func validateBankCodeFormat(bankCode string) error {
    regex := regexp.MustCompile(`^[A-Z0-9]+$`)
    if !regex.MatchString(bankCode) {
        return errors.New("Bank Code must contain only uppercase alphanumeric characters")
    }
    return nil
}
```

**Related Entities:**
- Bank (bankCode field)

**User Story Context:**
This validation ensures that bank codes follow standard banking conventions and can be used for routing and identification purposes.

**Dependencies:**
- VR-002 (Bank Code Required Validation) - bankCode must be present before format check

---

### Rule VR-012: Currency Code Format Validation

**Field/Entity:** operationalParams.currencies

**Validation Type:** Format Validation (ISO Currency Code)

**Rule Description:**
Currency codes in the currencies array must be valid 3-letter ISO 4217 currency codes.

**Validation Logic:**

- **Condition:** When currencies array is provided in operationalParams
- **Check:** Validate that each currency code matches the pattern `^[A-Z]{3}$` and is a valid ISO 4217 code
- **Valid Criteria:** 
  - Each currency code is exactly 3 uppercase letters
  - Each currency code is a valid ISO 4217 code (e.g., USD, EUR, GBP, JPY)
- **Invalid Criteria:**
  - Currency code is not 3 characters
  - Currency code contains lowercase letters or numbers
  - Currency code is not a valid ISO 4217 code
- **Action on Success:** Proceed with processing
- **Action on Failure:** Return error response with 400 status code

**Error Handling:**

- **Error Message:** `Invalid currency code '{code}'. Must be a valid 3-letter ISO 4217 currency code`
- **Error Code:** `BANK-VAL-012`
- **HTTP Status Code:** `400 Bad Request`

**Implementation:**

- **Location:** `BankService` or `CurrencyValidator`
- **Method/Function:** `validateCurrencyCode()`
- **Endpoint:** `POST /api/banks`, `PUT /api/banks/{bankId}`

**Code Pattern:**
```go
func validateCurrencyCode(currencyCode string) error {
    validCurrencies := []string{"USD", "EUR", "GBP", "JPY", "CHF", "CAD", "AUD", "INR", ...}
    regex := regexp.MustCompile(`^[A-Z]{3}$`)
    if !regex.MatchString(currencyCode) {
        return errors.New("Currency code must be a 3-letter ISO code")
    }
    if !contains(validCurrencies, currencyCode) {
        return errors.New("Invalid ISO 4217 currency code")
    }
    return nil
}
```

**Related Entities:**
- Bank (operationalParams.currencies field)
- BankOperationalParams (currencies field)

**User Story Context:**
This validation ensures that banks are configured with valid currency codes as specified in the technical context: "Operational parameters (business hours, limits, currencies)."

**Dependencies:**
- VR-005 (Operational Parameters Required Validation) - currencies must be present before format check

---

## Category: Length/Boundary Validation

### Rule VR-013: Bank Name Length Validation

**Field/Entity:** bankName

**Validation Type:** Length Validation

**Rule Description:**
Bank Name must be between 1 and 255 characters in length.

**Validation Logic:**

- **Condition:** When bankName is provided in any API request
- **Check:** Validate that bankName length is within acceptable bounds
- **Valid Criteria:** 
  - Length is at least 1 character
  - Length is at most 255 characters
- **Invalid Criteria:**
  - Length is 0 (empty string)
  - Length exceeds 255 characters
- **Action on Success:** Proceed with processing
- **Action on Failure:** Return error response with 400 status code

**Error Handling:**

- **Error Message:** `Bank Name must be between 1 and 255 characters`
- **Error Code:** `BANK-VAL-013`
- **HTTP Status Code:** `400 Bad Request`

**Implementation:**

- **Location:** `BankService` or `BankValidator`
- **Method/Function:** `validateBankNameLength()`
- **Endpoint:** `POST /api/banks`, `PUT /api/banks/{bankId}`

**Code Pattern:**
```go
func validateBankNameLength(bankName string) error {
    if len(bankName) < 1 || len(bankName) > 255 {
        return errors.New("Bank Name must be between 1 and 255 characters")
    }
    return nil
}
```

**Related Entities:**
- Bank (bankName field)

**User Story Context:**
This validation ensures that bank names are of reasonable length for display and storage purposes.

**Dependencies:**
- VR-003 (Bank Name Required Validation) - bankName must be present before length check

---

### Rule VR-014: Currencies Array Size Validation

**Field/Entity:** operationalParams.currencies

**Validation Type:** Boundary Validation (Collection Size)

**Rule Description:**
The currencies array must contain at least one currency code and should not exceed a reasonable maximum (e.g., 50 currencies).

**Validation Logic:**

- **Condition:** When currencies array is provided in operationalParams
- **Check:** Validate that currencies array has at least 1 element and at most 50 elements
- **Valid Criteria:** 
  - Array contains at least 1 currency code
  - Array contains at most 50 currency codes
- **Invalid Criteria:**
  - Array is empty (0 elements)
  - Array exceeds 50 elements
- **Action on Success:** Proceed with processing
- **Action on Failure:** Return error response with 400 status code

**Error Handling:**

- **Error Message:** `Currencies array must contain between 1 and 50 currency codes`
- **Error Code:** `BANK-VAL-014`
- **HTTP Status Code:** `400 Bad Request`

**Implementation:**

- **Location:** `BankService` or `BankValidator`
- **Method/Function:** `validateCurrenciesArraySize()`
- **Endpoint:** `POST /api/banks`, `PUT /api/banks/{bankId}`

**Code Pattern:**
```go
func validateCurrenciesArraySize(currencies []string) error {
    if len(currencies) < 1 {
        return errors.New("At least one currency code is required")
    }
    if len(currencies) > 50 {
        return errors.New("Maximum 50 currency codes allowed")
    }
    return nil
}
```

**Related Entities:**
- Bank (operationalParams.currencies field)
- BankOperationalParams (currencies field)

**User Story Context:**
This validation ensures that banks have at least one supported currency and prevents excessive configuration data.

**Dependencies:**
- VR-005 (Operational Parameters Required Validation) - currencies must be present before size check

---

## Category: Cross-Field Validation

### Rule VR-015: Update Request Field Validation

**Field/Entity:** Multiple fields (bankName, branding, operationalParams)

**Validation Type:** Cross-Field Validation

**Rule Description:**
When updating a Bank entity, at least one field must be provided for update. Empty update requests should be rejected.

**Validation Logic:**

- **Condition:** When a PUT request is made to `/api/banks/{bankId}`
- **Check:** Validate that at least one of bankName, branding, or operationalParams is provided in the request body
- **Valid Criteria:** 
  - At least one of the following is present and non-null:
    - bankName
    - branding (with logo or colors)
    - operationalParams (with businessHours, limits, or currencies)
- **Invalid Criteria:**
  - All fields are null or missing
  - Request body is empty
- **Action on Success:** Proceed with bank update process
- **Action on Failure:** Return error response with 400 status code

**Error Handling:**

- **Error Message:** `At least one field must be provided for update`
- **Error Code:** `BANK-VAL-015`
- **HTTP Status Code:** `400 Bad Request`

**Implementation:**

- **Location:** `BankService`
- **Method/Function:** `validateUpdateRequest()`
- **Endpoint:** `PUT /api/banks/{bankId}`

**Code Pattern:**
```go
func validateUpdateRequest(req UpdateBankRequest) error {
    if req.BankName == "" && req.Branding == nil && req.OperationalParams == nil {
        return errors.New("At least one field must be provided for update")
    }
    return nil
}
```

**Related Entities:**
- Bank (all updatable fields)

**User Story Context:**
This validation ensures meaningful update operations as specified in the acceptance criteria: "System must allow managing (updating) Bank identification information, branding elements, and operational parameters."

**Dependencies:**
- VR-009 (Bank Existence Validation) - bank must exist before validating update fields

---

## Validation Summary Table

| Rule ID | Field | Type | Error Code | HTTP Status |
|---------|-------|------|------------|-------------|
| VR-001 | bankId | Required | BANK-VAL-001 | 400 |
| VR-002 | bankCode | Required | BANK-VAL-002 | 400 |
| VR-003 | bankName | Required | BANK-VAL-003 | 400 |
| VR-004 | branding | Required | BANK-VAL-004 | 400 |
| VR-005 | operationalParams | Required | BANK-VAL-005 | 400 |
| VR-006 | bankId (path) | Required | BANK-VAL-006 | 400 |
| VR-007 | bankId | Uniqueness | BANK-VAL-007 | 409 |
| VR-008 | bankCode | Uniqueness | BANK-VAL-008 | 409 |
| VR-009 | bankId | Existence | BANK-VAL-009 | 404 |
| VR-010 | bankId | Format | BANK-VAL-010 | 400 |
| VR-011 | bankCode | Format | BANK-VAL-011 | 400 |
| VR-012 | currencies | Format | BANK-VAL-012 | 400 |
| VR-013 | bankName | Length | BANK-VAL-013 | 400 |
| VR-014 | currencies | Boundary | BANK-VAL-014 | 400 |
| VR-015 | Multiple | Cross-Field | BANK-VAL-015 | 400 |

---

## Endpoint-Validation Mapping

### POST /api/banks (Create Bank)
- VR-001: Bank ID Required
- VR-002: Bank Code Required
- VR-003: Bank Name Required
- VR-004: Branding Required
- VR-005: Operational Parameters Required
- VR-007: Bank ID Uniqueness
- VR-008: Bank Code Uniqueness
- VR-010: Bank ID Format
- VR-011: Bank Code Format
- VR-012: Currency Code Format
- VR-013: Bank Name Length
- VR-014: Currencies Array Size

### PUT /api/banks/{bankId} (Update Bank)
- VR-006: Bank ID Path Parameter Required
- VR-009: Bank Existence
- VR-010: Bank ID Format
- VR-012: Currency Code Format (if currencies updated)
- VR-013: Bank Name Length (if bankName updated)
- VR-014: Currencies Array Size (if currencies updated)
- VR-015: Update Request Field Validation

---

## Notes

- All validation rules should be executed in order of dependency (required fields first, then format, then business constraints)
- Error messages should be clear and actionable for API consumers
- HTTP status codes follow REST conventions (400 for client errors, 404 for not found, 409 for conflicts)
- Validation should fail fast - return the first validation error encountered
- Consider implementing batch validation to return all errors at once for better UX

## Questions for SME (from User Story)

1. What specific fields are required in identification?
   - Based on analysis: bankId, bankCode, bankName are the core identification fields

2. What specific branding elements need to be configured?
   - Based on analysis: logo (string/URL) and colors (string/object) are the branding elements

3. What specific operational parameters need to be managed?
   - Based on analysis: businessHours (string), limits (object), currencies (array of ISO codes)
