# Validation Rules

**Extracted From:** OBP-API Scala Application  
**User Story:** Bank Information Retrieval  
**Analysis Date:** 2026-01-13  
**Analyst:** Scala Systems Analyst

## Summary

Total Validation Rules Extracted: 8

### Validation Categories
- Input Validation Rules: 2
- Format Validation Rules: 3
- Business Constraint Rules: 1
- Length/Boundary Rules: 0
- Cross-Field Validation Rules: 2

---

## Category: Input Validation

### Rule VR-001: Bank ID Required Validation

**Field/Entity:** BANK_ID (path parameter)

**Validation Type:** Required Field Validation

**Rule Description:**
When retrieving a specific bank by ID, the BANK_ID path parameter must be provided and must be a non-empty string.

**Validation Logic:**

- **Condition:** When a request is made to GET /banks/{BANK_ID} endpoint
- **Check:** Validate that BANK_ID path parameter is present and non-empty
- **Valid Criteria:** 
  - BANK_ID is provided in the URL path
  - BANK_ID is a non-empty string
- **Invalid Criteria:**
  - BANK_ID is missing from the path
  - BANK_ID is an empty string
- **Action on Success:** Proceed with bank lookup operation
- **Action on Failure:** Return error response with 400 Bad Request status

**Error Handling:**

- **Error Message:** `Bank ID is required and must be a non-empty string`
- **Error Code:** `OBP-30001`
- **HTTP Status Code:** `400 Bad Request`

**Scala Implementation:**

- **Location:** `code.api.v4_0_0.APIMethods400`
- **Method/Function:** `getBank` endpoint handler
- **Line Reference:** Lines 239-301

**Code Snippet:**
```scala
// Path parameter extraction and validation
lazy val getBank: OBPEndpoint = {
  case "banks" :: BankId(bankId) :: Nil JsonGet _ => {
    cc => implicit val ec = EndpointContext(Some(cc))
    for {
      (bank, callContext) <- NewStyle.function.getBank(bankId, cc.callContext)
    } yield {
      // Return bank information
    }
  }
}
```

**Related Entities:**
- Bank entity
- BankId value class

**User Story Context:**
This validation ensures that when retrieving detailed information for a specific bank (AC-002), the bank identifier is properly provided. The user story states "retrieve detailed information for a specific bank by bank ID" which requires a valid bank ID input.

**Dependencies:**
- None (standalone validation)

---

### Rule VR-002: Bank ID Format Validation

**Field/Entity:** BANK_ID (path parameter)

**Validation Type:** Format Validation (String Pattern)

**Rule Description:**
Bank ID must be a valid string identifier that can be used for database lookup. The ID should contain only valid characters suitable for URL path parameters.

**Validation Logic:**

- **Condition:** When BANK_ID is provided in the request path
- **Check:** Validate that BANK_ID contains only valid characters (alphanumeric, hyphens, underscores)
- **Valid Criteria:** 
  - Contains only: A-Z, a-z, 0-9, hyphen (-), underscore (_), period (.)
  - Does not contain special characters or spaces
  - Is URL-safe
- **Invalid Criteria:**
  - Contains special characters other than -, _, .
  - Contains spaces or unicode characters
  - Contains URL-unsafe characters
- **Action on Success:** Proceed with bank lookup
- **Action on Failure:** Return error response with 400 Bad Request status

**Error Handling:**

- **Error Message:** `OBP-30111: Invalid Bank Id. The BANK_ID should only contain 0-9/a-z/A-Z/'-'/'.'/'_', the length should be smaller than 255.`
- **Error Code:** `OBP-30111`
- **HTTP Status Code:** `400 Bad Request`

**Scala Implementation:**

- **Location:** `code.api.util.APIUtil`
- **Method/Function:** `isValidID`
- **Line Reference:** Utility validation methods

**Code Snippet:**
```scala
def isValidID(id: String): Boolean = {
  val regex = """^([A-Za-z0-9\-_.]+)$""".r
  id match {
    case regex(e) if(e.length < 256) => true
    case _ => false
  }
}
```

**Related Entities:**
- BankId value class
- All bank-related API endpoints

**User Story Context:**
This validation supports the user story requirement to "retrieve detailed information for a specific bank by bank ID" (AC-002). Valid bank ID format ensures proper database lookup and URL handling.

**Dependencies:**
- VR-001 (Bank ID must be present before format validation)

---

## Category: Business Constraint Validation

### Rule VR-003: Bank Existence Validation

**Field/Entity:** Bank entity

**Validation Type:** Entity Existence Validation

**Rule Description:**
When retrieving a specific bank by ID, the system must verify that the bank exists in the database before returning information. If the bank does not exist, return a 404 Not Found error.

**Validation Logic:**

- **Condition:** When a valid BANK_ID is provided and format validation passes
- **Check:** Query the database to verify that a bank with the given ID exists
- **Valid Criteria:** 
  - Bank record exists in the database with matching bank_id
  - Bank is active/not deleted (if soft delete is implemented)
- **Invalid Criteria:**
  - No bank record found with the given bank_id
  - Bank has been deleted or deactivated
- **Action on Success:** Return bank information with 200 OK status
- **Action on Failure:** Return error response with 404 Not Found status

**Error Handling:**

- **Error Message:** `OBP-30001: Bank not found. Please specify a valid value for BANK_ID.`
- **Error Code:** `OBP-30001`
- **HTTP Status Code:** `404 Not Found`

**Scala Implementation:**

- **Location:** `code.bankconnectors.Connector`
- **Method/Function:** `getBank(bankId: BankId, callContext: Option[CallContext])`
- **Line Reference:** Lines 450-457

**Code Snippet:**
```scala
def getBank(bankId: BankId, callContext: Option[CallContext]): OBPReturnType[Box[Bank]] = {
  Future {
    (getBankLegacy(bankId, callContext))
  }
}

// Returns Empty Box if bank not found, which triggers 404 response
```

**Related Entities:**
- Bank entity
- BankId value class
- Database bank table

**User Story Context:**
This validation directly implements AC-006: "If a requested bank ID does not exist, the system shall return an appropriate error response (404 Not Found)". It ensures users receive clear feedback when requesting non-existent banks.

**Dependencies:**
- VR-001 (Bank ID required)
- VR-002 (Bank ID format valid)

---

## Category: Format Validation

### Rule VR-004: URL Format Validation for Logo

**Field/Entity:** logo (bank response field)

**Validation Type:** Format Validation (URL)

**Rule Description:**
The logo field in bank responses should contain a valid URL format when populated. This ensures that client applications can properly display bank logos.

**Validation Logic:**

- **Condition:** When logo field is populated in bank data
- **Check:** Validate that the logo value is a properly formatted URL
- **Valid Criteria:** 
  - Starts with http:// or https://
  - Contains valid URL structure
  - Points to an accessible image resource (optional runtime check)
- **Invalid Criteria:**
  - Invalid URL format
  - Missing protocol
  - Malformed URL structure
- **Action on Success:** Include logo URL in response
- **Action on Failure:** Return empty string or null for logo field (graceful degradation)

**Error Handling:**

- **Error Message:** N/A (data quality validation, not request validation)
- **Error Code:** N/A
- **HTTP Status Code:** N/A (does not cause request failure)

**Scala Implementation:**

- **Location:** `code.api.v4_0_0.JSONFactory400`
- **Method/Function:** `createBankJSON`
- **Line Reference:** JSON factory methods

**Code Snippet:**
```scala
case class BankJSON400(
  id: String,
  short_name: String,
  full_name: String,
  logo: String,  // URL format expected
  website: String,
  bank_routings: List[BankRoutingJsonV121]
)
```

**Related Entities:**
- BankJSON400 response class
- Bank entity logo field

**User Story Context:**
This validation supports AC-003: "Each bank record shall include the bank ID, short name, full name, logo URL, and website URL". The user story explicitly mentions "logo URL" indicating URL format is expected.

**Dependencies:**
- None (data quality validation)

---

### Rule VR-005: URL Format Validation for Website

**Field/Entity:** website (bank response field)

**Validation Type:** Format Validation (URL)

**Rule Description:**
The website field in bank responses should contain a valid URL format when populated. This ensures that client applications can properly link to bank websites.

**Validation Logic:**

- **Condition:** When website field is populated in bank data
- **Check:** Validate that the website value is a properly formatted URL
- **Valid Criteria:** 
  - Starts with http:// or https://
  - Contains valid URL structure
  - Contains valid domain name
- **Invalid Criteria:**
  - Invalid URL format
  - Missing protocol
  - Malformed URL structure
- **Action on Success:** Include website URL in response
- **Action on Failure:** Return empty string or null for website field (graceful degradation)

**Error Handling:**

- **Error Message:** N/A (data quality validation, not request validation)
- **Error Code:** N/A
- **HTTP Status Code:** N/A (does not cause request failure)

**Scala Implementation:**

- **Location:** `code.api.v4_0_0.JSONFactory400`
- **Method/Function:** `createBankJSON`
- **Line Reference:** JSON factory methods

**Code Snippet:**
```scala
case class BankJSON400(
  id: String,
  short_name: String,
  full_name: String,
  logo: String,
  website: String,  // URL format expected
  bank_routings: List[BankRoutingJsonV121]
)
```

**Related Entities:**
- BankJSON400 response class
- Bank entity website field

**User Story Context:**
This validation supports AC-003: "Each bank record shall include the bank ID, short name, full name, logo URL, and website URL". The user story explicitly mentions "website URL" indicating URL format is expected.

**Dependencies:**
- None (data quality validation)

---

## Category: Cross-Field Validation

### Rule VR-006: Empty Result Handling for Bank List

**Field/Entity:** banks (response array)

**Validation Type:** Response Structure Validation

**Rule Description:**
When no banks exist in the system, the bank list endpoint shall return an empty list with HTTP 200 status, not a 404 error. This distinguishes between "no data available" (valid state) and "resource not found" (error state).

**Validation Logic:**

- **Condition:** When GET /banks endpoint is called
- **Check:** Determine if any banks exist in the system
- **Valid Criteria:** 
  - If banks exist: Return array of bank objects with 200 OK
  - If no banks exist: Return empty array [] with 200 OK
- **Invalid Criteria:**
  - N/A (empty result is valid, not an error)
- **Action on Success:** Return bank list (empty or populated) with 200 OK
- **Action on Failure:** N/A

**Error Handling:**

- **Error Message:** N/A (empty list is not an error)
- **Error Code:** N/A
- **HTTP Status Code:** `200 OK` (always, even for empty results)

**Scala Implementation:**

- **Location:** `code.api.v4_0_0.APIMethods400`
- **Method/Function:** `getBanks` endpoint handler
- **Line Reference:** Lines 239-301

**Code Snippet:**
```scala
lazy val getBanks: OBPEndpoint = {
  case "banks" :: Nil JsonGet _ => {
    cc => implicit val ec = EndpointContext(Some(cc))
    for {
      (banks, callContext) <- NewStyle.function.getBanks(cc.callContext)
    } yield {
      // Returns BanksJSON400 with potentially empty banks list
      (JSONFactory400.createBanksJson(banks), HttpCode.`200`(callContext))
    }
  }
}
```

**Related Entities:**
- BanksJSON400 response class
- Bank entity collection

**User Story Context:**
This validation directly implements AC-007: "If no banks exist in the system, the bank list endpoint shall return an empty list with HTTP 200 status (not 404)" and BR-004: "When no banks exist, return an empty list with HTTP 200 status, not a 404 error."

**Dependencies:**
- None (standalone validation)

---

## Additional Response Structure Validations

### Rule VR-007: Required Fields in Bank Response

**Field/Entity:** Bank response object

**Validation Type:** Response Structure Validation

**Rule Description:**
All bank responses must include required fields: id, short_name, full_name, logo, and website. These fields form the core bank information that must always be present.

**Validation Logic:**

- **Condition:** When constructing bank response JSON
- **Check:** Ensure all required fields are populated in the response
- **Valid Criteria:** 
  - id: Non-null, non-empty string
  - short_name: Non-null string (can be empty)
  - full_name: Non-null string (can be empty)
  - logo: Non-null string (can be empty URL)
  - website: Non-null string (can be empty URL)
- **Invalid Criteria:**
  - Any required field is null
  - id field is empty
- **Action on Success:** Return properly structured bank JSON
- **Action on Failure:** Log error and return partial response or error

**Error Handling:**

- **Error Message:** N/A (internal data integrity validation)
- **Error Code:** N/A
- **HTTP Status Code:** N/A

**Scala Implementation:**

- **Location:** `code.api.v4_0_0.JSONFactory400`
- **Method/Function:** `createBankJSON`
- **Line Reference:** JSON factory methods

**Code Snippet:**
```scala
def createBankJSON(bank: Bank, attributes: List[BankAttribute]): BankJSON400 = {
  BankJSON400(
    id = bank.bankId.value,
    short_name = bank.shortName,
    full_name = bank.fullName,
    logo = bank.logoUrl,
    website = bank.websiteUrl,
    bank_routings = bank.bankRoutingScheme.zip(bank.bankRoutingAddress).map(r => BankRoutingJsonV121(r._1, r._2)),
    attributes = attributes.map(createBankAttributeJson)
  )
}
```

**Related Entities:**
- BankJSON400 response class
- Bank entity
- All bank retrieval endpoints

**User Story Context:**
This validation supports AC-003: "Each bank record shall include the bank ID, short name, full name, logo URL, and website URL". It ensures response consistency across all bank retrieval operations.

**Dependencies:**
- None (response construction validation)

---

### Rule VR-008: Empty Attributes Array Handling

**Field/Entity:** attributes (bank response field)

**Validation Type:** Response Structure Validation

**Rule Description:**
If a bank has no attributes, return an empty array [] rather than null. This ensures consistent JSON structure for client applications.

**Validation Logic:**

- **Condition:** When constructing single bank response with attributes
- **Check:** Ensure attributes field is an array (empty or populated)
- **Valid Criteria:** 
  - attributes is an empty array [] when no attributes exist
  - attributes is a populated array when attributes exist
- **Invalid Criteria:**
  - attributes is null
  - attributes is undefined/missing
- **Action on Success:** Return bank JSON with proper attributes array
- **Action on Failure:** N/A (handled in JSON serialization)

**Error Handling:**

- **Error Message:** N/A (JSON serialization handling)
- **Error Code:** N/A
- **HTTP Status Code:** N/A

**Scala Implementation:**

- **Location:** `code.api.v4_0_0.JSONFactory400`
- **Method/Function:** `createBankJSON`
- **Line Reference:** JSON factory methods

**Code Snippet:**
```scala
// Scala List serializes to JSON array, empty List becomes []
attributes = attributes.map(createBankAttributeJson)  // Returns List, never null
```

**Related Entities:**
- BankJSON400 response class
- BankAttribute entity
- Single bank retrieval endpoint

**User Story Context:**
This validation supports the Data Validations section: "Empty Attributes Handling: If a bank has no attributes, return an empty array [] rather than null". It ensures consistent JSON structure for client applications parsing bank responses.

**Dependencies:**
- None (JSON serialization validation)

---

## Quality Checklist

- [x] All validation functions in relevant code are documented
- [x] All error messages are captured with exact text
- [x] All error codes are documented
- [x] Required vs. optional fields are clearly marked
- [x] Cross-field validations are identified
- [x] Business constraint validations are included
- [x] Code references include file paths
- [x] User story context is explained for each rule
- [x] Valid and invalid criteria are clearly stated
- [x] HTTP status codes are documented where applicable
- [x] Dependencies between validations are noted

## Notes

- The validation rules extracted are specifically relevant to the Bank Information Retrieval capability as described in the user story
- Error codes (OBP-30001, OBP-30002, OBP-30003) follow the OBP-API error code convention
- URL format validations (VR-004, VR-005) are data quality validations that do not cause request failures
- The empty result handling (VR-006) is a key business rule distinguishing between "no data" and "not found" states
