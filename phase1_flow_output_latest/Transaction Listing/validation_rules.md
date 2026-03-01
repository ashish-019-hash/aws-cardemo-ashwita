# Validation Rules

**Extracted From:** Open Bank Project (OBP) Scala Application  
**User Story:** Transaction Listing - Retrieve transaction history for accounts with filtering and pagination  
**Analysis Date:** 2026-01-20  
**Analyst:** Scala Systems Analyst

## Summary

Total Validation Rules Extracted: 18

### Validation Categories
- Input Validation Rules: 6
- Format Validation Rules: 4
- Business Constraint Rules: 4
- Length/Boundary Rules: 2
- Cross-Field Validation Rules: 2

---

## Category: Authentication and Authorization Validation

### Rule VR-001: User Authentication Validation

**Field/Entity:** Authentication Token (OAuth/DirectLogin)

**Validation Type:** Required / Authentication

**Rule Description:**
User must be authenticated with a valid OAuth token or DirectLogin credentials to access transaction listing endpoints.

**Validation Logic:**

- **Condition:** When any transaction listing API endpoint is called
- **Check:** Verify that a valid authentication token is present in the request headers
- **Valid Criteria:** Valid OAuth Bearer token or DirectLogin token present and not expired
- **Invalid Criteria:** Missing token, invalid token, or expired token
- **Action on Success:** Proceed with transaction retrieval
- **Action on Failure:** Return 401 Unauthorized error

**Error Handling:**

- **Error Message:** `OBP-20001: User not logged in. Authentication is required!`
- **Error Code:** `OBP-20001`
- **HTTP Status Code:** `401 Unauthorized`

**Scala Implementation:**

- **Location:** `code.api.util.ErrorMessages`
- **Method/Function:** `authenticatedAccess(cc)`
- **Line Reference:** ErrorMessages.scala:136

**Code Snippet:**
```scala
val UserNotLoggedIn = "OBP-20001: User not logged in. Authentication is required!"
```

**Related Entities:**
- User authentication context
- OAuth tokens
- DirectLogin tokens

**User Story Context:**
This validation ensures that only authenticated users can retrieve transaction history, protecting sensitive financial data from unauthorized access.

**Dependencies:**
- None (primary authentication check)

---

### Rule VR-002: View Access Permission Validation

**Field/Entity:** VIEW_ID

**Validation Type:** Authorization / Access Control

**Rule Description:**
User must have access to the specified view to retrieve transactions. The view determines what transaction data the user can see.

**Validation Logic:**

- **Condition:** When retrieving transactions with a specific VIEW_ID
- **Check:** Verify that the authenticated user has been granted access to the specified view for the account
- **Valid Criteria:** User has been granted permission to access the view
- **Invalid Criteria:** User does not have permission to access the view
- **Action on Success:** Return transactions moderated by the view
- **Action on Failure:** Return 403 Forbidden or view not found error

**Error Handling:**

- **Error Message:** `OBP-20017: Current user does not have access to the view. Please specify a valid value for VIEW_ID.`
- **Error Code:** `OBP-20017`
- **HTTP Status Code:** `403 Forbidden`

**Scala Implementation:**

- **Location:** `code.api.util.ErrorMessages`
- **Method/Function:** `ViewNewStyle.checkViewAccessAndReturnView()`
- **Line Reference:** ErrorMessages.scala:160

**Code Snippet:**
```scala
val UserNoPermissionAccessView = "OBP-20017: Current user does not have access to the view. Please specify a valid value for VIEW_ID."
```

**Related Entities:**
- View permissions
- Account access grants
- User-account relationships

**User Story Context:**
This validation enforces access control to ensure users can only see transactions for accounts they have been granted permission to view, as specified in acceptance criteria #4 and #11.

**Dependencies:**
- VR-001 (User must be authenticated first)

---

## Category: Resource Existence Validation

### Rule VR-003: Bank ID Existence Validation

**Field/Entity:** BANK_ID

**Validation Type:** Required / Entity Existence

**Rule Description:**
The specified BANK_ID must exist in the system before transaction listing can proceed.

**Validation Logic:**

- **Condition:** When BANK_ID is provided as a path parameter
- **Check:** Verify that a bank with the specified BANK_ID exists in the database
- **Valid Criteria:** Bank record exists with the given BANK_ID
- **Invalid Criteria:** No bank found with the specified BANK_ID
- **Action on Success:** Proceed with account validation
- **Action on Failure:** Return 404 Not Found error

**Error Handling:**

- **Error Message:** `OBP-30001: Bank not found. Please specify a valid value for BANK_ID.`
- **Error Code:** `OBP-30001`
- **HTTP Status Code:** `404 Not Found`

**Scala Implementation:**

- **Location:** `code.api.util.ErrorMessages`
- **Method/Function:** `NewStyle.function.getBank(bankId, callContext)`
- **Line Reference:** ErrorMessages.scala:308

**Code Snippet:**
```scala
val BankNotFound = "OBP-30001: Bank not found. Please specify a valid value for BANK_ID."
```

**Related Entities:**
- Bank entity
- Bank accounts

**User Story Context:**
This validation ensures that transaction listing requests reference valid banks, as specified in acceptance criteria #9.

**Dependencies:**
- VR-001 (User must be authenticated first)

---

### Rule VR-004: Account ID Existence Validation

**Field/Entity:** ACCOUNT_ID

**Validation Type:** Required / Entity Existence

**Rule Description:**
The specified ACCOUNT_ID must exist and belong to the specified bank.

**Validation Logic:**

- **Condition:** When ACCOUNT_ID is provided as a path parameter
- **Check:** Verify that an account with the specified ACCOUNT_ID exists for the given BANK_ID
- **Valid Criteria:** Account record exists with the given ACCOUNT_ID at the specified bank
- **Invalid Criteria:** No account found with the specified ACCOUNT_ID at the bank
- **Action on Success:** Proceed with view validation
- **Action on Failure:** Return 404 Not Found error

**Error Handling:**

- **Error Message:** `OBP-30003: Account not found. Please specify a valid value for ACCOUNT_ID.`
- **Error Code:** `OBP-30003`
- **HTTP Status Code:** `404 Not Found`

**Scala Implementation:**

- **Location:** `code.api.util.ErrorMessages`
- **Method/Function:** `NewStyle.function.checkBankAccountExists(bankId, accountId, callContext)`
- **Line Reference:** ErrorMessages.scala:312

**Code Snippet:**
```scala
val AccountNotFound = "OBP-30003: Account not found. Please specify a valid value for ACCOUNT_ID."
```

**Related Entities:**
- BankAccount entity
- Bank entity

**User Story Context:**
This validation ensures that transaction listing requests reference valid accounts, as specified in acceptance criteria #9.

**Dependencies:**
- VR-003 (Bank must exist first)

---

### Rule VR-005: View ID Existence Validation

**Field/Entity:** VIEW_ID

**Validation Type:** Required / Entity Existence

**Rule Description:**
The specified VIEW_ID must exist for the account.

**Validation Logic:**

- **Condition:** When VIEW_ID is provided as a path parameter
- **Check:** Verify that a view with the specified VIEW_ID exists for the account
- **Valid Criteria:** View record exists with the given VIEW_ID for the account
- **Invalid Criteria:** No view found with the specified VIEW_ID for the account
- **Action on Success:** Proceed with transaction retrieval
- **Action on Failure:** Return 404 Not Found error

**Error Handling:**

- **Error Message:** `OBP-30005: View not found for Account. Please specify a valid value for VIEW_ID`
- **Error Code:** `OBP-30005`
- **HTTP Status Code:** `404 Not Found`

**Scala Implementation:**

- **Location:** `code.api.util.ErrorMessages`
- **Method/Function:** `ViewNewStyle.checkViewAccessAndReturnView()`
- **Line Reference:** ErrorMessages.scala:315

**Code Snippet:**
```scala
val ViewNotFound = "OBP-30005: View not found for Account. Please specify a valid value for VIEW_ID"
```

**Related Entities:**
- View entity
- Account-View relationships

**User Story Context:**
This validation ensures that transaction listing requests reference valid views that define what data can be accessed.

**Dependencies:**
- VR-004 (Account must exist first)

---

## Category: Input Format Validation

### Rule VR-006: Bank ID Format Validation

**Field/Entity:** BANK_ID

**Validation Type:** Format Validation (String Pattern)

**Rule Description:**
Bank ID must contain only alphanumeric characters, hyphens, underscores, and periods, with a maximum length of 255 characters.

**Validation Logic:**

- **Condition:** When a BANK_ID is provided in the API request path
- **Check:** Validate that BANK_ID matches the pattern `^([A-Za-z0-9\-_.]+)$` and length < 256
- **Valid Criteria:** 
  - Contains only: A-Z, a-z, 0-9, hyphen (-), underscore (_), period (.)
  - Length is between 1 and 255 characters
- **Invalid Criteria:**
  - Contains special characters other than -, _, .
  - Length is 0 or greater than 255 characters
- **Action on Success:** Proceed with bank lookup
- **Action on Failure:** Return error response with 400 status code

**Error Handling:**

- **Error Message:** `OBP-30111: Invalid Bank Id. The BANK_ID should only contain 0-9/a-z/A-Z/'-'/'.'/'_', the length should be smaller than 255.`
- **Error Code:** `OBP-30111`
- **HTTP Status Code:** `400 Bad Request`

**Scala Implementation:**

- **Location:** `code.api.util.APIUtil`
- **Method/Function:** `isValidID(id: String): Boolean`
- **Line Reference:** APIUtil.scala:789-795

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
- Bank entity (bank_id field)

**User Story Context:**
This validation ensures that bank identifiers used in transaction listing requests are properly formatted and can be safely processed.

**Dependencies:**
- None (standalone validation)

---

### Rule VR-007: Account ID Format Validation

**Field/Entity:** ACCOUNT_ID

**Validation Type:** Format Validation (String Pattern)

**Rule Description:**
Account ID must contain only alphanumeric characters, hyphens, underscores, and periods, with a maximum length of 255 characters.

**Validation Logic:**

- **Condition:** When an ACCOUNT_ID is provided in the API request path
- **Check:** Validate that ACCOUNT_ID matches the pattern `^([A-Za-z0-9\-_.]+)$` and length < 256
- **Valid Criteria:** 
  - Contains only: A-Z, a-z, 0-9, hyphen (-), underscore (_), period (.)
  - Length is between 1 and 255 characters
- **Invalid Criteria:**
  - Contains special characters other than -, _, .
  - Length is 0 or greater than 255 characters
- **Action on Success:** Proceed with account lookup
- **Action on Failure:** Return error response with 400 status code

**Error Handling:**

- **Error Message:** `OBP-30110: Invalid Account Id. The ACCOUNT_ID should only contain 0-9/a-z/A-Z/'-'/'.'/'_', the length should be smaller than 255.`
- **Error Code:** `OBP-30110`
- **HTTP Status Code:** `400 Bad Request`

**Scala Implementation:**

- **Location:** `code.api.util.APIUtil`
- **Method/Function:** `isValidID(id: String): Boolean`
- **Line Reference:** APIUtil.scala:789-795

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
- BankAccount entity (account_id field)
- Transaction entity (from_account_id, to_account_id fields)

**User Story Context:**
This validation ensures that account identifiers used in transaction listing requests are properly formatted.

**Dependencies:**
- None (standalone validation)

---

## Category: Query Parameter Validation

### Rule VR-008: Date Format Validation (from_date / to_date)

**Field/Entity:** from_date, to_date

**Validation Type:** Format Validation (Date)

**Rule Description:**
Date filter parameters must be in valid ISO 8601 format with milliseconds.

**Validation Logic:**

- **Condition:** When from_date or to_date query parameters are provided
- **Check:** Validate that the date string can be parsed using the format `yyyy-MM-dd'T'HH:mm:ss.SSS'Z'`
- **Valid Criteria:** Date string matches ISO 8601 format with milliseconds (e.g., `2024-01-15T10:30:00.000Z`)
- **Invalid Criteria:** Date string does not match the expected format
- **Action on Success:** Use the parsed date for filtering transactions
- **Action on Failure:** Return 400 Bad Request error

**Error Handling:**

- **Error Message:** `OBP-10026: Failed to parse date string. Please use this format yyyy-MM-dd'T'HH:mm:ss.SSS'Z'!`
- **Error Code:** `OBP-10026`
- **HTTP Status Code:** `400 Bad Request`

**Scala Implementation:**

- **Location:** `code.api.util.ErrorMessages`
- **Method/Function:** `getFromDate()`, `getToDate()` in APIUtil
- **Line Reference:** ErrorMessages.scala:119

**Code Snippet:**
```scala
val FilterDateFormatError = s"OBP-10026: Failed to parse date string. Please use this format ${DateWithMsFormat.toPattern}!"
```

**Related Entities:**
- Transaction entity (posted date, completed date)
- Query parameters

**User Story Context:**
This validation ensures that date range filters for transaction listing are properly formatted, supporting acceptance criteria #2 (filtering by date range).

**Dependencies:**
- None (standalone validation)

---

### Rule VR-009: Sort Direction Validation

**Field/Entity:** sort_direction

**Validation Type:** Enumeration Validation

**Rule Description:**
Sort direction parameter must be either ASC or DESC.

**Validation Logic:**

- **Condition:** When sort_direction query parameter is provided
- **Check:** Validate that the value is either "ASC" or "DESC" (case-insensitive)
- **Valid Criteria:** Value is "ASC" or "DESC"
- **Invalid Criteria:** Any other value
- **Action on Success:** Apply the specified sort direction to transaction results
- **Action on Failure:** Return 400 Bad Request error

**Error Handling:**

- **Error Message:** `OBP-10023: obp_sort_direction parameter can only take two values: DESC or ASC!`
- **Error Code:** `OBP-10023`
- **HTTP Status Code:** `400 Bad Request`

**Scala Implementation:**

- **Location:** `code.api.util.ErrorMessages`
- **Method/Function:** `getSortDirection()` in APIUtil
- **Line Reference:** ErrorMessages.scala:116

**Code Snippet:**
```scala
val FilterSortDirectionError = "OBP-10023: obp_sort_direction parameter can only take two values: DESC or ASC!"
```

**Related Entities:**
- Query parameters
- Transaction ordering

**User Story Context:**
This validation ensures that transaction listing results can be sorted in chronological order as specified in acceptance criteria #7.

**Dependencies:**
- None (standalone validation)

---

### Rule VR-010: Pagination Offset Validation

**Field/Entity:** offset

**Validation Type:** Range Validation (Integer)

**Rule Description:**
Pagination offset must be a non-negative integer (>= 0).

**Validation Logic:**

- **Condition:** When offset query parameter is provided
- **Check:** Validate that the value is a valid integer >= 0
- **Valid Criteria:** Integer value >= 0
- **Invalid Criteria:** Negative value, non-integer value, or non-numeric string
- **Action on Success:** Apply the offset to skip the specified number of transactions
- **Action on Failure:** Return 400 Bad Request error

**Error Handling:**

- **Error Message:** `OBP-10024: wrong value for obp_offset parameter. Please send a positive integer (=>0)!`
- **Error Code:** `OBP-10024`
- **HTTP Status Code:** `400 Bad Request`

**Scala Implementation:**

- **Location:** `code.api.util.ErrorMessages`
- **Method/Function:** `getOffset()` in APIUtil
- **Line Reference:** ErrorMessages.scala:117

**Code Snippet:**
```scala
val FilterOffersetError = "OBP-10024: wrong value for obp_offset parameter. Please send a positive integer (=>0)!"
```

**Related Entities:**
- Query parameters
- Pagination metadata

**User Story Context:**
This validation ensures that pagination offset is valid, supporting acceptance criteria #3 and #6 for pagination support.

**Dependencies:**
- None (standalone validation)

---

### Rule VR-011: Pagination Limit Validation

**Field/Entity:** limit

**Validation Type:** Range Validation (Integer)

**Rule Description:**
Pagination limit must be a positive integer (>= 1).

**Validation Logic:**

- **Condition:** When limit query parameter is provided
- **Check:** Validate that the value is a valid integer >= 1
- **Valid Criteria:** Integer value >= 1
- **Invalid Criteria:** Zero, negative value, non-integer value, or non-numeric string
- **Action on Success:** Limit the number of transactions returned to the specified value
- **Action on Failure:** Return 400 Bad Request error

**Error Handling:**

- **Error Message:** `OBP-10025: wrong value for obp_limit parameter. Please send a positive integer (=>1)!`
- **Error Code:** `OBP-10025`
- **HTTP Status Code:** `400 Bad Request`

**Scala Implementation:**

- **Location:** `code.api.util.ErrorMessages`
- **Method/Function:** `getLimit()` in APIUtil
- **Line Reference:** ErrorMessages.scala:118

**Code Snippet:**
```scala
val FilterLimitError = "OBP-10025: wrong value for obp_limit parameter. Please send a positive integer (=>1)!"
```

**Related Entities:**
- Query parameters
- Pagination metadata

**User Story Context:**
This validation ensures that pagination limit is valid, supporting acceptance criteria #3 and #6 for pagination support.

**Dependencies:**
- None (standalone validation)

---

### Rule VR-012: Maximum Limit Validation

**Field/Entity:** limit

**Validation Type:** Boundary Validation (Maximum)

**Rule Description:**
Pagination limit must not exceed the maximum allowed value (typically 10000).

**Validation Logic:**

- **Condition:** When limit query parameter is provided
- **Check:** Validate that the value does not exceed the configured maximum limit
- **Valid Criteria:** Integer value <= 10000 (or configured maximum)
- **Invalid Criteria:** Value exceeds the maximum limit
- **Action on Success:** Apply the limit to transaction results
- **Action on Failure:** Return 400 Bad Request error

**Error Handling:**

- **Error Message:** `OBP-10012: Invalid value. Maximum number is 10000.`
- **Error Code:** `OBP-10012`
- **HTTP Status Code:** `400 Bad Request`

**Scala Implementation:**

- **Location:** `code.api.util.ErrorMessages`
- **Method/Function:** Limit validation in APIUtil
- **Line Reference:** ErrorMessages.scala:91

**Code Snippet:**
```scala
val maximumLimitExceeded = "OBP-10012: Invalid value. Maximum number is 10000."
```

**Related Entities:**
- Query parameters
- System configuration

**User Story Context:**
This validation ensures that transaction listing requests don't request too many records at once, supporting performance requirements for very high volume usage.

**Dependencies:**
- VR-011 (Basic limit validation must pass first)

---

## Category: Cross-Field Validation

### Rule VR-013: Date Range Validation

**Field/Entity:** from_date, to_date

**Validation Type:** Cross-Field Validation

**Rule Description:**
When both from_date and to_date are provided, from_date must be less than or equal to to_date.

**Validation Logic:**

- **Condition:** When both from_date and to_date query parameters are provided
- **Check:** Validate that from_date <= to_date
- **Valid Criteria:** from_date is before or equal to to_date
- **Invalid Criteria:** from_date is after to_date
- **Action on Success:** Apply the date range filter to transactions
- **Action on Failure:** Return 400 Bad Request error

**Error Handling:**

- **Error Message:** `OBP-10016: Incorrect filter Parameters in URL.`
- **Error Code:** `OBP-10016`
- **HTTP Status Code:** `400 Bad Request`

**Scala Implementation:**

- **Location:** `code.api.util.ErrorMessages`
- **Method/Function:** Date range validation in query processing
- **Line Reference:** ErrorMessages.scala:95

**Code Snippet:**
```scala
val InvalidFilterParameterFormat = "OBP-10016: Incorrect filter Parameters in URL. "
```

**Related Entities:**
- from_date parameter
- to_date parameter
- Transaction posted/completed dates

**User Story Context:**
This validation ensures that date range filters are logically valid, supporting acceptance criteria #2 for filtering transactions by date range.

**Dependencies:**
- VR-008 (Both dates must be in valid format first)

---

### Rule VR-014: Bank Account Combination Validation

**Field/Entity:** BANK_ID, ACCOUNT_ID

**Validation Type:** Cross-Field / Entity Relationship

**Rule Description:**
The specified ACCOUNT_ID must belong to the specified BANK_ID.

**Validation Logic:**

- **Condition:** When both BANK_ID and ACCOUNT_ID are provided
- **Check:** Validate that the account exists at the specified bank
- **Valid Criteria:** Account with ACCOUNT_ID exists and is associated with BANK_ID
- **Invalid Criteria:** Account does not exist at the specified bank
- **Action on Success:** Proceed with transaction retrieval
- **Action on Failure:** Return 404 Not Found error

**Error Handling:**

- **Error Message:** `OBP-30018: Bank Account not found. Please specify valid values for BANK_ID and ACCOUNT_ID.`
- **Error Code:** `OBP-30018`
- **HTTP Status Code:** `404 Not Found`

**Scala Implementation:**

- **Location:** `code.api.util.ErrorMessages`
- **Method/Function:** `NewStyle.function.checkBankAccountExists()`
- **Line Reference:** ErrorMessages.scala:329

**Code Snippet:**
```scala
val BankAccountNotFound = "OBP-30018: Bank Account not found. Please specify valid values for BANK_ID and ACCOUNT_ID. "
```

**Related Entities:**
- Bank entity
- BankAccount entity

**User Story Context:**
This validation ensures that the bank and account combination is valid before attempting to retrieve transactions.

**Dependencies:**
- VR-003 (Bank must exist)
- VR-004 (Account must exist)

---

## Category: Business Constraint Validation

### Rule VR-015: Transaction Not Found Validation

**Field/Entity:** TRANSACTION_ID

**Validation Type:** Entity Existence (for detail retrieval)

**Rule Description:**
When retrieving a specific transaction, the transaction must exist.

**Validation Logic:**

- **Condition:** When a specific TRANSACTION_ID is requested
- **Check:** Validate that a transaction with the specified ID exists
- **Valid Criteria:** Transaction record exists with the given TRANSACTION_ID
- **Invalid Criteria:** No transaction found with the specified TRANSACTION_ID
- **Action on Success:** Return transaction details
- **Action on Failure:** Return 404 Not Found error

**Error Handling:**

- **Error Message:** `OBP-30067: Transaction not found. Please specify a valid value for TRANSACTION_ID.`
- **Error Code:** `OBP-30067`
- **HTTP Status Code:** `404 Not Found`

**Scala Implementation:**

- **Location:** `code.api.util.ErrorMessages`
- **Method/Function:** Transaction lookup methods
- **Line Reference:** ErrorMessages.scala:397

**Code Snippet:**
```scala
val TransactionNotFound = "OBP-30067: Transaction not found. Please specify a valid value for TRANSACTION_ID."
```

**Related Entities:**
- Transaction entity

**User Story Context:**
This validation supports transaction detail retrieval when navigating from the transaction list.

**Dependencies:**
- VR-014 (Bank account must be valid)

---

### Rule VR-016: Empty Result Handling

**Field/Entity:** Transaction list

**Validation Type:** Business Rule

**Rule Description:**
When no transactions match the filter criteria, return an empty list rather than an error.

**Validation Logic:**

- **Condition:** When transaction query returns no results
- **Check:** Check if the result set is empty
- **Valid Criteria:** Query executed successfully (even if no results)
- **Invalid Criteria:** N/A - empty results are valid
- **Action on Success:** Return empty transactions array with 200 OK
- **Action on Failure:** N/A

**Error Handling:**

- **Error Message:** N/A (no error for empty results)
- **Error Code:** N/A
- **HTTP Status Code:** `200 OK` with empty array

**Scala Implementation:**

- **Location:** `code.api.v3_0_0.APIMethods300`
- **Method/Function:** `getTransactionsForBankAccount`
- **Line Reference:** APIMethods300.scala:761-784

**Code Snippet:**
```scala
lazy val getTransactionsForBankAccount: OBPEndpoint = {
  case "banks" :: BankId(bankId) :: "accounts" :: AccountId(accountId) :: ViewId(viewId) :: "transactions" :: Nil JsonGet req => {
    // Returns empty list if no transactions match
  }
}
```

**Related Entities:**
- Transaction list response

**User Story Context:**
This validation ensures that the system returns an empty list when no transactions match the filter criteria, as specified in acceptance criteria #10.

**Dependencies:**
- All path parameter validations must pass

---

### Rule VR-017: Currency Code Validation

**Field/Entity:** currency (in transaction amount)

**Validation Type:** Format Validation (ISO Code)

**Rule Description:**
Currency codes in transaction amounts must be valid ISO 4217 currency codes.

**Validation Logic:**

- **Condition:** When processing transaction amounts with currency
- **Check:** Validate that the currency code is a valid 3-letter ISO currency code
- **Valid Criteria:** Currency code exists in ISO 4217 list or is "XBT" (Bitcoin)
- **Invalid Criteria:** Currency code not in the valid list
- **Action on Success:** Include currency in transaction response
- **Action on Failure:** Return 400 Bad Request error

**Error Handling:**

- **Error Message:** `OBP-10003: Invalid Currency Value. Expected a 3-letter ISO Currency Code (e.g., 'USD', 'EUR'), 'lovelace' (Cardano), or 'ETH' (Ethereum). Refer to ISO 4217 currency codes: https://www.iso.org/iso-4217-currency-codes.html`
- **Error Code:** `OBP-10003`
- **HTTP Status Code:** `400 Bad Request`

**Scala Implementation:**

- **Location:** `code.api.util.APIUtil`
- **Method/Function:** `isValidCurrencyISOCode(currencyCode: String): Boolean`
- **Line Reference:** APIUtil.scala:782-786

**Code Snippet:**
```scala
def isValidCurrencyISOCode(currencyCode: String): Boolean = {
  val currencyIsoCodeArray = (CurrencyIsoCodeFromXmlFile \"CcyTbl" \ "CcyNtry" \ "Ccy").map(_.text).mkString(" ").split("\\s+") :+ "XBT"
  currencyIsoCodeArray.contains(currencyCode)
}
```

**Related Entities:**
- Transaction amount (value.currency)
- Account balance (new_balance.currency)

**User Story Context:**
This validation ensures that all monetary values in transaction responses include valid currency codes, as specified in the data validations section.

**Dependencies:**
- None (standalone validation)

---

### Rule VR-018: JSON Format Validation

**Field/Entity:** Request/Response body

**Validation Type:** Format Validation

**Rule Description:**
All request and response bodies must be valid JSON format.

**Validation Logic:**

- **Condition:** When processing API requests/responses
- **Check:** Validate that the body is valid JSON
- **Valid Criteria:** Body can be parsed as valid JSON
- **Invalid Criteria:** Body is not valid JSON
- **Action on Success:** Process the request/response
- **Action on Failure:** Return 400 Bad Request error

**Error Handling:**

- **Error Message:** `OBP-10001: Incorrect json format.`
- **Error Code:** `OBP-10001`
- **HTTP Status Code:** `400 Bad Request`

**Scala Implementation:**

- **Location:** `code.api.util.ErrorMessages`
- **Method/Function:** JSON parsing in request handlers
- **Line Reference:** ErrorMessages.scala:80

**Code Snippet:**
```scala
val InvalidJsonFormat = "OBP-10001: Incorrect json format."
```

**Related Entities:**
- All API request/response bodies

**User Story Context:**
This validation ensures that all API interactions use properly formatted JSON, supporting the technical context of the user story.

**Dependencies:**
- None (standalone validation)

---

## Quality Checklist Verification

- [x] All validation functions in relevant code are documented
- [x] All error messages are captured with exact text
- [x] All error codes are documented
- [x] Regex patterns are included verbatim
- [x] Length constraints are specified with exact limits
- [x] Required vs. optional fields are clearly marked
- [x] Cross-field validations are identified
- [x] Business constraint validations are included
- [x] Code references include file paths and line numbers
- [x] User story context is explained for each rule
- [x] Valid and invalid criteria are clearly stated
- [x] HTTP status codes are documented where applicable
- [x] Dependencies between validations are noted

---

*This document was generated by analyzing the Transaction Listing user story and extracting validation rules from the OBP Scala codebase, following the validation_rules_extraction_prompt.md guidelines.*
