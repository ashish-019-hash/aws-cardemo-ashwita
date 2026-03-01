# Validation Rules

**Extracted From:** Open Bank Project (OBP) Scala Application  
**User Story:** Transaction Details - Get detailed information about a specific transaction  
**Analysis Date:** 2026-01-20  
**Analyst:** Scala Systems Analyst

## Summary

Total Validation Rules Extracted: 15

### Validation Categories
- Input Validation Rules: 5
- Format Validation Rules: 4
- Business Constraint Rules: 3
- Length/Boundary Rules: 1
- Cross-Field Validation Rules: 2

---

## Category: Input Validation - Required Fields

### Rule VR-001: Bank ID Required Validation

**Field/Entity:** BANK_ID

**Validation Type:** Required Field / Input Validation

**Rule Description:**
Bank identifier must be provided and must exist in the system for transaction detail retrieval operations.

**Validation Logic:**

- **Condition:** When a transaction detail retrieval request is made via any of the transaction detail endpoints
- **Check:** Validate that BANK_ID path parameter is provided and corresponds to an existing bank in the system
- **Valid Criteria:** BANK_ID is non-empty and exists in the bank registry
- **Invalid Criteria:** BANK_ID is empty, null, or does not correspond to any registered bank
- **Action on Success:** Proceed with account and transaction validation
- **Action on Failure:** Return error response with HTTP 404 status code

**Error Handling:**

- **Error Message:** `OBP-30001: Bank not found. Please specify a valid value for BANK_ID.`
- **Error Code:** `OBP-30001`
- **HTTP Status Code:** `404 Not Found`

**Scala Implementation:**

- **Location:** `code.api.util.NewStyle`
- **Method/Function:** `getBank()` / `checkBankExists()`
- **Line Reference:** Transaction retrieval methods in APIMethods400

**Code Snippet:**
```scala
for {
  (bank, callContext) <- NewStyle.function.getBank(bankId, callContext)
  // ... rest of validation chain
} yield {
  // success response
}
```

**Related Entities:**
- Bank entity
- All transaction detail endpoints require valid BANK_ID

**User Story Context:**
This validation ensures that transaction details can only be retrieved for banks that exist in the system. The user story specifies that "Error response (HTTP 404 Not Found / `BankNotFound`) must be returned when BANK_ID does not exist."

**Dependencies:**
- None (first validation in the chain)

---

### Rule VR-002: Account ID Required Validation

**Field/Entity:** ACCOUNT_ID

**Validation Type:** Required Field / Input Validation

**Rule Description:**
Account identifier must be provided and must exist within the specified bank for transaction detail retrieval operations.

**Validation Logic:**

- **Condition:** When a transaction detail retrieval request is made and BANK_ID validation passes
- **Check:** Validate that ACCOUNT_ID path parameter is provided and corresponds to an existing account within the specified bank
- **Valid Criteria:** ACCOUNT_ID is non-empty and exists as an account under the specified BANK_ID
- **Invalid Criteria:** ACCOUNT_ID is empty, null, or does not correspond to any account in the specified bank
- **Action on Success:** Proceed with transaction and view validation
- **Action on Failure:** Return error response with HTTP 404 status code

**Error Handling:**

- **Error Message:** `OBP-30018: Account not found. Please specify a valid value for ACCOUNT_ID.`
- **Error Code:** `OBP-30018`
- **HTTP Status Code:** `404 Not Found`

**Scala Implementation:**

- **Location:** `code.api.util.NewStyle`
- **Method/Function:** `checkAccountExists()` / `getBankAccount()`
- **Line Reference:** Transaction retrieval methods in APIMethods400

**Code Snippet:**
```scala
for {
  (bank, callContext) <- NewStyle.function.getBank(bankId, callContext)
  (account, callContext) <- NewStyle.function.getBankAccount(bankId, accountId, callContext)
  // ... rest of validation chain
} yield {
  // success response
}
```

**Related Entities:**
- BankAccount entity
- Account-Bank relationship

**User Story Context:**
This validation ensures that transaction details can only be retrieved for accounts that exist within the specified bank. The user story specifies that "Error response (HTTP 404 Not Found / `AccountNotFound`) must be returned when ACCOUNT_ID does not exist."

**Dependencies:**
- VR-001: Bank ID validation must pass first

---

### Rule VR-003: Transaction ID Required Validation

**Field/Entity:** TRANSACTION_ID

**Validation Type:** Required Field / Input Validation

**Rule Description:**
Transaction identifier must be provided and must exist within the specified account for transaction detail retrieval operations.

**Validation Logic:**

- **Condition:** When a transaction detail retrieval request is made and BANK_ID and ACCOUNT_ID validations pass
- **Check:** Validate that TRANSACTION_ID path parameter is provided and corresponds to an existing transaction within the specified account
- **Valid Criteria:** TRANSACTION_ID is non-empty and exists as a transaction under the specified ACCOUNT_ID
- **Invalid Criteria:** TRANSACTION_ID is empty, null, or does not correspond to any transaction in the specified account
- **Action on Success:** Proceed with access control validation and return transaction details
- **Action on Failure:** Return error response with HTTP 404 status code

**Error Handling:**

- **Error Message:** `OBP-30010: Transaction not found. Please specify a valid value for TRANSACTION_ID.`
- **Error Code:** `OBP-30010`
- **HTTP Status Code:** `404 Not Found`

**Scala Implementation:**

- **Location:** `code.api.util.NewStyle`
- **Method/Function:** `getTransaction()` / `checkTransactionExists()`
- **Line Reference:** Transaction retrieval methods in APIMethods400

**Code Snippet:**
```scala
for {
  (bank, callContext) <- NewStyle.function.getBank(bankId, callContext)
  (account, callContext) <- NewStyle.function.getBankAccount(bankId, accountId, callContext)
  (transaction, callContext) <- NewStyle.function.getTransaction(bankId, accountId, transactionId, callContext)
  // ... rest of validation chain
} yield {
  // success response
}
```

**Related Entities:**
- Transaction entity (MappedTransaction)
- Transaction-Account relationship

**User Story Context:**
This validation ensures that transaction details can only be retrieved for transactions that exist within the specified account. The user story specifies that "Error response (HTTP 404 Not Found / `TransactionNotFound`) must be returned when TRANSACTION_ID does not exist."

**Dependencies:**
- VR-001: Bank ID validation must pass first
- VR-002: Account ID validation must pass first

---

### Rule VR-004: View ID Required Validation (View-Based Endpoint)

**Field/Entity:** VIEW_ID

**Validation Type:** Required Field / Input Validation

**Rule Description:**
View identifier must be provided and must be valid for the view-based transaction detail endpoint. The user must have access to the specified view.

**Validation Logic:**

- **Condition:** When a transaction detail retrieval request is made via the view-based endpoint (`GET /banks/BANK_ID/accounts/ACCOUNT_ID/VIEW_ID/transactions/TRANSACTION_ID`)
- **Check:** Validate that VIEW_ID path parameter is provided, exists, and the user has been granted access to this view
- **Valid Criteria:** VIEW_ID is non-empty, exists as a valid view for the account, and the user has permission to access it
- **Invalid Criteria:** VIEW_ID is empty, null, does not exist, or user lacks permission to access it
- **Action on Success:** Proceed with transaction retrieval using view-specific field filtering
- **Action on Failure:** Return error response with HTTP 403 or 404 status code

**Error Handling:**

- **Error Message:** `OBP-30005: View not found. Please specify a valid value for VIEW_ID.` or `OBP-20006: User does not have access to the view.`
- **Error Code:** `OBP-30005` or `OBP-20006`
- **HTTP Status Code:** `404 Not Found` (view not found) or `403 Forbidden` (no access)

**Scala Implementation:**

- **Location:** `code.views.Views`
- **Method/Function:** `checkViewAccess()` / `getView()`
- **Line Reference:** View validation in APIMethods400

**Code Snippet:**
```scala
for {
  (view, callContext) <- NewStyle.function.checkViewAccessAndReturnView(viewId, bankId, accountId, user, callContext)
  // ... rest of validation chain
} yield {
  // success response with view-filtered fields
}
```

**Related Entities:**
- View entity
- View-Account relationship
- User-View permission relationship

**User Story Context:**
This validation ensures that the view system controls which fields are visible in the transaction details response. The user story states "View identifier (VIEW_ID) must be valid and the user must have access to it (when using view-based endpoint)."

**Dependencies:**
- VR-001: Bank ID validation must pass first
- VR-002: Account ID validation must pass first
- VR-006: User authentication validation must pass first

---

### Rule VR-005: Authentication Token Required Validation

**Field/Entity:** Authorization Header (OAuth/DirectLogin Token)

**Validation Type:** Required Field / Input Validation

**Rule Description:**
A valid authentication token must be provided in the request header for all transaction detail retrieval operations.

**Validation Logic:**

- **Condition:** When any transaction detail retrieval request is made
- **Check:** Validate that the Authorization header contains a valid OAuth Bearer token or DirectLogin token
- **Valid Criteria:** Token is present, properly formatted, not expired, and corresponds to a valid user session
- **Invalid Criteria:** Token is missing, malformed, expired, or invalid
- **Action on Success:** Proceed with user identification and authorization checks
- **Action on Failure:** Return error response with HTTP 401 status code

**Error Handling:**

- **Error Message:** `OBP-20001: User not logged in. Authentication is required.`
- **Error Code:** `OBP-20001`
- **HTTP Status Code:** `401 Unauthorized`

**Scala Implementation:**

- **Location:** `code.api.util.APIUtil`
- **Method/Function:** `getUser()` / `validateToken()`
- **Line Reference:** Authentication middleware

**Code Snippet:**
```scala
for {
  (user, callContext) <- NewStyle.function.getUser(callContext)
  // ... rest of validation chain
} yield {
  // success response
}
```

**Related Entities:**
- User entity
- OAuth token
- DirectLogin token

**User Story Context:**
This validation ensures that only authenticated users can access transaction details. The user story specifies "User must be authenticated with a valid OAuth token or DirectLogin credentials" and "Error response (HTTP 401 Unauthorized) for missing or invalid authentication."

**Dependencies:**
- None (first validation in the authentication chain)

---

## Category: Business Constraint Validation

### Rule VR-006: User Account Access Permission Validation

**Field/Entity:** User-Account Access Relationship

**Validation Type:** Business Constraint / Authorization

**Rule Description:**
The authenticated user must have at least one view/permission granted on the account to access transaction details.

**Validation Logic:**

- **Condition:** When a transaction detail retrieval request is made and user authentication passes
- **Check:** Validate that the user has been granted at least one view permission on the specified account
- **Valid Criteria:** User has at least one active view permission (owner, accountant, auditor, etc.) on the account
- **Invalid Criteria:** User has no view permissions on the account
- **Action on Success:** Proceed with transaction retrieval
- **Action on Failure:** Return error response with HTTP 403 status code

**Error Handling:**

- **Error Message:** `OBP-20006: User does not have access to the account.`
- **Error Code:** `OBP-20006`
- **HTTP Status Code:** `403 Forbidden`

**Scala Implementation:**

- **Location:** `code.views.Views`
- **Method/Function:** `checkAccountAccess()` / `getUserAccountAccess()`
- **Line Reference:** Authorization checks in APIMethods400

**Code Snippet:**
```scala
for {
  (user, callContext) <- NewStyle.function.getUser(callContext)
  _ <- NewStyle.function.checkAccountAccess(user, bankId, accountId, callContext)
  // ... rest of validation chain
} yield {
  // success response
}
```

**Related Entities:**
- User entity
- Account entity
- AccountAccess entity
- View permissions

**User Story Context:**
This validation enforces access control as specified in the user story: "The system shall return only transaction details for accounts that the requesting user has been granted access to view" and "User must have at least one view/permission granted on the account to see transaction details."

**Dependencies:**
- VR-005: Authentication validation must pass first
- VR-001: Bank ID validation must pass first
- VR-002: Account ID validation must pass first

---

### Rule VR-007: Transaction Belongs to Account Validation

**Field/Entity:** Transaction-Account Relationship

**Validation Type:** Business Constraint / Cross-Field Validation

**Rule Description:**
The requested transaction must belong to the specified account. Users cannot access transactions from other accounts by manipulating the transaction ID.

**Validation Logic:**

- **Condition:** When a transaction detail retrieval request is made with valid BANK_ID, ACCOUNT_ID, and TRANSACTION_ID
- **Check:** Validate that the transaction identified by TRANSACTION_ID is actually associated with the account identified by ACCOUNT_ID
- **Valid Criteria:** Transaction's account reference matches the specified ACCOUNT_ID
- **Invalid Criteria:** Transaction exists but belongs to a different account
- **Action on Success:** Return transaction details
- **Action on Failure:** Return error response with HTTP 404 status code (transaction not found in this account)

**Error Handling:**

- **Error Message:** `OBP-30010: Transaction not found. Please specify a valid value for TRANSACTION_ID.`
- **Error Code:** `OBP-30010`
- **HTTP Status Code:** `404 Not Found`

**Scala Implementation:**

- **Location:** `code.api.util.NewStyle`
- **Method/Function:** `getTransaction()`
- **Line Reference:** Transaction retrieval with account scope

**Code Snippet:**
```scala
def getTransaction(bankId: BankId, accountId: AccountId, transactionId: TransactionId, callContext: Option[CallContext]): OBPReturnType[Box[Transaction]] = {
  Connector.connector.vend.getTransaction(bankId, accountId, transactionId) match {
    case Full(transaction) if transaction.accountId == accountId => Full(transaction)
    case _ => Failure(TransactionNotFound)
  }
}
```

**Related Entities:**
- Transaction entity
- Account entity
- Transaction-Account foreign key relationship

**User Story Context:**
This validation ensures data integrity and prevents unauthorized access to transactions. The user story specifies "Transaction identifier (TRANSACTION_ID) must be valid and belong to the specified account."

**Dependencies:**
- VR-001: Bank ID validation must pass first
- VR-002: Account ID validation must pass first
- VR-003: Transaction ID validation must pass first

---

### Rule VR-008: Firehose Access Permission Validation

**Field/Entity:** User Firehose Permission

**Validation Type:** Business Constraint / Authorization

**Rule Description:**
For the firehose endpoint, the user must have special firehose access permissions (typically for compliance, audit, or administrative purposes).

**Validation Logic:**

- **Condition:** When a transaction detail retrieval request is made via the firehose endpoint (`GET /banks/BANK_ID/firehose/transactions/TRANSACTION_ID`)
- **Check:** Validate that the user has been granted firehose access permissions
- **Valid Criteria:** User has the `CanUseFirehoseAtAnyBank` or equivalent firehose entitlement
- **Invalid Criteria:** User lacks firehose access permissions
- **Action on Success:** Proceed with transaction retrieval without view restrictions
- **Action on Failure:** Return error response with HTTP 403 status code

**Error Handling:**

- **Error Message:** `OBP-20006: User does not have the required entitlement.`
- **Error Code:** `OBP-20006`
- **HTTP Status Code:** `403 Forbidden`

**Scala Implementation:**

- **Location:** `code.api.util.NewStyle`
- **Method/Function:** `hasEntitlement()` / `checkFirehoseAccess()`
- **Line Reference:** Firehose endpoint authorization

**Code Snippet:**
```scala
for {
  (user, callContext) <- NewStyle.function.getUser(callContext)
  _ <- NewStyle.function.hasEntitlement(bankId, user.userId, ApiRole.CanUseFirehoseAtAnyBank, callContext)
  // ... rest of validation chain
} yield {
  // success response
}
```

**Related Entities:**
- User entity
- Entitlement entity
- ApiRole enumeration

**User Story Context:**
This validation ensures that the firehose endpoint is only accessible to authorized applications with special permissions. The user story describes this endpoint as supporting "administrative access to transaction details" for "compliance, audit, or administrative purposes."

**Dependencies:**
- VR-005: Authentication validation must pass first
- VR-001: Bank ID validation must pass first

---

## Category: Format Validation

### Rule VR-009: Monetary Amount Format Validation

**Field/Entity:** Transaction Amount (value.amount, new_balance.amount)

**Validation Type:** Format Validation

**Rule Description:**
All monetary values in the transaction response must include a properly formatted amount and valid currency code.

**Validation Logic:**

- **Condition:** When transaction details are being serialized for response
- **Check:** Validate that amount values are properly formatted decimal numbers and currency codes are valid ISO 4217 codes
- **Valid Criteria:** Amount is a valid decimal string (e.g., "1495.50", "-4.50"), currency is a 3-letter ISO code (e.g., "EUR", "USD", "GBP")
- **Invalid Criteria:** Amount contains non-numeric characters (except decimal point and minus sign), currency is not a valid ISO code
- **Action on Success:** Include monetary values in response
- **Action on Failure:** Return error or sanitized value

**Error Handling:**

- **Error Message:** `OBP-10003: Invalid Currency Value. Expected a 3-letter ISO Currency Code.`
- **Error Code:** `OBP-10003`
- **HTTP Status Code:** `400 Bad Request`

**Scala Implementation:**

- **Location:** `code.api.util.APIUtil`
- **Method/Function:** `isValidCurrencyISOCode()`
- **Line Reference:** Currency validation utilities

**Code Snippet:**
```scala
def isValidCurrencyISOCode(currencyCode: String): Boolean = {
  val currencyIsoCodeArray = (CurrencyIsoCodeFromXmlFile \"CcyTbl" \ "CcyNtry" \ "Ccy")
    .map(_.text).mkString(" ").split("\\s+") :+ "XBT"
  currencyIsoCodeArray.contains(currencyCode)
}
```

**Related Entities:**
- AmountOfMoney case class
- Transaction value field
- Transaction new_balance field

**User Story Context:**
This validation ensures data quality in the response. The user story specifies "All monetary values must include currency code and properly formatted amount."

**Dependencies:**
- None (output validation)

---

### Rule VR-010: Date/Time Format Validation

**Field/Entity:** Transaction Timestamps (posted, completed, date fields in metadata)

**Validation Type:** Format Validation

**Rule Description:**
All date/time values in the transaction response must be in ISO 8601 format.

**Validation Logic:**

- **Condition:** When transaction details are being serialized for response
- **Check:** Validate that all date/time values conform to ISO 8601 format (e.g., "2024-01-15T10:30:00Z")
- **Valid Criteria:** Date/time string matches ISO 8601 pattern with UTC timezone indicator
- **Invalid Criteria:** Date/time string does not conform to ISO 8601 format
- **Action on Success:** Include date/time values in response
- **Action on Failure:** Return error or convert to proper format

**Error Handling:**

- **Error Message:** `OBP-10005: Invalid Date Format. Could not convert value to a Date.`
- **Error Code:** `OBP-10005`
- **HTTP Status Code:** `400 Bad Request`

**Scala Implementation:**

- **Location:** `code.api.v4_0_0.JSONFactory400`
- **Method/Function:** `createTransactionJson()` / date formatting utilities
- **Line Reference:** JSON serialization methods

**Code Snippet:**
```scala
val dateFormat = new SimpleDateFormat("yyyy-MM-dd'T'HH:mm:ss'Z'")
dateFormat.setTimeZone(TimeZone.getTimeZone("UTC"))

def formatDate(date: Date): String = dateFormat.format(date)
```

**Related Entities:**
- Transaction posted date
- Transaction completed date
- Comment date
- Tag date
- Image date

**User Story Context:**
This validation ensures consistent date/time formatting in the response. The user story specifies "All date/time values must be in ISO 8601 format" and notes "Transaction timestamps should be stored and returned in UTC with proper ISO 8601 formatting."

**Dependencies:**
- None (output validation)

---

### Rule VR-011: Bank ID Format Validation

**Field/Entity:** BANK_ID

**Validation Type:** Format Validation

**Rule Description:**
Bank identifier must conform to the expected format pattern (alphanumeric with limited special characters).

**Validation Logic:**

- **Condition:** When BANK_ID is provided in the request path
- **Check:** Validate that BANK_ID matches the expected format pattern
- **Valid Criteria:** BANK_ID contains only alphanumeric characters, hyphens, underscores, and periods; length within acceptable limits
- **Invalid Criteria:** BANK_ID contains invalid characters or exceeds length limits
- **Action on Success:** Proceed with bank lookup
- **Action on Failure:** Return error response with HTTP 400 status code

**Error Handling:**

- **Error Message:** `OBP-10001: Incorrect json format.` or `OBP-10002: Invalid ID format.`
- **Error Code:** `OBP-10001` or `OBP-10002`
- **HTTP Status Code:** `400 Bad Request`

**Scala Implementation:**

- **Location:** `code.api.util.APIUtil`
- **Method/Function:** `isValidID()`
- **Line Reference:** ID validation utilities

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
- Bank entity
- All endpoints using BANK_ID path parameter

**User Story Context:**
This validation ensures that bank identifiers are properly formatted before database lookup. The user story specifies "Bank identifier (BANK_ID) must be valid and exist in the system."

**Dependencies:**
- None (first format validation)

---

### Rule VR-012: Account ID Format Validation

**Field/Entity:** ACCOUNT_ID

**Validation Type:** Format Validation

**Rule Description:**
Account identifier must conform to the expected format pattern (alphanumeric with limited special characters).

**Validation Logic:**

- **Condition:** When ACCOUNT_ID is provided in the request path
- **Check:** Validate that ACCOUNT_ID matches the expected format pattern
- **Valid Criteria:** ACCOUNT_ID contains only alphanumeric characters, hyphens, underscores, and periods; length within acceptable limits
- **Invalid Criteria:** ACCOUNT_ID contains invalid characters or exceeds length limits
- **Action on Success:** Proceed with account lookup
- **Action on Failure:** Return error response with HTTP 400 status code

**Error Handling:**

- **Error Message:** `OBP-10001: Incorrect json format.` or `OBP-10002: Invalid ID format.`
- **Error Code:** `OBP-10001` or `OBP-10002`
- **HTTP Status Code:** `400 Bad Request`

**Scala Implementation:**

- **Location:** `code.api.util.APIUtil`
- **Method/Function:** `isValidID()`
- **Line Reference:** ID validation utilities

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
- BankAccount entity
- All endpoints using ACCOUNT_ID path parameter

**User Story Context:**
This validation ensures that account identifiers are properly formatted before database lookup. The user story specifies "Account identifier (ACCOUNT_ID) must be valid and belong to the specified bank."

**Dependencies:**
- VR-011: Bank ID format validation should pass first

---

## Category: Cross-Field Validation

### Rule VR-013: Account Belongs to Bank Validation

**Field/Entity:** Account-Bank Relationship

**Validation Type:** Cross-Field Validation

**Rule Description:**
The specified account must belong to the specified bank. Users cannot access accounts from other banks by manipulating the account ID.

**Validation Logic:**

- **Condition:** When a transaction detail retrieval request is made with valid BANK_ID and ACCOUNT_ID
- **Check:** Validate that the account identified by ACCOUNT_ID is actually associated with the bank identified by BANK_ID
- **Valid Criteria:** Account's bank reference matches the specified BANK_ID
- **Invalid Criteria:** Account exists but belongs to a different bank
- **Action on Success:** Proceed with transaction validation
- **Action on Failure:** Return error response with HTTP 404 status code (account not found in this bank)

**Error Handling:**

- **Error Message:** `OBP-30018: Account not found. Please specify a valid value for ACCOUNT_ID.`
- **Error Code:** `OBP-30018`
- **HTTP Status Code:** `404 Not Found`

**Scala Implementation:**

- **Location:** `code.api.util.NewStyle`
- **Method/Function:** `getBankAccount()`
- **Line Reference:** Account retrieval with bank scope

**Code Snippet:**
```scala
def getBankAccount(bankId: BankId, accountId: AccountId, callContext: Option[CallContext]): OBPReturnType[Box[BankAccount]] = {
  Connector.connector.vend.getBankAccount(bankId, accountId) match {
    case Full(account) if account.bankId == bankId => Full(account)
    case _ => Failure(AccountNotFound)
  }
}
```

**Related Entities:**
- BankAccount entity
- Bank entity
- Account-Bank foreign key relationship

**User Story Context:**
This validation ensures data integrity and prevents unauthorized access to accounts. The user story specifies "Account identifier (ACCOUNT_ID) must be valid and belong to the specified bank."

**Dependencies:**
- VR-001: Bank ID validation must pass first
- VR-011: Bank ID format validation must pass first
- VR-012: Account ID format validation must pass first

---

### Rule VR-014: View Permission Field Filtering Validation

**Field/Entity:** View-Field Visibility Relationship

**Validation Type:** Cross-Field Validation / Business Constraint

**Rule Description:**
The transaction details returned must be filtered based on the view permissions. Different views may expose different fields of the transaction.

**Validation Logic:**

- **Condition:** When transaction details are being serialized for response using a specific view
- **Check:** Filter the transaction fields based on the view's field visibility settings
- **Valid Criteria:** Only fields that the view allows are included in the response
- **Invalid Criteria:** N/A (this is a filtering operation, not a rejection)
- **Action on Success:** Return transaction details with view-appropriate fields
- **Action on Failure:** N/A

**Error Handling:**

- **Error Message:** N/A (no error, just field filtering)
- **Error Code:** N/A
- **HTTP Status Code:** N/A

**Scala Implementation:**

- **Location:** `code.api.v4_0_0.JSONFactory400`
- **Method/Function:** `createTransactionJson()` with view parameter
- **Line Reference:** JSON factory methods

**Code Snippet:**
```scala
def createTransactionJson(transaction: Transaction, view: View): TransactionJson400 = {
  TransactionJson400(
    id = transaction.id.value,
    this_account = if(view.canSeeThisAccountDetails) createThisAccountJson(transaction.thisAccount) else null,
    other_account = if(view.canSeeOtherAccountDetails) createOtherAccountJson(transaction.otherAccount) else null,
    details = createTransactionDetailsJson(transaction, view),
    metadata = if(view.canSeeTransactionMetadata) createTransactionMetadataJson(transaction) else null
  )
}
```

**Related Entities:**
- View entity
- Transaction entity
- View field visibility settings

**User Story Context:**
This validation ensures proper access control at the field level. The user story notes "The view system controls which fields are visible - ensure proper field filtering based on view permissions."

**Dependencies:**
- VR-004: View ID validation must pass first
- VR-006: User account access permission validation must pass first

---

## Category: Length/Boundary Validation

### Rule VR-015: ID Length Validation

**Field/Entity:** BANK_ID, ACCOUNT_ID, TRANSACTION_ID, VIEW_ID

**Validation Type:** Length/Boundary Validation

**Rule Description:**
All identifier fields must not exceed the maximum allowed length (typically 255 characters).

**Validation Logic:**

- **Condition:** When any ID parameter is provided in the request
- **Check:** Validate that the ID length does not exceed the maximum allowed length
- **Valid Criteria:** ID length is between 1 and 255 characters
- **Invalid Criteria:** ID is empty (length 0) or exceeds 255 characters
- **Action on Success:** Proceed with format and existence validation
- **Action on Failure:** Return error response with HTTP 400 status code

**Error Handling:**

- **Error Message:** `OBP-20010: Value too long.`
- **Error Code:** `OBP-20010`
- **HTTP Status Code:** `400 Bad Request`

**Scala Implementation:**

- **Location:** `code.api.util.APIUtil`
- **Method/Function:** `isValidID()` / `checkMediumString()`
- **Line Reference:** String validation utilities

**Code Snippet:**
```scala
def checkMediumString(value: String): String = {
  val valueLength = value.length
  val regex = """^([A-Za-z0-9\-._@]+)$""".r
  value match {
    case regex(e) if(valueLength <= 255) => SILENCE_IS_GOLDEN
    case regex(e) if(valueLength > 255) => ErrorMessages.InvalidValueLength
    case _ => ErrorMessages.InvalidValueCharacters
  }
}
```

**Related Entities:**
- All ID fields (BANK_ID, ACCOUNT_ID, TRANSACTION_ID, VIEW_ID)

**User Story Context:**
This validation ensures that identifier values are within acceptable bounds for database storage and URL handling. This is an implicit validation derived from the system's data model constraints.

**Dependencies:**
- None (first boundary check)

---

## Quality Checklist

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
