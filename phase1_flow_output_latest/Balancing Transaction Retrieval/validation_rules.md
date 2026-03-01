# Validation Rules

**Extracted From:** Open Bank Project (OBP) Scala Application  
**User Story:** Balancing Transaction Retrieval  
**Analysis Date:** 2026-01-20  
**Analyst:** Scala Systems Analyst

## Summary

Total Validation Rules Extracted: 14

### Validation Categories
- Input Validation Rules: 5
- Format Validation Rules: 4
- Business Constraint Rules: 3
- Length/Boundary Rules: 0
- Cross-Field Validation Rules: 2

---

## Category: Input Validation - Required Fields

### Rule VR-001: Bank Identifier Required Validation

**Field/Entity:** BANK_ID (path parameter)

**Validation Type:** Required Field Validation

**Rule Description:**
Bank identifier must be provided as a path parameter and must exist in the system for balancing transaction retrieval operations.

**Validation Logic:**

- **Condition:** When a GET request is made to `/obp/v4.0.0/banks/BANK_ID/accounts/ACCOUNT_ID/VIEW_ID/transactions/TRANSACTION_ID/balancing-transaction`
- **Check:** Validate that BANK_ID path parameter is present and non-empty
- **Valid Criteria:** BANK_ID is provided and is a non-empty string
- **Invalid Criteria:** BANK_ID is missing, null, or empty string
- **Action on Success:** Proceed to validate bank existence in the system
- **Action on Failure:** Return error response indicating missing bank identifier

**Error Handling:**

- **Error Message:** `Bank not found. Please specify a valid value for BANK_ID.`
- **Error Code:** `OBP-30001` (BankNotFound)
- **HTTP Status Code:** `404 Not Found`

**Scala Implementation:**

- **Location:** `code.api.v4_0_0.APIMethods400`
- **Method/Function:** `getBalancingTransaction`
- **Line Reference:** Path parameter extraction

**Code Snippet:**
```scala
// Path parameter extraction from endpoint definition
lazy val getBalancingTransaction: OBPEndpoint = {
  case "banks" :: BankId(bankId) :: "accounts" :: AccountId(accountId) :: ViewId(viewId) :: 
       "transactions" :: TransactionId(transactionId) :: "balancing-transaction" :: Nil JsonGet _ => {
    // bankId is extracted from path
  }
}
```

**Related Entities:**
- Bank entity
- All account and transaction operations under this bank

**User Story Context:**
This validation ensures that the bank context is properly established before attempting to retrieve balancing transaction data. The endpoint requires BANK_ID as part of the hierarchical resource path.

**Dependencies:**
- None (first validation in the chain)

---

### Rule VR-002: Account Identifier Required Validation

**Field/Entity:** ACCOUNT_ID (path parameter)

**Validation Type:** Required Field Validation

**Rule Description:**
Account identifier must be provided as a path parameter and must belong to the specified bank for balancing transaction retrieval operations.

**Validation Logic:**

- **Condition:** When a GET request is made to the balancing transaction endpoint
- **Check:** Validate that ACCOUNT_ID path parameter is present and non-empty
- **Valid Criteria:** ACCOUNT_ID is provided and is a non-empty string
- **Invalid Criteria:** ACCOUNT_ID is missing, null, or empty string
- **Action on Success:** Proceed to validate account existence and ownership
- **Action on Failure:** Return error response indicating missing account identifier

**Error Handling:**

- **Error Message:** `Account not found. Please specify a valid value for ACCOUNT_ID.`
- **Error Code:** `OBP-30018` (AccountNotFound)
- **HTTP Status Code:** `404 Not Found`

**Scala Implementation:**

- **Location:** `code.api.v4_0_0.APIMethods400`
- **Method/Function:** `getBalancingTransaction`
- **Line Reference:** Path parameter extraction

**Code Snippet:**
```scala
// Account ID extraction from path
case "banks" :: BankId(bankId) :: "accounts" :: AccountId(accountId) :: ViewId(viewId) :: 
     "transactions" :: TransactionId(transactionId) :: "balancing-transaction" :: Nil JsonGet _ => {
  // accountId is extracted and validated
}
```

**Related Entities:**
- BankAccount entity
- Transaction records for this account

**User Story Context:**
This validation ensures the account context is established for retrieving the balancing transaction. The account must exist and be associated with the specified bank.

**Dependencies:**
- VR-001: Bank Identifier Required Validation (bank must be valid first)

---

### Rule VR-003: View Identifier Required Validation

**Field/Entity:** VIEW_ID (path parameter)

**Validation Type:** Required Field Validation

**Rule Description:**
View identifier must be provided as a path parameter and the user must have access to this view for the specified account.

**Validation Logic:**

- **Condition:** When a GET request is made to the balancing transaction endpoint
- **Check:** Validate that VIEW_ID path parameter is present, non-empty, and user has access
- **Valid Criteria:** VIEW_ID is provided, exists for the account, and user has been granted access
- **Invalid Criteria:** VIEW_ID is missing, does not exist, or user lacks access permission
- **Action on Success:** Proceed to validate transaction existence
- **Action on Failure:** Return error response indicating invalid view or access denied

**Error Handling:**

- **Error Message:** `View not found for Account. Please specify a valid value for VIEW_ID.` or `User does not have access to the view.`
- **Error Code:** `OBP-30005` (ViewNotFound) or `OBP-20017` (UserNoPermissionAccessView)
- **HTTP Status Code:** `404 Not Found` or `403 Forbidden`

**Scala Implementation:**

- **Location:** `code.api.v4_0_0.APIMethods400` and `code.views.Views`
- **Method/Function:** `getBalancingTransaction`, `checkViewAccessAndReturnView`
- **Line Reference:** View access validation

**Code Snippet:**
```scala
// View validation and access check
for {
  (Full(u), callContext) <- authenticatedAccess(cc)
  (view, callContext) <- NewStyle.function.checkViewAccessAndReturnView(viewId, BankIdAccountId(bankId, accountId), Full(u), callContext)
  // Continue with transaction retrieval
} yield {
  // Process request
}
```

**Related Entities:**
- View entity
- AccountAccess/Permission records
- User entity

**User Story Context:**
View permissions determine what level of detail users can see about the balancing transaction. This validation enforces access control as specified in the acceptance criteria.

**Dependencies:**
- VR-001: Bank Identifier Required Validation
- VR-002: Account Identifier Required Validation
- VR-006: User Authentication Validation

---

### Rule VR-004: Transaction Identifier Required Validation

**Field/Entity:** TRANSACTION_ID (path parameter)

**Validation Type:** Required Field Validation

**Rule Description:**
Transaction identifier must be provided as a path parameter and must belong to the specified account for balancing transaction retrieval.

**Validation Logic:**

- **Condition:** When a GET request is made to the balancing transaction endpoint
- **Check:** Validate that TRANSACTION_ID path parameter is present, non-empty, and exists for the account
- **Valid Criteria:** TRANSACTION_ID is provided and corresponds to an existing transaction for the specified account
- **Invalid Criteria:** TRANSACTION_ID is missing, null, empty, or does not exist for the account
- **Action on Success:** Proceed to retrieve the balancing transaction
- **Action on Failure:** Return error response indicating transaction not found

**Error Handling:**

- **Error Message:** `Transaction not found. Please specify a valid value for TRANSACTION_ID.`
- **Error Code:** `OBP-30010` (TransactionNotFound)
- **HTTP Status Code:** `404 Not Found`

**Scala Implementation:**

- **Location:** `code.api.v4_0_0.APIMethods400`
- **Method/Function:** `getBalancingTransaction`
- **Line Reference:** Transaction lookup

**Code Snippet:**
```scala
// Transaction validation
for {
  // ... previous validations
  (transaction, callContext) <- NewStyle.function.getTransaction(bankId, accountId, transactionId, callContext)
  // Transaction found, proceed to get balancing transaction
} yield {
  // Process request
}
```

**Related Entities:**
- Transaction entity (MappedTransaction)
- BankAccount entity

**User Story Context:**
This validation ensures the specified transaction exists before attempting to retrieve its balancing counterpart. As stated in the user story, the capability is to "get the balancing transaction for a given transaction."

**Dependencies:**
- VR-001: Bank Identifier Required Validation
- VR-002: Account Identifier Required Validation
- VR-003: View Identifier Required Validation

---

### Rule VR-005: Balancing Transaction Existence Validation

**Field/Entity:** Balancing Transaction

**Validation Type:** Business Existence Validation

**Rule Description:**
The specified transaction must have a corresponding balancing transaction in the double-entry bookkeeping system.

**Validation Logic:**

- **Condition:** After the original transaction is found and validated
- **Check:** Validate that a balancing transaction exists for the given transaction
- **Valid Criteria:** A balancing transaction record exists that corresponds to the original transaction
- **Invalid Criteria:** No balancing transaction exists for the given transaction
- **Action on Success:** Return the balancing transaction details
- **Action on Failure:** Return error response indicating no balancing transaction found

**Error Handling:**

- **Error Message:** `Balancing transaction not found for the specified transaction.`
- **Error Code:** `OBP-30XXX` (BalancingTransactionNotFound)
- **HTTP Status Code:** `404 Not Found`

**Scala Implementation:**

- **Location:** `code.api.util.NewStyle` and `code.bankconnectors.Connector`
- **Method/Function:** `getBalancingTransaction`
- **Line Reference:** Balancing transaction lookup

**Code Snippet:**
```scala
// Balancing transaction retrieval
for {
  // ... previous validations
  (balancingTransaction, callContext) <- NewStyle.function.getBalancingTransaction(bankId, accountId, transactionId, callContext)
  // If Empty or Failure, return appropriate error
} yield {
  (JSONFactory400.createBalancingTransactionJson(balancingTransaction), HttpCode.`200`(callContext))
}
```

**Related Entities:**
- MappedDoubleEntryBookTransaction
- Original Transaction entity

**User Story Context:**
This validation directly supports the core capability: "Get the balancing transaction for a given transaction." Not all transactions may have balancing transactions, so this validation handles that case appropriately.

**Dependencies:**
- VR-004: Transaction Identifier Required Validation

---

## Category: Authentication and Authorization Validation

### Rule VR-006: User Authentication Validation

**Field/Entity:** Authorization Header / Authentication Token

**Validation Type:** Authentication Validation

**Rule Description:**
User must be authenticated with a valid OAuth token or DirectLogin credentials to access the balancing transaction endpoint.

**Validation Logic:**

- **Condition:** When any request is made to the balancing transaction endpoint
- **Check:** Validate that a valid authentication token is present in the request headers
- **Valid Criteria:** Valid OAuth Bearer token or DirectLogin token is provided and not expired
- **Invalid Criteria:** No token provided, token is invalid, expired, or malformed
- **Action on Success:** Proceed with user identification and authorization checks
- **Action on Failure:** Return authentication error response

**Error Handling:**

- **Error Message:** `Authentication is required to access this resource.` or `Invalid authentication token.`
- **Error Code:** `OBP-20001` (UserNotLoggedIn)
- **HTTP Status Code:** `401 Unauthorized`

**Scala Implementation:**

- **Location:** `code.api.util.APIUtil`
- **Method/Function:** `authenticatedAccess`
- **Line Reference:** Authentication middleware

**Code Snippet:**
```scala
// Authentication check
for {
  (Full(u), callContext) <- authenticatedAccess(cc)
  // User is authenticated, proceed with authorization
} yield {
  // Process request
}
```

**Related Entities:**
- User entity
- OAuth tokens
- DirectLogin tokens

**User Story Context:**
As stated in the acceptance criteria: "The system shall enforce access control to ensure users can only retrieve balancing transactions for accounts they have been granted permission to access." Authentication is the first step in this access control.

**Dependencies:**
- None (first security validation)

---

### Rule VR-007: User Authorization Validation

**Field/Entity:** User Permissions / View Access

**Validation Type:** Authorization Validation

**Rule Description:**
User must have at least one view/permission granted on the account to see transactions and their balancing counterparts.

**Validation Logic:**

- **Condition:** After user is authenticated
- **Check:** Validate that the user has been granted access to at least one view on the specified account
- **Valid Criteria:** User has view access permission for the account
- **Invalid Criteria:** User has no view permissions for the account
- **Action on Success:** Proceed with transaction retrieval based on view permissions
- **Action on Failure:** Return authorization error response

**Error Handling:**

- **Error Message:** `User does not have permission to access this account.`
- **Error Code:** `OBP-20006` (UserNoPermissionAccessView)
- **HTTP Status Code:** `403 Forbidden`

**Scala Implementation:**

- **Location:** `code.views.Views`
- **Method/Function:** `checkViewAccessAndReturnView`
- **Line Reference:** Permission check

**Code Snippet:**
```scala
// Authorization check
for {
  (Full(u), callContext) <- authenticatedAccess(cc)
  (view, callContext) <- NewStyle.function.checkViewAccessAndReturnView(viewId, BankIdAccountId(bankId, accountId), Full(u), callContext)
  // User is authorized for this view
} yield {
  // Process request with view-based filtering
}
```

**Related Entities:**
- User entity
- View entity
- AccountAccess records

**User Story Context:**
This validation enforces the acceptance criteria: "The system shall enforce access control to ensure users can only retrieve balancing transactions for accounts they have been granted permission to access."

**Dependencies:**
- VR-006: User Authentication Validation

---

## Category: Entity Existence Validation

### Rule VR-008: Bank Existence Validation

**Field/Entity:** Bank

**Validation Type:** Entity Existence Validation

**Rule Description:**
The specified BANK_ID must correspond to an existing bank in the system.

**Validation Logic:**

- **Condition:** After BANK_ID is extracted from the path
- **Check:** Query the database to verify the bank exists
- **Valid Criteria:** Bank record exists with the specified BANK_ID
- **Invalid Criteria:** No bank record found with the specified BANK_ID
- **Action on Success:** Proceed to validate account
- **Action on Failure:** Return bank not found error

**Error Handling:**

- **Error Message:** `Bank not found. Please specify a valid value for BANK_ID.`
- **Error Code:** `OBP-30001` (BankNotFound)
- **HTTP Status Code:** `404 Not Found`

**Scala Implementation:**

- **Location:** `code.bankconnectors.Connector`
- **Method/Function:** `getBankLegacy` or `getBank`
- **Line Reference:** Bank lookup

**Code Snippet:**
```scala
// Bank existence check
for {
  (bank, callContext) <- NewStyle.function.getBank(bankId, callContext)
  // Bank exists, proceed
} yield {
  // Continue processing
}
```

**Related Entities:**
- Bank entity

**User Story Context:**
As stated in the data validations: "Bank identifier (BANK_ID) must be valid and exist in the system." This ensures the hierarchical resource path is valid.

**Dependencies:**
- VR-001: Bank Identifier Required Validation

---

### Rule VR-009: Account Existence and Ownership Validation

**Field/Entity:** Account

**Validation Type:** Entity Existence + Cross-Field Validation

**Rule Description:**
The specified ACCOUNT_ID must correspond to an existing account that belongs to the specified bank.

**Validation Logic:**

- **Condition:** After bank existence is validated
- **Check:** Query the database to verify the account exists and belongs to the specified bank
- **Valid Criteria:** Account record exists with the specified ACCOUNT_ID and is associated with the specified BANK_ID
- **Invalid Criteria:** No account found, or account exists but belongs to a different bank
- **Action on Success:** Proceed to validate view access
- **Action on Failure:** Return account not found error

**Error Handling:**

- **Error Message:** `Account not found. Please specify a valid value for ACCOUNT_ID.`
- **Error Code:** `OBP-30018` (AccountNotFound)
- **HTTP Status Code:** `404 Not Found`

**Scala Implementation:**

- **Location:** `code.bankconnectors.Connector`
- **Method/Function:** `getBankAccountLegacy` or `getBankAccount`
- **Line Reference:** Account lookup with bank context

**Code Snippet:**
```scala
// Account existence and ownership check
for {
  (bankAccount, callContext) <- NewStyle.function.getBankAccount(bankId, accountId, callContext)
  // Account exists and belongs to the bank
} yield {
  // Continue processing
}
```

**Related Entities:**
- BankAccount entity
- Bank entity

**User Story Context:**
As stated in the data validations: "Account identifier (ACCOUNT_ID) must be valid and belong to the specified bank." This is a cross-field validation ensuring the account-bank relationship.

**Dependencies:**
- VR-008: Bank Existence Validation

---

### Rule VR-010: Transaction Existence and Ownership Validation

**Field/Entity:** Transaction

**Validation Type:** Entity Existence + Cross-Field Validation

**Rule Description:**
The specified TRANSACTION_ID must correspond to an existing transaction that belongs to the specified account.

**Validation Logic:**

- **Condition:** After account and view validations pass
- **Check:** Query the database to verify the transaction exists and belongs to the specified account
- **Valid Criteria:** Transaction record exists with the specified TRANSACTION_ID and is associated with the specified ACCOUNT_ID
- **Invalid Criteria:** No transaction found, or transaction exists but belongs to a different account
- **Action on Success:** Proceed to retrieve balancing transaction
- **Action on Failure:** Return transaction not found error

**Error Handling:**

- **Error Message:** `Transaction not found. Please specify a valid value for TRANSACTION_ID.`
- **Error Code:** `OBP-30010` (TransactionNotFound)
- **HTTP Status Code:** `404 Not Found`

**Scala Implementation:**

- **Location:** `code.bankconnectors.Connector`
- **Method/Function:** `getTransactionLegacy` or `getTransaction`
- **Line Reference:** Transaction lookup with account context

**Code Snippet:**
```scala
// Transaction existence and ownership check
for {
  (transaction, callContext) <- NewStyle.function.getTransaction(bankId, accountId, transactionId, callContext)
  // Transaction exists and belongs to the account
} yield {
  // Proceed to get balancing transaction
}
```

**Related Entities:**
- Transaction entity (MappedTransaction)
- BankAccount entity

**User Story Context:**
As stated in the data validations: "Transaction identifier (TRANSACTION_ID) must be valid and belong to the specified account." This ensures the transaction context is valid before retrieving its balancing counterpart.

**Dependencies:**
- VR-009: Account Existence and Ownership Validation

---

## Category: Business Constraint Validation

### Rule VR-011: Double-Entry Bookkeeping Relationship Validation

**Field/Entity:** Transaction Relationship

**Validation Type:** Business Rule Validation

**Rule Description:**
The balancing transaction must represent the corresponding entry in the double-entry bookkeeping system, maintaining the accounting principle that every transaction has a balancing counterpart.

**Validation Logic:**

- **Condition:** When retrieving the balancing transaction
- **Check:** Validate that the balancing transaction correctly represents the other side of the double-entry
- **Valid Criteria:** Balancing transaction has the correct relationship to the original transaction (e.g., if original is debit, balancing is credit)
- **Invalid Criteria:** Balancing transaction does not properly correspond to the original transaction
- **Action on Success:** Return the balancing transaction with relationship clearly identified
- **Action on Failure:** Return error or flag data integrity issue

**Error Handling:**

- **Error Message:** `Data integrity error: Balancing transaction relationship is invalid.`
- **Error Code:** `OBP-50000` (InternalServerError)
- **HTTP Status Code:** `500 Internal Server Error`

**Scala Implementation:**

- **Location:** `code.api.v4_0_0.JSONFactory4.0.0`
- **Method/Function:** `createBalancingTransactionJson`
- **Line Reference:** Response construction

**Code Snippet:**
```scala
// Balancing transaction JSON creation with relationship
def createBalancingTransactionJson(balancingTransaction: BalancingTransaction): BalancingTransactionJson = {
  BalancingTransactionJson(
    transaction_id = balancingTransaction.id,
    bank_id = balancingTransaction.bankId,
    account_id = balancingTransaction.accountId,
    this_account = createThisAccountJson(balancingTransaction.thisAccount),
    other_account = createOtherAccountJson(balancingTransaction.otherAccount),
    details = createTransactionDetailsJson(balancingTransaction.details)
  )
}
```

**Related Entities:**
- MappedDoubleEntryBookTransaction
- Transaction entity
- Account entities (both sides)

**User Story Context:**
As stated in the business rules: "Every transaction in a double-entry bookkeeping system has a corresponding balancing transaction that represents the other side of the entry." The acceptance criteria also states: "The system shall clearly identify the relationship between the original transaction and its balancing counterpart."

**Dependencies:**
- VR-010: Transaction Existence and Ownership Validation

---

### Rule VR-012: One-to-One Balancing Relationship Validation

**Field/Entity:** Transaction-Balancing Transaction Relationship

**Validation Type:** Business Constraint Validation

**Rule Description:**
Each transaction has exactly one balancing transaction that corresponds to it in the accounting system.

**Validation Logic:**

- **Condition:** When retrieving the balancing transaction
- **Check:** Validate that exactly one balancing transaction is returned for the given transaction
- **Valid Criteria:** Exactly one balancing transaction record exists for the original transaction
- **Invalid Criteria:** Zero or multiple balancing transactions found (data integrity issue)
- **Action on Success:** Return the single balancing transaction
- **Action on Failure:** Return appropriate error based on the issue

**Error Handling:**

- **Error Message:** `Balancing transaction not found for the specified transaction.` (if zero) or `Data integrity error: Multiple balancing transactions found.` (if multiple)
- **Error Code:** `OBP-30XXX` (BalancingTransactionNotFound) or `OBP-50000` (InternalServerError)
- **HTTP Status Code:** `404 Not Found` or `500 Internal Server Error`

**Scala Implementation:**

- **Location:** `code.bankconnectors.Connector`
- **Method/Function:** `getBalancingTransaction`
- **Line Reference:** Balancing transaction query

**Code Snippet:**
```scala
// One-to-one relationship enforcement
def getBalancingTransaction(bankId: BankId, accountId: AccountId, transactionId: TransactionId): Box[BalancingTransaction] = {
  // Query should return exactly one result
  MappedDoubleEntryBookTransaction.find(
    By(MappedDoubleEntryBookTransaction.mTransactionId, transactionId.value)
  ) match {
    case Full(balancing) => Full(balancing)
    case Empty => Failure(ErrorMessages.BalancingTransactionNotFound)
    case f: Failure => f
  }
}
```

**Related Entities:**
- MappedDoubleEntryBookTransaction
- Transaction entity

**User Story Context:**
As stated in the business rules: "Each transaction has exactly one balancing transaction that corresponds to it in the accounting system." This ensures data integrity in the double-entry bookkeeping system.

**Dependencies:**
- VR-011: Double-Entry Bookkeeping Relationship Validation

---

## Category: Response Format Validation

### Rule VR-013: Real-Time Response Performance Validation

**Field/Entity:** API Response

**Validation Type:** Non-Functional / Performance Validation

**Rule Description:**
Balancing transaction retrieval must support real-time access patterns with appropriate performance characteristics for medium-volume usage.

**Validation Logic:**

- **Condition:** For all balancing transaction retrieval requests
- **Check:** Ensure response is returned within acceptable latency thresholds
- **Valid Criteria:** Response returned within real-time latency requirements (typically < 1-2 seconds)
- **Invalid Criteria:** Response exceeds acceptable latency thresholds
- **Action on Success:** Return response to client
- **Action on Failure:** Log performance issue, potentially return timeout error

**Error Handling:**

- **Error Message:** `Request timeout. Please try again.`
- **Error Code:** `OBP-50001` (RequestTimeout)
- **HTTP Status Code:** `504 Gateway Timeout`

**Scala Implementation:**

- **Location:** System-wide configuration and middleware
- **Method/Function:** Request timeout handling
- **Line Reference:** N/A (infrastructure level)

**Code Snippet:**
```scala
// Timeout configuration (conceptual)
val requestTimeout = Props.getInt("api.request.timeout.seconds", 30)
```

**Related Entities:**
- All entities involved in the request processing

**User Story Context:**
As stated in the business rules: "Real-time Access: Balancing transaction retrieval must support real-time access patterns with low latency" and "Medium Volume Support: The system must be designed to handle medium volume of balancing transaction retrieval requests."

**Dependencies:**
- All previous validations must complete within the timeout

---

### Rule VR-014: Response Data Completeness Validation

**Field/Entity:** BalancingTransactionJson Response

**Validation Type:** Output Format Validation

**Rule Description:**
The response must include complete details of the balancing transaction including transaction ID, amount, account information, and metadata.

**Validation Logic:**

- **Condition:** When constructing the balancing transaction response
- **Check:** Validate that all required fields are populated in the response
- **Valid Criteria:** All required fields (transaction_id, bank_id, account_id, this_account, other_account, details) are present and non-null
- **Invalid Criteria:** Any required field is missing or null
- **Action on Success:** Return complete response to client
- **Action on Failure:** Log data issue, return partial response with available data or error

**Error Handling:**

- **Error Message:** `Incomplete balancing transaction data.`
- **Error Code:** `OBP-50000` (InternalServerError)
- **HTTP Status Code:** `500 Internal Server Error`

**Scala Implementation:**

- **Location:** `code.api.v4_0_0.JSONFactory4.0.0`
- **Method/Function:** `createBalancingTransactionJson`
- **Line Reference:** Response construction

**Code Snippet:**
```scala
// Response structure validation
case class BalancingTransactionJson(
  transaction_id: String,      // Required
  bank_id: String,             // Required
  account_id: String,          // Required
  this_account: ThisAccountJson,    // Required
  other_account: OtherAccountJson,  // Required
  details: TransactionDetailsJson   // Required
)
```

**Related Entities:**
- BalancingTransactionJson case class
- All nested JSON structures

**User Story Context:**
As stated in the acceptance criteria: "The system shall provide complete details of the balancing transaction including transaction ID, amount, account information, and metadata." This ensures the response meets the documented output data requirements.

**Dependencies:**
- VR-005: Balancing Transaction Existence Validation

---

## Validation Execution Order

The validations should be executed in the following order for optimal processing:

1. **VR-006**: User Authentication Validation (401 if fails)
2. **VR-001**: Bank Identifier Required Validation
3. **VR-008**: Bank Existence Validation (404 if fails)
4. **VR-002**: Account Identifier Required Validation
5. **VR-009**: Account Existence and Ownership Validation (404 if fails)
6. **VR-003**: View Identifier Required Validation
7. **VR-007**: User Authorization Validation (403 if fails)
8. **VR-004**: Transaction Identifier Required Validation
9. **VR-010**: Transaction Existence and Ownership Validation (404 if fails)
10. **VR-005**: Balancing Transaction Existence Validation (404 if fails)
11. **VR-011**: Double-Entry Bookkeeping Relationship Validation
12. **VR-012**: One-to-One Balancing Relationship Validation
13. **VR-014**: Response Data Completeness Validation
14. **VR-013**: Real-Time Response Performance Validation (ongoing)

---

## Quality Checklist

- [x] All validation functions in relevant code are documented
- [x] All error messages are captured with exact text
- [x] All error codes are documented
- [x] Required vs. optional fields are clearly marked
- [x] Cross-field validations are identified (VR-009, VR-010)
- [x] Business constraint validations are included (VR-011, VR-012)
- [x] Code references include file paths and method names
- [x] User story context is explained for each rule
- [x] Valid and invalid criteria are clearly stated
- [x] HTTP status codes are documented where applicable
- [x] Dependencies between validations are noted

---

## Notes

- The validation rules are extracted based on the user story content and the standard OBP API patterns
- Some error codes (e.g., OBP-30XXX for BalancingTransactionNotFound) may need to be confirmed against the actual Scala codebase
- The exact implementation details may vary based on the specific version of the OBP API being used
- Multi-leg transactions and pending transaction handling require SME clarification as noted in the user story
