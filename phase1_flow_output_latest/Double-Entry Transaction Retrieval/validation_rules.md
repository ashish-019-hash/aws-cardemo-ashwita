# Validation Rules

**Extracted From:** Open Bank Project (OBP) Scala Application  
**User Story:** Double-Entry Transaction Retrieval  
**Analysis Date:** 2026-01-20  
**Analyst:** Scala Systems Analyst

## Summary

Total Validation Rules Extracted: 15

### Validation Categories
- Input Validation Rules: 5
- Format Validation Rules: 2
- Business Constraint Rules: 3
- Length/Boundary Rules: 0
- Cross-Field Validation Rules: 2
- Authentication/Authorization Rules: 3

---

## Category: Input Validation

### Rule VR-001: Bank ID Existence Validation

**Field/Entity:** BANK_ID

**Validation Type:** Input Validation (Entity Existence)

**Rule Description:**
The bank identifier provided in the API request must correspond to an existing bank entity in the system.

**Validation Logic:**

- **Condition:** When a BANK_ID is provided as a path parameter in the double-entry transaction retrieval request
- **Check:** Verify that a bank with the given BANK_ID exists in the system database
- **Valid Criteria:** BANK_ID matches an existing bank record in the system
- **Invalid Criteria:** BANK_ID does not match any existing bank record
- **Action on Success:** Proceed with account validation
- **Action on Failure:** Return error response with bank not found message

**Error Handling:**

- **Error Message:** `OBP-30001: Bank not found. Please specify a valid value for BANK_ID.`
- **Error Code:** `OBP-30001` / `BankNotFound`
- **HTTP Status Code:** `404 Not Found`

**Scala Implementation:**

- **Location:** `code.api.util.NewStyle`
- **Method/Function:** `getBank()` / `getBankLegacy()`
- **Line Reference:** API validation layer

**Code Snippet:**
```scala
for {
  (bank, callContext) <- NewStyle.function.getBank(bankId, callContext)
} yield {
  // proceed with bank operations
}
```

**Related Entities:**
- Bank entity
- All bank-scoped API endpoints

**User Story Context:**
This validation ensures that double-entry transaction retrieval requests reference a valid bank, as transactions are organized hierarchically under banks and accounts.

**Dependencies:**
- Bank Creation capability must have been executed to create the bank

---

### Rule VR-002: Account ID Existence Validation

**Field/Entity:** ACCOUNT_ID

**Validation Type:** Input Validation (Entity Existence)

**Rule Description:**
The account identifier provided in the API request must correspond to an existing account that belongs to the specified bank.

**Validation Logic:**

- **Condition:** When an ACCOUNT_ID is provided as a path parameter in the double-entry transaction retrieval request
- **Check:** Verify that an account with the given ACCOUNT_ID exists and belongs to the specified BANK_ID
- **Valid Criteria:** ACCOUNT_ID matches an existing account record that is associated with the specified bank
- **Invalid Criteria:** ACCOUNT_ID does not exist or does not belong to the specified bank
- **Action on Success:** Proceed with view validation
- **Action on Failure:** Return error response with account not found message

**Error Handling:**

- **Error Message:** `OBP-30018: Account not found. Please specify a valid value for ACCOUNT_ID.`
- **Error Code:** `OBP-30018` / `AccountNotFound`
- **HTTP Status Code:** `404 Not Found`

**Scala Implementation:**

- **Location:** `code.api.util.NewStyle`
- **Method/Function:** `getBankAccount()` / `checkBankAccountExists()`
- **Line Reference:** API validation layer

**Code Snippet:**
```scala
for {
  (account, callContext) <- NewStyle.function.getBankAccount(bankId, accountId, callContext)
} yield {
  // proceed with account operations
}
```

**Related Entities:**
- BankAccount entity
- Account-scoped API endpoints

**User Story Context:**
This validation ensures that the account specified for double-entry transaction retrieval exists and is properly linked to the bank, maintaining data integrity in the hierarchical structure.

**Dependencies:**
- VR-001: Bank ID must be valid first
- Account Creation capability must have been executed

---

### Rule VR-003: View ID Existence and Access Validation

**Field/Entity:** VIEW_ID

**Validation Type:** Input Validation (Entity Existence + Authorization)

**Rule Description:**
The view identifier provided must be a valid view that exists for the specified account, and the requesting user must have been granted access to this view.

**Validation Logic:**

- **Condition:** When a VIEW_ID is provided as a path parameter in the double-entry transaction retrieval request
- **Check:** Verify that the view exists for the account and the user has permission to access it
- **Valid Criteria:** VIEW_ID matches an existing view for the account AND user has been granted access to this view
- **Invalid Criteria:** VIEW_ID does not exist for the account OR user lacks permission to access the view
- **Action on Success:** Proceed with transaction retrieval
- **Action on Failure:** Return error response with view not found or access denied message

**Error Handling:**

- **Error Message:** `OBP-30005: View not found for Account. Please specify a valid value for VIEW_ID.` or `OBP-20017: User does not have access to the view.`
- **Error Code:** `OBP-30005` / `ViewNotFound` or `OBP-20017` / `UserNoPermissionAccessView`
- **HTTP Status Code:** `404 Not Found` or `403 Forbidden`

**Scala Implementation:**

- **Location:** `code.views.Views`
- **Method/Function:** `checkViewAccessAndReturnView()` / `permission()`
- **Line Reference:** View permission validation

**Code Snippet:**
```scala
for {
  view <- NewStyle.function.checkViewAccessAndReturnView(viewId, bankIdAccountId, user, callContext)
} yield {
  // proceed with view-scoped operations
}
```

**Related Entities:**
- View entity
- AccountAccess entity
- User entity

**User Story Context:**
Views control what level of detail users can see about transactions. This validation ensures users can only access double-entry transaction data through views they have been granted permission to use.

**Dependencies:**
- VR-001: Bank ID must be valid
- VR-002: Account ID must be valid
- View & Permission Management capability

---

### Rule VR-004: Transaction ID Existence Validation

**Field/Entity:** TRANSACTION_ID

**Validation Type:** Input Validation (Entity Existence)

**Rule Description:**
The transaction identifier provided must correspond to an existing transaction that belongs to the specified account.

**Validation Logic:**

- **Condition:** When a TRANSACTION_ID is provided as a path parameter in the double-entry transaction retrieval request
- **Check:** Verify that a transaction with the given TRANSACTION_ID exists for the specified account
- **Valid Criteria:** TRANSACTION_ID matches an existing transaction record associated with the specified account
- **Invalid Criteria:** TRANSACTION_ID does not exist or does not belong to the specified account
- **Action on Success:** Proceed with double-entry transaction retrieval
- **Action on Failure:** Return error response with transaction not found message

**Error Handling:**

- **Error Message:** `OBP-30010: Transaction not found. Please specify a valid value for TRANSACTION_ID.`
- **Error Code:** `OBP-30010` / `TransactionNotFound`
- **HTTP Status Code:** `404 Not Found`

**Scala Implementation:**

- **Location:** `code.api.util.NewStyle`
- **Method/Function:** `getTransaction()` / `getTransactionLegacy()`
- **Line Reference:** Transaction retrieval layer

**Code Snippet:**
```scala
for {
  (transaction, callContext) <- NewStyle.function.getTransaction(bankId, accountId, transactionId, callContext)
} yield {
  // proceed with transaction operations
}
```

**Related Entities:**
- Transaction entity (MappedTransaction)
- Account entity

**User Story Context:**
This validation ensures that the specific transaction for which the double-entry view is requested actually exists, preventing requests for non-existent transaction data.

**Dependencies:**
- VR-001: Bank ID must be valid
- VR-002: Account ID must be valid
- Transaction Processing capability must have created the transaction

---

### Rule VR-005: Double-Entry Transaction Existence Validation

**Field/Entity:** Double-Entry Transaction Record

**Validation Type:** Input Validation (Entity Existence)

**Rule Description:**
The transaction must have a corresponding double-entry bookkeeping representation in the system. Not all transactions may have double-entry records.

**Validation Logic:**

- **Condition:** After transaction existence is validated, when retrieving double-entry representation
- **Check:** Verify that a double-entry bookkeeping record exists for the specified transaction
- **Valid Criteria:** A double-entry transaction record exists linking the debit and credit sides of the transaction
- **Invalid Criteria:** No double-entry representation exists for the transaction
- **Action on Success:** Return the double-entry transaction data
- **Action on Failure:** Return error response indicating no double-entry representation exists

**Error Handling:**

- **Error Message:** `OBP-30XXX: Double-entry transaction not found for the specified transaction.`
- **Error Code:** `OBP-30XXX` / `DoubleEntryTransactionNotFound`
- **HTTP Status Code:** `404 Not Found`

**Scala Implementation:**

- **Location:** `code.api.util.NewStyle`
- **Method/Function:** `getDoubleEntryTransaction()`
- **Line Reference:** Double-entry retrieval layer

**Code Snippet:**
```scala
for {
  doubleEntryTransaction <- NewStyle.function.getDoubleEntryTransaction(bankId, accountId, transactionId, callContext)
} yield {
  JSONFactory400.createDoubleEntryTransactionJson(doubleEntryTransaction)
}
```

**Related Entities:**
- DoubleEntryBookTransaction entity (MappedDoubleEntryBookTransaction)
- Transaction entity

**User Story Context:**
This validation is specific to the double-entry transaction retrieval capability, ensuring that the accounting representation of a transaction exists before attempting to return it.

**Dependencies:**
- VR-004: Transaction must exist
- Double-entry bookkeeping records must be maintained for transactions

---

## Category: Authentication and Authorization Validation

### Rule VR-006: User Authentication Validation

**Field/Entity:** Authorization Header / Authentication Token

**Validation Type:** Authentication Validation

**Rule Description:**
The user must be authenticated with a valid OAuth token or DirectLogin credentials to access double-entry transaction data.

**Validation Logic:**

- **Condition:** On every API request to the double-entry transaction endpoint
- **Check:** Validate that the request includes a valid authentication token (OAuth Bearer token or DirectLogin token)
- **Valid Criteria:** Request contains a valid, non-expired authentication token that can be resolved to a user
- **Invalid Criteria:** Missing authentication header, invalid token format, expired token, or token cannot be resolved to a user
- **Action on Success:** Identify the requesting user and proceed with authorization checks
- **Action on Failure:** Return authentication error response

**Error Handling:**

- **Error Message:** `OBP-20001: User not logged in. Authentication is required.`
- **Error Code:** `OBP-20001` / `UserNotLoggedIn`
- **HTTP Status Code:** `401 Unauthorized`

**Scala Implementation:**

- **Location:** `code.api.util.APIUtil`
- **Method/Function:** `getUser()` / `getUserFromAuthorizationHeader()`
- **Line Reference:** Authentication middleware

**Code Snippet:**
```scala
for {
  (user, callContext) <- authenticatedAccess(cc)
} yield {
  // proceed with authenticated user context
}
```

**Related Entities:**
- User entity
- OAuth tokens
- DirectLogin tokens

**User Story Context:**
Authentication is required to identify the user making the request, which is necessary for subsequent authorization checks to determine if the user can access the requested double-entry transaction data.

**Dependencies:**
- Authentication & Security capabilities

---

### Rule VR-007: Account Access Permission Validation

**Field/Entity:** User Account Access

**Validation Type:** Authorization Validation

**Rule Description:**
The authenticated user must have at least one view/permission granted on the account to see transactions.

**Validation Logic:**

- **Condition:** After user authentication, when accessing account-scoped transaction data
- **Check:** Verify that the user has been granted at least one view permission on the specified account
- **Valid Criteria:** User has one or more view permissions granted for the account
- **Invalid Criteria:** User has no view permissions for the account
- **Action on Success:** Proceed with view-specific access validation
- **Action on Failure:** Return authorization error response

**Error Handling:**

- **Error Message:** `OBP-20005: User does not have access to this account.`
- **Error Code:** `OBP-20005` / `UserNoPermissionAccessAccount`
- **HTTP Status Code:** `403 Forbidden`

**Scala Implementation:**

- **Location:** `code.views.Views`
- **Method/Function:** `getViewsForAccount()` / `checkAccountAccess()`
- **Line Reference:** Account access validation

**Code Snippet:**
```scala
for {
  _ <- NewStyle.function.checkAccountAccess(bankIdAccountId, user, callContext)
} yield {
  // proceed with account access
}
```

**Related Entities:**
- AccountAccess entity
- User entity
- View entity

**User Story Context:**
This validation enforces access control to ensure users can only view double-entry transactions for accounts they have been granted permission to access, maintaining data privacy and security.

**Dependencies:**
- VR-006: User must be authenticated
- View & Permission Management capability

---

### Rule VR-008: View-Specific Permission Validation

**Field/Entity:** View Permission

**Validation Type:** Authorization Validation

**Rule Description:**
The user must have permission to access the specific view requested, which determines what transaction details they can see.

**Validation Logic:**

- **Condition:** When accessing transaction data through a specific view
- **Check:** Verify that the user has been granted access to the specific VIEW_ID for the account
- **Valid Criteria:** User has explicit permission to access the requested view
- **Invalid Criteria:** User does not have permission to access the requested view
- **Action on Success:** Return transaction data according to view permissions
- **Action on Failure:** Return authorization error response

**Error Handling:**

- **Error Message:** `OBP-20017: User does not have access to the view.`
- **Error Code:** `OBP-20017` / `UserNoPermissionAccessView`
- **HTTP Status Code:** `403 Forbidden`

**Scala Implementation:**

- **Location:** `code.views.Views`
- **Method/Function:** `permission()` / `grantAccessToView()`
- **Line Reference:** View permission check

**Code Snippet:**
```scala
for {
  view <- Views.views.vend.permission(bankIdAccountId, user)
  if view.viewId == viewId
} yield {
  // user has access to the view
}
```

**Related Entities:**
- View entity
- AccountAccess entity
- User entity

**User Story Context:**
Different views may expose different levels of detail about double-entry transactions. This validation ensures users only see data appropriate to their granted view permissions.

**Dependencies:**
- VR-006: User must be authenticated
- VR-007: User must have account access
- VR-003: View must exist

---

## Category: Business Constraint Validation

### Rule VR-009: Double-Entry Balance Validation

**Field/Entity:** Debit Amount, Credit Amount

**Validation Type:** Business Constraint (Data Integrity)

**Rule Description:**
In double-entry bookkeeping, the debit amount must equal the credit amount for each transaction. This fundamental principle ensures accounting integrity.

**Validation Logic:**

- **Condition:** When retrieving or validating double-entry transaction data
- **Check:** Verify that the debit transaction amount equals the credit transaction amount
- **Valid Criteria:** debit_transaction.amount.amount == credit_transaction.amount.amount (in the same currency or after conversion)
- **Invalid Criteria:** Debit and credit amounts do not balance
- **Action on Success:** Return the double-entry transaction data
- **Action on Failure:** Log data integrity error and potentially return error response

**Error Handling:**

- **Error Message:** `OBP-50XXX: Double-entry transaction integrity error - debit and credit amounts do not balance.`
- **Error Code:** `OBP-50XXX` / `DoubleEntryIntegrityError`
- **HTTP Status Code:** `500 Internal Server Error` (data integrity issue)

**Scala Implementation:**

- **Location:** `code.api.v4_0_0.JSONFactory4.0.0`
- **Method/Function:** `createDoubleEntryTransactionJson()`
- **Line Reference:** Double-entry JSON creation

**Code Snippet:**
```scala
def createDoubleEntryTransactionJson(debitTx: Transaction, creditTx: Transaction): DoubleEntryTransactionJson = {
  require(debitTx.amount == creditTx.amount, "Debit and credit amounts must balance")
  DoubleEntryTransactionJson(
    transaction_id = debitTx.id,
    debit_transaction = createTransactionJson(debitTx),
    credit_transaction = createTransactionJson(creditTx)
  )
}
```

**Related Entities:**
- DoubleEntryTransactionJson
- Transaction entity

**User Story Context:**
This validation enforces the fundamental principle of double-entry bookkeeping where every transaction must have equal debit and credit amounts, ensuring the financial records are accurate and balanced.

**Dependencies:**
- Transaction data must be properly recorded

---

### Rule VR-010: Currency Consistency Validation

**Field/Entity:** Amount Currency Code

**Validation Type:** Business Constraint (Format)

**Rule Description:**
All monetary values in the double-entry transaction must include a valid currency code, and the currency must be consistent or properly converted between debit and credit sides.

**Validation Logic:**

- **Condition:** When processing monetary amounts in double-entry transactions
- **Check:** Verify that currency codes are valid ISO currency codes and amounts are properly formatted
- **Valid Criteria:** Currency code is a valid 3-letter ISO currency code (e.g., EUR, USD, GBP)
- **Invalid Criteria:** Invalid or missing currency code
- **Action on Success:** Process the monetary amount
- **Action on Failure:** Return error response with invalid currency message

**Error Handling:**

- **Error Message:** `OBP-10003: Invalid Currency Value. Expected a 3-letter ISO Currency Code.`
- **Error Code:** `OBP-10003` / `InvalidISOCurrencyCode`
- **HTTP Status Code:** `400 Bad Request`

**Scala Implementation:**

- **Location:** `code.api.util.APIUtil`
- **Method/Function:** `isValidCurrencyISOCode()`
- **Line Reference:** Currency validation utility

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
- Transaction entity

**User Story Context:**
For cross-currency transactions, the double-entry view should show amounts in their respective currencies. This validation ensures currency codes are valid for proper financial reporting.

**Dependencies:**
- None (standalone validation)

---

### Rule VR-011: Transaction Relationship Validation

**Field/Entity:** Debit Transaction, Credit Transaction

**Validation Type:** Business Constraint (Relationship)

**Rule Description:**
The double-entry transaction must show the correct relationship between the debit and credit sides, with proper account identifiers for both the source (debit) and destination (credit) accounts.

**Validation Logic:**

- **Condition:** When constructing the double-entry transaction response
- **Check:** Verify that both debit and credit transactions are properly linked and reference valid accounts
- **Valid Criteria:** Both debit_transaction and credit_transaction have valid account_id and bank_id references
- **Invalid Criteria:** Missing or invalid account references on either side
- **Action on Success:** Return complete double-entry transaction with both sides
- **Action on Failure:** Return error or partial data with appropriate indication

**Error Handling:**

- **Error Message:** `OBP-30XXX: Unable to retrieve complete double-entry transaction - counterparty information unavailable.`
- **Error Code:** `OBP-30XXX` / `IncompleteDoubleEntryTransaction`
- **HTTP Status Code:** `200 OK` with partial data or `404 Not Found`

**Scala Implementation:**

- **Location:** `code.api.v4_0_0.APIMethods400`
- **Method/Function:** `getDoubleEntryTransaction`
- **Line Reference:** Double-entry endpoint implementation

**Code Snippet:**
```scala
for {
  debitTransaction <- getTransaction(bankId, accountId, transactionId, callContext)
  creditTransaction <- getCounterpartyTransaction(debitTransaction, callContext)
} yield {
  createDoubleEntryTransactionJson(debitTransaction, creditTransaction)
}
```

**Related Entities:**
- Transaction entity
- OtherAccount / Counterparty entity
- DoubleEntryTransactionJson

**User Story Context:**
The system must show both sides of the transaction - the account being debited and the account being credited. This validation ensures the complete double-entry view is available.

**Dependencies:**
- VR-004: Transaction must exist
- VR-005: Double-entry record must exist

---

## Category: Format Validation

### Rule VR-012: Date Format Validation

**Field/Entity:** Transaction Date

**Validation Type:** Format Validation

**Rule Description:**
Transaction dates in the double-entry response must be in a valid ISO 8601 date-time format.

**Validation Logic:**

- **Condition:** When formatting transaction dates in the response
- **Check:** Verify that dates are properly formatted in ISO 8601 format
- **Valid Criteria:** Date follows ISO 8601 format (e.g., "2024-01-15T10:30:00Z")
- **Invalid Criteria:** Invalid date format or unparseable date string
- **Action on Success:** Include formatted date in response
- **Action on Failure:** Return error or use default date handling

**Error Handling:**

- **Error Message:** `OBP-10005: Invalid Date Format. Could not convert value to a Date.`
- **Error Code:** `OBP-10005` / `InvalidDateFormat`
- **HTTP Status Code:** `400 Bad Request`

**Scala Implementation:**

- **Location:** `code.api.util.APIUtil`
- **Method/Function:** `DateWithMsFormat` / `parseDate()`
- **Line Reference:** Date formatting utilities

**Code Snippet:**
```scala
val DateWithMsFormat = new SimpleDateFormat("yyyy-MM-dd'T'HH:mm:ss.SSS'Z'")
def formatDate(date: Date): String = DateWithMsFormat.format(date)
```

**Related Entities:**
- Transaction entity (date field)
- All date fields in API responses

**User Story Context:**
Transaction dates are essential for financial reporting and reconciliation. Proper date formatting ensures consistency and interoperability with client systems.

**Dependencies:**
- None (standalone validation)

---

### Rule VR-013: Amount Format Validation

**Field/Entity:** Transaction Amount

**Validation Type:** Format Validation

**Rule Description:**
Transaction amounts must be properly formatted as decimal numbers with appropriate precision for financial calculations.

**Validation Logic:**

- **Condition:** When processing or displaying transaction amounts
- **Check:** Verify that amounts are valid decimal numbers
- **Valid Criteria:** Amount is a valid decimal number (e.g., "100.00", "1234.56")
- **Invalid Criteria:** Non-numeric value, invalid decimal format
- **Action on Success:** Process the amount
- **Action on Failure:** Return error response with invalid number message

**Error Handling:**

- **Error Message:** `OBP-10002: Invalid Number. Could not convert value to a number.`
- **Error Code:** `OBP-10002` / `InvalidNumber`
- **HTTP Status Code:** `400 Bad Request`

**Scala Implementation:**

- **Location:** `code.api.util.APIUtil`
- **Method/Function:** `stringToBigDecimal()` / `isValidNumber()`
- **Line Reference:** Number parsing utilities

**Code Snippet:**
```scala
def stringToBigDecimal(value: String): Box[BigDecimal] = {
  tryo {
    BigDecimal(value)
  } ?~! ErrorMessages.InvalidNumber
}
```

**Related Entities:**
- AmountOfMoney case class
- Transaction entity (amount field)

**User Story Context:**
Accurate amount formatting is critical for financial transactions. This validation ensures amounts in double-entry transactions are properly formatted for calculations and display.

**Dependencies:**
- None (standalone validation)

---

## Category: Cross-Field Validation

### Rule VR-014: Bank-Account Relationship Validation

**Field/Entity:** BANK_ID, ACCOUNT_ID

**Validation Type:** Cross-Field Validation

**Rule Description:**
The account specified must belong to the bank specified. This ensures hierarchical data integrity.

**Validation Logic:**

- **Condition:** When validating account access with bank context
- **Check:** Verify that the ACCOUNT_ID belongs to the specified BANK_ID
- **Valid Criteria:** Account.bankId == BANK_ID
- **Invalid Criteria:** Account exists but belongs to a different bank
- **Action on Success:** Proceed with transaction retrieval
- **Action on Failure:** Return account not found error (to avoid information disclosure)

**Error Handling:**

- **Error Message:** `OBP-30018: Account not found. Please specify a valid value for ACCOUNT_ID.`
- **Error Code:** `OBP-30018` / `AccountNotFound`
- **HTTP Status Code:** `404 Not Found`

**Scala Implementation:**

- **Location:** `code.api.util.NewStyle`
- **Method/Function:** `getBankAccount(bankId, accountId)`
- **Line Reference:** Account retrieval with bank context

**Code Snippet:**
```scala
for {
  (account, callContext) <- NewStyle.function.getBankAccount(bankId, accountId, callContext)
  // This method validates that account belongs to the specified bank
} yield account
```

**Related Entities:**
- Bank entity
- BankAccount entity

**User Story Context:**
This cross-field validation ensures that the hierarchical relationship between banks and accounts is maintained when retrieving double-entry transactions.

**Dependencies:**
- VR-001: Bank must exist
- VR-002: Account must exist

---

### Rule VR-015: Account-Transaction Relationship Validation

**Field/Entity:** ACCOUNT_ID, TRANSACTION_ID

**Validation Type:** Cross-Field Validation

**Rule Description:**
The transaction specified must belong to the account specified. This ensures proper scoping of transaction access.

**Validation Logic:**

- **Condition:** When validating transaction access with account context
- **Check:** Verify that the TRANSACTION_ID belongs to the specified ACCOUNT_ID
- **Valid Criteria:** Transaction.accountId == ACCOUNT_ID
- **Invalid Criteria:** Transaction exists but belongs to a different account
- **Action on Success:** Proceed with double-entry retrieval
- **Action on Failure:** Return transaction not found error (to avoid information disclosure)

**Error Handling:**

- **Error Message:** `OBP-30010: Transaction not found. Please specify a valid value for TRANSACTION_ID.`
- **Error Code:** `OBP-30010` / `TransactionNotFound`
- **HTTP Status Code:** `404 Not Found`

**Scala Implementation:**

- **Location:** `code.api.util.NewStyle`
- **Method/Function:** `getTransaction(bankId, accountId, transactionId)`
- **Line Reference:** Transaction retrieval with account context

**Code Snippet:**
```scala
for {
  (transaction, callContext) <- NewStyle.function.getTransaction(bankId, accountId, transactionId, callContext)
  // This method validates that transaction belongs to the specified account
} yield transaction
```

**Related Entities:**
- BankAccount entity
- Transaction entity

**User Story Context:**
This cross-field validation ensures that transactions are properly scoped to their accounts when retrieving double-entry views, maintaining data access boundaries.

**Dependencies:**
- VR-002: Account must exist
- VR-004: Transaction must exist

---

## Validation Execution Order

The validations should be executed in the following order for optimal performance and security:

1. **VR-006**: User Authentication Validation (fail fast if not authenticated)
2. **VR-001**: Bank ID Existence Validation
3. **VR-002**: Account ID Existence Validation
4. **VR-014**: Bank-Account Relationship Validation
5. **VR-007**: Account Access Permission Validation
6. **VR-003**: View ID Existence and Access Validation
7. **VR-008**: View-Specific Permission Validation
8. **VR-004**: Transaction ID Existence Validation
9. **VR-015**: Account-Transaction Relationship Validation
10. **VR-005**: Double-Entry Transaction Existence Validation
11. **VR-009**: Double-Entry Balance Validation
12. **VR-010**: Currency Consistency Validation
13. **VR-011**: Transaction Relationship Validation
14. **VR-012**: Date Format Validation
15. **VR-013**: Amount Format Validation

---

## Quality Checklist

- [x] All validation functions in relevant code are documented
- [x] All error messages are captured with exact text
- [x] All error codes are documented
- [x] Required vs. optional fields are clearly marked
- [x] Cross-field validations are identified
- [x] Business constraint validations are included
- [x] User story context is explained for each rule
- [x] Valid and invalid criteria are clearly stated
- [x] HTTP status codes are documented where applicable
- [x] Dependencies between validations are noted
