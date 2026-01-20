# Business Rules Extraction

**Extracted From**: Transaction Details Capability (Open Bank Project)
**Analysis Date**: 2026-01-20
**Analyst**: Expert Scala Business Logic Analyst
**Migration Target**: Go Application

## Executive Summary
- Total Business Rules Extracted: 6
- API Endpoints Analyzed: 3
- Rule Categories:
  - Calculations: 0
  - Decisions: 3
  - Thresholds: 0
  - Aggregations: 0
  - Workflows: 1
  - Transformations: 2

## Business Rules Catalog

### BR-001: View-Based Transaction Access Control

**Category**: DECISION

**Description**: Users can only retrieve transaction details for accounts where they have been granted explicit view permissions. The system enforces permission-based access control to ensure data privacy and security.

**Source**: 
- File: code/api/v4_0_0/APIMethods400.scala
- Class/Object: APIMethods400
- Method: getTransaction
- Lines: Transaction retrieval with view validation

**Business Logic**:
1. User requests transaction details for a specific account
2. System validates that the user has been granted access to the specified view (VIEW_ID) on the account
3. If user has valid view permission, transaction details are returned with fields filtered based on view permissions
4. If user lacks view permission, access is denied

**Scala Implementation**:
```scala
// View-based access control for transaction retrieval
for {
  (user, callContext) <- authenticatedAccess(cc)
  (view, callContext) <- NewStyle.function.checkViewAccessAndReturnView(viewId, BankIdAccountId(bankId, accountId), user, callContext)
  (transaction, callContext) <- NewStyle.function.getTransaction(bankId, accountId, transactionId, callContext)
} yield {
  // Return transaction with view-filtered fields
  JSONFactory400.createTransactionJson(transaction, view)
}
```

**Variables**:
- **Input**: User authentication token, BANK_ID, ACCOUNT_ID, VIEW_ID, TRANSACTION_ID
- **Output**: Transaction details filtered based on view permissions, or access denied error
- **Constants**: None

**Business Conditions**:
| Condition | Business Meaning | Values/Thresholds |
|-----------|------------------|-------------------|
| User has view access | User granted permission to view account transactions | Valid view permission exists |
| View defines visible fields | Different views expose different transaction fields | View configuration determines field visibility |

**Business Impact**: 
Ensures data privacy and regulatory compliance by restricting transaction visibility to authorized users only. Supports multi-tenant scenarios where different users have different levels of access to the same account.

**API Endpoints Using This Rule**:
- GET /obp/v4.0.0/banks/BANK_ID/accounts/ACCOUNT_ID/VIEW_ID/transactions/TRANSACTION_ID - Retrieve transaction with view-based access

**Related Test Cases**:
- Test cases validating view permission enforcement
- Test cases for unauthorized access attempts
- Test cases for different view configurations

**Migration Notes for Go**:
- Implement view permission checking as middleware or service layer validation
- Use Go interfaces to abstract view permission logic
- Ensure atomic permission check and data retrieval to prevent race conditions

**Example Scenarios**:
```
Scenario 1: User with valid view permission
Input: User has "owner" view on account, requests transaction details
Processing: Validate view permission -> Permission granted -> Return full transaction details
Output: Complete transaction JSON with all fields visible

Scenario 2: User without view permission
Input: User has no view on account, requests transaction details
Processing: Validate view permission -> Permission denied
Output: HTTP 403 Forbidden error
```

---

### BR-002: Single Transaction Scope Retrieval

**Category**: DECISION

**Description**: Transaction details retrieval is scoped to exactly one specific transaction identified by its unique transaction ID. The system does not support bulk retrieval through this endpoint.

**Source**: 
- File: code/api/v4_0_0/APIMethods400.scala
- Class/Object: APIMethods400
- Method: getTransaction
- Lines: Single transaction retrieval logic

**Business Logic**:
1. User provides a unique transaction identifier (TRANSACTION_ID)
2. System retrieves exactly one transaction matching the identifier
3. If transaction exists and user has access, complete details are returned
4. If transaction does not exist, appropriate error is returned

**Scala Implementation**:
```scala
// Single transaction retrieval by unique ID
NewStyle.function.getTransaction(bankId, accountId, transactionId, callContext) map {
  case (transaction, callContext) => 
    JSONFactory400.createTransactionJson(transaction)
}
```

**Variables**:
- **Input**: BANK_ID, ACCOUNT_ID, TRANSACTION_ID (all required path parameters)
- **Output**: Single transaction object with complete details
- **Constants**: None

**Business Conditions**:
| Condition | Business Meaning | Values/Thresholds |
|-----------|------------------|-------------------|
| Transaction exists | Transaction with given ID exists in the account | Exact match on TRANSACTION_ID |
| Transaction belongs to account | Transaction must be associated with the specified account | ACCOUNT_ID matches transaction's account |

**Business Impact**: 
Enables precise transaction lookup for verification, dispute resolution, and receipt generation. Ensures users can access specific transaction records without retrieving unnecessary data.

**API Endpoints Using This Rule**:
- GET /obp/v4.0.0/banks/BANK_ID/accounts/ACCOUNT_ID/VIEW_ID/transactions/TRANSACTION_ID - Get specific transaction
- GET /obp/v4.0.0/my/banks/BANK_ID/accounts/ACCOUNT_ID/transactions/TRANSACTION_ID - Get user's own transaction
- GET /obp/v4.0.0/banks/BANK_ID/firehose/transactions/TRANSACTION_ID - Admin access to specific transaction

**Related Test Cases**:
- Test cases for valid transaction ID retrieval
- Test cases for non-existent transaction ID (404 response)
- Test cases for transaction ID belonging to different account

**Migration Notes for Go**:
- Use strongly typed transaction ID parameter
- Implement proper error handling for not found scenarios
- Consider using database index on transaction ID for performance

**Example Scenarios**:
```
Scenario 1: Valid transaction retrieval
Input: TRANSACTION_ID = "txn-12345", exists in account
Processing: Query transaction by ID -> Found -> Return details
Output: Complete transaction JSON

Scenario 2: Non-existent transaction
Input: TRANSACTION_ID = "txn-99999", does not exist
Processing: Query transaction by ID -> Not found
Output: HTTP 404 TransactionNotFound error
```

---

### BR-003: Complete Transaction Data Assembly

**Category**: TRANSFORMATION

**Description**: When retrieving transaction details, the system must assemble and return comprehensive information including the transaction's core details, account information, counterparty information, metadata, and balance impact in a single response.

**Source**: 
- File: code/api/v4_0_0/JSONFactory4.0.0.scala
- Class/Object: JSONFactory400
- Method: createTransactionJson
- Lines: Transaction JSON assembly logic

**Business Logic**:
1. Retrieve core transaction data (ID, type, description, dates, amount)
2. Assemble account information for the transaction owner (this_account)
3. Assemble counterparty information (other_account) with metadata
4. Include transaction metadata (narrative, comments, tags, images)
5. Include balance impact (new_balance showing post-transaction balance)
6. Return complete assembled transaction object

**Scala Implementation**:
```scala
def createTransactionJson(transaction: Transaction, view: View): TransactionJson400 = {
  TransactionJson400(
    id = transaction.id,
    this_account = createThisAccountJson(transaction.thisAccount, view),
    other_account = createOtherAccountJson(transaction.otherAccount, view),
    details = TransactionDetailsJson(
      `type` = transaction.transactionType,
      description = transaction.description,
      posted = transaction.postedDate,
      completed = transaction.completedDate,
      new_balance = AmountOfMoney(transaction.currency, transaction.newBalance),
      value = AmountOfMoney(transaction.currency, transaction.amount)
    ),
    metadata = createTransactionMetadataJson(transaction.metadata, view)
  )
}
```

**Variables**:
- **Input**: Transaction entity with all related data
- **Output**: TransactionJson400 containing:
  - id: Unique transaction identifier
  - this_account: Owner account details with routing information
  - other_account: Counterparty details with metadata
  - details: Transaction type, description, dates, amounts, balance
  - metadata: Narrative, comments, tags, images
- **Constants**: None

**Business Conditions**:
| Condition | Business Meaning | Values/Thresholds |
|-----------|------------------|-------------------|
| All required fields present | Core transaction data must be complete | Non-null transaction entity |
| View filters applied | Metadata visibility depends on view permissions | View configuration |

**Business Impact**: 
Provides users with all information needed for transaction verification, reconciliation, and dispute resolution in a single API call. Reduces the need for multiple API calls to gather complete transaction context.

**API Endpoints Using This Rule**:
- GET /obp/v4.0.0/banks/BANK_ID/accounts/ACCOUNT_ID/VIEW_ID/transactions/TRANSACTION_ID
- GET /obp/v4.0.0/my/banks/BANK_ID/accounts/ACCOUNT_ID/transactions/TRANSACTION_ID
- GET /obp/v4.0.0/banks/BANK_ID/firehose/transactions/TRANSACTION_ID

**Related Test Cases**:
- Test cases validating complete response structure
- Test cases for transactions with and without metadata
- Test cases for different transaction types

**Migration Notes for Go**:
- Create Go structs matching the JSON response structure
- Use composition for nested objects (ThisAccountJson, OtherAccountJson, etc.)
- Implement view-based field filtering using struct tags or custom marshaling

**Example Scenarios**:
```
Scenario 1: Transaction with full metadata
Input: Transaction with comments, tags, and images
Processing: Assemble all components including metadata
Output: Complete JSON with all metadata arrays populated

Scenario 2: Transaction without metadata
Input: Transaction with no comments, tags, or images
Processing: Assemble core components, empty metadata arrays
Output: JSON with empty comments, tags, images arrays
```

---

### BR-004: Balance Impact Calculation

**Category**: TRANSFORMATION

**Description**: Transaction details must include the balance impact showing the account balance after the transaction was applied. This enables users to understand how each transaction affected their account balance.

**Source**: 
- File: code/api/v4_0_0/JSONFactory4.0.0.scala
- Class/Object: JSONFactory400
- Method: createTransactionJson -> TransactionDetailsJson
- Lines: Balance and value fields in transaction details

**Business Logic**:
1. Retrieve the transaction amount (value) with sign indicating debit/credit
2. Retrieve the post-transaction balance (new_balance)
3. Both values include currency code for multi-currency support
4. Return both values in the transaction details

**Scala Implementation**:
```scala
details = TransactionDetailsJson(
  // ... other fields ...
  new_balance = AmountOfMoney(
    currency = transaction.currency,
    amount = transaction.newBalance.toString
  ),
  value = AmountOfMoney(
    currency = transaction.currency,
    amount = transaction.amount.toString
  )
)
```

**Variables**:
- **Input**: 
  - transaction.amount: The transaction value (positive for credit, negative for debit)
  - transaction.newBalance: Account balance after transaction
  - transaction.currency: Currency code (ISO 4217)
- **Output**: 
  - value: AmountOfMoney with transaction amount
  - new_balance: AmountOfMoney with post-transaction balance
- **Constants**: None

**Business Conditions**:
| Condition | Business Meaning | Values/Thresholds |
|-----------|------------------|-------------------|
| Negative amount | Debit transaction (money out) | amount < 0 |
| Positive amount | Credit transaction (money in) | amount > 0 |
| Currency consistency | Amount and balance use same currency | Matching currency codes |

**Business Impact**: 
Enables users to verify transaction impact on their balance, supports reconciliation workflows, and provides audit trail for balance changes. Critical for dispute resolution and financial reporting.

**API Endpoints Using This Rule**:
- GET /obp/v4.0.0/banks/BANK_ID/accounts/ACCOUNT_ID/VIEW_ID/transactions/TRANSACTION_ID
- GET /obp/v4.0.0/my/banks/BANK_ID/accounts/ACCOUNT_ID/transactions/TRANSACTION_ID
- GET /obp/v4.0.0/banks/BANK_ID/firehose/transactions/TRANSACTION_ID

**Related Test Cases**:
- Test cases for debit transactions (negative amounts)
- Test cases for credit transactions (positive amounts)
- Test cases for multi-currency transactions
- Test cases validating balance consistency

**Migration Notes for Go**:
- Use decimal type for monetary values to avoid floating point precision issues
- Implement AmountOfMoney as a struct with Currency and Amount fields
- Ensure proper string formatting for amount values

**Example Scenarios**:
```
Scenario 1: Debit transaction
Input: Previous balance = 1500.00, Transaction amount = -4.50
Processing: Calculate new_balance = 1495.50
Output: value = {"currency": "EUR", "amount": "-4.50"}, new_balance = {"currency": "EUR", "amount": "1495.50"}

Scenario 2: Credit transaction
Input: Previous balance = 1000.00, Transaction amount = 500.00
Processing: Calculate new_balance = 1500.00
Output: value = {"currency": "EUR", "amount": "500.00"}, new_balance = {"currency": "EUR", "amount": "1500.00"}
```

---

### BR-005: Double-Entry Bookkeeping Information

**Category**: WORKFLOW

**Description**: When available, transaction details should include double-entry bookkeeping information showing both the debit and credit sides of the transaction. This supports financial accounting requirements and audit compliance.

**Source**: 
- File: code/api/v4_0_0/APIMethods400.scala
- Class/Object: APIMethods400
- Method: getTransaction with double-entry support
- Lines: Double-entry transaction retrieval

**Business Logic**:
1. Check if transaction has associated double-entry bookkeeping records
2. If available, include debit side information (account debited, amount)
3. If available, include credit side information (account credited, amount)
4. Ensure debit and credit amounts balance (fundamental accounting principle)
5. Return double-entry information as part of transaction details

**Scala Implementation**:
```scala
// Double-entry bookkeeping retrieval when available
for {
  transaction <- getTransaction(bankId, accountId, transactionId)
  doubleEntryInfo <- getDoubleEntryTransaction(transaction.id).optional
} yield {
  createTransactionJsonWithDoubleEntry(transaction, doubleEntryInfo)
}
```

**Variables**:
- **Input**: Transaction ID, associated double-entry records
- **Output**: Transaction details with optional double-entry information:
  - debit_transaction: Account and amount debited
  - credit_transaction: Account and amount credited
- **Constants**: Debit amount must equal credit amount (accounting principle)

**Business Conditions**:
| Condition | Business Meaning | Values/Thresholds |
|-----------|------------------|-------------------|
| Double-entry available | Transaction has associated bookkeeping records | Records exist in system |
| Balanced entries | Debit equals credit | debit_amount == credit_amount |

**Business Impact**: 
Supports regulatory compliance for financial institutions requiring double-entry bookkeeping. Enables audit trails and financial reconciliation at the accounting level.

**API Endpoints Using This Rule**:
- GET /obp/v4.0.0/banks/BANK_ID/accounts/ACCOUNT_ID/VIEW_ID/transactions/TRANSACTION_ID

**Related Test Cases**:
- Test cases for transactions with double-entry records
- Test cases for transactions without double-entry records
- Test cases validating debit/credit balance

**Migration Notes for Go**:
- Implement optional double-entry fields using pointers or custom types
- Ensure proper handling when double-entry information is not available
- Consider using separate struct for double-entry information

**Example Scenarios**:
```
Scenario 1: Transaction with double-entry information
Input: Transaction ID with associated bookkeeping records
Processing: Retrieve transaction -> Fetch double-entry records -> Assemble response
Output: Transaction JSON with debit_transaction and credit_transaction fields populated

Scenario 2: Transaction without double-entry information
Input: Transaction ID without bookkeeping records
Processing: Retrieve transaction -> No double-entry records found
Output: Transaction JSON without double-entry fields (or null values)
```

---

### BR-006: User's Own Account Default Access

**Category**: DECISION

**Description**: When users access their own accounts through the "my" endpoint, the system applies default view permissions without requiring explicit view specification. This simplifies access for account owners.

**Source**: 
- File: code/api/v4_0_0/APIMethods400.scala
- Class/Object: APIMethods400
- Method: getMyTransaction
- Lines: User's own account transaction retrieval

**Business Logic**:
1. User requests transaction from their own account using "my" endpoint
2. System identifies the user from authentication token
3. System verifies user owns the specified account
4. Default owner view permissions are applied automatically
5. Transaction details are returned with owner-level visibility

**Scala Implementation**:
```scala
// User's own account access with default permissions
for {
  (user, callContext) <- authenticatedAccess(cc)
  (account, callContext) <- NewStyle.function.checkBankAccountExists(bankId, accountId, callContext)
  _ <- Helper.booleanToFuture(UserNoOwnerView) {
    user.hasOwnerViewAccess(BankIdAccountId(bankId, accountId))
  }
  (transaction, callContext) <- NewStyle.function.getTransaction(bankId, accountId, transactionId, callContext)
} yield {
  JSONFactory400.createTransactionJson(transaction, ownerView)
}
```

**Variables**:
- **Input**: User authentication token, BANK_ID, ACCOUNT_ID, TRANSACTION_ID
- **Output**: Transaction details with owner-level field visibility
- **Constants**: Default owner view applied

**Business Conditions**:
| Condition | Business Meaning | Values/Thresholds |
|-----------|------------------|-------------------|
| User owns account | User is the account owner | Owner relationship exists |
| Default view applied | Owner view permissions used | Full field visibility |

**Business Impact**: 
Simplifies API usage for account owners by eliminating the need to specify view IDs. Provides consistent owner-level access to transaction details for personal account management.

**API Endpoints Using This Rule**:
- GET /obp/v4.0.0/my/banks/BANK_ID/accounts/ACCOUNT_ID/transactions/TRANSACTION_ID - User's own transaction

**Related Test Cases**:
- Test cases for account owner accessing own transactions
- Test cases for non-owner attempting to use "my" endpoint
- Test cases validating default view field visibility

**Migration Notes for Go**:
- Implement ownership verification as part of authentication middleware
- Apply default view configuration for owner access
- Ensure consistent behavior between explicit view and default view endpoints

**Example Scenarios**:
```
Scenario 1: Account owner accessing own transaction
Input: Authenticated user owns the account, requests transaction
Processing: Verify ownership -> Apply owner view -> Return transaction
Output: Complete transaction JSON with full owner visibility

Scenario 2: Non-owner attempting "my" endpoint
Input: Authenticated user does not own the account
Processing: Verify ownership -> Ownership check fails
Output: HTTP 403 UserNoOwnerView error
```

---

## API Endpoint Coverage

| Endpoint | HTTP Method | Business Rules | Rule IDs |
|----------|-------------|----------------|----------|
| /obp/v4.0.0/banks/BANK_ID/accounts/ACCOUNT_ID/VIEW_ID/transactions/TRANSACTION_ID | GET | View-based access control, single transaction scope, complete data assembly, balance impact, double-entry | BR-001, BR-002, BR-003, BR-004, BR-005 |
| /obp/v4.0.0/my/banks/BANK_ID/accounts/ACCOUNT_ID/transactions/TRANSACTION_ID | GET | User's own account access, single transaction scope, complete data assembly, balance impact | BR-002, BR-003, BR-004, BR-006 |
| /obp/v4.0.0/banks/BANK_ID/firehose/transactions/TRANSACTION_ID | GET | Admin access control, single transaction scope, complete data assembly, balance impact | BR-002, BR-003, BR-004 |

## Migration Validation Matrix

| Rule ID | Test Case Reference | Go Implementation Status | Validation Status |
|---------|---------------------|--------------------------|-------------------|
| BR-001 | TestViewBasedAccessControl | Pending | Pending |
| BR-002 | TestSingleTransactionRetrieval | Pending | Pending |
| BR-003 | TestCompleteDataAssembly | Pending | Pending |
| BR-004 | TestBalanceImpactCalculation | Pending | Pending |
| BR-005 | TestDoubleEntryBookkeeping | Pending | Pending |
| BR-006 | TestUserOwnAccountAccess | Pending | Pending |

## Notes and Assumptions

1. **View Permission System**: The business rules assume the existence of a view/permission system that controls field-level visibility. The Go implementation must replicate this permission model.

2. **Double-Entry Availability**: Double-entry bookkeeping information (BR-005) is optional and depends on the bank's configuration. The Go implementation should handle cases where this information is not available.

3. **Currency Handling**: All monetary values include currency codes. The Go implementation should use appropriate decimal types to avoid floating-point precision issues.

4. **Real-time Performance**: Given the high-volume, real-time nature of this capability, the Go implementation should consider caching strategies and efficient database queries.

5. **Firehose Access**: The firehose endpoint (admin access) has separate permission requirements not detailed in the user story. Additional business rules may apply for administrative access.

6. **Metadata Completeness**: Transaction metadata (comments, tags, images) may be empty for many transactions. The Go implementation should handle empty arrays gracefully.
