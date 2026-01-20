# Business Rules Extraction

**Extracted From**: Double-Entry Transaction Retrieval User Story
**Analysis Date**: 2026-01-20
**Analyst**: Expert Scala Business Logic Analyst
**Migration Target**: Go Application

## Executive Summary
- Total Business Rules Extracted: 7
- API Endpoints Analyzed: 2
- Rule Categories:
  - Calculations: 1
  - Decisions: 2
  - Thresholds: 0
  - Aggregations: 0
  - Workflows: 2
  - Transformations: 2

## Business Rules Catalog

### BR-001: Double-Entry Bookkeeping Principle

**Category**: WORKFLOW

**Description**: Every financial transaction in the system must have both a debit side and a credit side, representing the fundamental principle of double-entry bookkeeping where every transaction affects at least two accounts.

**Source**: 
- File: code/api/v4_0_0/APIMethods400.scala
- Class/Object: APIMethods400
- Method: getDoubleEntryTransaction
- Lines: (Referenced in user story technical context)

**Business Logic**:
1. When a transaction is recorded, it must create entries in at least two accounts
2. One account is debited (money flows out or liability increases)
3. Another account is credited (money flows in or asset increases)
4. The double-entry view exposes this fundamental accounting representation of transactions

**Variables**:
- **Input**: Transaction identifier, bank identifier, account identifier, view identifier
- **Output**: DoubleEntryTransactionJson containing debit_transaction and credit_transaction
- **Constants**: None

**Business Conditions**:
| Condition | Business Meaning | Values/Thresholds |
|-----------|------------------|-------------------|
| Transaction exists | Original transaction must be valid | Valid TRANSACTION_ID |
| Double-entry record exists | Both debit and credit sides recorded | Complete ledger entry |

**Business Impact**: 
Ensures accounting integrity by maintaining the fundamental principle of double-entry bookkeeping. This is essential for accurate financial reporting, reconciliation, audit compliance, and regulatory requirements.

**API Endpoints Using This Rule**:
- GET /obp/v4.0.0/banks/BANK_ID/accounts/ACCOUNT_ID/VIEW_ID/transactions/TRANSACTION_ID/double-entry-transaction - Retrieve double-entry transaction

**Related Test Cases**:
Test cases should validate that every transaction has both debit and credit sides properly recorded and retrievable.

**Migration Notes for Go**:
- Implement as a service method that queries the double-entry ledger
- Use Go structs to represent both debit and credit transaction sides
- Ensure proper handling of the relationship between the two sides

**Example Scenarios**:
```
Scenario 1: Standard payment transaction
Input: TRANSACTION_ID = "transaction-id-001"
Processing: Query double-entry ledger for both sides
Output: debit_transaction (Account A debited $100) and credit_transaction (Account B credited $100)

Scenario 2: Inter-bank transfer
Input: TRANSACTION_ID = "txn-interbank-001"
Processing: Query for both debit and credit entries across banks
Output: debit_transaction from bank-001, credit_transaction to bank-002
```

---

### BR-002: Debit-Credit Balance Requirement

**Category**: CALC

**Description**: The debit amount must equal the credit amount for each transaction. This is the fundamental validation rule ensuring that total debits always equal total credits in the double-entry system.

**Source**: 
- File: code/api/v4_0_0/APIMethods400.scala
- Class/Object: APIMethods400
- Method: getDoubleEntryTransaction
- Lines: (Referenced in user story technical context)

**Business Logic**:
1. For every transaction, the debit amount must equal the credit amount
2. This ensures the accounting equation remains balanced
3. Any discrepancy indicates a data integrity issue
4. The system validates this balance when retrieving double-entry transactions

**Variables**:
- **Input**: debit_transaction.amount, credit_transaction.amount
- **Output**: Balanced transaction (debit amount = credit amount)
- **Constants**: None

**Business Conditions**:
| Condition | Business Meaning | Values/Thresholds |
|-----------|------------------|-------------------|
| debit_amount == credit_amount | Accounting balance maintained | Exact equality required |
| Same currency | Amounts must be in same currency | Matching currency codes |

**Business Impact**: 
Maintains the integrity of the accounting system. Any imbalance would indicate errors in transaction processing and could lead to incorrect financial statements, failed audits, and regulatory non-compliance.

**API Endpoints Using This Rule**:
- GET /obp/v4.0.0/banks/BANK_ID/accounts/ACCOUNT_ID/VIEW_ID/transactions/TRANSACTION_ID/double-entry-transaction - Returns balanced debit/credit

**Related Test Cases**:
Test cases should verify that debit and credit amounts are always equal for any retrieved double-entry transaction.

**Migration Notes for Go**:
- Use decimal types (e.g., shopspring/decimal) for financial amounts to avoid floating-point precision issues
- Implement validation to ensure debit equals credit
- Consider adding a balance check method to the transaction struct

**Example Scenarios**:
```
Scenario 1: Balanced transaction
Input: Transaction with EUR 100.00 transfer
Processing: Verify debit_amount == credit_amount
Output: debit_transaction.amount = "100.00", credit_transaction.amount = "100.00"

Scenario 2: Cross-currency (same base amount)
Input: Transaction with currency conversion
Processing: Both sides show equivalent amounts in respective currencies
Output: Amounts balance according to exchange rate at transaction time
```

---

### BR-003: Two-Sided Transaction View

**Category**: TRANSFORMATION

**Description**: The system must show both sides of the transaction - the account being debited and the account being credited - with clear identification of which side is which.

**Source**: 
- File: code/api/v4_0_0/JSONFactory4.0.0.scala
- Class/Object: JSONFactory400
- Method: createDoubleEntryTransactionJson
- Lines: (Referenced in user story technical context)

**Business Logic**:
1. Response must include debit_transaction object with all debit side details
2. Response must include credit_transaction object with all credit side details
3. Each side includes account_id, bank_id, amount, date, description, and balance
4. Clear labeling distinguishes debit from credit entries

**Variables**:
- **Input**: Internal MappedTransaction / MappedDoubleEntryBookTransaction entities
- **Output**: DoubleEntryTransactionJson with debit_transaction and credit_transaction objects
- **Constants**: None

**Business Conditions**:
| Condition | Business Meaning | Values/Thresholds |
|-----------|------------------|-------------------|
| Both sides present | Complete double-entry view | debit_transaction and credit_transaction populated |
| Clear identification | Debit vs credit clearly labeled | Separate named objects |

**Business Impact**: 
Enables users to understand the complete financial picture of transactions, verify accounting accuracy, perform reconciliation between accounts, and audit financial records.

**API Endpoints Using This Rule**:
- GET /obp/v4.0.0/banks/BANK_ID/accounts/ACCOUNT_ID/VIEW_ID/transactions/TRANSACTION_ID/double-entry-transaction - Returns both sides

**Related Test Cases**:
Test cases should verify that both debit_transaction and credit_transaction objects are present and correctly populated.

**Migration Notes for Go**:
- Define separate structs for DebitTransaction and CreditTransaction (or use a common TransactionJson struct)
- Use clear field names in JSON response (debit_transaction, credit_transaction)
- Implement mapper functions to transform internal entities to response DTOs

**Example Scenarios**:
```
Scenario 1: Payment to supplier
Input: Transaction ID for supplier payment
Processing: Retrieve and format both sides
Output: 
  debit_transaction: {account_id: "account-001", amount: "100.00", description: "Payment to supplier"}
  credit_transaction: {account_id: "account-002", amount: "100.00", description: "Payment received from customer"}

Scenario 2: Internal transfer
Input: Transaction ID for internal transfer
Processing: Retrieve both account entries
Output: Both sides showing same bank_id but different account_ids
```

---

### BR-004: View-Based Access Control for Double-Entry Transactions

**Category**: DECISION

**Description**: Users can only view double-entry transactions for accounts they have been granted permission to access through the view system. The view identifier determines what level of detail the user can see.

**Source**: 
- File: code/views/Views.scala
- Class/Object: Views
- Method: (View permission validation methods)
- Lines: (Referenced in user story technical context)

**Business Logic**:
1. User must be authenticated with valid OAuth or DirectLogin credentials
2. User must have at least one view/permission granted on the account
3. The VIEW_ID parameter specifies which view's permissions apply
4. Different views may show different levels of detail (some may mask certain information)
5. Access is denied if user lacks permission to the specified view

**Variables**:
- **Input**: User authentication token, VIEW_ID, ACCOUNT_ID, BANK_ID
- **Output**: Authorized access to double-entry transaction data (filtered by view permissions)
- **Constants**: None

**Business Conditions**:
| Condition | Business Meaning | Values/Thresholds |
|-----------|------------------|-------------------|
| User authenticated | Valid session required | OAuth/DirectLogin token |
| View permission granted | User has access to view | VIEW_ID permission exists |
| Account access | User can access account | ACCOUNT_ID permission |

**Business Impact**: 
Protects sensitive financial information by ensuring users can only see transactions they are authorized to view. This supports regulatory compliance, privacy requirements, and multi-tenant banking operations.

**API Endpoints Using This Rule**:
- GET /obp/v4.0.0/banks/BANK_ID/accounts/ACCOUNT_ID/VIEW_ID/transactions/TRANSACTION_ID/double-entry-transaction - View-controlled access
- GET /obp/v4.0.0/banks/BANK_ID/accounts/ACCOUNT_ID/VIEW_ID/transactions/TRANSACTION_ID/other_account - View-controlled access

**Related Test Cases**:
Test cases should verify that unauthorized users receive HTTP 403 Forbidden responses and that view-specific data masking is applied correctly.

**Migration Notes for Go**:
- Implement middleware for authentication validation
- Create view permission checking service
- Use Go interfaces to abstract view permission logic
- Return appropriate HTTP status codes (401 for unauthenticated, 403 for unauthorized)

**Example Scenarios**:
```
Scenario 1: Authorized user with full view
Input: User with "owner" view on account
Processing: Validate view permissions, return full details
Output: Complete double-entry transaction with all fields

Scenario 2: Authorized user with limited view
Input: User with "public" view on account
Processing: Validate view permissions, apply data masking
Output: Double-entry transaction with some fields masked

Scenario 3: Unauthorized user
Input: User without view permission on account
Processing: Permission check fails
Output: HTTP 403 Forbidden error
```

---

### BR-005: Transaction Scope Specificity

**Category**: DECISION

**Description**: Double-entry view is scoped to a specific transaction - users must specify which transaction's double-entry representation to view. The system retrieves the double-entry representation for exactly one transaction at a time.

**Source**: 
- File: code/api/v4_0_0/APIMethods400.scala
- Class/Object: APIMethods400
- Method: getDoubleEntryTransaction
- Lines: (Referenced in user story technical context)

**Business Logic**:
1. User must provide a specific TRANSACTION_ID to view its double-entry representation
2. The system retrieves exactly one double-entry transaction per request
3. The transaction must belong to the specified account and bank
4. If the transaction does not exist, appropriate error is returned

**Variables**:
- **Input**: TRANSACTION_ID (unique identifier of the transaction)
- **Output**: Single DoubleEntryTransactionJson object
- **Constants**: None

**Business Conditions**:
| Condition | Business Meaning | Values/Thresholds |
|-----------|------------------|-------------------|
| Transaction specified | Specific transaction requested | Valid TRANSACTION_ID |
| Transaction exists | Transaction found in system | Exists in database |
| Transaction belongs to account | Correct account ownership | ACCOUNT_ID matches |

**Business Impact**: 
Enables precise retrieval of double-entry information for specific transactions, supporting detailed audit trails, transaction-level reconciliation, and focused financial analysis.

**API Endpoints Using This Rule**:
- GET /obp/v4.0.0/banks/BANK_ID/accounts/ACCOUNT_ID/VIEW_ID/transactions/TRANSACTION_ID/double-entry-transaction - Single transaction retrieval

**Related Test Cases**:
Test cases should verify that exactly one double-entry transaction is returned per request and appropriate errors for invalid transaction IDs.

**Migration Notes for Go**:
- Return a single struct, not a slice/array
- Handle the case where transaction does not exist with HTTP 404
- Validate that transaction belongs to the specified account

**Example Scenarios**:
```
Scenario 1: Valid transaction request
Input: TRANSACTION_ID = "transaction-id-001"
Processing: Query for specific transaction's double-entry representation
Output: Single DoubleEntryTransactionJson object

Scenario 2: Invalid transaction ID
Input: TRANSACTION_ID = "non-existent-txn"
Processing: Transaction not found
Output: HTTP 404 TransactionNotFound error
```

---

### BR-006: Real-Time Access Performance

**Category**: WORKFLOW

**Description**: Double-entry transaction retrieval must support real-time access patterns with low latency to meet the operational requirements of medium-volume usage patterns.

**Source**: 
- File: code/api/v4_0_0/APIMethods400.scala
- Class/Object: APIMethods400
- Method: getDoubleEntryTransaction
- Lines: (Referenced in user story technical context)

**Business Logic**:
1. API requests must be processed synchronously with immediate response
2. System must handle medium volume of concurrent requests
3. Response times must be appropriate for real-time user interactions
4. No batch processing or delayed responses for this capability

**Variables**:
- **Input**: API request with path parameters
- **Output**: Immediate JSON response
- **Constants**: None

**Business Conditions**:
| Condition | Business Meaning | Values/Thresholds |
|-----------|------------------|-------------------|
| Real-time processing | Synchronous request-response | Immediate response |
| Medium volume | Concurrent request handling | Medium throughput |

**Business Impact**: 
Enables users to quickly verify transaction double-entry representation during reconciliation workflows, supports real-time audit processes, and provides responsive user experience for financial analysts, auditors, and account holders.

**API Endpoints Using This Rule**:
- GET /obp/v4.0.0/banks/BANK_ID/accounts/ACCOUNT_ID/VIEW_ID/transactions/TRANSACTION_ID/double-entry-transaction - Real-time retrieval
- GET /obp/v4.0.0/banks/BANK_ID/accounts/ACCOUNT_ID/VIEW_ID/transactions/TRANSACTION_ID/other_account - Real-time retrieval

**Related Test Cases**:
Test cases should include performance benchmarks to verify response times under expected load conditions.

**Migration Notes for Go**:
- Use Go's efficient concurrency model for handling concurrent requests
- Implement connection pooling for database access
- Consider caching strategies for frequently accessed transactions
- Use context with timeout for request handling

**Example Scenarios**:
```
Scenario 1: Normal load retrieval
Input: Single request for double-entry transaction
Processing: Direct database query with indexed lookup
Output: Response within acceptable latency threshold

Scenario 2: Concurrent requests
Input: Multiple simultaneous requests
Processing: Parallel processing with connection pooling
Output: All responses within acceptable latency
```

---

### BR-007: Counterparty Account Identification

**Category**: TRANSFORMATION

**Description**: The system must provide information about the other account involved in the transaction (the counterparty), which represents the other side of the double-entry. This includes holder information, bank routing, and account routings.

**Source**: 
- File: code/api/v4_0_0/APIMethods400.scala
- Class/Object: APIMethods400
- Method: getOtherAccountForTransaction
- Lines: (Referenced in user story technical context)

**Business Logic**:
1. For any transaction, the system can retrieve details about the counterparty account
2. The counterparty represents the other side of the double-entry (if viewing debit, counterparty is credit account)
3. Response includes holder name, bank routing (BIC), and account routings (IBAN)
4. Metadata about the counterparty may include aliases, URLs, and location information

**Variables**:
- **Input**: TRANSACTION_ID, ACCOUNT_ID, BANK_ID, VIEW_ID
- **Output**: OtherAccountJson with holder, bank_routing, account_routings, and metadata
- **Constants**: None

**Business Conditions**:
| Condition | Business Meaning | Values/Thresholds |
|-----------|------------------|-------------------|
| Counterparty exists | Other account in transaction | Valid counterparty record |
| Routing information | Account identifiers available | Bank and account routings |

**Business Impact**: 
Enables complete understanding of transaction parties for reconciliation, compliance verification, and audit purposes. Users can identify who funds were sent to or received from.

**API Endpoints Using This Rule**:
- GET /obp/v4.0.0/banks/BANK_ID/accounts/ACCOUNT_ID/VIEW_ID/transactions/TRANSACTION_ID/other_account - Retrieve counterparty details

**Related Test Cases**:
Test cases should verify that counterparty information is correctly retrieved and includes all required routing information.

**Migration Notes for Go**:
- Define structs for OtherAccount, Holder, BankRouting, AccountRouting, and Metadata
- Implement service method to resolve counterparty from transaction
- Handle cases where counterparty information may be partially available

**Example Scenarios**:
```
Scenario 1: Payment to known supplier
Input: TRANSACTION_ID for supplier payment
Processing: Retrieve counterparty account details
Output: holder.name = "Supplier Company Ltd", bank_routing.address = "NDEAFIHH", account_routings[0].address = "FI9876543210987654"

Scenario 2: Payment to anonymous counterparty
Input: TRANSACTION_ID with limited counterparty info
Processing: Retrieve available counterparty details
Output: holder.is_alias = true, limited routing information
```

---

## API Endpoint Coverage

| Endpoint | HTTP Method | Business Rules | Rule IDs |
|----------|-------------|----------------|----------|
| /obp/v4.0.0/banks/BANK_ID/accounts/ACCOUNT_ID/VIEW_ID/transactions/TRANSACTION_ID/double-entry-transaction | GET | Double-entry principle, balance requirement, two-sided view, access control, transaction scope, real-time performance | BR-001, BR-002, BR-003, BR-004, BR-005, BR-006 |
| /obp/v4.0.0/banks/BANK_ID/accounts/ACCOUNT_ID/VIEW_ID/transactions/TRANSACTION_ID/other_account | GET | Access control, real-time performance, counterparty identification | BR-004, BR-006, BR-007 |

## Migration Validation Matrix

| Rule ID | Test Case Reference | Go Implementation Status | Validation Status |
|---------|---------------------|--------------------------|-------------------|
| BR-001 | TestDoubleEntryPrinciple | Pending | Pending |
| BR-002 | TestDebitCreditBalance | Pending | Pending |
| BR-003 | TestTwoSidedView | Pending | Pending |
| BR-004 | TestViewBasedAccessControl | Pending | Pending |
| BR-005 | TestTransactionScopeSpecificity | Pending | Pending |
| BR-006 | TestRealTimePerformance | Pending | Pending |
| BR-007 | TestCounterpartyIdentification | Pending | Pending |

## Notes and Assumptions

1. **Assumption**: The user story describes READ/RETRIEVAL operations only. No CREATE, UPDATE, or DELETE operations are included as they are not mentioned in the capability description ("View double-entry bookkeeping transactions").

2. **Assumption**: Multi-leg transactions (involving more than two accounts) handling is flagged as needing SME input in the user story. The business rules assume standard two-account transactions.

3. **Assumption**: The handling of pending/incomplete transactions in double-entry format requires clarification from SME.

4. **Assumption**: Access control for viewing the "other side" of a transaction when it belongs to a different customer requires clarification.

5. **Assumption**: For cross-currency transactions, historical exchange rates are preserved - this requires SME confirmation.

6. **Gap Identified**: Specific performance thresholds (e.g., response time SLAs) are not defined in the user story. These should be established during Go implementation.

7. **Gap Identified**: Error handling for edge cases (e.g., transactions without double-entry representation) should be tested thoroughly during migration.

8. **Currency Handling**: For cross-currency transactions, the double-entry view should show amounts in their respective currencies with any currency conversion clearly indicated.

9. **Settlement Accounts**: For transactions between accounts at different banks, settlement accounts may be involved as intermediaries in the double-entry chain.
