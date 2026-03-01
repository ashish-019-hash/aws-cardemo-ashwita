# Business Rules Extraction

**Extracted From**: Balancing Transaction Retrieval User Story
**Analysis Date**: 2026-01-20
**Analyst**: Expert Scala Business Logic Analyst
**Migration Target**: Go Application

## Executive Summary
- Total Business Rules Extracted: 6
- API Endpoints Analyzed: 1
- Rule Categories:
  - Calculations: 0
  - Decisions: 2
  - Thresholds: 0
  - Aggregations: 0
  - Workflows: 2
  - Transformations: 2

## Business Rules Catalog

### BR-001: Double-Entry Balancing Transaction Principle

**Category**: WORKFLOW

**Description**: Every transaction in the double-entry bookkeeping system must have a corresponding balancing transaction that represents the other side of the ledger entry. When a debit is recorded on one account, a corresponding credit must exist on another account.

**Source**: 
- File: code/api/v4_0_0/APIMethods400.scala
- Class/Object: APIMethods400
- Method: getBalancingTransaction
- Lines: (Referenced in user story technical context)

**Business Logic**:
1. When a transaction is recorded in the system, it affects at least two accounts
2. The original transaction represents one side of the entry (debit or credit)
3. The balancing transaction represents the corresponding opposite entry
4. For example, if Account A is debited $100, Account B must be credited $100

**Variables**:
- **Input**: Original transaction identifier, bank identifier, account identifier, view identifier
- **Output**: Balancing transaction with transaction_id, bank_id, account_id, amount, date, description, balance, type
- **Constants**: None

**Business Conditions**:
| Condition | Business Meaning | Values/Thresholds |
|-----------|------------------|-------------------|
| Transaction exists | Original transaction must be valid | Valid TRANSACTION_ID |
| Balancing entry exists | Double-entry record must be complete | Corresponding entry in ledger |

**Business Impact**: 
Ensures accounting integrity by maintaining the fundamental principle of double-entry bookkeeping where total debits always equal total credits. This is essential for accurate financial reporting, reconciliation, and audit compliance.

**API Endpoints Using This Rule**:
- GET /obp/v4.0.0/banks/BANK_ID/accounts/ACCOUNT_ID/VIEW_ID/transactions/TRANSACTION_ID/balancing-transaction - Retrieve balancing transaction

**Related Test Cases**:
Test cases should validate that every transaction has exactly one corresponding balancing transaction with equal and opposite amounts.

**Migration Notes for Go**:
- Implement as a service method that queries the double-entry ledger
- Use Go's strong typing to ensure transaction amounts are properly balanced
- Consider using decimal types for financial amounts to avoid floating-point precision issues

**Example Scenarios**:
```
Scenario 1: Standard payment transaction
Input: TRANSACTION_ID = "txn-001" (Debit of $100 from Account A)
Processing: Query double-entry ledger for corresponding entry
Output: Balancing transaction showing Credit of $100 to Account B

Scenario 2: Inter-bank transfer
Input: TRANSACTION_ID = "txn-002" (Debit from customer account)
Processing: Query for settlement account credit entry
Output: Balancing transaction showing Credit to settlement account
```

---

### BR-002: One-to-One Transaction Relationship

**Category**: DECISION

**Description**: Each transaction has exactly one balancing transaction that corresponds to it in the accounting system. The relationship is strictly one-to-one, ensuring clear traceability between original and balancing entries.

**Source**: 
- File: code/api/v4_0_0/APIMethods400.scala
- Class/Object: APIMethods400
- Method: getBalancingTransaction
- Lines: (Referenced in user story technical context)

**Business Logic**:
1. Given a transaction identifier, the system retrieves exactly one balancing transaction
2. The balancing transaction is uniquely linked to the original transaction
3. No transaction can have multiple balancing transactions
4. No balancing transaction can correspond to multiple original transactions

**Variables**:
- **Input**: TRANSACTION_ID (unique identifier of the original transaction)
- **Output**: Single BalancingTransactionJson object
- **Constants**: None

**Business Conditions**:
| Condition | Business Meaning | Values/Thresholds |
|-----------|------------------|-------------------|
| Unique mapping | One original to one balancing | 1:1 relationship |
| Transaction specificity | Scoped to specific transaction | Single TRANSACTION_ID |

**Business Impact**: 
Maintains data integrity and simplifies reconciliation by ensuring each transaction can be traced to exactly one corresponding entry. This supports audit trails and prevents accounting discrepancies.

**API Endpoints Using This Rule**:
- GET /obp/v4.0.0/banks/BANK_ID/accounts/ACCOUNT_ID/VIEW_ID/transactions/TRANSACTION_ID/balancing-transaction - Retrieve single balancing transaction

**Related Test Cases**:
Test cases should verify that querying for a balancing transaction always returns exactly one result (or appropriate error if not found).

**Migration Notes for Go**:
- Return a single struct, not a slice/array
- Handle the case where no balancing transaction exists with appropriate error response
- Use Go's error handling pattern to distinguish between "not found" and other errors

**Example Scenarios**:
```
Scenario 1: Valid transaction with balancing entry
Input: TRANSACTION_ID = "txn-001"
Processing: Query for unique balancing transaction
Output: Single balancing transaction object

Scenario 2: Transaction without balancing entry
Input: TRANSACTION_ID = "txn-orphan"
Processing: Query returns no results
Output: HTTP 404 BalancingTransactionNotFound error
```

---

### BR-003: View-Based Access Control for Balancing Transactions

**Category**: DECISION

**Description**: Users can only retrieve balancing transactions for accounts they have been granted permission to access through the view system. The view identifier determines what level of detail the user can see about the balancing transaction.

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
- **Input**: User authentication token, VIEW_ID, ACCOUNT_ID
- **Output**: Authorized access to balancing transaction data (filtered by view permissions)
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
- GET /obp/v4.0.0/banks/BANK_ID/accounts/ACCOUNT_ID/VIEW_ID/transactions/TRANSACTION_ID/balancing-transaction - View-controlled access

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
Output: Complete balancing transaction with all fields

Scenario 2: Authorized user with limited view
Input: User with "public" view on account
Processing: Validate view permissions, apply data masking
Output: Balancing transaction with some fields masked

Scenario 3: Unauthorized user
Input: User without view permission on account
Processing: Permission check fails
Output: HTTP 403 Forbidden error
```

---

### BR-004: Balancing Transaction Data Transformation

**Category**: TRANSFORMATION

**Description**: The balancing transaction response must include complete details structured according to the BalancingTransactionJson format, including transaction identifiers, account information, routing details, holder information, and transaction details.

**Source**: 
- File: code/api/v4_0_0/JSONFactory4.0.0.scala
- Class/Object: JSONFactory400
- Method: createBalancingTransactionJson
- Lines: (Referenced in user story technical context)

**Business Logic**:
1. Transform internal transaction data to standardized JSON response format
2. Include transaction_id, bank_id, and account_id for the balancing transaction
3. Include this_account details with bank routing and account routings
4. Include other_account details (the original transaction's account)
5. Include transaction details: type, description, posted date, completed date, new_balance, and value

**Variables**:
- **Input**: Internal MappedTransaction / MappedDoubleEntryBookTransaction entities
- **Output**: BalancingTransactionJson with structured account and transaction details
- **Constants**: None

**Business Conditions**:
| Condition | Business Meaning | Values/Thresholds |
|-----------|------------------|-------------------|
| Complete data | All required fields populated | Non-null values |
| Valid routing | Bank and account routing present | Valid scheme/address pairs |

**Business Impact**: 
Provides a standardized, comprehensive view of the balancing transaction that enables downstream systems to perform reconciliation, reporting, and audit functions with complete information.

**API Endpoints Using This Rule**:
- GET /obp/v4.0.0/banks/BANK_ID/accounts/ACCOUNT_ID/VIEW_ID/transactions/TRANSACTION_ID/balancing-transaction - Returns structured response

**Related Test Cases**:
Test cases should validate that all required fields are present in the response and that the JSON structure matches the expected schema.

**Migration Notes for Go**:
- Define Go structs matching the BalancingTransactionJson structure
- Use json tags for proper serialization
- Implement mapper functions to transform internal entities to response DTOs
- Consider using embedded structs for nested objects (this_account, other_account, details)

**Example Scenarios**:
```
Scenario 1: Complete balancing transaction response
Input: Internal transaction record with all data
Processing: Map to BalancingTransactionJson structure
Output: JSON with transaction_id, bank_id, account_id, this_account, other_account, details

Scenario 2: Response with amount and currency
Input: Transaction with EUR 100.00 value
Processing: Format amount with currency
Output: details.value = {"currency": "EUR", "amount": "100.00"}
```

---

### BR-005: Real-Time Transaction Retrieval Performance

**Category**: WORKFLOW

**Description**: Balancing transaction retrieval must support real-time access patterns with low latency to meet the operational requirements of medium-volume usage patterns.

**Source**: 
- File: code/api/v4_0_0/APIMethods400.scala
- Class/Object: APIMethods400
- Method: getBalancingTransaction
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
Enables users to quickly verify transaction balancing during reconciliation workflows, supports real-time audit processes, and provides responsive user experience for financial analysts and account holders.

**API Endpoints Using This Rule**:
- GET /obp/v4.0.0/banks/BANK_ID/accounts/ACCOUNT_ID/VIEW_ID/transactions/TRANSACTION_ID/balancing-transaction - Real-time retrieval

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
Input: Single request for balancing transaction
Processing: Direct database query with indexed lookup
Output: Response within acceptable latency threshold

Scenario 2: Concurrent requests
Input: Multiple simultaneous requests
Processing: Parallel processing with connection pooling
Output: All responses within acceptable latency
```

---

### BR-006: Transaction Relationship Identification

**Category**: TRANSFORMATION

**Description**: The system must clearly identify the relationship between the original transaction and its balancing counterpart by including both "this_account" (the balancing transaction's account) and "other_account" (the original transaction's account) in the response.

**Source**: 
- File: code/api/v4_0_0/JSONFactory4.0.0.scala
- Class/Object: JSONFactory400
- Method: createBalancingTransactionJson
- Lines: (Referenced in user story technical context)

**Business Logic**:
1. The response includes "this_account" representing the account involved in the balancing transaction
2. The response includes "other_account" representing the account from the original transaction
3. Both accounts include holder information, bank routing, and account routings
4. This structure enables users to understand the complete flow of funds between accounts

**Variables**:
- **Input**: Balancing transaction entity with linked account information
- **Output**: Structured response with this_account and other_account objects
- **Constants**: None

**Business Conditions**:
| Condition | Business Meaning | Values/Thresholds |
|-----------|------------------|-------------------|
| Both accounts identified | Clear relationship shown | this_account and other_account present |
| Routing information | Account identifiers included | Bank and account routings |

**Business Impact**: 
Enables complete understanding of the transaction flow for reconciliation, audit, and compliance purposes. Users can trace funds from source to destination accounts.

**API Endpoints Using This Rule**:
- GET /obp/v4.0.0/banks/BANK_ID/accounts/ACCOUNT_ID/VIEW_ID/transactions/TRANSACTION_ID/balancing-transaction - Returns relationship data

**Related Test Cases**:
Test cases should verify that both this_account and other_account are correctly populated with accurate relationship information.

**Migration Notes for Go**:
- Define separate structs for ThisAccount and OtherAccount
- Ensure proper foreign key relationships in data model
- Implement service methods to resolve account details from transaction records

**Example Scenarios**:
```
Scenario 1: Payment between two accounts
Input: Original transaction debits Account A
Processing: Retrieve balancing transaction crediting Account B
Output: this_account = Account B details, other_account = Account A details

Scenario 2: Transaction with settlement account
Input: Original transaction from customer account
Processing: Retrieve balancing transaction to settlement account
Output: this_account = Settlement account, other_account = Customer account
```

---

## API Endpoint Coverage

| Endpoint | HTTP Method | Business Rules | Rule IDs |
|----------|-------------|----------------|----------|
| /obp/v4.0.0/banks/BANK_ID/accounts/ACCOUNT_ID/VIEW_ID/transactions/TRANSACTION_ID/balancing-transaction | GET | Double-entry principle, one-to-one relationship, view-based access control, data transformation, real-time performance, relationship identification | BR-001, BR-002, BR-003, BR-004, BR-005, BR-006 |

## Migration Validation Matrix

| Rule ID | Test Case Reference | Go Implementation Status | Validation Status |
|---------|---------------------|--------------------------|-------------------|
| BR-001 | TestDoubleEntryBalancing | Pending | Pending |
| BR-002 | TestOneToOneRelationship | Pending | Pending |
| BR-003 | TestViewBasedAccessControl | Pending | Pending |
| BR-004 | TestBalancingTransactionResponse | Pending | Pending |
| BR-005 | TestRealTimePerformance | Pending | Pending |
| BR-006 | TestTransactionRelationship | Pending | Pending |

## Notes and Assumptions

1. **Assumption**: The user story describes a single GET endpoint for retrieving balancing transactions. No CREATE, UPDATE, or DELETE operations are included as they are not mentioned in the capability description.

2. **Assumption**: Multi-leg transactions (involving more than two accounts) handling is flagged as needing SME input in the user story. The business rules assume standard two-account transactions.

3. **Assumption**: The balancing transaction for pending/incomplete transactions may have different behavior - this requires clarification from SME.

4. **Assumption**: Access control for viewing balancing transactions belonging to different customers' accounts requires clarification.

5. **Gap Identified**: Specific performance thresholds (e.g., response time SLAs) are not defined in the user story. These should be established during Go implementation.

6. **Gap Identified**: Error handling for edge cases (e.g., orphaned transactions without balancing entries) should be tested thoroughly during migration.
