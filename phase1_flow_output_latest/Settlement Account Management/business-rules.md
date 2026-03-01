# Business Rules Extraction

**Extracted From**: Settlement Account Management Capability
**Analysis Date**: 2026-01-20
**Analyst**: Expert Scala Business Logic Analyst
**Migration Target**: Go Application

## Executive Summary
- Total Business Rules Extracted: 8
- API Endpoints Analyzed: 2
- Rule Categories:
  - Calculations: 0
  - Decisions: 3
  - Thresholds: 1
  - Aggregations: 0
  - Workflows: 2
  - Transformations: 2

## Business Rules Catalog

### BR-001: Settlement Account ID Generation

**Category**: TRANSFORMATION

**Description**: The system automatically generates settlement account IDs based on a standardized naming convention that combines the payment system and currency.

**Source**: 
- File: Settlement Account Service
- Class/Object: SettlementAccountService
- Method: createSettlementAccount
- Lines: N/A (derived from user story)

**Business Logic**:
1. When a new settlement account is created, the system generates the account ID automatically
2. The account ID follows the pattern: {PAYMENT_SYSTEM}_SETTLEMENT_ACCOUNT_{CURRENCY}
3. For example, a SEPA payment system with EUR currency generates: SEPA_SETTLEMENT_ACCOUNT_EUR

**Variables**:
- **Input**: payment_system (String), currency (String - ISO 4217 code)
- **Output**: account_id (String) - Generated unique identifier for the settlement account
- **Constants**: "_SETTLEMENT_ACCOUNT_" (separator pattern)

**Business Conditions**:
| Condition | Business Meaning | Values/Thresholds |
|-----------|------------------|-------------------|
| payment_system is provided | Identifies the payment network | SEPA, CARD, DEFAULT |
| currency is valid | ISO 4217 currency code | EUR, USD, GBP, etc. |

**Business Impact**: 
Ensures consistent and predictable settlement account identification across the banking system, enabling easy lookup and categorization of accounts by payment system and currency.

**API Endpoints Using This Rule**:
- POST /obp/v5.1.0/banks/{BANK_ID}/settlement-accounts - Create settlement account

**Related Test Cases**:
- Test that account ID is correctly generated for various payment system and currency combinations
- Test that account ID follows the exact naming pattern

**Migration Notes for Go**:
- Implement string concatenation using fmt.Sprintf or strings.Builder
- Ensure uppercase transformation for consistency
- Pattern: fmt.Sprintf("%s_SETTLEMENT_ACCOUNT_%s", paymentSystem, currency)

**Example Scenarios**:
```
Scenario 1: SEPA EUR Settlement Account
Input: payment_system = "SEPA", currency = "EUR"
Processing: Concatenate "SEPA" + "_SETTLEMENT_ACCOUNT_" + "EUR"
Output: account_id = "SEPA_SETTLEMENT_ACCOUNT_EUR"

Scenario 2: CARD USD Settlement Account
Input: payment_system = "CARD", currency = "USD"
Processing: Concatenate "CARD" + "_SETTLEMENT_ACCOUNT_" + "USD"
Output: account_id = "CARD_SETTLEMENT_ACCOUNT_USD"

Scenario 3: Default GBP Settlement Account
Input: payment_system = "DEFAULT", currency = "GBP"
Processing: Concatenate "DEFAULT" + "_SETTLEMENT_ACCOUNT_" + "GBP"
Output: account_id = "DEFAULT_SETTLEMENT_ACCOUNT_GBP"
```

---

### BR-002: Settlement Account Initial Balance Constraint

**Category**: THRESHOLD

**Description**: All newly created settlement accounts must have an initial balance of exactly zero to ensure proper accounting initialization.

**Source**: 
- File: Settlement Account Service
- Class/Object: SettlementAccountService
- Method: createSettlementAccount
- Lines: N/A (derived from user story)

**Business Logic**:
1. When creating a new settlement account, the system validates the initial balance
2. The balance amount MUST be exactly zero (0)
3. If a non-zero balance is provided, the creation request is rejected
4. This ensures all settlement accounts start from a clean slate for accurate double-entry bookkeeping

**Variables**:
- **Input**: balance.amount (Decimal/String)
- **Output**: Validation result (Boolean - pass/fail)
- **Constants**: REQUIRED_INITIAL_BALANCE = 0

**Business Conditions**:
| Condition | Business Meaning | Values/Thresholds |
|-----------|------------------|-------------------|
| balance.amount == 0 | Account starts with zero balance | Exactly 0 |
| balance.amount != 0 | Invalid initial balance | Reject creation |

**Business Impact**: 
Maintains accounting integrity by ensuring settlement accounts begin with zero balance, preventing incorrect initial states that could affect financial reconciliation and double-entry bookkeeping accuracy.

**API Endpoints Using This Rule**:
- POST /obp/v5.1.0/banks/{BANK_ID}/settlement-accounts - Create settlement account

**Related Test Cases**:
- Test that account creation succeeds with balance = 0
- Test that account creation fails with balance > 0
- Test that account creation fails with balance < 0

**Migration Notes for Go**:
- Use decimal comparison (not floating point) for accuracy
- Consider using shopspring/decimal library for precise decimal handling
- Validation: if balance.Amount.Cmp(decimal.Zero) != 0 { return error }

**Example Scenarios**:
```
Scenario 1: Valid Zero Balance
Input: balance = {"currency": "EUR", "amount": "0"}
Processing: Validate amount == 0
Output: Validation passes, account created

Scenario 2: Invalid Positive Balance
Input: balance = {"currency": "EUR", "amount": "100"}
Processing: Validate amount == 0
Output: Validation fails, error returned

Scenario 3: Invalid Negative Balance
Input: balance = {"currency": "EUR", "amount": "-50"}
Processing: Validate amount == 0
Output: Validation fails, error returned
```

---

### BR-003: Settlement Account Lookup Priority for Transactions

**Category**: DECISION

**Description**: When processing transactions that require a settlement account (counterparty not found in OBP), the system follows a specific priority order to determine which settlement account to use.

**Source**: 
- File: Transaction Processing Service / Mapped Connector
- Class/Object: TransactionService
- Method: findSettlementAccount
- Lines: N/A (derived from user story)

**Business Logic**:
1. First, look for a settlement account specific to the payment system AND currency (e.g., SEPA_SETTLEMENT_ACCOUNT_EUR)
2. If not found, look for a default settlement account for the counterparty currency (DEFAULT_SETTLEMENT_ACCOUNT_{CURRENCY})
3. If still not found, use one of the two OBP default settlement accounts based on transaction direction:
   - For incoming transactions: OBP_DEFAULT_INCOMING_ACCOUNT_ID
   - For outgoing transactions: OBP_DEFAULT_OUTGOING_ACCOUNT_ID

**Variables**:
- **Input**: payment_system (String), currency (String), transaction_direction (INCOMING/OUTGOING)
- **Output**: settlement_account_id (String)
- **Constants**: OBP_DEFAULT_INCOMING_ACCOUNT_ID, OBP_DEFAULT_OUTGOING_ACCOUNT_ID

**Business Conditions**:
| Condition | Business Meaning | Values/Thresholds |
|-----------|------------------|-------------------|
| Payment-specific account exists | Use dedicated settlement account | {PAYMENT_SYSTEM}_SETTLEMENT_ACCOUNT_{CURRENCY} |
| Default currency account exists | Use currency-specific default | DEFAULT_SETTLEMENT_ACCOUNT_{CURRENCY} |
| Direction is INCOMING | Use incoming default account | OBP_DEFAULT_INCOMING_ACCOUNT_ID |
| Direction is OUTGOING | Use outgoing default account | OBP_DEFAULT_OUTGOING_ACCOUNT_ID |

**Business Impact**: 
Ensures proper categorization and tracking of transactions by payment system while providing fallback mechanisms to guarantee all transactions can be recorded, maintaining double-entry bookkeeping integrity.

**API Endpoints Using This Rule**:
- Transaction processing endpoints (internal use during transaction creation)

**Related Test Cases**:
- Test lookup returns payment-specific account when it exists
- Test lookup falls back to default currency account when payment-specific not found
- Test lookup uses OBP_DEFAULT_INCOMING_ACCOUNT_ID for incoming transactions as last resort
- Test lookup uses OBP_DEFAULT_OUTGOING_ACCOUNT_ID for outgoing transactions as last resort

**Migration Notes for Go**:
- Implement as a chain of lookups with early return pattern
- Use switch/case or if-else chain for clarity
- Consider caching frequently used settlement accounts for performance

**Example Scenarios**:
```
Scenario 1: Payment-Specific Account Found
Input: payment_system = "SEPA", currency = "EUR", direction = "INCOMING"
Processing: Look for SEPA_SETTLEMENT_ACCOUNT_EUR -> Found
Output: settlement_account_id = "SEPA_SETTLEMENT_ACCOUNT_EUR"

Scenario 2: Fallback to Default Currency Account
Input: payment_system = "SWIFT", currency = "EUR", direction = "INCOMING"
Processing: Look for SWIFT_SETTLEMENT_ACCOUNT_EUR -> Not found
           Look for DEFAULT_SETTLEMENT_ACCOUNT_EUR -> Found
Output: settlement_account_id = "DEFAULT_SETTLEMENT_ACCOUNT_EUR"

Scenario 3: Fallback to OBP Default Incoming
Input: payment_system = "UNKNOWN", currency = "JPY", direction = "INCOMING"
Processing: Look for UNKNOWN_SETTLEMENT_ACCOUNT_JPY -> Not found
           Look for DEFAULT_SETTLEMENT_ACCOUNT_JPY -> Not found
           Direction is INCOMING -> Use default incoming
Output: settlement_account_id = "OBP_DEFAULT_INCOMING_ACCOUNT_ID"

Scenario 4: Fallback to OBP Default Outgoing
Input: payment_system = "UNKNOWN", currency = "JPY", direction = "OUTGOING"
Processing: Look for UNKNOWN_SETTLEMENT_ACCOUNT_JPY -> Not found
           Look for DEFAULT_SETTLEMENT_ACCOUNT_JPY -> Not found
           Direction is OUTGOING -> Use default outgoing
Output: settlement_account_id = "OBP_DEFAULT_OUTGOING_ACCOUNT_ID"
```

---

### BR-004: Default Settlement Account Auto-Provisioning on Bank Creation

**Category**: WORKFLOW

**Description**: When a new bank is created in the system, two default settlement accounts are automatically provisioned to ensure the bank can immediately process transactions.

**Source**: 
- File: Bank Service
- Class/Object: BankService
- Method: createBank
- Lines: N/A (derived from user story)

**Business Logic**:
1. When a new bank entity is created, the system automatically creates two default settlement accounts
2. The first account is OBP_DEFAULT_INCOMING_ACCOUNT_ID for receiving incoming transactions
3. The second account is OBP_DEFAULT_OUTGOING_ACCOUNT_ID for processing outgoing transactions
4. Both accounts are created with EUR as the default currency
5. Both accounts are initialized with zero balance

**Variables**:
- **Input**: bank_id (String) - The newly created bank's identifier
- **Output**: Two settlement accounts (OBP_DEFAULT_INCOMING_ACCOUNT_ID, OBP_DEFAULT_OUTGOING_ACCOUNT_ID)
- **Constants**: 
  - OBP_DEFAULT_INCOMING_ACCOUNT_ID
  - OBP_DEFAULT_OUTGOING_ACCOUNT_ID
  - DEFAULT_CURRENCY = "EUR"

**Business Conditions**:
| Condition | Business Meaning | Values/Thresholds |
|-----------|------------------|-------------------|
| Bank creation successful | Trigger auto-provisioning | N/A |
| Default currency | EUR used for default accounts | EUR |
| Initial balance | Zero balance for both accounts | 0 |

**Business Impact**: 
Ensures every bank in the system has the minimum required settlement accounts to process transactions immediately after creation, preventing transaction failures due to missing settlement accounts.

**API Endpoints Using This Rule**:
- Bank creation endpoint (triggers this workflow internally)

**Related Test Cases**:
- Test that bank creation automatically creates OBP_DEFAULT_INCOMING_ACCOUNT_ID
- Test that bank creation automatically creates OBP_DEFAULT_OUTGOING_ACCOUNT_ID
- Test that both default accounts have EUR currency
- Test that both default accounts have zero initial balance

**Migration Notes for Go**:
- Implement as a post-creation hook or within a transaction
- Use database transaction to ensure atomicity (bank + settlement accounts created together)
- Consider using goroutines for parallel account creation if performance is critical

**Example Scenarios**:
```
Scenario 1: New Bank Creation
Input: Create bank with bank_id = "bank-123"
Processing: 
  1. Create bank entity
  2. Auto-create OBP_DEFAULT_INCOMING_ACCOUNT_ID with EUR, balance = 0
  3. Auto-create OBP_DEFAULT_OUTGOING_ACCOUNT_ID with EUR, balance = 0
Output: Bank created with two default settlement accounts

Scenario 2: Bank Creation Rollback
Input: Create bank with bank_id = "bank-456", but settlement account creation fails
Processing:
  1. Create bank entity
  2. Attempt to create default accounts -> Fails
  3. Rollback entire transaction
Output: Bank creation fails, no partial state
```

---

### BR-005: Settlement Account Bank Association Requirement

**Category**: DECISION

**Description**: Every settlement account must be associated with a valid, existing bank entity in the system.

**Source**: 
- File: Settlement Account Service
- Class/Object: SettlementAccountService
- Method: createSettlementAccount
- Lines: N/A (derived from user story)

**Business Logic**:
1. Before creating a settlement account, validate that the specified BANK_ID exists in the system
2. If the bank does not exist, reject the settlement account creation request
3. The settlement account is permanently linked to the bank upon creation

**Variables**:
- **Input**: bank_id (String)
- **Output**: Validation result (Boolean - bank exists or not)
- **Constants**: None

**Business Conditions**:
| Condition | Business Meaning | Values/Thresholds |
|-----------|------------------|-------------------|
| Bank exists | Settlement account can be created | BANK_ID found in database |
| Bank does not exist | Settlement account creation rejected | BANK_ID not found |

**Business Impact**: 
Ensures data integrity by preventing orphaned settlement accounts and maintaining proper organizational hierarchy where all settlement accounts belong to a valid bank.

**API Endpoints Using This Rule**:
- POST /obp/v5.1.0/banks/{BANK_ID}/settlement-accounts - Create settlement account

**Related Test Cases**:
- Test that settlement account creation succeeds with valid BANK_ID
- Test that settlement account creation fails with non-existent BANK_ID
- Test appropriate error message returned for invalid BANK_ID

**Migration Notes for Go**:
- Implement bank existence check before account creation
- Use database lookup or cache for bank validation
- Return appropriate HTTP 404 or 400 error for invalid bank

**Example Scenarios**:
```
Scenario 1: Valid Bank ID
Input: bank_id = "existing-bank-123"
Processing: Check if bank exists in database -> Found
Output: Validation passes, proceed with account creation

Scenario 2: Invalid Bank ID
Input: bank_id = "non-existent-bank-999"
Processing: Check if bank exists in database -> Not found
Output: Validation fails, return error "Bank not found"
```

---

### BR-006: Settlement Account User Ownership Assignment

**Category**: DECISION

**Description**: Settlement accounts can be owned by a specific user, or if no user is specified, ownership defaults to the user creating the account.

**Source**: 
- File: Settlement Account Service
- Class/Object: SettlementAccountService
- Method: createSettlementAccount
- Lines: N/A (derived from user story)

**Business Logic**:
1. If user_id is provided in the request, validate that the user exists
2. If user_id is valid, assign ownership to that user
3. If user_id is not provided, assign ownership to the authenticated user making the request
4. The owner user_id is stored with the settlement account record

**Variables**:
- **Input**: user_id (String, optional), authenticated_user_id (String)
- **Output**: assigned_user_id (String)
- **Constants**: None

**Business Conditions**:
| Condition | Business Meaning | Values/Thresholds |
|-----------|------------------|-------------------|
| user_id provided and valid | Assign to specified user | User exists in system |
| user_id provided but invalid | Reject creation | User not found |
| user_id not provided | Assign to creating user | Use authenticated user |

**Business Impact**: 
Provides flexibility in account ownership while ensuring every settlement account has a valid owner for accountability and access control purposes.

**API Endpoints Using This Rule**:
- POST /obp/v5.1.0/banks/{BANK_ID}/settlement-accounts - Create settlement account

**Related Test Cases**:
- Test that specified user_id is assigned when valid
- Test that creation fails when specified user_id is invalid
- Test that authenticated user is assigned when user_id not provided

**Migration Notes for Go**:
- Extract authenticated user from request context
- Implement conditional assignment logic
- Validate user existence if user_id is provided

**Example Scenarios**:
```
Scenario 1: Specific User Ownership
Input: user_id = "user-456", authenticated_user = "admin-123"
Processing: Validate user-456 exists -> Found
Output: Settlement account owned by user-456

Scenario 2: Default to Creating User
Input: user_id = null, authenticated_user = "admin-123"
Processing: No user_id provided, use authenticated user
Output: Settlement account owned by admin-123

Scenario 3: Invalid Specified User
Input: user_id = "non-existent-user", authenticated_user = "admin-123"
Processing: Validate non-existent-user exists -> Not found
Output: Creation fails, error returned
```

---

### BR-007: Settlement Account Authorization Requirement

**Category**: WORKFLOW

**Description**: Users must have the specific role CanCreateSettlementAccountAtOneBank to create settlement accounts for a bank.

**Source**: 
- File: Settlement Account Service / Authorization Service
- Class/Object: AuthorizationService
- Method: checkEntitlement
- Lines: N/A (derived from user story)

**Business Logic**:
1. Before processing a settlement account creation request, check user's entitlements
2. User must have the CanCreateSettlementAccountAtOneBank role for the specific bank
3. If the user lacks this role, reject the request with an authorization error
4. This role is bank-specific, meaning a user may have permission for one bank but not another

**Variables**:
- **Input**: authenticated_user_id (String), bank_id (String)
- **Output**: Authorization result (Boolean - authorized or not)
- **Constants**: REQUIRED_ROLE = "CanCreateSettlementAccountAtOneBank"

**Business Conditions**:
| Condition | Business Meaning | Values/Thresholds |
|-----------|------------------|-------------------|
| User has role for bank | Authorized to create | CanCreateSettlementAccountAtOneBank |
| User lacks role | Not authorized | Return 403 Forbidden |

**Business Impact**: 
Enforces proper access control and separation of duties, ensuring only authorized personnel can create settlement accounts, which are critical for financial transaction processing.

**API Endpoints Using This Rule**:
- POST /obp/v5.1.0/banks/{BANK_ID}/settlement-accounts - Create settlement account

**Related Test Cases**:
- Test that user with CanCreateSettlementAccountAtOneBank role can create account
- Test that user without the role receives 403 Forbidden
- Test that role is checked for the specific bank, not globally

**Migration Notes for Go**:
- Implement middleware or decorator for authorization check
- Query entitlements/roles from database or auth service
- Return HTTP 403 with appropriate error message for unauthorized requests

**Example Scenarios**:
```
Scenario 1: Authorized User
Input: user = "admin-123", bank_id = "bank-456"
Processing: Check if admin-123 has CanCreateSettlementAccountAtOneBank for bank-456 -> Yes
Output: Authorization passes, proceed with creation

Scenario 2: Unauthorized User
Input: user = "regular-user-789", bank_id = "bank-456"
Processing: Check if regular-user-789 has CanCreateSettlementAccountAtOneBank for bank-456 -> No
Output: Authorization fails, return 403 Forbidden

Scenario 3: User Authorized for Different Bank
Input: user = "admin-123", bank_id = "bank-999"
Processing: Check if admin-123 has CanCreateSettlementAccountAtOneBank for bank-999 -> No (only has for bank-456)
Output: Authorization fails, return 403 Forbidden
```

---

### BR-008: Double-Entry Bookkeeping Settlement Account Usage

**Category**: TRANSFORMATION

**Description**: Settlement accounts serve as counterparty accounts in double-entry bookkeeping when the actual counterparty account is not found in the OBP system.

**Source**: 
- File: Transaction Processing Service / Mapped Connector
- Class/Object: TransactionService
- Method: saveTransaction
- Lines: N/A (derived from user story)

**Business Logic**:
1. When a transaction is processed, the system attempts to find the counterparty's OBP account
2. If no OBP account is found for the counterparty, the system uses a settlement account instead
3. The settlement account receives the opposite entry to maintain double-entry bookkeeping balance
4. This ensures every transaction has both a debit and credit side recorded in the system

**Variables**:
- **Input**: transaction (Transaction object), counterparty_info (CounterpartyInfo)
- **Output**: transaction_with_settlement (Transaction with settlement account as counterparty)
- **Constants**: None

**Business Conditions**:
| Condition | Business Meaning | Values/Thresholds |
|-----------|------------------|-------------------|
| Counterparty OBP account found | Use actual account | Direct account-to-account transaction |
| Counterparty OBP account not found | Use settlement account | Settlement account as counterparty |

**Business Impact**: 
Maintains accounting integrity by ensuring all transactions are properly recorded with both debit and credit entries, even when the counterparty is external to the OBP system. This is fundamental to proper financial record-keeping and reconciliation.

**API Endpoints Using This Rule**:
- Transaction creation/processing endpoints (internal use)

**Related Test Cases**:
- Test that transactions with known counterparty use actual account
- Test that transactions with unknown counterparty use settlement account
- Test that double-entry balance is maintained in both scenarios

**Migration Notes for Go**:
- Implement counterparty lookup before transaction save
- Use BR-003 (Settlement Account Lookup Priority) to find appropriate settlement account
- Ensure transaction and settlement entry are saved atomically

**Example Scenarios**:
```
Scenario 1: Known Counterparty
Input: Transaction from account-A to counterparty with known OBP account-B
Processing: Look up counterparty -> Found account-B
Output: Transaction recorded: Debit account-A, Credit account-B

Scenario 2: Unknown Counterparty (External)
Input: Transaction from account-A to external counterparty (no OBP account)
Processing: Look up counterparty -> Not found
           Find settlement account using BR-003 -> SEPA_SETTLEMENT_ACCOUNT_EUR
Output: Transaction recorded: Debit account-A, Credit SEPA_SETTLEMENT_ACCOUNT_EUR

Scenario 3: Incoming from External
Input: Incoming transaction to account-A from external sender
Processing: Look up sender -> Not found
           Find settlement account using BR-003 -> OBP_DEFAULT_INCOMING_ACCOUNT_ID
Output: Transaction recorded: Debit OBP_DEFAULT_INCOMING_ACCOUNT_ID, Credit account-A
```

---

## API Endpoint Coverage

| Endpoint | HTTP Method | Business Rules | Rule IDs |
|----------|-------------|----------------|----------|
| /obp/v5.1.0/banks/{BANK_ID}/settlement-accounts | POST | Account ID generation, Initial balance validation, Bank association, User ownership, Authorization | BR-001, BR-002, BR-005, BR-006, BR-007 |
| /obp/v5.1.0/banks/{BANK_ID}/accounts/{ACCOUNT_ID} | PUT | Account update (configuration changes) | N/A (update operations) |
| Bank creation endpoint | POST | Default account auto-provisioning | BR-004 |
| Transaction processing endpoints | Internal | Settlement account lookup, Double-entry bookkeeping | BR-003, BR-008 |

## Migration Validation Matrix

| Rule ID | Test Case Reference | Go Implementation Status | Validation Status |
|---------|---------------------|--------------------------|-------------------|
| BR-001 | TestSettlementAccountIDGeneration | Pending | Pending |
| BR-002 | TestInitialBalanceValidation | Pending | Pending |
| BR-003 | TestSettlementAccountLookupPriority | Pending | Pending |
| BR-004 | TestDefaultAccountAutoProvisioning | Pending | Pending |
| BR-005 | TestBankAssociationValidation | Pending | Pending |
| BR-006 | TestUserOwnershipAssignment | Pending | Pending |
| BR-007 | TestAuthorizationRequirement | Pending | Pending |
| BR-008 | TestDoubleEntryBookkeeping | Pending | Pending |

## Notes and Assumptions

1. **Payment Systems**: The user story mentions SEPA, CARD, and DEFAULT as payment systems. The complete list of supported payment systems should be confirmed with SME.

2. **Currency Support**: EUR is the default currency for auto-provisioned accounts. Additional currencies may be supported but require SME confirmation.

3. **Account Routings**: Settlement accounts support account routings for external payment system integration. The validation rules for routing schemes and addresses should be defined in detail.

4. **Audit Requirements**: The user story mentions considering audit trail logging. This should be implemented as a cross-cutting concern in the Go application.

5. **Reconciliation Process**: The reconciliation process between settlement accounts and external payment systems needs SME clarification.

6. **Branch Association**: Settlement accounts can be associated with a branch_id. The business rules for branch validation are not explicitly defined in the user story.

7. **Account Attributes**: Settlement accounts support account_attributes. The specific attributes and their business rules should be documented separately if needed.
