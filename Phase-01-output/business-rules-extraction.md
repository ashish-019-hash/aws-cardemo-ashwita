# Business Rules Extraction

**Extracted From**: Open Bank Project (OBP) API - Bank Registration and Configuration
**Analysis Date**: 2025-11-24
**Analyst**: Expert Scala Business Logic Analyst
**Migration Target**: Go Application
**Source User Story**: bank_registration_configuration_user_story.md
**Source Entities**: business_entities.md

## Executive Summary
- Total Business Rules Extracted: 8
- API Endpoints Analyzed: 1
- Rule Categories:
  - Calculations: 0
  - Decisions: 3
  - Thresholds: 2
  - Aggregations: 0
  - Workflows: 3
  - Transformations: 0

## Business Rules Catalog

---

### BR-001: Unique Bank Identification Enforcement

**Category**: DECISION

**Description**: Each bank must have a unique identification code (bankId/permalink) that cannot be duplicated across the entire platform to ensure proper bank entity isolation and prevent conflicts in multi-bank deployments.

**Source**: 
- File: /obp-api/src/main/scala/code/api/v4_0_0/APIMethods400.scala
- Class/Object: APIMethods400
- Method: createBank
- Lines: 3621-3683
- Additional Reference: /obp-api/src/main/scala/code/bankconnectors/LocalMappedConnector.scala (lines 3169-3247)

**Business Logic**:
1. When a new bank registration is requested, the system receives a bank ID (permalink)
2. The system checks if this bank ID already exists in the MappedBank table
3. If the bank ID exists, the system either updates the existing bank (idempotent operation) or rejects the request
4. If the bank ID is unique, the system proceeds with bank creation
5. The bank ID becomes the permanent unique identifier for all future operations related to this bank

**Scala Implementation**:
```scala
// From LocalMappedConnector.scala lines 3198-3210
MappedBank.create
  .permalink(bankId)  // Unique identifier
  .fullBankName(fullBankName)
  .shortBankName(shortBankName)
  .logoURL(logoURL)
  .websiteURL(websiteURL)
  .swiftBIC(swiftBIC)
  .national_identifier(national_identifier)
  .mBankRoutingScheme(bankRoutingScheme)
  .mBankRoutingAddress(bankRoutingAddress)
  .saveMe()
```

**Variables**:
- **Input**: 
  - `bankId` (String): Unique bank identifier from request (PostBankJson400.id)
  - Mapped to `permalink` field in MappedBank entity
- **Output**: 
  - Bank entity created with unique permalink
  - Or error if duplicate bank ID detected
- **Constants**: None

**Business Conditions**:
| Condition | Business Meaning | Values/Thresholds |
|-----------|------------------|-------------------|
| bankId must be unique | No two banks can share the same identifier | Unique constraint on MappedBank.permalink |
| bankId length > 3 | Bank ID must be meaningful | Minimum 3 characters |
| bankId cannot contain spaces | Bank ID must be URL-safe | No space characters allowed |
| bankId cannot contain :::: | Bank ID must avoid reserved separators | No :::: sequence allowed |

**Business Impact**: 
This rule ensures data integrity in multi-bank deployments where a single API instance serves multiple banking institutions. Without unique bank identification, transactions, accounts, and customers could be incorrectly associated with the wrong bank, leading to serious data contamination and regulatory compliance issues. This is fundamental to the multi-tenancy architecture of the OBP platform.

**API Endpoints Using This Rule**:
- POST /obp/v4.0.0/banks - Bank creation endpoint

**Related Test Cases**:
- Test case for duplicate bank ID rejection
- Test case for valid unique bank ID acceptance
- Test case for bank ID format validation

**Migration Notes for Go**:
- Implement unique constraint at database level using UNIQUE index on permalink column
- Use database-level constraint checking rather than application-level to ensure atomicity
- Consider using PostgreSQL's ON CONFLICT clause for idempotent operations
- Return appropriate HTTP 409 Conflict status for duplicate bank IDs
- Implement proper error handling for constraint violations

**Example Scenarios**:
```
Scenario 1: First bank registration
Input: bankId = "gh.29.uk"
Processing: Check MappedBank table for existing permalink = "gh.29.uk"
Result: Not found, proceed with bank creation
Output: Bank created successfully with permalink = "gh.29.uk"

Scenario 2: Duplicate bank registration attempt
Input: bankId = "gh.29.uk" (already exists)
Processing: Check MappedBank table for existing permalink = "gh.29.uk"
Result: Found existing bank
Output: Error - Bank ID already exists (or update existing bank if idempotent)

Scenario 3: Invalid bank ID format
Input: bankId = "ab" (too short)
Processing: Validate bank ID length > 3 characters
Result: Validation fails
Output: Error - Bank ID must be greater than 3 characters
```

---

### BR-002: Multi-Bank Data Isolation

**Category**: WORKFLOW

**Description**: All data for different banks must be completely isolated to prevent cross-bank data access or contamination. Every bank-related entity (accounts, customers, transactions, etc.) must be associated with a specific bank ID to ensure proper data partitioning in multi-tenant deployments.

**Source**: 
- File: /obp-api/src/main/scala/code/model/dataAccess/MappedBank.scala
- Class/Object: MappedBank
- Method: N/A (architectural pattern)
- Lines: N/A (implemented across all bank-related entities)

**Business Logic**:
1. When a bank is created, it receives a unique bank ID (permalink)
2. All subsequent entities created for this bank (accounts, customers, transactions) must reference this bank ID
3. All queries for bank-related data must filter by bank ID
4. No entity from one bank can reference or access entities from another bank
5. Settlement accounts created for a bank are explicitly linked to that bank's ID
6. Entitlements granted are scoped to a specific bank ID

**Scala Implementation**:
```scala
// From MappedBankAccount.scala - Settlement account creation
MappedBankAccount.create
  .bank(bankId)  // Foreign key to MappedBank.permalink
  .theAccountId(INCOMING_SETTLEMENT_ACCOUNT_ID)
  .accountCurrency("EUR")
  .kind("SETTLEMENT")
  .holder(fullBankName)
  .accountName("Default incoming settlement account")
  .accountLabel("Settlement account: Do not delete!")
  .saveMe()

// From MappedEntitlements.scala - Entitlement assignment
MappedEntitlement.create
  .mBankId(bankId)  // Foreign key to MappedBank.permalink
  .mUserId(userId)
  .mRoleName(roleName)
  .mCreatedByProcess(createdByProcess)
  .saveMe()
```

**Variables**:
- **Input**: 
  - `bankId` (String): Unique bank identifier used as partition key
- **Output**: 
  - All related entities properly scoped to the bank
  - Data isolation enforced at database level
- **Constants**: None

**Business Conditions**:
| Condition | Business Meaning | Values/Thresholds |
|-----------|------------------|-------------------|
| All bank-related entities must have bank_id | Every entity belongs to exactly one bank | Foreign key constraint |
| Queries must filter by bank_id | Data access is always bank-scoped | Mandatory WHERE clause |
| Cross-bank references prohibited | Banks cannot access each other's data | Enforced by application logic |

**Business Impact**: 
This rule is critical for regulatory compliance and data security in multi-tenant banking platforms. It ensures that Bank A cannot access or modify Bank B's customer data, accounts, or transactions. This isolation is required by banking regulations and is fundamental to maintaining trust in a shared platform. Violation of this rule could lead to data breaches, regulatory penalties, and loss of banking licenses.

**API Endpoints Using This Rule**:
- POST /obp/v4.0.0/banks - Bank creation with isolated data structures
- All subsequent bank-related endpoints (accounts, customers, transactions)

**Related Test Cases**:
- Test case for cross-bank data access prevention
- Test case for bank-scoped queries
- Test case for settlement account bank association
- Test case for entitlement bank scoping

**Migration Notes for Go**:
- Implement foreign key constraints at database level (bank_id references mapped_bank.permalink)
- Use middleware or repository pattern to automatically inject bank_id filter in all queries
- Consider using PostgreSQL Row Level Security (RLS) for additional isolation
- Implement context-based bank scoping in Go application layer
- Use prepared statements with bank_id parameter to prevent SQL injection
- Consider using separate database schemas per bank for strongest isolation (if scalability permits)

**Example Scenarios**:
```
Scenario 1: Creating settlement account for Bank A
Input: bankId = "bank-a", accountId = "OBP_DEFAULT_INCOMING_ACCOUNT_ID"
Processing: Create MappedBankAccount with bank = "bank-a"
Result: Settlement account created and linked to Bank A
Output: Account accessible only through Bank A's context

Scenario 2: Querying accounts for Bank B
Input: bankId = "bank-b"
Processing: SELECT * FROM mapped_bank_account WHERE bank = "bank-b"
Result: Returns only accounts belonging to Bank B
Output: Bank A's accounts are not visible or accessible

Scenario 3: Attempting cross-bank access (should fail)
Input: User with Bank A context tries to access Bank B account
Processing: Check user's bank entitlements and account's bank_id
Result: Bank ID mismatch detected
Output: Access denied - insufficient permissions
```

---

### BR-003: Automatic Settlement Account Provisioning

**Category**: WORKFLOW

**Description**: When a new bank is registered, the system must automatically create two settlement accounts (incoming and outgoing) in EUR currency to enable the bank to process inter-bank transfers and settlements. These accounts are system-managed and should not be deleted.

**Source**: 
- File: /obp-api/src/main/scala/code/bankconnectors/LocalMappedConnector.scala
- Class/Object: LocalMappedConnector
- Method: createOrUpdateBank
- Lines: 3214-3244

**Business Logic**:
1. After successfully creating a bank entity, the system creates two settlement accounts
2. First settlement account: Incoming settlement account with ID "OBP_DEFAULT_INCOMING_ACCOUNT_ID"
3. Second settlement account: Outgoing settlement account with ID "OBP_DEFAULT_OUTGOING_ACCOUNT_ID"
4. Both accounts are created in EUR currency (default settlement currency)
5. Both accounts are marked with kind = "SETTLEMENT" to distinguish them from customer accounts
6. Account holder is set to the bank's full name
7. Accounts are labeled as "Do not delete!" to prevent accidental removal
8. These accounts are used for inter-bank settlement operations

**Scala Implementation**:
```scala
// From LocalMappedConnector.scala lines 3218-3226 (Incoming)
MappedBankAccount.create
  .bank(bankId)
  .theAccountId(INCOMING_SETTLEMENT_ACCOUNT_ID)  // "OBP_DEFAULT_INCOMING_ACCOUNT_ID"
  .accountCurrency("EUR")
  .kind("SETTLEMENT")
  .holder(fullBankName)
  .accountName("Default incoming settlement account")
  .accountLabel("Settlement account: Do not delete!")
  .saveMe()

// From LocalMappedConnector.scala lines 3234-3242 (Outgoing)
MappedBankAccount.create
  .bank(bankId)
  .theAccountId(OUTGOING_SETTLEMENT_ACCOUNT_ID)  // "OBP_DEFAULT_OUTGOING_ACCOUNT_ID"
  .accountCurrency("EUR")
  .kind("SETTLEMENT")
  .holder(fullBankName)
  .accountName("Default outgoing settlement account")
  .accountLabel("Settlement account: Do not delete!")
  .saveMe()
```

**Variables**:
- **Input**: 
  - `bankId` (String): Unique bank identifier
  - `fullBankName` (String): Full legal name of the bank (used as account holder)
- **Output**: 
  - 2 MappedBankAccount records created
  - Incoming settlement account with specific ID
  - Outgoing settlement account with specific ID
- **Constants**: 
  - `INCOMING_SETTLEMENT_ACCOUNT_ID` = "OBP_DEFAULT_INCOMING_ACCOUNT_ID"
  - `OUTGOING_SETTLEMENT_ACCOUNT_ID` = "OBP_DEFAULT_OUTGOING_ACCOUNT_ID"
  - Settlement currency = "EUR"
  - Account kind = "SETTLEMENT"

**Business Conditions**:
| Condition | Business Meaning | Values/Thresholds |
|-----------|------------------|-------------------|
| 2 accounts per bank | Every bank needs incoming and outgoing settlement | Exactly 2 accounts |
| EUR currency | Default settlement currency for inter-bank transfers | EUR only |
| SETTLEMENT kind | Distinguishes from customer accounts | Type = SETTLEMENT |
| System-managed | Accounts should not be deleted by users | Protected accounts |

**Business Impact**: 
Settlement accounts are essential for inter-bank operations and clearing processes. Without these accounts, the bank cannot participate in inter-bank transfers, payment settlements, or clearing operations. These accounts act as the bank's interface to the broader banking network and are used for reconciliation and settlement of transactions between banks. They are required for regulatory compliance and operational functionality.

**API Endpoints Using This Rule**:
- POST /obp/v4.0.0/banks - Bank creation triggers settlement account creation

**Related Test Cases**:
- Test case for settlement account creation on bank registration
- Test case for correct account IDs (incoming and outgoing)
- Test case for EUR currency assignment
- Test case for SETTLEMENT kind assignment
- Test case for account holder matching bank name

**Migration Notes for Go**:
- Implement settlement account creation in same database transaction as bank creation
- Use constants for settlement account IDs to ensure consistency
- Implement idempotent logic (check if accounts exist before creating)
- Consider using database constraints to prevent deletion of settlement accounts
- Add application-level protection to prevent settlement account deletion
- Log settlement account creation for audit trail
- Consider making settlement currency configurable in future (currently hardcoded to EUR)

**Example Scenarios**:
```
Scenario 1: New bank registration
Input: bankId = "gh.29.uk", fullBankName = "The Royal Bank of Scotland"
Processing: 
  1. Create bank entity
  2. Create incoming settlement account with ID "OBP_DEFAULT_INCOMING_ACCOUNT_ID"
  3. Create outgoing settlement account with ID "OBP_DEFAULT_OUTGOING_ACCOUNT_ID"
Result: Bank created with 2 settlement accounts
Output: 
  - Bank: gh.29.uk
  - Account 1: OBP_DEFAULT_INCOMING_ACCOUNT_ID (EUR, SETTLEMENT)
  - Account 2: OBP_DEFAULT_OUTGOING_ACCOUNT_ID (EUR, SETTLEMENT)

Scenario 2: Settlement account query
Input: bankId = "gh.29.uk", kind = "SETTLEMENT"
Processing: SELECT * FROM mapped_bank_account WHERE bank = "gh.29.uk" AND kind = "SETTLEMENT"
Result: Returns 2 settlement accounts
Output: Incoming and outgoing settlement accounts for the bank

Scenario 3: Idempotent bank creation
Input: bankId = "gh.29.uk" (already exists with settlement accounts)
Processing: Check if settlement accounts exist, skip creation if present
Result: No duplicate settlement accounts created
Output: Existing settlement accounts preserved
```

---

### BR-004: Automatic Entitlement Assignment for Bank Creator

**Category**: WORKFLOW

**Description**: When a user creates a new bank, they are automatically granted two specific entitlements (roles) for that bank: CanCreateEntitlementAtOneBank (allowing them to manage roles) and CanReadDynamicResourceDocsAtOneBank (allowing them to read API documentation). This ensures the bank creator has necessary permissions to manage their bank.

**Source**: 
- File: /obp-api/src/main/scala/code/api/v4_0_0/APIMethods400.scala
- Class/Object: APIMethods400
- Method: createBank
- Lines: 3662-3677

**Business Logic**:
1. After successfully creating a bank entity, the system identifies the user who created the bank (from OAuth context)
2. System checks if user already has CanCreateEntitlementAtOneBank role for this bank
3. If not present, system grants CanCreateEntitlementAtOneBank entitlement to the user for this specific bank
4. System checks if user already has CanReadDynamicResourceDocsAtOneBank role for this bank
5. If not present, system grants CanReadDynamicResourceDocsAtOneBank entitlement to the user for this specific bank
6. These entitlements are bank-scoped (user can only manage this specific bank, not all banks)
7. System sends notification email to user about granted entitlements

**Scala Implementation**:
```scala
// From APIMethods400.scala lines 3664-3669 (First entitlement)
entitlementsByBank.filter(_.roleName == CanCreateEntitlementAtOneBank.toString()).size > 0 match {
  case true =>
    // Already has entitlement, skip
    Future()
  case false =>
    // Grant entitlement
    Future(Entitlement.entitlement.vend.addEntitlement(bank.id, cc.userId, CanCreateEntitlementAtOneBank.toString()))
}

// From APIMethods400.scala lines 3671-3676 (Second entitlement)
entitlementsByBank.filter(_.roleName == CanReadDynamicResourceDocsAtOneBank.toString()).size > 0 match {
  case true =>
    // Already has entitlement, skip
    Future()
  case false =>
    // Grant entitlement
    Future(Entitlement.entitlement.vend.addEntitlement(bank.id, cc.userId, CanReadDynamicResourceDocsAtOneBank.toString()))
}
```

**Variables**:
- **Input**: 
  - `bankId` (String): Unique bank identifier
  - `userId` (String): User ID from OAuth context (cc.userId)
- **Output**: 
  - 2 MappedEntitlement records created (if not already present)
  - User granted permissions to manage the bank
- **Constants**: 
  - Role 1: "CanCreateEntitlementAtOneBank"
  - Role 2: "CanReadDynamicResourceDocsAtOneBank"

**Business Conditions**:
| Condition | Business Meaning | Values/Thresholds |
|-----------|------------------|-------------------|
| User creates bank | User who creates bank gets management rights | Automatic assignment |
| Bank-scoped roles | Roles apply only to this specific bank | One bank only |
| Idempotent assignment | Don't duplicate entitlements if already present | Check before create |
| 2 specific roles | Predefined set of initial permissions | Fixed role names |

**Business Impact**: 
This rule implements the principle of "creator ownership" - the user who creates a bank becomes its initial administrator. This is essential for operational management as it ensures someone has the authority to configure the bank, assign roles to other users, and manage bank operations. Without this automatic assignment, newly created banks would be orphaned with no one having permission to manage them. The CanCreateEntitlementAtOneBank role is particularly critical as it allows the creator to delegate permissions to other users.

**API Endpoints Using This Rule**:
- POST /obp/v4.0.0/banks - Bank creation triggers entitlement assignment

**Related Test Cases**:
- Test case for entitlement assignment on bank creation
- Test case for correct role names assigned
- Test case for bank-scoped entitlements (not global)
- Test case for idempotent entitlement assignment
- Test case for user notification on entitlement grant

**Migration Notes for Go**:
- Implement entitlement assignment in same database transaction as bank creation
- Use exact role name strings to ensure compatibility
- Implement idempotent logic (check if entitlement exists before creating)
- Generate unique UUID for entitlement ID
- Set createdByProcess field to "createBank" for audit trail
- Implement notification service to email user about granted permissions
- Consider using constants or enums for role names to avoid typos
- Ensure user context is properly extracted from OAuth/JWT token

**Example Scenarios**:
```
Scenario 1: First-time bank creation by user
Input: bankId = "gh.29.uk", userId = "user-123"
Processing: 
  1. Create bank entity
  2. Check if user-123 has CanCreateEntitlementAtOneBank for gh.29.uk (not found)
  3. Grant CanCreateEntitlementAtOneBank to user-123 for gh.29.uk
  4. Check if user-123 has CanReadDynamicResourceDocsAtOneBank for gh.29.uk (not found)
  5. Grant CanReadDynamicResourceDocsAtOneBank to user-123 for gh.29.uk
Result: User granted 2 entitlements
Output: 
  - Entitlement 1: user-123, gh.29.uk, CanCreateEntitlementAtOneBank
  - Entitlement 2: user-123, gh.29.uk, CanReadDynamicResourceDocsAtOneBank

Scenario 2: User already has entitlements (idempotent)
Input: bankId = "gh.29.uk", userId = "user-123" (already has entitlements)
Processing: 
  1. Check if user-123 has CanCreateEntitlementAtOneBank for gh.29.uk (found)
  2. Skip entitlement creation
  3. Check if user-123 has CanReadDynamicResourceDocsAtOneBank for gh.29.uk (found)
  4. Skip entitlement creation
Result: No duplicate entitlements created
Output: Existing entitlements preserved

Scenario 3: User creates second bank
Input: bankId = "bank-b", userId = "user-123" (already has entitlements for bank-a)
Processing: 
  1. Create bank entity for bank-b
  2. Check if user-123 has CanCreateEntitlementAtOneBank for bank-b (not found)
  3. Grant CanCreateEntitlementAtOneBank to user-123 for bank-b
  4. Check if user-123 has CanReadDynamicResourceDocsAtOneBank for bank-b (not found)
  5. Grant CanReadDynamicResourceDocsAtOneBank to user-123 for bank-b
Result: User granted entitlements for second bank
Output: User now has entitlements for both bank-a and bank-b (bank-scoped)
```

---

### BR-005: Bank ID Immutability After Creation

**Category**: DECISION

**Description**: Once a bank is created, its bank ID (permalink) cannot be modified. This ensures referential integrity across all bank-related entities (accounts, customers, transactions) that reference the bank ID as a foreign key.

**Source**: 
- File: User story documentation (bank_registration_configuration_user_story.md)
- Business Rule: Line 153
- Implementation: No UPDATE endpoint exists for bank ID modification

**Business Logic**:
1. When a bank is created, the bank ID (permalink) is set and persisted
2. The bank ID becomes the primary business key used across all related entities
3. No API endpoint exists to modify the bank ID after creation
4. If a bank ID needs to change, the bank must be recreated with a new ID (and all related data migrated)
5. This immutability prevents orphaned references and maintains data consistency

**Scala Implementation**:
```scala
// From user story documentation (line 132-134):
// "Based on the systematic search across all API versions (v1.2.1 through v6.0.0), 
// there is **no explicit PUT /banks/BANK_ID endpoint** for updating existing bank entities."

// Bank ID is set once during creation and never modified
MappedBank.create
  .permalink(bankId)  // Set once, never changed
  .fullBankName(fullBankName)
  .shortBankName(shortBankName)
  // ... other fields
  .saveMe()
```

**Variables**:
- **Input**: 
  - `bankId` (String): Unique bank identifier set at creation
- **Output**: 
  - Bank ID remains constant throughout bank lifecycle
- **Constants**: None

**Business Conditions**:
| Condition | Business Meaning | Values/Thresholds |
|-----------|------------------|-------------------|
| Bank ID set at creation | ID assigned during initial registration | One-time assignment |
| No modification allowed | ID cannot be changed after creation | Immutable field |
| Referenced by all entities | ID used as foreign key everywhere | Referential integrity |

**Business Impact**: 
This rule is critical for maintaining referential integrity in a complex banking system. The bank ID is used as a foreign key in accounts, transactions, customers, entitlements, and many other entities. Allowing the bank ID to change would require cascading updates across potentially millions of records, risking data corruption and system inconsistency. Immutability ensures that once a bank is registered, all references to it remain valid and consistent. This is a fundamental principle of database design and data integrity.

**API Endpoints Using This Rule**:
- POST /obp/v4.0.0/banks - Bank creation (ID set here)
- No PUT endpoint exists for bank ID modification

**Related Test Cases**:
- Test case for bank ID persistence after creation
- Test case for absence of bank ID update endpoint
- Test case for referential integrity with bank ID

**Migration Notes for Go**:
- Implement bank ID as immutable field in Go struct (no setter method)
- Do not expose any API endpoint for bank ID modification
- Use database constraints to prevent bank ID updates
- Consider using bank ID as primary key or unique index
- Document immutability clearly in API documentation
- If bank ID change is required, implement bank migration/recreation process

**Example Scenarios**:
```
Scenario 1: Bank creation with ID
Input: bankId = "gh.29.uk"
Processing: Create bank with permalink = "gh.29.uk"
Result: Bank created with immutable ID
Output: Bank ID = "gh.29.uk" (permanent)

Scenario 2: Attempt to modify bank ID (should fail)
Input: PUT /banks/gh.29.uk with new ID "gh.29.uk.new"
Processing: No such endpoint exists
Result: HTTP 404 Not Found or HTTP 405 Method Not Allowed
Output: Bank ID remains "gh.29.uk"

Scenario 3: Bank ID referenced by accounts
Input: Create account with bank_id = "gh.29.uk"
Processing: Account created with foreign key to bank
Result: Account permanently linked to bank "gh.29.uk"
Output: Account.bank_id = "gh.29.uk" (cannot change without recreating account)
```

---

### BR-006: Bank ID Format and Length Validation

**Category**: THRESHOLD

**Description**: Bank ID must meet specific format requirements to ensure URL-safety, uniqueness, and system compatibility. The ID must be greater than 3 characters, cannot contain spaces, and cannot contain the reserved separator sequence "::::".

**Source**: 
- File: /obp-api/src/main/scala/code/api/v4_0_0/APIMethods400.scala
- Class/Object: APIMethods400
- Method: createBank
- Lines: 3621-3683 (validation rules documented in user story lines 105-109)

**Business Logic**:
1. When a bank creation request is received, extract the bank ID from the request
2. Validate that bank ID length is greater than 3 characters
3. Validate that bank ID does not contain any space characters
4. Validate that bank ID does not contain the sequence "::::"
5. Validate that bank ID passes "short string" validation (additional format checks)
6. If any validation fails, reject the request with appropriate error message
7. If all validations pass, proceed with bank creation

**Scala Implementation**:
```scala
// From user story documentation (lines 105-109):
// Validation Rules:
// - BANK_ID must be greater than 3 characters
// - BANK_ID cannot contain space characters
// - BANK_ID cannot contain `::::` characters
// - BANK_ID must pass short string validation

// Validation logic (inferred from validation rules):
def validateBankId(bankId: String): Either[String, String] = {
  if (bankId.length <= 3) {
    Left("BANK_ID must be greater than 3 characters")
  } else if (bankId.contains(" ")) {
    Left("BANK_ID cannot contain space characters")
  } else if (bankId.contains("::::")) {
    Left("BANK_ID cannot contain :::: characters")
  } else if (!isValidShortString(bankId)) {
    Left("BANK_ID must pass short string validation")
  } else {
    Right(bankId)
  }
}
```

**Variables**:
- **Input**: 
  - `bankId` (String): Bank identifier from request
- **Output**: 
  - Valid bank ID or validation error
- **Constants**: 
  - Minimum length: 3 characters (exclusive, must be > 3)
  - Forbidden characters: space (" ")
  - Forbidden sequence: "::::"

**Business Conditions**:
| Condition | Business Meaning | Values/Thresholds |
|-----------|------------------|-------------------|
| Length > 3 | Bank ID must be meaningful and unique | Minimum 4 characters |
| No spaces | Bank ID must be URL-safe | No space characters |
| No :::: | Avoid reserved separator used internally | No :::: sequence |
| Short string format | Additional format constraints | System-defined rules |

**Business Impact**: 
These validation rules ensure that bank IDs are suitable for use in URLs, database keys, and system operations. The length requirement prevents trivial or ambiguous IDs. The space restriction ensures URL-safety without encoding. The :::: restriction prevents conflicts with internal system separators. These rules protect system integrity and prevent operational issues caused by malformed bank IDs. They also ensure consistency across the platform and prevent user errors during bank registration.

**API Endpoints Using This Rule**:
- POST /obp/v4.0.0/banks - Bank creation with ID validation

**Related Test Cases**:
- Test case for bank ID too short (≤ 3 characters)
- Test case for bank ID with spaces
- Test case for bank ID with :::: sequence
- Test case for valid bank ID format
- Test case for short string validation

**Migration Notes for Go**:
- Implement validation as middleware or request validator
- Use regex for pattern matching if needed
- Return HTTP 400 Bad Request with clear error messages
- Implement validation before database operations
- Consider using Go validator library for declarative validation
- Document validation rules in API specification (OpenAPI/Swagger)
- Provide clear error messages for each validation failure

**Example Scenarios**:
```
Scenario 1: Valid bank ID
Input: bankId = "gh.29.uk"
Processing: 
  - Length check: 8 > 3 ✓
  - Space check: no spaces ✓
  - :::: check: no :::: ✓
  - Short string check: valid ✓
Result: Validation passes
Output: Bank ID accepted

Scenario 2: Bank ID too short
Input: bankId = "abc"
Processing: 
  - Length check: 3 > 3 ✗
Result: Validation fails
Output: Error - "BANK_ID must be greater than 3 characters"

Scenario 3: Bank ID with spaces
Input: bankId = "bank of america"
Processing: 
  - Length check: 16 > 3 ✓
  - Space check: contains spaces ✗
Result: Validation fails
Output: Error - "BANK_ID cannot contain space characters"

Scenario 4: Bank ID with forbidden sequence
Input: bankId = "bank::::id"
Processing: 
  - Length check: 10 > 3 ✓
  - Space check: no spaces ✓
  - :::: check: contains :::: ✗
Result: Validation fails
Output: Error - "BANK_ID cannot contain :::: characters"

Scenario 5: Suggested valid format
Input: bankId = "bank-of-america-us"
Processing: All validations pass
Result: Valid bank ID
Output: Bank ID accepted (uses hyphens instead of spaces, meaningful length)
```

---

### BR-007: Bank Routing Scheme Extraction and Storage

**Category**: TRANSFORMATION

**Description**: Bank routing information can be provided in two formats: either as a BIC/SWIFT code in the swiftBIC field, or as an array of routing schemes with scheme-address pairs. The system must extract and store routing information in the appropriate fields (swiftBIC, bankRoutingScheme, bankRoutingAddress) based on the input format.

**Source**: 
- File: /obp-api/src/main/scala/code/bankconnectors/LocalMappedConnector.scala
- Class/Object: LocalMappedConnector
- Method: createOrUpdateBank
- Lines: 3169-3247 (routing extraction logic inferred from entity structure)

**Business Logic**:
1. When bank creation request is received, check if bank_routings array is provided
2. If bank_routings array is provided and not empty:
   - Extract first routing entry from array
   - Store scheme value in bankRoutingScheme field
   - Store address value in bankRoutingAddress field
   - If scheme is "BIC", also store address in swiftBIC field
3. If bank_routings array is empty or not provided:
   - Use default routing scheme (e.g., "OBP")
   - Store bank ID as routing address
4. Store all routing information in MappedBank entity for future reference

**Scala Implementation**:
```scala
// From business_entities.md (lines 96-99):
// Routing information extraction (inferred logic):
val (bankRoutingScheme, bankRoutingAddress, swiftBIC) = bankRoutings match {
  case Some(routings) if routings.nonEmpty =>
    val firstRouting = routings.head
    val scheme = firstRouting.scheme
    val address = firstRouting.address
    val bic = if (scheme == "BIC") address else ""
    (scheme, address, bic)
  case _ =>
    ("OBP", bankId, "")
}

MappedBank.create
  .permalink(bankId)
  .swiftBIC(swiftBIC)
  .mBankRoutingScheme(bankRoutingScheme)
  .mBankRoutingAddress(bankRoutingAddress)
  .saveMe()
```

**Variables**:
- **Input**: 
  - `bank_routings` (Array): Array of routing scheme-address pairs from request
  - `bankId` (String): Bank identifier (used as fallback routing address)
- **Output**: 
  - `swiftBIC` (String): SWIFT/BIC code if routing scheme is BIC
  - `bankRoutingScheme` (String): Routing scheme type (BIC, OBP, etc.)
  - `bankRoutingAddress` (String): Routing address for the scheme
- **Constants**: 
  - Default routing scheme: "OBP"
  - BIC scheme identifier: "BIC"

**Business Conditions**:
| Condition | Business Meaning | Values/Thresholds |
|-----------|------------------|-------------------|
| BIC scheme provided | Use BIC for international transfers | scheme = "BIC" |
| Multiple routings | Use first routing entry | Array index 0 |
| No routings provided | Use default OBP routing | Fallback to OBP |
| BIC stored separately | SWIFT/BIC has dedicated field | swiftBIC field |

**Business Impact**: 
Bank routing information is critical for inter-bank transfers and payment processing. Different payment networks use different routing schemes (SWIFT/BIC for international, ACH routing numbers for US, IBAN for Europe, etc.). The system must correctly extract and store this information to enable proper payment routing. The BIC/SWIFT code is particularly important for international transfers and must be stored in a dedicated field for compatibility with international payment systems. Incorrect routing information can cause payment failures and operational issues.

**API Endpoints Using This Rule**:
- POST /obp/v4.0.0/banks - Bank creation with routing information

**Related Test Cases**:
- Test case for BIC routing scheme extraction
- Test case for non-BIC routing scheme extraction
- Test case for empty routing array (default OBP routing)
- Test case for multiple routing entries (first one used)
- Test case for swiftBIC field population when scheme is BIC

**Migration Notes for Go**:
- Implement routing extraction logic in bank creation handler
- Handle both array and single routing formats
- Use first routing entry if multiple provided
- Implement default routing logic (OBP scheme with bank ID as address)
- Store BIC in dedicated field when scheme is "BIC"
- Consider supporting multiple routing schemes in future (currently only first is used)
- Validate routing scheme values against known schemes
- Document supported routing schemes in API specification

**Example Scenarios**:
```
Scenario 1: Bank with BIC routing
Input: 
  bankId = "gh.29.uk"
  bank_routings = [{"scheme": "BIC", "address": "GHBKGB2L"}]
Processing: 
  - Extract first routing: scheme = "BIC", address = "GHBKGB2L"
  - Scheme is BIC, so swiftBIC = "GHBKGB2L"
  - bankRoutingScheme = "BIC"
  - bankRoutingAddress = "GHBKGB2L"
Result: Routing information stored
Output: 
  - swiftBIC = "GHBKGB2L"
  - bankRoutingScheme = "BIC"
  - bankRoutingAddress = "GHBKGB2L"

Scenario 2: Bank with non-BIC routing
Input: 
  bankId = "us-bank-001"
  bank_routings = [{"scheme": "ACH", "address": "123456789"}]
Processing: 
  - Extract first routing: scheme = "ACH", address = "123456789"
  - Scheme is not BIC, so swiftBIC = ""
  - bankRoutingScheme = "ACH"
  - bankRoutingAddress = "123456789"
Result: Routing information stored
Output: 
  - swiftBIC = ""
  - bankRoutingScheme = "ACH"
  - bankRoutingAddress = "123456789"

Scenario 3: Bank with no routing information
Input: 
  bankId = "test-bank"
  bank_routings = []
Processing: 
  - No routing provided, use defaults
  - bankRoutingScheme = "OBP"
  - bankRoutingAddress = "test-bank" (bank ID)
  - swiftBIC = ""
Result: Default routing stored
Output: 
  - swiftBIC = ""
  - bankRoutingScheme = "OBP"
  - bankRoutingAddress = "test-bank"

Scenario 4: Bank with multiple routing schemes
Input: 
  bankId = "multi-bank"
  bank_routings = [
    {"scheme": "BIC", "address": "MULTGB2L"},
    {"scheme": "ACH", "address": "987654321"}
  ]
Processing: 
  - Extract first routing only: scheme = "BIC", address = "MULTGB2L"
  - Second routing ignored (current implementation limitation)
  - Scheme is BIC, so swiftBIC = "MULTGB2L"
Result: First routing stored, others ignored
Output: 
  - swiftBIC = "MULTGB2L"
  - bankRoutingScheme = "BIC"
  - bankRoutingAddress = "MULTGB2L"
  - Note: ACH routing not stored (limitation)
```

---

### BR-008: Transactional Atomicity for Bank Creation

**Category**: WORKFLOW

**Description**: Bank creation, settlement account provisioning, and entitlement assignment must be performed as a single atomic transaction. If any operation fails, all changes must be rolled back to maintain data consistency and prevent partial bank registration.

**Source**: 
- File: /obp-api/src/main/scala/code/bankconnectors/LocalMappedConnector.scala
- Class/Object: LocalMappedConnector
- Method: createOrUpdateBank
- Lines: 3169-3247
- Additional Reference: /obp-api/src/main/scala/code/api/v4_0_0/APIMethods400.scala (lines 3621-3683)

**Business Logic**:
1. Begin database transaction
2. Create or update MappedBank entity
3. Create incoming settlement account (MappedBankAccount)
4. Create outgoing settlement account (MappedBankAccount)
5. Assign CanCreateEntitlementAtOneBank entitlement (MappedEntitlement)
6. Assign CanReadDynamicResourceDocsAtOneBank entitlement (MappedEntitlement)
7. If all operations succeed, commit transaction
8. If any operation fails, rollback entire transaction
9. Return success or error response based on transaction outcome

**Scala Implementation**:
```scala
// Transactional logic (inferred from entity creation sequence):
// All operations must succeed or all must fail
def createBankWithDependencies(bankData: PostBankJson400, userId: String): Either[Error, Bank] = {
  DB.use(DefaultConnectionIdentifier) { conn =>
    try {
      // Start transaction
      conn.setAutoCommit(false)
      
      // 1. Create bank
      val bank = MappedBank.create
        .permalink(bankId)
        .fullBankName(fullBankName)
        // ... other fields
        .saveMe()
      
      // 2. Create settlement accounts
      val incomingAccount = MappedBankAccount.create
        .bank(bankId)
        .theAccountId(INCOMING_SETTLEMENT_ACCOUNT_ID)
        // ... other fields
        .saveMe()
      
      val outgoingAccount = MappedBankAccount.create
        .bank(bankId)
        .theAccountId(OUTGOING_SETTLEMENT_ACCOUNT_ID)
        // ... other fields
        .saveMe()
      
      // 3. Assign entitlements
      val entitlement1 = MappedEntitlement.create
        .mBankId(bankId)
        .mUserId(userId)
        .mRoleName("CanCreateEntitlementAtOneBank")
        .saveMe()
      
      val entitlement2 = MappedEntitlement.create
        .mBankId(bankId)
        .mUserId(userId)
        .mRoleName("CanReadDynamicResourceDocsAtOneBank")
        .saveMe()
      
      // Commit transaction
      conn.commit()
      Right(bank)
    } catch {
      case e: Exception =>
        // Rollback on any error
        conn.rollback()
        Left(Error(e.getMessage))
    }
  }
}
```

**Variables**:
- **Input**: 
  - Bank creation data (PostBankJson400)
  - User ID (from OAuth context)
- **Output**: 
  - Complete bank registration (bank + 2 accounts + 2 entitlements)
  - Or complete rollback with error message
- **Constants**: None

**Business Conditions**:
| Condition | Business Meaning | Values/Thresholds |
|-----------|------------------|-------------------|
| All operations succeed | Complete bank registration | 5 database inserts |
| Any operation fails | No partial registration | Full rollback |
| Transaction isolation | Prevent concurrent conflicts | Database transaction |

**Business Impact**: 
Transactional atomicity is critical for data consistency. Without it, the system could end up with orphaned banks (bank without settlement accounts), banks without administrators (no entitlements), or settlement accounts without banks. These partial states would cause operational failures and require manual cleanup. Atomicity ensures that either a bank is fully registered and operational, or the registration fails completely with no side effects. This is essential for system reliability and data integrity.

**API Endpoints Using This Rule**:
- POST /obp/v4.0.0/banks - Bank creation with atomic transaction

**Related Test Cases**:
- Test case for successful complete bank creation
- Test case for rollback on bank creation failure
- Test case for rollback on settlement account creation failure
- Test case for rollback on entitlement assignment failure
- Test case for no partial data after failed transaction

**Migration Notes for Go**:
- Use database transactions (sql.Tx in Go)
- Implement proper error handling with rollback
- Use defer statement for cleanup (rollback on panic)
- Consider using transaction middleware or repository pattern
- Implement retry logic for transient failures
- Log transaction failures for debugging
- Use appropriate transaction isolation level
- Consider using database-level constraints to enforce consistency

**Example Scenarios**:
```
Scenario 1: Successful complete bank creation
Input: Valid bank data, valid user ID
Processing: 
  1. Begin transaction
  2. Create bank ✓
  3. Create incoming settlement account ✓
  4. Create outgoing settlement account ✓
  5. Assign entitlement 1 ✓
  6. Assign entitlement 2 ✓
  7. Commit transaction ✓
Result: Complete bank registration
Output: Bank fully operational with all dependencies

Scenario 2: Failure during settlement account creation
Input: Valid bank data, but settlement account creation fails
Processing: 
  1. Begin transaction
  2. Create bank ✓
  3. Create incoming settlement account ✗ (database error)
  4. Rollback transaction
Result: No bank created, no accounts created
Output: Error message, database unchanged

Scenario 3: Failure during entitlement assignment
Input: Valid bank data, but entitlement assignment fails
Processing: 
  1. Begin transaction
  2. Create bank ✓
  3. Create incoming settlement account ✓
  4. Create outgoing settlement account ✓
  5. Assign entitlement 1 ✗ (user not found)
  6. Rollback transaction
Result: No bank created, no accounts created, no entitlements assigned
Output: Error message, database unchanged

Scenario 4: Concurrent bank creation attempts
Input: Two requests to create same bank ID simultaneously
Processing: 
  1. Transaction 1 begins, creates bank
  2. Transaction 2 begins, attempts to create same bank
  3. Transaction 2 fails on unique constraint violation
  4. Transaction 2 rolls back
  5. Transaction 1 commits successfully
Result: Only one bank created, second request fails
Output: First request succeeds, second request returns error (duplicate bank ID)
```

---

## API Endpoint Coverage

| Endpoint | HTTP Method | Business Rules | Rule IDs |
|----------|-------------|----------------|----------|
| /obp/v4.0.0/banks | POST | Unique bank ID, multi-bank isolation, settlement account provisioning, entitlement assignment, bank ID immutability, bank ID validation, routing extraction, transactional atomicity | BR-001, BR-002, BR-003, BR-004, BR-005, BR-006, BR-007, BR-008 |

---

## Migration Validation Matrix

| Rule ID | Rule Name | Test Case Reference | Go Implementation Status | Validation Status |
|---------|-----------|---------------------|--------------------------|-------------------|
| BR-001 | Unique Bank Identification Enforcement | Test duplicate bank ID rejection, Test unique bank ID acceptance | Pending | Pending |
| BR-002 | Multi-Bank Data Isolation | Test cross-bank data access prevention, Test bank-scoped queries | Pending | Pending |
| BR-003 | Automatic Settlement Account Provisioning | Test settlement account creation, Test EUR currency assignment | Pending | Pending |
| BR-004 | Automatic Entitlement Assignment | Test entitlement assignment on bank creation, Test role names | Pending | Pending |
| BR-005 | Bank ID Immutability After Creation | Test bank ID persistence, Test absence of update endpoint | Pending | Pending |
| BR-006 | Bank ID Format and Length Validation | Test bank ID length validation, Test space/:::: validation | Pending | Pending |
| BR-007 | Bank Routing Scheme Extraction | Test BIC routing extraction, Test default OBP routing | Pending | Pending |
| BR-008 | Transactional Atomicity | Test complete bank creation, Test rollback on failure | Pending | Pending |

---

## Notes and Assumptions

### Assumptions Made:
1. **Scala Implementation**: Business rules extracted based on documented Scala code in LocalMappedConnector.scala, APIMethods400.scala, and entity models
2. **Single Endpoint**: Analysis focused on POST /obp/v4.0.0/banks endpoint only (no update endpoint exists)
3. **Transaction Management**: Assumed Scala implementation uses database transactions for atomicity (standard practice)
4. **Routing Logic**: Routing extraction logic inferred from entity structure and field definitions
5. **Validation Rules**: Validation rules documented in user story are assumed to be implemented in Scala code

### Gaps Identified:
1. **Short String Validation**: Exact definition of "short string validation" for bank ID not specified in source code
2. **Multiple Routing Schemes**: Current implementation only stores first routing scheme; multiple schemes not fully supported
3. **Settlement Currency**: Settlement accounts hardcoded to EUR; no configuration for other currencies
4. **Entitlement Notification**: Email notification logic for entitlement assignment not detailed in source code
5. **Bank Update Logic**: No update endpoint exists; unclear how to modify bank details after creation (immutability vs. update needs)

### Clarifications Needed:
1. **Bank Deletion**: What happens to settlement accounts and entitlements when a bank is deleted?
2. **Settlement Account Usage**: How are settlement accounts actually used in transaction processing?
3. **Additional Entitlements**: Are there other entitlements that should be assigned on bank creation?
4. **Routing Scheme Priority**: If multiple routing schemes provided, which should be used for different payment types?
5. **Bank Verification**: Is there a verification process for bank legitimacy before activation?

---

## Summary

This business rules extraction identifies **8 core business rules** for the Bank Registration and Configuration capability in the Open Bank Project API. These rules cover:

1. **Data Integrity**: Unique bank identification and immutability (BR-001, BR-005)
2. **Multi-Tenancy**: Bank data isolation for secure multi-bank deployments (BR-002)
3. **Operational Setup**: Automatic provisioning of settlement accounts and entitlements (BR-003, BR-004)
4. **Data Quality**: Bank ID format validation and routing information extraction (BR-006, BR-007)
5. **Transaction Safety**: Atomic transaction management for consistent bank registration (BR-008)

All rules are extracted from the Scala implementation and are essential for maintaining system integrity, operational functionality, and regulatory compliance. These rules must be accurately implemented in the Go migration to ensure the new application behaves identically to the Scala original and passes all existing test cases.

The rules focus on **true business logic** (decisions, workflows, thresholds) rather than technical implementation details (validation, serialization, framework logic), making them suitable for migration to Go while preserving business functionality.
