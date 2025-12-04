# Business Rules Extraction

**Extracted From**: Multi-Bank Support User Story
**Analysis Date**: December 01, 2025
**Analyst**: Expert Scala Business Logic Analyst
**Migration Target**: Go Application

## Executive Summary
- Total Business Rules Extracted: 8
- API Endpoints Analyzed: 5 (representative patterns)
- Rule Categories:
  - Calculations: 0
  - Decisions: 5
  - Thresholds: 0
  - Aggregations: 0
  - Workflows: 1
  - Transformations: 2

## Business Rules Catalog

### BR-001: Mandatory Bank Identifier for Resource Access

**Category**: DECISION

**Description**: All bank-specific resource access operations must include a valid bank identifier in the URL path. This is a fundamental architectural requirement that ensures every request is scoped to a specific banking institution.

**Source**: 
- File: ResourceAccessService.scala (inferred from user story)
- Class/Object: ResourceAccessService
- Method: validateBankContext
- Lines: N/A (derived from user story)

**Business Logic**:
1. Extract bank identifier (BANK_ID) from the URL path
2. Verify bank identifier is present and non-empty
3. If bank identifier is missing, reject the request with appropriate error
4. If bank identifier is present, proceed with bank context establishment

**Variables**:
- **Input**: bankId (string) - Bank identifier from URL path parameter
- **Output**: Validation result determining if request can proceed
- **Constants**: None

**Business Conditions**:
| Condition | Business Meaning | Values/Thresholds |
|-----------|------------------|-------------------|
| Bank ID present | Request is properly scoped to a bank | Non-empty bank identifier |
| Bank ID missing | Request lacks bank context | Empty or missing identifier |

**Business Impact**: 
This rule is the foundation of multi-tenancy in the banking API. It ensures that every operation is explicitly associated with a specific bank, preventing accidental cross-bank data access and maintaining clear audit trails for regulatory compliance.

**API Endpoints Using This Rule**:
- GET /banks/BANK_ID/accounts - Bank-scoped account access
- GET /banks/BANK_ID/accounts/ACCOUNT_ID/VIEW_ID/transactions - Bank-scoped transaction access
- GET /banks/BANK_ID/attributes - Bank-scoped attribute access
- GET /banks/BANK_ID/entitlements - Bank-scoped entitlements access
- GET /management/banks/BANK_ID/dynamic-entities - Bank-scoped dynamic entities
- All other bank-specific endpoints

**Related Test Cases**:
- Test case for request with valid bank ID proceeding
- Test case for request without bank ID returning error

**Migration Notes for Go**:
- Implement as middleware that extracts and validates bank ID from URL path
- Use Go's context package to pass bank context through the request lifecycle
- Consider using chi or gorilla/mux for URL parameter extraction

**Example Scenarios**:
```
Scenario 1: Valid bank identifier provided
Input: GET /banks/bank-001/accounts
Processing: Extract "bank-001" from path, validate presence
Output: Request proceeds with bank context established

Scenario 2: Missing bank identifier
Input: GET /banks//accounts (empty bank ID)
Processing: Extract empty string, validation fails
Output: Return 400 Bad Request - Bank identifier required
```

---

### BR-002: Bank Existence Validation

**Category**: DECISION

**Description**: Before accessing any bank-specific resources, the system must verify that the specified bank identifier corresponds to an existing, registered bank in the system.

**Source**: 
- File: BankService.scala (inferred from user story)
- Class/Object: BankService
- Method: validateBankExists
- Lines: N/A (derived from user story)

**Business Logic**:
1. Receive bank identifier from the request
2. Query bank repository to check if bank exists
3. If bank exists, proceed with the operation
4. If bank does not exist, return 404 Not Found error

**Variables**:
- **Input**: bankId (string) - Bank identifier to validate
- **Output**: Validation result (success/failure)
- **Constants**: None

**Business Conditions**:
| Condition | Business Meaning | Values/Thresholds |
|-----------|------------------|-------------------|
| Bank exists | Bank is registered and operational | Bank record found |
| Bank does not exist | Invalid or unknown bank | No bank record found |

**Business Impact**: 
This rule prevents operations against non-existent banks, ensuring data integrity and providing clear error feedback. It protects against typos in bank identifiers and attempts to access decommissioned banks.

**API Endpoints Using This Rule**:
- All bank-scoped endpoints (GET, POST, PUT, DELETE /banks/BANK_ID/*)

**Related Test Cases**:
- Test case for valid bank ID allowing access
- Test case for invalid bank ID returning 404

**Migration Notes for Go**:
- Implement as a reusable validation function called early in request processing
- Consider caching bank existence checks for performance
- Return appropriate HTTP 404 status for non-existent banks

**Example Scenarios**:
```
Scenario 1: Existing bank
Input: bankId = "bank-001"
Processing: Query bank repository, bank found
Output: Proceed with resource access

Scenario 2: Non-existent bank
Input: bankId = "invalid-bank-xyz"
Processing: Query bank repository, no bank found
Output: Return 404 Not Found - Bank does not exist
```

---

### BR-003: Data Isolation Enforcement

**Category**: DECISION

**Description**: All database queries and data operations must be scoped to the specified bank identifier, ensuring complete logical separation of data between different banking institutions sharing the same API instance.

**Source**: 
- File: DataIsolationService.scala (inferred from user story)
- Class/Object: DataIsolationService
- Method: scopeQueryToBank
- Lines: N/A (derived from user story)

**Business Logic**:
1. Receive bank identifier from the request context
2. Apply bank filter to all database queries
3. Ensure all SELECT, INSERT, UPDATE, DELETE operations include bank scope
4. Return only data belonging to the specified bank

**Variables**:
- **Input**: 
  - bankId (string) - Bank identifier for scoping
  - query (object) - Database query to be scoped
- **Output**: Bank-scoped query results
- **Constants**: None

**Business Conditions**:
| Condition | Business Meaning | Values/Thresholds |
|-----------|------------------|-------------------|
| Query scoped to bank | Data isolation maintained | Bank filter applied |
| Query not scoped | Potential data leakage | Missing bank filter |

**Business Impact**: 
This is a critical security and compliance rule. It ensures that Bank A's customers, accounts, and transactions are never visible to Bank B's users, even though both banks share the same database infrastructure. This enables cost-effective multi-tenancy while maintaining strict data privacy.

**API Endpoints Using This Rule**:
- GET /banks/BANK_ID/accounts - Returns only accounts for specified bank
- GET /banks/BANK_ID/accounts/ACCOUNT_ID/VIEW_ID/transactions - Returns only transactions for specified bank
- GET /banks/BANK_ID/attributes - Returns only attributes for specified bank
- All other bank-scoped data retrieval endpoints

**Related Test Cases**:
- Test case verifying Bank A cannot see Bank B's accounts
- Test case verifying data isolation across all resource types

**Migration Notes for Go**:
- Implement bank scoping at the repository/data access layer
- Use query builders that automatically inject bank filter
- Consider using database row-level security as additional protection
- Ensure all ORM queries include bank scope

**Example Scenarios**:
```
Scenario 1: Retrieve accounts for Bank A
Input: bankId = "bank-a", query = SELECT * FROM accounts
Processing: Apply filter WHERE bank_id = 'bank-a'
Output: Only Bank A's accounts returned

Scenario 2: Retrieve transactions for Bank B
Input: bankId = "bank-b", query = SELECT * FROM transactions
Processing: Apply filter WHERE bank_id = 'bank-b'
Output: Only Bank B's transactions returned, Bank A's transactions not visible
```

---

### BR-004: Cross-Bank Access Prevention

**Category**: DECISION

**Description**: The system must prevent users from accessing resources that belong to a different bank than the one specified in the request URL. Even if a user has valid credentials, they cannot access Bank B's resources through Bank A's endpoint.

**Source**: 
- File: ResourceAccessService.scala (inferred from user story)
- Class/Object: ResourceAccessService
- Method: validateResourceBankOwnership
- Lines: N/A (derived from user story)

**Business Logic**:
1. Extract bank identifier from request URL (BANK_ID)
2. Retrieve the requested resource (account, transaction, etc.)
3. Verify the resource's bank_id matches the URL's BANK_ID
4. If mismatch, reject access with appropriate error
5. If match, allow access to proceed

**Variables**:
- **Input**: 
  - urlBankId (string) - Bank ID from URL path
  - resourceBankId (string) - Bank ID associated with the resource
- **Output**: Access decision (allow/deny)
- **Constants**: None

**Business Conditions**:
| Condition | Business Meaning | Values/Thresholds |
|-----------|------------------|-------------------|
| Bank IDs match | Resource belongs to requested bank | urlBankId == resourceBankId |
| Bank IDs mismatch | Cross-bank access attempt | urlBankId != resourceBankId |

**Business Impact**: 
This rule provides defense-in-depth against cross-bank data access. Even if a resource ID is somehow known or guessed, the system will reject access if the bank context doesn't match. This protects against both accidental and malicious cross-bank access attempts.

**API Endpoints Using This Rule**:
- GET /banks/BANK_ID/accounts/ACCOUNT_ID - Verify account belongs to bank
- GET /banks/BANK_ID/accounts/ACCOUNT_ID/VIEW_ID/transactions/TRANSACTION_ID - Verify transaction belongs to bank
- PUT /banks/BANK_ID/attributes/ATTRIBUTE_ID - Verify attribute belongs to bank
- All endpoints accessing specific resources by ID

**Related Test Cases**:
- Test case for accessing own bank's resource (success)
- Test case for accessing other bank's resource (denied)

**Migration Notes for Go**:
- Implement ownership validation in service layer before returning data
- Return 404 (not 403) to avoid revealing resource existence to unauthorized banks
- Log cross-bank access attempts for security monitoring

**Example Scenarios**:
```
Scenario 1: Valid same-bank access
Input: URL = /banks/bank-a/accounts/acc-123, account acc-123 belongs to bank-a
Processing: Compare bank-a == bank-a, match confirmed
Output: Access allowed, account details returned

Scenario 2: Cross-bank access attempt
Input: URL = /banks/bank-a/accounts/acc-456, account acc-456 belongs to bank-b
Processing: Compare bank-a != bank-b, mismatch detected
Output: Access denied, return 404 Not Found
```

---

### BR-005: Bank-Scoped Entitlements

**Category**: DECISION

**Description**: User permissions and entitlements are scoped to specific banks. A user's permission to perform an action at Bank A does not grant them the same permission at Bank B. Each bank-user combination has its own set of entitlements.

**Source**: 
- File: AuthorizationService.scala (inferred from user story)
- Class/Object: AuthorizationService
- Method: checkBankScopedEntitlement
- Lines: N/A (derived from user story)

**Business Logic**:
1. Identify the user from authentication context
2. Identify the bank from request URL (BANK_ID)
3. Identify the required entitlement for the operation
4. Check if user has the specific entitlement for the specific bank
5. If entitled, allow operation; if not, deny with 403 Forbidden

**Variables**:
- **Input**: 
  - userId (string) - Authenticated user identifier
  - bankId (string) - Bank identifier from URL
  - entitlement (string) - Required permission (e.g., canCreateAccount)
- **Output**: Authorization decision (allow/deny)
- **Constants**: None

**Business Conditions**:
| Condition | Business Meaning | Values/Thresholds |
|-----------|------------------|-------------------|
| User has entitlement for bank | User authorized for operation at this bank | Entitlement record exists |
| User lacks entitlement for bank | User not authorized for operation at this bank | No entitlement record |

**Business Impact**: 
This rule enables fine-grained access control in a multi-bank environment. A bank employee can have full access to their own bank's resources while having no access to other banks. This supports scenarios where a user might work with multiple banks but with different permission levels at each.

**API Endpoints Using This Rule**:
- POST /banks/BANK_ID/accounts - Requires canCreateAccount at BANK_ID
- PUT /banks/BANK_ID/accounts/ACCOUNT_ID - Requires canUpdateAccount at BANK_ID
- DELETE /banks/BANK_ID/attributes/ATTRIBUTE_ID - Requires canDeleteBankAttribute at BANK_ID
- All bank-scoped write operations

**Related Test Cases**:
- Test case for user with entitlement at Bank A accessing Bank A (success)
- Test case for user with entitlement at Bank A accessing Bank B (denied)
- Test case for user with entitlements at multiple banks

**Migration Notes for Go**:
- Implement entitlement checks as middleware or service layer validation
- Store entitlements with composite key (userId, bankId, entitlement)
- Consider caching entitlements per user session for performance

**Example Scenarios**:
```
Scenario 1: User with bank-specific entitlement
Input: userId = "user-1", bankId = "bank-a", entitlement = "canCreateAccount"
Processing: Check entitlements table for (user-1, bank-a, canCreateAccount), found
Output: Operation allowed

Scenario 2: User without entitlement at specific bank
Input: userId = "user-1", bankId = "bank-b", entitlement = "canCreateAccount"
Processing: Check entitlements table for (user-1, bank-b, canCreateAccount), not found
Output: Return 403 Forbidden - User not authorized for this operation at this bank

Scenario 3: User with entitlement at one bank, not another
Input: User has canCreateAccount at bank-a, attempts operation at bank-b
Processing: Check entitlements for bank-b, not found despite having it at bank-a
Output: Return 403 Forbidden - Entitlements are bank-specific
```

---

### BR-006: Bank-Scoped Resource Ownership

**Category**: TRANSFORMATION

**Description**: All resources (accounts, transactions, attributes, customers, etc.) belong to exactly one bank. When a resource is created, it is permanently associated with the bank specified in the creation request.

**Source**: 
- File: ResourceAccessService.scala (inferred from user story)
- Class/Object: ResourceAccessService
- Method: createBankScopedResource
- Lines: N/A (derived from user story)

**Business Logic**:
1. Receive resource creation request with bank identifier
2. Associate the new resource with the specified bank
3. Store bank_id as part of the resource record
4. Resource cannot be transferred to another bank after creation

**Variables**:
- **Input**: 
  - bankId (string) - Bank identifier from URL
  - resourceData (object) - Resource details to create
- **Output**: Created resource with bank association
- **Constants**: None

**Business Conditions**:
| Condition | Business Meaning | Values/Thresholds |
|-----------|------------------|-------------------|
| Resource created with bank_id | Resource belongs to specific bank | bank_id field populated |
| Resource bank association | Permanent ownership | Cannot be changed |

**Business Impact**: 
This rule establishes clear ownership of all data in the system. Every piece of data has a definitive bank owner, enabling accurate reporting, compliance audits, and data governance. It also simplifies data isolation by making bank ownership explicit at the data level.

**API Endpoints Using This Rule**:
- POST /banks/BANK_ID/accounts - Create account for specific bank
- POST /banks/BANK_ID/attribute - Create attribute for specific bank
- POST /banks/BANK_ID/transactions - Create transaction for specific bank
- All resource creation endpoints

**Related Test Cases**:
- Test case for resource creation with correct bank association
- Test case verifying resource bank_id cannot be modified

**Migration Notes for Go**:
- Include bank_id as required field in all resource structs
- Set bank_id from URL path during creation, not from request body
- Implement database constraints to prevent null bank_id

**Example Scenarios**:
```
Scenario 1: Create account for Bank A
Input: POST /banks/bank-a/accounts with account details
Processing: Create account record with bank_id = "bank-a"
Output: Account created, permanently associated with Bank A

Scenario 2: Create attribute for Bank B
Input: POST /banks/bank-b/attribute with attribute details
Processing: Create attribute record with bank_id = "bank-b"
Output: Attribute created, permanently associated with Bank B
```

---

### BR-007: Independent Bank Operations

**Category**: WORKFLOW

**Description**: Operations performed on one bank's resources do not affect other banks' data or operations. Each bank operates independently within the shared infrastructure, with complete isolation of business processes.

**Source**: 
- File: DataIsolationService.scala (inferred from user story)
- Class/Object: DataIsolationService
- Method: executeIsolatedOperation
- Lines: N/A (derived from user story)

**Business Logic**:
1. Receive operation request for specific bank
2. Execute operation within bank's data scope
3. Ensure operation does not read, modify, or affect other banks' data
4. Transaction boundaries are bank-scoped
5. Failures in one bank's operations do not cascade to other banks

**Variables**:
- **Input**: 
  - bankId (string) - Bank identifier
  - operation (object) - Operation to execute
- **Output**: Operation result scoped to bank
- **Constants**: None

**Business Conditions**:
| Condition | Business Meaning | Values/Thresholds |
|-----------|------------------|-------------------|
| Operation isolated | Bank operations independent | No cross-bank effects |
| Failure contained | One bank's issues don't affect others | Error isolation |

**Business Impact**: 
This rule ensures operational independence between banks. If Bank A experiences high load or errors, Bank B's operations continue unaffected. This is critical for SLA compliance and ensures that issues at one bank don't create cascading failures across the platform.

**API Endpoints Using This Rule**:
- All bank-scoped endpoints
- Batch operations within a bank
- Report generation for specific banks

**Related Test Cases**:
- Test case for concurrent operations on different banks
- Test case for error isolation between banks

**Migration Notes for Go**:
- Use database transactions scoped to single bank operations
- Implement proper error handling that doesn't leak across bank boundaries
- Consider separate connection pools per bank for complete isolation (optional)

**Example Scenarios**:
```
Scenario 1: Concurrent operations on different banks
Input: Bank A processing 1000 transactions, Bank B processing 500 transactions
Processing: Operations execute independently, no shared state
Output: Both banks complete their operations without interference

Scenario 2: Error in one bank doesn't affect another
Input: Bank A transaction fails due to validation error
Processing: Error handled within Bank A's context
Output: Bank B's concurrent operations continue unaffected
```

---

### BR-008: Single API Instance Multi-Bank Support

**Category**: TRANSFORMATION

**Description**: Multiple banking institutions share the same API infrastructure and codebase while maintaining complete data separation. The system architecture supports hosting multiple banks without requiring separate deployments.

**Source**: 
- File: BankService.scala (inferred from user story)
- Class/Object: BankService
- Method: resolveBankContext
- Lines: N/A (derived from user story)

**Business Logic**:
1. Single API deployment serves all registered banks
2. Bank context is determined per-request from URL path
3. Same codebase handles all banks with bank-specific data routing
4. No code changes required to add new banks
5. Banks are added through configuration/data, not deployment

**Variables**:
- **Input**: 
  - request (object) - Incoming API request
  - bankId (string) - Bank identifier from URL
- **Output**: Bank-contextualized response
- **Constants**: None

**Business Conditions**:
| Condition | Business Meaning | Values/Thresholds |
|-----------|------------------|-------------------|
| Single deployment | Shared infrastructure | One API instance |
| Multiple banks | Multi-tenant support | N banks supported |
| Data separation | Logical isolation | Per-bank data scoping |

**Business Impact**: 
This architectural rule enables cost-effective scaling of the banking platform. New banks can be onboarded without infrastructure changes, reducing time-to-market and operational costs. It also simplifies maintenance as updates are deployed once and benefit all banks.

**API Endpoints Using This Rule**:
- All endpoints - single codebase serves all banks
- Bank registration endpoints for adding new banks
- System-wide configuration endpoints

**Related Test Cases**:
- Test case for multiple banks operating simultaneously
- Test case for adding new bank without code changes

**Migration Notes for Go**:
- Design Go application with multi-tenancy from the start
- Use middleware to establish bank context early in request lifecycle
- Implement bank configuration as data, not code
- Consider feature flags for bank-specific customizations

**Example Scenarios**:
```
Scenario 1: Multiple banks on single instance
Input: Bank A, Bank B, Bank C all registered
Processing: Single API instance handles requests for all three banks
Output: Each bank's requests processed with appropriate data isolation

Scenario 2: Adding new bank
Input: Register Bank D in the system
Processing: Add bank record to database, no code deployment needed
Output: Bank D immediately operational on existing API instance
```

---

## API Endpoint Coverage

| Endpoint | HTTP Method | Business Rules | Rule IDs |
|----------|-------------|----------------|----------|
| /banks/BANK_ID/accounts | GET | Mandatory bank ID, Bank existence, Data isolation, Cross-bank prevention, Entitlements | BR-001, BR-002, BR-003, BR-004, BR-005 |
| /banks/BANK_ID/accounts/ACCOUNT_ID/VIEW_ID/transactions | GET | Mandatory bank ID, Bank existence, Data isolation, Cross-bank prevention, Entitlements | BR-001, BR-002, BR-003, BR-004, BR-005 |
| /banks/BANK_ID/attributes | GET | Mandatory bank ID, Bank existence, Data isolation | BR-001, BR-002, BR-003 |
| /banks/BANK_ID/entitlements | GET | Mandatory bank ID, Bank existence, Bank-scoped entitlements | BR-001, BR-002, BR-005 |
| /management/banks/BANK_ID/dynamic-entities | GET | Mandatory bank ID, Bank existence, Data isolation | BR-001, BR-002, BR-003 |
| All POST /banks/BANK_ID/* | POST | Resource ownership, Independent operations | BR-006, BR-007 |
| All endpoints | ALL | Single instance multi-bank | BR-008 |

## Migration Validation Matrix

| Rule ID | Test Case Reference | Go Implementation Status | Validation Status |
|---------|---------------------|--------------------------|-------------------|
| BR-001 | TestMandatoryBankIdentifier | Pending | Pending |
| BR-002 | TestBankExistenceValidation | Pending | Pending |
| BR-003 | TestDataIsolationEnforcement | Pending | Pending |
| BR-004 | TestCrossBankAccessPrevention | Pending | Pending |
| BR-005 | TestBankScopedEntitlements | Pending | Pending |
| BR-006 | TestBankScopedResourceOwnership | Pending | Pending |
| BR-007 | TestIndependentBankOperations | Pending | Pending |
| BR-008 | TestSingleInstanceMultiBank | Pending | Pending |

## Notes and Assumptions

1. **Source Code Assumption**: Since the user story does not include actual Scala source code, business rules were derived from the documented business logic, acceptance criteria, and technical context in the user story.

2. **Cross-Cutting Concern**: Multi-bank support is described as a cross-cutting architectural concern. The business rules extracted here apply to virtually all bank-specific endpoints in the system.

3. **Service Layer Inference**: The BankService, AuthorizationService, ResourceAccessService, and DataIsolationService classes are inferred from the user story's technical context section.

4. **Authentication Exclusion**: User authentication (verifying user identity) is mentioned as a dependency but not extracted as a business rule since it's a prerequisite security concern handled separately from multi-bank logic.

5. **Representative Endpoints**: The user story lists representative endpoint patterns. The business rules apply to all bank-scoped endpoints following these patterns, not just the specific examples listed.

6. **Questions for SME**: The user story includes questions about bank limits, identifier assignment, multi-bank user access, shared resources, isolation testing, and bank deactivation. These should be clarified before Go implementation.

7. **Security Consideration**: BR-004 recommends returning 404 instead of 403 for cross-bank access attempts to avoid revealing resource existence. This is a security best practice that should be confirmed with the security team.

8. **Performance Consideration**: Several rules mention caching opportunities (bank existence, entitlements). These should be implemented carefully to balance performance with data freshness requirements.
