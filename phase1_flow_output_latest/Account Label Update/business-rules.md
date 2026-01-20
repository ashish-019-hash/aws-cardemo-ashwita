# Business Rules Extraction

**Extracted From**: Account Label Update User Story
**Analysis Date**: 2026-01-20
**Analyst**: Expert Business Logic Analyst
**Migration Target**: Go Application

## Executive Summary
- Total Business Rules Extracted: 5
- API Endpoints Analyzed: 2
- Rule Categories:
  - Calculations: 0
  - Decisions: 2
  - Thresholds: 1
  - Aggregations: 0
  - Workflows: 1
  - Transformations: 1

## Business Rules Catalog

### BR-001: Account Label Authorization Check

**Category**: DECISION

**Description**: Only users with appropriate permissions (e.g., owner view access) on the account can update its display label. Unauthorized users must be rejected with appropriate error messages.

**Source**: 
- File: Account Label Update User Story
- Section: Acceptance Criteria, Business Rules
- Reference: Criteria #6, Business Rule #3

**Business Logic**:
1. When a label update request is received, verify the user's identity and authentication status
2. Check if the user has appropriate view/permission access to the target account
3. If the user has owner view access or equivalent permissions, allow the update to proceed
4. If the user lacks appropriate permissions, reject the request with an authorization error

**Variables**:
- **Input**: User ID, Account ID, Bank ID, User permissions/entitlements
- **Output**: Authorization decision (allowed/denied)
- **Constants**: Required permission level (owner view access)

**Business Conditions**:
| Condition | Business Meaning | Values/Thresholds |
|-----------|------------------|-------------------|
| User has owner view access | User is authorized to modify account | Permission granted |
| User lacks appropriate permissions | User cannot modify account | Permission denied |

**Business Impact**: 
Ensures account security and data integrity by preventing unauthorized modifications to account labels. Protects customer accounts from being modified by users who should not have access.

**API Endpoints Using This Rule**:
- POST /obp/v5.1.0/banks/{BANK_ID}/accounts/{ACCOUNT_ID}/label - Label update authorization
- PUT /obp/v5.1.0/banks/{BANK_ID}/accounts/{ACCOUNT_ID} - Account update authorization

**Related Test Cases**:
- Test unauthorized user attempting label update (should fail)
- Test authorized user (owner) updating label (should succeed)
- Test user with read-only access attempting update (should fail)

**Migration Notes for Go**:
- Implement authorization middleware or handler that checks user permissions before processing label updates
- Use Go's context package to pass user authentication information through the request chain
- Return appropriate HTTP status codes (401 Unauthorized, 403 Forbidden) for authorization failures

**Example Scenarios**:
```
Scenario 1: Authorized user updates label
Input: User with owner view access, Account ID = "acc123", Bank ID = "bank456"
Processing: Check user permissions -> User has owner view -> Authorization granted
Output: Proceed with label update

Scenario 2: Unauthorized user attempts update
Input: User without account access, Account ID = "acc123", Bank ID = "bank456"
Processing: Check user permissions -> User lacks permissions -> Authorization denied
Output: Return 403 Forbidden error
```

---

### BR-002: Account Identification Validation

**Category**: DECISION

**Description**: The account to be updated must be uniquely identified by both Bank ID and Account ID. Both identifiers are required and must reference existing entities in the system.

**Source**: 
- File: Account Label Update User Story
- Section: Business Rules, Data Validations
- Reference: Business Rule #1

**Business Logic**:
1. Receive Bank ID and Account ID from the update request
2. Validate that Bank ID references an existing bank on the platform
3. Validate that Account ID references an existing account at the specified bank
4. If either validation fails, reject the request with appropriate error message
5. If both validations pass, proceed with the label update

**Variables**:
- **Input**: Bank ID (required), Account ID (required)
- **Output**: Validation result (valid/invalid), Error message if invalid
- **Constants**: None

**Business Conditions**:
| Condition | Business Meaning | Values/Thresholds |
|-----------|------------------|-------------------|
| Bank ID exists | Target bank is valid | Bank must exist in system |
| Account ID exists at bank | Target account is valid | Account must exist at specified bank |
| Both IDs valid | Account can be updated | Proceed with update |

**Business Impact**: 
Ensures data integrity by preventing updates to non-existent accounts or banks. Provides clear error feedback when invalid identifiers are provided.

**API Endpoints Using This Rule**:
- POST /obp/v5.1.0/banks/{BANK_ID}/accounts/{ACCOUNT_ID}/label - Account identification
- PUT /obp/v5.1.0/banks/{BANK_ID}/accounts/{ACCOUNT_ID} - Account identification

**Related Test Cases**:
- Test with valid Bank ID and Account ID (should succeed)
- Test with invalid Bank ID (should fail with bank not found error)
- Test with invalid Account ID (should fail with account not found error)
- Test with valid Bank ID but account at different bank (should fail)

**Migration Notes for Go**:
- Implement validation functions to check bank and account existence
- Use appropriate error types to distinguish between bank not found and account not found errors
- Consider using database lookups with proper error handling for existence checks

**Example Scenarios**:
```
Scenario 1: Valid identifiers
Input: Bank ID = "bank456" (exists), Account ID = "acc123" (exists at bank456)
Processing: Validate bank -> exists -> Validate account -> exists at bank
Output: Validation passed, proceed with update

Scenario 2: Invalid bank
Input: Bank ID = "invalid_bank", Account ID = "acc123"
Processing: Validate bank -> not found
Output: Return error "Bank not found"

Scenario 3: Invalid account
Input: Bank ID = "bank456" (exists), Account ID = "invalid_acc"
Processing: Validate bank -> exists -> Validate account -> not found
Output: Return error "Account not found at specified bank"
```

---

### BR-003: Label Value Validation

**Category**: THRESHOLD

**Description**: The new label value must meet format and length requirements. Empty or null values are rejected, and the label must comply with minimum/maximum length constraints and character restrictions.

**Source**: 
- File: Account Label Update User Story
- Section: Acceptance Criteria, Data Validations
- Reference: Criteria #3, #7

**Business Logic**:
1. Receive the new label value from the update request
2. Check that the label is not null or empty
3. Validate that the label meets minimum length requirements (if defined)
4. Validate that the label does not exceed maximum length requirements
5. Check for prohibited characters (if any restrictions apply)
6. If any validation fails, reject with appropriate error message
7. If all validations pass, proceed with the update

**Variables**:
- **Input**: New label value (string)
- **Output**: Validation result (valid/invalid), Error message if invalid
- **Constants**: Minimum length (TBD), Maximum length (TBD), Prohibited characters (TBD)

**Business Conditions**:
| Condition | Business Meaning | Values/Thresholds |
|-----------|------------------|-------------------|
| Label is not empty | Label has content | Non-null, non-empty string |
| Label meets min length | Label is meaningful | Minimum characters (TBD) |
| Label within max length | Label is not too long | Maximum characters (TBD) |
| No prohibited characters | Label is safe | No restricted characters |

**Business Impact**: 
Ensures data quality and consistency by enforcing label format standards. Prevents storage of invalid or potentially harmful label values.

**API Endpoints Using This Rule**:
- POST /obp/v5.1.0/banks/{BANK_ID}/accounts/{ACCOUNT_ID}/label - Label validation
- PUT /obp/v5.1.0/banks/{BANK_ID}/accounts/{ACCOUNT_ID} - Label validation

**Related Test Cases**:
- Test with valid label value (should succeed)
- Test with empty label (should fail)
- Test with null label (should fail)
- Test with label exceeding max length (should fail)
- Test with label containing prohibited characters (should fail if restrictions exist)

**Migration Notes for Go**:
- Implement string validation functions for length and character checks
- Use Go's strings package for validation operations
- Define constants for length limits and prohibited character patterns
- Return descriptive validation error messages

**Example Scenarios**:
```
Scenario 1: Valid label
Input: label = "Savings for Vacation"
Processing: Check not empty -> pass -> Check length -> within limits -> Check characters -> valid
Output: Validation passed

Scenario 2: Empty label
Input: label = ""
Processing: Check not empty -> fail
Output: Return error "Label cannot be empty"

Scenario 3: Label too long
Input: label = "This is an extremely long label that exceeds the maximum allowed characters..."
Processing: Check not empty -> pass -> Check length -> exceeds maximum
Output: Return error "Label exceeds maximum length"
```

---

### BR-004: Label Update Persistence Workflow

**Category**: WORKFLOW

**Description**: When a valid label update request is received and authorized, the system must persist the updated label in the account record and return confirmation with updated account details.

**Source**: 
- File: Account Label Update User Story
- Section: Acceptance Criteria
- Reference: Criteria #2, #4, #5

**Business Logic**:
1. After authorization and validation pass, retrieve the current account record
2. Apply the new label value to the account record
3. Update the timestamp to reflect the modification time
4. Persist the updated account record to the database
5. Return confirmation of successful update with updated account details
6. Handle any persistence errors gracefully with appropriate error messages

**Variables**:
- **Input**: Account ID, Bank ID, New label value
- **Output**: Updated account entity with new label, Updated timestamp, Success confirmation
- **Constants**: None

**Business Conditions**:
| Condition | Business Meaning | Values/Thresholds |
|-----------|------------------|-------------------|
| Authorization passed | User can update | Pre-condition met |
| Validation passed | Label is valid | Pre-condition met |
| Persistence successful | Update saved | Database write succeeded |

**Business Impact**: 
Ensures that label updates are reliably saved and users receive confirmation of their changes. The updated label will be displayed across all account views and interfaces.

**API Endpoints Using This Rule**:
- POST /obp/v5.1.0/banks/{BANK_ID}/accounts/{ACCOUNT_ID}/label - Label persistence
- PUT /obp/v5.1.0/banks/{BANK_ID}/accounts/{ACCOUNT_ID} - Account update persistence

**Related Test Cases**:
- Test successful label update and verify persistence
- Test that updated timestamp is set correctly
- Test that response contains updated account details
- Test database error handling during persistence

**Migration Notes for Go**:
- Use database transactions to ensure atomic updates
- Implement proper error handling for database operations
- Use Go's time package for timestamp management
- Return structured response with updated account details

**Example Scenarios**:
```
Scenario 1: Successful label update
Input: Account ID = "acc123", Bank ID = "bank456", New label = "Emergency Fund"
Processing: Retrieve account -> Update label -> Set timestamp -> Persist -> Return success
Output: {
  "account_id": "acc123",
  "bank_id": "bank456",
  "label": "Emergency Fund",
  "updated_at": "2026-01-20T14:48:00Z"
}

Scenario 2: Persistence failure
Input: Account ID = "acc123", Bank ID = "bank456", New label = "Emergency Fund"
Processing: Retrieve account -> Update label -> Persist fails (database error)
Output: Return error "Failed to update account label. Please try again."
```

---

### BR-005: Audit Trail for Label Changes

**Category**: TRANSFORMATION

**Description**: The system must maintain an audit trail of label changes for compliance purposes, recording the old value, new value, user who made the change, and timestamp.

**Source**: 
- File: Account Label Update User Story
- Section: Acceptance Criteria, Notes for Implementation
- Reference: Criteria #8

**Business Logic**:
1. Before applying the label update, capture the current (old) label value
2. Record the new label value being applied
3. Capture the user ID of the person making the change
4. Record the timestamp of the change
5. Persist the audit record to the audit trail
6. Proceed with the actual label update

**Variables**:
- **Input**: Account ID, Bank ID, Old label value, New label value, User ID, Timestamp
- **Output**: Audit record created
- **Constants**: None

**Business Conditions**:
| Condition | Business Meaning | Values/Thresholds |
|-----------|------------------|-------------------|
| Label change occurs | Audit event triggered | Any label modification |
| Audit record created | Change is logged | Compliance requirement met |

**Business Impact**: 
Ensures regulatory compliance and provides accountability for account modifications. Enables investigation of unauthorized or suspicious changes and supports audit requirements.

**API Endpoints Using This Rule**:
- POST /obp/v5.1.0/banks/{BANK_ID}/accounts/{ACCOUNT_ID}/label - Audit logging
- PUT /obp/v5.1.0/banks/{BANK_ID}/accounts/{ACCOUNT_ID} - Audit logging

**Related Test Cases**:
- Test that audit record is created on label update
- Test that audit record contains old value, new value, user ID, and timestamp
- Test that audit trail is queryable for compliance reporting

**Migration Notes for Go**:
- Implement audit logging as a separate service or middleware
- Use structured logging for audit records
- Consider using a dedicated audit table or log storage
- Ensure audit logging does not block the main update operation (consider async logging)

**Example Scenarios**:
```
Scenario 1: Audit record creation
Input: Account ID = "acc123", Old label = "My Savings", New label = "Emergency Fund", User ID = "user789"
Processing: Capture old value -> Record new value -> Log user and timestamp -> Create audit record
Output: Audit record: {
  "account_id": "acc123",
  "bank_id": "bank456",
  "old_label": "My Savings",
  "new_label": "Emergency Fund",
  "changed_by": "user789",
  "changed_at": "2026-01-20T14:48:00Z"
}
```

---

## API Endpoint Coverage

| Endpoint | HTTP Method | Business Rules | Rule IDs |
|----------|-------------|----------------|----------|
| /obp/v5.1.0/banks/{BANK_ID}/accounts/{ACCOUNT_ID}/label | POST | Authorization, Account identification, Label validation, Persistence, Audit | BR-001, BR-002, BR-003, BR-004, BR-005 |
| /obp/v5.1.0/banks/{BANK_ID}/accounts/{ACCOUNT_ID} | PUT | Authorization, Account identification, Label validation, Persistence, Audit | BR-001, BR-002, BR-003, BR-004, BR-005 |

## Migration Validation Matrix

| Rule ID | Test Case Reference | Go Implementation Status | Validation Status |
|---------|---------------------|--------------------------|-------------------|
| BR-001 | Authorization tests | Pending | Pending |
| BR-002 | Account identification tests | Pending | Pending |
| BR-003 | Label validation tests | Pending | Pending |
| BR-004 | Persistence workflow tests | Pending | Pending |
| BR-005 | Audit trail tests | Pending | Pending |

## Notes and Assumptions

1. **Label Length Limits**: The exact minimum and maximum length for account labels is not specified in the user story. This should be confirmed with SME input (see Open Questions in user story).

2. **Character Restrictions**: Whether there are prohibited characters for labels is not defined. This should be confirmed with SME input.

3. **Permission Model**: The exact permission/view model (e.g., "owner view access") should be confirmed with the existing Scala implementation.

4. **Audit Storage**: The specific storage mechanism for audit trails (separate table, log file, etc.) should be determined based on existing patterns in the codebase.

5. **Idempotency**: Label updates are naturally idempotent - updating to the same value should succeed without side effects.

6. **Concurrency**: Handling of concurrent update requests should be considered during Go implementation.

---

## Analysis Checklist

- [x] All API endpoints have been analyzed
- [x] Business rules from user story have been extracted
- [x] Each rule references specific user story sections
- [x] Each rule includes migration notes for Go
- [x] Each rule maps to test cases where applicable
- [x] No technical/framework logic is included
- [x] All rules are described in business terms
- [x] Endpoint coverage table is complete
- [x] Migration validation matrix is included
