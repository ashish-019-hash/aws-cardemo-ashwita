# Business Rules Extraction

**Extracted From**: Bank Registration and Configuration User Story
**Analysis Date**: November 25, 2025
**Analyst**: Expert Business Logic Analyst
**Migration Target**: Go Application

## Executive Summary
- Total Business Rules Extracted: 3
- API Endpoints Analyzed: 2
- Rule Categories:
  - Calculations: 0
  - Decisions: 1
  - Thresholds: 0
  - Aggregations: 0
  - Workflows: 0
  - Transformations: 0
  - Constraints: 2

## Business Rules Catalog

### BR-001: Unique Bank Identification Constraint

**Category**: DECISION

**Description**: Each Bank entity must have unique identification to prevent duplicate banks in the system and ensure data integrity.

**Source**: 
- File: bank_registration_user_story_final.md
- Section: Business Rules
- Line: 89

**Business Logic**:
1. When creating a new Bank entity, the system checks if the provided bank identification (bankId and bankCode) already exists
2. If the identification is already in use by another Bank entity, the creation is rejected
3. If the identification is unique, the Bank entity creation proceeds
4. This ensures no two Bank entities can have the same identification in the system

**Variables**:
- **Input**: 
  - bankId: Unique identifier for the Bank entity
  - bankCode: Business code for the Bank entity
- **Output**: 
  - Boolean decision: Allow creation (unique) or Reject creation (duplicate)
- **Constants**: None

**Business Conditions**:
| Condition | Business Meaning | Values/Thresholds |
|-----------|------------------|-------------------|
| bankId exists in system | Duplicate bank identifier | Must be unique across all banks |
| bankCode exists in system | Duplicate bank code | Must be unique across all banks |

**Business Impact**: 
This rule ensures data integrity and prevents confusion in the banking system. Each bank must be uniquely identifiable to avoid operational errors, incorrect transaction routing, and compliance issues. This is critical for multi-bank systems where each bank operates independently.

**API Endpoints Using This Rule**:
- POST /api/banks - Bank entity creation validates uniqueness

**Related Test Cases**:
- Test case for creating bank with duplicate bankId (should fail)
- Test case for creating bank with duplicate bankCode (should fail)
- Test case for creating bank with unique identification (should succeed)

**Migration Notes for Go**:
- Implement uniqueness check using database unique constraints or application-level validation
- Use Go's database/sql package with UNIQUE constraints on bankId and bankCode columns
- Return appropriate error when duplicate is detected (e.g., HTTP 409 Conflict)
- Consider using transactions to ensure atomicity of uniqueness check and creation

**Example Scenarios**:
```
Scenario 1: Attempting to create duplicate bank
Input: bankId = "BANK001", bankCode = "BNK001" (already exists)
Processing: Check if bankId or bankCode exists in database
Output: Rejection with error "Bank identification already exists"

Scenario 2: Creating bank with unique identification
Input: bankId = "BANK002", bankCode = "BNK002" (does not exist)
Processing: Check if bankId or bankCode exists in database
Output: Approval to proceed with bank creation
```

---

### BR-002: Required Fields for Bank Creation

**Category**: DECISION

**Description**: Identification, branding, and operational parameters must be provided when creating a Bank entity to ensure complete bank configuration.

**Source**: 
- File: bank_registration_user_story_final.md
- Section: Business Rules
- Line: 90

**Business Logic**:
1. When creating a Bank entity, the system validates that all required fields are present
2. Required fields include:
   - Identification: bankId, bankCode, bankName
   - Branding: logo, colors
   - Operational parameters: businessHours, limits, currencies
3. If any required field is missing, the creation is rejected with validation error
4. If all required fields are present, the Bank entity creation proceeds

**Variables**:
- **Input**: 
  - bankId: Bank identifier (required)
  - bankCode: Bank code (required)
  - bankName: Bank name (required)
  - branding.logo: Bank logo (required)
  - branding.colors: Bank colors (required)
  - operationalParams.businessHours: Business hours (required)
  - operationalParams.limits: Transaction limits (required)
  - operationalParams.currencies: Supported currencies (required)
- **Output**: 
  - Boolean decision: Allow creation (all fields present) or Reject creation (missing fields)
- **Constants**: List of required fields

**Business Conditions**:
| Condition | Business Meaning | Values/Thresholds |
|-----------|------------------|-------------------|
| All identification fields present | Complete bank identity | bankId, bankCode, bankName required |
| All branding fields present | Complete bank branding | logo, colors required |
| All operational params present | Complete bank configuration | businessHours, limits, currencies required |

**Business Impact**: 
This rule ensures that every Bank entity is fully configured from creation, preventing incomplete bank setups that could lead to operational issues. A bank without complete identification, branding, or operational parameters cannot function properly in the system.

**API Endpoints Using This Rule**:
- POST /api/banks - Bank entity creation validates required fields

**Related Test Cases**:
- Test case for creating bank with missing identification fields (should fail)
- Test case for creating bank with missing branding fields (should fail)
- Test case for creating bank with missing operational parameters (should fail)
- Test case for creating bank with all required fields (should succeed)

**Migration Notes for Go**:
- Implement struct validation using Go validation libraries (e.g., go-playground/validator)
- Use struct tags to mark required fields: `json:"bankId" validate:"required"`
- Return detailed validation errors indicating which fields are missing
- Consider using custom validation for nested structures (branding, operationalParams)

**Example Scenarios**:
```
Scenario 1: Creating bank with missing identification
Input: bankCode = "BNK001", bankName = "Test Bank" (bankId missing)
Processing: Validate all required fields are present
Output: Rejection with error "Required field 'bankId' is missing"

Scenario 2: Creating bank with all required fields
Input: All identification, branding, and operational parameters provided
Processing: Validate all required fields are present
Output: Approval to proceed with bank creation
```

---

### BR-003: Valid Updates Constraint

**Category**: DECISION

**Description**: Only existing Bank entities can be managed/updated to prevent operations on non-existent banks.

**Source**: 
- File: bank_registration_user_story_final.md
- Section: Business Rules
- Line: 91

**Business Logic**:
1. When updating a Bank entity, the system first verifies that the Bank entity exists
2. The system looks up the Bank entity by bankId provided in the request path
3. If the Bank entity does not exist, the update is rejected with "not found" error
4. If the Bank entity exists, the update operation proceeds
5. This prevents attempting to update non-existent banks

**Variables**:
- **Input**: 
  - bankId: Identifier of the Bank entity to update (from URL path)
  - Update data: bankName, branding, operationalParams
- **Output**: 
  - Boolean decision: Allow update (bank exists) or Reject update (bank not found)
- **Constants**: None

**Business Conditions**:
| Condition | Business Meaning | Values/Thresholds |
|-----------|------------------|-------------------|
| bankId exists in system | Bank entity is present | Must exist in database |
| bankId not found | Bank entity does not exist | Return 404 Not Found |

**Business Impact**: 
This rule ensures data integrity by preventing updates to non-existent Bank entities. It protects against errors in client applications and ensures that all update operations are performed on valid, existing banks. This is essential for maintaining system consistency and preventing orphaned data.

**API Endpoints Using This Rule**:
- PUT /api/banks/{bankId} - Bank entity update validates existence

**Related Test Cases**:
- Test case for updating non-existent bank (should fail with 404)
- Test case for updating existing bank (should succeed)
- Test case for updating with invalid bankId format (should fail)

**Migration Notes for Go**:
- Implement existence check using database query before update
- Use Go's database/sql package to query by bankId
- Return HTTP 404 Not Found if bank does not exist
- Consider using SELECT FOR UPDATE in transaction to prevent race conditions
- Handle database errors appropriately (e.g., connection errors vs not found)

**Example Scenarios**:
```
Scenario 1: Attempting to update non-existent bank
Input: bankId = "BANK999" (does not exist), update data provided
Processing: Query database for Bank entity with bankId = "BANK999"
Output: Rejection with HTTP 404 "Bank entity not found"

Scenario 2: Updating existing bank
Input: bankId = "BANK001" (exists), update data provided
Processing: Query database for Bank entity with bankId = "BANK001"
Output: Approval to proceed with bank update
```

---

## API Endpoint Coverage

| Endpoint | HTTP Method | Business Rules | Rule IDs |
|----------|-------------|----------------|----------|
| /api/banks | POST | Unique identification, Required fields validation | BR-001, BR-002 |
| /api/banks/{bankId} | PUT | Existence validation before update | BR-003 |

## Migration Validation Matrix

| Rule ID | Test Case Reference | Go Implementation Status | Validation Status |
|---------|---------------------|--------------------------|-------------------|
| BR-001 | Test duplicate bank creation | Pending | Pending |
| BR-002 | Test required fields validation | Pending | Pending |
| BR-003 | Test update non-existent bank | Pending | Pending |

## Notes and Assumptions

### Assumptions Made
1. The user story represents business requirements extracted from a Scala application
2. The business rules documented are the core rules for Bank Registration and Configuration capability
3. Additional validation rules (format validation, data type validation) are considered technical validations, not business rules per the Business_rule_prompt.md guidance

### Gaps Identified
1. Specific validation rules for identification format (e.g., bankId format, bankCode length) are not detailed
2. Specific branding element requirements (logo format, color format) are not specified
3. Specific operational parameter constraints (business hours format, limit ranges, currency codes) are not detailed
4. Business rules for concurrent updates or conflict resolution are not specified

### Clarifications Needed
1. Are there any business rules for bankId or bankCode format/pattern?
2. Are there business rules for operational parameter limits (e.g., maximum transaction limit)?
3. Are there business rules for supported currencies (e.g., must be valid ISO currency codes)?
4. Are there business rules for business hours (e.g., must be within 24-hour format)?
5. What happens if two administrators try to update the same bank simultaneously?

### Migration Considerations
1. All three business rules are constraint-based and should be implemented as validation logic in Go
2. Database schema should enforce BR-001 with UNIQUE constraints
3. Go struct validation should enforce BR-002 with required field tags
4. Go service layer should enforce BR-003 with existence checks before updates
5. Consider using Go middleware for common validation patterns
6. Ensure error responses match expected format for test case validation
