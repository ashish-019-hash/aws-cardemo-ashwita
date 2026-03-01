# Business Rules Extraction

**Extracted From**: Branch Management Capability (Scala Application)
**Analysis Date**: 2026-01-20
**Analyst**: Expert Scala Business Logic Analyst
**Migration Target**: Go Application

## Executive Summary
- Total Business Rules Extracted: 8
- API Endpoints Analyzed: 5
- Rule Categories:
  - Calculations: 0
  - Decisions: 3
  - Thresholds: 0
  - Aggregations: 0
  - Workflows: 2
  - Transformations: 3

## Business Rules Catalog

### BR-001: Bank Existence Validation for Branch Creation

**Category**: DECISION

**Description**: A branch can only be created for an existing bank. The bank_id must reference a valid bank in the system before any branch operations can proceed.

**Source**: 
- File: APIMethods210.scala
- Class/Object: APIMethods210
- Method: createBranch
- Lines: Branch creation endpoint implementation

**Business Logic**:
1. When a branch creation request is received, the system first validates that the specified bank_id exists in the system
2. If the bank does not exist, the operation is rejected with a "Bank not found" error
3. Only after bank validation passes can the branch creation proceed

**Variables**:
- **Input**: bank_id (String) - The unique identifier of the bank where the branch will be created
- **Output**: Boolean validation result - determines if branch creation can proceed
- **Constants**: None

**Business Conditions**:
| Condition | Business Meaning | Values/Thresholds |
|-----------|------------------|-------------------|
| bank_id exists | Bank must be registered in system | Valid bank_id reference |

**Business Impact**: 
Ensures data integrity by preventing orphan branch records. Maintains referential integrity between banks and their branches in the banking hierarchy.

**API Endpoints Using This Rule**:
- POST /obp/v2.1.0/banks/{BANK_ID}/branches - Create Branch

**Related Test Cases**:
- Test case for branch creation with valid bank_id
- Test case for branch creation with invalid bank_id (should fail)

**Migration Notes for Go**:
- Implement bank existence check before branch creation
- Use appropriate error handling for bank not found scenarios
- Consider using database foreign key constraints in Go implementation

**Example Scenarios**:
```
Scenario 1: Valid bank exists
Input: bank_id = "bank-123" (exists in system)
Processing: Check bank existence -> Bank found
Output: Validation passes, proceed with branch creation

Scenario 2: Bank does not exist
Input: bank_id = "invalid-bank" (not in system)
Processing: Check bank existence -> Bank not found
Output: Validation fails, return 404 Bank Not Found error
```

---

### BR-002: Branch Uniqueness Constraint

**Category**: DECISION

**Description**: Each branch must have a unique combination of bank_id and branch_id. Duplicate branch IDs within the same bank are not allowed.

**Source**: 
- File: MappedBranchesProvider.scala
- Class/Object: MappedBranchesProvider
- Method: createOrUpdateBranch
- Lines: Branch persistence logic

**Business Logic**:
1. When creating a new branch, check if a branch with the same branch_id already exists for the given bank_id
2. If a duplicate is found during creation, reject the operation
3. For updates, the existing branch record is modified rather than creating a duplicate

**Variables**:
- **Input**: bank_id (String), branch_id (String) - Composite key for branch identification
- **Output**: Boolean - indicates if the branch_id is unique within the bank
- **Constants**: None

**Business Conditions**:
| Condition | Business Meaning | Values/Thresholds |
|-----------|------------------|-------------------|
| (bank_id, branch_id) is unique | No duplicate branches within same bank | Composite unique constraint |

**Business Impact**: 
Prevents data duplication and ensures each branch can be uniquely identified within a bank's branch network. Critical for accurate branch lookups and operations.

**API Endpoints Using This Rule**:
- POST /obp/v2.1.0/banks/{BANK_ID}/branches - Create Branch

**Related Test Cases**:
- Test case for creating branch with unique ID
- Test case for creating branch with duplicate ID (should fail)

**Migration Notes for Go**:
- Implement composite unique constraint on (bank_id, branch_id)
- Handle duplicate key errors appropriately
- Consider database-level unique index for enforcement

**Example Scenarios**:
```
Scenario 1: Unique branch creation
Input: bank_id = "bank-123", branch_id = "branch-001" (not existing)
Processing: Check uniqueness -> No duplicate found
Output: Branch creation proceeds

Scenario 2: Duplicate branch attempt
Input: bank_id = "bank-123", branch_id = "branch-001" (already exists)
Processing: Check uniqueness -> Duplicate found
Output: Creation rejected with duplicate error
```

---

### BR-003: Entitlement-Based Authorization for Branch Operations

**Category**: DECISION

**Description**: Branch write operations (create, update, delete) require specific entitlements. Users must have appropriate permissions granted for the specific bank or system-wide access.

**Source**: 
- File: APIMethods210.scala, APIMethods310.scala
- Class/Object: APIMethods210, APIMethods310
- Method: createBranch, updateBranch, deleteBranch
- Lines: Authorization checks in endpoint implementations

**Business Logic**:
1. For branch creation: User must have CanCreateBranch entitlement for the specific bank OR CanCreateBranchAtAnyBank for system-wide access
2. For branch update: User must have CanUpdateBranch entitlement
3. For branch deletion: User must have CanDeleteBranch entitlement for the specific bank OR CanDeleteBranchAtAnyBank for system-wide access
4. If entitlement check fails, return 403 Forbidden error

**Variables**:
- **Input**: user_id (String), bank_id (String), operation_type (String)
- **Output**: Boolean - authorization decision
- **Constants**: 
  - CanCreateBranch, CanCreateBranchAtAnyBank
  - CanUpdateBranch
  - CanDeleteBranch, CanDeleteBranchAtAnyBank

**Business Conditions**:
| Condition | Business Meaning | Values/Thresholds |
|-----------|------------------|-------------------|
| hasEntitlement(CanCreateBranch, bank_id) | User can create branches for specific bank | Bank-specific permission |
| hasEntitlement(CanCreateBranchAtAnyBank) | User can create branches for any bank | System-wide permission |
| hasEntitlement(CanUpdateBranch) | User can update branch information | Update permission |
| hasEntitlement(CanDeleteBranch, bank_id) | User can delete branches for specific bank | Bank-specific permission |
| hasEntitlement(CanDeleteBranchAtAnyBank) | User can delete branches for any bank | System-wide permission |

**Business Impact**: 
Enforces role-based access control for branch management. Ensures only authorized personnel can modify branch data, maintaining security and audit compliance.

**API Endpoints Using This Rule**:
- POST /obp/v2.1.0/banks/{BANK_ID}/branches - Create Branch
- PUT /obp/v2.1.0/banks/{BANK_ID}/branches/{BRANCH_ID} - Update Branch
- DELETE /obp/v3.1.0/banks/{BANK_ID}/branches/{BRANCH_ID} - Delete Branch

**Related Test Cases**:
- Test case for branch creation with valid entitlement
- Test case for branch creation without entitlement (should fail with 403)
- Test case for branch update with valid entitlement
- Test case for branch deletion with bank-specific vs system-wide entitlement

**Migration Notes for Go**:
- Implement entitlement checking middleware or service
- Support both bank-specific and system-wide entitlements
- Return appropriate HTTP 403 status for authorization failures

**Example Scenarios**:
```
Scenario 1: User with bank-specific entitlement
Input: user has CanCreateBranch for bank-123, attempting to create branch in bank-123
Processing: Check entitlement -> Found bank-specific permission
Output: Authorization granted, proceed with creation

Scenario 2: User with system-wide entitlement
Input: user has CanCreateBranchAtAnyBank, attempting to create branch in bank-456
Processing: Check entitlement -> Found system-wide permission
Output: Authorization granted, proceed with creation

Scenario 3: User without required entitlement
Input: user has no branch creation entitlements, attempting to create branch
Processing: Check entitlement -> No permission found
Output: Authorization denied, return 403 Forbidden
```

---

### BR-004: Soft Delete Pattern for Branch Deletion

**Category**: WORKFLOW

**Description**: Branch deletion does not physically remove records but sets the isDeleted flag to true. Deleted branches are excluded from list queries but remain in the database for audit purposes.

**Source**: 
- File: MappedBranchesProvider.scala
- Class/Object: MappedBranchesProvider
- Method: deleteBranch
- Lines: Delete operation implementation

**Business Logic**:
1. When a delete request is received, locate the branch by bank_id and branch_id
2. Instead of physically removing the record, set the isDeleted flag to true
3. Save the updated record back to the database
4. List queries filter out records where isDeleted = true
5. Direct lookups may still return deleted branches (implementation dependent)

**Variables**:
- **Input**: bank_id (String), branch_id (String)
- **Output**: Updated branch record with isDeleted = true
- **Constants**: isDeleted flag values: true (deleted), false (active)

**Business Conditions**:
| Condition | Business Meaning | Values/Thresholds |
|-----------|------------------|-------------------|
| isDeleted = false | Branch is active and visible | Default state |
| isDeleted = true | Branch is soft-deleted | Excluded from listings |

**Business Impact**: 
Maintains audit trail and allows for potential data recovery. Supports compliance requirements for data retention while removing branches from active use.

**API Endpoints Using This Rule**:
- DELETE /obp/v3.1.0/banks/{BANK_ID}/branches/{BRANCH_ID} - Delete Branch
- GET /obp/v1.4.0/banks/{BANK_ID}/branches - List Branches (filters deleted)

**Related Test Cases**:
- Test case for deleting a branch (verify isDeleted flag set)
- Test case for listing branches (verify deleted branches excluded)
- Test case for direct branch lookup after deletion

**Migration Notes for Go**:
- Implement isDeleted boolean field in branch model
- Add filter condition to list queries: WHERE isDeleted = false
- Consider adding deletedAt timestamp for audit purposes
- Implement appropriate response (204 No Content) for successful deletion

**Example Scenarios**:
```
Scenario 1: Soft delete a branch
Input: DELETE request for bank_id = "bank-123", branch_id = "branch-001"
Processing: Find branch -> Set isDeleted = true -> Save
Output: Branch marked as deleted, return 204 No Content

Scenario 2: List branches after deletion
Input: GET request for all branches of bank_id = "bank-123"
Processing: Query branches WHERE bank_id = "bank-123" AND isDeleted = false
Output: List excludes soft-deleted branches
```

---

### BR-005: Accessibility Indicator Tristate Logic

**Category**: TRANSFORMATION

**Description**: The isAccessible field uses a tristate value system to indicate branch accessibility status: "Y" for accessible, "N" for not accessible, and empty string for unknown/not specified.

**Source**: 
- File: Branches.scala, MappedBranchesProvider.scala
- Class/Object: Branch case class
- Method: Branch data model and serialization
- Lines: isAccessible field definition

**Business Logic**:
1. When creating or updating a branch, the isAccessible field accepts three possible values
2. "Y" indicates the branch is fully accessible (wheelchair access, etc.)
3. "N" indicates the branch is not accessible
4. Empty string "" indicates accessibility status is unknown or not specified
5. This tristate allows for gradual data collection without forcing false information

**Variables**:
- **Input**: isAccessible (String) - Accessibility indicator from request
- **Output**: isAccessible (String) - Stored and returned in responses
- **Constants**: "Y" (accessible), "N" (not accessible), "" (unknown)

**Business Conditions**:
| Condition | Business Meaning | Values/Thresholds |
|-----------|------------------|-------------------|
| isAccessible = "Y" | Branch is accessible | Positive confirmation |
| isAccessible = "N" | Branch is not accessible | Negative confirmation |
| isAccessible = "" | Accessibility unknown | No information available |

**Business Impact**: 
Enables accurate representation of accessibility information for customers with disabilities. Supports compliance with accessibility regulations and helps customers make informed decisions about branch visits.

**API Endpoints Using This Rule**:
- POST /obp/v2.1.0/banks/{BANK_ID}/branches - Create Branch
- PUT /obp/v2.1.0/banks/{BANK_ID}/branches/{BRANCH_ID} - Update Branch
- GET /obp/v2.1.0/banks/{BANK_ID}/branches/{BRANCH_ID} - Retrieve Branch

**Related Test Cases**:
- Test case for creating branch with isAccessible = "Y"
- Test case for creating branch with isAccessible = "N"
- Test case for creating branch with isAccessible = "" (empty)

**Migration Notes for Go**:
- Use string type for isAccessible field (not boolean)
- Validate input to only accept "Y", "N", or empty string
- Consider using custom type or enum for type safety

**Example Scenarios**:
```
Scenario 1: Accessible branch
Input: isAccessible = "Y"
Processing: Store "Y" value
Output: Branch marked as accessible in response

Scenario 2: Not accessible branch
Input: isAccessible = "N"
Processing: Store "N" value
Output: Branch marked as not accessible in response

Scenario 3: Unknown accessibility
Input: isAccessible = ""
Processing: Store empty string
Output: Branch accessibility shown as unknown in response
```

---

### BR-006: Default Branch Routing Assignment

**Category**: TRANSFORMATION

**Description**: If branch routing scheme and address are not provided during branch creation, the system defaults to using "BRANCH_ID" as the scheme and the branch_id as the address.

**Source**: 
- File: MappedBranchesProvider.scala
- Class/Object: MappedBranchesProvider
- Method: createOrUpdateBranch
- Lines: Branch routing default logic

**Business Logic**:
1. When creating a branch, check if branch_routing scheme and address are provided
2. If branch_routing is not provided or is empty, apply defaults
3. Default scheme = "BRANCH_ID"
4. Default address = the branch_id value
5. This ensures every branch has a valid routing configuration

**Variables**:
- **Input**: branch_routing.scheme (String, optional), branch_routing.address (String, optional), branch_id (String)
- **Output**: branch_routing object with scheme and address populated
- **Constants**: Default scheme = "BRANCH_ID"

**Business Conditions**:
| Condition | Business Meaning | Values/Thresholds |
|-----------|------------------|-------------------|
| branch_routing not provided | Use default routing | Apply defaults |
| branch_routing.scheme empty | Use BRANCH_ID scheme | Default scheme |
| branch_routing.address empty | Use branch_id as address | Default address |

**Business Impact**: 
Ensures all branches have consistent routing information for integration with payment systems and inter-branch operations. Simplifies branch creation by not requiring routing details upfront.

**API Endpoints Using This Rule**:
- POST /obp/v2.1.0/banks/{BANK_ID}/branches - Create Branch

**Related Test Cases**:
- Test case for branch creation without routing (verify defaults applied)
- Test case for branch creation with custom routing (verify custom values used)

**Migration Notes for Go**:
- Implement default value assignment in branch creation logic
- Check for nil/empty routing before applying defaults
- Ensure defaults are applied before persistence

**Example Scenarios**:
```
Scenario 1: No routing provided
Input: branch_id = "branch-001", branch_routing = null
Processing: Apply defaults -> scheme = "BRANCH_ID", address = "branch-001"
Output: Branch created with default routing

Scenario 2: Custom routing provided
Input: branch_id = "branch-001", branch_routing = {scheme: "ABA", address: "123456789"}
Processing: Use provided values
Output: Branch created with custom routing {scheme: "ABA", address: "123456789"}
```

---

### BR-007: Public Access Configuration for Branch Retrieval

**Category**: DECISION

**Description**: Branch retrieval (GET) operations can be configured as public or require authentication based on the apiOptions.getBranchesIsPublic configuration property.

**Source**: 
- File: APIMethods140.scala, APIMethods210.scala
- Class/Object: APIMethods140, APIMethods210
- Method: getBranches, getBranch
- Lines: Authentication check logic

**Business Logic**:
1. Check the system configuration property getBranchesIsPublic
2. If getBranchesIsPublic = true, allow unauthenticated access to branch data
3. If getBranchesIsPublic = false, require user authentication before returning branch data
4. This allows banks to control whether branch information is publicly accessible

**Variables**:
- **Input**: apiOptions.getBranchesIsPublic (Boolean) - System configuration
- **Output**: Boolean - determines if authentication is required
- **Constants**: Configuration key: getBranchesIsPublic

**Business Conditions**:
| Condition | Business Meaning | Values/Thresholds |
|-----------|------------------|-------------------|
| getBranchesIsPublic = true | Branch data is public | No auth required |
| getBranchesIsPublic = false | Branch data is protected | Auth required |

**Business Impact**: 
Provides flexibility for banks to control data exposure. Public access enables easy branch finder functionality for customers, while protected access ensures data privacy when required.

**API Endpoints Using This Rule**:
- GET /obp/v1.4.0/banks/{BANK_ID}/branches - List Branches
- GET /obp/v2.1.0/banks/{BANK_ID}/branches/{BRANCH_ID} - Retrieve Branch

**Related Test Cases**:
- Test case for branch retrieval with public access enabled (no auth)
- Test case for branch retrieval with public access disabled (auth required)

**Migration Notes for Go**:
- Implement configuration-based authentication middleware
- Support runtime configuration changes if possible
- Document configuration options clearly

**Example Scenarios**:
```
Scenario 1: Public access enabled
Input: GET /branches, getBranchesIsPublic = true, no auth token
Processing: Check config -> Public access allowed
Output: Return branch data without authentication

Scenario 2: Public access disabled
Input: GET /branches, getBranchesIsPublic = false, no auth token
Processing: Check config -> Auth required -> No token provided
Output: Return 401 Unauthorized

Scenario 3: Protected access with valid auth
Input: GET /branches, getBranchesIsPublic = false, valid auth token
Processing: Check config -> Auth required -> Token valid
Output: Return branch data
```

---

### BR-008: Branch Hours Structure Handling

**Category**: TRANSFORMATION

**Description**: The system supports both lobby hours (detailed per-day schedule with multiple time slots) and drive-up hours (single time slot per day). Both deprecated string-based hours and structured time objects are supported for backward compatibility.

**Source**: 
- File: Branches.scala, JSONFactory220.scala
- Class/Object: Branch case class, JSON serialization
- Method: Branch data model and JSON transformation
- Lines: Lobby and DriveUp field definitions

**Business Logic**:
1. Lobby hours support multiple time slots per day (List of OpeningTimes per day)
2. Drive-up hours support only a single time slot per day
3. Times follow 24-hour clock format (e.g., "13:45")
4. Times after midnight can be represented as values > 24:00 (e.g., "25:30" for 1:30 AM)
5. Both lobbyString/driveUpString (deprecated) and lobby/driveUp (structured) fields are maintained
6. Empty time slots indicate the branch is closed for that day/service

**Variables**:
- **Input**: lobby (Map of day to List[OpeningTimes]), driveUp (Map of day to OpeningTimes)
- **Output**: Structured hours in response, plus deprecated string formats
- **Constants**: Days of week: monday, tuesday, wednesday, thursday, friday, saturday, sunday

**Business Conditions**:
| Condition | Business Meaning | Values/Thresholds |
|-----------|------------------|-------------------|
| lobby[day] = [] | Lobby closed on that day | Empty list |
| driveUp[day].opening_time = "" | Drive-up closed on that day | Empty string |
| time > "24:00" | After midnight (next day) | e.g., "25:30" = 1:30 AM |

**Business Impact**: 
Enables accurate representation of branch operating hours for customer planning. Supports complex schedules including extended hours and multiple service windows. Backward compatibility ensures existing integrations continue to work.

**API Endpoints Using This Rule**:
- POST /obp/v2.1.0/banks/{BANK_ID}/branches - Create Branch
- PUT /obp/v2.1.0/banks/{BANK_ID}/branches/{BRANCH_ID} - Update Branch
- GET /obp/v2.1.0/banks/{BANK_ID}/branches/{BRANCH_ID} - Retrieve Branch
- GET /obp/v1.4.0/banks/{BANK_ID}/branches - List Branches

**Related Test Cases**:
- Test case for branch with multiple lobby time slots per day
- Test case for branch with after-midnight hours
- Test case for branch with closed days (empty slots)
- Test case for backward compatibility with string-based hours

**Migration Notes for Go**:
- Implement separate structures for lobby ([]OpeningTime per day) and driveUp (single OpeningTime per day)
- Support 24-hour time format with values > 24:00
- Maintain both structured and string-based fields for API compatibility
- Consider using time.Duration or custom type for time handling

**Example Scenarios**:
```
Scenario 1: Multiple lobby time slots
Input: lobby.monday = [{opening: "09:00", closing: "12:00"}, {opening: "13:00", closing: "17:00"}]
Processing: Store multiple time slots for Monday
Output: Branch has split lobby hours on Monday (morning and afternoon)

Scenario 2: After-midnight drive-up hours
Input: driveUp.friday = {opening: "08:00", closing: "25:30"}
Processing: Store extended hours (closes at 1:30 AM Saturday)
Output: Drive-up open Friday 8 AM to Saturday 1:30 AM

Scenario 3: Closed on Sunday
Input: lobby.sunday = [], driveUp.sunday = {opening: "", closing: ""}
Processing: Store empty values for Sunday
Output: Branch closed on Sunday for both lobby and drive-up
```

---

## API Endpoint Coverage

| Endpoint | HTTP Method | Business Rules | Rule IDs |
|----------|-------------|----------------|----------|
| /obp/v2.1.0/banks/{BANK_ID}/branches | POST | Bank validation, uniqueness, entitlements, routing defaults, accessibility, hours | BR-001, BR-002, BR-003, BR-005, BR-006, BR-008 |
| /obp/v2.1.0/banks/{BANK_ID}/branches/{BRANCH_ID} | PUT | Entitlements, accessibility, hours | BR-003, BR-005, BR-008 |
| /obp/v2.1.0/banks/{BANK_ID}/branches/{BRANCH_ID} | GET | Public access configuration, hours | BR-007, BR-008 |
| /obp/v1.4.0/banks/{BANK_ID}/branches | GET | Public access, soft delete filtering, hours | BR-004, BR-007, BR-008 |
| /obp/v3.1.0/banks/{BANK_ID}/branches/{BRANCH_ID} | DELETE | Entitlements, soft delete | BR-003, BR-004 |

## Migration Validation Matrix

| Rule ID | Test Case Reference | Go Implementation Status | Validation Status |
|---------|---------------------|--------------------------|-------------------|
| BR-001 | TestBranchCreationWithValidBank, TestBranchCreationWithInvalidBank | Pending | Pending |
| BR-002 | TestBranchUniqueness, TestDuplicateBranchCreation | Pending | Pending |
| BR-003 | TestBranchEntitlements, TestUnauthorizedBranchCreation | Pending | Pending |
| BR-004 | TestBranchSoftDelete, TestDeletedBranchFiltering | Pending | Pending |
| BR-005 | TestAccessibilityTristate | Pending | Pending |
| BR-006 | TestDefaultBranchRouting, TestCustomBranchRouting | Pending | Pending |
| BR-007 | TestPublicBranchAccess, TestProtectedBranchAccess | Pending | Pending |
| BR-008 | TestLobbyHours, TestDriveUpHours, TestAfterMidnightHours | Pending | Pending |

## Notes and Assumptions

1. **Source Code Reference**: Business rules were extracted based on the user story documentation which references the Scala implementation files (Branches.scala, MappedBranchesProvider.scala, APIMethods140.scala, APIMethods210.scala, APIMethods310.scala).

2. **Entitlement System**: The entitlement system is assumed to be a separate service/module that the Go implementation will need to integrate with or replicate.

3. **Database Schema**: The soft delete pattern assumes a boolean isDeleted field exists in the branch table. The Go implementation should maintain this schema.

4. **Time Format**: The 24-hour time format with support for values > 24:00 is a specific business requirement that must be preserved in the Go implementation.

5. **Backward Compatibility**: The dual support for string-based and structured hours fields suggests API versioning considerations for the Go migration.

6. **Configuration Management**: The public access configuration (getBranchesIsPublic) implies a configuration management system that the Go implementation will need to support.
