# User Story for User Lock/Unlock

## Story Overview

**As a** System Administrator or Security Officer
**I want to** lock or unlock user accounts for security purposes
**So that** I can protect the platform from unauthorized access by disabling compromised accounts and restore access for legitimate users when security concerns are resolved

## Acceptance Criteria

1. An authenticated administrator with the appropriate role (CanLockUser) can lock a user account by specifying the provider and username
2. An authenticated administrator with the appropriate role (CanUnlockUser) can unlock a previously locked user account by specifying the provider and username
3. When a user account is locked, the user cannot authenticate or access the system until unlocked
4. When a user account is unlocked, the user regains the ability to authenticate and access the system
5. The system returns appropriate error messages when attempting to lock/unlock a non-existent user
6. The system enforces role-based access control - only users with CanLockUser role can lock accounts, only users with CanUnlockUser role can unlock accounts
7. Lock operations are recorded with timestamp information for audit purposes
8. Unlocking a user also resets any bad login attempt counters associated with the account

## Technical Context

- **Classes/Services Involved**:
  - `UserLocksProvider` - Core service providing lock/unlock operations
  - `UserLocks` - Data model for storing user lock records with userId, typeOfLock, and lastLockDate
  - `LoginAttempt` - Service for managing bad login attempt tracking (reset on unlock)
  - `Users` - User lookup service for validating user existence by provider and username
  - `APIMethods510` - API endpoint implementations for v5.1.0
  - `APIMethods400` - API endpoint implementations for v4.0.0
  - `APIMethods310` - API endpoint implementations for v3.1.0

- **Input Data**:
  - Path parameters: `PROVIDER` (identity provider), `USERNAME` (user's username)
  - Authentication: OAuth token or Direct Login credentials
  - No request body required for lock/unlock operations

- **Output Data**:
  - Lock operation: `UserLockStatusJson` containing userId, typeOfLock, lastLockDate
  - Unlock operation: `BadLoginStatusJson` containing username, provider, bad_attempts_since_last_success_or_reset, last_failure_date

- **Processing Type**: API (Real-time HTTP request-response)

## Relevant Endpoints

**IMPORTANT**: The capability description states "Lock or unlock user accounts for security purposes". Based on the Operation Derivation Rules, only "lock" and "unlock" operations are explicitly mentioned.

### Endpoint 1: Lock User by Provider and Username (v5.1.0)
- **Endpoint**: `POST /obp/v5.1.0/users/PROVIDER/USERNAME/locks`
- **Justification (from description)**: "Lock" - explicitly mentioned in "Lock or unlock user accounts"
- **Purpose**: Lock a specific user account identified by provider and username to prevent authentication
- **Request**: 
  - Path parameters: `PROVIDER` (string), `USERNAME` (string)
  - Body: Empty
  - Headers: Authorization (OAuth/DirectLogin token)
- **Response**: 
  ```json
  {
    "user_id": "string",
    "type_of_lock": "lock_via_api",
    "last_lock_date": "2025-01-21T10:00:00Z"
  }
  ```
- **Required Role**: CanLockUser

### Endpoint 2: Lock User by Username (v4.0.0 - Local Provider Only)
- **Endpoint**: `POST /obp/v4.0.0/users/USERNAME/locks`
- **Justification (from description)**: "Lock" - explicitly mentioned in "Lock or unlock user accounts"
- **Purpose**: Lock a user account using local identity provider (simplified endpoint)
- **Request**: 
  - Path parameters: `USERNAME` (string)
  - Body: Empty
  - Headers: Authorization (OAuth/DirectLogin token)
- **Response**: 
  ```json
  {
    "user_id": "string",
    "type_of_lock": "lock_via_api",
    "last_lock_date": "2025-01-21T10:00:00Z"
  }
  ```
- **Required Role**: CanLockUser

### Endpoint 3: Unlock User by Provider and Username (v5.1.0)
- **Endpoint**: `PUT /obp/v5.1.0/users/PROVIDER/USERNAME/lock-status`
- **Justification (from description)**: "unlock" - explicitly mentioned in "Lock or unlock user accounts"
- **Purpose**: Unlock a previously locked user account and reset bad login attempts
- **Request**: 
  - Path parameters: `PROVIDER` (string), `USERNAME` (string)
  - Body: Empty
  - Headers: Authorization (OAuth/DirectLogin token)
- **Response**: 
  ```json
  {
    "username": "string",
    "provider": "string",
    "bad_attempts_since_last_success_or_reset": 0,
    "last_failure_date": "2025-01-21T10:00:00Z"
  }
  ```
- **Required Role**: CanUnlockUser

### Endpoint 4: Unlock User by Username (v3.1.0 - Local Provider Only)
- **Endpoint**: `PUT /obp/v3.1.0/users/USERNAME/lock-status`
- **Justification (from description)**: "unlock" - explicitly mentioned in "Lock or unlock user accounts"
- **Purpose**: Unlock a user account using local identity provider (simplified endpoint)
- **Request**: 
  - Path parameters: `USERNAME` (string)
  - Body: Empty
  - Headers: Authorization (OAuth/DirectLogin token)
- **Response**: 
  ```json
  {
    "username": "string",
    "provider": "string",
    "bad_attempts_since_last_success_or_reset": 0,
    "last_failure_date": "2025-01-21T10:00:00Z"
  }
  ```
- **Required Role**: CanUnlockUser

**Note**: GET endpoints for checking lock status exist in the codebase but are NOT included here because the capability description only mentions "Lock or unlock" - no "view", "retrieve", "check", or "get" verbs are present.

## Business Rules (from capability description)

1. **Security Purpose**: Lock/unlock operations are performed for security purposes, implying they should be used to protect the platform from unauthorized access
2. **Account-Level Operation**: Operations target user accounts, not individual sessions or tokens
3. **Reversible Action**: Locked accounts can be unlocked, indicating this is not a permanent deletion but a temporary security measure
4. **Administrative Action**: The nature of the operation (security purposes) implies this is an administrative function requiring elevated privileges

## Data Validations

- **User Existence**: The system validates that the specified user (by provider and username) exists before attempting lock/unlock operations. Returns 404 if user not found.
- **Authentication Required**: All lock/unlock operations require the caller to be authenticated
- **Role Authorization**: 
  - Lock operations require CanLockUser entitlement
  - Unlock operations require CanUnlockUser entitlement
  - Returns 403 if user lacks required role
- **Provider Validation**: Provider must be a valid identity provider string
- **Username Validation**: Username must be a non-empty string matching an existing user

## Dependencies

- **Upstream**: 
  - User must exist in the system (created via User Creation capability)
  - Administrator must be authenticated and have appropriate entitlements granted
  - Entitlement system must be operational for role checking

- **Downstream**: 
  - Authentication system checks lock status during login attempts
  - Locked users are prevented from accessing any API endpoints
  - Audit logs may record lock/unlock events for compliance

- **External Systems**: 
  - Identity providers (for multi-provider user lookup)
  - Database for persisting UserLocks records

## Notes for Implementation

### Special Considerations
1. **Automatic Locking**: The system also supports automatic locking after multiple failed login attempts (configurable threshold). The unlock operation resets both manual locks and automatic locks from failed attempts.

2. **Lock Types**: The current implementation uses "lock_via_api" as the type of lock when locked through the API. This distinguishes API-initiated locks from other potential lock mechanisms.

3. **Timestamp Tracking**: Lock operations record the last lock date, which can be useful for audit trails and determining how long an account has been locked.

4. **Bad Login Attempt Reset**: Unlocking a user also resets the bad login attempt counter, ensuring the user doesn't get immediately locked again due to previous failed attempts.

### API Version Considerations
- v5.1.0 endpoints support specifying both provider and username, allowing management of users from different identity providers
- v4.0.0 and v3.1.0 endpoints only support local identity provider users (simplified interface)

### Open Questions for SME Input
1. Should there be a maximum lock duration after which accounts are automatically unlocked?
2. Should lock/unlock events trigger notifications to the affected user or other administrators?
3. Are there specific audit logging requirements for lock/unlock operations beyond the timestamp tracking?
4. Should there be a "reason" field to document why an account was locked?

## Quality Checklist Verification

- [x] Written from business perspective (not technical)
- [x] User role is clearly identified (System Administrator/Security Officer)
- [x] Business value is stated (protect platform, restore access)
- [x] Acceptance criteria are testable
- [x] All major logic paths from the description are covered (lock and unlock)
- [x] Dependencies mentioned in the description are documented
- [x] Unclear areas are flagged (Open Questions section)
- [x] Only relevant endpoints are included (lock and unlock only)
- [x] All details align with the capability description provided
- [x] No information from other capabilities is included
- [x] For each endpoint, specific words from the capability description justify inclusion ("Lock", "unlock")
- [x] No CRUD operations are inferred beyond what the description explicitly states
- [x] Words like "manage" interpreted narrowly - only lock/unlock operations included as explicitly stated
