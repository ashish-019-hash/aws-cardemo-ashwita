# User Story for Entitlement Creation

## Story Overview

**As a** System Administrator or Super Admin
**I want to** grant entitlements/roles to users
**So that** users can be authorized to perform specific actions and access specific resources within the Open Bank Project platform based on their assigned roles

## Acceptance Criteria

1. The system shall allow authorized administrators to grant entitlements (roles) to specified users
2. The system shall support granting both system-level roles (with empty bank_id) and bank-level roles (with specific bank_id)
3. The system shall validate that the role being granted is a valid, recognized role in the system
4. The system shall prevent duplicate entitlements from being granted (same user, same role, same bank)
5. The system shall verify that the bank exists when granting bank-level roles
6. The system shall require appropriate authorization (Super Admin or CanCreateEntitlement roles) to grant entitlements
7. The system shall return the created entitlement details upon successful grant operation
8. The system shall support bulk creation of entitlements for a user in a single request

## Technical Context

- **Classes/Services Involved**:
  - `EntitlementProvider` - Core trait defining entitlement operations including `addEntitlement`
  - `MappedEntitlementsProvider` - Implementation of EntitlementProvider for database persistence
  - `Entitlement` - Trait representing an entitlement entity with entitlementId, bankId, userId, roleName, and createdByProcess
  - `APIMethods200` / `APIMethods400` - API endpoint implementations for entitlement creation
  - `JSONFactory200` / `JSONFactory400` - JSON serialization/deserialization for entitlement requests and responses

- **Input Data**:
  - `CreateEntitlementJSON`: Request body containing:
    - `bank_id` (String): The bank identifier (empty string for system-level roles)
    - `role_name` (String): The name of the role/entitlement to grant
  - `PostCreateUserWithRolesJsonV400`: For bulk creation containing:
    - `username` (String): Target user's username
    - `provider` (String): Authentication provider
    - `roles` (List[CreateEntitlementJSON]): List of entitlements to grant

- **Output Data**:
  - `EntitlementJSON`: Response containing:
    - `entitlement_id` (String): Unique identifier for the created entitlement
    - `role_name` (String): The granted role name
    - `bank_id` (String): The associated bank identifier
  - `EntitlementJSONs`: For bulk operations, containing list of created entitlements

- **Processing Type**: API (HTTP request-response, synchronous)

## Relevant Endpoints

**IMPORTANT**: Only endpoints justified by the description "Grant entitlements/roles to users" are included.

### Endpoint 1: Add Entitlement for a User

- **Endpoint**: `POST /users/{USER_ID}/entitlements`
  - **Justification (from description)**: "Grant entitlements/roles to users" - the word "grant" justifies a POST/create operation
  - **Purpose**: Grant a single entitlement/role to a specific user identified by USER_ID
  - **Request**:
    ```json
    {
      "bank_id": "string (empty for system-level, bank ID for bank-level)",
      "role_name": "string (valid role name e.g., CanCreateAccount, CanGetAnyUser)"
    }
    ```
  - **Response** (HTTP 201 Created):
    ```json
    {
      "entitlement_id": "string",
      "role_name": "string",
      "bank_id": "string"
    }
    ```

### Endpoint 2: Create User with Roles (Bulk Entitlement Grant)

- **Endpoint**: `POST /user-entitlements`
  - **Justification (from description)**: "Grant entitlements/roles to users" - supports granting multiple roles in a single operation
  - **Purpose**: Create or identify a user and grant multiple entitlements/roles in a single request
  - **Request**:
    ```json
    {
      "username": "string",
      "provider": "string",
      "roles": [
        {
          "bank_id": "string",
          "role_name": "string"
        }
      ]
    }
    ```
  - **Response** (HTTP 201 Created):
    ```json
    {
      "list": [
        {
          "entitlement_id": "string",
          "role_name": "string",
          "bank_id": "string"
        }
      ]
    }
    ```

## Business Rules (from capability description)

1. **Authorization Required**: Only Super Admins or users with `CanCreateEntitlementAtOneBank` or `CanCreateEntitlementAtAnyBank` roles can grant entitlements
2. **Role Scope Validation**: System-level roles require an empty `bank_id`; bank-level roles require a valid, existing `bank_id`
3. **No Duplicate Entitlements**: A user cannot be granted the same role for the same bank more than once
4. **Valid Role Names**: Only recognized/valid role names can be granted (e.g., CanCreateAccount, CanGetAnyUser, CanCreateBranch)
5. **Delegation Constraint**: Users can only grant roles that they themselves possess (unless they are Super Admin)
6. **User Existence**: The target user must exist in the system before entitlements can be granted

## Data Validations (if applicable)

- **bank_id**: Must be empty string for system-level roles, or a valid existing bank ID for bank-level roles
- **role_name**: Must be a valid, recognized role name in the ApiRole enumeration
- **USER_ID**: Must correspond to an existing user in the system
- **Authentication**: Request must include valid authentication credentials
- **Authorization**: Requesting user must have appropriate entitlement creation permissions
- **Duplicate Check**: System validates that the exact entitlement (user + role + bank combination) does not already exist

## Dependencies

- **Upstream**:
  - User must be authenticated via OAuth 1.0a, OAuth 2.0/OIDC, or Direct Login
  - Target user must exist in the system (User Creation capability)
  - For bank-level roles, the bank must exist (Bank Creation capability)
  - Granting user must have appropriate permissions (CanCreateEntitlementAtOneBank or CanCreateEntitlementAtAnyBank)

- **Downstream**:
  - Granted entitlements enable users to access protected API endpoints
  - Entitlements are checked during authorization for subsequent API calls
  - Entitlements can be listed via Entitlement Listing capability
  - Entitlements can be removed via Entitlement Deletion capability

- **External Systems**:
  - Database persistence layer (RDBMS via Lift Mapper)
  - Authentication providers (OAuth, OIDC)

## Notes for Implementation

- **Role Enumeration**: The system maintains a comprehensive list of valid roles in the `ApiRole` object. New implementations should reference this enumeration for valid role names.
- **Super Admin Configuration**: Super Admins are configured in the Props file and have elevated privileges to grant any entitlement.
- **Audit Trail**: The `createdByProcess` field tracks how the entitlement was created (manual vs automated).
- **Grantor Tracking**: The `grantorUserId` optional field can track which user granted the entitlement.
- **Error Handling**: Comprehensive error messages are returned for various failure scenarios including:
  - `UserNotLoggedIn`: Authentication required
  - `UserNotFoundById`: Target user does not exist
  - `UserNotSuperAdmin`: Insufficient privileges for operation
  - `InvalidJsonFormat`: Malformed request body
  - `EntitlementAlreadyExists`: Duplicate entitlement attempt
  - `EntitlementCannotBeGranted`: Granting user lacks the role they're trying to grant
  - `InvalidUserProvider`: Invalid provider specified in bulk creation

### Open Questions for SME Review

1. Should there be a limit on the number of entitlements that can be granted to a single user?
2. Are there any role combinations that should be mutually exclusive?
3. Should entitlement grants trigger notifications to the target user?
4. Is there a need for time-limited/expiring entitlements?
