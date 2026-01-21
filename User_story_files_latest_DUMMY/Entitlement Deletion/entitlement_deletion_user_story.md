# User Story for Entitlement Deletion

## Story Overview

**As a** Platform Administrator / Super Admin  
**I want to** remove entitlements from users  
**So that** I can revoke access permissions and roles when users no longer require specific capabilities, ensuring proper access control and security compliance on the platform

## Acceptance Criteria

1. The system shall allow authorized administrators to remove an entitlement from a specified user
2. The system shall validate that the entitlement exists before attempting deletion
3. The system shall verify that the entitlement belongs to the specified user before deletion
4. The system shall permanently remove the entitlement record from the system upon successful deletion
5. The system shall return appropriate confirmation (HTTP 204) upon successful entitlement removal
6. The system shall reject deletion requests from unauthorized users with appropriate error messages
7. The system shall return an error if the specified entitlement does not exist
8. The system shall return an error if the entitlement does not belong to the specified user

## Technical Context

- **Classes/Services Involved**: 
  - Entitlement entity/model classes (MappedEntitlement)
  - Entitlement provider service (MappedEntitlementsProvider)
  - API endpoint handler (APIMethods200 - deleteEntitlement)
  - Authentication and authorization services
  - Database/persistence layer for entitlement storage

- **Input Data**: 
  - USER_ID (path parameter) - The unique identifier of the user whose entitlement is to be removed
  - ENTITLEMENT_ID (path parameter) - The unique identifier of the entitlement to be deleted

- **Output Data**: 
  - HTTP 204 No Content on successful deletion
  - Error response with appropriate message on failure (UserNotLoggedIn, UserHasMissingRoles, EntitlementNotFound, EntitlementCannotBeDeleted)

- **Processing Type**: API / On-demand

## Relevant Endpoints

**IMPORTANT**: For every endpoint included, justification from the description is provided.

### Endpoint 1: Delete Entitlement
- **Endpoint**: `DELETE /obp/v2.0.0/users/USER_ID/entitlement/ENTITLEMENT_ID`
  - **Justification (from description)**: "Remove entitlements from users" - the word "Remove" explicitly justifies a DELETE endpoint for entitlement deletion
  - **Purpose**: Remove a specific entitlement/role from a user, revoking their associated permissions
  - **Request**: 
    - Method: DELETE
    - Path Parameters:
      - `USER_ID`: String - The unique identifier of the user
      - `ENTITLEMENT_ID`: String - The unique identifier of the entitlement to delete
    - Request Body: None (EmptyBody)
    - Headers: 
      - Authorization: OAuth token required
  - **Response**: 
    - Success: HTTP 204 No Content (empty body)
    - Error Responses:
      ```json
      {
        "code": 401,
        "message": "OBP-20001: User not logged in. Authentication is required!"
      }
      ```
      ```json
      {
        "code": 403,
        "message": "OBP-20006: User is missing one or more roles: CanDeleteEntitlementAtAnyBank"
      }
      ```
      ```json
      {
        "code": 404,
        "message": "OBP-30212: EntitlementNotFound"
      }
      ```
      ```json
      {
        "code": 404,
        "message": "OBP-30213: EntitlementCannotBeDeleted"
      }
      ```

**Note**: Based on the Operation Derivation Rules, the following endpoints are explicitly EXCLUDED because their operation types are not mentioned in the capability description ("Remove entitlements from users"):
- POST /users/{user_id}/entitlements - No "create", "add", or "grant" mentioned
- GET /users/{user_id}/entitlements - No "view", "retrieve", "get", or "list" mentioned
- PUT /users/{user_id}/entitlement/{entitlement_id} - No "update" or "modify" mentioned
- GET /entitlements - No "list" or "search" mentioned

## Business Rules (from capability description)

1. **Authorization Required**: Only users with the `CanDeleteEntitlementAtAnyBank` role can delete entitlements
2. **Entitlement Existence**: The entitlement must exist in the system before it can be deleted
3. **User-Entitlement Association**: The entitlement must belong to the specified user (USER_ID must match)
4. **Permanent Removal**: Entitlement deletion is permanent and removes the record from the database
5. **On-demand Processing**: Entitlement deletion is performed on-demand (not batch or scheduled)
6. **Low Volume Operation**: Entitlement deletion is expected to be a low-volume operation

## Data Validations (if applicable)

- USER_ID must be a valid user identifier in the system
- ENTITLEMENT_ID must be a valid entitlement identifier in the system
- The requesting user must be authenticated (logged in)
- The requesting user must have the `CanDeleteEntitlementAtAnyBank` entitlement
- The entitlement's userId field must match the USER_ID path parameter
- The entitlement must exist (getEntitlementById must return a valid record)

## Dependencies

- **Upstream**: 
  - User authentication must be completed (OAuth)
  - User must have the `CanDeleteEntitlementAtAnyBank` role/entitlement
  - The target entitlement must exist in the system
  - The target user must exist in the system

- **Downstream**: 
  - After entitlement deletion:
    - The user loses the associated role/permission immediately
    - The user can no longer perform actions that required the deleted entitlement
    - Audit logs may be updated to reflect the entitlement removal
    - Any cached permissions may need to be invalidated

- **External Systems**: 
  - Database/persistence layer (MappedEntitlement table) for storing and deleting entitlement records
  - Authentication provider for validating user credentials

## Notes for Implementation

- **Authorization**: Ensure proper role-based access control - only users with `CanDeleteEntitlementAtAnyBank` should be able to delete entitlements
- **Validation Order**: 
  1. First validate user authentication
  2. Then validate the requesting user has the required role
  3. Then validate the entitlement exists
  4. Then validate the entitlement belongs to the specified user
  5. Finally perform the deletion
- **Idempotency**: Deletion is not idempotent - attempting to delete an already-deleted entitlement will return EntitlementNotFound
- **Error Handling**: Provide clear, actionable error messages for each failure scenario
- **Audit Trail**: Consider logging entitlement deletion events for compliance and audit purposes
- **Super Admin Access**: The documentation mentions Super Admins (listed in Props file) have special access for this operation

### Open Questions (Needs SME Input)

1. Should there be a soft-delete option instead of permanent deletion for audit purposes?
2. Are there any entitlements that cannot be deleted (protected roles)?
3. Should entitlement deletion trigger notifications to the affected user?
4. Is there a need for bulk entitlement deletion capability?
5. Should there be a confirmation step or approval workflow for deleting certain critical entitlements?
6. What happens to in-flight requests if an entitlement is deleted while a user is actively using the system?

---

## Quality Checklist Verification

- [x] Written from business perspective (not technical)
- [x] User role is clearly identified (Platform Administrator / Super Admin)
- [x] Business value is stated (revoking access permissions for security compliance)
- [x] Acceptance criteria are testable
- [x] All major logic paths from the description are covered
- [x] Dependencies mentioned in the description are documented
- [x] Unclear areas are flagged (Open Questions section)
- [x] Only relevant endpoints are included (DELETE for removal only)
- [x] All details align with the capability description provided
- [x] No information from other capabilities is included
- [x] For each endpoint, specific word from capability description justifies inclusion ("Remove")
- [x] No endpoint type has been added unless its verb appears in the description
- [x] No CRUD operations inferred beyond what description explicitly states
