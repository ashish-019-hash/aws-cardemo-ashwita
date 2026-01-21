# User Story for View Access Revocation

## Story Overview

**As a** Bank Administrator or Account Owner
**I want to** revoke user access to views
**So that** I can remove unauthorized or no longer needed access permissions from users, ensuring proper access control and security compliance for account data

## Acceptance Criteria

1. The system shall allow authorized users to revoke another user's access to a specific view on an account
2. Upon successful revocation, the target user shall immediately lose access to the specified view
3. The system shall return appropriate confirmation when access is successfully revoked
4. The system shall return an error if attempting to revoke access that does not exist
5. The system shall validate that the requesting user has permission to revoke view access
6. The system shall support revocation of access for any valid bank, account, view, and user combination

## Technical Context

- **Classes/Services Involved**: View Access Management Service, Permission Validation Service, User Authorization Service
- **Input Data**: Bank ID, Account ID, View ID, User ID (provider and provider ID)
- **Output Data**: Confirmation of revocation or error response
- **Processing Type**: API (HTTP request-response)

## Relevant Endpoints

**IMPORTANT**: The following endpoint is justified by the explicit use of "Revoke" in the capability description.

- **Endpoint**: DELETE /banks/{BANK_ID}/accounts/{ACCOUNT_ID}/views/{VIEW_ID}/users/{USER_ID}
  - **Justification (from description)**: "Revoke user access to views" - the word "Revoke" explicitly justifies a DELETE operation
  - **Purpose**: Remove a user's access permission to a specific view on an account
  - **Request**: 
    - Path Parameters:
      - `BANK_ID` (string, required): The identifier of the bank
      - `ACCOUNT_ID` (string, required): The identifier of the account
      - `VIEW_ID` (string, required): The identifier of the view to revoke access from
      - `USER_ID` (string, required): The identifier of the user whose access is being revoked (format: provider/provider_id)
    - Headers:
      - Authorization token required
  - **Response**: 
    - Success (204 No Content or 200 OK): Access successfully revoked
    - Error (404 Not Found): User does not have access to the specified view
    - Error (401 Unauthorized): Requesting user lacks permission to revoke access
    - Error (400 Bad Request): Invalid parameters provided

## Business Rules

1. Only authorized users (account owners or administrators with appropriate entitlements) can revoke view access
2. Access revocation is immediate and takes effect upon successful API response
3. Revoking access does not delete the view itself, only the user's permission to access it
4. The system must maintain audit trail of access revocation events for compliance purposes
5. Users cannot revoke their own owner-level access to prevent accidental lockout

## Data Validations

- Bank ID must be a valid, existing bank identifier
- Account ID must be a valid account within the specified bank
- View ID must be a valid view associated with the account
- User ID must follow the correct format (provider/provider_id)
- The target user must currently have access to the specified view for revocation to succeed

## Dependencies

- **Upstream**: 
  - User authentication must be completed
  - View Access Grant capability (ID: 67) - access must have been previously granted before it can be revoked
  - View Creation capability (ID: 62) - views must exist before access can be managed
- **Downstream**: 
  - User's subsequent requests to the revoked view will be denied
  - Audit logging systems will record the revocation event
- **External Systems**: None explicitly mentioned

## Notes for Implementation

- The revocation operation should be idempotent where possible - repeated revocation attempts for already-revoked access should be handled gracefully
- Consider implementing soft-delete or audit logging to track historical access for compliance requirements
- Ensure proper error messages distinguish between "user never had access" and "access already revoked" scenarios
- **Needs SME Input**: Clarify if there are any grace periods or notification requirements when revoking access
- **Needs SME Input**: Determine if bulk revocation (revoking access for multiple users or views at once) is required
- **Needs SME Input**: Clarify the exact user ID format expected (provider/provider_id pattern)

---

## Quality Checklist Verification

- [x] Written from business perspective (not technical)
- [x] User role is clearly identified (Bank Administrator or Account Owner)
- [x] Business value is stated (access control and security compliance)
- [x] Acceptance criteria are testable
- [x] All major logic paths from the description are covered
- [x] Dependencies mentioned in the description are documented
- [x] Unclear areas are flagged for SME review
- [x] Only relevant endpoints are included (DELETE for revocation only)
- [x] All details align with the capability description provided
- [x] No information from other capabilities is included
- [x] For each endpoint, specific word "Revoke" from description justifies the DELETE endpoint
- [x] No endpoint type has been added unless its verb appears in the description
- [x] No CRUD operations inferred beyond what the description explicitly states (only revoke/delete)
