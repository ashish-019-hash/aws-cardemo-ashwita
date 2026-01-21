# User Story for Counterparty Deletion

## Story Overview

**As a** Bank Account Holder or Account Administrator  
**I want to** remove counterparty records  
**So that** I can maintain an accurate and up-to-date list of payment recipients, remove outdated or incorrect counterparty information, and ensure data hygiene in my account's counterparty registry

## Acceptance Criteria

1. The system shall allow authorized users to delete/remove counterparty records associated with their accounts
2. The system shall remove the counterparty record and any related counterparty metadata when a deletion is performed
3. The system shall validate that the user has appropriate permissions (can_delete_counterparty) before allowing deletion
4. The system shall validate that the specified counterparty exists before attempting deletion
5. The system shall return appropriate error messages if the counterparty cannot be found or if the user lacks permissions
6. The system shall support deletion of counterparties through both account-specific and management endpoints
7. The system shall ensure that deleted counterparties can no longer be used for payment initiation

## Technical Context

- **Classes/Services Involved**: Counterparty Service, Counterparty Repository, Counterparty Metadata Service, View Permission Service
- **Input Data**: Bank ID, Account ID, View ID, Counterparty ID (path parameters for identifying the counterparty to delete)
- **Output Data**: Confirmation of successful deletion or appropriate error response
- **Processing Type**: API (REST DELETE endpoints)

## Relevant Endpoints

**IMPORTANT**: Each endpoint is justified by specific words in the capability description.

### Delete Counterparty (Explicit)

- **Endpoint**: DELETE /obp/v4.0.0/banks/{BANK_ID}/accounts/{ACCOUNT_ID}/{VIEW_ID}/counterparties/{COUNTERPARTY_ID}
  - **Justification (from description)**: "Remove" - explicitly stated in "Remove counterparty records"
  - **Purpose**: Delete a specific counterparty record associated with an account and view, along with any related metadata
  - **Request**: 
    - Path Parameters:
      - `BANK_ID`: The identifier of the bank
      - `ACCOUNT_ID`: The identifier of the account
      - `VIEW_ID`: The identifier of the view (must have can_delete_counterparty permission)
      - `COUNTERPARTY_ID`: The identifier of the counterparty to delete
  - **Response**: 
    - Success: HTTP 200 with confirmation
    - Error: Appropriate error code with message (e.g., 401 Unauthorized, 403 Forbidden, 404 Not Found)

### Delete Counterparty for Any Account (Management)

- **Endpoint**: DELETE /obp/v4.0.0/management/banks/{BANK_ID}/accounts/{ACCOUNT_ID}/{VIEW_ID}/counterparties/{COUNTERPARTY_ID}
  - **Justification (from description)**: "Remove" - explicitly stated in "Remove counterparty records"
  - **Purpose**: Management endpoint for administrators to delete any specified counterparty along with related metadata
  - **Request**: 
    - Path Parameters:
      - `BANK_ID`: The identifier of the bank
      - `ACCOUNT_ID`: The identifier of the account
      - `VIEW_ID`: The identifier of the view
      - `COUNTERPARTY_ID`: The identifier of the counterparty to delete
    - Required Roles: canDeleteCounterparty or canDeleteCounterpartyAtAnyBank
  - **Response**: 
    - Success: HTTP 200 with confirmation
    - Error: Appropriate error code with message

## Business Rules (from capability description)

1. Only authorized users with appropriate view permissions (can_delete_counterparty) can delete counterparties
2. Deleting a counterparty also removes all associated counterparty metadata
3. The counterparty must exist in the system before it can be deleted
4. Management endpoints require specific entitlements (canDeleteCounterparty or canDeleteCounterpartyAtAnyBank)
5. Bank ID and Account ID must be valid and exist in the system
6. The view specified must grant the can_delete_counterparty permission for non-management endpoints

## Data Validations (if applicable)

- **Bank ID Validation**: The specified BANK_ID must be a valid format and exist in the system
- **Account ID Validation**: The specified ACCOUNT_ID must be a valid format and exist within the specified bank
- **Counterparty ID Validation**: The specified COUNTERPARTY_ID must exist and be associated with the account/view
- **View Permission Validation**: The view must have can_delete_counterparty permission enabled
- **User Authentication**: User must be logged in and authenticated
- **Role Validation**: For management endpoints, user must have canDeleteCounterparty or canDeleteCounterpartyAtAnyBank entitlement

## Dependencies

- **Upstream**: 
  - Counterparty must have been previously created and exist in the system
  - User must be authenticated and have appropriate view access or entitlements
  - Bank and Account must exist in the system
- **Downstream**: 
  - Counterparty metadata is automatically deleted when the counterparty is removed
  - Any pending payments to the deleted counterparty may need to be handled
  - Audit logs should record the deletion event
- **External Systems**: 
  - None explicitly mentioned in the capability description

## Notes for Implementation

- **Cascading Deletion**: When a counterparty is deleted, ensure all related metadata (aliases, URLs, locations, etc.) is also removed
- **Audit Trail**: Consider logging counterparty deletions for compliance and audit purposes
- **Soft Delete vs Hard Delete**: Clarify with SME whether counterparties should be soft-deleted (marked as inactive) or hard-deleted (permanently removed)
- **Payment Impact**: Consider the impact on any scheduled or pending payments that reference the deleted counterparty
- **Needs SME Input**: 
  - Should there be a confirmation step before deletion?
  - Should deleted counterparties be recoverable within a certain time period?
  - What happens to transaction history that references a deleted counterparty?
- **Error Handling**: Ensure proper error messages are returned for various failure scenarios (not found, permission denied, etc.)
