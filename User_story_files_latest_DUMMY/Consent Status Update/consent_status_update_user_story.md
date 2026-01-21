# User Story for Consent Status Update

## Story Overview

**As a** Consent Administrator / Third-Party Application / Account Information Service Provider (AISP)  
**I want to** update the status of consent records  
**So that** consent lifecycle can be properly managed, reflecting changes in authorization state such as activation, suspension, or expiration of account access permissions

## Acceptance Criteria

1. The system shall allow authorized users to update the status of an existing consent record
2. The system shall validate that the consent record exists before allowing status updates
3. The system shall validate that the new status is a valid consent status value
4. The system shall enforce proper status transition rules (e.g., cannot transition from revoked to active)
5. The system shall record the timestamp of the status update
6. The system shall return confirmation of successful status update with the updated consent details
7. The system shall reject status update requests for non-existent consents with appropriate error messages
8. The system shall reject invalid status transitions with appropriate error messages
9. The system shall maintain an audit trail of status changes for compliance purposes

## Technical Context

- **Classes/Services Involved**: 
  - Consent entity/model classes
  - Consent status update service/handler
  - Consent validation service
  - Status transition rules engine
  - Database/persistence layer for consent storage
  - Audit logging service

- **Input Data**: 
  - Consent ID (required) - identifier of the consent to update
  - New status value (required) - the target status for the consent
  - Status reason (optional) - reason for the status change
  - Timestamp (optional) - effective date of status change

- **Output Data**: 
  - Updated consent record with new status
  - Previous status value
  - Status update timestamp
  - Success/error response

- **Processing Type**: API / On-demand

## Relevant Endpoints

**IMPORTANT**: For every endpoint included, justification from the description is provided.

### Endpoint 1: Update Consent Status
- **Endpoint**: `PUT /obp/v5.1.0/banks/{BANK_ID}/consents/{CONSENT_ID}/status`
  - **Justification (from description)**: "Update the status" - the word "Update" explicitly justifies a PUT endpoint for modifying consent status
  - **Purpose**: Update the status of an existing consent record to reflect changes in authorization state
  - **Request**: 
    ```json
    {
      "status": "string (e.g., INITIATED, ACCEPTED, REJECTED, REVOKED, VALID, EXPIRED, TERMINATED_BY_TPP, TERMINATED_BY_ASPSP)"
    }
    ```
  - **Response**: 
    ```json
    {
      "consent_id": "string",
      "status": "string",
      "previous_status": "string",
      "updated_at": "timestamp",
      "bank_id": "string"
    }
    ```

### Alternative Endpoint: Partial Update Consent Status
- **Endpoint**: `PATCH /obp/v5.1.0/banks/{BANK_ID}/consents/{CONSENT_ID}`
  - **Justification (from description)**: "Update the status" - the word "Update" also justifies a PATCH endpoint for partial updates to consent records
  - **Purpose**: Partially update consent record, specifically the status field
  - **Request**: 
    ```json
    {
      "status": "string"
    }
    ```
  - **Response**: 
    ```json
    {
      "consent_id": "string",
      "status": "string",
      "updated_at": "timestamp"
    }
    ```

**Note**: Based on the Operation Derivation Rules, the following endpoints are explicitly EXCLUDED because their operation types are not mentioned in the capability description:
- GET /consents/{consent_id} - No "view", "retrieve", or "get" mentioned
- GET /consents - No "list" or "search" mentioned
- POST /consents - No "create" or "register" mentioned
- DELETE /consents/{consent_id} - No "delete", "remove", or "revoke" mentioned

## Business Rules (from capability description)

1. **Consent Existence**: The consent record must exist before its status can be updated
2. **Valid Status Values**: Status updates must use valid consent status values defined by the system
3. **Status Transition Rules**: Status changes must follow valid transition paths (e.g., INITIATED -> ACCEPTED -> VALID)
4. **Authorization Required**: Only authorized users/applications can update consent status
5. **On-demand Processing**: Consent status updates are performed on-demand (not batch or scheduled)
6. **Medium Volume Operation**: Consent status updates are expected to be a medium-volume operation
7. **Audit Requirements**: All status changes must be logged for regulatory compliance

## Data Validations (if applicable)

- Consent ID must exist in the system
- New status must be a valid consent status value
- Status transition must be valid according to business rules
- User/application must have appropriate permissions to update the consent
- Bank ID must be valid and the consent must belong to the specified bank
- Status reason (if provided) must not exceed maximum length

## Dependencies

- **Upstream**: 
  - User/application authentication and authorization must be completed
  - User must have appropriate entitlements/roles for consent status updates (e.g., CanUpdateConsent)
  - The consent record must already exist in the system (created via Consent Creation capability)
  - Platform must be operational and accepting requests

- **Downstream**: 
  - After consent status update:
    - Account access permissions may be affected based on new status
    - Third-party applications may gain or lose access to account data
    - Notifications may be triggered to relevant parties
    - Audit logs are updated with the status change

- **External Systems**: 
  - Database/persistence layer for storing consent records
  - Audit logging system for compliance tracking
  - Notification service for status change alerts (if applicable)

## Notes for Implementation

- **Authorization**: Ensure proper role-based access control - only users with CanUpdateConsent or similar entitlement should be able to update consent status
- **Status Transition Validation**: Implement a state machine or transition rules engine to enforce valid status transitions
- **Idempotency**: Consider implementing idempotency for status updates to handle duplicate requests gracefully
- **Concurrency**: Handle concurrent status update requests to prevent race conditions
- **Audit Trail**: Log all status changes with timestamp, user, previous status, and new status for compliance
- **Error Handling**: Provide clear, actionable error messages for validation failures and invalid transitions

### Open Questions (Needs SME Input)

1. What are the valid consent status values supported by the system?
2. What are the valid status transition rules (state machine definition)?
3. Should status updates trigger notifications to account holders or third-party applications?
4. What audit information must be captured for each status change?
5. Are there time-based restrictions on status updates (e.g., cannot update within X minutes of creation)?
6. Should the system support bulk status updates for multiple consents?
7. What happens to active sessions/tokens when consent status changes to revoked or expired?

---

## Quality Checklist Verification

- [x] Written from business perspective (not technical)
- [x] User role is clearly identified (Consent Administrator / Third-Party Application / AISP)
- [x] Business value is stated (managing consent lifecycle and authorization state)
- [x] Acceptance criteria are testable
- [x] All major logic paths from the description are covered
- [x] Dependencies mentioned in the description are documented
- [x] Unclear areas are flagged (Open Questions section)
- [x] Only relevant endpoints are included (PUT/PATCH for update only)
- [x] All details align with the capability description provided
- [x] No information from other capabilities is included
- [x] For each endpoint, specific word from capability description justifies inclusion ("Update")
- [x] No endpoint type has been added unless its verb appears in the description
- [x] No CRUD operations inferred beyond what description explicitly states
