# User Story for Entitlement Listing

## Story Overview

**As a** system administrator or authorized user  
**I want to** list entitlements for users  
**So that** I can view and audit user permissions across the system and ensure proper access control compliance

## Acceptance Criteria

1. The system shall provide the ability to list all entitlements assigned to a specific user
2. The system shall support listing entitlements at both system-level and bank-level
3. The entitlement list shall include relevant details such as entitlement name, role, and scope
4. The system shall return an appropriate response when no entitlements are found for a user
5. The system shall enforce proper authorization before allowing entitlement listing operations
6. The system shall support pagination for large entitlement lists

## Technical Context

- **Classes/Services Involved**: 
  - EntitlementService - Handles entitlement retrieval operations
  - UserService - Provides user context for entitlement queries
  - AuthorizationService - Validates access permissions for listing entitlements

- **Input Data**: 
  - User ID (path parameter) - Identifier of the user whose entitlements are being listed
  - Bank ID (optional path parameter) - For bank-scoped entitlement queries
  - Pagination parameters (optional query parameters) - offset, limit

- **Output Data**: 
  - List of entitlement objects containing:
    - Entitlement ID
    - Entitlement name/role name
    - Bank ID (for bank-scoped entitlements)
    - User ID
    - Created date
    - Status

- **Processing Type**: API/Real-time

## Relevant Endpoints

**IMPORTANT**: Each endpoint is justified by the word "List" in the capability description "List entitlements for users".

- **Endpoint**: GET /users/{USER_ID}/entitlements
  - **Justification (from description)**: "List" - explicitly states listing entitlements for users
  - **Purpose**: Retrieve all entitlements assigned to a specific user across all banks
  - **Request**: 
    - Path Parameter: USER_ID (string) - The unique identifier of the user
    - Query Parameters (optional): offset (integer), limit (integer)
  - **Response**: 
    ```json
    {
      "entitlements": [
        {
          "entitlement_id": "string",
          "role_name": "string",
          "bank_id": "string",
          "user_id": "string",
          "created_date": "datetime"
        }
      ],
      "total_count": "integer"
    }
    ```

- **Endpoint**: GET /banks/{BANK_ID}/users/{USER_ID}/entitlements
  - **Justification (from description)**: "List" - explicitly states listing entitlements for users (bank-scoped)
  - **Purpose**: Retrieve entitlements assigned to a specific user at a specific bank
  - **Request**: 
    - Path Parameters: 
      - BANK_ID (string) - The unique identifier of the bank
      - USER_ID (string) - The unique identifier of the user
    - Query Parameters (optional): offset (integer), limit (integer)
  - **Response**: 
    ```json
    {
      "entitlements": [
        {
          "entitlement_id": "string",
          "role_name": "string",
          "bank_id": "string",
          "user_id": "string",
          "created_date": "datetime"
        }
      ],
      "total_count": "integer"
    }
    ```

- **Endpoint**: GET /my/entitlements
  - **Justification (from description)**: "List" - listing entitlements for the current authenticated user
  - **Purpose**: Retrieve entitlements for the currently authenticated user (self-service)
  - **Request**: 
    - Query Parameters (optional): offset (integer), limit (integer)
  - **Response**: 
    ```json
    {
      "entitlements": [
        {
          "entitlement_id": "string",
          "role_name": "string",
          "bank_id": "string",
          "created_date": "datetime"
        }
      ],
      "total_count": "integer"
    }
    ```

## Business Rules (from capability description)

1. **BR-001: User Context Required** - Entitlements must be listed in the context of a specific user (either specified by ID or the authenticated user)
2. **BR-002: Authorization Check** - The requesting user must have appropriate permissions to view entitlements (either their own or others based on admin privileges)
3. **BR-003: Bank Scope Filtering** - When a bank ID is provided, only entitlements for that specific bank should be returned
4. **BR-004: Empty Result Handling** - If a user has no entitlements, return an empty list with appropriate metadata rather than an error

## Data Validations

- User ID must be a valid, existing user identifier
- Bank ID (when provided) must be a valid, existing bank identifier
- Pagination parameters must be non-negative integers
- The requesting user must be authenticated
- The requesting user must have permission to view the requested entitlements

## Dependencies

- **Upstream**: 
  - User authentication must be completed before listing entitlements
  - User must exist in the system
  - Bank must exist (for bank-scoped queries)

- **Downstream**: 
  - Entitlement data can be used for access control decisions
  - Entitlement lists can be used for audit and compliance reporting

- **External Systems**: 
  - Identity Provider (for user authentication)
  - Authorization Service (for permission validation)

## Notes for Implementation

- Consider implementing caching for frequently accessed entitlement lists to improve performance
- Ensure proper indexing on user_id and bank_id fields for efficient queries
- Implement rate limiting to prevent abuse of the listing endpoints
- Consider adding filtering options (e.g., by role name, by date range) in future iterations
- **Needs SME Input**: Confirm the exact structure of entitlement response objects
- **Needs SME Input**: Clarify if there are any entitlement types that should be excluded from listing (e.g., system-internal entitlements)
- **Needs SME Input**: Determine the default and maximum pagination limits

---

## Quality Checklist Verification

- [x] Written from business perspective (not technical)
- [x] User role is clearly identified (system administrator or authorized user)
- [x] Business value is stated (view and audit user permissions, ensure access control compliance)
- [x] Acceptance criteria are testable
- [x] All major logic paths from the description are covered
- [x] Dependencies mentioned in the description are documented
- [x] Unclear areas are flagged (marked as "Needs SME Input")
- [x] Only relevant endpoints are included (GET endpoints for listing)
- [x] All details align with the capability description provided
- [x] No information from other capabilities is included
- [x] For each endpoint, specific word "List" from the capability description justifies inclusion
- [x] No endpoint type (create, update, delete) has been added - only list/retrieval operations
- [x] No CRUD operations are inferred beyond what the description explicitly states
