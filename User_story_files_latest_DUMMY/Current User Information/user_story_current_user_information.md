# User Story for Current User Information

## Story Overview

**As a** authenticated API user (third-party developer, fintech application, or account holder)
**I want to** retrieve information about the currently authenticated user
**So that** I can display user profile details, personalize the application experience, and verify the identity of the logged-in user for security and audit purposes

## Acceptance Criteria

1. The system shall return information about the currently authenticated user when a valid authentication token is provided
2. The system shall return user profile details including user identifiers and associated metadata
3. The system shall only return information for the user associated with the current authentication session
4. The system shall reject requests with invalid or expired authentication tokens with appropriate error responses
5. The system shall support real-time retrieval with high volume capacity (Very High frequency as per BRD)
6. The system shall return consistent user information across multiple retrieval requests within the same session

## Technical Context

- **Classes/Services Involved**: User service, Authentication service, Session management
- **Input Data**: Authentication token (OAuth token, JWT, or session identifier)
- **Output Data**: User profile information including user ID, username, email, profile details, and associated metadata
- **Processing Type**: API / Real-time

## Relevant Endpoints

**IMPORTANT**: For every endpoint included, justification from the description is provided.

- **Endpoint**: GET /users/current
  - **Justification (from description)**: "Retrieve information about the currently authenticated user" - the word "retrieve" explicitly justifies a GET endpoint
  - **Purpose**: Retrieve complete profile information for the currently authenticated user
  - **Request**: 
    - Headers: Authorization token (Bearer token or OAuth credentials)
    - No request body required
  - **Response**: 
    ```json
    {
      "user_id": "string",
      "username": "string",
      "email": "string",
      "provider_id": "string",
      "provider": "string",
      "entitlements": [],
      "linked_customers": [],
      "views": [],
      "last_login_date": "datetime",
      "is_locked": "boolean"
    }
    ```

- **Endpoint**: GET /my/user
  - **Justification (from description)**: "Retrieve information about the currently authenticated user" - the word "retrieve" explicitly justifies a GET endpoint for self-service user information
  - **Purpose**: Alternative endpoint for retrieving current user information in a self-service context
  - **Request**: 
    - Headers: Authorization token
    - No request body required
  - **Response**: User profile object with personal details and permissions

## Business Rules (from capability description)

1. Only authenticated users can retrieve their own information - authentication is mandatory
2. Users can only access their own profile information through this capability - no cross-user access
3. The capability operates in real-time with very high volume expectations, requiring optimized performance
4. User information retrieval must respect the current authentication session context

## Data Validations (if applicable)

- Authentication token must be valid and not expired
- Authentication token must be properly formatted (Bearer token, OAuth signature, etc.)
- Session must be active for the requesting user
- Rate limiting may apply based on consumer application limits

## Dependencies

- **Upstream**: 
  - User must be authenticated through one of the supported authentication methods (OAuth 1.0a, OAuth 2.0/OIDC, Direct Login, Gateway Login)
  - Valid authentication token must be present in the request
- **Downstream**: 
  - Retrieved user information can be used for personalization, access control decisions, and audit logging
  - User profile data may be cached for performance optimization
- **External Systems**: 
  - Authentication providers (OAuth servers, OIDC providers)
  - User identity stores

## Notes for Implementation

- This is a high-frequency, real-time capability with Very High volume expectations - performance optimization is critical
- Consider caching strategies for frequently accessed user profile data
- Ensure proper security measures to prevent information leakage across user sessions
- The endpoint should be idempotent - multiple calls should return consistent results
- Consider implementing ETag or Last-Modified headers for client-side caching
- **Needs SME Input**: Confirm the exact fields to be returned in the user profile response
- **Needs SME Input**: Clarify if linked customers and entitlements should be included in the basic response or require separate API calls

---

## Quality Checklist Verification

- [x] Written from business perspective (not technical)
- [x] User role is clearly identified (authenticated API user)
- [x] Business value is stated (display profile, personalize experience, verify identity)
- [x] Acceptance criteria are testable
- [x] All major logic paths from the description are covered
- [x] Dependencies mentioned in the description are documented
- [x] Unclear areas are flagged (SME input needed)
- [x] Only relevant endpoints are included (GET endpoints only)
- [x] All details align with the capability description provided
- [x] No information from other capabilities is included
- [x] For each endpoint, specific word from capability description justifies inclusion ("retrieve")
- [x] No endpoint type has been added unless its verb appears in the description
- [x] No CRUD operations inferred beyond what description explicitly states (only retrieval)
