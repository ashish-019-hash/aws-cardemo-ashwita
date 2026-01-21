# User Story for Signing Basket Deletion

## Story Overview
**As a** Payment Service Provider (PSP) or Third-Party Provider (TPP) Application
**I want to** delete signing baskets
**So that** I can remove signing baskets that are no longer needed, have been cancelled, or have expired, maintaining clean data and freeing up resources in the PSD2 Berlin Group compliant payment authorization workflow

## Acceptance Criteria
1. The system shall allow authorized users/applications to delete a signing basket by its unique identifier
2. The system shall validate that the signing basket exists before attempting deletion
3. The system shall verify that the requesting party has appropriate authorization to delete the specified signing basket
4. The system shall return a success confirmation upon successful deletion of the signing basket
5. The system shall return an appropriate error response if the signing basket does not exist
6. The system shall return an appropriate error response if the requesting party lacks authorization to delete the signing basket
7. The system shall handle deletion of signing baskets regardless of their current status (pending, partially authorized, etc.)
8. The system shall ensure that deletion is permanent and the signing basket cannot be recovered after deletion

## Technical Context
- **Classes/Services Involved**: Signing basket management service, authorization service, PSD2 Berlin Group compliance service
- **Input Data**: Signing basket identifier (basketId), authentication/authorization credentials
- **Output Data**: Deletion confirmation response or error response
- **Processing Type**: API (HTTP request-response)

## Relevant Endpoints

**IMPORTANT**: Each endpoint below is justified by specific words/phrases from the capability description.

- **Endpoint**: DELETE /v1/signing-baskets/{basketId}
  - **Justification (from description)**: "Delete signing baskets" - the word "Delete" explicitly justifies a DELETE endpoint
  - **Purpose**: Delete a signing basket that is no longer needed or has been cancelled
  - **Request**: 
    ```
    DELETE /v1/signing-baskets/{basketId}
    Headers:
      - X-Request-ID: string (required) - Unique request identifier
      - Authorization: Bearer {access_token} (required) - OAuth2 access token
      - PSU-IP-Address: string (conditional) - IP address of the Payment Service User
      - PSU-ID: string (optional) - Payment Service User identifier
      - PSU-ID-Type: string (optional) - Type of PSU-ID
      - PSU-Corporate-ID: string (optional) - Corporate identifier for PSU
      - PSU-Corporate-ID-Type: string (optional) - Type of corporate identifier
    Path Parameters:
      - basketId: string (required) - Unique identifier of the signing basket to delete
    ```
  - **Response**: 
    ```
    Success (HTTP 204 No Content):
    - Empty response body indicating successful deletion
    
    Success (HTTP 200 OK) - Alternative:
    {
      "transactionStatus": "CANC",
      "message": "Signing basket successfully deleted"
    }
    
    Error (HTTP 404 Not Found):
    {
      "tppMessages": [
        {
          "category": "ERROR",
          "code": "RESOURCE_UNKNOWN",
          "text": "The signing basket with the given basketId does not exist"
        }
      ]
    }
    
    Error (HTTP 403 Forbidden):
    {
      "tppMessages": [
        {
          "category": "ERROR",
          "code": "CONSENT_INVALID",
          "text": "The TPP is not authorized to delete this signing basket"
        }
      ]
    }
    ```

## Business Rules (from capability description)
1. Only the TPP/PSP that created the signing basket or has appropriate authorization can delete it
2. Signing baskets can be deleted regardless of their authorization status
3. Deletion of a signing basket should cascade to invalidate any associated payment initiations that were pending authorization within the basket
4. Deleted signing baskets cannot be recovered or reused
5. The deletion operation must comply with PSD2 Berlin Group specification requirements
6. Audit trail must be maintained for deleted signing baskets for regulatory compliance

## Data Validations (if applicable)
- basketId must be a valid, non-empty string identifier
- basketId must correspond to an existing signing basket in the system
- The requesting party must have valid authentication credentials
- The requesting party must have authorization to delete the specified signing basket
- X-Request-ID header must be provided for request tracking and idempotency

## Dependencies
- **Upstream**: 
  - User/application must be authenticated with valid OAuth2 credentials
  - Signing basket must have been previously created (via Signing Basket Creation capability)
  - TPP must be registered and authorized to perform PSD2 operations
- **Downstream**: 
  - Associated payment initiations within the basket may be affected (cancelled/invalidated)
  - Audit logs must be updated to record the deletion
  - Any pending SCA (Strong Customer Authentication) processes for the basket should be terminated
- **External Systems**: 
  - May need to notify core banking systems of basket cancellation
  - May need to update consent management systems

## Notes for Implementation
- The capability description only mentions "Delete signing baskets" - no retrieval, creation, update, or listing operations are included as these fall under separate capabilities (Signing Basket Creation, Signing Basket Retrieval, Signing Basket Authorisation, Signing Basket Status)
- Consider implementing soft delete vs hard delete based on regulatory requirements for audit trail retention (Needs SME Input)
- Idempotency should be implemented using X-Request-ID to handle duplicate deletion requests gracefully
- The Berlin Group specification may have specific requirements for the response format and status codes - verify against the latest NextGenPSD2 specification
- Consider whether deletion should be allowed for baskets that are in the middle of an SCA process (Needs SME Input)
- Determine the appropriate HTTP status code for successful deletion (204 No Content vs 200 OK with body) based on Berlin Group specification requirements

## Quality Checklist Verification
- [x] Written from business perspective (not technical)
- [x] User role is clearly identified (PSP/TPP Application)
- [x] Business value is stated (removing unneeded baskets, maintaining clean data)
- [x] Acceptance criteria are testable
- [x] All major logic paths from the description are covered
- [x] Dependencies mentioned in the description are documented
- [x] Unclear areas are flagged (soft vs hard delete, SCA process handling)
- [x] Only relevant endpoints are included (DELETE only)
- [x] All details align with the capability description provided
- [x] No information from other capabilities is included
- [x] For each endpoint, specific words from the capability description justify inclusion ("Delete" justifies DELETE endpoint)
- [x] No endpoint type has been added unless its verb appears in the description
- [x] No GET/view/list/search endpoints included as these are not mentioned in the description
- [x] No POST/create endpoints included as "create" is not mentioned in the description
- [x] No PUT/PATCH/update endpoints included as "update/manage" is not mentioned in the description
