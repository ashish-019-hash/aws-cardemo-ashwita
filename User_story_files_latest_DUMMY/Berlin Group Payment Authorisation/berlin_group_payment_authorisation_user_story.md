# User Story for Berlin Group Payment Authorisation

## Story Overview
**As a** Payment Service Provider (PSP) or Third-Party Provider (TPP)
**I want to** manage payment authorisation sub-resources for PSD2-compliant payment initiation
**So that** I can handle the Strong Customer Authentication (SCA) process for payment transactions in compliance with Berlin Group NextGenPSD2 specifications

## Acceptance Criteria
1. The system shall allow authorized TPPs to manage authorisation sub-resources for initiated payments
2. The system shall support updating the authorisation status of payment authorisation sub-resources
3. The system shall allow configuration of authorisation parameters for payment sub-resources
4. The system shall maintain authorisation sub-resources in compliance with Berlin Group NextGenPSD2 specification
5. The system shall support managing multiple authorisation sub-resources per payment when required
6. The system shall allow updating SCA method selection for payment authorisation
7. The system shall support managing PSU (Payment Service User) authentication data within authorisation sub-resources

## Technical Context
- **Classes/Services Involved**: Payment authorisation service, SCA management service, Berlin Group compliance service
- **Input Data**: Payment ID, authorisation sub-resource ID, SCA authentication data, PSU credentials, authorisation status updates
- **Output Data**: Updated authorisation sub-resource status, SCA challenge data, authorisation confirmation
- **Processing Type**: API (HTTP request-response, Real-time)

## Relevant Endpoints

**IMPORTANT**: Each endpoint below is justified by specific words/phrases from the capability description.

- **Endpoint**: PUT /obp/v5.1.0/berlin-group/v1.3/payments/{payment-service}/{payment-product}/{paymentId}/authorisations/{authorisationId}
  - **Justification (from description)**: "Manage payment authorisation sub-resources" - the word "Manage" explicitly justifies update/configure operations on authorisation sub-resources
  - **Purpose**: Update an existing payment authorisation sub-resource with SCA data or status changes
  - **Request**: 
    ```json
    {
      "scaAuthenticationData": "string",
      "authenticationMethodId": "string"
    }
    ```
  - **Response**: 
    ```json
    {
      "scaStatus": "string",
      "authorisationId": "string",
      "psuMessage": "string",
      "_links": {
        "scaStatus": {
          "href": "string"
        }
      }
    }
    ```

- **Endpoint**: PUT /obp/v5.1.0/berlin-group/v1.3/periodic-payments/{payment-product}/{paymentId}/authorisations/{authorisationId}
  - **Justification (from description)**: "Manage payment authorisation sub-resources" - the word "Manage" covers managing authorisation for periodic payments
  - **Purpose**: Update authorisation sub-resource for periodic payment initiation
  - **Request**: 
    ```json
    {
      "scaAuthenticationData": "string",
      "authenticationMethodId": "string"
    }
    ```
  - **Response**: 
    ```json
    {
      "scaStatus": "string",
      "authorisationId": "string",
      "psuMessage": "string",
      "_links": {
        "scaStatus": {
          "href": "string"
        }
      }
    }
    ```

- **Endpoint**: PUT /obp/v5.1.0/berlin-group/v1.3/bulk-payments/{payment-product}/{paymentId}/authorisations/{authorisationId}
  - **Justification (from description)**: "Manage payment authorisation sub-resources" - the word "Manage" covers managing authorisation for bulk payments
  - **Purpose**: Update authorisation sub-resource for bulk payment initiation
  - **Request**: 
    ```json
    {
      "scaAuthenticationData": "string",
      "authenticationMethodId": "string"
    }
    ```
  - **Response**: 
    ```json
    {
      "scaStatus": "string",
      "authorisationId": "string",
      "psuMessage": "string",
      "_links": {
        "scaStatus": {
          "href": "string"
        }
      }
    }
    ```

## Business Rules (from capability description)
1. Payment authorisation sub-resources must be managed in compliance with Berlin Group NextGenPSD2 specification
2. Only authorized TPPs with valid eIDAS certificates can manage payment authorisation sub-resources
3. Authorisation sub-resources are linked to specific payment initiation requests
4. SCA status transitions must follow the defined state machine (received -> scaMethodSelected -> started -> finalised/failed)
5. Multiple authorisation sub-resources may exist for a single payment (multi-level authorisation scenarios)
6. Authorisation management must respect the payment's current status and lifecycle

## Data Validations (if applicable)
- Payment ID must reference an existing, valid payment initiation request
- Authorisation ID must reference an existing authorisation sub-resource for the specified payment
- SCA authentication data must be valid for the selected authentication method
- Authentication method ID must be from the list of available SCA methods for the PSU
- Status transitions must follow valid state machine rules
- TPP must have appropriate PIS (Payment Initiation Service) role

## Dependencies
- **Upstream**: 
  - Payment must be initiated via Berlin Group Payment Initiation capability (ID: 87)
  - Authorisation sub-resource must exist for the payment
  - TPP must be authenticated with valid eIDAS certificate
  - PSU must have initiated the SCA process
- **Downstream**: 
  - Successful authorisation management leads to payment execution
  - Failed authorisation may result in payment cancellation
  - Authorisation status affects Berlin Group Payment Status (ID: 90)
- **External Systems**: 
  - Integration with SCA providers (SMS OTP, Push notification, etc.)
  - ASPSP (Account Servicing Payment Service Provider) backend systems
  - eIDAS certificate validation services

## Notes for Implementation
- The capability description uses "Manage" which is interpreted narrowly as update/configure operations only - no retrieval (GET), listing, or deletion (DELETE) endpoints are included as these operations are not explicitly mentioned
- Creation of authorisation sub-resources would fall under "Berlin Group Payment Initiation" capability (ID: 87) as authorisation is typically created as part of payment initiation
- Retrieval of authorisation status would fall under "Berlin Group SCA Status" capability (ID: 93)
- The exact SCA methods supported and their data formats need SME input based on ASPSP implementation
- Consider implementing idempotency for authorisation updates to handle network retries gracefully
- Timeout handling for SCA processes needs to be defined - SME input required
- Multi-level authorisation scenarios (corporate payments) may require additional business rules - Needs SME Input

## Quality Checklist Verification
- [x] Written from business perspective (not technical)
- [x] User role is clearly identified (Payment Service Provider/Third-Party Provider)
- [x] Business value is stated (PSD2-compliant SCA process management)
- [x] Acceptance criteria are testable
- [x] All major logic paths from the description are covered
- [x] Dependencies mentioned in the description are documented
- [x] Unclear areas are flagged (SCA methods, timeout handling, multi-level auth)
- [x] Only relevant endpoints are included (PUT for manage/update operations)
- [x] All details align with the capability description provided
- [x] No information from other capabilities is included
- [x] For each endpoint, specific words from the capability description justify inclusion
- [x] No endpoint type has been added unless its verb appears in the description
- [x] "Manage" has been interpreted narrowly as update/configure operations only - no GET/list/delete operations included
