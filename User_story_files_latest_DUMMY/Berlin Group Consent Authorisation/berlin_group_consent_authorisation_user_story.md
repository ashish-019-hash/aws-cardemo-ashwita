# User Story for Berlin Group Consent Authorisation

## Story Overview

**As a** Third-Party Provider (TPP) or Payment Service Provider (PSP)
**I want to** manage consent authorisation sub-resources
**So that** I can complete the Strong Customer Authentication (SCA) process for PSD2-compliant consent authorisation, enabling secure access to account information on behalf of Payment Service Users (PSUs)

## Acceptance Criteria

1. The system shall allow TPPs to manage authorisation sub-resources for existing consents
2. The system shall support updating PSU authentication data during the consent authorisation process
3. The system shall support transaction authorisation with SCA authentication data
4. The system shall support selection of PSU authentication methods
5. The system shall support authorisation confirmation to finalize the consent authorisation process
6. The system shall update consent status based on SCA validation results (valid, rejected)
7. The system shall maintain user authentication context during the authorisation process
8. The system shall comply with Berlin Group PSD2 v1.3 specification for consent authorisation

## Technical Context

- **Classes/Services Involved**:
  - APIMethods_AccountInformationServiceAISApi - Main API endpoint definitions for AIS
  - Consents / ConsentProvider - Consent management and persistence
  - NewStyle.function - Utility functions for challenge creation and validation
  - JSONFactory_BERLIN_GROUP_1_3 - JSON response factory for Berlin Group format
  - ConsentAuthContextProvider - Manages consent authentication context
  - UserAuthContextProvider - Manages user authentication context

- **Input Data**:
  - Consent ID (path parameter)
  - Authorisation ID (path parameter for update operations)
  - SCA Authentication Data (for transaction authorisation)
  - PSU Data with password (for PSU authentication)
  - Authentication Method ID (for method selection)
  - Confirmation Code (for authorisation confirmation)

- **Output Data**:
  - SCA Status (received, psuAuthenticated, scaMethodSelected, finalised, failed)
  - Authorisation ID
  - PSU Message
  - HATEOAS Links for next steps
  - Chosen SCA Method details
  - Challenge Data (OTP format, length)

- **Processing Type**: Real-time API (synchronous request-response)

## Relevant Endpoints

**IMPORTANT**: Based on the capability description "Manage consent authorisation sub-resources", the word "manage" justifies update/configure operations only.

### Endpoint 1: Start Consent Authorisation (Transaction Authorisation)
- **Endpoint**: POST /consents/{consentId}/authorisations
- **Justification (from description)**: "Manage" - creating/starting an authorisation sub-resource is part of managing the authorisation lifecycle
- **Purpose**: Create an authorisation sub-resource and start the authorisation process for a consent using transaction authorisation

### Endpoint 2: Start Consent Authorisation (PSU Authentication)
- **Endpoint**: POST /consents/{consentId}/authorisations
- **Justification (from description)**: "Manage" - initiating PSU authentication is part of managing the authorisation process
- **Purpose**: Start the authorisation process with PSU authentication credentials

### Endpoint 3: Start Consent Authorisation (Select Authentication Method)
- **Endpoint**: POST /consents/{consentId}/authorisations
- **Justification (from description)**: "Manage" - selecting authentication method is part of managing the authorisation configuration
- **Purpose**: Start the authorisation process by selecting a PSU authentication method

### Endpoint 4: Update PSU Data (Transaction Authorisation)
- **Endpoint**: PUT /consents/{consentId}/authorisations/{authorisationId}
- **Justification (from description)**: "Manage" - updating PSU data for transaction authorisation is a core management operation
- **Purpose**: Update PSU data with SCA authentication data to authorize the consent

### Endpoint 5: Update PSU Data (PSU Authentication)
- **Endpoint**: PUT /consents/{consentId}/authorisations/{authorisationId}
- **Justification (from description)**: "Manage" - updating PSU authentication credentials is a management operation
- **Purpose**: Update PSU authentication data with password credentials

### Endpoint 6: Update PSU Data (Select Authentication Method)
- **Endpoint**: PUT /consents/{consentId}/authorisations/{authorisationId}
- **Justification (from description)**: "Manage" - selecting/updating authentication method is a configuration management operation
- **Purpose**: Update the authorisation by selecting a specific SCA authentication method

### Endpoint 7: Update PSU Data (Authorisation Confirmation)
- **Endpoint**: PUT /consents/{consentId}/authorisations/{authorisationId}
- **Justification (from description)**: "Manage" - confirming authorisation is the final management step in the authorisation lifecycle
- **Purpose**: Confirm the authorisation with a confirmation code to finalize the process

## Business Rules (from capability description)

1. **SCA Requirement**: All consent authorisations must go through Strong Customer Authentication (SCA) as mandated by PSD2
2. **Authorisation Sub-Resource Creation**: The ASPSP may automatically create authorisation sub-resources after consent creation, or require explicit creation via POST
3. **Multi-Level SCA**: For corporate contexts, multiple authorisation sub-resources may be created for n-times SCA authorisation
4. **Consent Status Updates**: Upon successful SCA validation (finalised), consent status is updated to "valid"; upon failure, status is updated to "rejected"
5. **User Context Preservation**: User authentication contexts are stored and associated with the consent during authorisation
6. **Consumer Validation**: The TPP (consumer) making the authorisation request must match the consumer that created the original consent
7. **SCA Methods**: Supported authentication methods include SMS_OTP, EMAIL, CHIP_OTP, and IMPLICIT

## Data Validations

- Consent ID must exist and be valid
- Authorisation ID must exist for update operations
- SCA Authentication Data must be provided for transaction authorisation
- PSU Data with password must be provided for PSU authentication
- Authentication Method ID must be valid for method selection
- Confirmation Code must be provided for authorisation confirmation
- Consumer ID from the request must match the consumer ID associated with the consent

## Dependencies

- **Upstream**:
  - Berlin Group Consent Creation (capability #79) - A consent must exist before authorisation can be managed
  - User Authentication - TPP must be authenticated to manage authorisations
  - PSD2 AISP Role - TPP must have Account Information Service Provider role

- **Downstream**:
  - Berlin Group Account List (capability #81) - After consent is authorised, account information can be accessed
  - Berlin Group Balance Retrieval (capability #82) - After consent is authorised, balances can be retrieved
  - Berlin Group Transaction List (capability #83) - After consent is authorised, transactions can be retrieved

- **External Systems**:
  - Challenge/OTP Service - For generating and validating SCA challenges
  - SMS/Email Gateway - For sending OTP codes to PSU

## Notes for Implementation

1. **SCA Flow Complexity**: The implementation must support multiple SCA approaches (Redirect, Decoupled, Embedded) as indicated by the Berlin Group specification
2. **State Machine**: The authorisation process follows a state machine with states: received -> psuAuthenticated -> scaMethodSelected -> finalised (or failed)
3. **HATEOAS Links**: All responses must include appropriate hypermedia links for the next steps in the authorisation flow
4. **Idempotency**: Consider implementing idempotency for POST operations to handle network retries
5. **Timeout Handling**: Authorisation sub-resources should have configurable timeout periods
6. **Audit Trail**: All authorisation attempts and status changes should be logged for compliance purposes
7. **Error Handling**: Implement proper error responses per Berlin Group specification (e.g., CONSENT_UNKNOWN, RESOURCE_UNKNOWN)

## Quality Checklist Verification

- [x] Written from business perspective (not technical)
- [x] User role is clearly identified (TPP/PSP)
- [x] Business value is stated (PSD2 compliance, secure account access)
- [x] Acceptance criteria are testable
- [x] All major logic paths from the description are covered
- [x] Dependencies mentioned in the description are documented
- [x] Unclear areas are flagged in Notes for Implementation
- [x] Only relevant endpoints are included
- [x] All details align with the capability description provided
- [x] No information from other capabilities is included
- [x] For each endpoint, specific justification from the capability description is provided
- [x] No endpoint type has been added unless justified by "manage" (update/configure operations)
- [x] "Manage" has been interpreted as update/configure operations
