# User Story for Bank Creation

## Story Overview
**As a** Platform Administrator or Bank Onboarding Specialist
**I want to** create new bank entities on the platform with associated metadata and configuration
**So that** new financial institutions can be onboarded to the Open Bank Project platform and made available for third-party applications and services to interact with

## Acceptance Criteria
1. The system shall allow authorized users to create a new bank entity with required metadata fields
2. The system shall accept and store associated metadata for the bank entity including identifiers, names, logos, and website details
3. The system shall accept and store configuration settings during bank creation
4. The system shall validate all required fields before creating the bank entity
5. The system shall automatically create settlement accounts when a bank is created (incoming and outgoing accounts)
6. The system shall assign the CanCreateEntitlementAtOneBank role to the user creating the bank
7. The system shall return a confirmation with the created bank details upon successful creation
8. The system shall reject creation requests with invalid or missing required data with appropriate error messages
9. The system shall ensure bank IDs are unique across the platform
10. The system shall validate bank ID format (minimum length, no spaces, no invalid characters)

## Technical Context
- **Classes/Services Involved**: 
  - APIMethods500/APIMethods600 (API endpoint handlers)
  - NewStyle.function.createOrUpdateBank (bank creation service)
  - JSONFactory500 (response formatting)
  - Entitlement.entitlement.vend (entitlement management)
- **Input Data**: 
  - Bank metadata: id (optional, auto-generated if not provided), full_name, short_name, bank_code, logo URL, website URL
  - Bank routings: array of scheme/address pairs (e.g., BIC routing)
- **Output Data**: 
  - Created bank entity JSON with all metadata fields
  - HTTP 201 Created status code
- **Processing Type**: API (HTTP request-response, synchronous)

## Relevant Endpoints

**IMPORTANT**: Each endpoint below is justified by specific words/phrases from the capability description.

- **Endpoint**: POST /obp/v5.0.0/banks
  - **Justification (from description)**: "Create new bank entities" - the word "Create" explicitly justifies a POST endpoint for creating bank resources
  - **Purpose**: Create a new bank entity on the platform with associated metadata and configuration
  - **Request**: 
    ```json
    {
      "id": "string (optional, auto-generated if not provided)",
      "short_name": "string",
      "full_name": "string",
      "bank_code": "string",
      "logo": "string (URL, optional)",
      "website": "string (URL, optional)",
      "bank_routings": [
        {
          "scheme": "string (e.g., BIC)",
          "address": "string"
        }
      ]
    }
    ```
  - **Response**: 
    ```json
    {
      "id": "string",
      "short_name": "string",
      "full_name": "string",
      "bank_code": "string",
      "logo": "string (URL)",
      "website": "string (URL)",
      "bank_routings": [
        {
          "scheme": "string",
          "address": "string"
        }
      ],
      "attributes": []
    }
    ```

## Business Rules (from capability description)
1. Each bank entity must have a unique identifier within the platform
2. Bank ID must be greater than 3 characters in length
3. Bank ID cannot contain space characters
4. Bank ID cannot contain "::::" characters
5. Only authenticated users with appropriate entitlements (canCreateBank) can create new bank entities
6. Valid consumer credentials are required for bank creation
7. The user creating the bank is automatically granted CanCreateEntitlementAtOneBank role for that bank
8. The user creating the bank is automatically granted CanReadDynamicResourceDocsAtOneBank role for that bank
9. Settlement accounts are automatically created when the bank is created:
   - Incoming account (name: Default incoming settlement account, Account ID: OBP_DEFAULT_INCOMING_ACCOUNT_ID, currency: EUR)
   - Outgoing account (name: Default outgoing settlement account, Account ID: OBP_DEFAULT_OUTGOING_ACCOUNT_ID, currency: EUR)

## Data Validations (if applicable)
- Bank ID validation:
  - Must be unique across all banks on the platform
  - Minimum length greater than 3 characters
  - Cannot contain space characters
  - Cannot contain "::::" character sequence
  - If not provided, system auto-generates a UUID
- Consumer credentials must be valid and present
- JSON request body must conform to PostBankJson500 schema
- Bank routing schemes should follow standard formats (e.g., BIC)
- Logo URL should be a valid URL format if provided
- Website URL should be a valid URL format if provided

## Dependencies
- **Upstream**: 
  - User must be authenticated with valid credentials
  - User must have canCreateBank entitlement
  - Valid consumer application must be registered
  - Platform must be in SANDBOX mode (connector=mapped) for automatic settlement account creation
- **Downstream**: 
  - Once created, bank entity becomes available for:
    - Account creation at the bank
    - Branch management
    - ATM management
    - Product association
    - Customer onboarding
  - Settlement accounts are created automatically for payment processing
  - User receives entitlements to manage the created bank
- **External Systems**: 
  - Core banking system connectors (for non-sandbox mode)
  - Bank verification services (Needs SME Input)

## Notes for Implementation
- The capability description specifies "Create new bank entities on the platform with associated metadata and configuration"
- Only the CREATE operation (POST) is explicitly justified by the description - the word "Create" is present
- No retrieval (GET), listing, search, or deletion (DELETE) endpoints are included as these verbs are not mentioned in the capability description
- No update (PUT/PATCH) endpoints are included as verbs like "update", "manage", "modify", or "configure" (as a verb) are not present in the description
- The word "configuration" in the description is a noun (the configuration associated with the bank) not a verb, so it does not justify update operations
- Settlement account creation is automatic in SANDBOX mode - behavior in production mode needs SME clarification
- The exact configuration parameters that can be set during creation need SME input for complete documentation
- Consider implementing idempotency for bank creation to handle duplicate requests gracefully

## Quality Checklist Verification
- [x] Written from business perspective (not technical)
- [x] User role is clearly identified (Platform Administrator/Bank Onboarding Specialist)
- [x] Business value is stated (onboarding new financial institutions)
- [x] Acceptance criteria are testable
- [x] All major logic paths from the description are covered
- [x] Dependencies mentioned in the description are documented
- [x] Unclear areas are flagged (configuration parameters, production mode behavior)
- [x] Only relevant endpoints are included (POST for create only)
- [x] All details align with the capability description provided
- [x] No information from other capabilities is included
- [x] For each endpoint, specific words from the capability description justify inclusion
- [x] No endpoint type has been added unless its verb appears in the description
- [x] Words interpreted literally - "configuration" as noun does not justify update operations
