# User Story for ATM Management

## Story Overview

**As a** Bank Administrator or Operations Manager  
**I want to** create, update, retrieve, and delete ATM information including locations, supported languages, currencies, and accessibility features  
**So that** customers can easily find and use ATMs that meet their needs, and the bank can maintain accurate ATM network information for operational and customer service purposes

## Acceptance Criteria

1. The system shall allow authorized users to create new ATM records with location details, supported languages, currencies, and accessibility features
2. The system shall allow authorized users to update existing ATM information including location changes, language support modifications, currency updates, and accessibility feature changes
3. The system shall allow users to retrieve ATM information by ATM identifier
4. The system shall allow users to retrieve a list of ATMs for a specific bank
5. The system shall allow authorized users to delete ATM records that are no longer active or relevant
6. ATM location information shall include geographic coordinates, address, and bank identifier
7. Supported languages shall be configurable per ATM to reflect local language availability
8. Supported currencies shall be configurable per ATM to reflect available cash denominations
9. Accessibility features shall be documented per ATM (e.g., wheelchair access, audio guidance, braille keypad)

## Technical Context

- **Classes/Services Involved**: ATM Service, ATM Repository, Bank Service (for bank validation), Location Validator, Language Validator, Currency Validator
- **Input Data**: ATM creation/update requests containing location data (latitude, longitude, address), language codes (ISO 639-1), currency codes (ISO 4217), accessibility feature flags, operating hours
- **Output Data**: ATM records with full details including ID, bank ID, name, location, languages, currencies, accessibility features, operating hours, and fee information
- **Processing Type**: API (REST endpoints for CRUD operations)

## Relevant Endpoints

**IMPORTANT**: Each endpoint is justified by specific words in the capability description: "Create, update, retrieve, and delete ATM information including locations, supported languages, currencies, and accessibility features"

### Create ATM

- **Endpoint**: POST /obp/v5.1.0/banks/{BANK_ID}/atms
  - **Justification (from description)**: "Create" - explicitly stated in "Create, update, retrieve, and delete ATM information"
  - **Purpose**: Create a new ATM record for a specific bank with all associated information
  - **Request**: 
    ```json
    {
      "id": "atm-id-123",
      "bank_id": "bank-id-123",
      "name": "Main Street ATM",
      "address": {
        "line_1": "123 Main Street",
        "line_2": "",
        "line_3": "",
        "city": "Berlin",
        "county": "",
        "state": "Berlin",
        "postcode": "10115",
        "country_code": "DE"
      },
      "location": {
        "latitude": 52.520008,
        "longitude": 13.404954
      },
      "meta": {
        "license": {
          "id": "license-id",
          "name": "License Name"
        }
      },
      "monday": { "opening_time": "08:00", "closing_time": "20:00" },
      "tuesday": { "opening_time": "08:00", "closing_time": "20:00" },
      "wednesday": { "opening_time": "08:00", "closing_time": "20:00" },
      "thursday": { "opening_time": "08:00", "closing_time": "20:00" },
      "friday": { "opening_time": "08:00", "closing_time": "20:00" },
      "saturday": { "opening_time": "09:00", "closing_time": "14:00" },
      "sunday": { "opening_time": "closed", "closing_time": "closed" },
      "is_accessible": true,
      "located_at": "Inside shopping mall",
      "more_info": "Near entrance",
      "has_deposit_capability": true,
      "supported_languages": ["en", "de", "fr"],
      "services": ["CASH_WITHDRAWAL", "BALANCE_INQUIRY", "DEPOSIT"],
      "accessibility_features": ["WHEELCHAIR_ACCESS", "AUDIO_GUIDANCE", "BRAILLE_KEYPAD"],
      "supported_currencies": ["EUR", "USD", "GBP"],
      "notes": ["24/7 access available"],
      "location_categories": ["SHOPPING_MALL", "URBAN"],
      "minimum_withdrawal": "10",
      "branch_identification": "branch-001",
      "site_identification": "site-001",
      "site_name": "Main Shopping Center",
      "cash_withdrawal_national_fee": "0.00",
      "cash_withdrawal_international_fee": "2.50",
      "balance_inquiry_fee": "0.00"
    }
    ```
  - **Response**: Created ATM record with assigned ID and all provided details (HTTP 201 Created)

### Update ATM

- **Endpoint**: PUT /obp/v5.1.0/banks/{BANK_ID}/atms/{ATM_ID}
  - **Justification (from description)**: "update" - explicitly stated in "Create, update, retrieve, and delete ATM information"
  - **Purpose**: Update an existing ATM's information including locations, languages, currencies, and accessibility features
  - **Request**: Same structure as create request with updated values
  - **Response**: Updated ATM record with all current details (HTTP 200 OK)

### Retrieve ATM by ID

- **Endpoint**: GET /obp/v5.1.0/banks/{BANK_ID}/atms/{ATM_ID}
  - **Justification (from description)**: "retrieve" - explicitly stated in "Create, update, retrieve, and delete ATM information"
  - **Purpose**: Retrieve detailed information about a specific ATM
  - **Request**: Path parameters BANK_ID and ATM_ID
  - **Response**: 
    ```json
    {
      "id": "atm-id-123",
      "bank_id": "bank-id-123",
      "name": "Main Street ATM",
      "address": {
        "line_1": "123 Main Street",
        "city": "Berlin",
        "state": "Berlin",
        "postcode": "10115",
        "country_code": "DE"
      },
      "location": {
        "latitude": 52.520008,
        "longitude": 13.404954
      },
      "is_accessible": true,
      "supported_languages": ["en", "de", "fr"],
      "supported_currencies": ["EUR", "USD", "GBP"],
      "accessibility_features": ["WHEELCHAIR_ACCESS", "AUDIO_GUIDANCE", "BRAILLE_KEYPAD"],
      "monday": { "opening_time": "08:00", "closing_time": "20:00" },
      "cash_withdrawal_national_fee": "0.00",
      "cash_withdrawal_international_fee": "2.50",
      "balance_inquiry_fee": "0.00"
    }
    ```

### Retrieve ATMs for Bank (List)

- **Endpoint**: GET /obp/v5.1.0/banks/{BANK_ID}/atms
  - **Justification (from description)**: "retrieve" - explicitly stated in "Create, update, retrieve, and delete ATM information" (retrieve implies ability to list/get multiple ATMs)
  - **Purpose**: Retrieve a list of all ATMs for a specific bank
  - **Request**: Path parameter BANK_ID, optional query parameters for filtering/pagination
  - **Response**: 
    ```json
    {
      "atms": [
        {
          "id": "atm-id-123",
          "bank_id": "bank-id-123",
          "name": "Main Street ATM",
          "location": { "latitude": 52.520008, "longitude": 13.404954 },
          "is_accessible": true,
          "supported_languages": ["en", "de", "fr"],
          "supported_currencies": ["EUR", "USD", "GBP"]
        },
        {
          "id": "atm-id-456",
          "bank_id": "bank-id-123",
          "name": "Airport ATM",
          "location": { "latitude": 52.559686, "longitude": 13.287711 },
          "is_accessible": true,
          "supported_languages": ["en", "de"],
          "supported_currencies": ["EUR", "USD"]
        }
      ]
    }
    ```

### Delete ATM

- **Endpoint**: DELETE /obp/v5.1.0/banks/{BANK_ID}/atms/{ATM_ID}
  - **Justification (from description)**: "delete" - explicitly stated in "Create, update, retrieve, and delete ATM information"
  - **Purpose**: Remove an ATM record from the system when it is no longer active or relevant
  - **Request**: Path parameters BANK_ID and ATM_ID
  - **Response**: Success confirmation (HTTP 204 No Content) or appropriate error message

## Business Rules (from capability description)

1. **Bank Association Rule**: ATMs must be associated with a valid bank entity (BANK_ID must exist in the system before ATM creation)
2. **ATM ID Uniqueness**: ATM IDs must be unique within a bank to prevent duplicate records
3. **Location Requirement**: ATM location information must include geographic coordinates for mapping purposes
4. **Language Code Standard**: Supported languages must be valid ISO 639-1 two-letter language codes (e.g., "en", "de", "fr")
5. **Currency Code Standard**: Supported currencies must be valid ISO 4217 three-letter currency codes (e.g., "EUR", "USD", "GBP")
6. **Accessibility Documentation**: Accessibility features must be documented to comply with accessibility regulations and customer information requirements
7. **Operating Hours Format**: ATM records should maintain operating hours in HH:MM 24-hour format or "closed" for customer convenience
8. **Fee Transparency**: Fee information for withdrawals and inquiries should be transparent and clearly documented
9. **Authorization Requirement**: Users must have appropriate entitlements/permissions to perform create, update, or delete operations

## Data Validations (if applicable)

- **Bank ID Validation**: The specified BANK_ID must exist in the system (HTTP 404 if not found)
- **ATM ID Uniqueness**: ATM IDs must be unique within a bank (HTTP 409 Conflict if duplicate)
- **Latitude Range**: Latitude must be between -90 and 90 degrees
- **Longitude Range**: Longitude must be between -180 and 180 degrees
- **Language Code Format**: Supported languages must be valid ISO 639-1 language codes
- **Currency Code Format**: Supported currencies must be valid ISO 4217 currency codes
- **Time Format**: Operating hours must be in valid HH:MM format or "closed"
- **Required Fields**: ATM ID, Bank ID, name, and location (latitude/longitude) are required for creation
- **Country Code Format**: Address country code must be valid ISO 3166-1 alpha-2 code

## Dependencies

- **Upstream**: 
  - Bank entity must exist before ATMs can be created for that bank
  - User must have appropriate entitlements/permissions (CanCreateAtm, CanUpdateAtm, CanDeleteAtm) to create, update, or delete ATMs
  - Authentication service must validate user credentials and entitlements
- **Downstream**: 
  - ATM information may be used by customer-facing applications for ATM locator features
  - ATM data may be consumed by third-party applications via API
  - Mobile banking applications may display ATM information to customers
- **External Systems**: 
  - Geographic mapping services may consume ATM location data
  - Currency exchange services may reference supported currencies
  - Accessibility compliance systems may audit accessibility features

## Notes for Implementation

- **Accessibility Compliance**: Ensure accessibility features are comprehensive and follow relevant accessibility standards (e.g., ADA compliance in US, similar regulations in other jurisdictions)
- **Multi-language Support**: The system should support storing and returning ATM information in multiple languages where applicable
- **Currency Handling**: Consider that ATMs may support multiple currencies and this should be reflected in the data model with proper ISO 4217 validation
- **Geolocation Precision**: Location coordinates should have sufficient precision (at least 6 decimal places) for accurate mapping
- **Operating Hours Complexity**: Consider handling special hours for holidays or seasonal variations in future enhancements
- **Fee Transparency**: Ensure fee information is clearly documented for regulatory compliance
- **Audit Trail**: Consider logging changes to ATM records for audit purposes (create, update, delete operations)
- **Soft Delete Consideration**: Consider implementing soft delete vs hard delete based on audit and recovery requirements
- **Needs SME Input**: 
  - Clarify the complete list of valid accessibility features that should be supported
  - Confirm the complete list of valid ATM services (CASH_WITHDRAWAL, BALANCE_INQUIRY, DEPOSIT, etc.)
  - Verify specific entitlement names used in the Scala implementation
