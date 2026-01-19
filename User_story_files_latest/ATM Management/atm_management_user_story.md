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

- **Classes/Services Involved**: ATM Service, ATM Repository, Bank Service (for bank validation)
- **Input Data**: ATM creation/update requests containing location data, language codes, currency codes, accessibility feature flags
- **Output Data**: ATM records with full details including ID, location, languages, currencies, and accessibility features
- **Processing Type**: API (REST endpoints for CRUD operations)

## Relevant Endpoints

**IMPORTANT**: Each endpoint is justified by specific words in the capability description.

### Create ATM

- **Endpoint**: POST /obp/v5.1.0/banks/{BANK_ID}/atms
  - **Justification (from description)**: "Create" - explicitly stated in "Create, update, retrieve, and delete ATM information"
  - **Purpose**: Create a new ATM record for a specific bank
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
  - **Response**: Created ATM record with assigned ID and all provided details

### Update ATM

- **Endpoint**: PUT /obp/v5.1.0/banks/{BANK_ID}/atms/{ATM_ID}
  - **Justification (from description)**: "update" - explicitly stated in "Create, update, retrieve, and delete ATM information"
  - **Purpose**: Update an existing ATM's information including locations, languages, currencies, and accessibility features
  - **Request**: Same structure as create request with updated values
  - **Response**: Updated ATM record with all current details

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
      "address": { ... },
      "location": { "latitude": 52.520008, "longitude": 13.404954 },
      "is_accessible": true,
      "supported_languages": ["en", "de", "fr"],
      "supported_currencies": ["EUR", "USD", "GBP"],
      "accessibility_features": ["WHEELCHAIR_ACCESS", "AUDIO_GUIDANCE", "BRAILLE_KEYPAD"],
      ...
    }
    ```

### Retrieve ATMs for Bank

- **Endpoint**: GET /obp/v5.1.0/banks/{BANK_ID}/atms
  - **Justification (from description)**: "retrieve" - explicitly stated in "Create, update, retrieve, and delete ATM information" (retrieve implies ability to list/get multiple)
  - **Purpose**: Retrieve a list of all ATMs for a specific bank
  - **Request**: Path parameter BANK_ID, optional query parameters for filtering/pagination
  - **Response**: 
    ```json
    {
      "atms": [
        { "id": "atm-id-123", "name": "Main Street ATM", ... },
        { "id": "atm-id-456", "name": "Airport ATM", ... }
      ]
    }
    ```

### Delete ATM

- **Endpoint**: DELETE /obp/v5.1.0/banks/{BANK_ID}/atms/{ATM_ID}
  - **Justification (from description)**: "delete" - explicitly stated in "Create, update, retrieve, and delete ATM information"
  - **Purpose**: Remove an ATM record from the system
  - **Request**: Path parameters BANK_ID and ATM_ID
  - **Response**: Success confirmation or appropriate error message

## Business Rules (from capability description)

1. ATMs must be associated with a valid bank entity (BANK_ID must exist)
2. ATM location information must include geographic coordinates for mapping purposes
3. Supported languages must be valid ISO language codes
4. Supported currencies must be valid ISO currency codes
5. Accessibility features must be documented to comply with accessibility regulations and customer information requirements
6. ATM records should maintain operating hours for customer convenience
7. Fee information for withdrawals and inquiries should be transparent

## Data Validations (if applicable)

- **Bank ID Validation**: The specified BANK_ID must exist in the system
- **ATM ID Uniqueness**: ATM IDs must be unique within a bank
- **Location Validation**: Latitude must be between -90 and 90, longitude must be between -180 and 180
- **Language Code Validation**: Supported languages must be valid ISO 639-1 language codes
- **Currency Code Validation**: Supported currencies must be valid ISO 4217 currency codes
- **Time Format Validation**: Operating hours must be in valid time format (HH:MM)
- **Required Fields**: ATM ID, Bank ID, name, and location are required for creation

## Dependencies

- **Upstream**: 
  - Bank entity must exist before ATMs can be created for that bank
  - User must have appropriate entitlements/permissions to create, update, or delete ATMs
- **Downstream**: 
  - ATM information may be used by customer-facing applications for ATM locator features
  - ATM data may be consumed by third-party applications via API
- **External Systems**: 
  - Geographic mapping services may consume ATM location data
  - Mobile banking applications may display ATM information to customers

## Notes for Implementation

- **Accessibility Compliance**: Ensure accessibility features are comprehensive and follow relevant accessibility standards (e.g., ADA compliance)
- **Multi-language Support**: The system should support storing and returning ATM information in multiple languages where applicable
- **Currency Handling**: Consider that ATMs may support multiple currencies and this should be reflected in the data model
- **Geolocation Precision**: Location coordinates should have sufficient precision for accurate mapping
- **Operating Hours Complexity**: Consider handling special hours for holidays or seasonal variations
- **Fee Transparency**: Ensure fee information is clearly documented for regulatory compliance
- **Audit Trail**: Consider logging changes to ATM records for audit purposes
- **Needs SME Input**: Clarify the complete list of valid accessibility features and services that should be supported
