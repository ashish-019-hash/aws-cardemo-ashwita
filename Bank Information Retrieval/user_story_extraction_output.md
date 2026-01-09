# User Story for Bank Information Retrieval

## Capability Analyzed
- **Name**: Bank Information Retrieval
- **Description**: Retrieve information about banks supported on the platform including bank ID, name, logo, and website details.

## Operation Verb Analysis
**Verbs identified in description**: "Retrieve"

**Mapping to operations**:
- "Retrieve" maps to READ/RETRIEVAL operations (GET endpoints)

**Operations NOT justified by description**:
- CREATE (no "create", "register", "add" mentioned)
- UPDATE (no "manage", "update", "modify" mentioned)
- DELETE (no "delete", "remove", "deactivate" mentioned)

---

## Story Overview
**As a** Platform User (API Consumer)
**I want to** retrieve information about banks supported on the platform including bank ID, name, logo, and website details
**So that** I can access and display bank information for integration purposes, customer-facing applications, or operational needs

## Acceptance Criteria
1. System must allow retrieving a list of all banks supported on the platform
2. System must allow retrieving details for a specific bank by bank identifier
3. Retrieved bank information must include bank ID
4. Retrieved bank information must include bank name (short and full name)
5. Retrieved bank information must include logo URL
6. Retrieved bank information must include website URL
7. System must return appropriate error response if a specific bank does not exist (for single bank retrieval)
8. System must return an empty list (not error) if no banks exist (for list retrieval)

## Technical Context
- **Classes/Services Involved**: 
  - BankService: Handles retrieval of Bank entities
  - BankRepository: Data access layer for Bank entities
  - BankAttributeService: Retrieves bank attributes (for single bank retrieval)
  - JSONFactory400: Creates JSON response objects for bank data
- **Input Data**: 
  - Bank identifier (BANK_ID) in URL path (for single bank retrieval)
  - No input required (for list all banks)
- **Output Data**: 
  - List of banks with details (id, short_name, full_name, logo, website, bank_routings)
  - Single bank details with attributes (id, short_name, full_name, logo, website, bank_routings, attributes)
  - Error messages for not found cases
- **Processing Type**: REST API (Synchronous)

## Relevant Endpoints

### 1. Retrieve All Banks
- **Endpoint**: GET /banks
  - **Justification (from description)**: "Retrieve information about banks supported on the platform" - the word "Retrieve" and plural "banks" justifies a GET endpoint to retrieve all banks
  - **Purpose**: Retrieve a list of all banks supported on the platform with their basic information
  - **Request**: 
    - No parameters required
  - **Response**: 
    ```json
    {
      "banks": [
        {
          "id": "string",
          "short_name": "string",
          "full_name": "string",
          "logo": "string (URL)",
          "website": "string (URL)",
          "bank_routings": [
            {
              "scheme": "string",
              "address": "string"
            }
          ]
        }
      ]
    }
    ```

### 2. Retrieve Single Bank Details
- **Endpoint**: GET /banks/BANK_ID
  - **Justification (from description)**: "Retrieve information about banks...including bank ID" - the word "Retrieve" combined with "bank ID" justifies a GET endpoint to retrieve a specific bank by its identifier
  - **Purpose**: Retrieve complete details for a specific bank including name, logo, website, routing information, and bank attributes
  - **Request**: 
    - Path parameter: BANK_ID (bank identifier)
  - **Response**: 
    ```json
    {
      "id": "string",
      "short_name": "string",
      "full_name": "string",
      "logo": "string (URL)",
      "website": "string (URL)",
      "bank_routings": [
        {
          "scheme": "string",
          "address": "string"
        }
      ],
      "attributes": [
        {
          "bank_id": "string",
          "name": "string",
          "type": "string",
          "value": "string",
          "is_active": "boolean"
        }
      ]
    }
    ```

**Note**: Only GET (retrieval) endpoints are included because the capability description explicitly uses the word "Retrieve". No POST, PUT, PATCH, or DELETE endpoints are included as there are no creation, update, or deletion verbs in the description.

## Business Rules (from capability description)

1. **Bank Existence Validation**: When retrieving a specific bank by ID, the system must verify the bank exists before returning information. If not found, return appropriate error (404 Not Found).

2. **Complete Information for Single Bank**: When retrieving a single bank, the response must include all specified fields: bank ID, name (short and full), logo URL, website URL, routing information, and bank attributes.

3. **Basic Information for Bank List**: When retrieving all banks, the response must include basic information for each bank (id, name, logo, website, routing) but may exclude detailed attributes for performance optimization.

4. **Empty List Handling**: When no banks exist in the system, the list endpoint must return an empty array with HTTP 200 status, not a 404 error.

## Data Validations

- For GET /banks/BANK_ID: Bank identifier (BANK_ID) must be provided in the URL path
- For GET /banks/BANK_ID: Bank identifier must exist in the system (return 404 if not found)
- For GET /banks: No validation required (returns empty list if no banks exist)

## Dependencies

- **Upstream**: 
  - Authentication service (to verify user has permission to access bank information)
  - Bank data must be pre-configured in the system
- **Downstream**: 
  - API consumers can use retrieved bank information for display, integration, or further operations
- **External Systems**: 
  - None explicitly mentioned in the capability description

## Notes for Implementation

### Special Considerations
- GET /banks returns a list of banks WITHOUT detailed attributes (for performance optimization)
- GET /banks/BANK_ID returns a single bank WITH attributes (complete information)
- Handle cases where bank exists but has no attributes (return empty attributes array, not null)
- Return appropriate HTTP status codes:
  - 200 OK for successful retrieval
  - 404 Not Found for non-existent bank ID (single bank retrieval only)
- GET /banks should return empty list [] if no banks exist (not 404)
- Bank routing information should be included in responses

### Scala Source Code Reference
Based on the OBP-API Scala implementation:
- Endpoint implementation: `APIMethods400.scala` (lines 259-301)
- Service layer: `NewStyle.function.getBanks()` and `NewStyle.function.getBank()`
- JSON factory: `JSONFactory400.createBanksJson()` and `JSONFactory400.createBankJSON400()`
- Bank attributes: `NewStyle.function.getBankAttributesByBank()`

### Questions for SME
1. Are there any access control restrictions on which users can retrieve bank details?
2. Should deprecated fields (swiftBic, nationalIdentifier) be included in the response?
3. Is pagination required for the bank list endpoint?
4. Should bank routing information always be included or be optional?

---

## Quality Checklist Verification
- [x] Written from business perspective (not technical)
- [x] User role is clearly identified (Platform User/API Consumer)
- [x] Business value is stated (access and display bank information)
- [x] Acceptance criteria are testable
- [x] All major logic paths from the description are covered
- [x] Dependencies mentioned in the description are documented
- [x] Unclear areas are flagged (Questions for SME)
- [x] Only relevant endpoints are included (GET only)
- [x] All details align with the capability description provided
- [x] No information from other capabilities is included
- [x] For each endpoint, specific word/phrase from description justifies inclusion ("Retrieve")
- [x] No endpoint type added unless its verb appears in description
- [x] No CRUD operations inferred beyond what description explicitly states
