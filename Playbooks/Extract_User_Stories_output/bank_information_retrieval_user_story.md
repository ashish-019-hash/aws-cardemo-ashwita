User story: Bank Information Retrieval

## Story Overview
**As a** Bank System User  
**I want to** retrieve bank details including name, logo, website and attributes  
**So that** I can access and display complete bank information for operational and customer-facing purposes

## Acceptance Criteria
1. System must allow retrieving details for all banks
2. System must allow retrieving bank details by bank identifier
3. Retrieved bank details must include name (short and full)
4. Retrieved bank details must include logo URL
5. Retrieved bank details must include website URL
6. Retrieved bank details must include bank routing information
7. Retrieved single bank details must include bank attributes (operational parameters)
8. System must return appropriate error if bank does not exist (for single bank retrieval)

## Technical Context
- **Classes/Services Involved**: 
  - BankService: Handles retrieval of Bank entities (single and list)
  - BankRepository: Data access for Bank entities
  - BankAttributeService: Retrieves bank attributes (for single bank retrieval)
- **Input Data**: 
  - Bank identifier (bankId) in URL path (for single bank retrieval)
  - No input required (for list all banks)
- **Output Data**: 
  - List of banks with details (name, logo, website, routing)
  - Single bank details with attributes
  - Error messages for not found cases
- **Processing Type**: REST API

## Relevant Endpoints

### 1. Retrieve All Banks
- **Endpoint**: GET /banks
  - **Purpose**: Retrieve details for all banks including name, logo, website and routing information
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
          "logo": "string",
          "website": "string",
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
  - **Purpose**: Retrieve complete bank details including name, logo, website, routing and attributes for a specific bank
  - **Request**: 
    - Path parameter: BANK_ID (bank identifier)
  - **Response**: 
    ```json
    {
      "id": "string",
      "short_name": "string",
      "full_name": "string",
      "logo": "string",
      "website": "string",
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

**Note**: Both retrieval endpoints are included because the capability "Bank Information Retrieval" encompasses retrieving bank details for both multiple banks (list) and single banks (by ID). The GET /banks endpoint returns basic bank information without attributes, while GET /banks/BANK_ID returns complete information including bank attributes. No creation, update, or deletion operations are included as they are not part of this retrieval capability.

## Business Rules

1. **Valid Bank Identifier**: Bank identifier must exist in the system to retrieve single bank details (GET /banks/BANK_ID)
2. **Complete Information for Single Bank**: Retrieved single bank details must include all specified fields (name, logo, website, routing, attributes)
3. **Basic Information for Bank List**: Retrieved bank list must include basic fields for each bank (name, logo, website, routing) but not attributes

## Data Validations

- For GET /banks/BANK_ID: Bank identifier must be provided and non-empty
- For GET /banks/BANK_ID: Bank identifier must exist in the system (return 404 if not found)
- For GET /banks: No validation required (returns empty list if no banks exist)

## Dependencies

- Authentication service (to verify user permissions for viewing bank details)

## Notes for Implementation

### Special Considerations
- GET /banks returns a list of banks WITHOUT attributes (for performance reasons)
- GET /banks/BANK_ID returns a single bank WITH attributes (complete information)
- Handle cases where bank exists but has no attributes (return empty attributes array)
- Return appropriate HTTP status codes (200 for success, 404 for not found on single bank retrieval)
- GET /banks should return empty list if no banks exist (not 404)

### Questions for SME
1. Are there any access control restrictions on which users can retrieve bank details?
2. Should the response include deprecated fields (swiftBic, nationalIdentifier)?
3. Should bank routing information be included in the retrieval response?
