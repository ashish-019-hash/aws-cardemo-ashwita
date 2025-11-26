User story: Bank Information Retrieval

## Story Overview
**As a** Bank System User  
**I want to** retrieve bank details including name, logo, website and attributes  
**So that** I can access and display complete bank information for operational and customer-facing purposes

## Acceptance Criteria
1. System must allow retrieving bank details by bank identifier
2. Retrieved bank details must include name (short and full)
3. Retrieved bank details must include logo URL
4. Retrieved bank details must include website URL
5. Retrieved bank details must include bank attributes (operational parameters)
6. System must return appropriate error if bank does not exist

## Technical Context
- **Classes/Services Involved**: 
  - BankService: Handles retrieval of Bank entities
  - BankRepository: Data access for Bank entities
  - BankAttributeService: Retrieves bank attributes
- **Input Data**: 
  - Bank identifier (bankId) in URL path
- **Output Data**: 
  - Bank details (name, logo, website)
  - Bank attributes list
  - Error messages for not found cases
- **Processing Type**: REST API

## Relevant Endpoints

### 1. Retrieve Bank Details
- **Endpoint**: GET /api/banks/{bankId}
  - **Purpose**: Retrieve complete bank details including name, logo, website and attributes
  - **Request**: 
    - Path parameter: bankId (bank identifier)
  - **Response**: 
    ```json
    {
      "bankId": "string",
      "shortName": "string",
      "fullName": "string",
      "logo": "string",
      "website": "string",
      "attributes": [
        {
          "name": "string",
          "type": "string",
          "value": "string",
          "isActive": "boolean"
        }
      ]
    }
    ```

**Note**: Only retrieval endpoint is included because the description mentions "Retrieve bank details" - no creation, update, or deletion operations are mentioned in the description.

## Business Rules

1. **Valid Bank Identifier**: Bank identifier must exist in the system to retrieve details
2. **Complete Information**: Retrieved bank details must include all specified fields (name, logo, website, attributes)

## Data Validations

- Bank identifier must be provided and non-empty
- Bank identifier must exist in the system (return 404 if not found)

## Dependencies

- Authentication service (to verify user permissions for viewing bank details)

## Notes for Implementation

### Special Considerations
- Ensure all bank attributes are included in the response
- Handle cases where bank exists but has no attributes
- Return appropriate HTTP status codes (200 for success, 404 for not found)

### Questions for SME
1. Are there any access control restrictions on which users can retrieve bank details?
2. Should the response include deprecated fields (swiftBic, nationalIdentifier)?
3. Should bank routing information be included in the retrieval response?
