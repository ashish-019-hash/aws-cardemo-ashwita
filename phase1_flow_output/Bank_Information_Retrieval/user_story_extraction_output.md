# User Story for Bank Information Retrieval

## Story Overview

**As a** platform user or third-party application developer

**I want to** retrieve information about banks supported on the platform including bank ID, name, logo, and website details

**So that** I can display bank information to end users, enable bank selection in applications, and integrate with the appropriate banking services

## Acceptance Criteria

1. **AC-001**: The system shall provide an endpoint to retrieve a list of all banks supported on the platform
2. **AC-002**: The system shall provide an endpoint to retrieve detailed information for a specific bank by bank ID
3. **AC-003**: Each bank record shall include the bank ID, short name, full name, logo URL, and website URL
4. **AC-004**: The bank list endpoint shall return bank routing information for each bank
5. **AC-005**: The single bank retrieval endpoint shall return bank attributes in addition to core bank information
6. **AC-006**: If a requested bank ID does not exist, the system shall return an appropriate error response (404 Not Found)
7. **AC-007**: If no banks exist in the system, the bank list endpoint shall return an empty list with HTTP 200 status (not 404)

## Technical Context

- **Classes/Services Involved**:
  - `APIMethods400.scala` - API endpoint definitions for bank retrieval
  - `Connector.scala` - Bank data access interface with `getBank` and `getBanks` methods
  - `LocalMappedConnector.scala` - Local database implementation for bank retrieval
  - `JSONFactory400.scala` - JSON response factory for creating bank response objects
  - `NewStyle.function` - Service layer functions for bank operations

- **Input Data**:
  - GET /banks: No input parameters required
  - GET /banks/BANK_ID: Path parameter `BANK_ID` (string) - unique identifier for the bank

- **Output Data**:
  - Bank list response containing array of bank objects with: id, short_name, full_name, logo, website, bank_routings
  - Single bank response containing: id, short_name, full_name, logo, website, bank_routings, attributes

- **Processing Type**: Real-time API (synchronous REST endpoints)

## Relevant Endpoints

### Endpoint 1: GET /banks

- **Justification (from description)**: "Retrieve information about banks supported on the platform" - the word "Retrieve" justifies a GET operation for listing banks
- **Purpose**: Retrieve a list of all banks supported on the platform with basic information
- **Request**: 
  - Method: GET
  - Path: /banks
  - Headers: Optional authentication headers
  - Body: None
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

### Endpoint 2: GET /banks/{BANK_ID}

- **Justification (from description)**: "Retrieve information about banks" - the word "Retrieve" justifies a GET operation for fetching specific bank details; "including bank ID" indicates retrieval by identifier
- **Purpose**: Retrieve detailed information about a specific bank including attributes
- **Request**:
  - Method: GET
  - Path: /banks/{BANK_ID}
  - Path Parameters: BANK_ID (string) - unique identifier for the bank
  - Headers: Optional authentication headers
  - Body: None
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

## Business Rules

1. **BR-001 - Bank Existence Validation**: When retrieving a specific bank by ID, the system must verify that the bank exists before returning information. If the bank does not exist, return 404 Not Found.

2. **BR-002 - Complete Bank Information Composition**: Single bank retrieval must return complete information including core fields (id, short_name, full_name, logo, website), bank routing information, and bank attributes.

3. **BR-003 - Basic Bank Information for List**: Bank list retrieval returns basic information (core fields and routing) but excludes attributes for performance optimization.

4. **BR-004 - Empty Result Handling**: When no banks exist, return an empty list with HTTP 200 status, not a 404 error. This distinguishes between "no data available" (valid state) and "resource not found" (error state).

## Data Validations

- **Bank ID Format**: Bank ID must be a valid non-empty string when provided as a path parameter
- **Response Structure**: All bank responses must include required fields (id, short_name, full_name, logo, website)
- **URL Validation**: Logo and website fields should contain valid URL formats when populated
- **Empty Attributes Handling**: If a bank has no attributes, return an empty array [] rather than null

## Dependencies

- **Upstream**:
  - Bank data must be pre-configured in the system database
  - Bank routing information must be associated with bank records
  - Bank attributes (if any) must be configured for individual banks

- **Downstream**:
  - Bank information is used by account management features to display bank context
  - Bank selection is required for account creation and transaction initiation
  - Third-party applications use bank information for user interface display

- **External Systems**:
  - No external system dependencies for basic bank information retrieval
  - Bank data is stored in the local database (RDBMS via Lift/Mapper ORM)

## Notes for Implementation

### Migration Considerations for Go

1. **HTTP Router Setup**: Implement two routes:
   - `GET /banks` - handler for listing all banks
   - `GET /banks/:bank_id` - handler for single bank retrieval with path parameter

2. **Response Structures**: Define Go structs for:
   - `BankListResponse` - containing array of basic bank info
   - `BankDetailResponse` - containing full bank info with attributes
   - `BankRouting` - for routing scheme and address
   - `BankAttribute` - for attribute metadata

3. **Error Handling**: 
   - Return 404 with appropriate error message for non-existent bank ID
   - Return 200 with empty array for empty bank list
   - Use consistent error response format

4. **Database Layer**:
   - Implement repository pattern for bank data access
   - Support retrieval of bank with associated routing information
   - Support retrieval of bank attributes by bank ID

5. **JSON Serialization**:
   - Ensure empty slices serialize as `[]` not `null`
   - Use proper JSON tags for field naming (snake_case)

### Open Questions for SME

1. Are there any deprecated fields (e.g., swiftBic, nationalIdentifier) that should be excluded from the new Go implementation?
2. Should bank list support pagination for large numbers of banks?
3. Are there any rate limiting requirements for these endpoints?
4. Should bank attributes be cached for performance optimization?

## Source Code References

- **Scala API Implementation**: `obp-api/src/main/scala/code/api/v4_0_0/APIMethods400.scala` (lines 239-301)
- **Connector Interface**: `obp-api/src/main/scala/code/bankconnectors/Connector.scala` (lines 450-457)
- **JSON Factory**: `obp-api/src/main/scala/code/api/v4_0_0/JSONFactory400.scala`

## Quality Checklist

- [x] Written from business perspective (not technical)
- [x] User role is clearly identified
- [x] Business value is stated
- [x] Acceptance criteria are testable
- [x] All major logic paths from the description are covered
- [x] Dependencies mentioned in the description are documented
- [x] Unclear areas are flagged
- [x] Only relevant endpoints are included (GET operations for retrieval)
- [x] All details align with the capability description provided
- [x] No information from other capabilities is included
- [x] For each endpoint, specific words from the capability description justify inclusion
- [x] No CRUD operations are inferred beyond what the description explicitly states ("Retrieve" only)
