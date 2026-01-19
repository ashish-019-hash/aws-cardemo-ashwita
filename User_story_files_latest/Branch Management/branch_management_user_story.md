# User Story for Branch Management

## Story Overview

**As a** Bank Administrator or API Consumer
**I want to** create, update, retrieve, and delete bank branch information including locations and services
**So that** I can maintain accurate and up-to-date branch data for customers to locate physical banking services, access branch operating hours, and find accessibility information

## Acceptance Criteria

1. The system shall allow authorized users to create new branch records for a bank with complete location and service information
2. The system shall allow authorized users to update existing branch information including address, operating hours, and accessibility features
3. The system shall allow users to retrieve branch information by bank ID and branch ID
4. The system shall allow users to retrieve a list of all branches for a specific bank with pagination support
5. The system shall allow authorized users to delete (soft delete) branch records from the system
6. Branch creation shall require valid bank ID and branch details including name, address, and location coordinates
7. Branch updates shall validate that the branch exists before applying changes
8. Branch retrieval shall return comprehensive branch data including lobby hours, drive-up hours, and accessibility information
9. Branch deletion shall mark the branch as deleted rather than physically removing the record (soft delete)
10. All create, update, and delete operations shall require appropriate entitlements (CanCreateBranch, CanUpdateBranch, CanDeleteBranch)

## Technical Context

### Classes/Services Involved
- **Branches.scala** - Core domain model and provider trait defining Branch case class and BranchesProvider interface
- **MappedBranchesProvider.scala** - Database persistence layer implementing branch CRUD operations using Lift Mapper
- **MappedBranch** - ORM entity class mapping branch data to database columns
- **APIMethods140.scala** - API endpoint implementation for getBranches (list branches)
- **APIMethods210.scala** - API endpoint implementations for getBranch, createBranch, updateBranch
- **APIMethods310.scala** - API endpoint implementation for deleteBranch
- **JSONFactory1_4_0** - JSON serialization/deserialization for branch data
- **JSONFactory220** - Extended JSON factory for branch data with additional fields

### Input Data
- **Create Branch Request**: BranchJsonPostV210 containing bank_id, name, address (line1, line2, line3, city, county, state, country_code, post_code), location (latitude, longitude), lobby hours, drive-up hours, branch_routing, is_accessible, accessible_features, branch_type, more_info, phone_number
- **Update Branch Request**: BranchJsonPutV210 with similar fields to create request
- **Retrieve Branch Request**: Path parameters bank_id and branch_id
- **List Branches Request**: Path parameter bank_id with optional query parameters for pagination (limit, offset)
- **Delete Branch Request**: Path parameters bank_id and branch_id

### Output Data
- **Branch Response**: JSON object containing branchId, bankId, name, address object, location object (latitude, longitude), meta (license info), lobbyString (hours), driveUpString (hours), lobby (detailed opening times per day), driveUp (detailed opening times per day), branchRouting (scheme, address), isAccessible, accessibleFeatures, branchType, moreInfo, phoneNumber, isDeleted
- **Branches List Response**: Array of branch objects with pagination metadata

### Processing Type
- **API/Real-time** - All operations are synchronous REST API calls with immediate response

## Relevant Endpoints

### Endpoint 1: Create Branch
- **Endpoint**: `POST /obp/v2.1.0/banks/{BANK_ID}/branches`
- **Justification (from description)**: "Create" - explicitly mentioned in capability description
- **Purpose**: Creates a new branch record for the specified bank with complete location, service, and accessibility information
- **Request**: 
  ```json
  {
    "id": "branch-id-123",
    "bank_id": "bank-id-123",
    "name": "Main Street Branch",
    "address": {
      "line_1": "123 Main Street",
      "line_2": "Suite 100",
      "line_3": "",
      "city": "New York",
      "county": "Manhattan",
      "state": "NY",
      "postcode": "10001",
      "country_code": "US"
    },
    "location": {
      "latitude": 40.7128,
      "longitude": -74.0060
    },
    "meta": {
      "license": {
        "id": "license-id",
        "name": "Open Data License"
      }
    },
    "lobby": {
      "monday": [{"opening_time": "09:00", "closing_time": "17:00"}],
      "tuesday": [{"opening_time": "09:00", "closing_time": "17:00"}],
      "wednesday": [{"opening_time": "09:00", "closing_time": "17:00"}],
      "thursday": [{"opening_time": "09:00", "closing_time": "17:00"}],
      "friday": [{"opening_time": "09:00", "closing_time": "17:00"}],
      "saturday": [{"opening_time": "10:00", "closing_time": "14:00"}],
      "sunday": []
    },
    "drive_up": {
      "monday": {"opening_time": "08:00", "closing_time": "18:00"},
      "tuesday": {"opening_time": "08:00", "closing_time": "18:00"},
      "wednesday": {"opening_time": "08:00", "closing_time": "18:00"},
      "thursday": {"opening_time": "08:00", "closing_time": "18:00"},
      "friday": {"opening_time": "08:00", "closing_time": "18:00"},
      "saturday": {"opening_time": "09:00", "closing_time": "15:00"},
      "sunday": {"opening_time": "", "closing_time": ""}
    },
    "branch_routing": {
      "scheme": "BRANCH_ID",
      "address": "branch-id-123"
    },
    "is_accessible": "Y",
    "accessible_features": "Wheelchair ramp, automatic doors, braille signage",
    "branch_type": "Full Service",
    "more_info": "ATM available 24/7",
    "phone_number": "+1-555-123-4567"
  }
  ```
- **Response**: HTTP 201 Created with the created branch object

### Endpoint 2: Update Branch
- **Endpoint**: `PUT /obp/v2.1.0/banks/{BANK_ID}/branches/{BRANCH_ID}`
- **Justification (from description)**: "update" - explicitly mentioned in capability description
- **Purpose**: Updates an existing branch record with new information for location, services, or accessibility
- **Request**: Same structure as create request (BranchJsonPutV210)
- **Response**: HTTP 200 OK with the updated branch object

### Endpoint 3: Retrieve Single Branch
- **Endpoint**: `GET /obp/v2.1.0/banks/{BANK_ID}/branches/{BRANCH_ID}`
- **Justification (from description)**: "retrieve" - explicitly mentioned in capability description
- **Purpose**: Retrieves detailed information about a specific branch by bank ID and branch ID
- **Request**: Path parameters BANK_ID and BRANCH_ID
- **Response**: HTTP 200 OK with branch object containing all branch details

### Endpoint 4: Retrieve All Branches (List)
- **Endpoint**: `GET /obp/v1.4.0/banks/{BANK_ID}/branches`
- **Justification (from description)**: "retrieve" - explicitly mentioned in capability description (retrieving branch information implies listing capability)
- **Purpose**: Retrieves a list of all branches for a specific bank with pagination support
- **Request**: Path parameter BANK_ID, optional query parameters: limit, offset
- **Response**: HTTP 200 OK with array of branch objects

### Endpoint 5: Delete Branch
- **Endpoint**: `DELETE /obp/v3.1.0/banks/{BANK_ID}/branches/{BRANCH_ID}`
- **Justification (from description)**: "delete" - explicitly mentioned in capability description
- **Purpose**: Soft deletes a branch record by marking it as deleted (isDeleted = true)
- **Request**: Path parameters BANK_ID and BRANCH_ID
- **Response**: HTTP 204 No Content on successful deletion

## Business Rules

1. **Bank Existence Validation**: A branch can only be created for an existing bank. The bank_id must reference a valid bank in the system.

2. **Branch Uniqueness**: Each branch must have a unique combination of bank_id and branch_id. Duplicate branch IDs within the same bank are not allowed.

3. **Entitlement Requirements**: 
   - Creating a branch requires CanCreateBranch entitlement for the specific bank OR CanCreateBranchAtAnyBank for system-wide access
   - Updating a branch requires CanUpdateBranch entitlement
   - Deleting a branch requires CanDeleteBranch entitlement for the specific bank OR CanDeleteBranchAtAnyBank

4. **Public Access Configuration**: Branch retrieval (GET) operations can be configured as public or require authentication based on the `apiOptions.getBranchesIsPublic` configuration property.

5. **Soft Delete Pattern**: Branch deletion does not physically remove records but sets the isDeleted flag to true. Deleted branches are excluded from list queries.

6. **Time Format**: Opening and closing times must follow 24-hour clock format (e.g., "13:45"). Times after midnight can be represented as values greater than 24:00 (e.g., "25:30" for 1:30 AM).

7. **Accessibility Indicator**: The isAccessible field uses a tristate value: "Y" for accessible, "N" for not accessible, empty string for unknown.

8. **Branch Routing**: If branch routing scheme and address are not provided, the system defaults to using "BRANCH_ID" as the scheme and the branch_id as the address.

## Data Validations

- **Required Fields**: bank_id, branch_id, name, and address are required for branch creation
- **Location Coordinates**: Latitude must be between -90 and 90, longitude must be between -180 and 180
- **Country Code**: Must be a valid 2-character ISO country code
- **Time Format Validation**: Opening and closing times must match the 24-hour clock format pattern (HH:MM)
- **JSON Format Validation**: Request body must be valid JSON matching the expected schema (BranchJsonPostV210 or BranchJsonPutV210)
- **Bank Existence Check**: The specified bank_id must exist in the system
- **Branch Existence Check (for update/delete)**: The specified branch_id must exist for the given bank_id

### Error Conditions
- **400 Bad Request**: Invalid JSON format, missing required fields, or validation failures
- **401 Unauthorized**: Missing or invalid authentication
- **403 Forbidden**: User lacks required entitlements (InsufficientAuthorisationToCreateBranch, InsufficientAuthorisationToDeleteBranch)
- **404 Not Found**: Bank or branch not found (BranchNotFoundByBranchId)
- **204 No Content**: No branches available for the specified bank (when license may not be set)

## Dependencies

### Upstream
- **Bank Management**: A bank must exist before branches can be created for it
- **User Authentication**: User must be authenticated for create, update, and delete operations
- **Entitlement Management**: User must have appropriate entitlements granted for write operations
- **License Configuration**: Data license must be configured for the bank to expose branch data

### Downstream
- **ATM Management**: ATMs may reference branch locations
- **Customer Services**: Customers may be associated with specific branches
- **Transaction Processing**: Some transactions may reference the originating branch
- **Reporting**: Branch data feeds into various operational and compliance reports

### External Systems
- **Connector Layer**: Branch operations are routed through the OBP connector framework which can integrate with various backend banking systems (LocalMappedConnector, AkkaConnector, RestConnector, RabbitMQConnector, StoredProcedureConnector)
- **Database**: Branch data is persisted using Lift Mapper ORM to the configured database

## Notes for Implementation

### Special Considerations
1. **Lobby vs DriveUp Hours**: The system supports both lobby hours (detailed per-day schedule with multiple time slots) and drive-up hours (single time slot per day). Both deprecated string-based hours and structured time objects are supported for backward compatibility.

2. **Pagination**: List branches endpoint supports pagination via limit and offset query parameters to handle banks with many branches efficiently.

3. **Soft Delete Implementation**: The delete operation sets isDeleted to true rather than removing the record, allowing for audit trails and potential recovery.

4. **Multi-version API Support**: Branch endpoints exist across multiple API versions (v1.4.0, v2.1.0, v2.2.0, v3.0.0, v3.1.0) with varying levels of detail and features.

### Known Complexity
1. **Opening Hours Structure**: The lobby hours support multiple time slots per day (List of OpeningTimes) while drive-up hours support only a single time slot per day. This asymmetry requires careful handling in the Go implementation.

2. **Backward Compatibility**: Both lobbyString/driveUpString (deprecated string format) and lobby/driveUp (structured format) fields must be supported for API compatibility.

### Missing or Unclear Requirements (Needs SME Input)
1. **Branch Services**: The capability description mentions "services" but the current data model does not have a dedicated services field. Clarification needed on how branch services should be represented.

2. **Search/Filter Capabilities**: The description mentions "retrieve" but does not specify search or filter criteria beyond bank_id. Should branches be searchable by location, name, or other attributes?

3. **Bulk Operations**: Should bulk create/update/delete operations be supported for managing multiple branches efficiently?

4. **Branch Status**: Beyond isDeleted, should there be additional status fields (e.g., temporarily closed, under renovation)?

5. **Geospatial Queries**: Should the system support finding branches within a certain radius of a location?
