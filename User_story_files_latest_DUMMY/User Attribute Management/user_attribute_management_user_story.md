# User Story for User Attribute Management

## Story Overview

**As a** Bank Administrator or System Administrator
**I want to** manage custom attributes associated with users
**So that** I can extend user profiles with additional typed key-value pairs for business-specific requirements such as department codes, employee IDs, access levels, custom preferences, and other user-specific metadata

## Acceptance Criteria

1. The system shall allow authorized administrators to create new custom attributes for a specific user
2. The system shall allow authorized administrators to update existing custom attributes on a user
3. User attributes must be linked to a specific user via USER_ID
4. Each attribute must have a name, type, and value
5. The attribute type must be one of the supported types: STRING, INTEGER, DOUBLE, or DATE_WITH_DAY
6. The system shall validate the attribute type before creating or updating
7. The system shall verify that the bank and user exist before creating/updating attributes
8. The system shall enforce proper authorization (entitlements) for attribute management operations
9. The system shall return appropriate error messages for invalid requests

## Technical Context

- **Classes/Services Involved**:
  - `UserAttributeProvider` - Trait defining the user attribute operations
  - `MappedUserAttributeProvider` - Implementation of the user attribute provider
  - `UserAttributeX` - Injector for user attribute provider
  - `APIMethods` - API endpoint definitions for user attribute operations
  - `NewStyle.function` - Helper functions for async operations and validation

- **Input Data**:
  - `UserAttributeJson` - Request body containing:
    - `name`: String - The attribute name (e.g., "DEPARTMENT", "EMPLOYEE_ID", "ACCESS_LEVEL")
    - `type`: String - The attribute type (STRING, INTEGER, DOUBLE, DATE_WITH_DAY)
    - `value`: String - The attribute value
  - Path parameters: BANK_ID, USER_ID, USER_ATTRIBUTE_ID (for updates)

- **Output Data**:
  - `UserAttributeResponseJson` - Response containing:
    - `user_attribute_id`: String - Unique identifier for the attribute
    - `name`: String - The attribute name
    - `type`: String - The attribute type
    - `value`: String - The attribute value
    - `bank_id`: String - The bank identifier
    - `user_id`: String - The user identifier

- **Processing Type**: API (REST request-response)

## Relevant Endpoints

**IMPORTANT**: The capability description states "Manage custom attributes associated with users". According to the Operation Derivation Rules, "manage" means ONLY update/configure/maintain operations.

### Endpoint 1: Create User Attribute

- **Endpoint**: `POST /banks/BANK_ID/users/USER_ID/attributes`
- **Justification (from description)**: "Manage" - creating is part of managing/configuring user attributes
- **Purpose**: Create a new custom attribute for a specific user within a bank
- **Request**:
  ```json
  {
    "name": "DEPARTMENT",
    "type": "STRING",
    "value": "Finance"
  }
  ```
- **Response**:
  ```json
  {
    "user_attribute_id": "7uy8a7e4-6d02-40e3-a129-0b2bf89de8uh",
    "name": "DEPARTMENT",
    "type": "STRING",
    "value": "Finance",
    "bank_id": "gh.29.uk",
    "user_id": "9ca9a8e5-7e03-51f4-b230-1c3cf90ef0g1"
  }
  ```
- **Required Entitlement**: `canCreateUserAttributeAtOneBank`
- **HTTP Status**: 201 Created

### Endpoint 2: Update User Attribute

- **Endpoint**: `PUT /banks/BANK_ID/users/USER_ID/attributes/USER_ATTRIBUTE_ID`
- **Justification (from description)**: "Manage" - updating is the core meaning of managing/maintaining attributes
- **Purpose**: Update an existing custom attribute for a specific user
- **Request**:
  ```json
  {
    "name": "DEPARTMENT",
    "type": "STRING",
    "value": "Operations"
  }
  ```
- **Response**:
  ```json
  {
    "user_attribute_id": "7uy8a7e4-6d02-40e3-a129-0b2bf89de8uh",
    "name": "DEPARTMENT",
    "type": "STRING",
    "value": "Operations",
    "bank_id": "gh.29.uk",
    "user_id": "9ca9a8e5-7e03-51f4-b230-1c3cf90ef0g1"
  }
  ```
- **Required Entitlement**: `canUpdateUserAttribute`
- **HTTP Status**: 200 OK

## Business Rules

1. **Attribute Type Validation**: The `type` field must be one of the following valid types:
   - `STRING` - For text values (e.g., "Finance", "Manager")
   - `INTEGER` - For whole number values (e.g., "12345")
   - `DOUBLE` - For decimal values (e.g., "1500.50")
   - `DATE_WITH_DAY` - For date values (e.g., "2024-01-15")

2. **Entity Existence Validation**: Before creating or updating an attribute:
   - The bank (BANK_ID) must exist in the system
   - The user (USER_ID) must exist within the specified bank

3. **Authorization Requirements**:
   - Creating attributes requires `canCreateUserAttributeAtOneBank` entitlement
   - Updating attributes requires `canUpdateUserAttribute` entitlement

4. **Attribute Uniqueness**: Each user attribute is uniquely identified by its `user_attribute_id`

5. **Bank Association**: User attributes are scoped to a specific bank, enabling bank-specific metadata on users

## Data Validations

- **Type Field Validation**: The system validates that the `type` field contains a valid `UserAttributeType` enum value
- **JSON Format Validation**: The request body must be valid JSON matching the `UserAttributeJson` schema
- **Entity Existence Checks**: 
  - Bank must exist (returns error if not found)
  - User must exist within the bank (returns error if not found)
- **Attribute Existence Check (for updates)**: The attribute with the specified `USER_ATTRIBUTE_ID` must exist before updating
- **Authentication Required**: User must be logged in to perform any attribute management operations

## Dependencies

- **Upstream**:
  - Bank must be created and exist in the system
  - User must be created within the bank
  - Administrator must be authenticated and have appropriate entitlements

- **Downstream**:
  - User attributes may be included in user detail responses
  - User attributes may be used for filtering/searching users
  - User attributes may be used in reporting and analytics
  - User attributes may be used for access control decisions

- **External Systems**:
  - Backend connector (LocalMappedConnector, AkkaConnector, RestConnector, etc.) for data persistence
  - Authentication system for user validation
  - Entitlement system for authorization checks

## Notes for Implementation

1. **Typical Use Cases**: User attributes are commonly used for:
   - DEPARTMENT - User's department or division
   - EMPLOYEE_ID - Internal employee identifier
   - ACCESS_LEVEL - Custom access tier or clearance level
   - COST_CENTER - Financial cost center assignment
   - MANAGER_ID - Reference to user's manager
   - HIRE_DATE - Date when user was onboarded
   - CUSTOM_ROLE - Business-specific role designation

2. **Attribute Definition Management**: There may be separate endpoints for managing User Attribute Definitions (schema/metadata for attributes) which are not part of this capability but may be relevant:
   - Create/Update User Attribute Definition
   - Get User Attribute Definition
   - Delete User Attribute Definition

3. **Needs SME Input**:
   - What are the specific attribute names and types required for the target system?
   - Are there any constraints on attribute values beyond type validation?
   - Should there be a maximum number of attributes per user?
   - Are there any mandatory attributes that must be present on all users?
   - Should attributes be inherited or shared across banks for the same user?

4. **Note on Excluded Operations**: The capability description states "Manage custom attributes" which, per the Operation Derivation Rules, includes only create and update operations. The following operations may exist in the codebase but are NOT included in this capability scope as they are not explicitly mentioned in the description:
   - GET operations for retrieving/viewing attributes
   - DELETE operations for removing attributes
   - LIST operations for browsing attributes
   
   If these operations are needed, the capability description should be updated to explicitly include verbs like "view", "retrieve", "list", or "delete".
