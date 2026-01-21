# User Story for Customer Attribute Management

## Story Overview

**As a** Bank Administrator or API Consumer
**I want to** manage custom attributes associated with customers
**So that** I can extend customer metadata with additional typed key-value pairs for business-specific requirements such as loyalty tiers, risk scores, preferred communication channels, and other customer-specific attributes

## Acceptance Criteria

1. The system shall allow authorized users to create new custom attributes for a specific customer
2. The system shall allow authorized users to update existing custom attributes on a customer
3. Customer attributes must be linked to a specific customer via CUSTOMER_ID
4. Customer attributes must be associated with a bank via BANK_ID
5. Each attribute must have a name, type, and value
6. The attribute type must be one of the valid attribute types (e.g., STRING, INTEGER, DOUBLE, DATE_WITH_DAY)
7. The system shall validate the attribute type before creating or updating
8. The system shall verify that the bank and customer exist before creating/updating attributes
9. The system shall enforce proper authorization (entitlements) for attribute management operations
10. The system shall return the created/updated attribute with its unique identifier

## Technical Context

- **Classes/Services Involved**:
  - `CustomerAttributeProvider` - Trait defining the customer attribute operations
  - `MappedCustomerAttributeProvider` - Implementation of the customer attribute provider
  - `CustomerAttributeX` - Injector for customer attribute provider
  - API endpoint definitions for customer attribute operations
  - `NewStyle.function` - Helper functions for async operations and validation

- **Input Data**:
  - `CustomerAttributeJson` - Request body containing:
    - `name`: String - The attribute name (e.g., "LOYALTY_TIER", "RISK_SCORE", "PREFERRED_CHANNEL")
    - `type`: String - The attribute type (STRING, INTEGER, DOUBLE, DATE_WITH_DAY)
    - `value`: String - The attribute value
  - Path parameters: BANK_ID, CUSTOMER_ID, CUSTOMER_ATTRIBUTE_ID (for updates)

- **Output Data**:
  - `CustomerAttributeResponseJson` - Response containing:
    - `customer_attribute_id`: String - Unique identifier for the attribute
    - `name`: String - The attribute name
    - `type`: String - The attribute type
    - `value`: String - The attribute value
    - `bank_id`: String - The bank identifier
    - `customer_id`: String - The customer identifier

- **Processing Type**: API (REST request-response)

## Relevant Endpoints

**IMPORTANT**: The capability description states "Manage custom attributes associated with customers". According to the Operation Derivation Rules, "manage" means ONLY update/configure/maintain operations, which includes create and update.

### Endpoint 1: Create Customer Attribute

- **Endpoint**: `POST /banks/BANK_ID/customers/CUSTOMER_ID/attribute`
- **Justification (from description)**: "Manage" - creating is part of managing/configuring customer attributes
- **Purpose**: Create a new custom attribute for a specific customer at a bank
- **Request**:
  ```json
  {
    "name": "LOYALTY_TIER",
    "type": "STRING",
    "value": "GOLD"
  }
  ```
- **Response**:
  ```json
  {
    "customer_attribute_id": "7uy8a7e4-6d02-40e3-a129-0b2bf89de8uh",
    "name": "LOYALTY_TIER",
    "type": "STRING",
    "value": "GOLD",
    "bank_id": "gh.29.uk",
    "customer_id": "8ca8a7e4-6d02-40e3-a129-0b2bf89de9f0"
  }
  ```
- **Required Entitlement**: `canCreateCustomerAttributeAtOneBank`
- **HTTP Status**: 201 Created

### Endpoint 2: Update Customer Attribute

- **Endpoint**: `PUT /banks/BANK_ID/customers/CUSTOMER_ID/attributes/CUSTOMER_ATTRIBUTE_ID`
- **Justification (from description)**: "Manage" - updating is the core meaning of managing/maintaining attributes
- **Purpose**: Update an existing custom attribute for a specific customer
- **Request**:
  ```json
  {
    "name": "LOYALTY_TIER",
    "type": "STRING",
    "value": "PLATINUM"
  }
  ```
- **Response**:
  ```json
  {
    "customer_attribute_id": "7uy8a7e4-6d02-40e3-a129-0b2bf89de8uh",
    "name": "LOYALTY_TIER",
    "type": "STRING",
    "value": "PLATINUM",
    "bank_id": "gh.29.uk",
    "customer_id": "8ca8a7e4-6d02-40e3-a129-0b2bf89de9f0"
  }
  ```
- **Required Entitlement**: `canUpdateCustomerAttribute`
- **HTTP Status**: 200 OK

## Business Rules

1. **Attribute Type Validation**: The `type` field must be one of the following valid types:
   - `STRING` - For text values (e.g., "GOLD", "HIGH")
   - `INTEGER` - For whole number values (e.g., "100")
   - `DOUBLE` - For decimal values (e.g., "85.5")
   - `DATE_WITH_DAY` - For date values (e.g., "2024-01-15")

2. **Entity Existence Validation**: Before creating or updating an attribute:
   - The bank (BANK_ID) must exist in the system
   - The customer (CUSTOMER_ID) must exist within the specified bank

3. **Authorization Requirements**:
   - Creating attributes requires `canCreateCustomerAttributeAtOneBank` entitlement
   - Updating attributes requires `canUpdateCustomerAttribute` entitlement

4. **Attribute Uniqueness**: Each customer attribute is uniquely identified by its `customer_attribute_id`

5. **Bank-Customer Association**: Customer attributes are linked to both a customer and a bank, ensuring proper data isolation between banks

## Data Validations

- **Type Field Validation**: The system validates that the `type` field contains a valid `CustomerAttributeType` enum value
- **JSON Format Validation**: The request body must be valid JSON matching the `CustomerAttributeJson` schema
- **Entity Existence Checks**: 
  - Bank must exist (returns error if not found)
  - Customer must exist within the bank (returns error if not found)
- **Attribute Existence Check (for updates)**: The attribute with the specified `CUSTOMER_ATTRIBUTE_ID` must exist before updating
- **Authentication Required**: User must be logged in to perform any attribute management operations

## Dependencies

- **Upstream**:
  - Bank must be created and exist in the system
  - Customer must be created within the bank
  - User must be authenticated and have appropriate entitlements

- **Downstream**:
  - Customer attributes may be included in customer detail responses
  - Customer attributes may be used for filtering/searching customers
  - Customer attributes may be used in reporting and analytics
  - Customer attributes may be used for personalization and targeting

- **External Systems**:
  - Backend connector (LocalMappedConnector, AkkaConnector, RestConnector, etc.) for data persistence
  - Authentication system for user validation
  - Entitlement system for authorization checks

## Notes for Implementation

1. **Typical Use Cases**: Customer attributes are commonly used for:
   - LOYALTY_TIER - Customer loyalty program tier (e.g., "GOLD", "PLATINUM")
   - RISK_SCORE - Customer risk assessment score
   - PREFERRED_CHANNEL - Preferred communication channel (e.g., "EMAIL", "SMS")
   - CUSTOMER_SEGMENT - Marketing segment classification
   - ONBOARDING_DATE - Date when customer was onboarded
   - KYC_LEVEL - Level of KYC verification completed
   - CREDIT_LIMIT_OVERRIDE - Custom credit limit override value

2. **Attribute Definition Management**: There may be separate endpoints for managing Customer Attribute Definitions (schema/metadata for attributes) which are not part of this capability but may be relevant:
   - Create/Update Customer Attribute Definition
   - Get Customer Attribute Definition
   - Delete Customer Attribute Definition

3. **Needs SME Input**:
   - What are the specific attribute names and types required for the target system?
   - Are there any constraints on attribute values beyond type validation?
   - Should there be a maximum number of attributes per customer?
   - Are there any mandatory attributes that must be present on all customers?
   - Should attribute names be validated against a predefined list of allowed names?

4. **Note on Excluded Operations**: The capability description states "Manage custom attributes" which, per the Operation Derivation Rules, includes only create and update operations. The following operations exist in the codebase but are NOT included in this capability scope as they are not explicitly mentioned in the description:
   - GET operations for retrieving/viewing attributes
   - DELETE operations for removing attributes
   - LIST operations for browsing attributes
   
   If these operations are needed, the capability description should be updated to explicitly include verbs like "view", "retrieve", "list", or "delete".
