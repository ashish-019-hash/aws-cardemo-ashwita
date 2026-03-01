# Validation Rules

**Extracted From:** Bank Attribute Management User Story  
**User Story:** Bank Attribute Management  
**Analysis Date:** December 01, 2025  
**Analyst:** Scala Systems Analyst

## Summary

Total Validation Rules Extracted: 15

### Validation Categories
- Input Validation Rules: 6
- Format Validation Rules: 4
- Business Constraint Rules: 4
- Response Validation Rules: 1

---

## Category: Input Validation

### Rule VR-001: Bank Identifier Required Validation

**Field/Entity:** BANK_ID (path parameter)

**Validation Type:** Required Field Validation

**Rule Description:**
Bank identifier must be provided and non-empty for all bank attribute operations (create, retrieve, update, delete).

**Validation Logic:**

- **Condition:** When a request is made to any bank attribute endpoint
- **Check:** Validate that BANK_ID path parameter is provided and is not empty
- **Valid Criteria:** 
  - BANK_ID is present in the URL path
  - BANK_ID is not an empty string
- **Invalid Criteria:**
  - BANK_ID is missing from the URL path
  - BANK_ID is an empty string
- **Action on Success:** Proceed with bank existence validation
- **Action on Failure:** Return error response indicating missing or empty bank identifier

**Error Handling:**

- **Error Message:** `Bank identifier must be provided and non-empty`
- **Error Code:** `N/A (to be defined during implementation)`
- **HTTP Status Code:** `400 Bad Request`

**Related Entities:**
- Bank entity (id field)
- BankAttribute entity
- BankAttributeService

**User Story Context:**
This validation ensures that all bank attribute operations are associated with a valid bank identifier. Without a bank identifier, the system cannot determine which bank's attributes to manage.

**Dependencies:**
- None (standalone validation)

---

### Rule VR-002: Bank Identifier Existence Validation

**Field/Entity:** BANK_ID (path parameter)

**Validation Type:** Entity Existence Validation

**Rule Description:**
Bank identifier must exist in the system before any attribute operations can be performed. This applies to all endpoints (create, retrieve, update, delete).

**Validation Logic:**

- **Condition:** When a request is made to any bank attribute endpoint with a valid (non-empty) BANK_ID
- **Check:** Validate that a bank with the given BANK_ID exists in the database
- **Valid Criteria:** 
  - Bank record with matching BANK_ID exists in the system
- **Invalid Criteria:**
  - No bank record found with the given BANK_ID
- **Action on Success:** Proceed with the requested attribute operation
- **Action on Failure:** Return 404 Not Found error response

**Error Handling:**

- **Error Message:** `Bank not found` or `Bank with identifier [BANK_ID] does not exist`
- **Error Code:** `N/A (to be defined during implementation)`
- **HTTP Status Code:** `404 Not Found`

**Related Entities:**
- Bank entity
- BankService (validates bank existence)

**User Story Context:**
This validation ensures that attributes can only be managed for banks that exist in the system, maintaining referential integrity.

**Dependencies:**
- VR-001: Bank Identifier Required Validation (must pass first)

---

### Rule VR-003: Attribute Name Required Validation

**Field/Entity:** name (request body field)

**Validation Type:** Required Field Validation

**Rule Description:**
Attribute name must be provided and non-empty when creating or updating a bank attribute.

**Validation Logic:**

- **Condition:** When a request is made to POST /banks/BANK_ID/attribute or PUT /banks/BANK_ID/attributes/BANK_ATTRIBUTE_ID
- **Check:** Validate that name field is provided in request body and is not empty
- **Valid Criteria:** 
  - name field is present in request body
  - name is not an empty string
  - name is not null
- **Invalid Criteria:**
  - name field is missing from request body
  - name is an empty string
  - name is null
- **Action on Success:** Proceed with type validation
- **Action on Failure:** Return error response indicating missing or empty attribute name

**Error Handling:**

- **Error Message:** `Attribute name must be provided and non-empty`
- **Error Code:** `N/A (to be defined during implementation)`
- **HTTP Status Code:** `400 Bad Request`

**Related Entities:**
- BankAttribute entity (name field)

**User Story Context:**
This validation ensures that every bank attribute has a meaningful name for identification and retrieval purposes.

**Dependencies:**
- VR-002: Bank Identifier Existence Validation

---

### Rule VR-004: Attribute Type Required Validation

**Field/Entity:** type (request body field)

**Validation Type:** Required Field Validation

**Rule Description:**
Attribute type must be provided when creating or updating a bank attribute. The type must be one of the supported values.

**Validation Logic:**

- **Condition:** When a request is made to POST /banks/BANK_ID/attribute or PUT /banks/BANK_ID/attributes/BANK_ATTRIBUTE_ID
- **Check:** Validate that type field is provided in request body
- **Valid Criteria:** 
  - type field is present in request body
  - type is one of: STRING, INTEGER, DOUBLE, DATE_WITH_DAY
- **Invalid Criteria:**
  - type field is missing from request body
  - type is null or empty
  - type is not one of the supported values
- **Action on Success:** Proceed with value validation
- **Action on Failure:** Return error response indicating invalid attribute type

**Error Handling:**

- **Error Message:** `Attribute type must be one of: STRING, INTEGER, DOUBLE, DATE_WITH_DAY`
- **Error Code:** `N/A (to be defined during implementation)`
- **HTTP Status Code:** `400 Bad Request`

**Related Entities:**
- BankAttribute entity (type field)

**User Story Context:**
This validation ensures that attribute types are restricted to supported values, enabling proper type-based validation of attribute values.

**Dependencies:**
- VR-003: Attribute Name Required Validation

---

### Rule VR-005: Attribute ID Required Validation

**Field/Entity:** BANK_ATTRIBUTE_ID (path parameter)

**Validation Type:** Required Field Validation

**Rule Description:**
Bank attribute identifier must be provided and non-empty for update, retrieve single, and delete operations.

**Validation Logic:**

- **Condition:** When a request is made to PUT, GET (single), or DELETE /banks/BANK_ID/attributes/BANK_ATTRIBUTE_ID
- **Check:** Validate that BANK_ATTRIBUTE_ID path parameter is provided and is not empty
- **Valid Criteria:** 
  - BANK_ATTRIBUTE_ID is present in the URL path
  - BANK_ATTRIBUTE_ID is not an empty string
- **Invalid Criteria:**
  - BANK_ATTRIBUTE_ID is missing from the URL path
  - BANK_ATTRIBUTE_ID is an empty string
- **Action on Success:** Proceed with attribute existence validation
- **Action on Failure:** Return error response indicating missing or empty attribute identifier

**Error Handling:**

- **Error Message:** `Bank attribute identifier must be provided and non-empty`
- **Error Code:** `N/A (to be defined during implementation)`
- **HTTP Status Code:** `400 Bad Request`

**Related Entities:**
- BankAttribute entity (bank_attribute_id field)

**User Story Context:**
This validation ensures that operations targeting specific attributes have a valid identifier to locate the attribute.

**Dependencies:**
- VR-002: Bank Identifier Existence Validation

---

### Rule VR-006: Attribute ID Existence Validation

**Field/Entity:** BANK_ATTRIBUTE_ID (path parameter)

**Validation Type:** Entity Existence Validation

**Rule Description:**
Bank attribute identifier must exist in the system for update, retrieve single, and delete operations. If the attribute does not exist, return a 404 Not Found error.

**Validation Logic:**

- **Condition:** When a request is made to PUT, GET (single), or DELETE /banks/BANK_ID/attributes/BANK_ATTRIBUTE_ID
- **Check:** Validate that a bank attribute with the given BANK_ATTRIBUTE_ID exists for the specified bank
- **Valid Criteria:** 
  - Bank attribute record with matching BANK_ATTRIBUTE_ID exists for the bank
- **Invalid Criteria:**
  - No bank attribute record found with the given BANK_ATTRIBUTE_ID
  - Attribute exists but belongs to a different bank
- **Action on Success:** Proceed with the requested operation (update, retrieve, or delete)
- **Action on Failure:** Return 404 Not Found error response

**Error Handling:**

- **Error Message:** `Bank attribute not found` or `Bank attribute with identifier [BANK_ATTRIBUTE_ID] does not exist`
- **Error Code:** `N/A (to be defined during implementation)`
- **HTTP Status Code:** `404 Not Found`

**Related Entities:**
- BankAttribute entity
- BankAttributeService
- BankAttributeRepository

**User Story Context:**
This validation ensures that update, retrieve, and delete operations can only be performed on existing attributes, preventing operations on non-existent resources.

**Dependencies:**
- VR-005: Attribute ID Required Validation

---

## Category: Format Validation

### Rule VR-007: STRING Type Value Validation

**Field/Entity:** value (request body field) when type is STRING

**Validation Type:** Type-Value Consistency Validation

**Rule Description:**
When attribute type is STRING, the value can be any text value. No specific format restrictions apply.

**Validation Logic:**

- **Condition:** When creating or updating an attribute with type = "STRING"
- **Check:** Validate that value is a valid string
- **Valid Criteria:** 
  - Any text value is acceptable
  - Empty string may be allowed (implementation decision)
- **Invalid Criteria:**
  - Value is not a string type (if strongly typed)
- **Action on Success:** Accept the value as-is
- **Action on Failure:** Return error response indicating invalid value for STRING type

**Error Handling:**

- **Error Message:** `Invalid value for STRING type`
- **Error Code:** `N/A (to be defined during implementation)`
- **HTTP Status Code:** `400 Bad Request`

**Related Entities:**
- BankAttribute entity (type, value fields)

**User Story Context:**
This validation ensures that STRING type attributes accept text values, providing flexibility for free-form attribute data.

**Dependencies:**
- VR-004: Attribute Type Required Validation

---

### Rule VR-008: INTEGER Type Value Validation

**Field/Entity:** value (request body field) when type is INTEGER

**Validation Type:** Type-Value Consistency Validation

**Rule Description:**
When attribute type is INTEGER, the value must be a whole number (e.g., 123). Decimal values are not allowed.

**Validation Logic:**

- **Condition:** When creating or updating an attribute with type = "INTEGER"
- **Check:** Validate that value represents a valid whole number
- **Valid Criteria:** 
  - Value is a whole number (e.g., "123", "-456", "0")
  - Value can be parsed as an integer
- **Invalid Criteria:**
  - Value contains decimal point (e.g., "12.34")
  - Value contains non-numeric characters (e.g., "12abc")
  - Value is empty or null
- **Action on Success:** Accept the value
- **Action on Failure:** Return error response indicating invalid value for INTEGER type

**Error Handling:**

- **Error Message:** `Invalid value for INTEGER type. Expected a whole number (e.g., 123)`
- **Error Code:** `N/A (to be defined during implementation)`
- **HTTP Status Code:** `400 Bad Request`

**Related Entities:**
- BankAttribute entity (type, value fields)

**User Story Context:**
This validation ensures data integrity by enforcing that INTEGER type attributes contain only valid whole numbers.

**Dependencies:**
- VR-004: Attribute Type Required Validation

---

### Rule VR-009: DOUBLE Type Value Validation

**Field/Entity:** value (request body field) when type is DOUBLE

**Validation Type:** Type-Value Consistency Validation

**Rule Description:**
When attribute type is DOUBLE, the value must be a decimal number (e.g., 12.1234). Whole numbers are also acceptable.

**Validation Logic:**

- **Condition:** When creating or updating an attribute with type = "DOUBLE"
- **Check:** Validate that value represents a valid decimal number
- **Valid Criteria:** 
  - Value is a decimal number (e.g., "12.1234", "0.5", "-3.14")
  - Value is a whole number (e.g., "123" - implicitly 123.0)
  - Value can be parsed as a double/float
- **Invalid Criteria:**
  - Value contains non-numeric characters (except decimal point and minus sign)
  - Value has multiple decimal points
  - Value is empty or null
- **Action on Success:** Accept the value
- **Action on Failure:** Return error response indicating invalid value for DOUBLE type

**Error Handling:**

- **Error Message:** `Invalid value for DOUBLE type. Expected a decimal number (e.g., 12.1234)`
- **Error Code:** `N/A (to be defined during implementation)`
- **HTTP Status Code:** `400 Bad Request`

**Related Entities:**
- BankAttribute entity (type, value fields)

**User Story Context:**
This validation ensures data integrity by enforcing that DOUBLE type attributes contain valid decimal numbers for precise numeric data.

**Dependencies:**
- VR-004: Attribute Type Required Validation

---

### Rule VR-010: DATE_WITH_DAY Type Value Validation

**Field/Entity:** value (request body field) when type is DATE_WITH_DAY

**Validation Type:** Type-Value Consistency Validation

**Rule Description:**
When attribute type is DATE_WITH_DAY, the value must be in date format (e.g., 2012-04-23). The format should be YYYY-MM-DD.

**Validation Logic:**

- **Condition:** When creating or updating an attribute with type = "DATE_WITH_DAY"
- **Check:** Validate that value represents a valid date in YYYY-MM-DD format
- **Valid Criteria:** 
  - Value matches format YYYY-MM-DD (e.g., "2012-04-23")
  - Year, month, and day are valid calendar values
  - Month is between 01-12
  - Day is valid for the given month (accounting for leap years)
- **Invalid Criteria:**
  - Value does not match YYYY-MM-DD format
  - Invalid date components (e.g., month 13, day 32)
  - Value is empty or null
  - Value uses different separators (e.g., "2012/04/23")
- **Action on Success:** Accept the value
- **Action on Failure:** Return error response indicating invalid value for DATE_WITH_DAY type

**Error Handling:**

- **Error Message:** `Invalid value for DATE_WITH_DAY type. Expected date format YYYY-MM-DD (e.g., 2012-04-23)`
- **Error Code:** `N/A (to be defined during implementation)`
- **HTTP Status Code:** `400 Bad Request`

**Related Entities:**
- BankAttribute entity (type, value fields)

**User Story Context:**
This validation ensures data integrity by enforcing that DATE_WITH_DAY type attributes contain valid dates in a consistent format.

**Dependencies:**
- VR-004: Attribute Type Required Validation

---

## Category: Business Constraint Validation

### Rule VR-011: Unique Attribute Name Within Bank

**Field/Entity:** name (request body field)

**Validation Type:** Business Constraint Validation

**Rule Description:**
Attribute names should be unique within a bank's attribute set. A bank should not have multiple attributes with the same name.

**Validation Logic:**

- **Condition:** When creating a new attribute (POST) or updating an existing attribute (PUT)
- **Check:** Validate that no other attribute with the same name exists for the bank
- **Valid Criteria:** 
  - No existing attribute with the same name for this bank
  - For updates: the attribute being updated can keep its own name
- **Invalid Criteria:**
  - Another attribute with the same name already exists for this bank
- **Action on Success:** Proceed with create/update operation
- **Action on Failure:** Return error response indicating duplicate attribute name

**Error Handling:**

- **Error Message:** `Attribute with name [name] already exists for this bank`
- **Error Code:** `N/A (to be defined during implementation)`
- **HTTP Status Code:** `400 Bad Request` or `409 Conflict`

**Related Entities:**
- BankAttribute entity (name field)
- Bank entity

**User Story Context:**
This validation ensures that attribute names are unique within a bank, preventing confusion and enabling reliable attribute lookup by name.

**Dependencies:**
- VR-003: Attribute Name Required Validation
- VR-002: Bank Identifier Existence Validation

---

### Rule VR-012: Existing Attribute for Updates

**Field/Entity:** BANK_ATTRIBUTE_ID (path parameter)

**Validation Type:** Business Rule Validation

**Rule Description:**
Bank attribute must exist before it can be managed/updated. This is a business constraint ensuring that update operations target existing resources.

**Validation Logic:**

- **Condition:** When a PUT request is made to /banks/BANK_ID/attributes/BANK_ATTRIBUTE_ID
- **Check:** Verify that the bank attribute exists and belongs to the specified bank
- **Valid Criteria:** 
  - Attribute with BANK_ATTRIBUTE_ID exists
  - Attribute belongs to the bank specified by BANK_ID
- **Invalid Criteria:**
  - Attribute does not exist
  - Attribute exists but belongs to a different bank
- **Action on Success:** Proceed with update operation
- **Action on Failure:** Return 404 Not Found error

**Error Handling:**

- **Error Message:** `Bank attribute not found`
- **Error Code:** `N/A (to be defined during implementation)`
- **HTTP Status Code:** `404 Not Found`

**Related Entities:**
- BankAttribute entity
- BankAttributeService

**User Story Context:**
This business rule ensures that only existing attributes can be updated, maintaining data consistency.

**Dependencies:**
- VR-006: Attribute ID Existence Validation

---

### Rule VR-013: Existing Attribute for Deletion

**Field/Entity:** BANK_ATTRIBUTE_ID (path parameter)

**Validation Type:** Business Rule Validation

**Rule Description:**
Bank attribute must exist before it can be deleted. This is a business constraint ensuring that delete operations target existing resources.

**Validation Logic:**

- **Condition:** When a DELETE request is made to /banks/BANK_ID/attributes/BANK_ATTRIBUTE_ID
- **Check:** Verify that the bank attribute exists and belongs to the specified bank
- **Valid Criteria:** 
  - Attribute with BANK_ATTRIBUTE_ID exists
  - Attribute belongs to the bank specified by BANK_ID
- **Invalid Criteria:**
  - Attribute does not exist
  - Attribute exists but belongs to a different bank
- **Action on Success:** Proceed with delete operation, return 204 No Content
- **Action on Failure:** Return 404 Not Found error

**Error Handling:**

- **Error Message:** `Bank attribute not found`
- **Error Code:** `N/A (to be defined during implementation)`
- **HTTP Status Code:** `404 Not Found`

**Related Entities:**
- BankAttribute entity
- BankAttributeService

**User Story Context:**
This business rule ensures that only existing attributes can be deleted, preventing errors when attempting to delete non-existent resources.

**Dependencies:**
- VR-006: Attribute ID Existence Validation

---

### Rule VR-014: Active/Inactive Status Validation

**Field/Entity:** is_active (request body field)

**Validation Type:** Business Constraint Validation

**Rule Description:**
The is_active field must be a boolean value indicating whether the attribute is active or inactive. This supports soft deactivation without deletion.

**Validation Logic:**

- **Condition:** When creating or updating an attribute with is_active field
- **Check:** Validate that is_active is a valid boolean value
- **Valid Criteria:** 
  - is_active is true or false (boolean)
- **Invalid Criteria:**
  - is_active is not a boolean type
  - is_active is a string like "true" or "false" (if strict typing is enforced)
- **Action on Success:** Accept the value and set attribute status accordingly
- **Action on Failure:** Return error response indicating invalid is_active value

**Error Handling:**

- **Error Message:** `is_active must be a boolean value (true or false)`
- **Error Code:** `N/A (to be defined during implementation)`
- **HTTP Status Code:** `400 Bad Request`

**Related Entities:**
- BankAttribute entity (is_active field)

**User Story Context:**
This validation ensures that the active/inactive status is properly set, enabling soft deactivation of attributes without permanent deletion.

**Dependencies:**
- VR-002: Bank Identifier Existence Validation

---

## Category: Response Validation

### Rule VR-015: Empty Attribute List Handling

**Field/Entity:** bank_attributes collection (response)

**Validation Type:** Response Handling Validation

**Rule Description:**
GET /banks/BANK_ID/attributes endpoint should return an empty list if the bank has no attributes, NOT a 404 error. This ensures consistent API behavior.

**Validation Logic:**

- **Condition:** When processing GET /banks/BANK_ID/attributes request and the bank has no attributes
- **Check:** Verify response behavior when attribute collection is empty
- **Valid Criteria:** 
  - Return HTTP 200 OK with empty bank_attributes array: `{"bank_attributes": []}`
- **Invalid Criteria:**
  - Returning 404 Not Found when bank has no attributes
  - Returning null or undefined instead of empty array
- **Action on Success:** Return 200 OK with empty bank_attributes array
- **Action on Failure:** Incorrect error response

**Error Handling:**

- **Error Message:** `N/A (no error for empty list)`
- **Error Code:** `N/A`
- **HTTP Status Code:** `200 OK` (always for GET /banks/BANK_ID/attributes, even if empty)

**Related Entities:**
- BankAttribute collection

**User Story Context:**
This validation ensures that the API returns a consistent response structure even when a bank has no attributes configured, allowing client applications to handle the response uniformly.

**Dependencies:**
- VR-002: Bank Identifier Existence Validation

---

## Validation Rules Summary Table

| Rule ID | Rule Name | Field/Entity | Validation Type | HTTP Status on Failure |
|---------|-----------|--------------|-----------------|------------------------|
| VR-001 | Bank Identifier Required | BANK_ID | Required Field | 400 |
| VR-002 | Bank Identifier Existence | BANK_ID | Entity Existence | 404 |
| VR-003 | Attribute Name Required | name | Required Field | 400 |
| VR-004 | Attribute Type Required | type | Required Field | 400 |
| VR-005 | Attribute ID Required | BANK_ATTRIBUTE_ID | Required Field | 400 |
| VR-006 | Attribute ID Existence | BANK_ATTRIBUTE_ID | Entity Existence | 404 |
| VR-007 | STRING Type Value | value (STRING) | Type-Value Consistency | 400 |
| VR-008 | INTEGER Type Value | value (INTEGER) | Type-Value Consistency | 400 |
| VR-009 | DOUBLE Type Value | value (DOUBLE) | Type-Value Consistency | 400 |
| VR-010 | DATE_WITH_DAY Type Value | value (DATE_WITH_DAY) | Type-Value Consistency | 400 |
| VR-011 | Unique Attribute Name | name | Business Constraint | 400/409 |
| VR-012 | Existing Attribute for Updates | BANK_ATTRIBUTE_ID | Business Rule | 404 |
| VR-013 | Existing Attribute for Deletion | BANK_ATTRIBUTE_ID | Business Rule | 404 |
| VR-014 | Active/Inactive Status | is_active | Business Constraint | 400 |
| VR-015 | Empty Attribute List Handling | bank_attributes | Response Handling | N/A |

---

## Endpoint-Validation Mapping

### POST /banks/BANK_ID/attribute (Create)
- VR-001: Bank Identifier Required
- VR-002: Bank Identifier Existence
- VR-003: Attribute Name Required
- VR-004: Attribute Type Required
- VR-007 to VR-010: Type-Value Consistency (based on type)
- VR-011: Unique Attribute Name
- VR-014: Active/Inactive Status

### PUT /banks/BANK_ID/attributes/BANK_ATTRIBUTE_ID (Update)
- VR-001: Bank Identifier Required
- VR-002: Bank Identifier Existence
- VR-005: Attribute ID Required
- VR-006: Attribute ID Existence
- VR-003: Attribute Name Required
- VR-004: Attribute Type Required
- VR-007 to VR-010: Type-Value Consistency (based on type)
- VR-011: Unique Attribute Name
- VR-012: Existing Attribute for Updates
- VR-014: Active/Inactive Status

### GET /banks/BANK_ID/attributes (Retrieve All)
- VR-001: Bank Identifier Required
- VR-002: Bank Identifier Existence
- VR-015: Empty Attribute List Handling

### GET /banks/BANK_ID/attributes/BANK_ATTRIBUTE_ID (Retrieve Single)
- VR-001: Bank Identifier Required
- VR-002: Bank Identifier Existence
- VR-005: Attribute ID Required
- VR-006: Attribute ID Existence

### DELETE /banks/BANK_ID/attributes/BANK_ATTRIBUTE_ID (Delete)
- VR-001: Bank Identifier Required
- VR-002: Bank Identifier Existence
- VR-005: Attribute ID Required
- VR-006: Attribute ID Existence
- VR-013: Existing Attribute for Deletion

---

## Notes

1. **No Scala Code References**: Since this extraction is based solely on the user story document content, no actual Scala code snippets or file references are included. During implementation, these validation rules should be mapped to appropriate service and repository methods.

2. **Error Codes**: Specific error codes (e.g., OBP-XXXXX format) should be defined during implementation based on the application's error code conventions.

3. **Authentication & Authorization**: The user story mentions dependencies on authentication service and authorization service with specific entitlements (canCreateBankAttribute, canGetBankAttribute, canUpdateBankAttribute, canDeleteBankAttribute). Authentication and authorization validation rules should be extracted from relevant security user stories.

4. **Attribute Name Restrictions**: The user story raises a question about attribute name restrictions (reserved keywords, naming conventions). Additional validation rules may be needed once SME clarifies these requirements.

5. **Attribute Count Limit**: The user story raises a question about limiting the number of attributes per bank. If implemented, an additional validation rule would be needed.

6. **DATE_WITH_DAY Format**: The exact validation rules for DATE_WITH_DAY format should be confirmed with SME. The assumed format is YYYY-MM-DD based on the example provided (2012-04-23).

7. **Soft Deletion**: The user story mentions using is_active=false for soft deletion as an alternative to permanent deletion via DELETE endpoint.
