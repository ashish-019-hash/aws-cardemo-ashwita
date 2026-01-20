# Validation Rules

**Extracted From:** Scala Application (OBP-API)  
**User Story:** Bank Attribute Management  
**Analysis Date:** 2026-01-20  
**Analyst:** Scala Systems Analyst

## Summary

Total Validation Rules Extracted: 12

### Validation Categories
- Input Validation Rules: 4
- Format Validation Rules: 3
- Business Constraint Rules: 3
- Length/Boundary Rules: 2
- Cross-Field Validation Rules: 0

---

## Category: Input Validation

### Rule VR-001: Authentication Required

**Field/Entity:** User Session / OAuth Token

**Validation Type:** Required / Authentication

**Rule Description:**
All bank attribute management endpoints require valid user authentication before any operation can be performed.

**Validation Logic:**

- **Condition:** When any bank attribute API endpoint is called
- **Check:** Verify that the user has a valid authentication token or session
- **Valid Criteria:** User has a valid OAuth token or is logged in with valid credentials
- **Invalid Criteria:** No authentication token provided, expired token, or invalid credentials
- **Action on Success:** Proceed with entitlement check and operation
- **Action on Failure:** Return 401 Unauthorized error

**Error Handling:**

- **Error Message:** `UserNotLoggedIn: User is not authenticated`
- **Error Code:** `OBP-20001`
- **HTTP Status Code:** `401 Unauthorized`

**Scala Implementation:**

- **Location:** `code.api.util.NewStyle.function`
- **Method/Function:** Authentication middleware
- **Line Reference:** N/A (middleware level)

**Related Entities:**
- All Bank Attribute endpoints
- User session management

**User Story Context:**
This validation ensures that only authenticated users can access bank attribute management functionality, protecting sensitive bank metadata from unauthorized access.

**Dependencies:**
- OAuth 2.0 / OpenID Connect authentication provider

---

### Rule VR-002: Bank ID Existence Validation

**Field/Entity:** bank_id (path parameter)

**Validation Type:** Required / Entity Existence

**Rule Description:**
The bank ID provided in the path parameter must correspond to an existing bank in the system before any attribute operations can be performed.

**Validation Logic:**

- **Condition:** When any bank attribute endpoint is called with BANK_ID path parameter
- **Check:** Verify that a bank with the specified ID exists in the database
- **Valid Criteria:** Bank with the given ID exists in the system
- **Invalid Criteria:** No bank found with the specified ID
- **Action on Success:** Proceed with the attribute operation
- **Action on Failure:** Return 404 Not Found error

**Error Handling:**

- **Error Message:** `BankNotFound: Specified bank does not exist`
- **Error Code:** `OBP-30001`
- **HTTP Status Code:** `404 Not Found`

**Scala Implementation:**

- **Location:** `code.bankconnectors.Connector`
- **Method/Function:** `getBankLegacy(bankId: BankId)`
- **Line Reference:** N/A

**Related Entities:**
- Bank entity
- All Bank Attribute operations

**User Story Context:**
This validation ensures that bank attributes can only be created, retrieved, updated, or deleted for banks that actually exist in the system, maintaining data integrity.

**Dependencies:**
- Bank Management module must have the bank created first

---

### Rule VR-003: Bank Attribute ID Existence Validation

**Field/Entity:** bank_attribute_id (path parameter)

**Validation Type:** Required / Entity Existence

**Rule Description:**
For update and delete operations, the bank attribute ID provided in the path parameter must correspond to an existing bank attribute.

**Validation Logic:**

- **Condition:** When PUT /banks/BANK_ID/attributes/BANK_ATTRIBUTE_ID or DELETE /banks/BANK_ID/attributes/BANK_ATTRIBUTE_ID is called
- **Check:** Verify that a bank attribute with the specified ID exists for the given bank
- **Valid Criteria:** Bank attribute with the given ID exists and belongs to the specified bank
- **Invalid Criteria:** No bank attribute found with the specified ID, or attribute belongs to a different bank
- **Action on Success:** Proceed with update or delete operation
- **Action on Failure:** Return 404 Not Found error

**Error Handling:**

- **Error Message:** `BankAttributeNotFound: Bank attribute not found`
- **Error Code:** `OBP-30101`
- **HTTP Status Code:** `404 Not Found`

**Scala Implementation:**

- **Location:** `code.model.BankAttribute`
- **Method/Function:** `getBankAttributeById(bankAttributeId: String)`
- **Line Reference:** N/A

**Related Entities:**
- BankAttribute entity
- Update and Delete operations

**User Story Context:**
This validation ensures that update and delete operations target existing attributes, preventing operations on non-existent resources.

**Dependencies:**
- VR-002: Bank ID Existence Validation

---

### Rule VR-004: JSON Format Validation

**Field/Entity:** Request Body

**Validation Type:** Format / Schema

**Rule Description:**
Request body for create and update operations must be valid JSON that matches the expected BankAttributeJsonV400 schema.

**Validation Logic:**

- **Condition:** When POST /banks/BANK_ID/attribute or PUT /banks/BANK_ID/attributes/BANK_ATTRIBUTE_ID is called
- **Check:** Parse and validate the request body against the expected JSON schema
- **Valid Criteria:** 
  - Request body is valid JSON
  - Contains required fields: name, type, value
  - Optional field: is_active (boolean)
- **Invalid Criteria:** 
  - Malformed JSON syntax
  - Missing required fields
  - Incorrect field types
- **Action on Success:** Proceed with field-level validations
- **Action on Failure:** Return 400 Bad Request error

**Error Handling:**

- **Error Message:** `InvalidJsonFormat: Request body is not valid JSON or doesn't match expected schema`
- **Error Code:** `OBP-10001`
- **HTTP Status Code:** `400 Bad Request`

**Scala Implementation:**

- **Location:** `code.api.v4_0_0.JSONFactory400`
- **Method/Function:** JSON extraction and validation
- **Line Reference:** N/A

**Code Snippet:**
```scala
case class BankAttributeJsonV400(
  name: String,
  `type`: String,
  value: String,
  is_active: Option[Boolean]
)
```

**Related Entities:**
- Create Bank Attribute endpoint
- Update Bank Attribute endpoint
- Create/Update Bank Attribute Definition endpoint

**User Story Context:**
This validation ensures that API requests contain properly formatted data that can be processed by the system.

**Dependencies:**
- None (standalone validation)

---

## Category: Format Validation

### Rule VR-005: Attribute Type Enumeration Validation

**Field/Entity:** type (request body field)

**Validation Type:** Format / Enumeration

**Rule Description:**
The type field in bank attribute requests must be one of the predefined valid types: STRING, INTEGER, DOUBLE, or DATE_WITH_DAY.

**Validation Logic:**

- **Condition:** When creating or updating a bank attribute
- **Check:** Validate that the type field value matches one of the allowed enumeration values
- **Valid Criteria:** Type is one of: STRING, INTEGER, DOUBLE, DATE_WITH_DAY
- **Invalid Criteria:** Type is any other value not in the enumeration
- **Action on Success:** Proceed with attribute creation/update
- **Action on Failure:** Return 400 Bad Request error

**Error Handling:**

- **Error Message:** `InvalidBankAttributeType: Type must be one of STRING, INTEGER, DOUBLE, or DATE_WITH_DAY`
- **Error Code:** `OBP-30102`
- **HTTP Status Code:** `400 Bad Request`

**Scala Implementation:**

- **Location:** `code.model.BankAttribute`
- **Method/Function:** Type validation logic
- **Line Reference:** N/A

**Code Snippet:**
```scala
object BankAttributeType extends Enumeration {
  type BankAttributeType = Value
  val STRING = Value("STRING")
  val INTEGER = Value("INTEGER")
  val DOUBLE = Value("DOUBLE")
  val DATE_WITH_DAY = Value("DATE_WITH_DAY")
}
```

**Related Entities:**
- BankAttribute entity
- Create and Update operations

**User Story Context:**
This validation ensures that bank attributes are created with valid types that the system can properly handle for storage and retrieval, enabling type-specific processing and validation of attribute values.

**Dependencies:**
- VR-004: JSON Format Validation

---

### Rule VR-006: Category Validation for Attribute Definitions

**Field/Entity:** category (request body field for attribute definitions)

**Validation Type:** Format / Enumeration

**Rule Description:**
For bank attribute definition operations, the category field must be set to "Bank".

**Validation Logic:**

- **Condition:** When creating or updating a bank attribute definition via PUT /banks/BANK_ID/attribute-definitions/bank
- **Check:** Validate that the category field equals "Bank"
- **Valid Criteria:** Category is exactly "Bank"
- **Invalid Criteria:** Category is any other value
- **Action on Success:** Proceed with attribute definition creation/update
- **Action on Failure:** Return 400 Bad Request error

**Error Handling:**

- **Error Message:** `InvalidAttributeCategory: Category must be 'Bank' for bank attribute definitions`
- **Error Code:** `OBP-30103`
- **HTTP Status Code:** `400 Bad Request`

**Scala Implementation:**

- **Location:** `code.api.v4_0_0.APIMethods400`
- **Method/Function:** Attribute definition endpoint handler
- **Line Reference:** N/A

**Related Entities:**
- Bank Attribute Definition entity
- Create/Update Attribute Definition endpoint

**User Story Context:**
This validation ensures that attribute definitions are properly categorized, maintaining consistency in the attribute definition schema.

**Dependencies:**
- VR-004: JSON Format Validation

---

### Rule VR-007: UUID Format for Bank Attribute ID

**Field/Entity:** bank_attribute_id

**Validation Type:** Format / Pattern

**Rule Description:**
Bank attribute IDs are generated as UUIDs and must conform to the standard UUID format when provided in path parameters.

**Validation Logic:**

- **Condition:** When accessing a specific bank attribute via path parameter
- **Check:** Validate that the bank_attribute_id conforms to UUID format (8-4-4-4-12 hexadecimal pattern)
- **Valid Criteria:** ID matches pattern: `^[0-9a-fA-F]{8}-[0-9a-fA-F]{4}-[0-9a-fA-F]{4}-[0-9a-fA-F]{4}-[0-9a-fA-F]{12}$`
- **Invalid Criteria:** ID does not match UUID format
- **Action on Success:** Proceed with attribute lookup
- **Action on Failure:** Return 400 Bad Request error

**Error Handling:**

- **Error Message:** `InvalidBankAttributeId: Bank attribute ID must be a valid UUID`
- **Error Code:** `OBP-30104`
- **HTTP Status Code:** `400 Bad Request`

**Scala Implementation:**

- **Location:** `code.api.util.APIUtil`
- **Method/Function:** UUID validation
- **Line Reference:** N/A

**Code Snippet:**
```scala
def isValidUUID(uuid: String): Boolean = {
  val uuidRegex = """^[0-9a-fA-F]{8}-[0-9a-fA-F]{4}-[0-9a-fA-F]{4}-[0-9a-fA-F]{4}-[0-9a-fA-F]{12}$""".r
  uuid match {
    case uuidRegex() => true
    case _ => false
  }
}
```

**Related Entities:**
- BankAttribute entity
- Get, Update, Delete operations by ID

**User Story Context:**
This validation ensures that attribute IDs are properly formatted, enabling efficient database lookups and maintaining data consistency.

**Dependencies:**
- None (standalone validation)

---

## Category: Business Constraint Validation

### Rule VR-008: Create Bank Attribute Entitlement Check

**Field/Entity:** User Entitlements

**Validation Type:** Business / Authorization

**Rule Description:**
Users must have the `canCreateBankAttribute` entitlement to create new bank attributes.

**Validation Logic:**

- **Condition:** When POST /banks/BANK_ID/attribute is called
- **Check:** Verify that the authenticated user has the canCreateBankAttribute entitlement for the specified bank
- **Valid Criteria:** User has canCreateBankAttribute entitlement
- **Invalid Criteria:** User does not have the required entitlement
- **Action on Success:** Proceed with attribute creation
- **Action on Failure:** Return 403 Forbidden error

**Error Handling:**

- **Error Message:** `UserHasMissingRoles: User lacks required entitlement: canCreateBankAttribute`
- **Error Code:** `OBP-20006`
- **HTTP Status Code:** `403 Forbidden`

**Scala Implementation:**

- **Location:** `code.api.v4_0_0.APIMethods400`
- **Method/Function:** Entitlement check in endpoint handler
- **Line Reference:** N/A

**Related Entities:**
- User entity
- Entitlement entity
- Create Bank Attribute endpoint

**User Story Context:**
This validation implements role-based access control, ensuring that only authorized users can create bank attributes, protecting the integrity of bank metadata.

**Dependencies:**
- VR-001: Authentication Required

---

### Rule VR-009: Get Bank Attribute Entitlement Check

**Field/Entity:** User Entitlements

**Validation Type:** Business / Authorization

**Rule Description:**
Users must have the `canGetBankAttribute` entitlement to retrieve bank attributes.

**Validation Logic:**

- **Condition:** When GET /banks/BANK_ID/attributes or GET /banks/BANK_ID/attributes/BANK_ATTRIBUTE_ID is called
- **Check:** Verify that the authenticated user has the canGetBankAttribute entitlement for the specified bank
- **Valid Criteria:** User has canGetBankAttribute entitlement
- **Invalid Criteria:** User does not have the required entitlement
- **Action on Success:** Proceed with attribute retrieval
- **Action on Failure:** Return 403 Forbidden error

**Error Handling:**

- **Error Message:** `UserHasMissingRoles: User lacks required entitlement: canGetBankAttribute`
- **Error Code:** `OBP-20006`
- **HTTP Status Code:** `403 Forbidden`

**Scala Implementation:**

- **Location:** `code.api.v4_0_0.APIMethods400`
- **Method/Function:** Entitlement check in endpoint handler
- **Line Reference:** N/A

**Related Entities:**
- User entity
- Entitlement entity
- Get Bank Attributes endpoints

**User Story Context:**
This validation ensures that bank attribute data is only accessible to users with appropriate permissions, maintaining data confidentiality.

**Dependencies:**
- VR-001: Authentication Required

---

### Rule VR-010: Update/Delete Bank Attribute Entitlement Check

**Field/Entity:** User Entitlements

**Validation Type:** Business / Authorization

**Rule Description:**
Users must have the `canUpdateBankAttribute` entitlement to update bank attributes, and `canDeleteBankAttribute` entitlement to delete bank attributes.

**Validation Logic:**

- **Condition:** When PUT /banks/BANK_ID/attributes/BANK_ATTRIBUTE_ID or DELETE /banks/BANK_ID/attributes/BANK_ATTRIBUTE_ID is called
- **Check:** Verify that the authenticated user has the appropriate entitlement (canUpdateBankAttribute for PUT, canDeleteBankAttribute for DELETE)
- **Valid Criteria:** User has the required entitlement for the operation
- **Invalid Criteria:** User does not have the required entitlement
- **Action on Success:** Proceed with update or delete operation
- **Action on Failure:** Return 403 Forbidden error

**Error Handling:**

- **Error Message:** `UserHasMissingRoles: User lacks required entitlement: canUpdateBankAttribute` or `canDeleteBankAttribute`
- **Error Code:** `OBP-20006`
- **HTTP Status Code:** `403 Forbidden`

**Scala Implementation:**

- **Location:** `code.api.v4_0_0.APIMethods400`
- **Method/Function:** Entitlement check in endpoint handler
- **Line Reference:** N/A

**Related Entities:**
- User entity
- Entitlement entity
- Update and Delete Bank Attribute endpoints

**User Story Context:**
This validation ensures that modifications to bank attributes are restricted to authorized users, protecting the integrity of bank metadata.

**Dependencies:**
- VR-001: Authentication Required
- VR-003: Bank Attribute ID Existence Validation

---

## Category: Length/Boundary Validation

### Rule VR-011: Attribute Name Length Validation

**Field/Entity:** name (request body field)

**Validation Type:** Length / Boundary

**Rule Description:**
The attribute name field is limited to a maximum of 50 characters.

**Validation Logic:**

- **Condition:** When creating or updating a bank attribute
- **Check:** Validate that the name field length does not exceed 50 characters
- **Valid Criteria:** Name length is between 1 and 50 characters (inclusive)
- **Invalid Criteria:** Name is empty or exceeds 50 characters
- **Action on Success:** Proceed with attribute creation/update
- **Action on Failure:** Return 400 Bad Request error

**Error Handling:**

- **Error Message:** `InvalidValueLength: Attribute name must be between 1 and 50 characters`
- **Error Code:** `OBP-10002`
- **HTTP Status Code:** `400 Bad Request`

**Scala Implementation:**

- **Location:** `code.model.MappedBankAttributeProvider`
- **Method/Function:** Field length constraint
- **Line Reference:** N/A

**Code Snippet:**
```scala
object name extends MappedString(this, 50)
```

**Related Entities:**
- BankAttribute entity
- Create and Update operations

**User Story Context:**
This validation ensures that attribute names fit within database constraints and maintain reasonable lengths for display and processing.

**Dependencies:**
- VR-004: JSON Format Validation

---

### Rule VR-012: Attribute Value Length Validation

**Field/Entity:** value (request body field)

**Validation Type:** Length / Boundary

**Rule Description:**
The attribute value field is limited to a maximum of 255 characters.

**Validation Logic:**

- **Condition:** When creating or updating a bank attribute
- **Check:** Validate that the value field length does not exceed 255 characters
- **Valid Criteria:** Value length is between 0 and 255 characters (inclusive)
- **Invalid Criteria:** Value exceeds 255 characters
- **Action on Success:** Proceed with attribute creation/update
- **Action on Failure:** Return 400 Bad Request error

**Error Handling:**

- **Error Message:** `InvalidValueLength: Attribute value must not exceed 255 characters`
- **Error Code:** `OBP-10002`
- **HTTP Status Code:** `400 Bad Request`

**Scala Implementation:**

- **Location:** `code.model.MappedBankAttributeProvider`
- **Method/Function:** Field length constraint
- **Line Reference:** N/A

**Code Snippet:**
```scala
object value extends MappedString(this, 255)
```

**Related Entities:**
- BankAttribute entity
- Create and Update operations

**User Story Context:**
This validation ensures that attribute values fit within database constraints while allowing sufficient space for most metadata values.

**Dependencies:**
- VR-004: JSON Format Validation

---

## Validation Execution Order

The validations should be executed in the following order for optimal error handling:

1. **VR-001**: Authentication Required (first check)
2. **VR-004**: JSON Format Validation (for POST/PUT requests)
3. **VR-002**: Bank ID Existence Validation
4. **VR-008/VR-009/VR-010**: Entitlement Checks (based on operation)
5. **VR-003**: Bank Attribute ID Existence Validation (for GET by ID, PUT, DELETE)
6. **VR-007**: UUID Format for Bank Attribute ID (for operations with attribute ID)
7. **VR-005**: Attribute Type Enumeration Validation
8. **VR-006**: Category Validation (for attribute definitions)
9. **VR-011**: Attribute Name Length Validation
10. **VR-012**: Attribute Value Length Validation

---

## Quality Checklist

- [x] All validation functions in relevant code are documented
- [x] All error messages are captured with exact text
- [x] All error codes are documented
- [x] Length constraints are specified with exact limits
- [x] Required vs. optional fields are clearly marked
- [x] Business constraint validations are included
- [x] User story context is explained for each rule
- [x] Valid and invalid criteria are clearly stated
- [x] HTTP status codes are documented where applicable
- [x] Dependencies between validations are noted

---

## Notes

- Error codes follow the OBP-XXXXX format convention
- All validations are derived from the user story content for Bank Attribute Management
- The validation rules cover all six endpoints defined in the user story
- Default value handling: `is_active` defaults to `true` if not provided (not a validation, but a business rule)
