# Validation Rules

**Extracted From:** Scala Application (OBP-API)  
**User Story:** Bank Attribute Management  
**Analysis Date:** 2026-01-20  
**Analyst:** Scala Systems Analyst

## Summary

Total Validation Rules Extracted: 12

### Validation Categories
- Input Validation Rules: 4
- Format Validation Rules: 2
- Business Constraint Rules: 3
- Length/Boundary Rules: 2
- Cross-Field Validation Rules: 1

---

## Category: Authentication and Authorization Validation

### Rule VR-001: User Authentication Validation

**Field/Entity:** User Session / OAuth Token

**Validation Type:** Required / Authentication

**Rule Description:**
All bank attribute operations require valid user authentication. The system must verify that the user is logged in before processing any request.

**Validation Logic:**

- **Condition:** When any bank attribute endpoint is accessed
- **Check:** Verify that a valid authentication token or session exists
- **Valid Criteria:** User has a valid OAuth token or active session
- **Invalid Criteria:** No authentication token provided or token is expired/invalid
- **Action on Success:** Proceed with entitlement check
- **Action on Failure:** Return authentication error response

**Error Handling:**

- **Error Message:** `UserNotLoggedIn: User is not authenticated`
- **Error Code:** `OBP-20001`
- **HTTP Status Code:** `401 Unauthorized`

**Scala Implementation:**

- **Location:** `code.api.util.APIUtil`
- **Method/Function:** `getUserFromToken` / `isLoggedIn`

**Related Entities:**
- User
- OAuth Token
- Session

**User Story Context:**
This validation ensures that only authenticated users can access bank attribute management functionality, as stated in acceptance criteria: "All operations require valid authentication and appropriate role-based entitlements."

**Dependencies:**
- OAuth 2.0 / OpenID Connect authentication provider

---

### Rule VR-002: Entitlement Validation for Create Operation

**Field/Entity:** User Entitlements

**Validation Type:** Authorization / Business Constraint

**Rule Description:**
Users must have the `canCreateBankAttribute` entitlement to create new bank attributes.

**Validation Logic:**

- **Condition:** When POST /banks/BANK_ID/attribute endpoint is accessed
- **Check:** Verify user has `canCreateBankAttribute` entitlement
- **Valid Criteria:** User has `canCreateBankAttribute` role assigned
- **Invalid Criteria:** User does not have the required entitlement
- **Action on Success:** Proceed with bank attribute creation
- **Action on Failure:** Return authorization error response

**Error Handling:**

- **Error Message:** `UserHasMissingRoles: User lacks required entitlement for the operation`
- **Error Code:** `OBP-20006`
- **HTTP Status Code:** `403 Forbidden`

**Scala Implementation:**

- **Location:** `code.api.v4_0_0.APIMethods400`
- **Method/Function:** `hasEntitlement`

**Related Entities:**
- User
- Entitlement
- BankAttribute

**User Story Context:**
This validation implements acceptance criteria #1: "Authenticated users with `canCreateBankAttribute` entitlement can create new bank attributes."

**Dependencies:**
- VR-001 (User Authentication)

---

### Rule VR-003: Entitlement Validation for Read Operation

**Field/Entity:** User Entitlements

**Validation Type:** Authorization / Business Constraint

**Rule Description:**
Users must have the `canGetBankAttribute` entitlement to retrieve bank attributes.

**Validation Logic:**

- **Condition:** When GET /banks/BANK_ID/attributes or GET /banks/BANK_ID/attributes/BANK_ATTRIBUTE_ID endpoint is accessed
- **Check:** Verify user has `canGetBankAttribute` entitlement
- **Valid Criteria:** User has `canGetBankAttribute` role assigned
- **Invalid Criteria:** User does not have the required entitlement
- **Action on Success:** Proceed with bank attribute retrieval
- **Action on Failure:** Return authorization error response

**Error Handling:**

- **Error Message:** `UserHasMissingRoles: User lacks required entitlement for the operation`
- **Error Code:** `OBP-20006`
- **HTTP Status Code:** `403 Forbidden`

**Scala Implementation:**

- **Location:** `code.api.v4_0_0.APIMethods400`
- **Method/Function:** `hasEntitlement`

**Related Entities:**
- User
- Entitlement
- BankAttribute

**User Story Context:**
This validation implements acceptance criteria #2 and #3: "Authenticated users with `canGetBankAttribute` entitlement can retrieve all attributes for a specific bank" and "retrieve a specific bank attribute by its ID."

**Dependencies:**
- VR-001 (User Authentication)

---

### Rule VR-004: Entitlement Validation for Update Operation

**Field/Entity:** User Entitlements

**Validation Type:** Authorization / Business Constraint

**Rule Description:**
Users must have the `canUpdateBankAttribute` entitlement to update existing bank attributes.

**Validation Logic:**

- **Condition:** When PUT /banks/BANK_ID/attributes/BANK_ATTRIBUTE_ID endpoint is accessed
- **Check:** Verify user has `canUpdateBankAttribute` entitlement
- **Valid Criteria:** User has `canUpdateBankAttribute` role assigned
- **Invalid Criteria:** User does not have the required entitlement
- **Action on Success:** Proceed with bank attribute update
- **Action on Failure:** Return authorization error response

**Error Handling:**

- **Error Message:** `UserHasMissingRoles: User lacks required entitlement for the operation`
- **Error Code:** `OBP-20006`
- **HTTP Status Code:** `403 Forbidden`

**Scala Implementation:**

- **Location:** `code.api.v4_0_0.APIMethods400`
- **Method/Function:** `hasEntitlement`

**Related Entities:**
- User
- Entitlement
- BankAttribute

**User Story Context:**
This validation implements acceptance criteria #4: "Authenticated users with `canUpdateBankAttribute` entitlement can update existing bank attributes."

**Dependencies:**
- VR-001 (User Authentication)

---

### Rule VR-005: Entitlement Validation for Delete Operation

**Field/Entity:** User Entitlements

**Validation Type:** Authorization / Business Constraint

**Rule Description:**
Users must have the `canDeleteBankAttribute` entitlement to delete bank attributes.

**Validation Logic:**

- **Condition:** When DELETE /banks/BANK_ID/attributes/BANK_ATTRIBUTE_ID endpoint is accessed
- **Check:** Verify user has `canDeleteBankAttribute` entitlement
- **Valid Criteria:** User has `canDeleteBankAttribute` role assigned
- **Invalid Criteria:** User does not have the required entitlement
- **Action on Success:** Proceed with bank attribute deletion
- **Action on Failure:** Return authorization error response

**Error Handling:**

- **Error Message:** `UserHasMissingRoles: User lacks required entitlement for the operation`
- **Error Code:** `OBP-20006`
- **HTTP Status Code:** `403 Forbidden`

**Scala Implementation:**

- **Location:** `code.api.v4_0_0.APIMethods400`
- **Method/Function:** `hasEntitlement`

**Related Entities:**
- User
- Entitlement
- BankAttribute

**User Story Context:**
This validation implements acceptance criteria #5: "Authenticated users with `canDeleteBankAttribute` entitlement can delete bank attributes."

**Dependencies:**
- VR-001 (User Authentication)

---

## Category: Entity Existence Validation

### Rule VR-006: Bank Existence Validation

**Field/Entity:** bank_id (Path Parameter)

**Validation Type:** Entity Existence / Business Constraint

**Rule Description:**
The bank specified by BANK_ID must exist in the system before any attribute operations can be performed.

**Validation Logic:**

- **Condition:** When any bank attribute endpoint is accessed with a BANK_ID path parameter
- **Check:** Verify that a bank with the specified ID exists in the database
- **Valid Criteria:** Bank with the given ID exists in the system
- **Invalid Criteria:** No bank found with the specified ID
- **Action on Success:** Proceed with the requested operation
- **Action on Failure:** Return bank not found error response

**Error Handling:**

- **Error Message:** `BankNotFound: Specified bank does not exist`
- **Error Code:** `OBP-30001`
- **HTTP Status Code:** `404 Not Found`

**Scala Implementation:**

- **Location:** `code.bankconnectors.Connector`
- **Method/Function:** `getBankLegacy` / `getBank`

**Related Entities:**
- Bank
- BankAttribute

**User Story Context:**
This validation implements business rule #1: "A bank must exist in the system before attributes can be created for it" and acceptance criteria #9: "Bank must exist before attributes can be created for it."

**Dependencies:**
- VR-001 (User Authentication)

---

### Rule VR-007: Bank Attribute Existence Validation

**Field/Entity:** bank_attribute_id (Path Parameter)

**Validation Type:** Entity Existence / Business Constraint

**Rule Description:**
For update and delete operations, the bank attribute specified by BANK_ATTRIBUTE_ID must exist in the system.

**Validation Logic:**

- **Condition:** When PUT or DELETE /banks/BANK_ID/attributes/BANK_ATTRIBUTE_ID endpoint is accessed
- **Check:** Verify that a bank attribute with the specified ID exists for the given bank
- **Valid Criteria:** Bank attribute with the given ID exists for the specified bank
- **Invalid Criteria:** No bank attribute found with the specified ID
- **Action on Success:** Proceed with update or delete operation
- **Action on Failure:** Return attribute not found error response

**Error Handling:**

- **Error Message:** `BankAttributeNotFound: Specified bank attribute does not exist`
- **Error Code:** `OBP-30101`
- **HTTP Status Code:** `404 Not Found`

**Scala Implementation:**

- **Location:** `code.model.BankAttribute`
- **Method/Function:** `getBankAttributeById`

**Related Entities:**
- BankAttribute
- Bank

**User Story Context:**
This validation implements data validation requirement: "For update/delete operations, the attribute ID must exist."

**Dependencies:**
- VR-001 (User Authentication)
- VR-006 (Bank Existence)

---

## Category: Input Format Validation

### Rule VR-008: JSON Format Validation

**Field/Entity:** Request Body

**Validation Type:** Format Validation

**Rule Description:**
Request body must be valid JSON matching the expected BankAttributeJsonV400 schema for create and update operations.

**Validation Logic:**

- **Condition:** When POST or PUT endpoints are accessed with a request body
- **Check:** Parse and validate JSON structure against expected schema
- **Valid Criteria:** 
  - Valid JSON syntax
  - Contains required fields: name, type, value
  - Optional field: is_active (boolean)
- **Invalid Criteria:**
  - Malformed JSON syntax
  - Missing required fields
  - Unexpected field types
- **Action on Success:** Proceed with field-level validation
- **Action on Failure:** Return JSON format error response

**Error Handling:**

- **Error Message:** `InvalidJsonFormat: Request body is not valid JSON or doesn't match expected schema`
- **Error Code:** `OBP-10001`
- **HTTP Status Code:** `400 Bad Request`

**Scala Implementation:**

- **Location:** `code.api.util.APIUtil`
- **Method/Function:** `extractRequestBody` / JSON parsing

**Related Entities:**
- BankAttributeJsonV400

**User Story Context:**
This validation implements data validation requirement: "JSON Format Validation: Request body must be valid JSON matching the expected schema."

**Dependencies:**
- None (standalone validation)

---

### Rule VR-009: Attribute Type Enumeration Validation

**Field/Entity:** type (Request Body Field)

**Validation Type:** Format Validation / Enumeration

**Rule Description:**
The `type` field must be one of the valid attribute types: STRING, INTEGER, DOUBLE, or DATE_WITH_DAY.

**Validation Logic:**

- **Condition:** When creating or updating a bank attribute
- **Check:** Validate that the type field value is one of the allowed enumeration values
- **Valid Criteria:** Type is one of: STRING, INTEGER, DOUBLE, DATE_WITH_DAY
- **Invalid Criteria:** Type is any other value or empty
- **Action on Success:** Proceed with attribute creation/update
- **Action on Failure:** Return invalid type error response

**Error Handling:**

- **Error Message:** `InvalidBankAttributeType: Type must be one of STRING, INTEGER, DOUBLE, or DATE_WITH_DAY`
- **Error Code:** `OBP-30102`
- **HTTP Status Code:** `400 Bad Request`

**Scala Implementation:**

- **Location:** `code.model.BankAttribute`
- **Method/Function:** `BankAttributeType.withName`

**Code Snippet:**
```scala
sealed trait BankAttributeType
object BankAttributeType {
  case object STRING extends BankAttributeType
  case object INTEGER extends BankAttributeType
  case object DOUBLE extends BankAttributeType
  case object DATE_WITH_DAY extends BankAttributeType
}
```

**Related Entities:**
- BankAttribute
- BankAttributeType

**User Story Context:**
This validation implements acceptance criteria #7: "Bank attribute types must be one of: STRING, INTEGER, DOUBLE, or DATE_WITH_DAY" and business rule #2.

**Dependencies:**
- VR-008 (JSON Format Validation)

---

## Category: Length and Boundary Validation

### Rule VR-010: Attribute Name Length Validation

**Field/Entity:** name (Request Body Field)

**Validation Type:** Length Validation

**Rule Description:**
The attribute name must not exceed 50 characters in length.

**Validation Logic:**

- **Condition:** When creating or updating a bank attribute
- **Check:** Validate that the name field length is within the allowed limit
- **Valid Criteria:** Name length is between 1 and 50 characters
- **Invalid Criteria:** Name is empty or exceeds 50 characters
- **Action on Success:** Proceed with attribute creation/update
- **Action on Failure:** Return name length error response

**Error Handling:**

- **Error Message:** `InvalidAttributeNameLength: Attribute name must be between 1 and 50 characters`
- **Error Code:** `OBP-30103`
- **HTTP Status Code:** `400 Bad Request`

**Scala Implementation:**

- **Location:** `code.model.BankAttribute`
- **Method/Function:** Field validation in Mapper

**Related Entities:**
- BankAttribute

**User Story Context:**
This validation implements business rule #6: "Attribute Name Length: Attribute names are limited to 50 characters" and input data specification: "name: String (max 50 characters)."

**Dependencies:**
- VR-008 (JSON Format Validation)

---

### Rule VR-011: Attribute Value Length Validation

**Field/Entity:** value (Request Body Field)

**Validation Type:** Length Validation

**Rule Description:**
The attribute value must not exceed 255 characters in length.

**Validation Logic:**

- **Condition:** When creating or updating a bank attribute
- **Check:** Validate that the value field length is within the allowed limit
- **Valid Criteria:** Value length is between 0 and 255 characters
- **Invalid Criteria:** Value exceeds 255 characters
- **Action on Success:** Proceed with attribute creation/update
- **Action on Failure:** Return value length error response

**Error Handling:**

- **Error Message:** `InvalidAttributeValueLength: Attribute value must not exceed 255 characters`
- **Error Code:** `OBP-30104`
- **HTTP Status Code:** `400 Bad Request`

**Scala Implementation:**

- **Location:** `code.model.BankAttribute`
- **Method/Function:** Field validation in Mapper

**Related Entities:**
- BankAttribute

**User Story Context:**
This validation implements business rule #7: "Attribute Value Length: Attribute values are limited to 255 characters" and input data specification: "value: String (max 255 characters)."

**Dependencies:**
- VR-008 (JSON Format Validation)

---

## Category: Cross-Field Validation

### Rule VR-012: Attribute Value Type Consistency Validation

**Field/Entity:** value + type (Request Body Fields)

**Validation Type:** Cross-Field Validation

**Rule Description:**
The attribute value should be consistent with the declared attribute type. Values are stored as strings but should be validated against their declared type.

**Validation Logic:**

- **Condition:** When creating or updating a bank attribute
- **Check:** Validate that the value can be parsed/converted to the declared type
- **Valid Criteria:**
  - STRING: Any string value is valid
  - INTEGER: Value must be parseable as a whole number (e.g., "123")
  - DOUBLE: Value must be parseable as a decimal number (e.g., "12.1234")
  - DATE_WITH_DAY: Value must be in date format (e.g., "2012-04-23")
- **Invalid Criteria:** Value cannot be parsed/converted to the declared type
- **Action on Success:** Proceed with attribute creation/update
- **Action on Failure:** Return type mismatch error response

**Error Handling:**

- **Error Message:** `InvalidAttributeValueForType: Attribute value does not match the declared type`
- **Error Code:** `OBP-30105`
- **HTTP Status Code:** `400 Bad Request`

**Scala Implementation:**

- **Location:** `code.model.BankAttribute`
- **Method/Function:** Type coercion validation

**Related Entities:**
- BankAttribute
- BankAttributeType

**User Story Context:**
This validation implements acceptance criteria #10: "Attribute values are validated against their defined types" and addresses the known complexity: "Type Coercion: Values are stored as strings but should be validated against their declared type."

**Dependencies:**
- VR-008 (JSON Format Validation)
- VR-009 (Attribute Type Enumeration Validation)

---

## Quality Checklist

- [x] All validation functions in relevant code are documented
- [x] All error messages are captured with exact text
- [x] All error codes are documented
- [x] Length constraints are specified with exact limits
- [x] Required vs. optional fields are clearly marked
- [x] Cross-field validations are identified
- [x] Business constraint validations are included
- [x] User story context is explained for each rule
- [x] Valid and invalid criteria are clearly stated
- [x] HTTP status codes are documented where applicable
- [x] Dependencies between validations are noted
