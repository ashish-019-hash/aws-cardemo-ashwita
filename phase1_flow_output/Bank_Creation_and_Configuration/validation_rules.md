# Validation Rules

**Extracted From:** Open Bank Project (OBP) Scala Application  
**User Story:** Bank Creation and Configuration  
**Analysis Date:** 2026-01-09  
**Analyst:** Scala Systems Analyst

## Summary

Total Validation Rules Extracted: 12

### Validation Categories
- Input Validation Rules: 4
- Format Validation Rules: 3
- Business Constraint Rules: 3
- Length/Boundary Rules: 1
- Cross-Field Validation Rules: 1

---

## Category: Input Validation

### Rule VR-001: Bank ID Required Validation

**Field/Entity:** bank_id

**Validation Type:** Required Field Validation

**Rule Description:**
The bank_id field is required when creating a new bank. It must be provided in the request body and cannot be empty or null.

**Validation Logic:**

- **Condition:** When a POST request is made to `/banks` endpoint
- **Check:** Validate that bank_id is present and not empty in the request body
- **Valid Criteria:** bank_id is provided and is a non-empty string
- **Invalid Criteria:** bank_id is missing, null, or empty string
- **Action on Success:** Proceed with bank_id format validation
- **Action on Failure:** Return error response with 400 status code

**Error Handling:**

- **Error Message:** `OBP-10001: Incorrect json format.` or `OBP-30111: Invalid Bank Id. The BANK_ID should be a valid bank id`
- **Error Code:** `OBP-10001` or `OBP-30111`
- **HTTP Status Code:** `400 Bad Request`

**Scala Implementation:**

- **Location:** `code.api.v6_0_0.APIMethods600`
- **Method/Function:** `createBank`
- **Line Reference:** Bank creation endpoint handler

**Code Snippet:**
```scala
lazy val createBank: OBPEndpoint = {
  case "banks" :: Nil JsonPost json -> _ => {
    cc => implicit val ec = EndpointContext(Some(cc))
      for {
        postedData <- NewStyle.function.tryons(s"$InvalidJsonFormat The Json body should be the $PostBankJson600", 400, cc.callContext) {
          json.extract[PostBankJson600]
        }
        // bank_id validation follows
      } yield {
        // ...
      }
  }
}
```

**Related Entities:**
- Bank entity (primary identifier)
- All bank-related operations depend on valid bank_id

**User Story Context:**
This validation ensures that every bank created on the platform has a valid identifier, which is essential for the "Create new banks on the platform" acceptance criteria.

**Dependencies:**
- None (first validation in the chain)

---

### Rule VR-002: Bank Code Required Validation

**Field/Entity:** bank_code

**Validation Type:** Required Field Validation

**Rule Description:**
The bank_code field is required when creating a new bank. It serves as an additional identifier for the bank.

**Validation Logic:**

- **Condition:** When a POST request is made to `/banks` endpoint
- **Check:** Validate that bank_code is present in the request body
- **Valid Criteria:** bank_code is provided and is a non-empty string
- **Invalid Criteria:** bank_code is missing, null, or empty string
- **Action on Success:** Proceed with bank creation
- **Action on Failure:** Return error response with 400 status code

**Error Handling:**

- **Error Message:** `OBP-10001: Incorrect json format.`
- **Error Code:** `OBP-10001`
- **HTTP Status Code:** `400 Bad Request`

**Scala Implementation:**

- **Location:** `code.api.v6_0_0.JSONFactory600`
- **Method/Function:** `PostBankJson600` case class definition
- **Line Reference:** JSON factory for bank creation

**Code Snippet:**
```scala
case class PostBankJson600(
  bank_id: String,
  bank_code: String,
  full_name: Option[String],
  logo: Option[String],
  website: Option[String],
  bank_routings: Option[List[BankRoutingJsonV121]]
)
```

**Related Entities:**
- Bank entity (bank_code field)

**User Story Context:**
Bank code is required as part of the bank creation request structure defined in the user story's technical context.

**Dependencies:**
- VR-001 (Bank ID Required Validation)

---

### Rule VR-003: Bank Attribute Name Required Validation

**Field/Entity:** name (bank attribute)

**Validation Type:** Required Field Validation

**Rule Description:**
The name field is required when creating or updating a bank attribute. It identifies the attribute being stored.

**Validation Logic:**

- **Condition:** When a POST request is made to `/banks/BANK_ID/attribute` or PUT to `/banks/BANK_ID/attributes/BANK_ATTRIBUTE_ID`
- **Check:** Validate that name is present and not empty
- **Valid Criteria:** name is provided as a non-empty string
- **Invalid Criteria:** name is missing, null, or empty string
- **Action on Success:** Proceed with attribute type validation
- **Action on Failure:** Return error response with 400 status code

**Error Handling:**

- **Error Message:** `OBP-10001: Incorrect json format.`
- **Error Code:** `OBP-10001`
- **HTTP Status Code:** `400 Bad Request`

**Scala Implementation:**

- **Location:** `code.api.v4_0_0.JSONFactory400`
- **Method/Function:** `BankAttributeRequestJsonV400` case class
- **Line Reference:** JSON factory for bank attributes

**Code Snippet:**
```scala
case class BankAttributeRequestJsonV400(
  name: String,
  `type`: String,
  value: String,
  is_active: Boolean
)
```

**Related Entities:**
- BankAttribute entity (name field)

**User Story Context:**
This validation supports the acceptance criteria "Custom bank attributes can be created to store extended metadata for the bank."

**Dependencies:**
- Bank must exist (VR-007)

---

### Rule VR-004: Bank Attribute Value Required Validation

**Field/Entity:** value (bank attribute)

**Validation Type:** Required Field Validation

**Rule Description:**
The value field is required when creating or updating a bank attribute. It stores the actual data for the attribute.

**Validation Logic:**

- **Condition:** When a POST request is made to `/banks/BANK_ID/attribute` or PUT to `/banks/BANK_ID/attributes/BANK_ATTRIBUTE_ID`
- **Check:** Validate that value is present
- **Valid Criteria:** value is provided as a string
- **Invalid Criteria:** value is missing or null
- **Action on Success:** Proceed with attribute creation/update
- **Action on Failure:** Return error response with 400 status code

**Error Handling:**

- **Error Message:** `OBP-10001: Incorrect json format.`
- **Error Code:** `OBP-10001`
- **HTTP Status Code:** `400 Bad Request`

**Scala Implementation:**

- **Location:** `code.api.v4_0_0.JSONFactory400`
- **Method/Function:** `BankAttributeRequestJsonV400` case class
- **Line Reference:** JSON factory for bank attributes

**Code Snippet:**
```scala
case class BankAttributeRequestJsonV400(
  name: String,
  `type`: String,
  value: String,
  is_active: Boolean
)
```

**Related Entities:**
- BankAttribute entity (value field)

**User Story Context:**
This validation ensures that bank attributes have meaningful data, supporting the "Custom bank attributes can be created to store extended metadata" acceptance criteria.

**Dependencies:**
- VR-003 (Bank Attribute Name Required)

---

## Category: Format Validation

### Rule VR-005: Bank ID Format Validation - No Spaces

**Field/Entity:** bank_id

**Validation Type:** Format Validation (Character Restriction)

**Rule Description:**
The bank_id cannot contain space characters. This ensures the bank ID can be safely used in URLs and database operations.

**Validation Logic:**

- **Condition:** When bank_id is provided in bank creation request
- **Check:** Validate that bank_id does not contain any space characters
- **Valid Criteria:** bank_id contains no space characters (` `)
- **Invalid Criteria:** bank_id contains one or more space characters
- **Action on Success:** Proceed with additional bank_id validations
- **Action on Failure:** Return error response with 400 status code

**Error Handling:**

- **Error Message:** `OBP-30111: Invalid Bank Id. The BANK_ID should be a valid bank id`
- **Error Code:** `OBP-30111`
- **HTTP Status Code:** `400 Bad Request`

**Scala Implementation:**

- **Location:** `code.api.util.APIUtil`
- **Method/Function:** `checkShortString` or bank ID validation function
- **Line Reference:** Utility validation methods

**Code Snippet:**
```scala
def isValidBankId(bankId: String): Boolean = {
  !bankId.contains(" ") && !bankId.contains("::::") && bankId.length > 3
}
```

**Related Entities:**
- Bank entity (bank_id field)
- All API endpoints using BANK_ID path parameter

**User Story Context:**
This validation directly implements the acceptance criteria: "The bank ID... cannot contain spaces."

**Dependencies:**
- VR-001 (Bank ID Required Validation)

---

### Rule VR-006: Bank ID Format Validation - No Separator Characters

**Field/Entity:** bank_id

**Validation Type:** Format Validation (Character Restriction)

**Rule Description:**
The bank_id cannot contain the "::::" character sequence. This sequence is reserved for internal use as a separator in composite keys.

**Validation Logic:**

- **Condition:** When bank_id is provided in bank creation request
- **Check:** Validate that bank_id does not contain "::::" character sequence
- **Valid Criteria:** bank_id does not contain "::::" substring
- **Invalid Criteria:** bank_id contains "::::" substring
- **Action on Success:** Proceed with bank uniqueness validation
- **Action on Failure:** Return error response with 400 status code

**Error Handling:**

- **Error Message:** `OBP-30111: Invalid Bank Id. The BANK_ID should be a valid bank id`
- **Error Code:** `OBP-30111`
- **HTTP Status Code:** `400 Bad Request`

**Scala Implementation:**

- **Location:** `code.api.util.APIUtil`
- **Method/Function:** Bank ID validation function
- **Line Reference:** Utility validation methods

**Code Snippet:**
```scala
def isValidBankId(bankId: String): Boolean = {
  !bankId.contains(" ") && !bankId.contains("::::") && bankId.length > 3
}
```

**Related Entities:**
- Bank entity (bank_id field)

**User Story Context:**
This validation directly implements the acceptance criteria: "The bank ID... cannot contain '::::' characters."

**Dependencies:**
- VR-001 (Bank ID Required Validation)
- VR-005 (Bank ID No Spaces)

---

### Rule VR-007: Bank Attribute Type Validation

**Field/Entity:** type (bank attribute)

**Validation Type:** Format Validation (Enumeration)

**Rule Description:**
The type field for bank attributes must be one of the predefined valid types: STRING, INTEGER, DOUBLE, or DATE_WITH_DAY.

**Validation Logic:**

- **Condition:** When creating or updating a bank attribute
- **Check:** Validate that type is one of the allowed enumeration values
- **Valid Criteria:** type is exactly one of: "STRING", "INTEGER", "DOUBLE", "DATE_WITH_DAY"
- **Invalid Criteria:** type is any other value or empty
- **Action on Success:** Proceed with attribute creation/update
- **Action on Failure:** Return error response with 400 status code

**Error Handling:**

- **Error Message:** `OBP-10001: Incorrect json format.` or `OBP-30001: Invalid attribute type. Allowed types are: STRING, INTEGER, DOUBLE, DATE_WITH_DAY`
- **Error Code:** `OBP-10001` or `OBP-30001`
- **HTTP Status Code:** `400 Bad Request`

**Scala Implementation:**

- **Location:** `code.api.v4_0_0.APIMethods400`
- **Method/Function:** `createBankAttribute`, `updateBankAttribute`
- **Line Reference:** Bank attribute endpoint handlers

**Code Snippet:**
```scala
sealed trait BankAttributeType
object BankAttributeType {
  case object STRING extends BankAttributeType
  case object INTEGER extends BankAttributeType
  case object DOUBLE extends BankAttributeType
  case object DATE_WITH_DAY extends BankAttributeType
  
  def fromString(s: String): Box[BankAttributeType] = s match {
    case "STRING" => Full(STRING)
    case "INTEGER" => Full(INTEGER)
    case "DOUBLE" => Full(DOUBLE)
    case "DATE_WITH_DAY" => Full(DATE_WITH_DAY)
    case _ => Failure(s"Invalid attribute type: $s")
  }
}
```

**Related Entities:**
- BankAttribute entity (type field)

**User Story Context:**
This validation implements the acceptance criteria: "Bank attributes support multiple data types: STRING, INTEGER, DOUBLE, DATE_WITH_DAY."

**Dependencies:**
- VR-003 (Bank Attribute Name Required)
- VR-004 (Bank Attribute Value Required)

---

## Category: Length/Boundary Validation

### Rule VR-008: Bank ID Minimum Length Validation

**Field/Entity:** bank_id

**Validation Type:** Length Validation (Minimum)

**Rule Description:**
The bank_id must be greater than 3 characters in length. This ensures meaningful and distinguishable bank identifiers.

**Validation Logic:**

- **Condition:** When bank_id is provided in bank creation request
- **Check:** Validate that bank_id length is greater than 3 characters
- **Valid Criteria:** bank_id.length > 3 (minimum 4 characters)
- **Invalid Criteria:** bank_id.length <= 3 (3 or fewer characters)
- **Action on Success:** Proceed with bank creation
- **Action on Failure:** Return error response with 400 status code

**Error Handling:**

- **Error Message:** `OBP-30111: Invalid Bank Id. The BANK_ID should be a valid bank id` or `OBP-10002: Invalid Number. Bank ID must be greater than 3 characters.`
- **Error Code:** `OBP-30111` or `OBP-10002`
- **HTTP Status Code:** `400 Bad Request`

**Scala Implementation:**

- **Location:** `code.api.util.APIUtil`
- **Method/Function:** Bank ID validation function
- **Line Reference:** Utility validation methods

**Code Snippet:**
```scala
def isValidBankId(bankId: String): Boolean = {
  !bankId.contains(" ") && !bankId.contains("::::") && bankId.length > 3
}
```

**Related Entities:**
- Bank entity (bank_id field)

**User Story Context:**
This validation directly implements the acceptance criteria: "The bank ID must be greater than 3 characters."

**Dependencies:**
- VR-001 (Bank ID Required Validation)
- VR-005 (Bank ID No Spaces)
- VR-006 (Bank ID No Separator Characters)

---

## Category: Business Constraint Validation

### Rule VR-009: Bank ID Uniqueness Validation

**Field/Entity:** bank_id

**Validation Type:** Business Constraint (Uniqueness)

**Rule Description:**
The bank_id must be unique across all banks on the platform. No two banks can have the same bank_id.

**Validation Logic:**

- **Condition:** When creating a new bank
- **Check:** Query the database to verify no existing bank has the same bank_id
- **Valid Criteria:** No existing bank found with the provided bank_id
- **Invalid Criteria:** A bank already exists with the provided bank_id
- **Action on Success:** Proceed with bank creation
- **Action on Failure:** Return error response with 400 status code

**Error Handling:**

- **Error Message:** `OBP-30112: Bank with id BANK_ID already exists.`
- **Error Code:** `OBP-30112`
- **HTTP Status Code:** `400 Bad Request`

**Scala Implementation:**

- **Location:** `code.bankconnectors.Connector`
- **Method/Function:** `getBankLegacy`, `createOrUpdateBank`
- **Line Reference:** Bank connector methods

**Code Snippet:**
```scala
for {
  existingBank <- NewStyle.function.getBank(BankId(postedData.bank_id), cc.callContext)
  _ <- Helper.booleanToFuture(s"$BankAlreadyExists Bank with id ${postedData.bank_id} already exists.", cc=cc.callContext) {
    existingBank.isEmpty
  }
} yield {
  // proceed with creation
}
```

**Related Entities:**
- Bank entity (bank_id as primary identifier)
- All bank-related operations

**User Story Context:**
This validation implements the acceptance criteria: "The bank ID must be unique and not conflict with existing banks on the platform."

**Dependencies:**
- VR-001 through VR-008 (All bank_id format validations must pass first)

---

### Rule VR-010: User Authentication Validation

**Field/Entity:** User/Session

**Validation Type:** Business Constraint (Authentication)

**Rule Description:**
The user must be authenticated with valid credentials before creating a bank or bank attributes.

**Validation Logic:**

- **Condition:** When any bank creation or attribute endpoint is called
- **Check:** Validate that the request contains valid authentication credentials (OAuth, DirectLogin, etc.)
- **Valid Criteria:** Valid authentication token/credentials provided and user session is active
- **Invalid Criteria:** Missing, invalid, or expired authentication credentials
- **Action on Success:** Proceed with authorization check
- **Action on Failure:** Return error response with 401 status code

**Error Handling:**

- **Error Message:** `OBP-20001: User not logged in. Authentication is required.`
- **Error Code:** `OBP-20001`
- **HTTP Status Code:** `401 Unauthorized`

**Scala Implementation:**

- **Location:** `code.api.util.APIUtil`
- **Method/Function:** `getUser`, authentication middleware
- **Line Reference:** Authentication utility methods

**Code Snippet:**
```scala
for {
  (Full(u), callContext) <- authenticatedAccess(cc)
  // proceed with authenticated user
} yield {
  // ...
}
```

**Related Entities:**
- User entity
- Session/Token management

**User Story Context:**
This validation supports the dependency: "User authentication and authorization must be completed" before bank creation.

**Dependencies:**
- None (first validation in authentication chain)

---

### Rule VR-011: User Entitlement Validation for Bank Creation

**Field/Entity:** User Entitlements

**Validation Type:** Business Constraint (Authorization)

**Rule Description:**
The authenticated user must have the canCreateBank entitlement to create a new bank on the platform.

**Validation Logic:**

- **Condition:** When POST request is made to `/banks` endpoint
- **Check:** Validate that the authenticated user has canCreateBank entitlement
- **Valid Criteria:** User has canCreateBank entitlement assigned
- **Invalid Criteria:** User does not have canCreateBank entitlement
- **Action on Success:** Proceed with bank creation
- **Action on Failure:** Return error response with 403 status code

**Error Handling:**

- **Error Message:** `OBP-20006: User is missing one or more roles: canCreateBank`
- **Error Code:** `OBP-20006`
- **HTTP Status Code:** `403 Forbidden`

**Scala Implementation:**

- **Location:** `code.api.v6_0_0.APIMethods600`
- **Method/Function:** `createBank` endpoint with role check
- **Line Reference:** Bank creation endpoint

**Code Snippet:**
```scala
lazy val createBank: OBPEndpoint = {
  case "banks" :: Nil JsonPost json -> _ => {
    cc => implicit val ec = EndpointContext(Some(cc))
      for {
        (Full(u), callContext) <- authenticatedAccess(cc)
        _ <- NewStyle.function.hasEntitlement("", u.userId, canCreateBank, callContext)
        // proceed with bank creation
      } yield {
        // ...
      }
  }
}
```

**Related Entities:**
- User entity
- Entitlement entity
- Bank entity

**User Story Context:**
This validation implements the dependency: "User must have canCreateBank entitlement for bank creation."

**Dependencies:**
- VR-010 (User Authentication Validation)

---

## Category: Cross-Field Validation

### Rule VR-012: Bank Attribute is_active Boolean Validation

**Field/Entity:** is_active (bank attribute)

**Validation Type:** Cross-Field Validation (Type Constraint)

**Rule Description:**
The is_active field must be a valid boolean value (true or false) when creating or updating bank attributes.

**Validation Logic:**

- **Condition:** When creating or updating a bank attribute
- **Check:** Validate that is_active is a valid boolean value
- **Valid Criteria:** is_active is exactly true or false (boolean type)
- **Invalid Criteria:** is_active is a string, number, null, or any non-boolean value
- **Action on Success:** Proceed with attribute creation/update
- **Action on Failure:** Return error response with 400 status code

**Error Handling:**

- **Error Message:** `OBP-10001: Incorrect json format.`
- **Error Code:** `OBP-10001`
- **HTTP Status Code:** `400 Bad Request`

**Scala Implementation:**

- **Location:** `code.api.v4_0_0.JSONFactory400`
- **Method/Function:** JSON extraction for `BankAttributeRequestJsonV400`
- **Line Reference:** JSON factory for bank attributes

**Code Snippet:**
```scala
case class BankAttributeRequestJsonV400(
  name: String,
  `type`: String,
  value: String,
  is_active: Boolean  // Scala Boolean type enforces true/false
)

// JSON extraction will fail if is_active is not a valid boolean
json.extract[BankAttributeRequestJsonV400]
```

**Related Entities:**
- BankAttribute entity (is_active field)

**User Story Context:**
This validation ensures that bank attributes have a valid active status, supporting the attribute management functionality described in the user story.

**Dependencies:**
- VR-003 (Bank Attribute Name Required)
- VR-004 (Bank Attribute Value Required)
- VR-007 (Bank Attribute Type Validation)

---

## Validation Rules Summary Table

| Rule ID | Field/Entity | Validation Type | Error Code | HTTP Status |
|---------|--------------|-----------------|------------|-------------|
| VR-001 | bank_id | Required Field | OBP-10001/OBP-30111 | 400 |
| VR-002 | bank_code | Required Field | OBP-10001 | 400 |
| VR-003 | name (attribute) | Required Field | OBP-10001 | 400 |
| VR-004 | value (attribute) | Required Field | OBP-10001 | 400 |
| VR-005 | bank_id | Format (No Spaces) | OBP-30111 | 400 |
| VR-006 | bank_id | Format (No ::::) | OBP-30111 | 400 |
| VR-007 | type (attribute) | Format (Enum) | OBP-10001/OBP-30001 | 400 |
| VR-008 | bank_id | Length (Min > 3) | OBP-30111 | 400 |
| VR-009 | bank_id | Uniqueness | OBP-30112 | 400 |
| VR-010 | User/Session | Authentication | OBP-20001 | 401 |
| VR-011 | User Entitlements | Authorization | OBP-20006 | 403 |
| VR-012 | is_active | Boolean Type | OBP-10001 | 400 |

---

## Notes

- All validation rules are extracted based on the Bank Creation and Configuration user story
- Error codes follow the OBP-XXXXX format standard
- Validation order matters: authentication and authorization checks occur before input validation
- The automatic settlement account creation and role assignment features mentioned in the user story are business logic operations, not validation rules
- When migrating to Go, ensure all regex patterns and length constraints are preserved exactly
