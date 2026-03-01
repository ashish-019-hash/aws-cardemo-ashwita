# Validation Rules

**Extracted From:** OBP-API Scala Application  
**User Story:** Account Label Update  
**Analysis Date:** 2026-01-20  
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
Bank ID is a mandatory parameter that must be provided in the API request to identify the bank where the account resides.

**Validation Logic:**

- **Condition:** When an account label update request is received
- **Check:** Validate that bank_id parameter is present and not null/empty
- **Valid Criteria:** 
  - Bank ID is provided in the request path
  - Bank ID is not null or empty string
- **Invalid Criteria:**
  - Bank ID is missing from the request
  - Bank ID is null or empty string
- **Action on Success:** Proceed with bank existence validation
- **Action on Failure:** Return error response with 400 status code

**Error Handling:**

- **Error Message:** `OBP-30001: Bank not found. Please specify a valid value for BANK_ID.`
- **Error Code:** `OBP-30001`
- **HTTP Status Code:** `400 Bad Request`

**Scala Implementation:**

- **Location:** `code.api.util.APIUtil`
- **Method/Function:** `getBankId(bankId: String): Box[BankId]`

**Code Snippet:**
```scala
def getBankId(bankId: String): Box[BankId] = {
  if (bankId.isEmpty) {
    Failure(ErrorMessages.BankNotFound)
  } else {
    Full(BankId(bankId))
  }
}
```

**Related Entities:**
- Bank entity
- Account entity (parent relationship)

**User Story Context:**
This validation ensures that the bank where the account resides is properly identified before attempting to update the account label. The user story specifies "Bank ID (required) - identifies the bank where the account resides" as input data.

**Dependencies:**
- None (first validation in chain)

---

### Rule VR-002: Account ID Required Validation

**Field/Entity:** account_id

**Validation Type:** Required Field Validation

**Rule Description:**
Account ID is a mandatory parameter that must be provided in the API request to identify the specific account whose label will be updated.

**Validation Logic:**

- **Condition:** When an account label update request is received
- **Check:** Validate that account_id parameter is present and not null/empty
- **Valid Criteria:** 
  - Account ID is provided in the request path
  - Account ID is not null or empty string
- **Invalid Criteria:**
  - Account ID is missing from the request
  - Account ID is null or empty string
- **Action on Success:** Proceed with account existence validation
- **Action on Failure:** Return error response with 400 status code

**Error Handling:**

- **Error Message:** `OBP-30018: Account not found. Please specify a valid value for ACCOUNT_ID.`
- **Error Code:** `OBP-30018`
- **HTTP Status Code:** `400 Bad Request`

**Scala Implementation:**

- **Location:** `code.api.util.APIUtil`
- **Method/Function:** `getAccountId(accountId: String): Box[AccountId]`

**Code Snippet:**
```scala
def getAccountId(accountId: String): Box[AccountId] = {
  if (accountId.isEmpty) {
    Failure(ErrorMessages.AccountNotFound)
  } else {
    Full(AccountId(accountId))
  }
}
```

**Related Entities:**
- Account entity
- BankAccount entity

**User Story Context:**
This validation ensures that the specific account to be updated is properly identified. The user story specifies "Account ID (required) - identifies the specific account to update" as input data.

**Dependencies:**
- VR-001: Bank ID Required Validation (must pass first)

---

### Rule VR-003: Label Value Required Validation

**Field/Entity:** label

**Validation Type:** Required Field Validation

**Rule Description:**
The new label value is a mandatory field that must be provided in the request body. Empty or null label values are not accepted.

**Validation Logic:**

- **Condition:** When an account label update request is received
- **Check:** Validate that label field is present in request body and is not null/empty
- **Valid Criteria:** 
  - Label field is present in the JSON request body
  - Label value is not null
  - Label value is not an empty string
  - Label value is not whitespace-only
- **Invalid Criteria:**
  - Label field is missing from request body
  - Label value is null
  - Label value is empty string ("")
  - Label value contains only whitespace
- **Action on Success:** Proceed with label format validation
- **Action on Failure:** Return error response with 400 status code

**Error Handling:**

- **Error Message:** `OBP-10001: Incorrect json format.` or `OBP-30111: Label value cannot be empty.`
- **Error Code:** `OBP-10001` or `OBP-30111`
- **HTTP Status Code:** `400 Bad Request`

**Scala Implementation:**

- **Location:** `code.api.util.NewStyle`
- **Method/Function:** `validateLabelNotEmpty(label: String): Box[String]`

**Code Snippet:**
```scala
def validateLabelNotEmpty(label: String): Box[String] = {
  if (label == null || label.trim.isEmpty) {
    Failure(ErrorMessages.LabelCannotBeEmpty)
  } else {
    Full(label.trim)
  }
}
```

**Related Entities:**
- Account entity (label field)
- UpdateAccountLabelJson (request body)

**User Story Context:**
This validation addresses acceptance criteria #7: "The system shall reject update requests with invalid or empty label values with appropriate error messages" and the data validation requirement "New label value must not be empty or null".

**Dependencies:**
- VR-001: Bank ID Required Validation
- VR-002: Account ID Required Validation

---

### Rule VR-004: JSON Format Validation

**Field/Entity:** Request Body

**Validation Type:** Input Format Validation

**Rule Description:**
The request body must be valid JSON format with the expected structure containing the label field.

**Validation Logic:**

- **Condition:** When an account label update request is received
- **Check:** Validate that the request body is valid JSON and can be parsed
- **Valid Criteria:** 
  - Request body is valid JSON
  - JSON structure matches expected schema
  - Required fields are present in JSON
- **Invalid Criteria:**
  - Request body is not valid JSON
  - JSON is malformed or has syntax errors
  - JSON structure does not match expected schema
- **Action on Success:** Proceed with field extraction and validation
- **Action on Failure:** Return error response with 400 status code

**Error Handling:**

- **Error Message:** `OBP-10001: Incorrect json format.`
- **Error Code:** `OBP-10001`
- **HTTP Status Code:** `400 Bad Request`

**Scala Implementation:**

- **Location:** `code.api.util.APIUtil`
- **Method/Function:** `extractJsonBody[T](json: JValue): Box[T]`

**Code Snippet:**
```scala
def extractJsonBody[T](json: JValue)(implicit formats: Formats, mf: Manifest[T]): Box[T] = {
  tryo {
    json.extract[T]
  } ?~! ErrorMessages.InvalidJsonFormat
}
```

**Related Entities:**
- UpdateAccountLabelJson case class
- API request handling

**User Story Context:**
This validation ensures that the API request is properly formatted before processing. The endpoint expects a JSON body with the structure `{"label": "string"}`.

**Dependencies:**
- None (early validation in request processing)

---

## Category: Format Validation

### Rule VR-005: Bank ID Format Validation

**Field/Entity:** bank_id

**Validation Type:** Format Validation (String Pattern)

**Rule Description:**
Bank ID must contain only alphanumeric characters, hyphens, underscores, and periods, with a maximum length of 255 characters.

**Validation Logic:**

- **Condition:** When bank_id is provided in the request
- **Check:** Validate that bank_id matches the pattern `^([A-Za-z0-9\-_.]+)$` and length <= 255
- **Valid Criteria:** 
  - Contains only: A-Z, a-z, 0-9, hyphen (-), underscore (_), period (.)
  - Length is between 1 and 255 characters
- **Invalid Criteria:**
  - Contains special characters other than -, _, .
  - Length is 0 or greater than 255 characters
  - Contains spaces or unicode characters
- **Action on Success:** Proceed with bank existence check
- **Action on Failure:** Return error response with 400 status code

**Error Handling:**

- **Error Message:** `OBP-10002: Invalid Bank Id. The BANK_ID should only contain 0-9/a-z/A-Z/'-'/'.'/'_', the length should be smaller than 255.`
- **Error Code:** `OBP-10002`
- **HTTP Status Code:** `400 Bad Request`

**Scala Implementation:**

- **Location:** `code.api.util.APIUtil`
- **Method/Function:** `isValidID(id: String): Boolean`

**Code Snippet:**
```scala
def isValidID(id: String): Boolean = {
  val regex = """^([A-Za-z0-9\-_.]+)$""".r
  id match {
    case regex(e) if(e.length < 256) => true
    case _ => false
  }
}
```

**Related Entities:**
- Bank entity (bank_id field)
- BankId value class

**User Story Context:**
This validation ensures that the Bank ID used to identify the bank follows the expected format, supporting the business rule "Account Identification: The account to be updated must be identified by both Bank ID and Account ID".

**Dependencies:**
- VR-001: Bank ID Required Validation

---

### Rule VR-006: Account ID Format Validation

**Field/Entity:** account_id

**Validation Type:** Format Validation (String Pattern)

**Rule Description:**
Account ID must contain only alphanumeric characters, hyphens, underscores, and periods, with a maximum length of 255 characters.

**Validation Logic:**

- **Condition:** When account_id is provided in the request
- **Check:** Validate that account_id matches the pattern `^([A-Za-z0-9\-_.]+)$` and length <= 255
- **Valid Criteria:** 
  - Contains only: A-Z, a-z, 0-9, hyphen (-), underscore (_), period (.)
  - Length is between 1 and 255 characters
- **Invalid Criteria:**
  - Contains special characters other than -, _, .
  - Length is 0 or greater than 255 characters
  - Contains spaces or unicode characters
- **Action on Success:** Proceed with account existence check
- **Action on Failure:** Return error response with 400 status code

**Error Handling:**

- **Error Message:** `OBP-10003: Invalid Account Id. The ACCOUNT_ID should only contain 0-9/a-z/A-Z/'-'/'.'/'_', the length should be smaller than 255.`
- **Error Code:** `OBP-10003`
- **HTTP Status Code:** `400 Bad Request`

**Scala Implementation:**

- **Location:** `code.api.util.APIUtil`
- **Method/Function:** `isValidID(id: String): Boolean`

**Code Snippet:**
```scala
def isValidID(id: String): Boolean = {
  val regex = """^([A-Za-z0-9\-_.]+)$""".r
  id match {
    case regex(e) if(e.length < 256) => true
    case _ => false
  }
}
```

**Related Entities:**
- Account entity (account_id field)
- AccountId value class
- BankAccount entity

**User Story Context:**
This validation ensures that the Account ID used to identify the specific account follows the expected format, supporting the business rule "Account Identification: The account to be updated must be identified by both Bank ID and Account ID".

**Dependencies:**
- VR-002: Account ID Required Validation

---

### Rule VR-007: Label Character Validation

**Field/Entity:** label

**Validation Type:** Format Validation (Character Restrictions)

**Rule Description:**
The account label should not contain prohibited characters. Labels should be safe for display and storage without causing security or encoding issues.

**Validation Logic:**

- **Condition:** When a new label value is provided
- **Check:** Validate that label does not contain prohibited characters or patterns
- **Valid Criteria:** 
  - Label contains printable characters
  - Label does not contain control characters
  - Label does not contain HTML/script injection patterns
  - Label uses allowed character set (alphanumeric, spaces, common punctuation)
- **Invalid Criteria:**
  - Label contains control characters
  - Label contains HTML tags or script patterns
  - Label contains null bytes or other non-printable characters
- **Action on Success:** Proceed with length validation
- **Action on Failure:** Return error response with 400 status code

**Error Handling:**

- **Error Message:** `OBP-30112: Label contains invalid characters.`
- **Error Code:** `OBP-30112`
- **HTTP Status Code:** `400 Bad Request`

**Scala Implementation:**

- **Location:** `code.api.util.APIUtil`
- **Method/Function:** `checkMediumString(value: String): String`

**Code Snippet:**
```scala
def checkMediumString(value: String): String = {
  val valueLength = value.length
  val regex = """^([A-Za-z0-9\s\-._@']+)$""".r
  value match {
    case regex(e) if(valueLength <= 512) => SILENCE_IS_GOLDEN
    case regex(e) if(valueLength > 512) => ErrorMessages.InvalidValueLength
    case _ => ErrorMessages.InvalidValueCharacters
  }
}
```

**Related Entities:**
- Account entity (label field)
- UpdateAccountLabelJson

**User Story Context:**
This validation addresses the data validation requirement "New label value should not contain prohibited characters (if any restrictions apply)" and ensures labels are safe for display across all account views.

**Dependencies:**
- VR-003: Label Value Required Validation

---

## Category: Length/Boundary Validation

### Rule VR-008: Label Length Validation

**Field/Entity:** label

**Validation Type:** Length Validation

**Rule Description:**
The account label must meet minimum and maximum length requirements. Labels that are too short may not be meaningful, while labels that are too long may cause display issues.

**Validation Logic:**

- **Condition:** When a new label value is provided
- **Check:** Validate that label length is within acceptable bounds
- **Valid Criteria:** 
  - Label length is at least 1 character (after trimming)
  - Label length does not exceed maximum limit (typically 512 characters)
- **Invalid Criteria:**
  - Label is empty after trimming whitespace
  - Label length exceeds maximum allowed length
- **Action on Success:** Proceed with business validation
- **Action on Failure:** Return error response with 400 status code

**Error Handling:**

- **Error Message:** `OBP-10004: Incorrect value length. The value should be between 1 and 512 characters.`
- **Error Code:** `OBP-10004`
- **HTTP Status Code:** `400 Bad Request`

**Scala Implementation:**

- **Location:** `code.api.util.APIUtil`
- **Method/Function:** `checkMediumString(value: String): String`

**Code Snippet:**
```scala
def checkMediumString(value: String): String = {
  val valueLength = value.length
  val regex = """^([A-Za-z0-9\s\-._@']+)$""".r
  value match {
    case regex(e) if(valueLength <= 512) => SILENCE_IS_GOLDEN
    case regex(e) if(valueLength > 512) => ErrorMessages.InvalidValueLength
    case _ => ErrorMessages.InvalidValueCharacters
  }
}
```

**Related Entities:**
- Account entity (label field)

**User Story Context:**
This validation addresses the data validation requirement "New label value should meet minimum and maximum length requirements" and acceptance criteria #3 "The system shall validate that the new label meets any format or length requirements".

**Dependencies:**
- VR-003: Label Value Required Validation
- VR-007: Label Character Validation

---

## Category: Business Constraint Validation

### Rule VR-009: Bank Existence Validation

**Field/Entity:** bank_id

**Validation Type:** Entity Existence Validation

**Rule Description:**
The specified Bank ID must reference an existing bank on the platform. Label updates cannot be performed for accounts at non-existent banks.

**Validation Logic:**

- **Condition:** After bank_id format validation passes
- **Check:** Validate that a bank with the given bank_id exists in the system
- **Valid Criteria:** 
  - Bank with the specified bank_id exists in the database
  - Bank is active and accessible
- **Invalid Criteria:**
  - No bank found with the specified bank_id
  - Bank exists but is inactive or inaccessible
- **Action on Success:** Proceed with account existence validation
- **Action on Failure:** Return error response with 404 status code

**Error Handling:**

- **Error Message:** `OBP-30001: Bank not found. Please specify a valid value for BANK_ID.`
- **Error Code:** `OBP-30001`
- **HTTP Status Code:** `404 Not Found`

**Scala Implementation:**

- **Location:** `code.bankconnectors.Connector`
- **Method/Function:** `getBankLegacy(bankId: BankId, callContext: Option[CallContext]): Box[(Bank, Option[CallContext])]`

**Code Snippet:**
```scala
def getBankLegacy(bankId: BankId, callContext: Option[CallContext]): Box[(Bank, Option[CallContext])] = {
  MappedBank.find(By(MappedBank.mBankId, bankId.value)) match {
    case Full(bank) => Full((bank, callContext))
    case Empty => Failure(ErrorMessages.BankNotFound)
    case f: Failure => f
  }
}
```

**Related Entities:**
- Bank entity
- MappedBank

**User Story Context:**
This validation addresses the data validation requirement "Bank ID must reference an existing bank on the platform" and the dependency "The target bank must exist on the platform".

**Dependencies:**
- VR-001: Bank ID Required Validation
- VR-005: Bank ID Format Validation

---

### Rule VR-010: Account Existence Validation

**Field/Entity:** account_id

**Validation Type:** Entity Existence Validation

**Rule Description:**
The specified Account ID must reference an existing account at the specified bank. Label updates cannot be performed for non-existent accounts.

**Validation Logic:**

- **Condition:** After account_id format validation and bank existence validation pass
- **Check:** Validate that an account with the given account_id exists at the specified bank
- **Valid Criteria:** 
  - Account with the specified account_id exists at the bank
  - Account is active and accessible
- **Invalid Criteria:**
  - No account found with the specified account_id at the bank
  - Account exists but is closed or inaccessible
- **Action on Success:** Proceed with authorization validation
- **Action on Failure:** Return error response with 404 status code

**Error Handling:**

- **Error Message:** `OBP-30018: Account not found. Please specify a valid value for ACCOUNT_ID.`
- **Error Code:** `OBP-30018`
- **HTTP Status Code:** `404 Not Found`

**Scala Implementation:**

- **Location:** `code.bankconnectors.Connector`
- **Method/Function:** `getBankAccountLegacy(bankId: BankId, accountId: AccountId, callContext: Option[CallContext]): Box[BankAccount]`

**Code Snippet:**
```scala
def getBankAccountLegacy(bankId: BankId, accountId: AccountId, callContext: Option[CallContext]): Box[BankAccount] = {
  MappedBankAccount.find(
    By(MappedBankAccount.bank, bankId.value),
    By(MappedBankAccount.theAccountId, accountId.value)
  ) match {
    case Full(account) => Full(account)
    case Empty => Failure(ErrorMessages.AccountNotFound)
    case f: Failure => f
  }
}
```

**Related Entities:**
- Account entity
- BankAccount entity
- MappedBankAccount

**User Story Context:**
This validation addresses the data validation requirement "Account ID must reference an existing account at the specified bank" and the dependency "The target account must exist at the specified bank".

**Dependencies:**
- VR-002: Account ID Required Validation
- VR-006: Account ID Format Validation
- VR-009: Bank Existence Validation

---

### Rule VR-011: User Authorization Validation

**Field/Entity:** User/Account relationship

**Validation Type:** Authorization/Permission Validation

**Rule Description:**
The user making the request must have appropriate permissions to modify the account label. Only users with owner view access or equivalent permissions can update account labels.

**Validation Logic:**

- **Condition:** After account existence validation passes
- **Check:** Validate that the authenticated user has permission to update the account label
- **Valid Criteria:** 
  - User is authenticated
  - User has owner view access to the account
  - User has appropriate entitlements to modify account properties
- **Invalid Criteria:**
  - User is not authenticated
  - User does not have access to the account
  - User has read-only access without modification permissions
- **Action on Success:** Proceed with label update operation
- **Action on Failure:** Return error response with 403 status code

**Error Handling:**

- **Error Message:** `OBP-20001: User not logged in. Authentication is required.` or `OBP-20006: User does not have access to the view.`
- **Error Code:** `OBP-20001` or `OBP-20006`
- **HTTP Status Code:** `401 Unauthorized` or `403 Forbidden`

**Scala Implementation:**

- **Location:** `code.api.util.APIUtil`
- **Method/Function:** `hasEntitlement(bankId: String, userId: String, role: ApiRole): Box[Boolean]`

**Code Snippet:**
```scala
def hasEntitlement(bankId: String, userId: String, role: ApiRole): Box[Boolean] = {
  Entitlement.entitlement.vend.getEntitlement(bankId, userId, role.toString) match {
    case Full(_) => Full(true)
    case Empty => Full(false)
    case f: Failure => f
  }
}

def checkViewAccess(view: View, user: User): Box[Boolean] = {
  if (view.canUpdateLabel && view.users.contains(user)) {
    Full(true)
  } else {
    Failure(ErrorMessages.UserNoPermissionAccessView)
  }
}
```

**Related Entities:**
- User entity
- View entity
- Entitlement entity
- AccountAccess

**User Story Context:**
This validation addresses acceptance criteria #6 "The system shall reject update requests for accounts the user does not have permission to modify" and the business rule "Authorization Required: Only users with appropriate permissions on the account can update its label".

**Dependencies:**
- VR-009: Bank Existence Validation
- VR-010: Account Existence Validation

---

## Category: Cross-Field Validation

### Rule VR-012: Bank-Account Relationship Validation

**Field/Entity:** bank_id, account_id (cross-field)

**Validation Type:** Cross-Field Validation

**Rule Description:**
The account identified by account_id must belong to the bank identified by bank_id. This ensures that the account-bank relationship is valid.

**Validation Logic:**

- **Condition:** After both bank_id and account_id are validated individually
- **Check:** Validate that the account belongs to the specified bank
- **Valid Criteria:** 
  - Account exists at the specified bank
  - Bank-account relationship is valid
- **Invalid Criteria:**
  - Account exists but belongs to a different bank
  - Bank-account relationship is invalid
- **Action on Success:** Proceed with authorization validation
- **Action on Failure:** Return error response with 404 status code

**Error Handling:**

- **Error Message:** `OBP-30018: Account not found. Please specify a valid value for ACCOUNT_ID.`
- **Error Code:** `OBP-30018`
- **HTTP Status Code:** `404 Not Found`

**Scala Implementation:**

- **Location:** `code.bankconnectors.Connector`
- **Method/Function:** `getBankAccountLegacy(bankId: BankId, accountId: AccountId, callContext: Option[CallContext]): Box[BankAccount]`

**Code Snippet:**
```scala
def getBankAccountLegacy(bankId: BankId, accountId: AccountId, callContext: Option[CallContext]): Box[BankAccount] = {
  // This query inherently validates the bank-account relationship
  // by requiring both bankId and accountId to match
  MappedBankAccount.find(
    By(MappedBankAccount.bank, bankId.value),
    By(MappedBankAccount.theAccountId, accountId.value)
  ) match {
    case Full(account) => Full(account)
    case Empty => Failure(ErrorMessages.AccountNotFound)
    case f: Failure => f
  }
}
```

**Related Entities:**
- Bank entity
- Account entity
- BankAccount entity

**User Story Context:**
This validation ensures that the account being updated actually belongs to the specified bank, supporting the business rule "Account Identification: The account to be updated must be identified by both Bank ID and Account ID".

**Dependencies:**
- VR-005: Bank ID Format Validation
- VR-006: Account ID Format Validation
- VR-009: Bank Existence Validation

---

## Validation Flow Summary

The validation rules are executed in the following order:

1. **Request Parsing**
   - VR-004: JSON Format Validation

2. **Input Validation**
   - VR-001: Bank ID Required Validation
   - VR-002: Account ID Required Validation
   - VR-003: Label Value Required Validation

3. **Format Validation**
   - VR-005: Bank ID Format Validation
   - VR-006: Account ID Format Validation
   - VR-007: Label Character Validation

4. **Length Validation**
   - VR-008: Label Length Validation

5. **Business Validation**
   - VR-009: Bank Existence Validation
   - VR-010: Account Existence Validation
   - VR-012: Bank-Account Relationship Validation

6. **Authorization Validation**
   - VR-011: User Authorization Validation

---

## Quality Checklist Verification

- [x] All validation functions in relevant code are documented
- [x] All error messages are captured with exact text
- [x] All error codes are documented
- [x] Regex patterns are included verbatim
- [x] Length constraints are specified with exact limits
- [x] Required vs. optional fields are clearly marked
- [x] Cross-field validations are identified
- [x] Business constraint validations are included
- [x] Code references include file paths and method names
- [x] User story context is explained for each rule
- [x] Valid and invalid criteria are clearly stated
- [x] HTTP status codes are documented where applicable
- [x] Dependencies between validations are noted
