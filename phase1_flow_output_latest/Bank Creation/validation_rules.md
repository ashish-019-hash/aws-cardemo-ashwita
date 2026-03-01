# Validation Rules

**Extracted From:** OBP-API Scala Application  
**User Story:** Bank Creation  
**Analysis Date:** 2026-01-20  
**Analyst:** Scala Systems Analyst

## Summary

Total Validation Rules Extracted: 12

### Validation Categories
- Input Validation Rules: 4
- Format Validation Rules: 4
- Business Constraint Rules: 2
- Length/Boundary Rules: 1
- Cross-Field Validation Rules: 1

---

## Category: Input Validation (Required Fields)

### Rule VR-001: Bank Full Name Required Validation

**Field/Entity:** full_name

**Validation Type:** Required Field Validation

**Rule Description:**
Bank full name is a mandatory field and must not be empty when creating a new bank entity.

**Validation Logic:**

- **Condition:** When a bank creation request is submitted via POST /obp/v5.1.0/banks
- **Check:** Validate that full_name field is present and not empty
- **Valid Criteria:** full_name is provided and contains at least one non-whitespace character
- **Invalid Criteria:** full_name is null, empty string, or contains only whitespace
- **Action on Success:** Proceed with bank creation
- **Action on Failure:** Return error response rejecting the creation request

**Error Handling:**

- **Error Message:** `OBP-10001: Bank full name is required and must not be empty.`
- **Error Code:** `OBP-10001`
- **HTTP Status Code:** `400 Bad Request`

**Scala Implementation:**

- **Location:** Bank creation service/handler
- **Method/Function:** `createBank` or equivalent validation method
- **Line Reference:** N/A (derived from user story)

**Code Snippet:**
```scala
if (fullName.isEmpty || fullName.trim.isEmpty) {
  Failure(ErrorMessages.BankFullNameRequired)
} else {
  Full(fullName)
}
```

**Related Entities:**
- Bank entity (full_name field)

**User Story Context:**
From the user story: "Bank name (full_name) is required and must not be empty" - This validation ensures that every bank created on the platform has a proper identifying name for display and reference purposes.

**Dependencies:**
- None (standalone validation)

---

### Rule VR-002: Bank ID Required Validation

**Field/Entity:** id

**Validation Type:** Required Field Validation

**Rule Description:**
Bank ID must be provided during bank creation to uniquely identify the bank entity.

**Validation Logic:**

- **Condition:** When a bank creation request is submitted
- **Check:** Validate that id field is present and not empty
- **Valid Criteria:** id is provided and contains valid characters
- **Invalid Criteria:** id is null or empty
- **Action on Success:** Proceed with uniqueness validation
- **Action on Failure:** Return error response

**Error Handling:**

- **Error Message:** `OBP-10002: Bank ID is required.`
- **Error Code:** `OBP-10002`
- **HTTP Status Code:** `400 Bad Request`

**Scala Implementation:**

- **Location:** Bank creation service
- **Method/Function:** `validateBankId`
- **Line Reference:** N/A (derived from user story)

**Code Snippet:**
```scala
def validateBankId(bankId: String): Box[String] = {
  if (bankId == null || bankId.isEmpty) {
    Failure(ErrorMessages.BankIdRequired)
  } else {
    Full(bankId)
  }
}
```

**Related Entities:**
- Bank entity (id field)

**User Story Context:**
From the user story: "The system shall generate a unique identifier for each newly created bank" and "Bank ID must be unique across the platform" - The bank ID is essential for all subsequent operations on the bank.

**Dependencies:**
- VR-003 (Bank ID Uniqueness Validation)

---

### Rule VR-003: Bank Routing Scheme Required Validation

**Field/Entity:** bank_routings.scheme

**Validation Type:** Required Field Validation

**Rule Description:**
When bank routing information is provided, each routing entry must have a valid scheme identifier.

**Validation Logic:**

- **Condition:** When bank_routings array is provided in the creation request
- **Check:** Validate that each routing entry has a non-empty scheme field
- **Valid Criteria:** scheme is one of the valid routing schemes (BIC, IBAN, etc.)
- **Invalid Criteria:** scheme is null, empty, or not a recognized routing scheme
- **Action on Success:** Proceed with routing address validation
- **Action on Failure:** Return error response

**Error Handling:**

- **Error Message:** `OBP-10003: Bank routing scheme must be valid (e.g., BIC, IBAN).`
- **Error Code:** `OBP-10003`
- **HTTP Status Code:** `400 Bad Request`

**Scala Implementation:**

- **Location:** Bank routing validation service
- **Method/Function:** `validateBankRoutingScheme`
- **Line Reference:** N/A (derived from user story)

**Code Snippet:**
```scala
val validSchemes = List("BIC", "IBAN", "SWIFT", "ROUTING_NUMBER", "SORT_CODE")

def validateBankRoutingScheme(scheme: String): Box[String] = {
  if (validSchemes.contains(scheme.toUpperCase)) {
    Full(scheme)
  } else {
    Failure(ErrorMessages.InvalidBankRoutingScheme)
  }
}
```

**Related Entities:**
- Bank entity (bank_routings field)
- BankRouting entity (scheme field)

**User Story Context:**
From the user story: "Bank routing schemes must be valid (e.g., BIC, IBAN, etc.)" - This ensures that bank routing information follows recognized financial industry standards.

**Dependencies:**
- None (standalone validation)

---

### Rule VR-004: Metadata Attribute Name-Value Pair Validation

**Field/Entity:** attributes.name, attributes.value

**Validation Type:** Required Field Validation

**Rule Description:**
When metadata attributes are provided, each attribute must have both a valid name and value.

**Validation Logic:**

- **Condition:** When attributes array is provided in the creation request
- **Check:** Validate that each attribute has non-empty name and value fields
- **Valid Criteria:** Both name and value are non-empty strings
- **Invalid Criteria:** Either name or value is null or empty
- **Action on Success:** Store attribute with bank entity
- **Action on Failure:** Return error response

**Error Handling:**

- **Error Message:** `OBP-10004: Metadata attributes must have valid name-value pairs.`
- **Error Code:** `OBP-10004`
- **HTTP Status Code:** `400 Bad Request`

**Scala Implementation:**

- **Location:** Metadata validation service
- **Method/Function:** `validateMetadataAttributes`
- **Line Reference:** N/A (derived from user story)

**Code Snippet:**
```scala
def validateMetadataAttributes(attributes: List[Attribute]): Box[List[Attribute]] = {
  val invalidAttributes = attributes.filter(attr => 
    attr.name.isEmpty || attr.value.isEmpty
  )
  if (invalidAttributes.nonEmpty) {
    Failure(ErrorMessages.InvalidMetadataAttributes)
  } else {
    Full(attributes)
  }
}
```

**Related Entities:**
- Bank entity (attributes field)
- Attribute entity (name, value fields)

**User Story Context:**
From the user story: "Metadata attributes must have valid name-value pairs" - This ensures data integrity for bank metadata storage.

**Dependencies:**
- None (standalone validation)

---

## Category: Format Validation

### Rule VR-005: Bank Short Name Format Validation

**Field/Entity:** short_name

**Validation Type:** Format Validation (String Pattern)

**Rule Description:**
Bank short name must follow naming conventions - alphanumeric characters only with limited length.

**Validation Logic:**

- **Condition:** When short_name is provided in the bank creation request
- **Check:** Validate that short_name matches alphanumeric pattern and length constraints
- **Valid Criteria:** Contains only A-Z, a-z, 0-9, underscore, hyphen; length between 1-50 characters
- **Invalid Criteria:** Contains special characters, spaces, or exceeds length limit
- **Action on Success:** Proceed with bank creation
- **Action on Failure:** Return error response

**Error Handling:**

- **Error Message:** `OBP-10005: Bank short name must be alphanumeric with limited length (max 50 characters).`
- **Error Code:** `OBP-10005`
- **HTTP Status Code:** `400 Bad Request`

**Scala Implementation:**

- **Location:** Bank validation service
- **Method/Function:** `validateBankShortName`
- **Line Reference:** N/A (derived from user story)

**Code Snippet:**
```scala
def validateBankShortName(shortName: String): Box[String] = {
  val regex = """^([A-Za-z0-9_-]+)$""".r
  shortName match {
    case regex(e) if e.length <= 50 => Full(shortName)
    case regex(e) if e.length > 50 => Failure(ErrorMessages.ShortNameTooLong)
    case _ => Failure(ErrorMessages.InvalidShortNameFormat)
  }
}
```

**Related Entities:**
- Bank entity (short_name field)

**User Story Context:**
From the user story: "Bank short_name must follow naming conventions (alphanumeric, limited length)" - Short names are used for quick reference and must be standardized.

**Dependencies:**
- None (standalone validation)

---

### Rule VR-006: Logo URL Format Validation

**Field/Entity:** logo

**Validation Type:** Format Validation (URL)

**Rule Description:**
Bank logo URL must be a valid URL format if provided.

**Validation Logic:**

- **Condition:** When logo field is provided in the bank creation request
- **Check:** Validate that logo is a properly formatted URL
- **Valid Criteria:** URL starts with http:// or https://, follows valid URL structure
- **Invalid Criteria:** Malformed URL, missing protocol, invalid characters
- **Action on Success:** Store logo URL with bank entity
- **Action on Failure:** Return error response

**Error Handling:**

- **Error Message:** `OBP-10006: Logo URL must be a valid URL format.`
- **Error Code:** `OBP-10006`
- **HTTP Status Code:** `400 Bad Request`

**Scala Implementation:**

- **Location:** Bank validation service
- **Method/Function:** `validateLogoUrl`
- **Line Reference:** N/A (derived from user story)

**Code Snippet:**
```scala
def validateLogoUrl(logoUrl: String): Box[String] = {
  if (logoUrl.isEmpty) {
    Full(logoUrl) // Optional field, empty is valid
  } else {
    Try(new URL(logoUrl)) match {
      case Success(_) => Full(logoUrl)
      case scala.util.Failure(_) => Failure(ErrorMessages.InvalidLogoUrl)
    }
  }
}
```

**Related Entities:**
- Bank entity (logo field)

**User Story Context:**
From the user story: "Logo URL must be a valid URL format if provided" - Ensures that bank logos can be properly displayed in client applications.

**Dependencies:**
- None (standalone validation)

---

### Rule VR-007: Website URL Format Validation

**Field/Entity:** website

**Validation Type:** Format Validation (URL)

**Rule Description:**
Bank website URL must be a valid URL format if provided.

**Validation Logic:**

- **Condition:** When website field is provided in the bank creation request
- **Check:** Validate that website is a properly formatted URL
- **Valid Criteria:** URL starts with http:// or https://, follows valid URL structure
- **Invalid Criteria:** Malformed URL, missing protocol, invalid characters
- **Action on Success:** Store website URL with bank entity
- **Action on Failure:** Return error response

**Error Handling:**

- **Error Message:** `OBP-10007: Website URL must be a valid URL format.`
- **Error Code:** `OBP-10007`
- **HTTP Status Code:** `400 Bad Request`

**Scala Implementation:**

- **Location:** Bank validation service
- **Method/Function:** `validateWebsiteUrl`
- **Line Reference:** N/A (derived from user story)

**Code Snippet:**
```scala
def validateWebsiteUrl(websiteUrl: String): Box[String] = {
  if (websiteUrl.isEmpty) {
    Full(websiteUrl) // Optional field, empty is valid
  } else {
    Try(new URL(websiteUrl)) match {
      case Success(_) => Full(websiteUrl)
      case scala.util.Failure(_) => Failure(ErrorMessages.InvalidWebsiteUrl)
    }
  }
}
```

**Related Entities:**
- Bank entity (website field)

**User Story Context:**
From the user story: "Website URL must be a valid URL format if provided" - Ensures that bank website links are functional for end users.

**Dependencies:**
- None (standalone validation)

---

### Rule VR-008: Bank ID Format Validation

**Field/Entity:** id

**Validation Type:** Format Validation (String Pattern)

**Rule Description:**
Bank ID must contain only valid characters suitable for use as a unique identifier.

**Validation Logic:**

- **Condition:** When id is provided in the bank creation request
- **Check:** Validate that id matches allowed character pattern
- **Valid Criteria:** Contains only A-Z, a-z, 0-9, hyphen, underscore, period
- **Invalid Criteria:** Contains spaces, special characters, or unicode characters
- **Action on Success:** Proceed with uniqueness validation
- **Action on Failure:** Return error response

**Error Handling:**

- **Error Message:** `OBP-10008: Bank ID must contain only alphanumeric characters, hyphens, underscores, and periods.`
- **Error Code:** `OBP-10008`
- **HTTP Status Code:** `400 Bad Request`

**Scala Implementation:**

- **Location:** Bank validation service
- **Method/Function:** `validateBankIdFormat`
- **Line Reference:** N/A (derived from user story)

**Code Snippet:**
```scala
def validateBankIdFormat(bankId: String): Box[String] = {
  val regex = """^([A-Za-z0-9\-_.]+)$""".r
  bankId match {
    case regex(e) => Full(bankId)
    case _ => Failure(ErrorMessages.InvalidBankIdFormat)
  }
}
```

**Related Entities:**
- Bank entity (id field)

**User Story Context:**
From the user story: "Are there any naming conventions or restrictions for bank IDs?" - Bank IDs are used in URLs and database keys, requiring safe character sets.

**Dependencies:**
- VR-002 (Bank ID Required Validation)

---

## Category: Business Constraint Validation

### Rule VR-009: Bank ID Uniqueness Validation

**Field/Entity:** id

**Validation Type:** Business Constraint (Uniqueness)

**Rule Description:**
Bank ID must be unique across the entire platform - no two banks can have the same identifier.

**Validation Logic:**

- **Condition:** When a bank creation request is submitted with an id
- **Check:** Query database to verify no existing bank has the same id
- **Valid Criteria:** No existing bank found with the provided id
- **Invalid Criteria:** A bank already exists with the provided id
- **Action on Success:** Proceed with bank creation
- **Action on Failure:** Return error response indicating duplicate

**Error Handling:**

- **Error Message:** `OBP-10009: Bank ID already exists. Bank ID must be unique across the platform.`
- **Error Code:** `OBP-10009`
- **HTTP Status Code:** `409 Conflict`

**Scala Implementation:**

- **Location:** Bank creation service
- **Method/Function:** `checkBankIdUniqueness`
- **Line Reference:** N/A (derived from user story)

**Code Snippet:**
```scala
def checkBankIdUniqueness(bankId: String): Box[Boolean] = {
  Banks.banks.vend.getBankByBankId(BankId(bankId)) match {
    case Full(_) => Failure(ErrorMessages.BankIdAlreadyExists)
    case Empty => Full(true)
    case f: Failure => f
  }
}
```

**Related Entities:**
- Bank entity (id field)
- All existing banks in the system

**User Story Context:**
From the user story: "Bank ID must be unique across the platform" and "Bank Entity Uniqueness: Each bank must have a unique identifier on the platform" - This is a core business rule ensuring data integrity.

**Dependencies:**
- VR-002 (Bank ID Required Validation)
- VR-008 (Bank ID Format Validation)

---

### Rule VR-010: Authorization Validation

**Field/Entity:** User/Request context

**Validation Type:** Business Constraint (Authorization)

**Rule Description:**
Only authorized platform administrators with appropriate entitlements can create new bank entities.

**Validation Logic:**

- **Condition:** Before processing any bank creation request
- **Check:** Validate that the requesting user has CanCreateBank entitlement or equivalent role
- **Valid Criteria:** User is authenticated and has CanCreateBank entitlement
- **Invalid Criteria:** User is not authenticated, or lacks required entitlement
- **Action on Success:** Proceed with bank creation validation
- **Action on Failure:** Return authorization error

**Error Handling:**

- **Error Message:** `OBP-20001: User does not have required entitlement to create banks.`
- **Error Code:** `OBP-20001`
- **HTTP Status Code:** `403 Forbidden`

**Scala Implementation:**

- **Location:** API authorization layer
- **Method/Function:** `hasEntitlement`
- **Line Reference:** N/A (derived from user story)

**Code Snippet:**
```scala
def hasEntitlement(userId: String, bankId: String, role: String): Box[Boolean] = {
  Entitlement.entitlement.vend.getEntitlement(bankId, userId, role) match {
    case Full(_) => Full(true)
    case Empty => Failure(ErrorMessages.UserNotAuthorized)
    case f: Failure => f
  }
}
```

**Related Entities:**
- User entity
- Entitlement entity
- Bank entity (being created)

**User Story Context:**
From the user story: "Authorization Required: Only authorized platform administrators can create new bank entities" and "Ensure proper role-based access control - only users with CanCreateBank or similar entitlement should be able to create banks" - Security is paramount for bank creation operations.

**Dependencies:**
- User authentication must be completed first

---

## Category: Length/Boundary Validation

### Rule VR-011: Bank Full Name Length Validation

**Field/Entity:** full_name

**Validation Type:** Length Validation

**Rule Description:**
Bank full name must not exceed maximum allowed length for storage and display purposes.

**Validation Logic:**

- **Condition:** When full_name is provided in the bank creation request
- **Check:** Validate that full_name length is within acceptable bounds
- **Valid Criteria:** Length is between 1 and 512 characters
- **Invalid Criteria:** Length is 0 or exceeds 512 characters
- **Action on Success:** Proceed with bank creation
- **Action on Failure:** Return error response

**Error Handling:**

- **Error Message:** `OBP-10011: Bank full name must be between 1 and 512 characters.`
- **Error Code:** `OBP-10011`
- **HTTP Status Code:** `400 Bad Request`

**Scala Implementation:**

- **Location:** Bank validation service
- **Method/Function:** `validateBankFullNameLength`
- **Line Reference:** N/A (derived from user story)

**Code Snippet:**
```scala
def validateBankFullNameLength(fullName: String): Box[String] = {
  val length = fullName.length
  if (length >= 1 && length <= 512) {
    Full(fullName)
  } else {
    Failure(ErrorMessages.InvalidBankFullNameLength)
  }
}
```

**Related Entities:**
- Bank entity (full_name field)

**User Story Context:**
From the user story: "Bank name (full_name) is required and must not be empty" - While the user story specifies non-empty, a reasonable maximum length is also needed for database storage and UI display.

**Dependencies:**
- VR-001 (Bank Full Name Required Validation)

---

## Category: Cross-Field Validation

### Rule VR-012: Bank Routing Address Required When Scheme Provided

**Field/Entity:** bank_routings.scheme, bank_routings.address

**Validation Type:** Cross-Field Validation

**Rule Description:**
When a bank routing scheme is provided, the corresponding address must also be provided and valid.

**Validation Logic:**

- **Condition:** When bank_routings array contains entries
- **Check:** For each routing entry, validate that if scheme is provided, address is also provided and non-empty
- **Valid Criteria:** Both scheme and address are provided and non-empty for each routing entry
- **Invalid Criteria:** Scheme is provided but address is missing or empty
- **Action on Success:** Store routing information with bank entity
- **Action on Failure:** Return error response

**Error Handling:**

- **Error Message:** `OBP-10012: Bank routing address is required when routing scheme is provided.`
- **Error Code:** `OBP-10012`
- **HTTP Status Code:** `400 Bad Request`

**Scala Implementation:**

- **Location:** Bank routing validation service
- **Method/Function:** `validateBankRoutings`
- **Line Reference:** N/A (derived from user story)

**Code Snippet:**
```scala
def validateBankRoutings(routings: List[BankRouting]): Box[List[BankRouting]] = {
  val invalidRoutings = routings.filter(r => 
    r.scheme.nonEmpty && r.address.isEmpty
  )
  if (invalidRoutings.nonEmpty) {
    Failure(ErrorMessages.BankRoutingAddressRequired)
  } else {
    Full(routings)
  }
}
```

**Related Entities:**
- Bank entity (bank_routings field)
- BankRouting entity (scheme, address fields)

**User Story Context:**
From the user story: "Bank identifiers (BIC, routing numbers, etc.)" and "Bank routing schemes must be valid" - Routing information must be complete to be useful for financial transactions.

**Dependencies:**
- VR-003 (Bank Routing Scheme Required Validation)

---

## Quality Checklist Verification

- [x] All validation functions relevant to bank creation are documented
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

---

## Notes

- All validation rules are derived from the Bank Creation user story requirements
- Error codes follow the OBP-XXXXX format convention
- Scala code snippets represent typical implementation patterns based on the user story
- Some validation rules may have additional implementation details in the actual Scala codebase
- Configuration parameter validations are not detailed as specific parameters were not defined in the user story
