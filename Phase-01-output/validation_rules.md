# Validation Rules

**Extracted From:** Open Bank Project API (OBP-API) - Scala Application  
**User Story:** Bank Registration and Configuration  
**Analysis Date:** November 17, 2025  
**Analyst:** Scala Systems Analyst

## Summary

Total Validation Rules Extracted: 15

### Validation Categories
- Input Validation Rules: 7
- Format Validation Rules: 3
- Business Constraint Rules: 2
- Authentication/Authorization Rules: 3
- Cross-Field Validation Rules: 0

---

## Category: Input Format Validation

### Rule VR-001: Bank ID Format Validation

**Field/Entity:** bank.id (Bank identifier)

**Validation Type:** Format Validation (String Pattern)

**Rule Description:**
Bank ID must contain only alphanumeric characters, hyphens, underscores, and periods, with a maximum length of 16 characters. This ensures the bank ID is URL-safe and compatible with database constraints.

**Validation Logic:**

- **Condition:** When a bank ID is provided in the POST /banks request body
- **Check:** Validate that bank.id matches the pattern `^([A-Za-z0-9\-._]+)$` and length <= 16
- **Valid Criteria:** 
  - Contains only: A-Z, a-z, 0-9, hyphen (-), underscore (_), period (.)
  - Length is between 1 and 16 characters
  - At least one character is required
- **Invalid Criteria:**
  - Contains special characters other than -, _, .
  - Contains spaces or unicode characters
  - Length is 0 or greater than 16 characters
- **Action on Success:** Proceed with bank creation/update operation
- **Action on Failure:** Return error response with 400 status code

**Error Handling:**

- **Error Message:** `OBP-20010: Value too long` (if length > 16) or `OBP-20011: Value contains invalid characters` (if pattern doesn't match)
- **Error Code:** `OBP-20010` or `OBP-20011`
- **HTTP Status Code:** `400 Bad Request`

**Scala Implementation:**

- **Location:** `code.api.util.APIUtil`
- **Method/Function:** `checkShortString(value: String): String`
- **Line Reference:** Lines 907-915

**Code Snippet:**
```scala
def checkShortString(value:String): String ={
  val valueLength = value.length
  val regex = """^([A-Za-z0-9\-._]+)$""".r
  value match {
    case regex(e) if(valueLength <= 16) => SILENCE_IS_GOLDEN
    case regex(e) if(valueLength > 16) => ErrorMessages.InvalidValueLength
    case _ => ErrorMessages.InvalidValueCharacters
  }
}
```

**Related Entities:**
- Bank (id field / permalink)
- PostBankJson400 (request DTO)
- BankJson400 (response DTO)

**User Story Context:**
This validation ensures that bank identifiers used in the bank registration configuration are properly formatted and can be safely stored in the database and used in URLs without encoding issues. It's the first validation check performed on the bank ID in the createBank endpoint.

**Dependencies:**
- None (standalone validation)

---

### Rule VR-002: Bank ID Minimum Length Validation

**Field/Entity:** bank.id (Bank identifier)

**Validation Type:** Length Validation (Minimum Boundary)

**Rule Description:**
Bank ID must be greater than 3 characters in length to ensure meaningful identifiers and avoid conflicts with reserved keywords or overly short identifiers.

**Validation Logic:**

- **Condition:** When a bank ID is provided in the POST /banks request body
- **Check:** Validate that bank.id.length > 3
- **Valid Criteria:** 
  - Bank ID length is 4 characters or more
- **Invalid Criteria:**
  - Bank ID length is 3 characters or less
- **Action on Success:** Continue with subsequent validations
- **Action on Failure:** Return error response with 400 status code

**Error Handling:**

- **Error Message:** `OBP-10001: Incorrect json format. Min length of BANK_ID should be greater than 3 characters.`
- **Error Code:** `OBP-10001`
- **HTTP Status Code:** `400 Bad Request`

**Scala Implementation:**

- **Location:** `code.api.v4_0_0.APIMethods400`
- **Method/Function:** `createBank` endpoint
- **Line Reference:** Lines 3639-3641

**Code Snippet:**
```scala
_ <- Helper.booleanToFuture(failMsg = s"$InvalidJsonFormat Min length of BANK_ID should be greater than 3 characters.", cc=cc.callContext) {
  bank.id.length > 3
}
```

**Related Entities:**
- Bank (id field)
- PostBankJson400 (request DTO)

**User Story Context:**
This validation prevents the creation of banks with overly short identifiers that could cause confusion or conflicts. It ensures bank IDs are meaningful and distinguishable from system-reserved short codes.

**Dependencies:**
- Must pass VR-001 (Bank ID Format Validation) first

---

### Rule VR-003: Bank ID Space Character Restriction

**Field/Entity:** bank.id (Bank identifier)

**Validation Type:** Format Validation (Character Restriction)

**Rule Description:**
Bank ID cannot contain space characters to maintain URL-safe identifiers and prevent issues with URL encoding and database queries.

**Validation Logic:**

- **Condition:** When a bank ID is provided in the POST /banks request body
- **Check:** Validate that bank.id does not contain any space characters
- **Valid Criteria:** 
  - Bank ID contains no space characters (ASCII 32)
- **Invalid Criteria:**
  - Bank ID contains one or more space characters
- **Action on Success:** Continue with subsequent validations
- **Action on Failure:** Return error response with 400 status code

**Error Handling:**

- **Error Message:** `OBP-10001: Incorrect json format. BANK_ID can not contain space characters`
- **Error Code:** `OBP-10001`
- **HTTP Status Code:** `400 Bad Request`

**Scala Implementation:**

- **Location:** `code.api.v4_0_0.APIMethods400`
- **Method/Function:** `createBank` endpoint
- **Line Reference:** Lines 3643-3645

**Code Snippet:**
```scala
_ <- Helper.booleanToFuture(failMsg = s"$InvalidJsonFormat BANK_ID can not contain space characters", cc=cc.callContext) {
  !bank.id.contains(" ")
}
```

**Related Entities:**
- Bank (id field)
- PostBankJson400 (request DTO)

**User Story Context:**
This validation ensures that bank IDs are URL-safe and can be used directly in REST API paths without requiring URL encoding. It prevents issues with routing and database queries that might be sensitive to whitespace.

**Dependencies:**
- Must pass VR-001 (Bank ID Format Validation) first
- Must pass VR-002 (Bank ID Minimum Length Validation) first

---

### Rule VR-004: Bank ID Special Character Sequence Restriction

**Field/Entity:** bank.id (Bank identifier)

**Validation Type:** Format Validation (Character Sequence Restriction)

**Rule Description:**
Bank ID cannot contain the character sequence `::::` which is reserved for internal delimiter usage in the OBP system for separating composite identifiers.

**Validation Logic:**

- **Condition:** When a bank ID is provided in the POST /banks request body
- **Check:** Validate that bank.id does not contain the substring `::::`
- **Valid Criteria:** 
  - Bank ID does not contain the four-colon sequence `::::`
- **Invalid Criteria:**
  - Bank ID contains the substring `::::`
- **Action on Success:** Continue with bank creation/update operation
- **Action on Failure:** Return error response with 400 status code

**Error Handling:**

- **Error Message:** `OBP-10001: Incorrect json format. BANK_ID can not contain '::::' characters`
- **Error Code:** `OBP-10001`
- **HTTP Status Code:** `400 Bad Request`

**Scala Implementation:**

- **Location:** `code.api.v4_0_0.APIMethods400`
- **Method/Function:** `createBank` endpoint
- **Line Reference:** Lines 3647-3649

**Code Snippet:**
```scala
_ <- Helper.booleanToFuture(failMsg = s"$InvalidJsonFormat BANK_ID can not contain `::::` characters", cc=cc.callContext) {
  !`checkIfContains::::` (bank.id)
}
```

**Related Entities:**
- Bank (id field)
- PostBankJson400 (request DTO)

**User Story Context:**
This validation prevents conflicts with the OBP system's internal use of `::::` as a delimiter for composite keys and identifiers. The four-colon sequence is used in various parts of the system to separate components of compound identifiers.

**Dependencies:**
- Must pass VR-001 (Bank ID Format Validation) first
- Must pass VR-002 (Bank ID Minimum Length Validation) first
- Must pass VR-003 (Bank ID Space Character Restriction) first

---

## Category: JSON Format Validation

### Rule VR-005: Request Body JSON Structure Validation

**Field/Entity:** Request body (entire JSON payload)

**Validation Type:** Input Validation (JSON Structure)

**Rule Description:**
The request body must be valid JSON that can be deserialized into the PostBankJson400 case class structure with all required fields present and correctly typed.

**Validation Logic:**

- **Condition:** When a POST request is made to /banks endpoint
- **Check:** Attempt to extract/deserialize JSON into PostBankJson400 case class
- **Valid Criteria:** 
  - Valid JSON syntax
  - Contains all required fields: id, short_name, full_name, logo, website, bank_routings
  - All fields have correct data types (String for most, List[BankRoutingJsonV121] for bank_routings)
  - bank_routings is a valid array of objects with scheme and address fields
- **Invalid Criteria:**
  - Malformed JSON syntax
  - Missing required fields
  - Incorrect data types
  - Invalid structure
- **Action on Success:** Extract bank object and proceed with validations
- **Action on Failure:** Return error response with 400 status code

**Error Handling:**

- **Error Message:** `OBP-10001: Incorrect json format. The Json body should be the BankJson400`
- **Error Code:** `OBP-10001`
- **HTTP Status Code:** `400 Bad Request`

**Scala Implementation:**

- **Location:** `code.api.v4_0_0.APIMethods400`
- **Method/Function:** `createBank` endpoint
- **Line Reference:** Lines 3624-3628

**Code Snippet:**
```scala
val failMsg = s"$InvalidJsonFormat The Json body should be the $BankJson400 "
for {
  bank <- NewStyle.function.tryons(failMsg, 400, cc.callContext) {
    json.extract[BankJson400]
  }
  // ... rest of validation
}
```

**Related Entities:**
- PostBankJson400 case class (request structure)
- BankRoutingJsonV121 case class (nested structure)

**User Story Context:**
This is the first validation performed on the request. It ensures that the client has sent a properly formatted JSON payload that matches the expected structure for bank creation. This prevents processing of malformed requests and provides clear error messages to API consumers.

**Dependencies:**
- None (first validation in the chain)

---

### Rule VR-006: Bank Routing Scheme Format Validation

**Field/Entity:** bank.bank_routings (List of routing schemes)

**Validation Type:** Format Validation (Nested Object Structure)

**Rule Description:**
Bank routing information must be provided as a list of objects, each containing a scheme and address field. The system extracts BIC routing separately and stores other routing schemes in generic fields.

**Validation Logic:**

- **Condition:** When bank_routings is provided in the POST /banks request body
- **Check:** Validate that bank_routings is a valid array of objects with scheme and address fields
- **Valid Criteria:** 
  - bank_routings is a valid JSON array
  - Each element has "scheme" and "address" fields (both strings)
  - Can be empty array
- **Invalid Criteria:**
  - bank_routings is not an array
  - Elements missing scheme or address fields
  - Invalid data types for scheme or address
- **Action on Success:** Extract BIC routing (if present) and other routings for storage
- **Action on Failure:** JSON parsing fails at VR-005 level

**Error Handling:**

- **Error Message:** Handled by JSON parsing error in VR-005
- **Error Code:** `OBP-10001`
- **HTTP Status Code:** `400 Bad Request`

**Scala Implementation:**

- **Location:** `code.api.v4_0_0.APIMethods400`
- **Method/Function:** `createBank` endpoint
- **Line Reference:** Lines 3657-3660

**Code Snippet:**
```scala
bank.bank_routings.find(_.scheme == "BIC").map(_.address).getOrElse(""),
"",
bank.bank_routings.filterNot(_.scheme == "BIC").headOption.map(_.scheme).getOrElse(""),
bank.bank_routings.filterNot(_.scheme == "BIC").headOption.map(_.address).getOrElse(""),
```

**Related Entities:**
- BankRoutingJsonV121 case class (scheme, address fields)
- MappedBank (swiftBIC, mBankRoutingScheme, mBankRoutingAddress fields)

**User Story Context:**
This validation ensures that multiple routing schemes can be provided for a bank (BIC/SWIFT, national identifiers, custom schemes). The system handles BIC specially by storing it in a dedicated field, while other routing schemes are stored in generic routing fields. Only the first non-BIC routing is stored.

**Dependencies:**
- Must pass VR-005 (Request Body JSON Structure Validation) first

---

## Category: Authentication and Authorization

### Rule VR-007: Consumer Authentication Validation

**Field/Entity:** OAuth Consumer credentials

**Validation Type:** Authentication Validation

**Rule Description:**
Bank creation requires an authenticated OAuth consumer (application), not just a user. This ensures proper API client tracking, rate limiting, and accountability for bank creation operations.

**Validation Logic:**

- **Condition:** When a POST request is made to /banks endpoint
- **Check:** Verify that the request includes valid OAuth consumer credentials and that cc.callContext.consumer is defined
- **Valid Criteria:** 
  - Request includes valid OAuth 1.0a or OAuth 2.0 consumer credentials
  - Consumer is registered in the system
  - Consumer is active (not disabled)
  - cc.callContext.consumer.isDefined == true
- **Invalid Criteria:**
  - No consumer credentials provided
  - Invalid consumer key/secret
  - Consumer is disabled
  - Consumer not found in system
- **Action on Success:** Continue with authorization checks
- **Action on Failure:** Return error response with 401 status code

**Error Handling:**

- **Error Message:** `OBP-20009: Invalid consumer credentials`
- **Error Code:** `OBP-20009`
- **HTTP Status Code:** `401 Unauthorized`

**Scala Implementation:**

- **Location:** `code.api.v4_0_0.APIMethods400`
- **Method/Function:** `createBank` endpoint
- **Line Reference:** Lines 3629-3631

**Code Snippet:**
```scala
_ <- Helper.booleanToFuture(failMsg = ErrorMessages.InvalidConsumerCredentials, cc=cc.callContext) {
  cc.callContext.map(_.consumer.isDefined == true).isDefined
}
```

**Related Entities:**
- Consumer (OAuth application credentials)
- CallContext (request context with consumer information)

**User Story Context:**
This validation enforces that bank creation is performed by registered applications (OAuth consumers) rather than just authenticated users. This enables proper tracking of which applications are creating banks, supports rate limiting per application, and ensures accountability in multi-tenant environments.

**Dependencies:**
- Must pass VR-005 (Request Body JSON Structure Validation) first

---

### Rule VR-008: User Authentication Validation

**Field/Entity:** User credentials

**Validation Type:** Authentication Validation

**Rule Description:**
The request must be made by an authenticated user. The user must have valid authentication credentials (OAuth token, Direct Login token, or other supported authentication method).

**Validation Logic:**

- **Condition:** When a POST request is made to /banks endpoint
- **Check:** Verify that a valid user is authenticated in the call context
- **Valid Criteria:** 
  - User is authenticated via OAuth 1.0a, OAuth 2.0, Direct Login, or Gateway Login
  - User exists in the system
  - User is not deleted
  - Authentication token is valid and not expired
- **Invalid Criteria:**
  - No authentication credentials provided
  - Invalid or expired authentication token
  - User not found
  - User is deleted
- **Action on Success:** Extract user ID and proceed with authorization checks
- **Action on Failure:** Return error response with 401 status code

**Error Handling:**

- **Error Message:** `OBP-20001: User not logged in. Authentication is required!`
- **Error Code:** `OBP-20001`
- **HTTP Status Code:** `401 Unauthorized`

**Scala Implementation:**

- **Location:** `code.api.OBPRestHelper` (authentication layer)
- **Method/Function:** Authentication flow in OBPRestHelper trait
- **Line Reference:** N/A (handled by framework before endpoint execution)

**Code Snippet:**
```scala
// Authentication is handled by OBPRestHelper before endpoint execution
// User is available in cc.userId if authentication succeeds
```

**Related Entities:**
- ResourceUser (user profile)
- AuthUser (authentication credentials)
- CallContext (contains authenticated user information)

**User Story Context:**
This validation ensures that only authenticated users can create banks. It's a prerequisite for the authorization check (VR-009) which verifies the user has the CanCreateBank entitlement. Authentication happens at the framework level before the endpoint logic executes.

**Dependencies:**
- None (framework-level validation before endpoint execution)

---

### Rule VR-009: User Authorization Validation (CanCreateBank Entitlement)

**Field/Entity:** User entitlements

**Validation Type:** Authorization Validation (Role-Based Access Control)

**Rule Description:**
The authenticated user must have the CanCreateBank entitlement to create or update banks. This role-based access control prevents unauthorized bank registration and ensures only privileged users can create banks.

**Validation Logic:**

- **Condition:** When a POST request is made to /banks endpoint (implicit check via ResourceDoc)
- **Check:** Verify that the authenticated user has the CanCreateBank role/entitlement
- **Valid Criteria:** 
  - User has CanCreateBank entitlement (either system-wide or for specific banks)
  - Entitlement is active
- **Invalid Criteria:**
  - User does not have CanCreateBank entitlement
  - User's entitlement has been revoked
- **Action on Success:** Allow bank creation/update to proceed
- **Action on Failure:** Return error response with 403 status code

**Error Handling:**

- **Error Message:** `OBP-20006: User is missing one or more roles: CanCreateBank` or similar authorization error
- **Error Code:** `OBP-20006`
- **HTTP Status Code:** `403 Forbidden`

**Scala Implementation:**

- **Location:** `code.api.v4_0_0.APIMethods400` (ResourceDoc definition)
- **Method/Function:** `createBank` ResourceDoc with roles specification
- **Line Reference:** Lines 3591-3620 (ResourceDoc definition)

**Code Snippet:**
```scala
staticResourceDocs += ResourceDoc(
  createBank,
  implementedInApiVersion,
  "createBank",
  "POST",
  "/banks",
  "Create Bank",
  // ... description ...
  postBankJson400,
  bankJson400,
  List(
    $UserNotLoggedIn,
    InvalidJsonFormat,
    InvalidConsumerCredentials,
    $UserHasMissingRoles,
    UnknownError
  ),
  List(apiTagBank),
  Some(List(canCreateBank))  // Required role
)
```

**Related Entities:**
- Entitlement (user role assignments)
- ApiRole (role definitions including CanCreateBank)
- ResourceUser (user with entitlements)

**User Story Context:**
This validation implements role-based access control for bank creation. Only users who have been granted the CanCreateBank entitlement by system administrators can create new banks. This prevents unauthorized bank registration and ensures proper governance of the banking platform.

**Dependencies:**
- Must pass VR-008 (User Authentication Validation) first
- Must pass VR-007 (Consumer Authentication Validation) first

---

## Category: Business Constraint Validation

### Rule VR-010: Automatic Entitlement Grant for Bank Creator

**Field/Entity:** User entitlements for newly created bank

**Validation Type:** Business Rule (Automatic Permission Assignment)

**Rule Description:**
Upon successful bank creation, the system automatically grants the creating user two entitlements for the newly created bank: CanCreateEntitlementAtOneBank and CanReadDynamicResourceDocsAtOneBank. This enables self-service bank management without requiring system administrator intervention.

**Validation Logic:**

- **Condition:** After successful bank creation or update
- **Check:** Check if user already has CanCreateEntitlementAtOneBank and CanReadDynamicResourceDocsAtOneBank for this bank
- **Valid Criteria:** 
  - Bank creation/update succeeds
  - Entitlement system is available
- **Invalid Criteria:**
  - Entitlement system is unavailable
  - Database error during entitlement creation
- **Action on Success:** Grant entitlements if not already present
- **Action on Failure:** Bank is created but entitlements may not be granted (non-blocking)

**Error Handling:**

- **Error Message:** No explicit error (operation continues even if entitlement grant fails)
- **Error Code:** N/A
- **HTTP Status Code:** 201 (bank creation succeeds regardless)

**Scala Implementation:**

- **Location:** `code.api.v4_0_0.APIMethods400`
- **Method/Function:** `createBank` endpoint
- **Line Reference:** Lines 3663-3678

**Code Snippet:**
```scala
entitlements <- NewStyle.function.getEntitlementsByUserId(cc.userId, callContext)
entitlementsByBank = entitlements.filter(_.bankId==bank.id)
_ <- entitlementsByBank.filter(_.roleName == CanCreateEntitlementAtOneBank.toString()).size > 0 match {
  case true =>
    // Already has entitlement
    Future()
  case false =>
    Future(Entitlement.entitlement.vend.addEntitlement(bank.id, cc.userId, CanCreateEntitlementAtOneBank.toString()))
}
_ <- entitlementsByBank.filter(_.roleName == CanReadDynamicResourceDocsAtOneBank.toString()).size > 0 match {
  case true =>
    // Already has entitlement
    Future()
  case false =>
    Future(Entitlement.entitlement.vend.addEntitlement(bank.id, cc.userId, CanReadDynamicResourceDocsAtOneBank.toString()))
}
```

**Related Entities:**
- Entitlement (role assignments)
- ApiRole (CanCreateEntitlementAtOneBank, CanReadDynamicResourceDocsAtOneBank)
- Bank (newly created bank)

**User Story Context:**
This business rule enables a self-service model where bank creators can manage their own banks without requiring system administrator intervention for every permission. The CanCreateEntitlementAtOneBank role allows them to grant permissions to other users for their bank, and CanReadDynamicResourceDocsAtOneBank allows them to access API documentation for their bank.

**Dependencies:**
- Must complete bank creation successfully first
- Requires VR-009 (User Authorization Validation) to pass

---

### Rule VR-011: Idempotent Bank Creation/Update Operation

**Field/Entity:** Bank entity

**Validation Type:** Business Rule (Idempotency)

**Rule Description:**
The bank creation operation is idempotent - calling it multiple times with the same bank ID updates the existing record rather than failing or creating duplicates. This design supports retry logic in distributed systems and allows configuration updates without separate update endpoints.

**Validation Logic:**

- **Condition:** When a POST request is made to /banks endpoint
- **Check:** Check if a bank with the given ID already exists
- **Valid Criteria:** 
  - If bank exists: Update existing bank record with new values
  - If bank doesn't exist: Create new bank record
  - Operation succeeds in both cases
- **Invalid Criteria:**
  - Database error during check or update/insert
  - Connector error
- **Action on Success:** Return HTTP 201 with bank details (same for create and update)
- **Action on Failure:** Return error response with 500 status code

**Error Handling:**

- **Error Message:** "Create bank error" or "Update bank error" (from connector layer)
- **Error Code:** N/A (connector-specific)
- **HTTP Status Code:** `500 Internal Server Error`

**Scala Implementation:**

- **Location:** `code.api.util.NewStyle` and connector implementations
- **Method/Function:** `createOrUpdateBank()` in NewStyle and Connector trait
- **Line Reference:** Lines 3651-3662 in APIMethods400.scala

**Code Snippet:**
```scala
(success, callContext) <- NewStyle.function.createOrUpdateBank(
  bank.id,
  bank.full_name,
  bank.short_name,
  bank.logo,
  bank.website,
  bank.bank_routings.find(_.scheme == "BIC").map(_.address).getOrElse(""),
  "",
  bank.bank_routings.filterNot(_.scheme == "BIC").headOption.map(_.scheme).getOrElse(""),
  bank.bank_routings.filterNot(_.scheme == "BIC").headOption.map(_.address).getOrElse(""),
  cc.callContext
)
```

**Related Entities:**
- Bank (domain entity)
- MappedBank (database entity)
- Connector (backend abstraction)

**User Story Context:**
This business rule makes the bank creation API idempotent, which is important for distributed systems where requests might be retried due to network issues or timeouts. It also simplifies the API by allowing the same endpoint to be used for both creation and updates, reducing the number of endpoints clients need to understand.

**Dependencies:**
- Must pass all validation rules (VR-001 through VR-009) first

---

## Category: URL and String Validation

### Rule VR-012: Bank Logo URL Validation

**Field/Entity:** bank.logo (Bank logo URL)

**Validation Type:** Input Validation (String)

**Rule Description:**
Bank logo URL is accepted as a string field. The system does not validate that the URL is accessible or returns a valid image, but it must be a valid string that can be stored in the database.

**Validation Logic:**

- **Condition:** When bank.logo is provided in the POST /banks request body
- **Check:** Accept any string value (including empty string)
- **Valid Criteria:** 
  - Any string value
  - Can be empty string
  - No format validation performed
- **Invalid Criteria:**
  - None (any string is accepted)
- **Action on Success:** Store logo URL in database
- **Action on Failure:** N/A (no validation failure possible)

**Error Handling:**

- **Error Message:** N/A (no validation performed)
- **Error Code:** N/A
- **HTTP Status Code:** N/A

**Scala Implementation:**

- **Location:** `code.api.v4_0_0.APIMethods400`
- **Method/Function:** `createBank` endpoint
- **Line Reference:** Line 3655

**Code Snippet:**
```scala
(success, callContext) <- NewStyle.function.createOrUpdateBank(
  bank.id,
  bank.full_name,
  bank.short_name,
  bank.logo,  // Passed directly without validation
  bank.website,
  // ... other parameters
)
```

**Related Entities:**
- Bank (logo field)
- MappedBank (logoURL field)
- PostBankJson400 (logo field in request)

**User Story Context:**
The logo URL is stored as-is without validation. This allows flexibility for banks to provide various types of URLs (HTTP, HTTPS, data URLs, etc.) but means the system does not verify that the URL is accessible or returns a valid image. This is a design decision that prioritizes flexibility over strict validation.

**Dependencies:**
- Must pass VR-005 (Request Body JSON Structure Validation) first

---

### Rule VR-013: Bank Website URL Validation

**Field/Entity:** bank.website (Bank website URL)

**Validation Type:** Input Validation (String)

**Rule Description:**
Bank website URL is accepted as a string field. The system does not validate that the URL is accessible or properly formatted, but it must be a valid string that can be stored in the database.

**Validation Logic:**

- **Condition:** When bank.website is provided in the POST /banks request body
- **Check:** Accept any string value (including empty string)
- **Valid Criteria:** 
  - Any string value
  - Can be empty string
  - No format validation performed
- **Invalid Criteria:**
  - None (any string is accepted)
- **Action on Success:** Store website URL in database
- **Action on Failure:** N/A (no validation failure possible)

**Error Handling:**

- **Error Message:** N/A (no validation performed)
- **Error Code:** N/A
- **HTTP Status Code:** N/A

**Scala Implementation:**

- **Location:** `code.api.v4_0_0.APIMethods400`
- **Method/Function:** `createBank` endpoint
- **Line Reference:** Line 3656

**Code Snippet:**
```scala
(success, callContext) <- NewStyle.function.createOrUpdateBank(
  bank.id,
  bank.full_name,
  bank.short_name,
  bank.logo,
  bank.website,  // Passed directly without validation
  // ... other parameters
)
```

**Related Entities:**
- Bank (website field)
- MappedBank (websiteURL field)
- PostBankJson400 (website field in request)

**User Story Context:**
The website URL is stored as-is without validation. This allows flexibility for banks to provide various types of URLs but means the system does not verify that the URL is accessible or properly formatted. This is a design decision that prioritizes flexibility over strict validation.

**Dependencies:**
- Must pass VR-005 (Request Body JSON Structure Validation) first

---

## Category: String Field Validation

### Rule VR-014: Bank Full Name Validation

**Field/Entity:** bank.full_name (Bank full legal name)

**Validation Type:** Input Validation (String)

**Rule Description:**
Bank full name is accepted as a string field without specific format validation. It represents the full legal name of the banking institution.

**Validation Logic:**

- **Condition:** When bank.full_name is provided in the POST /banks request body
- **Check:** Accept any string value
- **Valid Criteria:** 
  - Any non-null string value
  - Can contain any characters including spaces, special characters, unicode
  - No length restrictions enforced at API level
- **Invalid Criteria:**
  - Null value (would fail JSON parsing at VR-005)
- **Action on Success:** Store full name in database
- **Action on Failure:** JSON parsing fails at VR-005 level

**Error Handling:**

- **Error Message:** Handled by JSON parsing error in VR-005 if missing
- **Error Code:** `OBP-10001`
- **HTTP Status Code:** `400 Bad Request`

**Scala Implementation:**

- **Location:** `code.api.v4_0_0.APIMethods400`
- **Method/Function:** `createBank` endpoint
- **Line Reference:** Line 3653

**Code Snippet:**
```scala
(success, callContext) <- NewStyle.function.createOrUpdateBank(
  bank.id,
  bank.full_name,  // Passed directly without validation
  bank.short_name,
  // ... other parameters
)
```

**Related Entities:**
- Bank (fullName field)
- MappedBank (fullBankName field)
- PostBankJson400 (full_name field in request)

**User Story Context:**
The full bank name is stored without format restrictions to accommodate various naming conventions across different countries and banking institutions. This allows for maximum flexibility in representing official bank names, including those with special characters, multiple languages, or complex legal structures.

**Dependencies:**
- Must pass VR-005 (Request Body JSON Structure Validation) first

---

### Rule VR-015: Bank Short Name Validation

**Field/Entity:** bank.short_name (Bank abbreviated display name)

**Validation Type:** Input Validation (String)

**Rule Description:**
Bank short name is accepted as a string field without specific format validation. It represents an abbreviated or display name for the banking institution.

**Validation Logic:**

- **Condition:** When bank.short_name is provided in the POST /banks request body
- **Check:** Accept any string value
- **Valid Criteria:** 
  - Any non-null string value
  - Can contain any characters including spaces, special characters, unicode
  - No length restrictions enforced at API level
- **Invalid Criteria:**
  - Null value (would fail JSON parsing at VR-005)
- **Action on Success:** Store short name in database
- **Action on Failure:** JSON parsing fails at VR-005 level

**Error Handling:**

- **Error Message:** Handled by JSON parsing error in VR-005 if missing
- **Error Code:** `OBP-10001`
- **HTTP Status Code:** `400 Bad Request`

**Scala Implementation:**

- **Location:** `code.api.v4_0_0.APIMethods400`
- **Method/Function:** `createBank` endpoint
- **Line Reference:** Line 3654

**Code Snippet:**
```scala
(success, callContext) <- NewStyle.function.createOrUpdateBank(
  bank.id,
  bank.full_name,
  bank.short_name,  // Passed directly without validation
  // ... other parameters
)
```

**Related Entities:**
- Bank (shortName field)
- MappedBank (shortBankName field)
- PostBankJson400 (short_name field in request)

**User Story Context:**
The short bank name is stored without format restrictions to accommodate various naming conventions and display preferences. This field is typically used in UI displays where space is limited, but the system does not enforce any specific length or format constraints.

**Dependencies:**
- Must pass VR-005 (Request Body JSON Structure Validation) first

---

## Additional Validation Context

### Validation Execution Order

The validations are executed in the following order in the createBank endpoint:

1. **VR-005**: Request Body JSON Structure Validation (JSON parsing)
2. **VR-007**: Consumer Authentication Validation
3. **VR-001**: Bank ID Format Validation (checkShortString)
4. **VR-002**: Bank ID Minimum Length Validation
5. **VR-003**: Bank ID Space Character Restriction
6. **VR-004**: Bank ID Special Character Sequence Restriction
7. **VR-011**: Idempotent Bank Creation/Update Operation (createOrUpdateBank)
8. **VR-010**: Automatic Entitlement Grant for Bank Creator

### Implicit Validations

The following validations are performed implicitly by the framework before the endpoint logic executes:

- **VR-008**: User Authentication Validation (handled by OBPRestHelper)
- **VR-009**: User Authorization Validation (handled by ResourceDoc role checking)

### Non-Validated Fields

The following fields are accepted without validation:
- **VR-012**: bank.logo (any string)
- **VR-013**: bank.website (any string)
- **VR-014**: bank.full_name (any string)
- **VR-015**: bank.short_name (any string)
- **VR-006**: bank.bank_routings (validated only for JSON structure, not content)

### Error Response Format

All validation errors return JSON responses in the following format:

```json
{
  "code": 400,
  "message": "OBP-XXXXX: Error message description"
}
```

For authentication/authorization errors:
```json
{
  "code": 401,  // or 403
  "message": "OBP-2XXXX: Error message description"
}
```

### Database Constraints

Additional validation may occur at the database level:
- Bank ID uniqueness (handled by createOrUpdateBank logic)
- Field length constraints (database column sizes)
- Foreign key constraints (for related entities)

### Connector-Level Validations

The connector layer (LocalMappedConnector, RestConnector, etc.) may perform additional validations:
- Database connection availability
- Backend system availability
- Data persistence validation

These are not documented here as they are implementation-specific and not part of the API contract.

---

## Summary of Validation Coverage

### Covered Validation Areas

✓ Bank ID format and character restrictions  
✓ Bank ID length constraints  
✓ JSON structure and format  
✓ Authentication (user and consumer)  
✓ Authorization (CanCreateBank entitlement)  
✓ Business rules (idempotency, auto-entitlements)  
✓ Bank routing structure  

### Not Validated (By Design)

✗ Bank logo URL accessibility or format  
✗ Bank website URL accessibility or format  
✗ Bank full name format or length  
✗ Bank short name format or length  
✗ Bank routing scheme values (any string accepted)  
✗ Bank routing address format (any string accepted)  
✗ BIC/SWIFT code format validation  
✗ National identifier format validation  

### Recommendations for Additional Validations

Based on the user story analysis, the following additional validations could be considered:

1. **URL Format Validation**: Validate that logo and website fields contain valid URLs
2. **BIC/SWIFT Format Validation**: Validate BIC codes against ISO 9362 standard
3. **Name Length Validation**: Enforce reasonable length limits for full_name and short_name
4. **Routing Scheme Validation**: Validate known routing schemes (BIC, IBAN, etc.) against their respective formats
5. **Duplicate Bank Name Check**: Warn or prevent creation of banks with duplicate names
6. **Bank Attribute Validation**: Validate custom bank attributes if provided

These are not currently implemented in the codebase but could enhance data quality and prevent common errors.

---

## Glossary

**SILENCE_IS_GOLDEN**: Constant string returned by validation functions when validation succeeds (no error)

**Box[T]**: Lift framework container type that can be Full(value), Empty, or Failure(message, exception, chain)

**Future[T]**: Scala asynchronous computation type

**CallContext**: Request context object containing user, consumer, headers, and other request metadata

**NewStyle.function**: Utility object providing standardized patterns for common operations

**Helper.booleanToFuture**: Utility function that converts boolean validation checks to Future for error handling

**OBPEndpoint**: Type alias for PartialFunction[Req, JsonResponse] representing a single API route

**ResourceDoc**: Case class documenting an API endpoint with metadata for Swagger generation

**Entitlement**: Role-based permission granting system or bank-level access

**Connector**: Backend abstraction layer for banking system integration

---

## Migration Notes for Go Implementation

When migrating these validation rules to Go, consider the following:

1. **Regex Patterns**: All regex patterns should be compiled once and reused (use `regexp.MustCompile`)

2. **Error Codes**: Maintain the same OBP-XXXXX error code format for consistency

3. **Validation Order**: Preserve the validation execution order to maintain consistent error reporting

4. **Idempotency**: Implement the same create-or-update logic to maintain API compatibility

5. **Authentication**: Implement equivalent OAuth consumer and user authentication middleware

6. **Authorization**: Implement role-based access control with the same entitlement model

7. **Error Responses**: Use the same JSON error response format for client compatibility

8. **String Validation**: Implement equivalent string validation functions (checkShortString, etc.)

9. **Business Rules**: Implement the automatic entitlement grant logic after bank creation

10. **Database Operations**: Use transactions to ensure atomicity of bank creation and entitlement grants

---

**End of Validation Rules Document**
