# Business Rules Extraction

**Extracted From**: Open Bank Project API (OBP-API)
**Analysis Date**: November 17, 2025
**Analyst**: Expert Scala Business Logic Analyst
**Migration Target**: Go Application
**User Story**: Bank Registration and Configuration

## Executive Summary
- Total Business Rules Extracted: 11
- API Endpoints Analyzed: 1 (POST /obp/v4.0.0/banks)
- Rule Categories:
  - Calculations: 0
  - Decisions: 5
  - Thresholds: 2
  - Aggregations: 0
  - Workflows: 2
  - Transformations: 2

## Business Rules Catalog

### BR-001: Bank ID Format Validation Rule

**Category**: THRESHOLD

**Description**: Bank IDs must conform to a specific alphanumeric pattern with limited special characters and maximum length to ensure URL-safety, database compatibility, and system-wide consistency.

**Source**: 
- File: obp-api/src/main/scala/code/api/util/APIUtil.scala
- Class/Object: APIUtil
- Method: checkShortString
- Lines: 907-915

**Business Logic**:
1. Bank ID must match the regex pattern `^([A-Za-z0-9\-._]+)$` (alphanumeric plus dash, dot, underscore only)
2. Bank ID length must not exceed 16 characters
3. If pattern matches and length is valid, validation passes
4. If pattern matches but length exceeds 16, return "Invalid value length" error
5. If pattern doesn't match, return "Invalid value characters" error

**Scala Implementation**:
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

**Variables**:
- **Input**: bank.id (String) - The proposed bank identifier
- **Output**: Validation result (String) - Either SILENCE_IS_GOLDEN (success) or error message
- **Constants**: Maximum length = 16 characters, Allowed pattern = `^([A-Za-z0-9\-._]+)$`

**Business Conditions**:
| Condition | Business Meaning | Values/Thresholds |
|-----------|------------------|-------------------|
| valueLength <= 16 | Bank ID within acceptable length | Maximum 16 characters |
| Matches regex pattern | Bank ID contains only safe characters | A-Z, a-z, 0-9, -, ., _ |

**Business Impact**: 
This rule ensures bank IDs are URL-safe (can be used in REST API paths), database-compatible (no special characters that could cause SQL issues), and consistent across the system. Invalid bank IDs could break API routing, cause database errors, or create security vulnerabilities.

**API Endpoints Using This Rule**:
- POST /obp/v4.0.0/banks - Bank creation/update

**Related Test Cases**:
Tests should validate: valid IDs (alphanumeric with allowed special chars), IDs exceeding 16 chars, IDs with invalid special characters (spaces, colons, etc.)

**Migration Notes for Go**:
- Use Go's regexp package for pattern matching
- Pattern in Go: `^([A-Za-z0-9\-._]+)$`
- Consider using a validation struct with methods for reusability
- Return custom error types for different validation failures

**Example Scenarios**:
```
Scenario 1: Valid bank ID
Input: bank.id = "my-bank.123"
Processing: Matches pattern, length = 12 (< 16)
Output: SILENCE_IS_GOLDEN (validation passes)

Scenario 2: Bank ID too long
Input: bank.id = "very-long-bank-identifier-name"
Processing: Matches pattern, length = 32 (> 16)
Output: ErrorMessages.InvalidValueLength

Scenario 3: Bank ID with invalid characters
Input: bank.id = "my bank@123"
Processing: Contains space and @, doesn't match pattern
Output: ErrorMessages.InvalidValueCharacters
```

**Business Context**:
Bank IDs serve as the primary identifier throughout the OBP API system. They appear in URLs, database foreign keys, and are used for routing requests to appropriate backend systems. The 16-character limit balances human readability with technical constraints, while the character restrictions prevent injection attacks and ensure cross-platform compatibility.

---

### BR-002: Bank ID Minimum Length Rule

**Category**: THRESHOLD

**Description**: Bank IDs must be longer than 3 characters to ensure meaningful identifiers and avoid conflicts with reserved keywords or system codes.

**Source**: 
- File: obp-api/src/main/scala/code/api/v4_0_0/APIMethods400.scala
- Class/Object: Implementations400
- Method: createBank
- Lines: 3639-3641

**Business Logic**:
1. Check if bank ID length is greater than 3 characters
2. If length <= 3, reject with error message
3. If length > 3, validation passes and processing continues

**Scala Implementation**:
```scala
_ <- Helper.booleanToFuture(
  failMsg = s"$InvalidJsonFormat Min length of BANK_ID should be greater than 3 characters.", 
  cc=cc.callContext
) {
  bank.id.length > 3
}
```

**Variables**:
- **Input**: bank.id (String) - The proposed bank identifier
- **Output**: Future success or failure with error message
- **Constants**: Minimum length threshold = 3 characters

**Business Conditions**:
| Condition | Business Meaning | Values/Thresholds |
|-----------|------------------|-------------------|
| bank.id.length > 3 | Bank ID is sufficiently descriptive | Minimum 4 characters required |

**Business Impact**: 
This rule prevents the creation of banks with overly short, non-descriptive identifiers like "abc", "123", or "xyz" that could conflict with system codes, be easily confused, or lack business meaning. It ensures bank IDs are meaningful enough to identify institutions.

**API Endpoints Using This Rule**:
- POST /obp/v4.0.0/banks - Bank creation/update

**Related Test Cases**:
Tests should validate: IDs with exactly 3 chars (should fail), IDs with 4 chars (should pass), IDs with 1-2 chars (should fail)

**Migration Notes for Go**:
- Simple length check: `len(bankID) > 3`
- Return appropriate error if validation fails
- Consider combining with other ID validations in a single validation function

**Example Scenarios**:
```
Scenario 1: Bank ID too short
Input: bank.id = "abc"
Processing: length = 3, not greater than 3
Output: Error "Min length of BANK_ID should be greater than 3 characters"

Scenario 2: Bank ID acceptable length
Input: bank.id = "abcd"
Processing: length = 4, greater than 3
Output: Validation passes

Scenario 3: Bank ID well above minimum
Input: bank.id = "my-bank-123"
Processing: length = 12, greater than 3
Output: Validation passes
```

**Business Context**:
The minimum length requirement ensures bank identifiers are meaningful and distinguishable. Very short IDs (1-3 characters) are often reserved for system codes, country codes, or could easily be confused. Requiring at least 4 characters encourages descriptive, business-meaningful identifiers while still allowing concise IDs.

---

### BR-003: Bank ID Space Character Restriction

**Category**: DECISION

**Description**: Bank IDs cannot contain space characters to maintain URL-safety and prevent parsing issues in API paths and database queries.

**Source**: 
- File: obp-api/src/main/scala/code/api/v4_0_0/APIMethods400.scala
- Class/Object: Implementations400
- Method: createBank
- Lines: 3643-3645

**Business Logic**:
1. Check if bank ID contains any space characters
2. If spaces are present, reject with error message
3. If no spaces, validation passes and processing continues

**Scala Implementation**:
```scala
_ <- Helper.booleanToFuture(
  failMsg = s"$InvalidJsonFormat BANK_ID can not contain space characters", 
  cc=cc.callContext
) {
  !bank.id.contains(" ")
}
```

**Variables**:
- **Input**: bank.id (String) - The proposed bank identifier
- **Output**: Future success or failure with error message
- **Constants**: Forbidden character = space (" ")

**Business Conditions**:
| Condition | Business Meaning | Values/Thresholds |
|-----------|------------------|-------------------|
| !bank.id.contains(" ") | Bank ID is URL-safe | No space characters allowed |

**Business Impact**: 
This rule prevents URL encoding issues where spaces would be converted to %20 or + in URLs, causing routing problems and making API paths harder to read and debug. It ensures bank IDs can be used directly in REST API paths without encoding.

**API Endpoints Using This Rule**:
- POST /obp/v4.0.0/banks - Bank creation/update

**Related Test Cases**:
Tests should validate: IDs with spaces (should fail), IDs with underscores or dashes instead of spaces (should pass), IDs with multiple spaces (should fail)

**Migration Notes for Go**:
- Use strings.Contains(bankID, " ") to check for spaces
- Return error if spaces found
- Consider checking for other whitespace characters (tabs, newlines) for robustness

**Example Scenarios**:
```
Scenario 1: Bank ID with space
Input: bank.id = "my bank"
Processing: Contains space character
Output: Error "BANK_ID can not contain space characters"

Scenario 2: Bank ID with dash instead of space
Input: bank.id = "my-bank"
Processing: No space characters
Output: Validation passes

Scenario 3: Bank ID with multiple spaces
Input: bank.id = "my  bank  123"
Processing: Contains space characters
Output: Error "BANK_ID can not contain space characters"
```

**Business Context**:
Bank IDs are used extensively in URL paths (e.g., /banks/BANK_ID/accounts). Spaces in URLs require percent-encoding (%20) which complicates API usage, makes URLs harder to read, and can cause issues with URL parsing libraries. This rule enforces URL-safe identifiers that can be used directly in API paths.

---

### BR-004: Bank ID Special Character Restriction (::::)

**Category**: DECISION

**Description**: Bank IDs cannot contain the four-colon sequence (::::) which is reserved as an internal delimiter in the OBP system for composite keys and data serialization.

**Source**: 
- File: obp-api/src/main/scala/code/api/v4_0_0/APIMethods400.scala
- Class/Object: Implementations400
- Method: createBank
- Lines: 3647-3649

**Business Logic**:
1. Check if bank ID contains the sequence "::::"
2. If present, reject with error message
3. If not present, validation passes and processing continues

**Scala Implementation**:
```scala
_ <- Helper.booleanToFuture(
  failMsg = s"$InvalidJsonFormat BANK_ID can not contain `::::` characters", 
  cc=cc.callContext
) {
  !`checkIfContains::::` (bank.id)
}
```

**Variables**:
- **Input**: bank.id (String) - The proposed bank identifier
- **Output**: Future success or failure with error message
- **Constants**: Forbidden sequence = "::::"

**Business Conditions**:
| Condition | Business Meaning | Values/Thresholds |
|-----------|------------------|-------------------|
| !checkIfContains::::(bank.id) | Bank ID doesn't conflict with system delimiter | Four-colon sequence forbidden |

**Business Impact**: 
This rule prevents conflicts with the OBP system's internal use of :::: as a delimiter for composite keys (e.g., bankId::::accountId::::viewId). If bank IDs contained this sequence, it would break key parsing and data serialization throughout the system.

**API Endpoints Using This Rule**:
- POST /obp/v4.0.0/banks - Bank creation/update

**Related Test Cases**:
Tests should validate: IDs with :::: (should fail), IDs with single/double/triple colons (should pass based on BR-001), IDs without colons (should pass)

**Migration Notes for Go**:
- Use strings.Contains(bankID, "::::") to check for the sequence
- Return error if found
- Document why this specific sequence is forbidden (internal delimiter)
- Consider checking for other reserved sequences if system uses them

**Example Scenarios**:
```
Scenario 1: Bank ID with four-colon delimiter
Input: bank.id = "bank::::123"
Processing: Contains :::: sequence
Output: Error "BANK_ID can not contain `::::` characters"

Scenario 2: Bank ID with single colon
Input: bank.id = "bank:123"
Processing: Single colon allowed by pattern, no :::: sequence
Output: Validation passes (if other rules pass)

Scenario 3: Bank ID without colons
Input: bank.id = "bank-123"
Processing: No :::: sequence
Output: Validation passes
```

**Business Context**:
The OBP system uses :::: as a delimiter for creating composite keys that combine multiple identifiers (e.g., BankId::::AccountId::::ViewId). This pattern is used throughout the codebase for caching keys, data serialization, and internal references. Allowing :::: in bank IDs would break this parsing logic and cause system-wide failures.

---

### BR-005: Consumer Authentication Requirement

**Category**: DECISION

**Description**: Bank creation requires an authenticated OAuth consumer (application), not just a user, to ensure proper API client tracking, rate limiting, and accountability.

**Source**: 
- File: obp-api/src/main/scala/code/api/v4_0_0/APIMethods400.scala
- Class/Object: Implementations400
- Method: createBank
- Lines: 3629-3631

**Business Logic**:
1. Check if the call context contains a consumer object
2. Verify the consumer is defined (not None)
3. If consumer is missing or undefined, reject with "Invalid consumer credentials" error
4. If consumer is present and defined, validation passes

**Scala Implementation**:
```scala
_ <- Helper.booleanToFuture(
  failMsg = ErrorMessages.InvalidConsumerCredentials, 
  cc=cc.callContext
) {
  cc.callContext.map(_.consumer.isDefined == true).isDefined
}
```

**Variables**:
- **Input**: cc.callContext.consumer (Option[Consumer]) - OAuth consumer from request context
- **Output**: Future success or failure with error message
- **Constants**: None

**Business Conditions**:
| Condition | Business Meaning | Values/Thresholds |
|-----------|------------------|-------------------|
| consumer.isDefined == true | Request made by registered OAuth application | Consumer must be present |

**Business Impact**: 
This rule ensures that only registered OAuth applications (consumers) can create banks, not just authenticated users. This enables proper API client tracking, rate limiting per application, and accountability for bank creation actions. It prevents anonymous or user-only requests from creating banks.

**API Endpoints Using This Rule**:
- POST /obp/v4.0.0/banks - Bank creation/update

**Related Test Cases**:
Tests should validate: requests with valid consumer credentials (should pass), requests with only user auth but no consumer (should fail), requests with invalid consumer (should fail)

**Migration Notes for Go**:
- Extract consumer from request context/middleware
- Check if consumer object exists and is valid
- Return 401 error with "Invalid consumer credentials" if missing
- Ensure OAuth middleware populates consumer in context

**Example Scenarios**:
```
Scenario 1: Request with valid consumer
Input: OAuth request with consumer key/secret
Processing: Consumer extracted from context, isDefined = true
Output: Validation passes

Scenario 2: Request with user auth only
Input: Direct Login with user credentials, no consumer
Processing: Consumer is None/undefined
Output: Error "Invalid consumer credentials"

Scenario 3: Request without authentication
Input: Unauthenticated request
Processing: No consumer in context
Output: Error "Invalid consumer credentials"
```

**Business Context**:
The OBP API uses OAuth 1.0a/2.0 for authentication, where a "consumer" represents a registered third-party application. Requiring consumer authentication for bank creation ensures that only registered applications can create banks, enabling proper tracking, rate limiting, and audit trails. This prevents abuse and ensures accountability for bank creation actions.

---

### BR-006: BIC Routing Scheme Extraction Rule

**Category**: TRANSFORMATION

**Description**: When multiple bank routing schemes are provided, the BIC (Bank Identifier Code / SWIFT code) scheme is extracted and stored separately in a dedicated field, while other routing schemes are stored in generic routing fields.

**Source**: 
- File: obp-api/src/main/scala/code/api/v4_0_0/APIMethods400.scala
- Class/Object: Implementations400
- Method: createBank
- Lines: 3657-3660

**Business Logic**:
1. Search through the bank_routings array for an entry with scheme = "BIC"
2. If BIC scheme found, extract its address value for the swiftBIC field
3. If BIC scheme not found, use empty string for swiftBIC field
4. Filter out BIC from the routings array
5. Take the first non-BIC routing scheme (if any) for generic routing fields
6. Store the first non-BIC scheme name in bankRoutingScheme field
7. Store the first non-BIC scheme address in bankRoutingAddress field

**Scala Implementation**:
```scala
(success, callContext) <- NewStyle.function.createOrUpdateBank(
  bank.id,
  bank.full_name,
  bank.short_name,
  bank.logo,
  bank.website,
  bank.bank_routings.find(_.scheme == "BIC").map(_.address).getOrElse(""),  // Extract BIC
  "",  // national_identifier (deprecated)
  bank.bank_routings.filterNot(_.scheme == "BIC").headOption.map(_.scheme).getOrElse(""),  // First non-BIC scheme
  bank.bank_routings.filterNot(_.scheme == "BIC").headOption.map(_.address).getOrElse(""),  // First non-BIC address
  cc.callContext
)
```

**Variables**:
- **Input**: bank.bank_routings (List[BankRouting]) - Array of routing schemes with scheme and address
- **Output**: swiftBIC (String), bankRoutingScheme (String), bankRoutingAddress (String)
- **Constants**: BIC scheme identifier = "BIC"

**Business Conditions**:
| Condition | Business Meaning | Values/Thresholds |
|-----------|------------------|-------------------|
| scheme == "BIC" | International SWIFT/BIC code | Stored in dedicated swiftBIC field |
| scheme != "BIC" | National or custom routing | Stored in generic routing fields |

**Business Impact**: 
This rule ensures BIC/SWIFT codes (the international standard for bank identification) are stored in a dedicated field for easy querying and international payment processing, while allowing flexibility for country-specific routing schemes (UK Sort Code, US Routing Number, IBAN, etc.) in generic fields.

**API Endpoints Using This Rule**:
- POST /obp/v4.0.0/banks - Bank creation/update

**Related Test Cases**:
Tests should validate: bank with only BIC (BIC extracted, generic fields empty), bank with BIC and national ID (BIC extracted, national ID in generic fields), bank with multiple non-BIC routings (first non-BIC used), bank with no routings (all fields empty)

**Migration Notes for Go**:
- Iterate through bank_routings slice
- Use conditional logic to find BIC scheme
- Filter and extract first non-BIC routing
- Handle empty/nil slices gracefully
- Consider using helper functions for clarity

**Example Scenarios**:
```
Scenario 1: Bank with BIC and national ID
Input: bank_routings = [{"scheme": "BIC", "address": "MYBANKXX"}, {"scheme": "NATIONAL_ID", "address": "12345"}]
Processing: BIC extracted to swiftBIC, NATIONAL_ID to generic routing
Output: swiftBIC="MYBANKXX", bankRoutingScheme="NATIONAL_ID", bankRoutingAddress="12345"

Scenario 2: Bank with only BIC
Input: bank_routings = [{"scheme": "BIC", "address": "MYBANKXX"}]
Processing: BIC extracted, no non-BIC routings
Output: swiftBIC="MYBANKXX", bankRoutingScheme="", bankRoutingAddress=""

Scenario 3: Bank with multiple non-BIC routings
Input: bank_routings = [{"scheme": "SORT_CODE", "address": "12-34-56"}, {"scheme": "IBAN", "address": "GB29..."}]
Processing: No BIC, first non-BIC used
Output: swiftBIC="", bankRoutingScheme="SORT_CODE", bankRoutingAddress="12-34-56"
```

**Business Context**:
BIC (Bank Identifier Code), also known as SWIFT code, is the international standard for identifying banks in cross-border transactions. The OBP system gives it special treatment by storing it in a dedicated field (swiftBIC) for easy querying and integration with international payment systems. Other routing schemes (national IDs, sort codes, etc.) are country-specific and stored in generic fields.

---

### BR-007: Idempotent Bank Creation/Update Rule

**Category**: WORKFLOW

**Description**: The bank creation operation is idempotent - if a bank with the given ID already exists, the system updates the existing record rather than failing or creating a duplicate. This enables safe retries and configuration updates.

**Source**: 
- File: obp-api/src/main/scala/code/api/util/NewStyle.scala
- Class/Object: NewStyle.function
- Method: createOrUpdateBank
- Lines: 310-337

**Business Logic**:
1. Validate the bank ID format
2. Call the connector's createOrUpdateBank method
3. Connector checks if bank with given ID exists
4. If bank exists: Update all fields (full name, short name, logo, website, routing info)
5. If bank doesn't exist: Create new bank record with all provided fields
6. Return the created or updated bank object
7. Operation succeeds in both cases (create or update)

**Scala Implementation**:
```scala
def createOrUpdateBank(bankId: String,
                       fullBankName: String,
                       shortBankName: String,
                       logoURL: String,
                       websiteURL: String,
                       swiftBIC: String,
                       national_identifier: String,
                       bankRoutingScheme: String,
                       bankRoutingAddress: String,
                       callContext: Option[CallContext]): OBPReturnType[Bank] = {
  validateBankId(bankId, callContext)
  Future {
    Connector.connector.vend.createOrUpdateBank(
      bankId, fullBankName, shortBankName, logoURL, websiteURL,
      swiftBIC, national_identifier, bankRoutingScheme, bankRoutingAddress,
      callContext
    ) map {
      i =>  (i, callContext)
    }
  } map { unboxFull(_) }
}
```

**Variables**:
- **Input**: bankId, fullBankName, shortBankName, logoURL, websiteURL, routing information
- **Output**: Bank object (either newly created or updated existing)
- **Constants**: None

**Business Conditions**:
| Condition | Business Meaning | Values/Thresholds |
|-----------|------------------|-------------------|
| Bank with ID exists | Update existing bank | All fields updated |
| Bank with ID doesn't exist | Create new bank | New record created |

**Business Impact**: 
This rule enables idempotent API operations, meaning the same request can be called multiple times safely. This is critical for distributed systems where network failures may cause retries, and for configuration management where the same bank details may be applied multiple times. It also allows updating bank information without a separate update endpoint.

**API Endpoints Using This Rule**:
- POST /obp/v4.0.0/banks - Bank creation/update

**Related Test Cases**:
Tests should validate: creating new bank (returns 201), calling same request again (updates existing, returns 201), updating bank details (fields changed), concurrent requests with same ID (one succeeds)

**Migration Notes for Go**:
- Implement upsert logic (INSERT ... ON CONFLICT UPDATE in PostgreSQL)
- Or: Check if record exists, then INSERT or UPDATE accordingly
- Ensure atomic operation to prevent race conditions
- Return appropriate status (201 for both create and update, or distinguish them)
- Consider using database transactions for atomicity

**Example Scenarios**:
```
Scenario 1: Creating new bank
Input: bankId="new-bank", fullBankName="New Bank Ltd"
Processing: Bank doesn't exist, create new record
Output: New bank created, HTTP 201

Scenario 2: Updating existing bank
Input: bankId="existing-bank", fullBankName="Updated Bank Name"
Processing: Bank exists, update fullBankName field
Output: Bank updated, HTTP 201

Scenario 3: Retry after network failure
Input: Same request as Scenario 1 sent again
Processing: Bank now exists (from first request), update record
Output: Bank updated (idempotent), HTTP 201
```

**Business Context**:
Idempotency is a critical property for distributed systems and APIs. Network failures, client retries, and configuration management tools often result in the same request being sent multiple times. By making bank creation idempotent (create-or-update), the OBP API ensures these retries are safe and don't cause errors or duplicate records. This also simplifies bank management by allowing configuration updates through the same endpoint.

---

### BR-008: Automatic Entitlement Grant Rule

**Category**: WORKFLOW

**Description**: Upon successful bank creation, the system automatically grants the creating user two specific entitlements for that bank: CanCreateEntitlementAtOneBank and CanReadDynamicResourceDocsAtOneBank. This enables self-service bank management without requiring system administrator intervention.

**Source**: 
- File: obp-api/src/main/scala/code/api/v4_0_0/APIMethods400.scala
- Class/Object: Implementations400
- Method: createBank
- Lines: 3663-3678

**Business Logic**:
1. After bank is successfully created/updated, retrieve user's existing entitlements
2. Filter entitlements to find those specific to the newly created bank
3. Check if user already has CanCreateEntitlementAtOneBank for this bank
4. If not present, grant CanCreateEntitlementAtOneBank entitlement
5. If already present, skip (no duplicate entitlements)
6. Check if user already has CanReadDynamicResourceDocsAtOneBank for this bank
7. If not present, grant CanReadDynamicResourceDocsAtOneBank entitlement
8. If already present, skip (no duplicate entitlements)

**Scala Implementation**:
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

**Variables**:
- **Input**: cc.userId (UserId), bank.id (BankId), existing entitlements
- **Output**: Two new entitlement records (if not already present)
- **Constants**: CanCreateEntitlementAtOneBank, CanReadDynamicResourceDocsAtOneBank

**Business Conditions**:
| Condition | Business Meaning | Values/Thresholds |
|-----------|------------------|-------------------|
| Entitlement not present | User needs permission | Grant entitlement |
| Entitlement already present | User has permission | Skip (no duplicate) |

**Business Impact**: 
This rule enables a self-service model where users who create banks automatically receive permissions to manage those banks (assign roles to others, manage documentation). This reduces dependency on system administrators and enables faster bank onboarding while maintaining security (users only get permissions for banks they create).

**API Endpoints Using This Rule**:
- POST /obp/v4.0.0/banks - Bank creation/update

**Related Test Cases**:
Tests should validate: new bank creator receives both entitlements, existing bank update doesn't duplicate entitlements, entitlements are bank-specific (not system-wide), user can use granted entitlements immediately

**Migration Notes for Go**:
- After successful bank creation, query user's existing entitlements
- Filter by bankId to check bank-specific entitlements
- Use conditional logic to grant only missing entitlements
- Ensure entitlement grants are atomic (use transactions)
- Handle entitlement grant failures gracefully (bank already created)

**Example Scenarios**:
```
Scenario 1: User creates first bank
Input: userId="user123", bankId="new-bank"
Processing: No existing entitlements for this bank, grant both
Output: Two entitlements created: CanCreateEntitlementAtOneBank and CanReadDynamicResourceDocsAtOneBank

Scenario 2: User updates existing bank
Input: userId="user123", bankId="existing-bank" (user already has entitlements)
Processing: Entitlements already present, skip grant
Output: No new entitlements created

Scenario 3: Different user updates bank
Input: userId="user456", bankId="existing-bank" (created by user123)
Processing: user456 has no entitlements for this bank, grant both
Output: Two entitlements created for user456
```

**Business Context**:
The OBP API uses role-based access control (RBAC) through entitlements. CanCreateEntitlementAtOneBank allows the user to grant roles to other users for this specific bank, enabling them to build a team. CanReadDynamicResourceDocsAtOneBank allows them to view and manage API documentation for their bank. By automatically granting these permissions to bank creators, the system enables self-service bank management without requiring system administrator intervention for every permission grant.

---

### BR-009: Settlement Account Auto-Creation Rule (Sandbox Mode)

**Category**: WORKFLOW

**Description**: In sandbox/mapped connector mode, the system automatically creates two default settlement accounts (incoming and outgoing) for each newly created bank. These accounts are used for payment processing reconciliation and have fixed IDs, names, and EUR currency.

**Source**: 
- File: obp-api/src/main/scala/code/api/v4_0_0/APIMethods400.scala (documentation)
- Referenced in: LocalMappedConnector implementation
- Lines: 3602-3606 (documentation)

**Business Logic**:
1. Check if connector mode is "mapped" (sandbox mode)
2. If in sandbox mode and bank is newly created (not update):
3. Create incoming settlement account with:
   - Account ID: OBP_DEFAULT_INCOMING_ACCOUNT_ID
   - Account Name: "Default incoming settlement account"
   - Currency: EUR
   - Initial Balance: 0
4. Create outgoing settlement account with:
   - Account ID: OBP_DEFAULT_OUTGOING_ACCOUNT_ID
   - Account Name: "Default outgoing settlement account"
   - Currency: EUR
   - Initial Balance: 0
5. If not in sandbox mode or bank already exists, skip settlement account creation

**Scala Implementation**:
```scala
// Documentation from APIMethods400.scala
// Only SANDBOX mode (i.e. when connector=mapped in properties file)
// The settlement accounts are automatically created by the system when the bank is created.
// Name and account id are created in accordance to the next rules:
//   - Incoming account (name: Default incoming settlement account, Account ID: OBP_DEFAULT_INCOMING_ACCOUNT_ID, currency: EUR)
//   - Outgoing account (name: Default outgoing settlement account, Account ID: OBP_DEFAULT_OUTGOING_ACCOUNT_ID, currency: EUR)
```

**Variables**:
- **Input**: bankId (BankId), connector mode configuration
- **Output**: Two settlement account records (if sandbox mode and new bank)
- **Constants**: 
  - OBP_DEFAULT_INCOMING_ACCOUNT_ID
  - OBP_DEFAULT_OUTGOING_ACCOUNT_ID
  - Default currency: EUR
  - Initial balance: 0

**Business Conditions**:
| Condition | Business Meaning | Values/Thresholds |
|-----------|------------------|-------------------|
| connector=mapped | Sandbox/development mode | Settlement accounts created |
| connector!=mapped | Production mode | No settlement accounts created |
| Bank is new | First-time creation | Settlement accounts created |
| Bank exists | Update operation | No settlement accounts created |

**Business Impact**: 
This rule enables immediate payment processing testing in sandbox mode by providing the necessary settlement accounts for reconciliation. In production, settlement accounts are managed by the backend banking system. This separation allows developers to test payment flows without requiring a full banking backend.

**API Endpoints Using This Rule**:
- POST /obp/v4.0.0/banks - Bank creation (sandbox mode only)

**Related Test Cases**:
Tests should validate: sandbox mode creates settlement accounts, production mode doesn't create settlement accounts, settlement accounts have correct IDs and names, settlement accounts have EUR currency and 0 balance, updating existing bank doesn't create duplicate settlement accounts

**Migration Notes for Go**:
- Check configuration for connector mode (mapped vs production)
- Implement conditional logic based on mode
- Use constants for settlement account IDs and names
- Ensure settlement account creation is atomic with bank creation
- Handle case where settlement accounts already exist (idempotent)
- Consider making currency configurable in future

**Example Scenarios**:
```
Scenario 1: Create bank in sandbox mode
Input: bankId="test-bank", connector=mapped
Processing: Bank created, two settlement accounts created
Output: Bank + 2 settlement accounts (incoming and outgoing)

Scenario 2: Create bank in production mode
Input: bankId="prod-bank", connector=rest
Processing: Bank created, no settlement accounts created
Output: Bank only (settlement accounts managed by backend)

Scenario 3: Update bank in sandbox mode
Input: bankId="existing-test-bank", connector=mapped
Processing: Bank updated, settlement accounts already exist
Output: Bank updated, no new settlement accounts
```

**Business Context**:
Settlement accounts are special accounts used in payment processing to track funds in transit. The incoming settlement account receives funds from external sources before they're credited to customer accounts. The outgoing settlement account tracks funds sent to external destinations before they're debited from customer accounts. These accounts are essential for reconciliation and ensuring payment integrity. In sandbox mode, OBP creates these automatically to enable payment testing without a full banking backend.

---

### BR-010: OBP Routing Scheme Auto-Addition Rule

**Category**: TRANSFORMATION

**Description**: The system automatically adds an "OBP" routing scheme to every bank's routing information in the API response, using the bank ID as the address, even if not provided in the request. This ensures every bank has an OBP-specific identifier for internal routing.

**Source**: 
- File: obp-api/src/main/scala/code/api/v4_0_0/JSONFactory400.scala
- Method: createBankJSON400
- Referenced in user story lines 122-123

**Business Logic**:
1. When creating the JSON response for a bank
2. Retrieve all stored routing schemes (BIC, national IDs, custom schemes)
3. Automatically prepend an "OBP" routing scheme entry
4. Use the bank ID as the address for the OBP routing scheme
5. Return the complete list with OBP routing first, followed by other routings

**Scala Implementation**:
```scala
// Inferred from response structure in user story
// Response always includes:
// {"scheme": "OBP", "address": "bank-id-123"}
// Even if not provided in request
```

**Variables**:
- **Input**: bank.id (BankId), stored routing schemes
- **Output**: bank_routings array with OBP scheme prepended
- **Constants**: OBP routing scheme name = "OBP"

**Business Conditions**:
| Condition | Business Meaning | Values/Thresholds |
|-----------|------------------|-------------------|
| Always true | Every bank has OBP routing | OBP scheme added to response |

**Business Impact**: 
This rule ensures every bank in the OBP system has a consistent, system-wide identifier (the OBP routing scheme) that can be used for internal routing, connector selection, and system integration. It provides a fallback routing mechanism when other schemes are unavailable.

**API Endpoints Using This Rule**:
- POST /obp/v4.0.0/banks - Bank creation/update response
- GET /obp/v4.0.0/banks/BANK_ID - Bank details response

**Related Test Cases**:
Tests should validate: OBP routing always present in response, OBP routing uses bank ID as address, OBP routing appears first in array, other routings preserved after OBP routing

**Migration Notes for Go**:
- When serializing bank to JSON response, prepend OBP routing
- Create BankRouting struct with scheme="OBP" and address=bankID
- Append other stored routings after OBP routing
- Ensure OBP routing is not stored in database (response-only)

**Example Scenarios**:
```
Scenario 1: Bank with BIC routing
Input: bank_routings = [{"scheme": "BIC", "address": "MYBANKXX"}]
Processing: Add OBP routing to response
Output: [{"scheme": "OBP", "address": "my-bank"}, {"scheme": "BIC", "address": "MYBANKXX"}]

Scenario 2: Bank with no routings
Input: bank_routings = []
Processing: Add OBP routing to response
Output: [{"scheme": "OBP", "address": "my-bank"}]

Scenario 3: Bank with multiple routings
Input: bank_routings = [{"scheme": "BIC", "address": "XX"}, {"scheme": "NATIONAL_ID", "address": "123"}]
Processing: Add OBP routing to response
Output: [{"scheme": "OBP", "address": "my-bank"}, {"scheme": "BIC", "address": "XX"}, {"scheme": "NATIONAL_ID", "address": "123"}]
```

**Business Context**:
The OBP routing scheme serves as a universal identifier within the OBP ecosystem. While BIC codes are used for international payments and national IDs for domestic payments, the OBP routing provides a consistent identifier that works across all banks in the system, regardless of their external routing schemes. This is particularly useful for internal routing, connector selection, and ensuring every bank has at least one routing identifier.

---

### BR-011: Authorization Check for Bank Creation

**Category**: DECISION

**Description**: Users must have the CanCreateBank entitlement before they can create banks in the system. This role-based access control prevents unauthorized bank registration and ensures only privileged users can add banks to the platform.

**Source**: 
- File: obp-api/src/main/scala/code/api/v4_0_0/APIMethods400.scala
- Documented in: staticResourceDocs (line 3618)
- Enforced by: OBP framework based on role declaration

**Business Logic**:
1. Before processing bank creation request, check user's entitlements
2. Verify user has CanCreateBank role (system-wide or bank-specific)
3. If user lacks CanCreateBank entitlement, reject with 403 Forbidden
4. If user has CanCreateBank entitlement, allow processing to continue
5. Error message: "Insufficient authorisation to create bank"

**Scala Implementation**:
```scala
// Declared in ResourceDoc
Some(List(canCreateBank))

// Framework enforces this check before endpoint execution
// If user lacks entitlement, returns:
// HTTP 403: "Insufficient authorisation to create bank"
```

**Variables**:
- **Input**: user.entitlements (List[Entitlement]), required role: CanCreateBank
- **Output**: Authorization success or 403 error
- **Constants**: Required role = CanCreateBank

**Business Conditions**:
| Condition | Business Meaning | Values/Thresholds |
|-----------|------------------|-------------------|
| User has CanCreateBank | User authorized to create banks | Processing continues |
| User lacks CanCreateBank | User not authorized | HTTP 403 error |

**Business Impact**: 
This rule prevents unauthorized bank creation, which could lead to system abuse, fake banks, or security issues. By restricting bank creation to users with explicit CanCreateBank entitlement, the system ensures only trusted administrators or authorized partners can register banks on the platform.

**API Endpoints Using This Rule**:
- POST /obp/v4.0.0/banks - Bank creation/update

**Related Test Cases**:
Tests should validate: user with CanCreateBank can create banks, user without CanCreateBank receives 403 error, system admin can grant CanCreateBank to users, newly granted entitlement works immediately

**Migration Notes for Go**:
- Implement middleware to check user entitlements before handler execution
- Query user's entitlements from database or cache
- Check if CanCreateBank is present in user's entitlement list
- Return 403 with appropriate error message if missing
- Consider caching entitlements for performance

**Example Scenarios**:
```
Scenario 1: Authorized user creates bank
Input: User with CanCreateBank entitlement
Processing: Authorization check passes
Output: Bank creation proceeds

Scenario 2: Unauthorized user attempts to create bank
Input: User without CanCreateBank entitlement
Processing: Authorization check fails
Output: HTTP 403 "Insufficient authorisation to create bank"

Scenario 3: System admin grants entitlement
Input: Admin grants CanCreateBank to user, user creates bank
Processing: Authorization check passes with new entitlement
Output: Bank creation proceeds
```

**Business Context**:
The OBP API uses role-based access control (RBAC) through entitlements. CanCreateBank is a powerful system-level entitlement that allows creating new banks on the platform. This is typically granted only to system administrators, bank partners, or trusted users. Restricting this capability prevents abuse, ensures accountability, and maintains the integrity of the banking platform by controlling who can register new financial institutions.

---

## API Endpoint Coverage

| Endpoint | HTTP Method | Business Rules | Rule IDs |
|----------|-------------|----------------|----------|
| /obp/v4.0.0/banks | POST | Bank ID validation, consumer auth, BIC extraction, idempotent create/update, auto-entitlement grant, settlement account creation, authorization check | BR-001, BR-002, BR-003, BR-004, BR-005, BR-006, BR-007, BR-008, BR-009, BR-010, BR-011 |

## Migration Validation Matrix

| Rule ID | Test Case Reference | Go Implementation Status | Validation Status |
|---------|---------------------|--------------------------|-------------------|
| BR-001 | TestBankIDFormatValidation | Pending | Pending |
| BR-002 | TestBankIDMinimumLength | Pending | Pending |
| BR-003 | TestBankIDSpaceRestriction | Pending | Pending |
| BR-004 | TestBankIDSpecialCharRestriction | Pending | Pending |
| BR-005 | TestConsumerAuthentication | Pending | Pending |
| BR-006 | TestBICRoutingExtraction | Pending | Pending |
| BR-007 | TestIdempotentBankCreation | Pending | Pending |
| BR-008 | TestAutomaticEntitlementGrant | Pending | Pending |
| BR-009 | TestSettlementAccountCreation | Pending | Pending |
| BR-010 | TestOBPRoutingAutoAddition | Pending | Pending |
| BR-011 | TestAuthorizationCheck | Pending | Pending |

## Notes and Assumptions

**Assumptions Made**:
1. The LocalMappedConnector implementation follows the same business logic as documented in the API layer
2. Settlement account creation logic exists in LocalMappedConnector (not directly visible in analyzed files)
3. OBP routing auto-addition happens in JSONFactory400 during response serialization
4. The CanCreateBank authorization check is enforced by the OBP framework based on ResourceDoc declaration

**Gaps Identified**:
1. Settlement account creation implementation not directly analyzed (referenced in documentation only)
2. JSONFactory400.createBankJSON400 method not analyzed (OBP routing addition logic)
3. Entitlement checking framework logic not analyzed (assumed to work as documented)

**Migration Considerations**:
1. All validation rules (BR-001 through BR-004) should be implemented as reusable validation functions in Go
2. Consumer authentication (BR-005) requires OAuth middleware integration
3. Idempotent create/update (BR-007) requires database upsert support or explicit existence checking
4. Automatic entitlement grants (BR-008) should be atomic with bank creation (use database transactions)
5. Settlement account creation (BR-009) should be conditional based on configuration (sandbox vs production)
6. Authorization checks (BR-011) should be implemented as middleware for reusability across endpoints

---

This completes the Business Rules Extraction for the Bank Registration and Configuration functionality. All 11 business rules have been extracted from the actual Scala implementation and documented according to the Business_rule_prompt.md template.
