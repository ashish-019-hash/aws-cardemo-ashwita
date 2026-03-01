# Validation Rules

**Extracted From:** OBP-API Scala Application  
**User Story:** Branch Management  
**Analysis Date:** 2026-01-20  
**Analyst:** Scala Systems Analyst

## Summary

Total Validation Rules Extracted: 18

### Validation Categories
- Input Validation Rules: 6
- Format Validation Rules: 4
- Business Constraint Rules: 5
- Length/Boundary Rules: 3
- Cross-Field Validation Rules: 0

---

## Category: Input Validation

### Rule VR-001: Bank ID Required Validation

**Field/Entity:** bank_id

**Validation Type:** Required Field Validation

**Rule Description:**
The bank_id field is required for all branch operations. The system validates that a valid bank exists for the provided bank_id before allowing branch creation, update, or retrieval.

**Validation Logic:**

- **Condition:** When creating, updating, or retrieving a branch
- **Check:** Validate that bank_id is provided and references an existing bank in the system
- **Valid Criteria:** bank_id is non-empty and corresponds to an existing bank record
- **Invalid Criteria:** bank_id is empty, null, or does not match any existing bank
- **Action on Success:** Proceed with branch operation
- **Action on Failure:** Return error response with 404 status code

**Error Handling:**

- **Error Message:** `OBP-300010: Branch not found. Please specify a valid value for BRANCH_ID. Or License may not be set. meta.license.id and meta.license.name can not be empty`
- **Error Code:** `OBP-300010`
- **HTTP Status Code:** `404 Not Found`

**Scala Implementation:**

- **Location:** `code.api.util.ErrorMessages`
- **Method/Function:** Bank existence check in API methods
- **Line Reference:** ErrorMessages.scala line 321

**Code Snippet:**
```scala
val BranchNotFoundByBranchId = "OBP-300010: Branch not found. Please specify a valid value for BRANCH_ID. Or License may not be set. meta.license.id and meta.license.name can not be empty"
```

**Related Entities:**
- Bank (bank_id field)
- Branch (bank_id field)

**User Story Context:**
This validation ensures that branches can only be created for existing banks, maintaining referential integrity in the system.

**Dependencies:**
- Bank must exist before branch operations

---

### Rule VR-002: Branch ID Required Validation

**Field/Entity:** branch_id (id)

**Validation Type:** Required Field Validation

**Rule Description:**
The branch_id field is required for branch creation. For update and delete operations, the branch_id must exist in the system for the specified bank.

**Validation Logic:**

- **Condition:** When creating a new branch or accessing an existing branch
- **Check:** Validate that branch_id is provided for creation, and exists for update/delete operations
- **Valid Criteria:** branch_id is non-empty string for creation; branch_id exists in database for update/delete
- **Invalid Criteria:** branch_id is empty, null, or does not exist for the given bank_id
- **Action on Success:** Proceed with branch operation
- **Action on Failure:** Return error response with 404 status code

**Error Handling:**

- **Error Message:** `OBP-300010: Branch not found. Please specify a valid value for BRANCH_ID. Or License may not be set. meta.license.id and meta.license.name can not be empty`
- **Error Code:** `OBP-300010`
- **HTTP Status Code:** `404 Not Found`

**Scala Implementation:**

- **Location:** `code.branches.MappedBranchesProvider`
- **Method/Function:** `getBranchFromProvider`
- **Line Reference:** MappedBranchesProvider.scala lines 12-16

**Code Snippet:**
```scala
override protected def getBranchFromProvider(bankId: BankId, branchId: BranchId): Option[BranchT] =
  MappedBranch.find(
    By(MappedBranch.mBankId, bankId.value),
    By(MappedBranch.mBranchId, branchId.value)
  )
```

**Related Entities:**
- Branch (branchId field)
- BranchId value object

**User Story Context:**
This validation ensures that branch operations target valid, existing branches and that new branches have proper identifiers.

**Dependencies:**
- Bank must exist (VR-001)

---

### Rule VR-003: Branch Name Required Validation

**Field/Entity:** name

**Validation Type:** Required Field Validation

**Rule Description:**
The branch name field is required for branch creation and update operations.

**Validation Logic:**

- **Condition:** When creating or updating a branch
- **Check:** Validate that name field is provided in the request body
- **Valid Criteria:** name is a non-empty string
- **Invalid Criteria:** name is empty, null, or missing from request
- **Action on Success:** Proceed with branch creation/update
- **Action on Failure:** Return error response with 400 status code

**Error Handling:**

- **Error Message:** `OBP-10001: Incorrect json format.`
- **Error Code:** `OBP-10001`
- **HTTP Status Code:** `400 Bad Request`

**Scala Implementation:**

- **Location:** `code.api.v2_1_0.JSONFactory2.1.0`
- **Method/Function:** `BranchJsonPostV210` case class
- **Line Reference:** JSONFactory2.1.0.scala lines 292-300

**Code Snippet:**
```scala
case class BranchJsonPostV210(
  id: String,
  bank_id: String,
  name: String,
  address: AddressJsonV140,
  location: LocationJsonV140,
  meta: MetaJsonV140,
  lobby: LobbyStringJson,
  drive_up: DriveUpStringJson)
```

**Related Entities:**
- Branch (name field)

**User Story Context:**
Branch names are essential for customers to identify physical banking locations.

**Dependencies:**
- None (standalone validation)

---

### Rule VR-004: Address Required Validation

**Field/Entity:** address

**Validation Type:** Required Field Validation

**Rule Description:**
The address object is required for branch creation and update operations. The address must include line_1, city, state, postcode, and country fields.

**Validation Logic:**

- **Condition:** When creating or updating a branch
- **Check:** Validate that address object is provided with required sub-fields
- **Valid Criteria:** address object contains line_1, city, state, postcode, and country
- **Invalid Criteria:** address object is missing or has empty required sub-fields
- **Action on Success:** Proceed with branch creation/update
- **Action on Failure:** Return error response with 400 status code

**Error Handling:**

- **Error Message:** `OBP-10001: Incorrect json format.`
- **Error Code:** `OBP-10001`
- **HTTP Status Code:** `400 Bad Request`

**Scala Implementation:**

- **Location:** `code.api.v1_4_0.JSONFactory1_4_0`
- **Method/Function:** `AddressJsonV140` case class
- **Line Reference:** JSONFactory1_4_0.scala line 110

**Code Snippet:**
```scala
case class AddressJsonV140(line_1 : String, line_2 : String, line_3 : String, city : String, state : String, postcode : String, country : String)
```

**Related Entities:**
- Branch (address field)
- Address (line_1, line_2, line_3, city, state, postcode, country fields)

**User Story Context:**
Complete address information is essential for customers to locate physical branch locations.

**Dependencies:**
- None (standalone validation)

---

### Rule VR-005: Location Required Validation

**Field/Entity:** location

**Validation Type:** Required Field Validation

**Rule Description:**
The location object containing latitude and longitude coordinates is required for branch creation and update operations.

**Validation Logic:**

- **Condition:** When creating or updating a branch
- **Check:** Validate that location object is provided with latitude and longitude
- **Valid Criteria:** location object contains valid latitude and longitude values
- **Invalid Criteria:** location object is missing or has invalid coordinates
- **Action on Success:** Proceed with branch creation/update
- **Action on Failure:** Return error response with 400 status code

**Error Handling:**

- **Error Message:** `OBP-10001: Incorrect json format.`
- **Error Code:** `OBP-10001`
- **HTTP Status Code:** `400 Bad Request`

**Scala Implementation:**

- **Location:** `code.api.v1_4_0.JSONFactory1_4_0`
- **Method/Function:** `LocationJsonV140` case class
- **Line Reference:** JSONFactory1_4_0.scala line 81

**Code Snippet:**
```scala
case class LocationJsonV140(latitude : Double, longitude : Double)
```

**Related Entities:**
- Branch (location field)
- Location (latitude, longitude fields)

**User Story Context:**
Geographic coordinates enable map-based branch location services for customers.

**Dependencies:**
- None (standalone validation)

---

### Rule VR-006: JSON Format Validation

**Field/Entity:** Request Body

**Validation Type:** Format Validation

**Rule Description:**
All branch API requests must contain valid JSON that matches the expected schema (BranchJsonPostV210 for creation, BranchJsonPutV210 for updates).

**Validation Logic:**

- **Condition:** When receiving any branch API request with a body
- **Check:** Parse and validate JSON against expected schema
- **Valid Criteria:** JSON is well-formed and matches expected case class structure
- **Invalid Criteria:** Malformed JSON or missing required fields
- **Action on Success:** Extract data and proceed with operation
- **Action on Failure:** Return error response with 400 status code

**Error Handling:**

- **Error Message:** `OBP-10001: Incorrect json format.`
- **Error Code:** `OBP-10001`
- **HTTP Status Code:** `400 Bad Request`

**Scala Implementation:**

- **Location:** `code.api.v2_1_0.APIMethods210`
- **Method/Function:** `createBranch`, `updateBranch`
- **Line Reference:** APIMethods210.scala lines 1526-1527, 1573-1574

**Code Snippet:**
```scala
branchJsonPostV210 <- NewStyle.function.tryons(failMsg = InvalidJsonFormat + " BranchJsonPostV210", 400, callContext) {
  json.extract[BranchJsonPostV210]
}
```

**Related Entities:**
- BranchJsonPostV210
- BranchJsonPutV210

**User Story Context:**
Proper JSON format validation ensures data integrity and provides clear error messages for API consumers.

**Dependencies:**
- None (standalone validation)

---

## Category: Format Validation

### Rule VR-007: Country Code Format Validation

**Field/Entity:** country_code (country)

**Validation Type:** Format Validation (ISO Code)

**Rule Description:**
The country code in the address must be a valid 2-character ISO country code.

**Validation Logic:**

- **Condition:** When creating or updating a branch with address information
- **Check:** Validate that country code is a 2-character string
- **Valid Criteria:** 2-character ISO country code (e.g., "US", "GB", "DE")
- **Invalid Criteria:** Country code is not exactly 2 characters or not a valid ISO code
- **Action on Success:** Store country code in branch record
- **Action on Failure:** Return error response with 400 status code

**Error Handling:**

- **Error Message:** `OBP-10001: Incorrect json format.`
- **Error Code:** `OBP-10001`
- **HTTP Status Code:** `400 Bad Request`

**Scala Implementation:**

- **Location:** `code.branches.MappedBranchesProvider`
- **Method/Function:** `MappedBranch` class
- **Line Reference:** MappedBranchesProvider.scala line 50

**Code Snippet:**
```scala
object mCountryCode extends MappedString(this, 2)
```

**Related Entities:**
- Address (countryCode field)

**User Story Context:**
Standardized country codes ensure consistent address formatting and enable international branch location services.

**Dependencies:**
- Address validation (VR-004)

---

### Rule VR-008: Time Format Validation (24-Hour Clock)

**Field/Entity:** opening_time, closing_time

**Validation Type:** Format Validation (Time Pattern)

**Rule Description:**
Opening and closing times for lobby and drive-up hours must follow 24-hour clock format (HH:MM). Times after midnight can be represented as values greater than 24:00 (e.g., "25:30" for 1:30 AM).

**Validation Logic:**

- **Condition:** When creating or updating branch hours
- **Check:** Validate that time strings follow HH:MM format
- **Valid Criteria:** Time string matches pattern like "09:00", "17:30", "25:00" (for after midnight)
- **Invalid Criteria:** Time string does not match expected format
- **Action on Success:** Store time values in branch record
- **Action on Failure:** Return error response with 400 status code

**Error Handling:**

- **Error Message:** `OBP-10001: Incorrect json format.`
- **Error Code:** `OBP-10001`
- **HTTP Status Code:** `400 Bad Request`

**Scala Implementation:**

- **Location:** `code.util.UUIDString`
- **Method/Function:** `TwentyFourHourClockString` class
- **Line Reference:** UUIDString.scala lines 37-41

**Code Snippet:**
```scala
/*
So we can store a time of day without the date e.g. 23:33 - but also go past midnight e.g. 26:33 if we want to represent the following morning.
Being string gives us flexibility to store other unstructured code too.
*/
class TwentyFourHourClockString [T <: Mapper[T]](override val fieldOwner : T) extends MappedString(fieldOwner, TwentyFourHourClockString.MaxLength)

object TwentyFourHourClockString {
  val MaxLength = APIUtil.getPropsAsIntValue("time_string.length", 5)
}
```

**Related Entities:**
- Lobby (opening_time, closing_time per day)
- DriveUp (opening_time, closing_time per day)

**User Story Context:**
Proper time format validation ensures consistent operating hours display for customers.

**Dependencies:**
- None (standalone validation)

---

### Rule VR-009: Accessibility Indicator Format Validation

**Field/Entity:** is_accessible

**Validation Type:** Format Validation (Tristate Boolean)

**Rule Description:**
The isAccessible field uses a tristate value: "Y" for accessible, "N" for not accessible, empty string for unknown.

**Validation Logic:**

- **Condition:** When creating or updating branch accessibility information
- **Check:** Validate that is_accessible is one of the allowed values
- **Valid Criteria:** Value is "Y", "N", or empty string ""
- **Invalid Criteria:** Value is any other string
- **Action on Success:** Store accessibility status in branch record
- **Action on Failure:** Return error response with 400 status code

**Error Handling:**

- **Error Message:** `OBP-10001: Incorrect json format.`
- **Error Code:** `OBP-10001`
- **HTTP Status Code:** `400 Bad Request`

**Scala Implementation:**

- **Location:** `code.branches.MappedBranchesProvider`
- **Method/Function:** `isAccessible` method in MappedBranch class
- **Line Reference:** MappedBranchesProvider.scala lines 112, 246-250

**Code Snippet:**
```scala
object mIsAccessible extends MappedString(this, 1) // Easy access for people who use wheelchairs etc. Tristate boolean "Y"=true "N"=false ""=Unknown

// Easy access for people who use wheelchairs etc. "Y"=true "N"=false ""=Unknown
override def isAccessible = mIsAccessible.get match {
  case "Y" => Some(true)
  case "N" => Some(false)
  case _ => None
}
```

**Related Entities:**
- Branch (isAccessible field)

**User Story Context:**
Accessibility information helps customers with disabilities find suitable branch locations.

**Dependencies:**
- None (standalone validation)

---

### Rule VR-010: Branch Routing Default Validation

**Field/Entity:** branch_routing

**Validation Type:** Format Validation (Default Value)

**Rule Description:**
If branch routing scheme and address are not provided, the system defaults to using "BRANCH_ID" as the scheme and the branch_id as the address.

**Validation Logic:**

- **Condition:** When creating or updating a branch without explicit routing information
- **Check:** Check if branch_routing scheme and address are provided
- **Valid Criteria:** Either explicit routing provided or system uses defaults
- **Invalid Criteria:** N/A (defaults are always applied)
- **Action on Success:** Use provided routing or apply defaults
- **Action on Failure:** N/A (always succeeds with defaults)

**Error Handling:**

- **Error Message:** N/A (no error - defaults applied)
- **Error Code:** N/A
- **HTTP Status Code:** N/A

**Scala Implementation:**

- **Location:** `code.branches.MappedBranchesProvider`
- **Method/Function:** `branchRouting` method in MappedBranch class
- **Line Reference:** MappedBranchesProvider.scala lines 125-132

**Code Snippet:**
```scala
// If not set, use BRANCH_ID and this value
override def branchRouting: Option[RoutingT] = Some(new RoutingT {
  override def scheme: String = {
    if (mBranchRoutingScheme == null || mBranchRoutingScheme == "") "BRANCH_ID" else mBranchRoutingScheme.get
  }
  override def address: String = {
    if (mBranchRoutingAddress == null || mBranchRoutingAddress == "") mBranchId.get else mBranchRoutingAddress.get
  }
})
```

**Related Entities:**
- Branch (branchRouting field)
- Routing (scheme, address fields)

**User Story Context:**
Default routing ensures all branches have valid routing information for integration with other banking systems.

**Dependencies:**
- Branch ID validation (VR-002)

---

## Category: Business Constraint Validation

### Rule VR-011: Create Branch Entitlement Validation

**Field/Entity:** User Entitlements

**Validation Type:** Authorization Validation

**Rule Description:**
Creating a branch requires the user to have CanCreateBranch entitlement for the specific bank OR CanCreateBranchAtAnyBank for system-wide access.

**Validation Logic:**

- **Condition:** When attempting to create a new branch
- **Check:** Validate that the authenticated user has required entitlements
- **Valid Criteria:** User has CanCreateBranch for the specific bank_id OR CanCreateBranchAtAnyBank
- **Invalid Criteria:** User lacks both entitlements
- **Action on Success:** Proceed with branch creation
- **Action on Failure:** Return error response with 403 status code

**Error Handling:**

- **Error Message:** `OBP-30209: Insufficient authorisation to Create Branch. You do not have the role CanCreateBranch.`
- **Error Code:** `OBP-30209`
- **HTTP Status Code:** `403 Forbidden`

**Scala Implementation:**

- **Location:** `code.api.v2_1_0.APIMethods210`
- **Method/Function:** `createBranch`
- **Line Reference:** APIMethods210.scala lines 1560, 1580

**Code Snippet:**
```scala
_ <- 
  NewStyle.function.hasAllEntitlements(bank.bankId.value, u.userId, canCreateBranch::Nil, canCreateBranchAtAnyBank::Nil, cc.callContext)
```

**Related Entities:**
- User (entitlements)
- ApiRole (CanCreateBranch, CanCreateBranchAtAnyBank)

**User Story Context:**
Entitlement validation ensures only authorized bank administrators can create branch records.

**Dependencies:**
- User authentication
- Bank existence (VR-001)

---

### Rule VR-012: Update Branch Entitlement Validation

**Field/Entity:** User Entitlements

**Validation Type:** Authorization Validation

**Rule Description:**
Updating a branch requires the user to have CanUpdateBranch entitlement.

**Validation Logic:**

- **Condition:** When attempting to update an existing branch
- **Check:** Validate that the authenticated user has CanUpdateBranch entitlement
- **Valid Criteria:** User has CanUpdateBranch entitlement for the specific bank
- **Invalid Criteria:** User lacks CanUpdateBranch entitlement
- **Action on Success:** Proceed with branch update
- **Action on Failure:** Return error response with 403 status code

**Error Handling:**

- **Error Message:** `OBP-20006: User is missing one or more roles: CanUpdateBranch`
- **Error Code:** `OBP-20006`
- **HTTP Status Code:** `403 Forbidden`

**Scala Implementation:**

- **Location:** `code.api.v2_1_0.APIMethods210`
- **Method/Function:** `updateBranch`
- **Line Reference:** APIMethods210.scala line 1532

**Code Snippet:**
```scala
_ <- NewStyle.function.hasEntitlement(bankId.value, u.userId, ApiRole.canUpdateBranch, callContext)
```

**Related Entities:**
- User (entitlements)
- ApiRole (CanUpdateBranch)

**User Story Context:**
Entitlement validation ensures only authorized users can modify branch information.

**Dependencies:**
- User authentication
- Branch existence (VR-002)

---

### Rule VR-013: Delete Branch Entitlement Validation

**Field/Entity:** User Entitlements

**Validation Type:** Authorization Validation

**Rule Description:**
Deleting a branch requires the user to have CanDeleteBranch entitlement for the specific bank OR CanDeleteBranchAtAnyBank for system-wide access.

**Validation Logic:**

- **Condition:** When attempting to delete a branch
- **Check:** Validate that the authenticated user has required entitlements
- **Valid Criteria:** User has CanDeleteBranch for the specific bank_id OR CanDeleteBranchAtAnyBank
- **Invalid Criteria:** User lacks both entitlements
- **Action on Success:** Proceed with branch deletion (soft delete)
- **Action on Failure:** Return error response with 403 status code

**Error Handling:**

- **Error Message:** `OBP-30218: Insufficient authorisation to Create Branch. You do not have the role CanCreateBranch.`
- **Error Code:** `OBP-30218`
- **HTTP Status Code:** `403 Forbidden`

**Scala Implementation:**

- **Location:** `code.api.v3_1_0.APIMethods310`
- **Method/Function:** `deleteBranch`
- **Line Reference:** APIMethods310.scala lines 2904-2906, 2937

**Code Snippet:**
```scala
private[this] val deleteBranchEntitlementsRequiredForSpecificBank = CanDeleteBranch :: Nil
private[this] val deleteBranchEntitlementsRequiredForAnyBank = CanDeleteBranchAtAnyBank :: Nil
private[this] val deleteBranchEntitlementsRequiredText = UserHasMissingRoles + deleteBranchEntitlementsRequiredForSpecificBank.mkString(" and ") + " entitlements are required OR " + deleteBranchEntitlementsRequiredForAnyBank.mkString(" and ")

allowedEntitlements = canDeleteBranch ::canDeleteBranchAtAnyBank:: Nil
```

**Related Entities:**
- User (entitlements)
- ApiRole (CanDeleteBranch, CanDeleteBranchAtAnyBank)

**User Story Context:**
Entitlement validation ensures only authorized users can delete branch records.

**Dependencies:**
- User authentication
- Branch existence (VR-002)

---

### Rule VR-014: Branch Uniqueness Validation

**Field/Entity:** bank_id, branch_id combination

**Validation Type:** Business Constraint Validation

**Rule Description:**
Each branch must have a unique combination of bank_id and branch_id. Duplicate branch IDs within the same bank are not allowed.

**Validation Logic:**

- **Condition:** When creating a new branch
- **Check:** Validate that the combination of bank_id and branch_id does not already exist
- **Valid Criteria:** No existing branch with same bank_id and branch_id combination
- **Invalid Criteria:** Branch with same bank_id and branch_id already exists
- **Action on Success:** Create new branch record
- **Action on Failure:** Return error response with 400 status code

**Error Handling:**

- **Error Message:** `OBP-30015: Could not insert the Branch`
- **Error Code:** `OBP-30015`
- **HTTP Status Code:** `400 Bad Request`

**Scala Implementation:**

- **Location:** `code.branches.MappedBranchesProvider`
- **Method/Function:** `MappedBranch` object
- **Line Reference:** MappedBranchesProvider.scala line 263

**Code Snippet:**
```scala
object MappedBranch extends MappedBranch with LongKeyedMetaMapper[MappedBranch] {
  override def dbIndexes = UniqueIndex(mBankId, mBranchId) :: Index(mBankId) :: super.dbIndexes
}
```

**Related Entities:**
- Branch (bankId, branchId fields)

**User Story Context:**
Uniqueness constraint prevents duplicate branch records and ensures data integrity.

**Dependencies:**
- Bank existence (VR-001)

---

### Rule VR-015: Soft Delete Pattern Validation

**Field/Entity:** isDeleted

**Validation Type:** Business Constraint Validation

**Rule Description:**
Branch deletion does not physically remove records but sets the isDeleted flag to true. Deleted branches are excluded from list queries.

**Validation Logic:**

- **Condition:** When deleting a branch or listing branches
- **Check:** For deletion: set isDeleted to true; For listing: filter out deleted branches
- **Valid Criteria:** isDeleted flag properly managed
- **Invalid Criteria:** N/A (system-managed)
- **Action on Success:** Branch marked as deleted or filtered from results
- **Action on Failure:** N/A

**Error Handling:**

- **Error Message:** N/A (no error for soft delete)
- **Error Code:** N/A
- **HTTP Status Code:** `204 No Content` on successful deletion

**Scala Implementation:**

- **Location:** `code.branches.MappedBranchesProvider`
- **Method/Function:** `getBranchesFromProvider`
- **Line Reference:** MappedBranchesProvider.scala lines 18-30

**Code Snippet:**
```scala
override protected def getBranchesFromProvider(bankId: BankId, queryParams: List[OBPQueryParam]): Option[List[BranchT]] = {
  logger.debug(s"getBranchesFromProvider says bankId is $bankId")

  val limit = queryParams.collect { case OBPLimit(value) => MaxRows[MappedBranch](value) }.headOption
  val offset = queryParams.collect { case OBPOffset(value) => StartAt[MappedBranch](value) }.headOption

  val optionalParams : Seq[QueryParam[MappedBranch]] = Seq(limit.toSeq, offset.toSeq).flatten
  val mapperParams = Seq(By(MappedBranch.mBankId, bankId.value), By(MappedBranch.mIsDeleted, false)) ++ optionalParams

  val branches: Option[List[BranchT]] = Some(MappedBranch.findAll(mapperParams:_*))

  branches
}
```

**Related Entities:**
- Branch (isDeleted field)

**User Story Context:**
Soft delete pattern allows for audit trails and potential recovery of deleted branch records.

**Dependencies:**
- Branch existence (VR-002)

---

## Category: Length/Boundary Validation

### Rule VR-016: Branch Name Length Validation

**Field/Entity:** name

**Validation Type:** Length Validation

**Rule Description:**
The branch name field has a maximum length of 255 characters.

**Validation Logic:**

- **Condition:** When creating or updating a branch
- **Check:** Validate that name length does not exceed 255 characters
- **Valid Criteria:** name.length <= 255
- **Invalid Criteria:** name.length > 255
- **Action on Success:** Store name in branch record
- **Action on Failure:** Return error response with 400 status code

**Error Handling:**

- **Error Message:** `OBP-20010: Value too long`
- **Error Code:** `OBP-20010`
- **HTTP Status Code:** `400 Bad Request`

**Scala Implementation:**

- **Location:** `code.branches.MappedBranchesProvider`
- **Method/Function:** `MappedBranch` class
- **Line Reference:** MappedBranchesProvider.scala line 39

**Code Snippet:**
```scala
object mName extends MappedString(this, 255)
```

**Related Entities:**
- Branch (name field)

**User Story Context:**
Length validation ensures branch names fit within database constraints and display properly in user interfaces.

**Dependencies:**
- None (standalone validation)

---

### Rule VR-017: Address Line Length Validation

**Field/Entity:** line_1, line_2, line_3, city, county, state

**Validation Type:** Length Validation

**Rule Description:**
Address fields have a maximum length of 255 characters each.

**Validation Logic:**

- **Condition:** When creating or updating a branch address
- **Check:** Validate that each address field length does not exceed 255 characters
- **Valid Criteria:** Each field.length <= 255
- **Invalid Criteria:** Any field.length > 255
- **Action on Success:** Store address in branch record
- **Action on Failure:** Return error response with 400 status code

**Error Handling:**

- **Error Message:** `OBP-20010: Value too long`
- **Error Code:** `OBP-20010`
- **HTTP Status Code:** `400 Bad Request`

**Scala Implementation:**

- **Location:** `code.branches.MappedBranchesProvider`
- **Method/Function:** `MappedBranch` class
- **Line Reference:** MappedBranchesProvider.scala lines 44-49

**Code Snippet:**
```scala
object mLine1 extends MappedString(this, 255)
object mLine2 extends MappedString(this, 255)
object mLine3 extends MappedString(this, 255)
object mCity extends MappedString(this, 255)
object mCounty extends MappedString(this, 255)
object mState extends MappedString(this, 255)
```

**Related Entities:**
- Address (line_1, line_2, line_3, city, county, state fields)

**User Story Context:**
Length validation ensures address data fits within database constraints.

**Dependencies:**
- Address validation (VR-004)

---

### Rule VR-018: Postcode Length Validation

**Field/Entity:** postcode

**Validation Type:** Length Validation

**Rule Description:**
The postcode field has a maximum length of 20 characters.

**Validation Logic:**

- **Condition:** When creating or updating a branch address
- **Check:** Validate that postcode length does not exceed 20 characters
- **Valid Criteria:** postcode.length <= 20
- **Invalid Criteria:** postcode.length > 20
- **Action on Success:** Store postcode in branch record
- **Action on Failure:** Return error response with 400 status code

**Error Handling:**

- **Error Message:** `OBP-20010: Value too long`
- **Error Code:** `OBP-20010`
- **HTTP Status Code:** `400 Bad Request`

**Scala Implementation:**

- **Location:** `code.branches.MappedBranchesProvider`
- **Method/Function:** `MappedBranch` class
- **Line Reference:** MappedBranchesProvider.scala line 51

**Code Snippet:**
```scala
object mPostCode extends MappedString(this, 20)
```

**Related Entities:**
- Address (postCode field)

**User Story Context:**
Length validation ensures postcode data fits within database constraints and accommodates various international postal code formats.

**Dependencies:**
- Address validation (VR-004)

---

## Additional Error Messages Reference

The following error messages are defined in the Scala codebase for branch-related operations:

| Error Code | Error Message | HTTP Status |
|------------|---------------|-------------|
| OBP-300010 | Branch not found. Please specify a valid value for BRANCH_ID. Or License may not be set. | 404 |
| OBP-30015 | Could not insert the Branch | 400 |
| OBP-30016 | Could not update the Branch | 400 |
| OBP-30209 | Insufficient authorisation to Create Branch. You do not have the role CanCreateBranch. | 403 |
| OBP-30218 | Insufficient authorisation to Delete Branch. You do not have the role CanDeleteBranch. | 403 |
| OBP-32001 | No branches available. License may not be set. | 204 |
| OBP-32002 | No branches available. | 204 |
| OBP-10001 | Incorrect json format. | 400 |
| OBP-20001 | User not logged in. Authentication is required! | 401 |
| OBP-20006 | User is missing one or more roles | 403 |
| OBP-20010 | Value too long | 400 |

---

## Notes

- The Scala codebase uses the Lift Framework's Box pattern for validation results (Full, Empty, Failure)
- All branch operations are routed through the OBP connector framework which can integrate with various backend banking systems
- Branch data is persisted using Lift Mapper ORM to the configured database
- The system supports both deprecated string-based hours (lobbyString, driveUpString) and structured time objects (lobby, driveUp) for backward compatibility
- Public access to branch retrieval can be configured via the `apiOptions.getBranchesIsPublic` configuration property
