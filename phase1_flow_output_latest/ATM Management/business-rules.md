# Business Rules Extraction

**Extracted From**: ATM Management Capability (User Story)
**Analysis Date**: 2026-01-20
**Analyst**: Expert Scala Business Logic Analyst
**Migration Target**: Go Application

## Executive Summary
- Total Business Rules Extracted: 12
- API Endpoints Analyzed: 5
- Rule Categories:
  - Calculations: 0
  - Decisions: 3
  - Thresholds: 2
  - Aggregations: 0
  - Workflows: 3
  - Transformations: 4

## Business Rules Catalog

### BR-001: ATM Bank Association Validation

**Category**: DECISION

**Description**: Every ATM must be associated with a valid, existing bank entity before it can be created or managed in the system.

**Source**: 
- File: ATM Service Layer
- Class/Object: ATMService
- Method: createATM, updateATM
- Lines: N/A (derived from user story)

**Business Logic**:
1. When creating or updating an ATM, the system must verify that the specified BANK_ID exists in the bank repository
2. If the bank does not exist, the operation must be rejected with an appropriate error
3. This ensures data integrity and prevents orphaned ATM records

**Variables**:
- **Input**: BANK_ID (string) - The unique identifier of the bank
- **Output**: Boolean validation result indicating if the bank exists
- **Constants**: None

**Business Conditions**:
| Condition | Business Meaning | Values/Thresholds |
|-----------|------------------|-------------------|
| bank_exists(BANK_ID) == true | Bank is registered in the system | Valid bank ID required |
| bank_exists(BANK_ID) == false | Bank not found | Operation rejected |

**Business Impact**: 
Ensures ATMs are always linked to valid banking institutions, maintaining data integrity across the ATM network and preventing customer confusion from orphaned ATM records.

**API Endpoints Using This Rule**:
- POST /obp/v5.1.0/banks/{BANK_ID}/atms - Create ATM
- PUT /obp/v5.1.0/banks/{BANK_ID}/atms/{ATM_ID} - Update ATM

**Related Test Cases**:
- Test ATM creation with valid bank ID
- Test ATM creation with invalid bank ID (should fail)

**Migration Notes for Go**:
- Implement bank existence check before ATM operations
- Use repository pattern for bank validation
- Return appropriate HTTP 404 error when bank not found

**Example Scenarios**:
```
Scenario 1: Valid bank association
Input: BANK_ID = "bank-id-123" (exists in system)
Processing: Check bank repository for BANK_ID
Output: Validation passes, proceed with ATM creation

Scenario 2: Invalid bank association
Input: BANK_ID = "non-existent-bank" (does not exist)
Processing: Check bank repository for BANK_ID
Output: Validation fails, return error "Bank not found"
```

---

### BR-002: ATM ID Uniqueness Within Bank

**Category**: DECISION

**Description**: ATM identifiers must be unique within the scope of a single bank to prevent duplicate ATM records and ensure accurate ATM identification.

**Source**: 
- File: ATM Service Layer
- Class/Object: ATMService
- Method: createATM
- Lines: N/A (derived from user story)

**Business Logic**:
1. When creating a new ATM, check if an ATM with the same ID already exists for the specified bank
2. If a duplicate ID is found, reject the creation request
3. ATM IDs may be reused across different banks but must be unique within a single bank

**Variables**:
- **Input**: ATM_ID (string), BANK_ID (string)
- **Output**: Boolean indicating if the ATM ID is unique within the bank
- **Constants**: None

**Business Conditions**:
| Condition | Business Meaning | Values/Thresholds |
|-----------|------------------|-------------------|
| atm_exists(BANK_ID, ATM_ID) == false | ATM ID is available | Unique within bank |
| atm_exists(BANK_ID, ATM_ID) == true | Duplicate ATM ID | Creation rejected |

**Business Impact**: 
Prevents duplicate ATM records that could cause confusion in ATM locator services and operational reporting.

**API Endpoints Using This Rule**:
- POST /obp/v5.1.0/banks/{BANK_ID}/atms - Create ATM

**Related Test Cases**:
- Test ATM creation with unique ID
- Test ATM creation with duplicate ID (should fail)

**Migration Notes for Go**:
- Implement composite key check (BANK_ID + ATM_ID)
- Use database unique constraint or application-level validation
- Return HTTP 409 Conflict for duplicate ATM IDs

**Example Scenarios**:
```
Scenario 1: Unique ATM ID
Input: BANK_ID = "bank-123", ATM_ID = "atm-001" (does not exist for this bank)
Processing: Query ATM repository for existing ATM with same ID in bank
Output: Validation passes, ATM can be created

Scenario 2: Duplicate ATM ID
Input: BANK_ID = "bank-123", ATM_ID = "atm-001" (already exists for this bank)
Processing: Query ATM repository for existing ATM with same ID in bank
Output: Validation fails, return "ATM ID already exists for this bank"
```

---

### BR-003: Geographic Coordinate Validation

**Category**: THRESHOLD

**Description**: ATM location coordinates must fall within valid geographic ranges to ensure accurate mapping and location services.

**Source**: 
- File: ATM Service Layer
- Class/Object: ATMService / LocationValidator
- Method: validateLocation
- Lines: N/A (derived from user story)

**Business Logic**:
1. Latitude values must be between -90 and 90 degrees (inclusive)
2. Longitude values must be between -180 and 180 degrees (inclusive)
3. Both latitude and longitude are required for ATM location
4. Invalid coordinates must be rejected to prevent mapping errors

**Variables**:
- **Input**: latitude (double), longitude (double)
- **Output**: Boolean indicating if coordinates are valid
- **Constants**: 
  - MIN_LATITUDE = -90
  - MAX_LATITUDE = 90
  - MIN_LONGITUDE = -180
  - MAX_LONGITUDE = 180

**Business Conditions**:
| Condition | Business Meaning | Values/Thresholds |
|-----------|------------------|-------------------|
| -90 <= latitude <= 90 | Valid latitude range | -90 to 90 degrees |
| -180 <= longitude <= 180 | Valid longitude range | -180 to 180 degrees |

**Business Impact**: 
Ensures ATM locations can be accurately displayed on maps and used by ATM locator features in mobile banking applications.

**API Endpoints Using This Rule**:
- POST /obp/v5.1.0/banks/{BANK_ID}/atms - Create ATM
- PUT /obp/v5.1.0/banks/{BANK_ID}/atms/{ATM_ID} - Update ATM

**Related Test Cases**:
- Test ATM creation with valid coordinates
- Test ATM creation with latitude > 90 (should fail)
- Test ATM creation with longitude < -180 (should fail)

**Migration Notes for Go**:
- Implement coordinate validation function
- Use float64 for coordinate precision
- Return descriptive error messages for invalid coordinates

**Example Scenarios**:
```
Scenario 1: Valid coordinates (Berlin)
Input: latitude = 52.520008, longitude = 13.404954
Processing: Check -90 <= 52.520008 <= 90 AND -180 <= 13.404954 <= 180
Output: Validation passes

Scenario 2: Invalid latitude
Input: latitude = 95.0, longitude = 13.404954
Processing: Check -90 <= 95.0 <= 90 (fails)
Output: Validation fails, return "Latitude must be between -90 and 90"

Scenario 3: Invalid longitude
Input: latitude = 52.520008, longitude = -200.0
Processing: Check -180 <= -200.0 <= 180 (fails)
Output: Validation fails, return "Longitude must be between -180 and 180"
```

---

### BR-004: ISO Language Code Validation

**Category**: TRANSFORMATION

**Description**: Supported languages for an ATM must be specified using valid ISO 639-1 two-letter language codes to ensure standardization across the system.

**Source**: 
- File: ATM Service Layer
- Class/Object: ATMService / LanguageValidator
- Method: validateLanguages
- Lines: N/A (derived from user story)

**Business Logic**:
1. Each language code in the supported_languages array must be a valid ISO 639-1 code
2. Language codes should be lowercase two-letter codes (e.g., "en", "de", "fr")
3. Invalid language codes must be rejected
4. Empty language list may be allowed (ATM with no specific language support documented)

**Variables**:
- **Input**: supported_languages (array of strings)
- **Output**: Boolean indicating if all language codes are valid
- **Constants**: ISO 639-1 language code set

**Business Conditions**:
| Condition | Business Meaning | Values/Thresholds |
|-----------|------------------|-------------------|
| code in ISO_639_1_CODES | Valid language code | Standard ISO codes |
| code not in ISO_639_1_CODES | Invalid language code | Rejected |

**Business Impact**: 
Enables customers to find ATMs that support their preferred language, improving customer experience and accessibility.

**API Endpoints Using This Rule**:
- POST /obp/v5.1.0/banks/{BANK_ID}/atms - Create ATM
- PUT /obp/v5.1.0/banks/{BANK_ID}/atms/{ATM_ID} - Update ATM

**Related Test Cases**:
- Test ATM creation with valid language codes ["en", "de", "fr"]
- Test ATM creation with invalid language code ["xyz"] (should fail)

**Migration Notes for Go**:
- Implement ISO 639-1 validation using a predefined set or library
- Consider using a map for O(1) lookup of valid codes
- Normalize input to lowercase before validation

**Example Scenarios**:
```
Scenario 1: Valid language codes
Input: supported_languages = ["en", "de", "fr"]
Processing: Validate each code against ISO 639-1 standard
Output: All codes valid, validation passes

Scenario 2: Invalid language code
Input: supported_languages = ["en", "xyz", "de"]
Processing: Validate each code - "xyz" not found in ISO 639-1
Output: Validation fails, return "Invalid language code: xyz"
```

---

### BR-005: ISO Currency Code Validation

**Category**: TRANSFORMATION

**Description**: Supported currencies for an ATM must be specified using valid ISO 4217 three-letter currency codes to ensure standardization and accurate currency handling.

**Source**: 
- File: ATM Service Layer
- Class/Object: ATMService / CurrencyValidator
- Method: validateCurrencies
- Lines: N/A (derived from user story)

**Business Logic**:
1. Each currency code in the supported_currencies array must be a valid ISO 4217 code
2. Currency codes should be uppercase three-letter codes (e.g., "EUR", "USD", "GBP")
3. Invalid currency codes must be rejected
4. At least one currency should typically be supported by an ATM

**Variables**:
- **Input**: supported_currencies (array of strings)
- **Output**: Boolean indicating if all currency codes are valid
- **Constants**: ISO 4217 currency code set

**Business Conditions**:
| Condition | Business Meaning | Values/Thresholds |
|-----------|------------------|-------------------|
| code in ISO_4217_CODES | Valid currency code | Standard ISO codes |
| code not in ISO_4217_CODES | Invalid currency code | Rejected |

**Business Impact**: 
Enables customers to find ATMs that dispense their required currency, essential for international travelers and multi-currency banking.

**API Endpoints Using This Rule**:
- POST /obp/v5.1.0/banks/{BANK_ID}/atms - Create ATM
- PUT /obp/v5.1.0/banks/{BANK_ID}/atms/{ATM_ID} - Update ATM

**Related Test Cases**:
- Test ATM creation with valid currency codes ["EUR", "USD", "GBP"]
- Test ATM creation with invalid currency code ["XXX"] (should fail)

**Migration Notes for Go**:
- Implement ISO 4217 validation using a predefined set or library
- Normalize input to uppercase before validation
- Consider maintaining a currency code lookup table

**Example Scenarios**:
```
Scenario 1: Valid currency codes
Input: supported_currencies = ["EUR", "USD", "GBP"]
Processing: Validate each code against ISO 4217 standard
Output: All codes valid, validation passes

Scenario 2: Invalid currency code
Input: supported_currencies = ["EUR", "ABC"]
Processing: Validate each code - "ABC" not found in ISO 4217
Output: Validation fails, return "Invalid currency code: ABC"
```

---

### BR-006: Operating Hours Time Format Validation

**Category**: THRESHOLD

**Description**: ATM operating hours must be specified in valid 24-hour time format (HH:MM) or marked as "closed" to ensure consistent scheduling information.

**Source**: 
- File: ATM Service Layer
- Class/Object: ATMService / TimeValidator
- Method: validateOperatingHours
- Lines: N/A (derived from user story)

**Business Logic**:
1. Time values must be in HH:MM format (24-hour clock)
2. Hours must be between 00 and 23
3. Minutes must be between 00 and 59
4. Special value "closed" indicates the ATM is not operational on that day
5. Opening time must be before closing time (unless 24-hour operation)

**Variables**:
- **Input**: opening_time (string), closing_time (string) for each day
- **Output**: Boolean indicating if time format is valid
- **Constants**: 
  - TIME_FORMAT_REGEX = "^([01]?[0-9]|2[0-3]):[0-5][0-9]$"
  - CLOSED_VALUE = "closed"

**Business Conditions**:
| Condition | Business Meaning | Values/Thresholds |
|-----------|------------------|-------------------|
| time matches HH:MM format | Valid time format | 00:00 to 23:59 |
| time == "closed" | ATM not operational | Day is closed |
| opening_time < closing_time | Valid operating window | Logical time range |

**Business Impact**: 
Provides accurate operating hours information to customers, preventing wasted trips to closed ATMs and improving customer satisfaction.

**API Endpoints Using This Rule**:
- POST /obp/v5.1.0/banks/{BANK_ID}/atms - Create ATM
- PUT /obp/v5.1.0/banks/{BANK_ID}/atms/{ATM_ID} - Update ATM

**Related Test Cases**:
- Test ATM creation with valid time format "08:00" to "20:00"
- Test ATM creation with "closed" value
- Test ATM creation with invalid time "25:00" (should fail)

**Migration Notes for Go**:
- Use regex or time parsing for validation
- Handle "closed" as a special case
- Consider using time.Parse with "15:04" layout

**Example Scenarios**:
```
Scenario 1: Valid operating hours
Input: opening_time = "08:00", closing_time = "20:00"
Processing: Validate format and logical order
Output: Validation passes

Scenario 2: Closed day
Input: opening_time = "closed", closing_time = "closed"
Processing: Recognize "closed" as valid special value
Output: Validation passes, ATM marked as closed for this day

Scenario 3: Invalid time format
Input: opening_time = "8:00 AM", closing_time = "8:00 PM"
Processing: Format does not match HH:MM pattern
Output: Validation fails, return "Invalid time format, use HH:MM"
```

---

### BR-007: ATM Creation Workflow

**Category**: WORKFLOW

**Description**: The complete workflow for creating a new ATM record, including all validations and data persistence.

**Source**: 
- File: ATM Service Layer
- Class/Object: ATMService
- Method: createATM
- Lines: N/A (derived from user story)

**Business Logic**:
1. Verify user has authorization to create ATMs (entitlement check)
2. Validate that the specified bank exists (BR-001)
3. Validate ATM ID uniqueness within the bank (BR-002)
4. Validate geographic coordinates (BR-003)
5. Validate language codes if provided (BR-004)
6. Validate currency codes if provided (BR-005)
7. Validate operating hours format (BR-006)
8. Validate required fields are present (ATM ID, Bank ID, name, location)
9. Persist ATM record to database
10. Return created ATM with all details

**Variables**:
- **Input**: ATM creation request with all ATM attributes
- **Output**: Created ATM record with assigned ID and all details
- **Constants**: None

**Business Conditions**:
| Condition | Business Meaning | Values/Thresholds |
|-----------|------------------|-------------------|
| user.hasEntitlement("CanCreateAtm") | User authorized | Required permission |
| all validations pass | Data is valid | All rules satisfied |

**Business Impact**: 
Ensures new ATMs are properly validated and recorded in the system, maintaining data quality and enabling accurate ATM network management.

**API Endpoints Using This Rule**:
- POST /obp/v5.1.0/banks/{BANK_ID}/atms - Create ATM

**Related Test Cases**:
- Test complete ATM creation workflow
- Test creation with missing required fields
- Test creation without proper authorization

**Migration Notes for Go**:
- Implement as a service method with transaction support
- Use dependency injection for validators
- Return appropriate HTTP status codes (201 Created, 400 Bad Request, 401 Unauthorized, 404 Not Found)

**Example Scenarios**:
```
Scenario 1: Successful ATM creation
Input: Valid ATM request with all required fields
Processing: All validations pass, ATM persisted
Output: HTTP 201, ATM record returned

Scenario 2: Missing required field
Input: ATM request without location
Processing: Required field validation fails
Output: HTTP 400, error "Location is required"
```

---

### BR-008: ATM Update Workflow

**Category**: WORKFLOW

**Description**: The complete workflow for updating an existing ATM record, including validation of changes and data persistence.

**Source**: 
- File: ATM Service Layer
- Class/Object: ATMService
- Method: updateATM
- Lines: N/A (derived from user story)

**Business Logic**:
1. Verify user has authorization to update ATMs (entitlement check)
2. Validate that the specified bank exists (BR-001)
3. Validate that the ATM exists for the specified bank
4. Validate geographic coordinates if being updated (BR-003)
5. Validate language codes if being updated (BR-004)
6. Validate currency codes if being updated (BR-005)
7. Validate operating hours format if being updated (BR-006)
8. Update ATM record in database
9. Return updated ATM with all current details

**Variables**:
- **Input**: ATM update request with updated attributes, BANK_ID, ATM_ID
- **Output**: Updated ATM record with all current details
- **Constants**: None

**Business Conditions**:
| Condition | Business Meaning | Values/Thresholds |
|-----------|------------------|-------------------|
| user.hasEntitlement("CanUpdateAtm") | User authorized | Required permission |
| atm_exists(BANK_ID, ATM_ID) | ATM found | Must exist to update |

**Business Impact**: 
Enables maintenance of accurate ATM information as locations, services, and features change over time.

**API Endpoints Using This Rule**:
- PUT /obp/v5.1.0/banks/{BANK_ID}/atms/{ATM_ID} - Update ATM

**Related Test Cases**:
- Test complete ATM update workflow
- Test update of non-existent ATM (should fail)
- Test update without proper authorization

**Migration Notes for Go**:
- Implement as a service method with transaction support
- Handle partial updates if supported
- Return appropriate HTTP status codes (200 OK, 400 Bad Request, 401 Unauthorized, 404 Not Found)

**Example Scenarios**:
```
Scenario 1: Successful ATM update
Input: Valid update request for existing ATM
Processing: ATM found, validations pass, ATM updated
Output: HTTP 200, updated ATM record returned

Scenario 2: ATM not found
Input: Update request for non-existent ATM_ID
Processing: ATM lookup fails
Output: HTTP 404, error "ATM not found"
```

---

### BR-009: ATM Deletion Workflow

**Category**: WORKFLOW

**Description**: The complete workflow for deleting an ATM record from the system.

**Source**: 
- File: ATM Service Layer
- Class/Object: ATMService
- Method: deleteATM
- Lines: N/A (derived from user story)

**Business Logic**:
1. Verify user has authorization to delete ATMs (entitlement check)
2. Validate that the specified bank exists
3. Validate that the ATM exists for the specified bank
4. Remove ATM record from database
5. Return success confirmation

**Variables**:
- **Input**: BANK_ID, ATM_ID
- **Output**: Success confirmation or error message
- **Constants**: None

**Business Conditions**:
| Condition | Business Meaning | Values/Thresholds |
|-----------|------------------|-------------------|
| user.hasEntitlement("CanDeleteAtm") | User authorized | Required permission |
| atm_exists(BANK_ID, ATM_ID) | ATM found | Must exist to delete |

**Business Impact**: 
Allows removal of ATMs that are no longer active, keeping the ATM network information current and accurate for customers.

**API Endpoints Using This Rule**:
- DELETE /obp/v5.1.0/banks/{BANK_ID}/atms/{ATM_ID} - Delete ATM

**Related Test Cases**:
- Test complete ATM deletion workflow
- Test deletion of non-existent ATM (should fail)
- Test deletion without proper authorization

**Migration Notes for Go**:
- Implement as a service method
- Consider soft delete vs hard delete based on audit requirements
- Return appropriate HTTP status codes (204 No Content, 401 Unauthorized, 404 Not Found)

**Example Scenarios**:
```
Scenario 1: Successful ATM deletion
Input: BANK_ID = "bank-123", ATM_ID = "atm-001" (exists)
Processing: ATM found, user authorized, ATM deleted
Output: HTTP 204, no content

Scenario 2: ATM not found
Input: BANK_ID = "bank-123", ATM_ID = "non-existent"
Processing: ATM lookup fails
Output: HTTP 404, error "ATM not found"
```

---

### BR-010: Accessibility Features Documentation

**Category**: TRANSFORMATION

**Description**: ATM accessibility features must be documented using standardized feature codes to ensure consistent accessibility information across the ATM network.

**Source**: 
- File: ATM Service Layer
- Class/Object: ATMService
- Method: validateAccessibilityFeatures
- Lines: N/A (derived from user story)

**Business Logic**:
1. Accessibility features should be specified using predefined feature codes
2. Valid features include: WHEELCHAIR_ACCESS, AUDIO_GUIDANCE, BRAILLE_KEYPAD, and others as defined
3. The is_accessible flag provides a quick indicator of general accessibility
4. Detailed features provide specific accessibility capabilities

**Variables**:
- **Input**: accessibility_features (array of strings), is_accessible (boolean)
- **Output**: Validated accessibility information
- **Constants**: Valid accessibility feature codes (WHEELCHAIR_ACCESS, AUDIO_GUIDANCE, BRAILLE_KEYPAD, etc.)

**Business Conditions**:
| Condition | Business Meaning | Values/Thresholds |
|-----------|------------------|-------------------|
| feature in VALID_FEATURES | Valid accessibility feature | Predefined codes |
| is_accessible == true | ATM has accessibility support | General indicator |

**Business Impact**: 
Ensures customers with disabilities can find ATMs that meet their accessibility needs, supporting regulatory compliance and inclusive banking services.

**API Endpoints Using This Rule**:
- POST /obp/v5.1.0/banks/{BANK_ID}/atms - Create ATM
- PUT /obp/v5.1.0/banks/{BANK_ID}/atms/{ATM_ID} - Update ATM
- GET /obp/v5.1.0/banks/{BANK_ID}/atms/{ATM_ID} - Retrieve ATM
- GET /obp/v5.1.0/banks/{BANK_ID}/atms - List ATMs (for filtering)

**Related Test Cases**:
- Test ATM creation with valid accessibility features
- Test ATM retrieval includes accessibility information

**Migration Notes for Go**:
- Define accessibility feature enum or constants
- Validate features against allowed values
- Consider future extensibility for new accessibility features

**Example Scenarios**:
```
Scenario 1: ATM with full accessibility
Input: is_accessible = true, accessibility_features = ["WHEELCHAIR_ACCESS", "AUDIO_GUIDANCE", "BRAILLE_KEYPAD"]
Processing: Validate all features are recognized
Output: ATM marked as accessible with specific features

Scenario 2: ATM with limited accessibility
Input: is_accessible = true, accessibility_features = ["WHEELCHAIR_ACCESS"]
Processing: Validate feature is recognized
Output: ATM marked as accessible with wheelchair access only
```

---

### BR-011: ATM Fee Transparency

**Category**: TRANSFORMATION

**Description**: ATM fee information must be clearly documented for regulatory compliance and customer transparency.

**Source**: 
- File: ATM Service Layer
- Class/Object: ATMService
- Method: createATM, updateATM
- Lines: N/A (derived from user story)

**Business Logic**:
1. Cash withdrawal fees should be documented separately for national and international transactions
2. Balance inquiry fees should be documented
3. Fee amounts should be stored as string representations of decimal values
4. Fee information enables customers to make informed decisions about ATM usage

**Variables**:
- **Input**: cash_withdrawal_national_fee, cash_withdrawal_international_fee, balance_inquiry_fee (strings)
- **Output**: Stored fee information
- **Constants**: None

**Business Conditions**:
| Condition | Business Meaning | Values/Thresholds |
|-----------|------------------|-------------------|
| fee >= "0.00" | Valid fee amount | Non-negative value |
| fee format valid | Proper decimal format | e.g., "2.50" |

**Business Impact**: 
Supports regulatory requirements for fee disclosure and enables customers to compare ATM costs, promoting transparency in banking services.

**API Endpoints Using This Rule**:
- POST /obp/v5.1.0/banks/{BANK_ID}/atms - Create ATM
- PUT /obp/v5.1.0/banks/{BANK_ID}/atms/{ATM_ID} - Update ATM
- GET /obp/v5.1.0/banks/{BANK_ID}/atms/{ATM_ID} - Retrieve ATM

**Related Test Cases**:
- Test ATM creation with fee information
- Test ATM retrieval includes fee details

**Migration Notes for Go**:
- Store fees as strings to preserve decimal precision
- Consider using decimal library for fee calculations if needed
- Validate fee format on input

**Example Scenarios**:
```
Scenario 1: ATM with standard fees
Input: cash_withdrawal_national_fee = "0.00", cash_withdrawal_international_fee = "2.50", balance_inquiry_fee = "0.00"
Processing: Store fee information with ATM record
Output: ATM record includes transparent fee information

Scenario 2: ATM with all free services
Input: cash_withdrawal_national_fee = "0.00", cash_withdrawal_international_fee = "0.00", balance_inquiry_fee = "0.00"
Processing: Store zero fees
Output: ATM marked as having no fees for listed services
```

---

### BR-012: Authorization Entitlement Check

**Category**: DECISION

**Description**: Users must have appropriate entitlements/permissions to perform create, update, or delete operations on ATM records.

**Source**: 
- File: ATM Service Layer / Authorization Service
- Class/Object: ATMService, AuthorizationService
- Method: createATM, updateATM, deleteATM
- Lines: N/A (derived from user story)

**Business Logic**:
1. Create operations require "CanCreateAtm" or equivalent entitlement
2. Update operations require "CanUpdateAtm" or equivalent entitlement
3. Delete operations require "CanDeleteAtm" or equivalent entitlement
4. Read operations (GET) may have different or no entitlement requirements
5. Unauthorized requests must be rejected with appropriate error response

**Variables**:
- **Input**: User context with entitlements, requested operation
- **Output**: Boolean authorization decision
- **Constants**: Entitlement names (CanCreateAtm, CanUpdateAtm, CanDeleteAtm)

**Business Conditions**:
| Condition | Business Meaning | Values/Thresholds |
|-----------|------------------|-------------------|
| user.hasEntitlement(required) | User is authorized | Has required permission |
| !user.hasEntitlement(required) | User not authorized | Missing permission |

**Business Impact**: 
Ensures only authorized bank administrators and operations managers can modify ATM network information, maintaining data security and operational control.

**API Endpoints Using This Rule**:
- POST /obp/v5.1.0/banks/{BANK_ID}/atms - Create ATM (requires create entitlement)
- PUT /obp/v5.1.0/banks/{BANK_ID}/atms/{ATM_ID} - Update ATM (requires update entitlement)
- DELETE /obp/v5.1.0/banks/{BANK_ID}/atms/{ATM_ID} - Delete ATM (requires delete entitlement)

**Related Test Cases**:
- Test ATM creation with authorized user
- Test ATM creation with unauthorized user (should fail)
- Test ATM update with authorized user
- Test ATM deletion with authorized user

**Migration Notes for Go**:
- Implement middleware or decorator pattern for authorization checks
- Use context to pass user entitlements
- Return HTTP 401 Unauthorized or 403 Forbidden as appropriate

**Example Scenarios**:
```
Scenario 1: Authorized user creates ATM
Input: User with "CanCreateAtm" entitlement, valid ATM request
Processing: Check user entitlements, find required permission
Output: Authorization passes, proceed with creation

Scenario 2: Unauthorized user attempts creation
Input: User without "CanCreateAtm" entitlement, valid ATM request
Processing: Check user entitlements, required permission not found
Output: HTTP 403, error "User not authorized to create ATMs"
```

---

## API Endpoint Coverage

| Endpoint | HTTP Method | Business Rules | Rule IDs |
|----------|-------------|----------------|----------|
| /obp/v5.1.0/banks/{BANK_ID}/atms | POST | Bank validation, ID uniqueness, coordinate validation, language validation, currency validation, time validation, accessibility, fees, authorization | BR-001, BR-002, BR-003, BR-004, BR-005, BR-006, BR-007, BR-010, BR-011, BR-012 |
| /obp/v5.1.0/banks/{BANK_ID}/atms/{ATM_ID} | PUT | Bank validation, coordinate validation, language validation, currency validation, time validation, accessibility, fees, authorization | BR-001, BR-003, BR-004, BR-005, BR-006, BR-008, BR-010, BR-011, BR-012 |
| /obp/v5.1.0/banks/{BANK_ID}/atms/{ATM_ID} | GET | Data retrieval with accessibility and fee information | BR-010, BR-011 |
| /obp/v5.1.0/banks/{BANK_ID}/atms | GET | List ATMs with filtering capabilities | BR-010, BR-011 |
| /obp/v5.1.0/banks/{BANK_ID}/atms/{ATM_ID} | DELETE | ATM deletion workflow, authorization | BR-009, BR-012 |

## Migration Validation Matrix

| Rule ID | Test Case Reference | Go Implementation Status | Validation Status |
|---------|---------------------|--------------------------|-------------------|
| BR-001 | TestBankAssociationValidation | Pending | Pending |
| BR-002 | TestATMIDUniqueness | Pending | Pending |
| BR-003 | TestGeographicCoordinateValidation | Pending | Pending |
| BR-004 | TestISOLanguageCodeValidation | Pending | Pending |
| BR-005 | TestISOCurrencyCodeValidation | Pending | Pending |
| BR-006 | TestOperatingHoursValidation | Pending | Pending |
| BR-007 | TestATMCreationWorkflow | Pending | Pending |
| BR-008 | TestATMUpdateWorkflow | Pending | Pending |
| BR-009 | TestATMDeletionWorkflow | Pending | Pending |
| BR-010 | TestAccessibilityFeatures | Pending | Pending |
| BR-011 | TestFeeTransparency | Pending | Pending |
| BR-012 | TestAuthorizationEntitlements | Pending | Pending |

## Notes and Assumptions

1. **Source Code Availability**: Business rules were extracted from the user story document as the actual Scala source code was not provided. Rules are based on the documented requirements and acceptance criteria.

2. **Entitlement Names**: The specific entitlement names (CanCreateAtm, CanUpdateAtm, CanDeleteAtm) are assumed based on common patterns. Actual entitlement names should be verified against the Scala implementation.

3. **Accessibility Features**: The complete list of valid accessibility features should be confirmed with SME input as noted in the user story.

4. **Fee Handling**: Fees are stored as strings to preserve decimal precision. The actual calculation or application of fees is not covered in this capability.

5. **Audit Trail**: The user story mentions considering audit logging for ATM changes. This is an implementation consideration but not extracted as a business rule.

6. **Multi-language Support**: The user story mentions supporting ATM information in multiple languages. This may require additional business rules for internationalization.

7. **Holiday Hours**: The user story mentions handling special hours for holidays. This may require additional business rules for seasonal variations.
