# Business Rules Extraction

**Extracted From**: ATM Management User Story
**Analysis Date**: 2026-02-04
**Analyst**: Expert Business Logic Analyst
**Migration Target**: Go Application

## Executive Summary
- Total Business Rules Extracted: 9
- API Endpoints Analyzed: 5
- Rule Categories:
  - Calculations: 0
  - Decisions: 2
  - Thresholds: 0
  - Aggregations: 0
  - Workflows: 0
  - Transformations: 7

## Business Rules Catalog

### BR-001: Bank Association Rule

**Category**: DECISION

**Description**: ATMs must be associated with a valid bank entity before creation. The BANK_ID must exist in the system before an ATM can be created for that bank.

**Source**: 
- File: ATM Management User Story
- Section: Business Rules
- Rule: Bank Association Rule

**Business Logic**:
1. When creating an ATM, the system receives a BANK_ID parameter
2. The system validates that the specified BANK_ID exists in the bank repository
3. If the bank does not exist, the ATM creation is rejected with HTTP 404 Not Found
4. If the bank exists, the ATM creation proceeds with the bank association

**Variables**:
- **Input**: BANK_ID (bank identifier from path parameter)
- **Output**: Validation result (success/failure)
- **Constants**: None

**Business Conditions**:
| Condition | Business Meaning | Values/Thresholds |
|-----------|------------------|-------------------|
| BANK_ID exists | Bank is registered in the system | Valid bank identifier |
| BANK_ID not found | Bank is not registered | HTTP 404 response |

**Business Impact**: 
Ensures data integrity by preventing orphaned ATM records. All ATMs must belong to a valid bank for proper operational management and reporting.

**API Endpoints Using This Rule**:
- POST /obp/v5.1.0/banks/{BANK_ID}/atms - Create ATM
- PUT /obp/v5.1.0/banks/{BANK_ID}/atms/{ATM_ID} - Update ATM
- GET /obp/v5.1.0/banks/{BANK_ID}/atms/{ATM_ID} - Retrieve ATM
- GET /obp/v5.1.0/banks/{BANK_ID}/atms - List ATMs
- DELETE /obp/v5.1.0/banks/{BANK_ID}/atms/{ATM_ID} - Delete ATM

**Migration Notes for Go**:
- Implement bank existence check as a prerequisite validation in the ATM service layer
- Use repository pattern to query bank existence before ATM operations
- Return appropriate HTTP status codes (404 for not found)

**Example Scenarios**:
```
Scenario 1: Valid bank association
Input: BANK_ID = "bank-id-123" (exists in system)
Processing: Query bank repository for BANK_ID
Output: Validation passes, proceed with ATM operation

Scenario 2: Invalid bank association
Input: BANK_ID = "non-existent-bank"
Processing: Query bank repository for BANK_ID
Output: HTTP 404 Not Found - Bank does not exist
```

---

### BR-002: ATM ID Uniqueness Rule

**Category**: DECISION

**Description**: ATM IDs must be unique within a bank to prevent duplicate records. No two ATMs belonging to the same bank can have the same ATM ID.

**Source**: 
- File: ATM Management User Story
- Section: Business Rules
- Rule: ATM ID Uniqueness

**Business Logic**:
1. When creating a new ATM, the system receives an ATM ID
2. The system checks if an ATM with the same ID already exists for the specified bank
3. If a duplicate is found, the creation is rejected with HTTP 409 Conflict
4. If no duplicate exists, the ATM creation proceeds

**Variables**:
- **Input**: ATM_ID (ATM identifier), BANK_ID (bank identifier)
- **Output**: Uniqueness validation result (success/conflict)
- **Constants**: None

**Business Conditions**:
| Condition | Business Meaning | Values/Thresholds |
|-----------|------------------|-------------------|
| ATM_ID is unique within bank | No duplicate ATM record | Unique identifier |
| ATM_ID already exists | Duplicate ATM record detected | HTTP 409 Conflict |

**Business Impact**: 
Prevents data corruption and ensures each ATM can be uniquely identified for operations, maintenance tracking, and customer service purposes.

**API Endpoints Using This Rule**:
- POST /obp/v5.1.0/banks/{BANK_ID}/atms - Create ATM

**Migration Notes for Go**:
- Implement uniqueness check at the service layer before database insertion
- Consider using database unique constraints as a secondary safeguard
- Return HTTP 409 Conflict with appropriate error message for duplicates

**Example Scenarios**:
```
Scenario 1: Unique ATM ID
Input: BANK_ID = "bank-123", ATM_ID = "atm-new-001"
Processing: Check ATM repository for existing ATM with same ID in bank
Output: No duplicate found, proceed with creation

Scenario 2: Duplicate ATM ID
Input: BANK_ID = "bank-123", ATM_ID = "atm-existing-001"
Processing: Check ATM repository for existing ATM with same ID in bank
Output: HTTP 409 Conflict - ATM ID already exists for this bank
```

---

### BR-003: Location Requirement Rule

**Category**: TRANSFORMATION

**Description**: ATM location information must include geographic coordinates (latitude and longitude) for mapping purposes. This enables customers to locate ATMs using mapping applications.

**Source**: 
- File: ATM Management User Story
- Section: Business Rules
- Rule: Location Requirement

**Business Logic**:
1. ATM creation/update requests must include location object with latitude and longitude
2. Latitude must be a valid value between -90 and 90 degrees
3. Longitude must be a valid value between -180 and 180 degrees
4. Coordinates should have sufficient precision (at least 6 decimal places) for accurate mapping

**Variables**:
- **Input**: location.latitude (decimal), location.longitude (decimal)
- **Output**: Validated geographic coordinates
- **Constants**: Latitude range: -90 to 90, Longitude range: -180 to 180

**Business Conditions**:
| Condition | Business Meaning | Values/Thresholds |
|-----------|------------------|-------------------|
| latitude in valid range | Valid geographic latitude | -90 to 90 degrees |
| longitude in valid range | Valid geographic longitude | -180 to 180 degrees |
| sufficient precision | Accurate location for mapping | 6+ decimal places recommended |

**Business Impact**: 
Enables ATM locator features in customer-facing applications and mobile banking apps. Accurate location data is essential for customer convenience and service accessibility.

**API Endpoints Using This Rule**:
- POST /obp/v5.1.0/banks/{BANK_ID}/atms - Create ATM
- PUT /obp/v5.1.0/banks/{BANK_ID}/atms/{ATM_ID} - Update ATM

**Migration Notes for Go**:
- Use float64 for latitude and longitude fields
- Implement range validation for geographic coordinates
- Consider using a dedicated geo-location struct or type

**Example Scenarios**:
```
Scenario 1: Valid location
Input: latitude = 52.520008, longitude = 13.404954
Processing: Validate latitude in [-90, 90], longitude in [-180, 180]
Output: Valid location coordinates accepted

Scenario 2: Invalid latitude
Input: latitude = 95.0, longitude = 13.404954
Processing: Validate latitude in [-90, 90]
Output: Validation error - latitude out of range
```

---

### BR-004: Language Code Standard Rule

**Category**: TRANSFORMATION

**Description**: Supported languages must be valid ISO 639-1 two-letter language codes. This ensures standardized language identification across the system.

**Source**: 
- File: ATM Management User Story
- Section: Business Rules
- Rule: Language Code Standard

**Business Logic**:
1. ATM records include a list of supported languages
2. Each language code must be a valid ISO 639-1 two-letter code
3. Common examples include: "en" (English), "de" (German), "fr" (French), "es" (Spanish)
4. Invalid language codes should be rejected during creation/update

**Variables**:
- **Input**: supported_languages (array of language codes)
- **Output**: Validated language code list
- **Constants**: ISO 639-1 language code set

**Business Conditions**:
| Condition | Business Meaning | Values/Thresholds |
|-----------|------------------|-------------------|
| Valid ISO 639-1 code | Recognized language identifier | Two-letter codes (e.g., "en", "de") |
| Invalid code | Unrecognized language | Validation error |

**Business Impact**: 
Ensures customers can identify ATMs that support their preferred language, improving accessibility and user experience for international customers.

**API Endpoints Using This Rule**:
- POST /obp/v5.1.0/banks/{BANK_ID}/atms - Create ATM
- PUT /obp/v5.1.0/banks/{BANK_ID}/atms/{ATM_ID} - Update ATM
- GET /obp/v5.1.0/banks/{BANK_ID}/atms/{ATM_ID} - Retrieve ATM
- GET /obp/v5.1.0/banks/{BANK_ID}/atms - List ATMs

**Migration Notes for Go**:
- Implement ISO 639-1 validation using a lookup table or library
- Store language codes as string array in the ATM model
- Consider using a dedicated language code type for type safety

**Example Scenarios**:
```
Scenario 1: Valid language codes
Input: supported_languages = ["en", "de", "fr"]
Processing: Validate each code against ISO 639-1 standard
Output: All codes valid, accepted

Scenario 2: Invalid language code
Input: supported_languages = ["en", "xyz", "fr"]
Processing: Validate each code against ISO 639-1 standard
Output: Validation error - "xyz" is not a valid ISO 639-1 code
```

---

### BR-005: Currency Code Standard Rule

**Category**: TRANSFORMATION

**Description**: Supported currencies must be valid ISO 4217 three-letter currency codes. This ensures standardized currency identification for cash availability.

**Source**: 
- File: ATM Management User Story
- Section: Business Rules
- Rule: Currency Code Standard

**Business Logic**:
1. ATM records include a list of supported currencies for cash dispensing
2. Each currency code must be a valid ISO 4217 three-letter code
3. Common examples include: "EUR" (Euro), "USD" (US Dollar), "GBP" (British Pound)
4. Invalid currency codes should be rejected during creation/update

**Variables**:
- **Input**: supported_currencies (array of currency codes)
- **Output**: Validated currency code list
- **Constants**: ISO 4217 currency code set

**Business Conditions**:
| Condition | Business Meaning | Values/Thresholds |
|-----------|------------------|-------------------|
| Valid ISO 4217 code | Recognized currency identifier | Three-letter codes (e.g., "EUR", "USD") |
| Invalid code | Unrecognized currency | Validation error |

**Business Impact**: 
Enables customers to identify ATMs that dispense their required currency, essential for travelers and international banking customers.

**API Endpoints Using This Rule**:
- POST /obp/v5.1.0/banks/{BANK_ID}/atms - Create ATM
- PUT /obp/v5.1.0/banks/{BANK_ID}/atms/{ATM_ID} - Update ATM
- GET /obp/v5.1.0/banks/{BANK_ID}/atms/{ATM_ID} - Retrieve ATM
- GET /obp/v5.1.0/banks/{BANK_ID}/atms - List ATMs

**Migration Notes for Go**:
- Implement ISO 4217 validation using a lookup table or library
- Store currency codes as string array in the ATM model
- Consider using a dedicated currency code type for type safety

**Example Scenarios**:
```
Scenario 1: Valid currency codes
Input: supported_currencies = ["EUR", "USD", "GBP"]
Processing: Validate each code against ISO 4217 standard
Output: All codes valid, accepted

Scenario 2: Invalid currency code
Input: supported_currencies = ["EUR", "XXX", "GBP"]
Processing: Validate each code against ISO 4217 standard
Output: Validation error - "XXX" is not a valid ISO 4217 code
```

---

### BR-006: Accessibility Documentation Rule

**Category**: TRANSFORMATION

**Description**: Accessibility features must be documented per ATM to comply with accessibility regulations and customer information requirements.

**Source**: 
- File: ATM Management User Story
- Section: Business Rules
- Rule: Accessibility Documentation

**Business Logic**:
1. ATM records should include accessibility feature flags
2. Standard accessibility features include: WHEELCHAIR_ACCESS, AUDIO_GUIDANCE, BRAILLE_KEYPAD
3. The is_accessible boolean flag provides a quick indicator of general accessibility
4. Detailed accessibility_features array provides specific feature information

**Variables**:
- **Input**: is_accessible (boolean), accessibility_features (array of feature codes)
- **Output**: Documented accessibility information
- **Constants**: Standard accessibility feature codes

**Business Conditions**:
| Condition | Business Meaning | Values/Thresholds |
|-----------|------------------|-------------------|
| is_accessible = true | ATM has accessibility features | Boolean flag |
| WHEELCHAIR_ACCESS | ATM is wheelchair accessible | Feature code |
| AUDIO_GUIDANCE | ATM has audio assistance | Feature code |
| BRAILLE_KEYPAD | ATM has braille keypad | Feature code |

**Business Impact**: 
Ensures compliance with accessibility regulations (e.g., ADA in US) and enables customers with disabilities to find suitable ATMs, improving service inclusivity.

**API Endpoints Using This Rule**:
- POST /obp/v5.1.0/banks/{BANK_ID}/atms - Create ATM
- PUT /obp/v5.1.0/banks/{BANK_ID}/atms/{ATM_ID} - Update ATM
- GET /obp/v5.1.0/banks/{BANK_ID}/atms/{ATM_ID} - Retrieve ATM
- GET /obp/v5.1.0/banks/{BANK_ID}/atms - List ATMs

**Migration Notes for Go**:
- Use boolean for is_accessible field
- Use string array for accessibility_features
- Consider defining an enum or constant set for valid accessibility feature codes

**Example Scenarios**:
```
Scenario 1: Fully accessible ATM
Input: is_accessible = true, accessibility_features = ["WHEELCHAIR_ACCESS", "AUDIO_GUIDANCE", "BRAILLE_KEYPAD"]
Processing: Store accessibility information
Output: ATM marked as accessible with all features documented

Scenario 2: Limited accessibility
Input: is_accessible = true, accessibility_features = ["AUDIO_GUIDANCE"]
Processing: Store accessibility information
Output: ATM marked as accessible with audio guidance only
```

---

### BR-007: Operating Hours Format Rule

**Category**: TRANSFORMATION

**Description**: ATM records should maintain operating hours in HH:MM 24-hour format or "closed" for customer convenience.

**Source**: 
- File: ATM Management User Story
- Section: Business Rules
- Rule: Operating Hours Format

**Business Logic**:
1. Each day of the week has opening_time and closing_time fields
2. Time values must be in HH:MM 24-hour format (e.g., "08:00", "20:00")
3. "closed" is a valid value indicating the ATM is not available that day
4. Operating hours help customers plan their ATM visits

**Variables**:
- **Input**: monday through sunday objects with opening_time and closing_time
- **Output**: Validated operating hours schedule
- **Constants**: Time format pattern: HH:MM or "closed"

**Business Conditions**:
| Condition | Business Meaning | Values/Thresholds |
|-----------|------------------|-------------------|
| Valid HH:MM format | Operating time specified | 00:00 to 23:59 |
| "closed" value | ATM not available | String literal |
| opening_time < closing_time | Valid operating window | Logical time sequence |

**Business Impact**: 
Enables customers to know when ATMs are available, reducing wasted trips and improving customer satisfaction.

**API Endpoints Using This Rule**:
- POST /obp/v5.1.0/banks/{BANK_ID}/atms - Create ATM
- PUT /obp/v5.1.0/banks/{BANK_ID}/atms/{ATM_ID} - Update ATM
- GET /obp/v5.1.0/banks/{BANK_ID}/atms/{ATM_ID} - Retrieve ATM
- GET /obp/v5.1.0/banks/{BANK_ID}/atms - List ATMs

**Migration Notes for Go**:
- Use string type for time fields to accommodate both HH:MM format and "closed" value
- Implement time format validation using regex or time parsing
- Consider creating a custom type for operating hours

**Example Scenarios**:
```
Scenario 1: Standard operating hours
Input: monday = { opening_time: "08:00", closing_time: "20:00" }
Processing: Validate HH:MM format
Output: Valid operating hours accepted

Scenario 2: Closed day
Input: sunday = { opening_time: "closed", closing_time: "closed" }
Processing: Validate "closed" as valid value
Output: ATM marked as closed on Sunday
```

---

### BR-008: Fee Transparency Rule

**Category**: TRANSFORMATION

**Description**: Fee information for withdrawals and inquiries should be transparent and clearly documented for regulatory compliance and customer information.

**Source**: 
- File: ATM Management User Story
- Section: Business Rules
- Rule: Fee Transparency

**Business Logic**:
1. ATM records include fee information for various operations
2. cash_withdrawal_national_fee: Fee for domestic withdrawals
3. cash_withdrawal_international_fee: Fee for international withdrawals
4. balance_inquiry_fee: Fee for balance inquiries
5. Fees are stored as string values representing monetary amounts

**Variables**:
- **Input**: cash_withdrawal_national_fee, cash_withdrawal_international_fee, balance_inquiry_fee
- **Output**: Documented fee structure
- **Constants**: None (fees vary by ATM)

**Business Conditions**:
| Condition | Business Meaning | Values/Thresholds |
|-----------|------------------|-------------------|
| Fee = "0.00" | No fee charged | Free service |
| Fee > "0.00" | Fee applies | Monetary amount |

**Business Impact**: 
Ensures regulatory compliance with fee disclosure requirements and enables customers to make informed decisions about which ATM to use based on costs.

**API Endpoints Using This Rule**:
- POST /obp/v5.1.0/banks/{BANK_ID}/atms - Create ATM
- PUT /obp/v5.1.0/banks/{BANK_ID}/atms/{ATM_ID} - Update ATM
- GET /obp/v5.1.0/banks/{BANK_ID}/atms/{ATM_ID} - Retrieve ATM
- GET /obp/v5.1.0/banks/{BANK_ID}/atms - List ATMs

**Migration Notes for Go**:
- Use string type for fee fields to preserve decimal precision
- Consider using decimal library for fee calculations if needed
- Ensure fee values are properly formatted for display

**Example Scenarios**:
```
Scenario 1: Free national withdrawal
Input: cash_withdrawal_national_fee = "0.00"
Processing: Store fee information
Output: ATM shows no fee for national withdrawals

Scenario 2: International withdrawal fee
Input: cash_withdrawal_international_fee = "2.50"
Processing: Store fee information
Output: ATM shows $2.50 fee for international withdrawals
```

---

### BR-009: Authorization Requirement Rule

**Category**: DECISION

**Description**: Users must have appropriate entitlements/permissions to perform create, update, or delete operations on ATM records.

**Source**: 
- File: ATM Management User Story
- Section: Business Rules
- Rule: Authorization Requirement

**Business Logic**:
1. Create operations require CanCreateAtm entitlement
2. Update operations require CanUpdateAtm entitlement
3. Delete operations require CanDeleteAtm entitlement
4. Read operations (GET) may have different or no entitlement requirements
5. Users without required entitlements receive HTTP 403 Forbidden

**Variables**:
- **Input**: User credentials, requested operation type
- **Output**: Authorization decision (allowed/denied)
- **Constants**: Entitlement names: CanCreateAtm, CanUpdateAtm, CanDeleteAtm

**Business Conditions**:
| Condition | Business Meaning | Values/Thresholds |
|-----------|------------------|-------------------|
| Has CanCreateAtm | Authorized to create ATMs | Entitlement present |
| Has CanUpdateAtm | Authorized to update ATMs | Entitlement present |
| Has CanDeleteAtm | Authorized to delete ATMs | Entitlement present |
| Missing entitlement | Not authorized | HTTP 403 Forbidden |

**Business Impact**: 
Ensures only authorized personnel can modify ATM network data, maintaining data integrity and security. Prevents unauthorized changes to critical banking infrastructure information.

**API Endpoints Using This Rule**:
- POST /obp/v5.1.0/banks/{BANK_ID}/atms - Requires CanCreateAtm
- PUT /obp/v5.1.0/banks/{BANK_ID}/atms/{ATM_ID} - Requires CanUpdateAtm
- DELETE /obp/v5.1.0/banks/{BANK_ID}/atms/{ATM_ID} - Requires CanDeleteAtm

**Migration Notes for Go**:
- Implement middleware or decorator pattern for entitlement checking
- Use authentication service to validate user credentials and entitlements
- Return HTTP 403 Forbidden for unauthorized access attempts

**Example Scenarios**:
```
Scenario 1: Authorized create
Input: User with CanCreateAtm entitlement, POST request
Processing: Check user entitlements for CanCreateAtm
Output: Authorization granted, proceed with ATM creation

Scenario 2: Unauthorized delete
Input: User without CanDeleteAtm entitlement, DELETE request
Processing: Check user entitlements for CanDeleteAtm
Output: HTTP 403 Forbidden - User not authorized to delete ATMs
```

---

## API Endpoint Coverage

| Endpoint | HTTP Method | Business Rules | Rule IDs |
|----------|-------------|----------------|----------|
| /obp/v5.1.0/banks/{BANK_ID}/atms | POST | Bank validation, ID uniqueness, location validation, language/currency validation, accessibility, hours, fees, authorization | BR-001, BR-002, BR-003, BR-004, BR-005, BR-006, BR-007, BR-008, BR-009 |
| /obp/v5.1.0/banks/{BANK_ID}/atms/{ATM_ID} | PUT | Bank validation, location validation, language/currency validation, accessibility, hours, fees, authorization | BR-001, BR-003, BR-004, BR-005, BR-006, BR-007, BR-008, BR-009 |
| /obp/v5.1.0/banks/{BANK_ID}/atms/{ATM_ID} | GET | Bank validation, data transformation | BR-001, BR-004, BR-005, BR-006, BR-007, BR-008 |
| /obp/v5.1.0/banks/{BANK_ID}/atms | GET | Bank validation, data transformation | BR-001, BR-004, BR-005, BR-006, BR-007, BR-008 |
| /obp/v5.1.0/banks/{BANK_ID}/atms/{ATM_ID} | DELETE | Bank validation, authorization | BR-001, BR-009 |

## Migration Validation Matrix

| Rule ID | Rule Name | Go Implementation Status | Validation Status |
|---------|-----------|--------------------------|-------------------|
| BR-001 | Bank Association Rule | Pending | Pending |
| BR-002 | ATM ID Uniqueness Rule | Pending | Pending |
| BR-003 | Location Requirement Rule | Pending | Pending |
| BR-004 | Language Code Standard Rule | Pending | Pending |
| BR-005 | Currency Code Standard Rule | Pending | Pending |
| BR-006 | Accessibility Documentation Rule | Pending | Pending |
| BR-007 | Operating Hours Format Rule | Pending | Pending |
| BR-008 | Fee Transparency Rule | Pending | Pending |
| BR-009 | Authorization Requirement Rule | Pending | Pending |

## Notes and Assumptions

1. **Source Context**: Business rules were extracted from the ATM Management User Story document, not from Scala source code. The rules represent the expected business behavior for the ATM Management capability.

2. **Entitlement Names**: The specific entitlement names (CanCreateAtm, CanUpdateAtm, CanDeleteAtm) are assumed based on common patterns. These should be verified against the actual Scala implementation.

3. **Accessibility Features**: The complete list of valid accessibility features should be confirmed with SME input. Current examples include WHEELCHAIR_ACCESS, AUDIO_GUIDANCE, and BRAILLE_KEYPAD.

4. **ATM Services**: The complete list of valid ATM services (CASH_WITHDRAWAL, BALANCE_INQUIRY, DEPOSIT, etc.) should be verified with SME input.

5. **Fee Currency**: The currency for fee amounts is not explicitly specified in the user story. This should be clarified (likely the bank's default currency or specified per fee).

6. **Soft Delete vs Hard Delete**: The user story mentions considering soft delete vs hard delete. The actual implementation approach should be confirmed.

7. **Audit Trail**: While mentioned in implementation notes, audit logging is not captured as a business rule as it is typically a technical/operational concern.
