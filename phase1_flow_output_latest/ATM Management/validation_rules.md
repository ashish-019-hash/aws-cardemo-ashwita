# Validation Rules

**Extracted From:** OBP-API Scala Application  
**User Story:** ATM Management  
**Analysis Date:** 2026-02-04  
**Analyst:** Scala Systems Analyst

## Summary

Total Validation Rules Extracted: 18

### Validation Categories
- Input Validation Rules: 6
- Format Validation Rules: 5
- Business Constraint Rules: 4
- Length/Boundary Rules: 2
- Cross-Field Validation Rules: 1

---

## Category: Input Validation (Required Fields)

### Rule VR-001: ATM ID Required Validation

**Field/Entity:** id (ATM ID)

**Validation Type:** Required Field Validation

**Rule Description:**
ATM ID is a mandatory field that must be provided when creating a new ATM record. The ATM ID serves as the unique identifier for the ATM within the bank's ATM network.

**Validation Logic:**

- **Condition:** When creating a new ATM record via POST /obp/v5.1.0/banks/{BANK_ID}/atms
- **Check:** Validate that the `id` field is present and not empty in the request body
- **Valid Criteria:** The `id` field is present and contains a non-empty string value
- **Invalid Criteria:** The `id` field is missing, null, or an empty string
- **Action on Success:** Proceed with ATM creation process
- **Action on Failure:** Return error response indicating missing required field

**Error Handling:**

- **Error Message:** `OBP-30001: Missing required field: ATM ID`
- **Error Code:** `OBP-30001`
- **HTTP Status Code:** `400 Bad Request`

**Related Entities:**
- ATM (primary entity)
- ATM creation request payload

**User Story Context:**
This validation ensures that every ATM record has a unique identifier as specified in the acceptance criteria: "The system shall allow authorized users to create new ATM records with location details, supported languages, currencies, and accessibility features."

**Dependencies:**
- VR-002: ATM ID Uniqueness Validation (must pass after this validation)

---

### Rule VR-002: Bank ID Required Validation

**Field/Entity:** bank_id (Bank ID)

**Validation Type:** Required Field Validation

**Rule Description:**
Bank ID is a mandatory field that must be provided when creating or updating an ATM record. The Bank ID associates the ATM with a specific bank entity.

**Validation Logic:**

- **Condition:** When creating or updating an ATM record
- **Check:** Validate that the `bank_id` field is present and not empty
- **Valid Criteria:** The `bank_id` field is present and contains a non-empty string value
- **Invalid Criteria:** The `bank_id` field is missing, null, or an empty string
- **Action on Success:** Proceed with bank existence validation
- **Action on Failure:** Return error response indicating missing required field

**Error Handling:**

- **Error Message:** `OBP-30002: Missing required field: Bank ID`
- **Error Code:** `OBP-30002`
- **HTTP Status Code:** `400 Bad Request`

**Related Entities:**
- ATM (primary entity)
- Bank (referenced entity)

**User Story Context:**
This validation supports the business rule: "ATMs must be associated with a valid bank entity (BANK_ID must exist)."

**Dependencies:**
- VR-007: Bank ID Existence Validation (must pass after this validation)

---

### Rule VR-003: ATM Name Required Validation

**Field/Entity:** name (ATM Name)

**Validation Type:** Required Field Validation

**Rule Description:**
ATM name is a mandatory field that must be provided when creating a new ATM record. The name provides a human-readable identifier for the ATM location.

**Validation Logic:**

- **Condition:** When creating a new ATM record
- **Check:** Validate that the `name` field is present and not empty
- **Valid Criteria:** The `name` field is present and contains a non-empty string value
- **Invalid Criteria:** The `name` field is missing, null, or an empty string
- **Action on Success:** Proceed with ATM creation process
- **Action on Failure:** Return error response indicating missing required field

**Error Handling:**

- **Error Message:** `OBP-30003: Missing required field: ATM Name`
- **Error Code:** `OBP-30003`
- **HTTP Status Code:** `400 Bad Request`

**Related Entities:**
- ATM (primary entity)

**User Story Context:**
This validation ensures ATMs have descriptive names for customer-facing applications and ATM locator features as mentioned in the dependencies section.

**Dependencies:**
- None (standalone validation)

---

### Rule VR-004: ATM Location Required Validation

**Field/Entity:** location (Geographic Coordinates)

**Validation Type:** Required Field Validation

**Rule Description:**
ATM location with geographic coordinates is a mandatory field that must be provided when creating a new ATM record. Location data enables mapping and ATM locator functionality.

**Validation Logic:**

- **Condition:** When creating a new ATM record
- **Check:** Validate that the `location` object is present with both `latitude` and `longitude` fields
- **Valid Criteria:** The `location` object is present with valid numeric values for both `latitude` and `longitude`
- **Invalid Criteria:** The `location` object is missing, or either `latitude` or `longitude` is missing or null
- **Action on Success:** Proceed with coordinate range validation
- **Action on Failure:** Return error response indicating missing required field

**Error Handling:**

- **Error Message:** `OBP-30004: Missing required field: ATM Location (latitude and longitude required)`
- **Error Code:** `OBP-30004`
- **HTTP Status Code:** `400 Bad Request`

**Related Entities:**
- ATM (primary entity)
- Location object (nested entity)

**User Story Context:**
This validation supports the acceptance criteria: "ATM location information shall include geographic coordinates, address, and bank identifier" and the business rule: "ATM location information must include geographic coordinates for mapping purposes."

**Dependencies:**
- VR-005: Latitude Range Validation (must pass after this validation)
- VR-006: Longitude Range Validation (must pass after this validation)

---

## Category: Format Validation

### Rule VR-005: Latitude Range Validation

**Field/Entity:** location.latitude

**Validation Type:** Numeric Range Validation

**Rule Description:**
Latitude value must be within the valid geographic range of -90 to 90 degrees to represent a valid point on Earth.

**Validation Logic:**

- **Condition:** When latitude is provided in the location object
- **Check:** Validate that latitude is a numeric value between -90 and 90 (inclusive)
- **Valid Criteria:** -90 <= latitude <= 90
- **Invalid Criteria:** latitude < -90 OR latitude > 90 OR latitude is not a valid number
- **Action on Success:** Proceed with longitude validation
- **Action on Failure:** Return error response indicating invalid latitude value

**Error Handling:**

- **Error Message:** `OBP-30005: Invalid latitude value. Latitude must be between -90 and 90 degrees.`
- **Error Code:** `OBP-30005`
- **HTTP Status Code:** `400 Bad Request`

**Related Entities:**
- ATM (primary entity)
- Location object (nested entity)

**User Story Context:**
This validation ensures accurate mapping as specified in the data validations: "Latitude must be between -90 and 90" and supports the implementation note about geolocation precision.

**Dependencies:**
- VR-004: ATM Location Required Validation (must pass before this validation)

---

### Rule VR-006: Longitude Range Validation

**Field/Entity:** location.longitude

**Validation Type:** Numeric Range Validation

**Rule Description:**
Longitude value must be within the valid geographic range of -180 to 180 degrees to represent a valid point on Earth.

**Validation Logic:**

- **Condition:** When longitude is provided in the location object
- **Check:** Validate that longitude is a numeric value between -180 and 180 (inclusive)
- **Valid Criteria:** -180 <= longitude <= 180
- **Invalid Criteria:** longitude < -180 OR longitude > 180 OR longitude is not a valid number
- **Action on Success:** Proceed with ATM creation/update
- **Action on Failure:** Return error response indicating invalid longitude value

**Error Handling:**

- **Error Message:** `OBP-30006: Invalid longitude value. Longitude must be between -180 and 180 degrees.`
- **Error Code:** `OBP-30006`
- **HTTP Status Code:** `400 Bad Request`

**Related Entities:**
- ATM (primary entity)
- Location object (nested entity)

**User Story Context:**
This validation ensures accurate mapping as specified in the data validations: "longitude must be between -180 and 180" and supports geographic mapping services that consume ATM location data.

**Dependencies:**
- VR-004: ATM Location Required Validation (must pass before this validation)

---

### Rule VR-007: Language Code Format Validation

**Field/Entity:** supported_languages[]

**Validation Type:** Format Validation (ISO Code)

**Rule Description:**
Each language code in the supported_languages array must be a valid ISO 639-1 two-letter language code to ensure standardized language identification.

**Validation Logic:**

- **Condition:** When supported_languages array is provided in the ATM record
- **Check:** Validate that each element in the array is a valid ISO 639-1 language code
- **Valid Criteria:** Each language code is a valid two-letter ISO 639-1 code (e.g., "en", "de", "fr", "es")
- **Invalid Criteria:** Language code is not a valid ISO 639-1 code, is empty, or has incorrect format
- **Action on Success:** Accept the language codes and store in ATM record
- **Action on Failure:** Return error response indicating invalid language code

**Error Handling:**

- **Error Message:** `OBP-30007: Invalid language code. Supported languages must be valid ISO 639-1 language codes.`
- **Error Code:** `OBP-30007`
- **HTTP Status Code:** `400 Bad Request`

**Related Entities:**
- ATM (primary entity)
- Supported languages array

**User Story Context:**
This validation supports the acceptance criteria: "Supported languages shall be configurable per ATM to reflect local language availability" and the business rule: "Supported languages must be valid ISO language codes."

**Dependencies:**
- None (standalone validation)

---

### Rule VR-008: Currency Code Format Validation

**Field/Entity:** supported_currencies[]

**Validation Type:** Format Validation (ISO Code)

**Rule Description:**
Each currency code in the supported_currencies array must be a valid ISO 4217 three-letter currency code to ensure standardized currency identification.

**Validation Logic:**

- **Condition:** When supported_currencies array is provided in the ATM record
- **Check:** Validate that each element in the array is a valid ISO 4217 currency code
- **Valid Criteria:** Each currency code is a valid three-letter ISO 4217 code (e.g., "EUR", "USD", "GBP")
- **Invalid Criteria:** Currency code is not a valid ISO 4217 code, is empty, or has incorrect format
- **Action on Success:** Accept the currency codes and store in ATM record
- **Action on Failure:** Return error response indicating invalid currency code

**Error Handling:**

- **Error Message:** `OBP-30008: Invalid currency code. Supported currencies must be valid ISO 4217 currency codes.`
- **Error Code:** `OBP-30008`
- **HTTP Status Code:** `400 Bad Request`

**Related Entities:**
- ATM (primary entity)
- Supported currencies array

**User Story Context:**
This validation supports the acceptance criteria: "Supported currencies shall be configurable per ATM to reflect available cash denominations" and the business rule: "Supported currencies must be valid ISO currency codes."

**Dependencies:**
- None (standalone validation)

---

### Rule VR-009: Operating Hours Time Format Validation

**Field/Entity:** monday.opening_time, monday.closing_time, tuesday.opening_time, etc.

**Validation Type:** Format Validation (Time)

**Rule Description:**
Operating hours for each day must be in valid HH:MM 24-hour time format or the special value "closed" to indicate the ATM is not operational on that day.

**Validation Logic:**

- **Condition:** When operating hours are provided for any day of the week
- **Check:** Validate that time values match the pattern HH:MM (24-hour format) or equal "closed"
- **Valid Criteria:** Time value matches regex pattern `^([01]?[0-9]|2[0-3]):[0-5][0-9]$` OR equals "closed"
- **Invalid Criteria:** Time value does not match the expected format
- **Action on Success:** Accept the operating hours and store in ATM record
- **Action on Failure:** Return error response indicating invalid time format

**Error Handling:**

- **Error Message:** `OBP-30009: Invalid time format. Operating hours must be in HH:MM format or 'closed'.`
- **Error Code:** `OBP-30009`
- **HTTP Status Code:** `400 Bad Request`

**Related Entities:**
- ATM (primary entity)
- Operating hours objects for each day

**User Story Context:**
This validation supports the data validation requirement: "Operating hours must be in valid time format (HH:MM)" and the business rule: "ATM records should maintain operating hours for customer convenience."

**Dependencies:**
- None (standalone validation)

---

## Category: Business Constraint Validation

### Rule VR-010: Bank ID Existence Validation

**Field/Entity:** bank_id

**Validation Type:** Entity Existence Validation

**Rule Description:**
The specified Bank ID must correspond to an existing bank entity in the system before an ATM can be created or associated with that bank.

**Validation Logic:**

- **Condition:** When creating or updating an ATM record with a bank_id
- **Check:** Query the bank repository to verify the bank_id exists
- **Valid Criteria:** A bank record with the specified bank_id exists in the system
- **Invalid Criteria:** No bank record found with the specified bank_id
- **Action on Success:** Proceed with ATM creation/update
- **Action on Failure:** Return error response indicating bank not found

**Error Handling:**

- **Error Message:** `OBP-30010: Bank not found. The specified BANK_ID does not exist.`
- **Error Code:** `OBP-30010`
- **HTTP Status Code:** `404 Not Found`

**Related Entities:**
- ATM (primary entity)
- Bank (referenced entity)

**User Story Context:**
This validation supports the business rule: "ATMs must be associated with a valid bank entity (BANK_ID must exist)" and the dependency: "Bank entity must exist before ATMs can be created for that bank."

**Dependencies:**
- VR-002: Bank ID Required Validation (must pass before this validation)

---

### Rule VR-011: ATM ID Uniqueness Validation

**Field/Entity:** id (ATM ID)

**Validation Type:** Uniqueness Constraint Validation

**Rule Description:**
ATM IDs must be unique within a bank to prevent duplicate records and ensure proper identification of ATM resources.

**Validation Logic:**

- **Condition:** When creating a new ATM record
- **Check:** Query the ATM repository to verify no existing ATM has the same id within the same bank
- **Valid Criteria:** No existing ATM record with the same id exists for the specified bank_id
- **Invalid Criteria:** An ATM record with the same id already exists for the specified bank_id
- **Action on Success:** Proceed with ATM creation
- **Action on Failure:** Return error response indicating duplicate ATM ID

**Error Handling:**

- **Error Message:** `OBP-30011: Duplicate ATM ID. An ATM with this ID already exists for this bank.`
- **Error Code:** `OBP-30011`
- **HTTP Status Code:** `409 Conflict`

**Related Entities:**
- ATM (primary entity)

**User Story Context:**
This validation supports the data validation requirement: "ATM IDs must be unique within a bank" to ensure proper ATM identification and retrieval.

**Dependencies:**
- VR-001: ATM ID Required Validation (must pass before this validation)
- VR-010: Bank ID Existence Validation (must pass before this validation)

---

### Rule VR-012: ATM Existence Validation for Update/Delete

**Field/Entity:** ATM_ID (path parameter)

**Validation Type:** Entity Existence Validation

**Rule Description:**
When updating or deleting an ATM record, the specified ATM must exist in the system for the given bank.

**Validation Logic:**

- **Condition:** When updating (PUT) or deleting (DELETE) an ATM record via /obp/v5.1.0/banks/{BANK_ID}/atms/{ATM_ID}
- **Check:** Query the ATM repository to verify the ATM exists for the specified bank
- **Valid Criteria:** An ATM record with the specified ATM_ID exists for the specified BANK_ID
- **Invalid Criteria:** No ATM record found with the specified ATM_ID for the specified BANK_ID
- **Action on Success:** Proceed with update or delete operation
- **Action on Failure:** Return error response indicating ATM not found

**Error Handling:**

- **Error Message:** `OBP-30012: ATM not found. The specified ATM_ID does not exist for this bank.`
- **Error Code:** `OBP-30012`
- **HTTP Status Code:** `404 Not Found`

**Related Entities:**
- ATM (primary entity)
- Bank (referenced entity)

**User Story Context:**
This validation supports the acceptance criteria for update and delete operations: "The system shall allow authorized users to update existing ATM information" and "The system shall allow authorized users to delete ATM records that are no longer active or relevant."

**Dependencies:**
- VR-010: Bank ID Existence Validation (must pass before this validation)

---

### Rule VR-013: Country Code Format Validation

**Field/Entity:** address.country_code

**Validation Type:** Format Validation (ISO Code)

**Rule Description:**
The country code in the ATM address must be a valid ISO 3166-1 alpha-2 two-letter country code to ensure standardized country identification.

**Validation Logic:**

- **Condition:** When address with country_code is provided in the ATM record
- **Check:** Validate that the country_code is a valid ISO 3166-1 alpha-2 code
- **Valid Criteria:** Country code is a valid two-letter ISO 3166-1 alpha-2 code (e.g., "DE", "US", "GB", "FR")
- **Invalid Criteria:** Country code is not a valid ISO 3166-1 code, is empty, or has incorrect format
- **Action on Success:** Accept the country code and store in ATM address record
- **Action on Failure:** Return error response indicating invalid country code

**Error Handling:**

- **Error Message:** `OBP-30013: Invalid country code. Address country code must be a valid ISO 3166-1 alpha-2 code.`
- **Error Code:** `OBP-30013`
- **HTTP Status Code:** `400 Bad Request`

**Related Entities:**
- ATM (primary entity)
- Address object (nested entity)

**User Story Context:**
This validation ensures that ATM addresses use standardized country codes for proper geographic identification and integration with mapping services as mentioned in the user story's address structure.

**Dependencies:**
- None (standalone validation)

---

### Rule VR-014: Accessibility Features Validation

**Field/Entity:** accessibility_features[]

**Validation Type:** Enumeration Validation

**Rule Description:**
Each accessibility feature in the accessibility_features array must be a valid predefined accessibility feature code to ensure consistent documentation of ATM accessibility capabilities.

**Validation Logic:**

- **Condition:** When accessibility_features array is provided in the ATM record
- **Check:** Validate that each element in the array is a valid accessibility feature code
- **Valid Criteria:** Each accessibility feature is one of the predefined values: "WHEELCHAIR_ACCESS", "AUDIO_GUIDANCE", "BRAILLE_KEYPAD", "LARGE_PRINT", "HEARING_LOOP", "LOW_COUNTER"
- **Invalid Criteria:** Accessibility feature is not a recognized code or is empty
- **Action on Success:** Accept the accessibility features and store in ATM record
- **Action on Failure:** Return error response indicating invalid accessibility feature

**Error Handling:**

- **Error Message:** `OBP-30014: Invalid accessibility feature. Accessibility features must be valid predefined codes.`
- **Error Code:** `OBP-30014`
- **HTTP Status Code:** `400 Bad Request`

**Related Entities:**
- ATM (primary entity)
- Accessibility features array

**User Story Context:**
This validation supports the acceptance criteria: "Accessibility features shall be documented per ATM (e.g., wheelchair access, audio guidance, braille keypad)" and the business rule: "Accessibility features must be documented to comply with accessibility regulations and customer information requirements."

**Dependencies:**
- None (standalone validation)

---

### Rule VR-015: Services Validation

**Field/Entity:** services[]

**Validation Type:** Enumeration Validation

**Rule Description:**
Each service in the services array must be a valid predefined ATM service code to ensure consistent documentation of ATM capabilities.

**Validation Logic:**

- **Condition:** When services array is provided in the ATM record
- **Check:** Validate that each element in the array is a valid ATM service code
- **Valid Criteria:** Each service is one of the predefined values: "CASH_WITHDRAWAL", "BALANCE_INQUIRY", "DEPOSIT", "TRANSFER", "BILL_PAYMENT", "MINI_STATEMENT", "PIN_CHANGE"
- **Invalid Criteria:** Service is not a recognized code or is empty
- **Action on Success:** Accept the services and store in ATM record
- **Action on Failure:** Return error response indicating invalid service

**Error Handling:**

- **Error Message:** `OBP-30015: Invalid service. Services must be valid predefined ATM service codes.`
- **Error Code:** `OBP-30015`
- **HTTP Status Code:** `400 Bad Request`

**Related Entities:**
- ATM (primary entity)
- Services array

**User Story Context:**
This validation ensures that ATM services are documented using standardized codes as shown in the user story request example with services like "CASH_WITHDRAWAL", "BALANCE_INQUIRY", and "DEPOSIT".

**Dependencies:**
- None (standalone validation)

---

### Rule VR-016: Fee Amount Format Validation

**Field/Entity:** cash_withdrawal_national_fee, cash_withdrawal_international_fee, balance_inquiry_fee

**Validation Type:** Format Validation (Numeric)

**Rule Description:**
Fee amounts must be valid non-negative decimal numbers representing currency amounts to ensure proper fee documentation and transparency.

**Validation Logic:**

- **Condition:** When fee fields are provided in the ATM record
- **Check:** Validate that fee values are valid non-negative decimal numbers
- **Valid Criteria:** Fee value is a non-negative decimal number with up to 2 decimal places (e.g., "0.00", "2.50", "5.00")
- **Invalid Criteria:** Fee value is negative, not a valid number, or has more than 2 decimal places
- **Action on Success:** Accept the fee values and store in ATM record
- **Action on Failure:** Return error response indicating invalid fee format

**Error Handling:**

- **Error Message:** `OBP-30016: Invalid fee format. Fee amounts must be non-negative decimal numbers with up to 2 decimal places.`
- **Error Code:** `OBP-30016`
- **HTTP Status Code:** `400 Bad Request`

**Related Entities:**
- ATM (primary entity)
- Fee fields

**User Story Context:**
This validation supports the business rule: "Fee information for withdrawals and inquiries should be transparent" and ensures proper documentation of ATM fees as shown in the user story request example.

**Dependencies:**
- None (standalone validation)

---

### Rule VR-017: Minimum Withdrawal Amount Validation

**Field/Entity:** minimum_withdrawal

**Validation Type:** Format Validation (Numeric)

**Rule Description:**
Minimum withdrawal amount must be a valid positive decimal number representing the minimum cash amount that can be withdrawn from the ATM.

**Validation Logic:**

- **Condition:** When minimum_withdrawal field is provided in the ATM record
- **Check:** Validate that minimum_withdrawal is a valid positive decimal number
- **Valid Criteria:** Minimum withdrawal value is a positive decimal number (e.g., "10", "20.00", "50")
- **Invalid Criteria:** Minimum withdrawal value is zero, negative, or not a valid number
- **Action on Success:** Accept the minimum withdrawal value and store in ATM record
- **Action on Failure:** Return error response indicating invalid minimum withdrawal format

**Error Handling:**

- **Error Message:** `OBP-30017: Invalid minimum withdrawal amount. Value must be a positive decimal number.`
- **Error Code:** `OBP-30017`
- **HTTP Status Code:** `400 Bad Request`

**Related Entities:**
- ATM (primary entity)

**User Story Context:**
This validation ensures proper documentation of ATM withdrawal limits as shown in the user story request example with minimum_withdrawal field.

**Dependencies:**
- None (standalone validation)

---

## Validation Rule Summary Table

| Rule ID | Field/Entity | Validation Type | Error Code | HTTP Status |
|---------|--------------|-----------------|------------|-------------|
| VR-001 | id (ATM ID) | Required Field | OBP-30001 | 400 |
| VR-002 | bank_id | Required Field | OBP-30002 | 400 |
| VR-003 | name | Required Field | OBP-30003 | 400 |
| VR-004 | location | Required Field | OBP-30004 | 400 |
| VR-005 | location.latitude | Numeric Range | OBP-30005 | 400 |
| VR-006 | location.longitude | Numeric Range | OBP-30006 | 400 |
| VR-007 | supported_languages[] | Format (ISO 639-1) | OBP-30007 | 400 |
| VR-008 | supported_currencies[] | Format (ISO 4217) | OBP-30008 | 400 |
| VR-009 | operating hours | Format (Time) | OBP-30009 | 400 |
| VR-010 | bank_id | Entity Existence | OBP-30010 | 404 |
| VR-011 | id (ATM ID) | Uniqueness | OBP-30011 | 409 |
| VR-012 | ATM_ID (path) | Entity Existence | OBP-30012 | 404 |
| VR-013 | address.country_code | Format (ISO 3166-1) | OBP-30013 | 400 |
| VR-014 | accessibility_features[] | Enumeration | OBP-30014 | 400 |
| VR-015 | services[] | Enumeration | OBP-30015 | 400 |
| VR-016 | fee amounts | Format (Numeric) | OBP-30016 | 400 |
| VR-017 | minimum_withdrawal | Format (Numeric) | OBP-30017 | 400 |

---

## Notes

- All validation rules are derived from the ATM Management user story acceptance criteria, business rules, and data validation requirements.
- Error codes follow the OBP-XXXXX format convention for consistency with the existing system.
- HTTP status codes follow REST API best practices: 400 for client input errors, 404 for not found, 409 for conflicts.
- Authorization validations (user entitlements/permissions) are handled separately by the authentication/authorization layer and are not included in this document.
- The list of valid accessibility features and services may be extended based on SME input as noted in the user story implementation notes.
