# Business Rules Extraction

**Extracted From**: Bank Creation Capability (User Story)
**Analysis Date**: 2026-01-20
**Analyst**: Expert Scala Business Logic Analyst
**Migration Target**: Go Application

## Executive Summary
- Total Business Rules Extracted: 6
- API Endpoints Analyzed: 1
- Rule Categories:
  - Calculations: 0
  - Decisions: 2
  - Thresholds: 0
  - Aggregations: 0
  - Workflows: 2
  - Transformations: 2

## Business Rules Catalog

### BR-001: Bank Entity Uniqueness Validation

**Category**: DECISION

**Description**: Each bank entity created on the platform must have a unique identifier. The system must verify that no existing bank shares the same ID before allowing creation.

**Source**: 
- File: Bank Creation Service
- Class/Object: BankCreationService
- Method: createBank
- Lines: N/A (derived from user story)

**Business Logic**:
1. When a bank creation request is received, extract the bank ID from the request
2. Query the existing bank repository to check if a bank with the same ID exists
3. If a bank with the same ID exists, reject the creation request with an appropriate error
4. If the ID is unique, proceed with bank creation

**Variables**:
- **Input**: bank.id (String) - The unique identifier for the bank being created
- **Output**: Boolean - Whether the bank ID is unique and creation can proceed
- **Constants**: None

**Business Conditions**:
| Condition | Business Meaning | Values/Thresholds |
|-----------|------------------|-------------------|
| bank.id exists in system | Duplicate bank ID | Reject creation |
| bank.id not in system | Unique bank ID | Allow creation |

**Business Impact**: 
Ensures data integrity by preventing duplicate bank entities on the platform. This is critical for maintaining accurate bank records and preventing confusion in downstream operations like account creation and transaction processing.

**API Endpoints Using This Rule**:
- POST /obp/v5.1.0/banks - Bank creation endpoint

**Related Test Cases**:
- Test case for duplicate bank ID rejection
- Test case for successful creation with unique ID

**Migration Notes for Go**:
- Implement as a pre-creation validation check
- Use database unique constraint as a secondary safeguard
- Return appropriate HTTP 409 Conflict status for duplicate IDs

**Example Scenarios**:
```
Scenario 1: Unique bank ID
Input: bank.id = "NEW_BANK_001"
Processing: Check if "NEW_BANK_001" exists in database
Output: ID is unique, proceed with creation

Scenario 2: Duplicate bank ID
Input: bank.id = "EXISTING_BANK_001"
Processing: Check if "EXISTING_BANK_001" exists in database
Output: ID already exists, reject with error "Bank ID already exists"
```

---

### BR-002: Bank Metadata Requirements Validation

**Category**: DECISION

**Description**: Bank creation must include required metadata fields. The system validates that mandatory fields (full_name) are provided and non-empty before allowing bank creation.

**Source**: 
- File: Bank Validation Service
- Class/Object: BankValidationService
- Method: validateBankMetadata
- Lines: N/A (derived from user story)

**Business Logic**:
1. Receive bank creation request with metadata
2. Validate that full_name field is present and not empty
3. Validate that short_name follows naming conventions (alphanumeric, limited length)
4. If logo URL is provided, validate it is a valid URL format
5. If website URL is provided, validate it is a valid URL format
6. Validate bank routing schemes are valid (e.g., BIC, IBAN)
7. Validate metadata attributes have valid name-value pairs
8. If any validation fails, reject with specific error message
9. If all validations pass, proceed with creation

**Variables**:
- **Input**: 
  - bank.full_name (String, required) - Full name of the bank
  - bank.short_name (String) - Short name/code of the bank
  - bank.logo (String, URL) - Logo URL
  - bank.website (String, URL) - Website URL
  - bank.bank_routings (Array) - Bank routing information
  - bank.attributes (Array) - Additional metadata attributes
- **Output**: ValidationResult - Success or list of validation errors
- **Constants**: None

**Business Conditions**:
| Condition | Business Meaning | Values/Thresholds |
|-----------|------------------|-------------------|
| full_name is empty | Missing required field | Reject creation |
| logo URL invalid format | Invalid metadata | Reject creation |
| website URL invalid format | Invalid metadata | Reject creation |
| routing scheme invalid | Invalid bank routing | Reject creation |

**Business Impact**: 
Ensures all bank entities have complete and valid metadata for proper identification and integration with external systems. Incomplete metadata could cause issues in downstream operations and third-party integrations.

**API Endpoints Using This Rule**:
- POST /obp/v5.1.0/banks - Bank creation endpoint

**Related Test Cases**:
- Test case for missing full_name rejection
- Test case for invalid URL format rejection
- Test case for invalid routing scheme rejection
- Test case for successful creation with valid metadata

**Migration Notes for Go**:
- Implement validation as a separate function before persistence
- Use Go's url.Parse for URL validation
- Return HTTP 400 Bad Request with detailed error messages for validation failures

**Example Scenarios**:
```
Scenario 1: Valid metadata
Input: full_name = "First National Bank", short_name = "FNB", logo = "https://example.com/logo.png"
Processing: Validate all fields
Output: Validation passed, proceed with creation

Scenario 2: Missing required field
Input: full_name = "", short_name = "FNB"
Processing: Validate full_name is not empty
Output: Validation failed, error "Bank name (full_name) is required and must not be empty"

Scenario 3: Invalid URL format
Input: full_name = "Test Bank", logo = "not-a-valid-url"
Processing: Validate logo URL format
Output: Validation failed, error "Logo URL must be a valid URL format"
```

---

### BR-003: Authorization-Based Bank Creation Access

**Category**: DECISION

**Description**: Only authorized platform administrators with appropriate entitlements (e.g., CanCreateBank role) can create new bank entities on the platform.

**Source**: 
- File: Authorization Service
- Class/Object: AuthorizationService
- Method: checkBankCreationEntitlement
- Lines: N/A (derived from user story)

**Business Logic**:
1. Receive bank creation request with user authentication context
2. Extract user identity and roles from the authentication token
3. Check if user has CanCreateBank or equivalent entitlement
4. If user lacks required entitlement, reject with authorization error
5. If user has required entitlement, proceed with bank creation

**Variables**:
- **Input**: 
  - user.id (String) - Authenticated user identifier
  - user.roles (Array) - List of user roles/entitlements
- **Output**: Boolean - Whether user is authorized to create banks
- **Constants**: 
  - Required role: "CanCreateBank" or equivalent admin role

**Business Conditions**:
| Condition | Business Meaning | Values/Thresholds |
|-----------|------------------|-------------------|
| user has CanCreateBank | Authorized administrator | Allow creation |
| user lacks CanCreateBank | Unauthorized user | Reject with 403 |

**Business Impact**: 
Ensures platform security by restricting bank creation to authorized personnel only. Prevents unauthorized creation of bank entities which could compromise platform integrity.

**API Endpoints Using This Rule**:
- POST /obp/v5.1.0/banks - Bank creation endpoint

**Related Test Cases**:
- Test case for authorized user successful creation
- Test case for unauthorized user rejection

**Migration Notes for Go**:
- Implement as middleware or pre-handler authorization check
- Return HTTP 403 Forbidden for unauthorized access
- Log authorization failures for audit purposes

**Example Scenarios**:
```
Scenario 1: Authorized user
Input: user.roles = ["CanCreateBank", "PlatformAdmin"]
Processing: Check if "CanCreateBank" in user.roles
Output: Authorized, proceed with creation

Scenario 2: Unauthorized user
Input: user.roles = ["BasicUser"]
Processing: Check if "CanCreateBank" in user.roles
Output: Unauthorized, reject with "User does not have permission to create banks"
```

---

### BR-004: Bank Creation Workflow with ID Generation

**Category**: WORKFLOW

**Description**: The bank creation workflow generates a unique identifier for each newly created bank, associates all provided metadata, applies default configuration settings where not explicitly provided, and returns the complete bank entity with creation timestamp.

**Source**: 
- File: Bank Creation Service
- Class/Object: BankCreationService
- Method: createBank
- Lines: N/A (derived from user story)

**Business Logic**:
1. Validate user authorization (BR-003)
2. Validate bank uniqueness (BR-001)
3. Validate bank metadata (BR-002)
4. If bank ID not provided, generate a unique identifier
5. Apply default configuration settings for any parameters not explicitly provided
6. Associate all metadata (name, logo, website, identifiers) with the bank entity
7. Persist the bank entity to the database
8. Generate creation timestamp
9. Return confirmation with complete bank details including generated ID and timestamp

**Variables**:
- **Input**: 
  - Bank creation request with metadata and configuration
- **Output**: 
  - Created bank entity with:
    - id (String) - Generated or provided unique identifier
    - short_name (String)
    - full_name (String)
    - logo (String)
    - website (String)
    - bank_routings (Array)
    - attributes (Array)
    - created_at (Timestamp)
- **Constants**: Default configuration values

**Business Conditions**:
| Condition | Business Meaning | Values/Thresholds |
|-----------|------------------|-------------------|
| All validations pass | Ready for creation | Proceed to persist |
| Any validation fails | Invalid request | Return error response |

**Business Impact**: 
Provides a complete, auditable bank creation process that ensures all bank entities are properly configured and traceable from creation.

**API Endpoints Using This Rule**:
- POST /obp/v5.1.0/banks - Bank creation endpoint

**Related Test Cases**:
- Test case for complete bank creation workflow
- Test case for ID generation when not provided
- Test case for default configuration application

**Migration Notes for Go**:
- Implement as a transactional operation
- Use UUID generation for bank IDs if not provided
- Store creation timestamp in UTC format
- Return complete bank entity in response

**Example Scenarios**:
```
Scenario 1: Complete bank creation
Input: {
  "short_name": "FNB",
  "full_name": "First National Bank",
  "logo": "https://example.com/logo.png",
  "website": "https://fnb.com",
  "bank_routings": [{"scheme": "BIC", "address": "FNBAUS33"}]
}
Processing: Validate, generate ID, apply defaults, persist
Output: {
  "id": "generated-uuid-123",
  "short_name": "FNB",
  "full_name": "First National Bank",
  "logo": "https://example.com/logo.png",
  "website": "https://fnb.com",
  "bank_routings": [{"scheme": "BIC", "address": "FNBAUS33"}],
  "attributes": [],
  "created_at": "2026-01-20T15:00:00Z"
}
```

---

### BR-005: Default Configuration Application

**Category**: TRANSFORMATION

**Description**: When bank configuration parameters are not explicitly provided during creation, the system applies sensible default configuration settings to ensure the bank entity is fully operational.

**Source**: 
- File: Configuration Service
- Class/Object: BankConfigurationService
- Method: applyDefaultConfiguration
- Lines: N/A (derived from user story)

**Business Logic**:
1. Receive bank creation request with optional configuration parameters
2. Identify which configuration parameters are not provided
3. For each missing parameter, apply the predefined default value
4. Merge provided configuration with defaults (provided values take precedence)
5. Return complete configuration for the bank entity

**Variables**:
- **Input**: 
  - Partial configuration from bank creation request
- **Output**: 
  - Complete configuration with defaults applied
- **Constants**: 
  - Default configuration values (to be defined by SME)

**Business Conditions**:
| Condition | Business Meaning | Values/Thresholds |
|-----------|------------------|-------------------|
| Config param provided | Use provided value | N/A |
| Config param missing | Apply default value | Predefined defaults |

**Business Impact**: 
Ensures all bank entities have complete configuration even when minimal information is provided during creation. This reduces the burden on administrators and ensures consistent bank setup.

**API Endpoints Using This Rule**:
- POST /obp/v5.1.0/banks - Bank creation endpoint

**Related Test Cases**:
- Test case for default configuration application
- Test case for provided configuration override

**Migration Notes for Go**:
- Define default configuration as constants or configuration file
- Use struct merging to combine provided and default values
- Document all default values for transparency

**Example Scenarios**:
```
Scenario 1: Partial configuration provided
Input: Configuration with only "supported_features" specified
Processing: Apply defaults for all other configuration parameters
Output: Complete configuration with provided "supported_features" and default values for others

Scenario 2: No configuration provided
Input: Empty configuration
Processing: Apply all default configuration values
Output: Complete default configuration
```

---

### BR-006: Bank Creation Response Transformation

**Category**: TRANSFORMATION

**Description**: Upon successful bank creation, the system transforms the internal bank entity into a standardized response format containing all bank details, metadata, and creation timestamp.

**Source**: 
- File: Bank Response Mapper
- Class/Object: BankResponseMapper
- Method: toCreateBankResponse
- Lines: N/A (derived from user story)

**Business Logic**:
1. Receive persisted bank entity from database
2. Map internal bank entity fields to response format
3. Include all metadata (name, logo, website, identifiers)
4. Include all bank routing information
5. Include all attributes
6. Include creation timestamp
7. Return formatted response

**Variables**:
- **Input**: 
  - Internal bank entity from database
- **Output**: 
  - Bank creation response:
    ```json
    {
      "id": "string",
      "short_name": "string",
      "full_name": "string",
      "logo": "string",
      "website": "string",
      "bank_routings": [...],
      "attributes": [...],
      "created_at": "timestamp"
    }
    ```
- **Constants**: None

**Business Conditions**:
| Condition | Business Meaning | Values/Thresholds |
|-----------|------------------|-------------------|
| Bank created successfully | Return full details | HTTP 201 Created |
| Creation failed | Return error details | HTTP 4xx/5xx |

**Business Impact**: 
Provides consistent, predictable response format for API consumers, enabling reliable integration with third-party applications.

**API Endpoints Using This Rule**:
- POST /obp/v5.1.0/banks - Bank creation endpoint

**Related Test Cases**:
- Test case for response format validation
- Test case for all fields present in response

**Migration Notes for Go**:
- Define response struct matching the expected format
- Use JSON tags for proper field naming
- Ensure timestamp is in ISO 8601 format

**Example Scenarios**:
```
Scenario 1: Successful creation response
Input: Internal bank entity with all fields populated
Processing: Map to response format
Output: {
  "id": "bank-123",
  "short_name": "FNB",
  "full_name": "First National Bank",
  "logo": "https://example.com/logo.png",
  "website": "https://fnb.com",
  "bank_routings": [{"scheme": "BIC", "address": "FNBAUS33"}],
  "attributes": [{"name": "region", "value": "US"}],
  "created_at": "2026-01-20T15:00:00Z"
}
```

---

## API Endpoint Coverage

| Endpoint | HTTP Method | Business Rules | Rule IDs |
|----------|-------------|----------------|----------|
| /obp/v5.1.0/banks | POST | Bank uniqueness, metadata validation, authorization, creation workflow, default config, response transformation | BR-001, BR-002, BR-003, BR-004, BR-005, BR-006 |

## Migration Validation Matrix

| Rule ID | Test Case Reference | Go Implementation Status | Validation Status |
|---------|---------------------|--------------------------|-------------------|
| BR-001 | TestBankUniqueness | Pending | Pending |
| BR-002 | TestBankMetadataValidation | Pending | Pending |
| BR-003 | TestBankCreationAuthorization | Pending | Pending |
| BR-004 | TestBankCreationWorkflow | Pending | Pending |
| BR-005 | TestDefaultConfigApplication | Pending | Pending |
| BR-006 | TestBankCreationResponse | Pending | Pending |

## Notes and Assumptions

1. **Assumptions Made**:
   - Default configuration values are to be defined by SME (flagged in user story)
   - Bank ID can be either provided by the user or auto-generated by the system
   - Authorization is handled via role-based access control with "CanCreateBank" entitlement
   - All timestamps are stored and returned in UTC format

2. **Gaps Identified**:
   - Specific default configuration values not defined
   - Maximum length for short_name not specified
   - Maximum number of attributes per bank not specified
   - Specific validation rules for bank routing information not detailed

3. **Clarifications Needed** (from Open Questions in User Story):
   - What are the mandatory vs optional metadata fields for bank creation?
   - What default configuration settings should be applied to newly created banks?
   - Are there any naming conventions or restrictions for bank IDs?
   - Should bank creation trigger any downstream notifications or events?
   - What validation rules apply to bank routing information?
   - Is there a maximum number of attributes that can be associated with a bank during creation?
