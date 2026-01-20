# Validation Rules

**Extracted From:** OBP-API Scala Application  
**User Story:** Account Balance Retrieval  
**Analysis Date:** January 20, 2026  
**Analyst:** Scala Systems Analyst

## Summary

Total Validation Rules Extracted: 10

### Validation Categories
- Input Validation Rules: 4
- Format Validation Rules: 2
- Business Constraint Rules: 3
- Length/Boundary Rules: 0
- Cross-Field Validation Rules: 1

---

## Category: Input Validation

### Rule VR-001: Bank ID Required Validation

**Field/Entity:** BANK_ID

**Validation Type:** Required Field Validation

**Rule Description:**
Bank ID must be provided as a required path parameter for all account balance retrieval operations. The system cannot retrieve balance information without identifying which bank the account belongs to.

**Validation Logic:**

- **Condition:** When a request is made to retrieve account balances
- **Check:** Validate that BANK_ID path parameter is present and not empty
- **Valid Criteria:** BANK_ID is provided as a non-empty string in the request path
- **Invalid Criteria:** BANK_ID is missing, null, or empty string
- **Action on Success:** Proceed with bank existence validation
- **Action on Failure:** Return error response indicating missing bank identifier

**Error Handling:**

- **Error Message:** `Bank ID is required`
- **Error Code:** `OBP-30001`
- **HTTP Status Code:** `400 Bad Request`

**Related Entities:**
- Bank entity
- Account entity (depends on bank)

**User Story Context:**
The user story specifies BANK_ID as a required path parameter for both balance retrieval endpoints. This validation ensures the system can identify which bank's account to query.

**Dependencies:**
- None (first validation in chain)

---

### Rule VR-002: Account ID Required Validation

**Field/Entity:** ACCOUNT_ID

**Validation Type:** Required Field Validation

**Rule Description:**
Account ID must be provided as a required path parameter for all account balance retrieval operations. The system cannot retrieve balance information without identifying the specific account.

**Validation Logic:**

- **Condition:** When a request is made to retrieve account balances
- **Check:** Validate that ACCOUNT_ID path parameter is present and not empty
- **Valid Criteria:** ACCOUNT_ID is provided as a non-empty string in the request path
- **Invalid Criteria:** ACCOUNT_ID is missing, null, or empty string
- **Action on Success:** Proceed with account existence validation
- **Action on Failure:** Return error response indicating missing account identifier

**Error Handling:**

- **Error Message:** `Account ID is required`
- **Error Code:** `OBP-30002`
- **HTTP Status Code:** `400 Bad Request`

**Related Entities:**
- Account entity
- Bank entity (account belongs to bank)

**User Story Context:**
The user story specifies ACCOUNT_ID as a required path parameter for both balance retrieval endpoints. This validation ensures the system can identify which specific account's balance to retrieve.

**Dependencies:**
- VR-001: Bank ID must be validated first

---

### Rule VR-003: View ID Required Validation (View-Specific Endpoint)

**Field/Entity:** VIEW_ID

**Validation Type:** Required Field Validation (Conditional)

**Rule Description:**
View ID must be provided as a required path parameter when using the view-specific balance retrieval endpoint. The view determines the permission level and what balance information is accessible.

**Validation Logic:**

- **Condition:** When a request is made to the view-specific endpoint `/banks/{BANK_ID}/accounts/{ACCOUNT_ID}/{VIEW_ID}/balances`
- **Check:** Validate that VIEW_ID path parameter is present and not empty
- **Valid Criteria:** VIEW_ID is provided as a non-empty string in the request path
- **Invalid Criteria:** VIEW_ID is missing, null, or empty string
- **Action on Success:** Proceed with view existence and access validation
- **Action on Failure:** Return error response indicating missing view identifier

**Error Handling:**

- **Error Message:** `View ID is required for this endpoint`
- **Error Code:** `OBP-30003`
- **HTTP Status Code:** `400 Bad Request`

**Related Entities:**
- View entity
- Account entity (view is associated with account)
- User permissions

**User Story Context:**
The user story defines VIEW_ID as required for the view-specific endpoint. Views define access permissions and determine what balance information a user can see.

**Dependencies:**
- VR-001: Bank ID must be validated first
- VR-002: Account ID must be validated first

---

### Rule VR-004: Authentication Token Required Validation

**Field/Entity:** Authorization Header

**Validation Type:** Required Field Validation

**Rule Description:**
A valid authentication token must be provided in the Authorization header for all account balance retrieval operations. This ensures only authenticated users can access balance information.

**Validation Logic:**

- **Condition:** When any request is made to retrieve account balances
- **Check:** Validate that Authorization header is present and contains a valid Bearer token or OAuth credentials
- **Valid Criteria:** Authorization header is present with valid token format
- **Invalid Criteria:** Authorization header is missing, empty, or contains invalid format
- **Action on Success:** Proceed with token validation and user authentication
- **Action on Failure:** Return error response indicating authentication required

**Error Handling:**

- **Error Message:** `Authentication is required. Please provide valid credentials.`
- **Error Code:** `OBP-20001`
- **HTTP Status Code:** `401 Unauthorized`

**Related Entities:**
- User entity
- Authentication token
- OAuth credentials

**User Story Context:**
The user story specifies that authentication credentials/token are required input data. The Authorization header with Bearer token or OAuth credentials must be provided.

**Dependencies:**
- None (can be validated independently)

---

## Category: Format Validation

### Rule VR-005: Bank ID Format Validation

**Field/Entity:** BANK_ID

**Validation Type:** Format Validation (String Pattern)

**Rule Description:**
Bank ID must be a valid identifier format that matches the platform's bank identifier standards. It should contain only alphanumeric characters and allowed special characters.

**Validation Logic:**

- **Condition:** When BANK_ID is provided in the request path
- **Check:** Validate that BANK_ID matches the expected format pattern
- **Valid Criteria:** 
  - Contains only alphanumeric characters, hyphens, underscores, and periods
  - Matches pattern: `^[A-Za-z0-9\-_.]+$`
- **Invalid Criteria:**
  - Contains special characters other than allowed ones
  - Contains spaces or unicode characters
- **Action on Success:** Proceed with bank existence validation
- **Action on Failure:** Return error response indicating invalid bank ID format

**Error Handling:**

- **Error Message:** `Invalid Bank ID format. Bank ID must contain only alphanumeric characters, hyphens, underscores, and periods.`
- **Error Code:** `OBP-30111`
- **HTTP Status Code:** `400 Bad Request`

**Related Entities:**
- Bank entity

**User Story Context:**
The user story indicates that Bank ID must be a valid identifier for a bank on the platform. This validation ensures the format is correct before checking existence.

**Dependencies:**
- VR-001: Bank ID must be present first

---

### Rule VR-006: Account ID Format Validation

**Field/Entity:** ACCOUNT_ID

**Validation Type:** Format Validation (String Pattern)

**Rule Description:**
Account ID must be a valid identifier format that matches the platform's account identifier standards. It should contain only alphanumeric characters and allowed special characters.

**Validation Logic:**

- **Condition:** When ACCOUNT_ID is provided in the request path
- **Check:** Validate that ACCOUNT_ID matches the expected format pattern
- **Valid Criteria:** 
  - Contains only alphanumeric characters, hyphens, underscores, and periods
  - Matches pattern: `^[A-Za-z0-9\-_.]+$`
- **Invalid Criteria:**
  - Contains special characters other than allowed ones
  - Contains spaces or unicode characters
- **Action on Success:** Proceed with account existence validation
- **Action on Failure:** Return error response indicating invalid account ID format

**Error Handling:**

- **Error Message:** `Invalid Account ID format. Account ID must contain only alphanumeric characters, hyphens, underscores, and periods.`
- **Error Code:** `OBP-30112`
- **HTTP Status Code:** `400 Bad Request`

**Related Entities:**
- Account entity

**User Story Context:**
The user story indicates that Account ID must be a valid identifier for an account at the specified bank. This validation ensures the format is correct before checking existence.

**Dependencies:**
- VR-002: Account ID must be present first

---

## Category: Business Constraint Validation

### Rule VR-007: Bank Existence Validation

**Field/Entity:** BANK_ID

**Validation Type:** Entity Existence Validation

**Rule Description:**
The specified bank must exist and be active on the platform. Balance retrieval can only be performed for accounts at banks that are registered and active in the system.

**Validation Logic:**

- **Condition:** After BANK_ID format validation passes
- **Check:** Query the system to verify the bank exists and is active
- **Valid Criteria:** 
  - Bank with the specified BANK_ID exists in the system
  - Bank status is active
- **Invalid Criteria:**
  - Bank does not exist in the system
  - Bank exists but is inactive or unavailable
- **Action on Success:** Proceed with account existence validation
- **Action on Failure:** Return error response indicating bank not found or unavailable

**Error Handling:**

- **Error Message:** `Bank not found. Please verify the Bank ID.`
- **Error Code:** `OBP-30004`
- **HTTP Status Code:** `404 Not Found`

**Related Entities:**
- Bank entity

**User Story Context:**
The user story states "Bank must be active on the platform" as an upstream dependency. This validation ensures the bank exists and is operational before attempting to retrieve account balances.

**Dependencies:**
- VR-001: Bank ID must be present
- VR-005: Bank ID format must be valid

---

### Rule VR-008: Account Existence Validation

**Field/Entity:** ACCOUNT_ID

**Validation Type:** Entity Existence Validation

**Rule Description:**
The specified account must exist at the specified bank. Balance retrieval can only be performed for accounts that are registered in the system under the given bank.

**Validation Logic:**

- **Condition:** After bank existence validation passes
- **Check:** Query the system to verify the account exists at the specified bank
- **Valid Criteria:** 
  - Account with the specified ACCOUNT_ID exists in the system
  - Account belongs to the specified bank
- **Invalid Criteria:**
  - Account does not exist in the system
  - Account exists but belongs to a different bank
- **Action on Success:** Proceed with access permission validation
- **Action on Failure:** Return error response indicating account not found

**Error Handling:**

- **Error Message:** `Account not found at the specified bank. Please verify the Account ID.`
- **Error Code:** `OBP-30005`
- **HTTP Status Code:** `404 Not Found`

**Related Entities:**
- Account entity
- Bank entity

**User Story Context:**
The user story states "Account must exist in the system" as an upstream dependency. This validation ensures the account exists before attempting to retrieve its balance.

**Dependencies:**
- VR-007: Bank must exist first
- VR-002: Account ID must be present
- VR-006: Account ID format must be valid

---

### Rule VR-009: User Access Permission Validation

**Field/Entity:** User, Account, View

**Validation Type:** Authorization/Permission Validation

**Rule Description:**
The authenticated user must have appropriate permissions to access the balance information for the specified account. Users can only view balances for accounts they have been granted access to.

**Validation Logic:**

- **Condition:** After account existence validation passes and user is authenticated
- **Check:** Verify the user has view access permission for the specified account
- **Valid Criteria:** 
  - User has been granted access to view the account
  - User has the required entitlements/permissions
  - For view-specific endpoint: User has access to the specified view
- **Invalid Criteria:**
  - User has no access to the account
  - User lacks required permissions/entitlements
  - For view-specific endpoint: User does not have access to the specified view
- **Action on Success:** Proceed with balance retrieval
- **Action on Failure:** Return error response indicating access denied

**Error Handling:**

- **Error Message:** `Access denied. You do not have permission to view this account's balance.`
- **Error Code:** `OBP-20006`
- **HTTP Status Code:** `403 Forbidden`

**Related Entities:**
- User entity
- Account entity
- View entity
- AccountAccess entity
- Entitlements

**User Story Context:**
The user story explicitly states "User must have appropriate permissions/entitlements to access the account balance" and "Users can only access balance information for accounts they have permission to view" as business rules.

**Dependencies:**
- VR-004: User must be authenticated
- VR-008: Account must exist

---

## Category: Cross-Field Validation

### Rule VR-010: View Access Validation (View-Specific Endpoint)

**Field/Entity:** VIEW_ID, ACCOUNT_ID, User

**Validation Type:** Cross-Field Validation

**Rule Description:**
When using the view-specific endpoint, the specified view must exist for the account and the user must have access to that view. This validates the relationship between the view, account, and user permissions.

**Validation Logic:**

- **Condition:** When using the view-specific endpoint `/banks/{BANK_ID}/accounts/{ACCOUNT_ID}/{VIEW_ID}/balances`
- **Check:** 
  1. Verify the view exists for the specified account
  2. Verify the user has access to the specified view
- **Valid Criteria:** 
  - View with VIEW_ID exists and is associated with the account
  - User has been granted access to this specific view
  - View allows balance information access
- **Invalid Criteria:**
  - View does not exist
  - View exists but is not associated with the account
  - User does not have access to the view
  - View does not permit balance access
- **Action on Success:** Retrieve balance information according to view permissions
- **Action on Failure:** Return error response indicating view not found or access denied

**Error Handling:**

- **Error Message:** `View not found or access denied. Please verify the View ID and your permissions.`
- **Error Code:** `OBP-30006`
- **HTTP Status Code:** `404 Not Found` (if view doesn't exist) or `403 Forbidden` (if access denied)

**Related Entities:**
- View entity
- Account entity
- User entity
- ViewAccess entity

**User Story Context:**
The user story specifies that "View ID (when provided) must be a valid view that the user has access to" and that the view-specific endpoint retrieves "balance information for a specific account through a particular view (permission level)".

**Dependencies:**
- VR-003: View ID must be present (for view-specific endpoint)
- VR-008: Account must exist
- VR-009: User must have account access

---

## Validation Execution Order

The validations should be executed in the following order for optimal error handling:

1. **VR-004**: Authentication Token Required Validation
2. **VR-001**: Bank ID Required Validation
3. **VR-002**: Account ID Required Validation
4. **VR-003**: View ID Required Validation (if view-specific endpoint)
5. **VR-005**: Bank ID Format Validation
6. **VR-006**: Account ID Format Validation
7. **VR-007**: Bank Existence Validation
8. **VR-008**: Account Existence Validation
9. **VR-009**: User Access Permission Validation
10. **VR-010**: View Access Validation (if view-specific endpoint)

---

## Error Response Format

All validation failures should return a consistent error response format:

```json
{
  "code": "OBP-XXXXX",
  "message": "Error message describing the validation failure"
}
```

---

## Notes

- All validations are derived from the Account Balance Retrieval user story
- The validation rules support both balance retrieval endpoints defined in the user story
- Error codes follow the OBP-XXXXX format convention
- HTTP status codes follow REST API best practices (400 for bad request, 401 for unauthorized, 403 for forbidden, 404 for not found)
- The "Very High" volume classification mentioned in the user story suggests these validations should be optimized for performance
