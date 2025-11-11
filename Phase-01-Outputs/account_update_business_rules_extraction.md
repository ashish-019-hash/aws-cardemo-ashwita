# Business Rules Extraction: Account Update

## User Story Overview
**Feature:** Account Update  
**API Endpoints:**
- PUT /obp/v4.0.0/banks/{BANK_ID}/accounts/{ACCOUNT_ID}
- PUT /obp/v1.2.1/banks/{BANK_ID}/accounts/{ACCOUNT_ID}
- PUT /obp/v3.1.0/banks/{BANK_ID}/accounts/{ACCOUNT_ID}

**Purpose:** Modify account attributes such as labels and descriptions to keep account information current and organized

---

## Business Rules Extracted by Role

### Role 1: Bank Compliance Officer Perspective
**Focus:** Rules ensuring regulatory compliance and customer protection

#### Rule 1.1: Update Audit Trail Requirement
**What it does:** All account update attempts must be logged for regulatory audit purposes

**When it applies:** Every successful and failed account update request

**Who it affects:** Compliance officers, auditors, account owners, bank staff

**Example:** When a customer changes their account label from "Checking" to "Primary Checking", the system logs the user ID, timestamp, account ID, old value, new value, and update source, creating an audit trail for compliance reviews and dispute resolution.

#### Rule 1.2: Core Attribute Protection
**What it does:** Core account attributes (account number, type, currency) cannot be changed via update operations

**When it applies:** During update validation to prevent modification of immutable account attributes

**Who it affects:** Account owners, bank staff, system integrity

**Example:** When a user attempts to update an account, they can change the label and description, but cannot change the account type from "checking" to "savings" or modify the account number. These core attributes are immutable after account creation to maintain data integrity and regulatory compliance.

#### Rule 1.3: Update History Maintenance
**What it does:** Update history must be maintained for audit purposes showing who changed what and when

**When it applies:** After each successful account update

**Who it affects:** Compliance officers, auditors, dispute resolution teams

**Example:** The system maintains a complete history showing that on Jan 15 at 10:00 AM, User A changed the label from "Checking" to "Primary Checking", and on Jan 20 at 2:00 PM, User B changed the description from empty to "For monthly bills". This history is available for audit and dispute resolution.

---

### Role 2: Customer Service Manager Perspective
**Focus:** Rules governing customer interactions with the bank

#### Rule 2.1: Account Owner Self-Service Update
**What it does:** Account owners can update their own account labels and descriptions without requiring bank staff assistance

**When it applies:** When authenticated account owners access update endpoints

**Who it affects:** Account owners, customer service workload

**Example:** A customer logs into their mobile banking app and changes their savings account label from "Savings" to "Emergency Fund" without needing to call customer service or visit a branch, improving user experience and reducing support costs.

#### Rule 2.2: Immediate Update Reflection
**What it does:** Updated information must be immediately reflected in all account displays and listings

**When it applies:** After successful account update completion

**Who it affects:** Account owners, all users viewing the account

**Example:** When a customer updates their account label from "Checking" to "Primary Checking", the new label appears immediately in their account list, transaction history, and all other screens without requiring a page refresh or re-login.

#### Rule 2.3: Description Optional Nature
**What it does:** Account description is optional and can be empty, added, updated, or removed

**When it applies:** During account update operations

**Who it affects:** Account owners, bank staff

**Example:** A customer can create an account with just a label "Savings" and no description, later add a description "For vacation", then update it to "For emergency fund", or remove it entirely, all without validation errors.

---

### Role 3: Risk Management Specialist Perspective
**Focus:** Rules protecting the bank from fraud and financial risk

#### Rule 3.1: Dual Authorization Path
**What it does:** Updates can be performed either by account owner OR by users with CanUpdateAccountLabel/CanUpdateAccount entitlement

**When it applies:** During authorization validation for update requests

**Who it affects:** Account owners, authorized bank staff, customer service representatives

**Example:** A customer can update their own account label directly, OR a customer service representative with CanUpdateAccountLabel entitlement can update it on behalf of the customer during a support call. Either authorization path is valid.

#### Rule 3.2: Rate Limiting Consideration
**What it does:** Update operations should consider rate limiting to prevent abuse

**When it applies:** When processing multiple update requests from the same user or account

**Who it affects:** System performance, fraud prevention, legitimate users

**Example:** If a user attempts to update an account label 100 times in one minute, the system may throttle or temporarily block the requests to prevent automated abuse or system overload, while allowing normal usage patterns.

#### Rule 3.3: Concurrent Update Protection
**What it does:** Optimistic locking may be needed to handle concurrent updates safely

**When it applies:** When multiple users attempt to update the same account simultaneously

**Who it affects:** Account owners, authorized users, data integrity

**Example:** If a customer and a bank representative both try to update the same account label at the same time, the system uses version checking to ensure the second update doesn't overwrite the first without awareness, preventing lost updates.

---

### Role 4: Product Manager Perspective
**Focus:** Rules defining banking products and services

#### Rule 4.1: Label as Account Identifier
**What it does:** Account label serves as a user-friendly identifier to distinguish between multiple accounts

**When it applies:** Throughout the account lifecycle for display and identification

**Who it affects:** Account owners, user experience, account management

**Example:** A customer with three checking accounts can label them "Primary Checking", "Business Checking", and "Joint Checking" to easily identify which account to use for different purposes, improving usability and reducing transaction errors.

#### Rule 4.2: Multi-Version Update Capabilities
**What it does:** Different API versions support different update capabilities (v1.2.1 and v4.0.0 for label only, v3.1.0 for broader updates)

**When it applies:** Based on which API version endpoint is called

**Who it affects:** API consumers, application developers, feature availability

**Example:** An older mobile app using API v1.2.1 can only update the account label, while a newer web application using API v3.1.0 can update additional account details beyond just the label, providing enhanced functionality.

---

### Role 5: Operations Director Perspective
**Focus:** Rules governing internal bank processes and workflows

#### Rule 5.1: Bank ID and Account ID Matching Validation
**What it does:** The account must belong to the specified bank, and the bank ID in the request must match the account's bank

**When it applies:** During request validation before processing the update

**Who it affects:** API consumers, error handling systems, data integrity

**Example:** If a request tries to update account "ACC123" at "BANK_A" but the account actually belongs to "BANK_B", the system returns an error indicating the account does not exist at the specified bank, preventing cross-bank data corruption.

#### Rule 5.2: Account Existence and Active Status Validation
**What it does:** The account must exist and be active before updates can be applied

**When it applies:** During request validation for all update endpoints

**Who it affects:** API consumers, error handling systems

**Example:** If a request tries to update account "CLOSED_ACC_999" that has been closed, the system returns an error indicating the account is not active and cannot be updated, maintaining data integrity.

#### Rule 5.3: Atomic Update Operation
**What it does:** Account updates must be atomic - either all changes succeed or all fail

**When it applies:** During the update transaction processing

**Who it affects:** Data consistency, account owners, system reliability

**Example:** When updating both label and description, if the label update succeeds but the description update fails due to a database error, the entire operation is rolled back and the label remains unchanged, preventing partial updates.

#### Rule 5.4: Core Banking System Synchronization
**What it does:** Updates must be persisted to the core banking system via connector

**When it applies:** During the actual update operation

**Who it affects:** External system integration, data consistency, backend services

**Example:** The API calls the bank connector's updateBankAccount method to persist the changes to the core banking system, ensuring the account information is updated in the authoritative system and all downstream systems receive the changes.

---

### Role 6: Treasury and Payment Specialist Perspective
**Focus:** Rules controlling money movement and payment processing

#### Rule 6.1: No Financial Impact from Label Updates
**What it does:** Label and description updates do not affect account balance, limits, or financial operations

**When it applies:** During and after account update operations

**Who it affects:** Account owners, accounting systems, transaction processing

**Example:** When a customer changes their account label from "Checking" to "Primary Checking", the account balance remains unchanged, pending transactions are unaffected, and all financial operations continue normally. The update is purely informational.

---

### Role 7: Security and Access Control Manager Perspective
**Focus:** Rules protecting the system and controlling access

#### Rule 7.1: User Authentication Requirement
**What it does:** All account update requests must be made by authenticated users

**When it applies:** Every time a user attempts to update an account

**Who it affects:** All API consumers, security systems

**Example:** When a mobile app requests an account update, the system first validates the authentication token. If the token is expired or invalid, the request is rejected with a 401 Unauthorized error before any account data is accessed.

#### Rule 7.2: Owner or Entitlement Authorization
**What it does:** User must be the account owner OR have the appropriate entitlement (CanUpdateAccountLabel or CanUpdateAccount)

**When it applies:** After authentication but before processing the update

**Who it affects:** Account owners, authorized bank staff, access control

**Example:** When a user tries to update an account, the system checks if they are the account owner. If not, it checks if they have CanUpdateAccountLabel entitlement. Without either authorization, the request is denied with a 403 Forbidden error.

#### Rule 7.3: Unauthorized User Error Handling
**What it does:** Users without proper authorization receive a clear permission error

**When it applies:** When a user attempts to update an account they don't own and lack appropriate entitlements

**Who it affects:** Unauthorized users, security monitoring, error handling

**Example:** If User A tries to update User B's account without having CanUpdateAccountLabel entitlement, the system returns "403 Forbidden: You do not have permission to update this account" rather than revealing whether the account exists.

#### Rule 7.4: Label Non-Empty Validation
**What it does:** Account label cannot be empty - it must contain at least one character

**When it applies:** During update parameter validation

**Who it affects:** Account owners, bank staff, data quality

**Example:** If a user attempts to update an account label to an empty string "", the system rejects the request with an error "Label cannot be empty" to ensure all accounts have identifiable labels.

#### Rule 7.5: Label Length Validation
**What it does:** Account label length must be within defined limits (typically 1-255 characters)

**When it applies:** During update parameter validation

**Who it affects:** Account owners, bank staff, data storage

**Example:** If a user attempts to set an account label to a 500-character string, the system rejects the request with an error "Label must be between 1 and 255 characters" to prevent database overflow and maintain usability.

#### Rule 7.6: Label Format Validation
**What it does:** Account label must contain only allowed characters and follow format rules

**When it applies:** During update parameter validation

**Who it affects:** Account owners, bank staff, data quality

**Example:** If a user attempts to set an account label containing special characters that could cause SQL injection or XSS attacks, the system either sanitizes the input or rejects it with an error explaining the allowed character set.

---

## Summary of Business Rules by Category

### Access and Permission Rules
- Rule 3.1: Dual Authorization Path
- Rule 7.1: User Authentication Requirement
- Rule 7.2: Owner or Entitlement Authorization
- Rule 7.3: Unauthorized User Error Handling

### Validation and Verification Rules
- Rule 5.1: Bank ID and Account ID Matching Validation
- Rule 5.2: Account Existence and Active Status Validation
- Rule 7.4: Label Non-Empty Validation
- Rule 7.5: Label Length Validation
- Rule 7.6: Label Format Validation

### Processing and Workflow Rules
- Rule 2.2: Immediate Update Reflection
- Rule 5.3: Atomic Update Operation
- Rule 5.4: Core Banking System Synchronization
- Rule 4.2: Multi-Version Update Capabilities

### Financial and Calculation Rules
- Rule 6.1: No Financial Impact from Label Updates

### Compliance and Audit Rules
- Rule 1.1: Update Audit Trail Requirement
- Rule 1.2: Core Attribute Protection
- Rule 1.3: Update History Maintenance

### Customer and Account Rules
- Rule 2.1: Account Owner Self-Service Update
- Rule 2.3: Description Optional Nature
- Rule 4.1: Label as Account Identifier

### Transaction and Payment Rules
- (No specific transaction rules for account updates, covered by Rule 6.1)

### Security and Authentication Rules
- Rule 3.2: Rate Limiting Consideration
- Rule 3.3: Concurrent Update Protection

---

## Implementation Considerations

### Critical Business Rules for Migration
When migrating the Account Update functionality to Go, the following business rules are absolutely critical and must be preserved:

1. **Authentication and Authorization** (Rules 3.1, 7.1, 7.2): The dual authorization path (owner OR entitlement) must be replicated exactly
2. **Validation Rules** (Rules 5.1, 5.2, 7.4, 7.5, 7.6): All input validation must match the original behavior to maintain API contract
3. **Core Attribute Protection** (Rule 1.2): Immutable attributes must remain immutable
4. **Atomic Operations** (Rule 5.3): Updates must be atomic to maintain data consistency
5. **Audit Trail** (Rules 1.1, 1.3): All update attempts must be logged with complete history

### Testing Requirements
Each business rule should have corresponding test cases in the Go application:
- Test authenticated vs unauthenticated requests (Rule 7.1)
- Test account owner can update (Rule 7.2)
- Test user with CanUpdateAccountLabel entitlement can update (Rules 3.1, 7.2)
- Test user with CanUpdateAccount entitlement can update (Rules 3.1, 7.2)
- Test unauthorized user receives error (Rule 7.3)
- Test bank ID and account ID matching (Rule 5.1)
- Test account existence validation (Rule 5.2)
- Test label non-empty validation (Rule 7.4)
- Test label length validation (Rule 7.5)
- Test label format validation (Rule 7.6)
- Test description optional (Rule 2.3)
- Test core attributes cannot be changed (Rule 1.2)
- Test atomic update operation (Rule 5.3)
- Test immediate reflection of changes (Rule 2.2)
- Test audit trail logging (Rules 1.1, 1.3)
- Test multi-version API compatibility (Rule 4.2)
- Test concurrent update handling (Rule 3.3)

### Performance Considerations
- Implement rate limiting to prevent abuse (Rule 3.2)
- Optimize database queries for update operations
- Consider caching strategy for frequently updated accounts
- Implement efficient concurrent update handling (Rule 3.3)

### Audit and Compliance
- Implement comprehensive audit logging (Rule 1.1)
- Maintain complete update history (Rule 1.3)
- Ensure all update attempts are recorded with old and new values
- Log unauthorized access attempts for security monitoring

---

## Endpoint-Specific Business Rules

### PUT /obp/v4.0.0/banks/{BANK_ID}/accounts/{ACCOUNT_ID}

**Required Authorization:**
- Account owner OR CanUpdateAccountLabel entitlement

**Validation Rules:**
- User must be authenticated (Rule 7.1)
- User must be owner or have CanUpdateAccountLabel entitlement (Rules 3.1, 7.2)
- Bank ID must be valid (Rule 5.1)
- Account ID must be valid and active (Rule 5.2)
- Account must belong to specified bank (Rule 5.1)
- Label cannot be empty (Rule 7.4)
- Label length must be within limits (Rule 7.5)
- Label format must be valid (Rule 7.6)

**Processing Rules:**
- Update label only (not other attributes)
- Core attributes cannot be changed (Rule 1.2)
- Update must be atomic (Rule 5.3)
- Persist to core banking system (Rule 5.4)
- Reflect changes immediately (Rule 2.2)
- Log update attempt (Rule 1.1)
- Maintain update history (Rule 1.3)
- Return updated account information

### PUT /obp/v1.2.1/banks/{BANK_ID}/accounts/{ACCOUNT_ID}

**Required Authorization:**
- Account owner OR CanUpdateAccountLabel entitlement

**Validation Rules:**
- Same as v4.0.0 endpoint

**Processing Rules:**
- Same as v4.0.0 endpoint
- Support v1.2.1 request/response format (Rule 4.2)

### PUT /obp/v3.1.0/banks/{BANK_ID}/accounts/{ACCOUNT_ID}

**Required Authorization:**
- Account owner OR CanUpdateAccount entitlement

**Validation Rules:**
- User must be authenticated (Rule 7.1)
- User must be owner or have CanUpdateAccount entitlement (Rules 3.1, 7.2)
- Bank ID must be valid (Rule 5.1)
- Account ID must be valid and active (Rule 5.2)
- Account must belong to specified bank (Rule 5.1)
- Label cannot be empty if provided (Rule 7.4)
- Label length must be within limits if provided (Rule 7.5)
- Label format must be valid if provided (Rule 7.6)

**Processing Rules:**
- May support updating additional fields beyond label (Rule 4.2)
- Core attributes cannot be changed (Rule 1.2)
- Update must be atomic (Rule 5.3)
- Persist to core banking system (Rule 5.4)
- Reflect changes immediately (Rule 2.2)
- Log update attempt (Rule 1.1)
- Maintain update history (Rule 1.3)
- Return updated account information

---

## Input Parameter Business Rules

### label (Required for most endpoints)
**Validation:**
- Cannot be empty (Rule 7.4)
- Length must be 1-255 characters (Rule 7.5)
- Must contain only allowed characters (Rule 7.6)
- Special characters handling for security

### description (Optional)
**Validation:**
- Can be empty, null, or omitted (Rule 2.3)
- Length limits if provided
- Special characters handling for security

### Immutable Fields (Cannot be updated)
- account_number (Rule 1.2)
- account_type (Rule 1.2)
- currency (Rule 1.2)
- bank_id (Rule 1.2)
- account_id (Rule 1.2)

---

## Error Handling Business Rules

### Invalid Bank ID
**Response:** Error message indicating invalid bank  
**HTTP Status:** 400 Bad Request or 404 Not Found  
**Business Rule:** Rule 5.1

### Invalid Account ID
**Response:** Error message indicating account not found  
**HTTP Status:** 404 Not Found  
**Business Rule:** Rule 5.2

### Inactive Account
**Response:** Error message indicating account is not active  
**HTTP Status:** 400 Bad Request or 409 Conflict  
**Business Rule:** Rule 5.2

### Bank ID and Account ID Mismatch
**Response:** Error message indicating account does not belong to specified bank  
**HTTP Status:** 404 Not Found  
**Business Rule:** Rule 5.1

### Missing Authentication
**Response:** Authentication required error  
**HTTP Status:** 401 Unauthorized  
**Business Rule:** Rule 7.1

### Insufficient Authorization
**Response:** Permission denied error  
**HTTP Status:** 403 Forbidden  
**Business Rule:** Rules 7.2, 7.3

### Empty Label
**Response:** Error message indicating label cannot be empty  
**HTTP Status:** 400 Bad Request  
**Business Rule:** Rule 7.4

### Label Too Long
**Response:** Error message with length limits  
**HTTP Status:** 400 Bad Request  
**Business Rule:** Rule 7.5

### Invalid Label Format
**Response:** Error message with allowed character set  
**HTTP Status:** 400 Bad Request  
**Business Rule:** Rule 7.6

### Attempt to Modify Core Attributes
**Response:** Error message indicating attribute is immutable  
**HTTP Status:** 400 Bad Request or 403 Forbidden  
**Business Rule:** Rule 1.2

---

## Data Integrity and Consistency Rules

### Atomic Update Guarantee
- All field updates in a single request must succeed or fail together (Rule 5.3)
- No partial updates allowed
- Rollback on any failure

### Immediate Consistency
- Updates must be immediately visible after successful completion (Rule 2.2)
- No eventual consistency delay
- All views of the account show updated information

### Audit Trail Completeness
- Every update attempt logged (Rule 1.1)
- Complete history maintained (Rule 1.3)
- Old and new values recorded
- User and timestamp captured

---

## Conclusion

This business rules extraction identifies 22 distinct business rules governing the Account Update functionality across 7 different organizational perspectives. These rules cover authentication, authorization, validation, atomic operations, audit trail, and data integrity requirements. When migrating this functionality to Go, all these rules must be preserved to ensure the new implementation maintains functional equivalence with the Scala application and can be validated using the existing test cases.

The account update process is particularly important for user experience and data integrity. The Go implementation must ensure proper authorization (owner OR entitlement), comprehensive validation, atomic operations, immediate reflection of changes, and complete audit logging to maintain system reliability and regulatory compliance.
