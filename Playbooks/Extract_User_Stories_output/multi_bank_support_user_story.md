User story: Multi-Bank Support

## Story Overview
**As a** System Administrator  
**I want to** support multiple banking institutions on a single API instance with isolated data  
**So that** multiple banks can use the same API infrastructure while maintaining complete data separation and security

## Acceptance Criteria
1. System must support hosting multiple banking institutions on a single API instance
2. System must ensure complete data isolation between different banks
3. System must require bank identifier for all bank-specific resource access
4. System must prevent cross-bank data access (Bank A cannot access Bank B's data)
5. System must support bank-scoped resources (accounts, transactions, attributes, etc.)
6. System must validate bank identifier exists before allowing resource access
7. System must return appropriate errors when accessing non-existent banks

## Technical Context
- **Classes/Services Involved**: 
  - BankService: Validates bank existence and manages bank context
  - AuthorizationService: Ensures users have proper entitlements for specific banks
  - ResourceAccessService: Enforces bank-scoped access to all resources
  - DataIsolationService: Ensures queries are scoped to specific banks
- **Input Data**: 
  - Bank identifier (bankId) in URL path for all bank-specific operations
  - User authentication credentials
  - Resource identifiers (accountId, transactionId, etc.)
- **Output Data**: 
  - Bank-scoped resource data
  - Access denied errors for cross-bank access attempts
  - Bank not found errors for invalid bank identifiers
- **Processing Type**: REST API with bank-scoped access control

## Relevant Endpoints

The multi-bank support capability is implemented through bank-scoped endpoints across all resources. Key endpoint patterns include:

### 1. Bank-Scoped Account Access
- **Endpoint**: GET /banks/BANK_ID/accounts
  - **Purpose**: Retrieve accounts for a specific bank (data isolated by bank)
  - **Request**: 
    - Path parameter: BANK_ID (bank identifier)
  - **Response**: 
    - List of accounts belonging only to the specified bank

### 2. Bank-Scoped Transaction Access
- **Endpoint**: GET /banks/BANK_ID/accounts/ACCOUNT_ID/VIEW_ID/transactions
  - **Purpose**: Retrieve transactions for a specific bank's account (data isolated by bank)
  - **Request**: 
    - Path parameters: BANK_ID, ACCOUNT_ID, VIEW_ID
  - **Response**: 
    - Transactions belonging only to the specified bank's account

### 3. Bank-Scoped Attribute Access
- **Endpoint**: GET /banks/BANK_ID/attributes
  - **Purpose**: Retrieve attributes for a specific bank (data isolated by bank)
  - **Request**: 
    - Path parameter: BANK_ID
  - **Response**: 
    - Attributes belonging only to the specified bank

### 4. Bank-Scoped Entitlements
- **Endpoint**: GET /banks/BANK_ID/entitlements
  - **Purpose**: Retrieve entitlements for a specific bank (data isolated by bank)
  - **Request**: 
    - Path parameter: BANK_ID
  - **Response**: 
    - Entitlements scoped to the specified bank

### 5. Bank-Scoped Dynamic Entities
- **Endpoint**: GET /management/banks/BANK_ID/dynamic-entities
  - **Purpose**: Retrieve dynamic entities for a specific bank (data isolated by bank)
  - **Request**: 
    - Path parameter: BANK_ID
  - **Response**: 
    - Dynamic entities belonging only to the specified bank

**Note**: Multi-bank support is implemented as a cross-cutting concern across all bank-specific endpoints. Every endpoint that accesses bank-specific resources includes BANK_ID as a path parameter to ensure data isolation. The system validates the bank identifier exists and ensures users have appropriate entitlements for that specific bank before allowing access to any resources.

## Business Rules

1. **Mandatory Bank Identifier**: All bank-specific resource access must include a valid bank identifier
2. **Bank Existence Validation**: Bank identifier must exist in the system before accessing any bank-specific resources
3. **Data Isolation Enforcement**: System must ensure queries and operations are scoped to the specified bank only
4. **Cross-Bank Access Prevention**: Users cannot access resources from Bank A when the endpoint specifies Bank B
5. **Bank-Scoped Entitlements**: User permissions and entitlements are scoped to specific banks
6. **Bank-Scoped Resources**: All resources (accounts, transactions, attributes, etc.) belong to exactly one bank
7. **Single API Instance**: Multiple banks share the same API infrastructure and codebase
8. **Independent Bank Operations**: Operations on one bank do not affect other banks' data or operations

## Data Validations

- Bank identifier must be provided in URL path for all bank-specific operations
- Bank identifier must exist in the system (return 404 if bank not found)
- User must have appropriate entitlements for the specified bank
- Resource identifiers (accountId, transactionId, etc.) must belong to the specified bank
- Cross-bank resource access attempts must be rejected with appropriate error

## Dependencies

- Authentication service (to verify user identity)
- Authorization service (to check bank-specific entitlements)
- Bank service (to validate bank existence)
- Data access layer (to enforce bank-scoped queries)

## Notes for Implementation

### Special Considerations
- Multi-bank support is a cross-cutting architectural concern affecting all bank-specific endpoints
- Data isolation is enforced at multiple layers: URL routing, authorization, and data access
- Bank identifier is extracted from URL path and used to scope all database queries
- Each bank's data is logically separated even though stored in the same database instance
- User entitlements are bank-specific (e.g., canCreateAccount at Bank A ≠ canCreateAccount at Bank B)
- API instance serves multiple banks simultaneously with complete data separation
- Performance considerations: bank-scoped queries must be optimized to handle multiple banks efficiently
- Audit logs must include bank identifier to track operations across different banks

### Questions for SME
1. Are there any limits on the number of banks that can be supported on a single API instance?
2. How are bank identifiers assigned (auto-generated, user-provided, external system)?
3. Can users have access to multiple banks simultaneously?
4. Are there any shared resources across banks (e.g., system-level configurations)?
5. How is data isolation tested to ensure no cross-bank data leakage?
6. What happens to bank-specific resources when a bank is deactivated or removed?
