# Business Rules Extraction

**Extracted From**: Account Routing Lookup Capability
**Analysis Date**: 2026-01-20
**Analyst**: Expert Scala Business Logic Analyst
**Migration Target**: Go Application

## Executive Summary
- Total Business Rules Extracted: 6
- API Endpoints Analyzed: 3
- Rule Categories:
  - Calculations: 0
  - Decisions: 2
  - Thresholds: 0
  - Aggregations: 0
  - Workflows: 1
  - Transformations: 3

## Business Rules Catalog

### BR-001: Routing Scheme Validation

**Category**: DECISION

**Description**: The system must validate that the provided routing scheme is a supported type before performing account lookup operations.

**Source**: 
- File: Connector.scala, LocalMappedConnector.scala
- Class/Object: Connector, LocalMappedConnector
- Method: getBankAccountByRouting, getBankAccountByIban, getBankAccountByRoutings
- Lines: N/A (derived from user story)

**Business Logic**:
1. Receive routing scheme parameter from the request
2. Validate that the scheme is one of the supported types: IBAN, ACCOUNT_NUMBER, BIC, AccountNo, or other configured schemes
3. If scheme is not supported, reject the request with appropriate error
4. If scheme is valid, proceed with account lookup

**Variables**:
- **Input**: scheme (String) - The routing scheme type provided in the request
- **Output**: Boolean - Whether the scheme is valid and supported
- **Constants**: Supported schemes list: ["IBAN", "ACCOUNT_NUMBER", "BIC", "AccountNo"]

**Business Conditions**:
| Condition | Business Meaning | Values/Thresholds |
|-----------|------------------|-------------------|
| scheme in supported_schemes | Routing scheme is recognized | IBAN, ACCOUNT_NUMBER, BIC, AccountNo |
| scheme is empty | Invalid request | Reject with error |

**Business Impact**: 
Ensures that only valid routing schemes are processed, preventing invalid lookups and maintaining data integrity. This is critical for payment processing and account verification workflows.

**API Endpoints Using This Rule**:
- GET /obp/v4.0.0/banks/{BANK_ID}/accounts/account-routing-query - Account routing query
- GET /obp/v4.0.0/accounts/iban/{IBAN} - IBAN lookup
- GET /obp/v4.0.0/banks/{BANK_ID}/accounts/account-number/{ACCOUNT_NUMBER} - Account number lookup

**Related Test Cases**:
- Test valid routing scheme acceptance
- Test invalid routing scheme rejection
- Test empty scheme handling

**Migration Notes for Go**:
- Implement scheme validation as a separate function that can be reused across endpoints
- Use a map or slice to store supported schemes for easy configuration
- Return appropriate HTTP error codes (400 Bad Request) for invalid schemes

**Example Scenarios**:
```
Scenario 1: Valid IBAN scheme
Input: scheme = "IBAN"
Processing: Check if "IBAN" is in supported schemes list
Output: Valid - proceed with lookup

Scenario 2: Invalid scheme
Input: scheme = "SWIFT"
Processing: Check if "SWIFT" is in supported schemes list
Output: Invalid - return error OBP-10001
```

---

### BR-002: IBAN Format Validation

**Category**: TRANSFORMATION

**Description**: When IBAN is used as the routing scheme, the system must validate the IBAN format according to ISO 13616 standards before performing the account lookup.

**Source**: 
- File: Connector.scala, LocalMappedConnector.scala
- Class/Object: Connector, LocalMappedConnector
- Method: getBankAccountByIban
- Lines: N/A (derived from user story)

**Business Logic**:
1. Receive IBAN address from the request
2. Validate IBAN length is between 15-34 characters
3. Validate IBAN starts with a valid 2-letter country code
4. Validate IBAN contains valid check digits (positions 3-4)
5. Validate IBAN follows the country-specific format
6. If validation fails, reject with appropriate error
7. If validation passes, proceed with account lookup

**Variables**:
- **Input**: address (String) - The IBAN value provided in the request
- **Output**: Boolean - Whether the IBAN format is valid
- **Constants**: 
  - Minimum IBAN length: 15 characters
  - Maximum IBAN length: 34 characters
  - Country code position: characters 1-2
  - Check digits position: characters 3-4

**Business Conditions**:
| Condition | Business Meaning | Values/Thresholds |
|-----------|------------------|-------------------|
| length >= 15 AND length <= 34 | Valid IBAN length | 15-34 characters |
| starts with valid country code | Valid country identifier | 2-letter ISO country code |
| valid check digits | IBAN integrity verified | Positions 3-4 |

**Business Impact**: 
Ensures that only properly formatted IBANs are processed, preventing lookup failures and ensuring compliance with international banking standards. Critical for cross-border payment processing.

**API Endpoints Using This Rule**:
- GET /obp/v4.0.0/accounts/iban/{IBAN} - IBAN lookup
- GET /obp/v4.0.0/banks/{BANK_ID}/accounts/account-routing-query - When scheme is IBAN

**Related Test Cases**:
- Test valid IBAN format acceptance
- Test IBAN length validation (too short, too long)
- Test invalid country code rejection
- Test invalid check digit rejection

**Migration Notes for Go**:
- Implement IBAN validation as a dedicated utility function
- Consider using a Go IBAN validation library or implement ISO 13616 check digit algorithm
- Handle case-insensitive comparison for country codes
- Return specific error messages indicating which validation failed

**Example Scenarios**:
```
Scenario 1: Valid German IBAN
Input: address = "DE89370400440532013000"
Processing: Length=22 (valid), Country=DE (valid), Check digits=89 (valid)
Output: Valid - proceed with lookup

Scenario 2: Invalid IBAN - too short
Input: address = "DE893704"
Processing: Length=8 (invalid - less than 15)
Output: Invalid - return format error

Scenario 3: Invalid country code
Input: address = "XX89370400440532013000"
Processing: Country=XX (invalid - not a valid ISO country code)
Output: Invalid - return country code error
```

---

### BR-003: Account Number Scope Requirement

**Category**: DECISION

**Description**: When using account number for lookup, a bank_id must be provided to narrow the search scope, as account numbers may not be globally unique across banks.

**Source**: 
- File: Connector.scala, LocalMappedConnector.scala
- Class/Object: Connector, LocalMappedConnector
- Method: getBankAccountByRouting
- Lines: N/A (derived from user story)

**Business Logic**:
1. Receive account number lookup request
2. Check if bank_id is provided in the request
3. If bank_id is missing, reject the request or return appropriate guidance
4. If bank_id is provided, scope the search to that specific bank
5. Perform account lookup within the specified bank scope

**Variables**:
- **Input**: 
  - address (String) - The account number value
  - bank_id (String, required for account number lookups) - The bank identifier
- **Output**: Account details or error response
- **Constants**: None

**Business Conditions**:
| Condition | Business Meaning | Values/Thresholds |
|-----------|------------------|-------------------|
| scheme == "ACCOUNT_NUMBER" AND bank_id is empty | Invalid request - scope required | Reject or warn |
| scheme == "ACCOUNT_NUMBER" AND bank_id is provided | Valid scoped lookup | Proceed with lookup |

**Business Impact**: 
Prevents ambiguous account lookups that could return incorrect accounts. Account numbers are not globally unique, so bank scope is essential for accurate identification. Critical for payment routing accuracy.

**API Endpoints Using This Rule**:
- GET /obp/v4.0.0/banks/{BANK_ID}/accounts/account-number/{ACCOUNT_NUMBER} - Account number lookup (bank_id required in path)
- GET /obp/v4.0.0/banks/{BANK_ID}/accounts/account-routing-query - When scheme is ACCOUNT_NUMBER

**Related Test Cases**:
- Test account number lookup with valid bank_id
- Test account number lookup without bank_id (should fail or warn)
- Test account number uniqueness within bank scope

**Migration Notes for Go**:
- Enforce bank_id requirement at the API handler level for account number lookups
- Return clear error message when bank_id is missing for account number scheme
- Consider making bank_id optional for IBAN lookups but required for account number lookups

**Example Scenarios**:
```
Scenario 1: Valid account number lookup with bank_id
Input: scheme = "ACCOUNT_NUMBER", address = "0532013000", bank_id = "gh.29.uk"
Processing: Bank scope provided, search within bank "gh.29.uk"
Output: Account found - return account details

Scenario 2: Account number lookup without bank_id
Input: scheme = "ACCOUNT_NUMBER", address = "0532013000", bank_id = ""
Processing: No bank scope provided
Output: Error - bank_id required for account number lookups
```

---

### BR-004: Routing Address Case Insensitivity

**Category**: TRANSFORMATION

**Description**: Routing addresses should be handled in a case-insensitive manner where applicable, particularly for IBAN lookups.

**Source**: 
- File: Connector.scala, LocalMappedConnector.scala
- Class/Object: Connector, LocalMappedConnector
- Method: getBankAccountByIban, getBankAccountByRouting
- Lines: N/A (derived from user story)

**Business Logic**:
1. Receive routing address from the request
2. Normalize the address to uppercase for comparison (especially for IBAN)
3. Perform lookup using the normalized address
4. Return results regardless of the original case provided

**Variables**:
- **Input**: address (String) - The routing address in any case
- **Output**: Normalized address (String) - The address converted to uppercase
- **Constants**: None

**Business Conditions**:
| Condition | Business Meaning | Values/Thresholds |
|-----------|------------------|-------------------|
| scheme == "IBAN" | Case-insensitive comparison required | Normalize to uppercase |
| Any routing address | Consistent comparison | Apply normalization |

**Business Impact**: 
Improves user experience by accepting routing addresses in any case format. Prevents lookup failures due to case mismatches. Standard practice for IBAN handling in banking systems.

**API Endpoints Using This Rule**:
- GET /obp/v4.0.0/accounts/iban/{IBAN} - IBAN lookup
- GET /obp/v4.0.0/banks/{BANK_ID}/accounts/account-routing-query - All routing lookups

**Related Test Cases**:
- Test IBAN lookup with lowercase input
- Test IBAN lookup with mixed case input
- Test IBAN lookup with uppercase input

**Migration Notes for Go**:
- Use strings.ToUpper() for address normalization before database lookup
- Store IBANs in uppercase in the database for consistent comparison
- Apply normalization early in the request processing pipeline

**Example Scenarios**:
```
Scenario 1: Lowercase IBAN input
Input: address = "de89370400440532013000"
Processing: Normalize to "DE89370400440532013000"
Output: Account found using normalized IBAN

Scenario 2: Mixed case IBAN input
Input: address = "De89370400440532013000"
Processing: Normalize to "DE89370400440532013000"
Output: Account found using normalized IBAN
```

---

### BR-005: Single Account Return Policy

**Category**: WORKFLOW

**Description**: The account routing lookup should return a single account match. If multiple accounts match the same routing information (which should be rare for unique identifiers like IBAN), the system should handle this appropriately.

**Source**: 
- File: Connector.scala, LocalMappedConnector.scala
- Class/Object: Connector, LocalMappedConnector
- Method: getBankAccountByRouting, getBankAccountByIban
- Lines: N/A (derived from user story)

**Business Logic**:
1. Perform account lookup using routing information
2. Retrieve all matching accounts
3. If exactly one account matches, return that account
4. If no accounts match, return "Account not found" error
5. If multiple accounts match (edge case), apply business logic to select the appropriate account or return an error

**Variables**:
- **Input**: Routing scheme and address
- **Output**: Single account details or error response
- **Constants**: None

**Business Conditions**:
| Condition | Business Meaning | Values/Thresholds |
|-----------|------------------|-------------------|
| match_count == 0 | No account found | Return OBP-30018 error |
| match_count == 1 | Unique match found | Return account details |
| match_count > 1 | Ambiguous match | Handle according to policy |

**Business Impact**: 
Ensures deterministic account identification for payment processing. Prevents ambiguous routing that could lead to incorrect fund transfers. Critical for payment accuracy and regulatory compliance.

**API Endpoints Using This Rule**:
- GET /obp/v4.0.0/accounts/iban/{IBAN} - IBAN lookup
- GET /obp/v4.0.0/banks/{BANK_ID}/accounts/account-number/{ACCOUNT_NUMBER} - Account number lookup
- GET /obp/v4.0.0/banks/{BANK_ID}/accounts/account-routing-query - Routing query

**Related Test Cases**:
- Test single account match scenario
- Test no account match scenario
- Test multiple account match scenario (edge case)

**Migration Notes for Go**:
- Implement result count checking after database query
- Return appropriate HTTP status codes: 200 for found, 404 for not found
- Log multiple match scenarios for investigation
- Consider returning first match or error based on business policy

**Example Scenarios**:
```
Scenario 1: Single account found
Input: IBAN = "DE89370400440532013000"
Processing: Query returns 1 account
Output: Return account details with 200 OK

Scenario 2: No account found
Input: IBAN = "DE89370400440532013999"
Processing: Query returns 0 accounts
Output: Return OBP-30018 "Bank Account not found" with 404

Scenario 3: Multiple accounts found (edge case)
Input: Account number = "0532013000", bank_id = "gh.29.uk"
Processing: Query returns 2 accounts
Output: Return error or apply selection policy
```

---

### BR-006: Routing Scheme Normalization

**Category**: TRANSFORMATION

**Description**: Handle variations in routing scheme naming by normalizing to a standard format internally. Different banks may use different names for the same type of identifier.

**Source**: 
- File: Connector.scala, LocalMappedConnector.scala
- Class/Object: Connector, LocalMappedConnector
- Method: getBankAccountByRouting, getAccountRoutingsByScheme
- Lines: N/A (derived from user story)

**Business Logic**:
1. Receive routing scheme from the request
2. Map scheme variations to standard internal names:
   - "AccountNo" -> "ACCOUNT_NUMBER"
   - "account_number" -> "ACCOUNT_NUMBER"
   - "iban" -> "IBAN"
3. Use the normalized scheme for database lookup
4. Return results using the original or normalized scheme name

**Variables**:
- **Input**: scheme (String) - The routing scheme as provided in the request
- **Output**: normalized_scheme (String) - The standardized scheme name
- **Constants**: 
  - Scheme mapping: {"AccountNo": "ACCOUNT_NUMBER", "account_number": "ACCOUNT_NUMBER", "iban": "IBAN"}

**Business Conditions**:
| Condition | Business Meaning | Values/Thresholds |
|-----------|------------------|-------------------|
| scheme == "AccountNo" | Alternative account number naming | Map to "ACCOUNT_NUMBER" |
| scheme == "iban" (lowercase) | Case variation | Map to "IBAN" |

**Business Impact**: 
Improves interoperability with different banking systems that may use varying naming conventions. Reduces integration friction for third-party applications. Ensures consistent internal processing regardless of input variations.

**API Endpoints Using This Rule**:
- GET /obp/v4.0.0/banks/{BANK_ID}/accounts/account-routing-query - Routing query with scheme parameter

**Related Test Cases**:
- Test scheme normalization for "AccountNo" to "ACCOUNT_NUMBER"
- Test scheme normalization for lowercase "iban" to "IBAN"
- Test standard scheme names pass through unchanged

**Migration Notes for Go**:
- Implement a scheme normalization function using a map for lookups
- Apply normalization early in request processing
- Consider making normalization case-insensitive
- Document supported scheme aliases in API documentation

**Example Scenarios**:
```
Scenario 1: AccountNo scheme variation
Input: scheme = "AccountNo"
Processing: Normalize to "ACCOUNT_NUMBER"
Output: Lookup performed using "ACCOUNT_NUMBER" scheme

Scenario 2: Lowercase iban
Input: scheme = "iban"
Processing: Normalize to "IBAN"
Output: Lookup performed using "IBAN" scheme

Scenario 3: Standard scheme name
Input: scheme = "IBAN"
Processing: No normalization needed
Output: Lookup performed using "IBAN" scheme
```

---

## API Endpoint Coverage

| Endpoint | HTTP Method | Business Rules | Rule IDs |
|----------|-------------|----------------|----------|
| /obp/v4.0.0/banks/{BANK_ID}/accounts/account-routing-query | GET | Routing scheme validation, IBAN format validation, Account number scope, Case insensitivity, Single account return, Scheme normalization | BR-001, BR-002, BR-003, BR-004, BR-005, BR-006 |
| /obp/v4.0.0/accounts/iban/{IBAN} | GET | Routing scheme validation, IBAN format validation, Case insensitivity, Single account return | BR-001, BR-002, BR-004, BR-005 |
| /obp/v4.0.0/banks/{BANK_ID}/accounts/account-number/{ACCOUNT_NUMBER} | GET | Routing scheme validation, Account number scope, Single account return | BR-001, BR-003, BR-005 |

## Migration Validation Matrix

| Rule ID | Test Case Reference | Go Implementation Status | Validation Status |
|---------|---------------------|--------------------------|-------------------|
| BR-001 | TestRoutingSchemeValidation | Pending | Pending |
| BR-002 | TestIBANFormatValidation | Pending | Pending |
| BR-003 | TestAccountNumberScopeRequirement | Pending | Pending |
| BR-004 | TestRoutingAddressCaseInsensitivity | Pending | Pending |
| BR-005 | TestSingleAccountReturnPolicy | Pending | Pending |
| BR-006 | TestRoutingSchemeNormalization | Pending | Pending |

## Notes and Assumptions

1. **Source Code Reference**: The business rules are derived from the user story document as the actual Scala source code was not directly analyzed. The source file references (Connector.scala, LocalMappedConnector.scala) are based on the technical context provided in the user story.

2. **IBAN Validation**: The IBAN validation rule (BR-002) follows ISO 13616 standards. The actual implementation may use external validation services or libraries.

3. **Multiple Account Handling**: The behavior for multiple account matches (BR-005) is flagged as needing SME input in the user story. The current rule assumes an error or policy-based selection approach.

4. **Scheme Variations**: The scheme normalization rule (BR-006) is based on the user story's mention of handling variations like "AccountNo" vs "ACCOUNT_NUMBER". Additional variations may exist in the actual implementation.

5. **Authorization Rules**: While the user story mentions authorization requirements (CanGetAccountByRouting entitlement), these are considered technical/infrastructure concerns and are not included as business rules per the extraction guidelines.

6. **Error Codes**: The error codes referenced (OBP-30018, OBP-10001, OBP-20001, OBP-20006) are from the user story's data validation section and should be preserved in the Go implementation.
