# Business Rules Extraction: Account Listing User Story

## Overview
This document contains the comprehensive business rules extraction analysis for the Account Listing user story from the OBP API Account Management documentation. The analysis was performed using the 7-role business rules extraction framework.

**User Story Analyzed:** Account Listing (User Story 1)  
**Source:** obp_api_account_management_user_stories.md  
**Analysis Date:** November 6, 2025  
**Purpose:** Support migration from Scala application to Go application

---

## 🏦 Role 1: Bank Compliance Officer Perspective

### Rule Name: User Authentication Required for Account Access
**What it does:** Ensures only authenticated users can retrieve account lists  
**When it applies:** Every time a user attempts to retrieve their account list  
**Who it affects:** All banking application users and API consumers  
**Example:** Before displaying any account information, the system verifies the user's authentication token. If invalid or expired, the request is rejected with an authentication error.

### Rule Name: View Permission-Based Account Visibility
**What it does:** Only shows accounts where the user has at least one view permission  
**When it applies:** When generating the list of accounts to return to the user  
**Who it affects:** Users with partial access to accounts at a bank  
**Example:** A customer service representative with view access to only checking accounts will only see checking accounts in their account list, even if the bank has savings accounts they don't have permission to view.

---

## 👥 Role 2: Customer Service Manager Perspective

### Rule Name: Account Type Filtering Support
**What it does:** Allows users to filter account lists by account type (checking, savings, etc.)  
**When it applies:** When a user provides account type filter parameters in their request  
**Who it affects:** Users who want to view specific types of accounts only  
**Example:** A user can request to see only their savings accounts by including "account_type=SAVINGS" in the query parameters, hiding their checking and loan accounts from the response.

### Rule Name: Bank-Specific vs All-Banks Account Retrieval
**What it does:** Supports two modes: retrieve accounts at a specific bank OR across all banks the user has access to  
**When it applies:** Based on which API endpoint is called and whether Bank ID is provided  
**Who it affects:** Users with accounts at multiple banks  
**Example:** Using getAccountsHeldByUserAtBank requires specifying a bank_id and returns only accounts at Bank of America, while getAccountsHeldByUser returns accounts across all banks the user has access to.

---

## 🛡️ Role 3: Risk Management Specialist Perspective

### Rule Name: Entitlement-Based Access Scope Control
**What it does:** Restricts account listing scope based on user's entitlements  
**When it applies:** When validating if a user can retrieve accounts at one bank vs all banks  
**Who it affects:** Users with different privilege levels  
**Example:** A user with only "canGetAccountsHeldAtOneBank" entitlement can only query accounts at a single specified bank, while a user with "canGetAccountsHeldAtAnyBank" can query across all banks in the system.

### Rule Name: Performance-Based Response Time Requirement
**What it does:** Ensures account list retrieval completes within 2 seconds for typical requests  
**When it applies:** For every account listing request with standard number of accounts  
**Who it affects:** All users, especially those with time-sensitive applications  
**Example:** If a user has 50 accounts, the system must return the complete list within 2 seconds. If response time exceeds this, performance optimization is required (caching, indexing, etc.).

---

## 📦 Role 4: Product Manager Perspective

### Rule Name: Core Account Information Standard Response
**What it does:** Defines the minimum account information returned in list responses  
**When it applies:** For every account returned in the list  
**Who it affects:** API consumers and frontend applications  
**Example:** Each account in the response includes account ID, bank ID, label, and account_type fields. This ensures consistent data structure for all account listings regardless of the specific endpoint used.

### Rule Name: Pagination Support for Large Account Lists
**What it does:** Handles users with many accounts through pagination mechanism  
**When it applies:** When a user has more accounts than can be efficiently returned in a single response  
**Who it affects:** Users with numerous accounts, high-net-worth individuals, business customers  
**Example:** A business customer with 200 accounts receives paginated results with 50 accounts per page, allowing the system to maintain performance while still providing access to all accounts.

---

## ⚙️ Role 5: Operations Director Perspective

### Rule Name: View Permission Check Before Account Inclusion
**What it does:** Validates user has appropriate view permissions for each account before including it in results  
**When it applies:** During the account list generation process for each potential account  
**Who it affects:** System operations, data access layers  
**Example:** When building the account list, the system queries the ViewNewStyle service to check if the user has any view (owner, public, accountant, etc.) on each account before adding it to the response array.

### Rule Name: Bank Connector Integration for Account Data
**What it does:** Requires integration with bank connector to retrieve actual account information  
**When it applies:** When account data needs to be fetched from the underlying banking system  
**Who it affects:** External system integration, backend services  
**Example:** The APIMethods510 classes call the bank connector to fetch account records from the core banking system, ensuring the API returns current data rather than potentially stale cached information.

---

## 💰 Role 6: Treasury and Payment Specialist Perspective

### Rule Name: Account Holder Ownership Determination
**What it does:** Differentiates between accounts the user "holds" vs accounts they can "access"  
**When it applies:** When determining which accounts to include in the "held" account list  
**Who it affects:** Account owners, authorized users, delegates  
**Example:** A user who is the primary account holder will see their personal checking account in the list. An accountant granted view access to review the account will also see it, but the business logic may categorize these differently based on ownership vs access rights.

---

## 🔒 Role 7: Security and Access Control Manager Perspective

### Rule Name: User Identity Validation Requirement
**What it does:** Validates that the user ID exists in the system before processing the request  
**When it applies:** At the beginning of every account listing request  
**Who it affects:** All API users  
**Example:** If someone attempts to retrieve accounts for user_id="unknown_user", the system returns an error indicating the user does not exist, preventing information disclosure through enumeration attacks.

### Rule Name: Bank ID Validation When Specified
**What it does:** Validates that the bank ID is valid and active when provided  
**When it applies:** When a user requests accounts at a specific bank  
**Who it affects:** Users querying specific banks  
**Example:** If a request includes bank_id="INVALID_BANK", the system returns a validation error rather than attempting to query a non-existent bank, preventing potential security issues and improving error handling.

### Rule Name: Audit Trail for Account Access
**What it does:** Maintains logs of who accessed which account lists and when  
**When it applies:** For every successful and failed account listing request  
**Who it affects:** Security teams, auditors, compliance officers  
**Example:** Each account listing request is logged with timestamp, user ID, requested bank (if any), filters applied, and number of accounts returned, creating an audit trail for security and compliance reviews.

---

## 🎯 Additional Business Rules Identified

### Rule Name: Account Type Filter Validation
**What it does:** Validates that account type filter values match valid account types in the system  
**When it applies:** When account type filters are provided in the request  
**Who it affects:** API consumers providing filter parameters  
**Example:** If a user provides "account_type=INVALID_TYPE", the system returns a validation error listing the valid account types (CURRENT, SAVINGS, CREDIT, LOAN, etc.).

### Rule Name: Short TTL Caching for Frequent Requests
**What it does:** Implements short time-to-live caching to improve performance for repeated requests  
**When it applies:** For account list queries from the same user within a short time window  
**Who it affects:** High-frequency API users, mobile applications  
**Example:** When a user's mobile app requests the account list twice within 30 seconds, the second request may be served from cache rather than querying the database again, reducing load while ensuring reasonably fresh data.

---

## 📋 Summary of Business Rules Categories

**Access and Permission Rules:** 3 rules  
**Validation and Verification Rules:** 4 rules  
**Processing and Workflow Rules:** 2 rules  
**Financial and Calculation Rules:** 0 rules (not applicable for listing)  
**Compliance and Audit Rules:** 2 rules  
**Customer and Account Rules:** 2 rules  
**Security and Authentication Rules:** 4 rules  

**Total Business Rules Identified:** 17 distinct business rules

---

## 🔍 SME Input Required

Based on the analysis, the following items need Subject Matter Expert input:

1. **Held vs Accessible Accounts**: Clear business definition needed for what constitutes a "held" account versus an "accessible" account
2. **Default Account Types**: Complete list of valid account type values and their filtering logic
3. **Pagination Limits**: Specific threshold for when pagination triggers and page size limits
4. **Caching Policy**: Exact TTL values for caching and staleness tolerance
5. **Maximum Accounts Per Request**: If there's a hard limit on accounts returned in multi-account queries

---

## 📝 Original User Story Reference

### Story Overview
**As a** banking application user or API consumer  
**I want to** retrieve a list of all bank accounts I have access to  
**So that** I can view my accounts and select which one to perform operations on

### Acceptance Criteria (from original user story)
1. User can retrieve accounts held at a specific bank
2. User can retrieve accounts held across all banks they have access to
3. Response includes core account information (account ID, bank ID, account type)
4. Results can be filtered by account type (e.g., checking, savings)
5. Only accounts the user has permission to view are returned
6. System handles pagination for users with many accounts
7. Response time is under 2 seconds for typical user account lists

### Technical Context (from original user story)
- **Classes/Services Involved**: 
  - APIMethods510.getAccountsHeldByUserAtBank - retrieves accounts at specific bank
  - APIMethods510.getAccountsHeldByUser - retrieves accounts across all banks
  - ViewNewStyle - manages view permissions
  - JSONFactory300.createCoreAccountsByCoreAccountsJSON - formats response
- **Input Data**: User ID, Bank ID (optional), account type filters (query parameters)
- **Output Data**: JSON array of core account objects with id, bank_id, label, account_type
- **Processing Type**: Real-time REST API

### Business Rules from Original Code
1. User must be authenticated to retrieve account lists
2. Only accounts where user has at least one view permission are returned
3. Account type filtering is optional and supports multiple types
4. Results must respect user's entitlements (canGetAccountsHeldAtOneBank or canGetAccountsHeldAtAnyBank)

### Data Validations (from original user story)
- User ID must be valid and exist in system
- Bank ID must be valid if specified
- Account type filter values must match valid account types
- User must have appropriate entitlements for the requested scope

### Dependencies (from original user story)
- **Upstream**: User authentication and authorization
- **Downstream**: Account detail views, transaction retrieval, balance inquiries
- **External Systems**: Bank connector for retrieving actual account data

---

## 🚀 Migration Considerations for Go Application

When migrating this functionality from Scala to Go, ensure:

1. **Exact Endpoint Compatibility**: Maintain the same REST API endpoints and behavior
2. **Authentication & Authorization**: Implement the same authentication flow and entitlement checks
3. **View Permission Logic**: Replicate the ViewNewStyle permission checking mechanism
4. **Response Format**: Match the JSON structure exactly (JSONFactory300.createCoreAccountsByCoreAccountsJSON)
5. **Performance Requirements**: Meet the 2-second response time requirement
6. **Pagination Logic**: Implement consistent pagination behavior
7. **Error Handling**: Match error codes and messages for validation failures
8. **Bank Connector Integration**: Maintain compatibility with existing bank connector interface
9. **Caching Strategy**: Implement similar caching if present in Scala version
10. **Audit Logging**: Preserve audit trail functionality

---

## 📚 Related Documentation

- Source User Story: `Playbooks/user_stories/obp_api_account_management_user_stories.md`
- Business Rules Extraction Prompt: `Phase-01-Playbooks/business_rules_extraction_prompt.md` (obp-api repo)
- Original Scala Classes: APIMethods510, ViewNewStyle, JSONFactory300

---

*This analysis was generated as part of the Scala to Go migration project to ensure all business rules are properly captured and implemented in the new Go-based application.*
