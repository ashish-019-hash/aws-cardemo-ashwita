# Business Requirements Document (BRD)
# CardDemo Credit Card Management System

**Document Version:** 1.0  
**Date:** October 27, 2025  
**Prepared By:** Business Analysis Team  
**Source System:** CardDemo CICS COBOL Application  
**Purpose:** Comprehensive business requirements extraction for system modernization

---

## Table of Contents

1. [Executive Summary](#1-executive-summary)
2. [Business Context](#2-business-context)
3. [Functional Requirements](#3-functional-requirements)
4. [Business Rules](#4-business-rules)
5. [Data Requirements](#5-data-requirements)
6. [Process Requirements](#6-process-requirements)
7. [User Requirements](#7-user-requirements)
8. [Integration Requirements](#8-integration-requirements)
9. [Non-Functional Requirements](#9-non-functional-requirements)
10. [Assumptions & Constraints](#10-assumptions--constraints)
11. [Business Glossary](#11-business-glossary)
12. [Traceability Matrix](#12-traceability-matrix)
13. [Open Questions & Risks](#13-open-questions--risks)

---

## 1. Executive Summary

### 1.1 System Purpose
The CardDemo system is a comprehensive credit card management application enabling financial institutions to manage customer accounts, credit cards, transactions, and user access. It provides essential banking operations including account management, card lifecycle management, transaction processing, bill payment, and administrative functions.

### 1.2 Business Objectives
- **Customer Self-Service**: Enable customers to independently view and manage accounts, cards, and transactions
- **Financial Transaction Processing**: Accurately process bill payments and record all financial transactions
- **Credit Card Lifecycle Management**: Manage complete card lifecycle from issuance to expiration
- **Administrative Efficiency**: Provide administrators tools to manage users, generate reports, and oversee operations
- **Security & Compliance**: Ensure secure access through role-based authentication and maintain audit trails
- **Operational Excellence**: Maintain accurate account balances, credit limits, and transaction records

### 1.3 Scope

**In Scope:**
- User authentication and role-based access control
- Account viewing and updating capabilities
- Credit card portfolio management
- Transaction history browsing and entry
- Bill payment processing with balance updates
- User administration (creation, updates, deletion)
- Transaction reporting capabilities
- Comprehensive data validation (56 validation rules)

**Out of Scope:**
- Interest calculations and fee computations
- Credit scoring and fraud detection algorithms
- External payment gateway integrations
- Customer onboarding and KYC processes
- Dispute management and chargebacks
- Rewards and loyalty programs

### 1.4 Key Stakeholders
- **Regular Users (Customers)**: Credit card holders monitoring financial activity
- **Bank Administrators**: Staff managing system administration and operations
- **Compliance & Audit Teams**: Groups ensuring regulatory compliance
- **IT Operations**: Technical teams maintaining system availability
- **Business Management**: Leadership requiring analytics for decision-making

### 1.5 Business Value
- **Operational Efficiency**: Reduces manual processing through customer self-service
- **Cost Savings**: Minimizes branch visits and phone support requirements
- **Customer Satisfaction**: Provides 24/7 access to account information
- **Data Accuracy**: Ensures consistency through comprehensive validation
- **Regulatory Compliance**: Maintains audit trails and secure access controls

---

## 2. Business Context

### 2.1 Organizational Context
The CardDemo system operates within a financial institution's credit card division, supporting retail banking operations and administrative functions. The system is critical for day-to-day credit card account management and customer service.

**Department Ownership**: Credit Card Operations Division  
**System Criticality**: High - Essential for customer account access and payment processing

### 2.2 Business Drivers
1. **Digital Transformation**: Transition from manual operations to automated self-service
2. **Customer Experience**: Provide convenient, always-available account access
3. **Operational Excellence**: Standardize processes and reduce manual errors
4. **Risk Management**: Maintain accurate financial records and audit trails
5. **Cost Optimization**: Reduce operational costs through automation

### 2.3 Impact Assessment
**If System is Unavailable:**
- Customers cannot view account information or make payments
- Transactions cannot be recorded or processed
- Account balances become stale and inaccurate
- Customer service operations severely impacted
- Payment collection is disrupted

---

## 3. Functional Requirements

### 3.1 Authentication & Access Management

**FR-AUTH-001: User Sign-On**  
Priority: High  
The system shall provide secure sign-on where users enter user ID and password to gain access. System validates credentials against user security database and displays appropriate error messages for invalid credentials.

**FR-AUTH-002: Role-Based Dashboard Routing**  
Priority: High  
Upon successful authentication, system shall automatically route users to role-appropriate dashboards (Admin users to administrative menu, Regular users to main menu) based on user type from security record.

**FR-AUTH-003: Session Management**  
Priority: High  
System shall maintain user session context across all transactions and screens, preserving user identity and navigation history throughout user interaction.

### 3.2 Account Management

**FR-ACCT-001: Account Detail Viewing**  
Priority: High  
System shall allow authorized users to view complete account details including balance, credit limits, dates, and cycle information.

**FR-ACCT-002: Account Information Update**  
Priority: High  
System shall allow authorized users to update account information with comprehensive validation (23 validation rules applied). Confirmation required before committing changes.

**FR-ACCT-003: Cross-Reference Navigation**  
Priority: Medium  
System shall enable navigation between related accounts, customers, and cards using cross-reference data.

### 3.3 Credit Card Management

**FR-CARD-001: Card Portfolio Listing**  
Priority: High  
System shall display paginated list of credit cards with forward/backward navigation, showing card number, account ID, and status.

**FR-CARD-002: Card Detail Viewing**  
Priority: High  
System shall display complete card details including card number, CVV, embossed name, expiration date, active status, and associated account ID.

**FR-CARD-003: Card Information Update**  
Priority: High  
System shall allow authorized users to update card information (embossed name, expiration date, status) with validation (6 rules applied). Confirmation required before saving.

### 3.4 Transaction Management

**FR-TRAN-001: Transaction History Browsing**  
Priority: High  
System shall allow users to browse transaction history with pagination and filtering by account ID or card number, displaying date, amount, merchant, and type.

**FR-TRAN-002: Transaction Detail Viewing**  
Priority: High  
System shall display complete transaction details including ID, card number, type, category, source, description, amount, merchant information, and timestamps.

**FR-TRAN-003: Transaction Entry**  
Priority: High  
System shall allow authorized users to add new transactions with comprehensive validation (13 rules). Either account ID or card number must be provided; all transaction fields validated before creation.

### 3.5 Bill Payment Processing

**FR-BILL-001: Online Bill Payment**  
Priority: High  
System shall enable customers to pay account balance online with immediate balance update. Payment amount validated (must be positive and numeric). Payment reduces account balance immediately using calculation: New Balance = Current Balance - Payment Amount.

**FR-BILL-002: Payment Transaction Recording**  
Priority: High  
System shall automatically create transaction record for all bill payments with type 'Bill Payment', linking to account and card.

### 3.6 User Administration

**FR-USER-001: User Listing**  
Priority: High  
System shall display paginated list of all system users for administrators, showing user ID, name, and user type.

**FR-USER-002: User Creation**  
Priority: High  
System shall allow administrators to create new user accounts with role assignment. User ID must be unique (8 characters), with validation rules (9 rules) applied.

**FR-USER-003: User Information Update**  
Priority: High  
System shall allow administrators to modify user information including password and role changes. Validation applied to all changes.

**FR-USER-004: User Deletion**  
Priority: High  
System shall allow administrators to remove user accounts with explicit confirmation before deletion.

### 3.7 Reporting & Analytics

**FR-REPORT-001: Transaction Report Generation**  
Priority: Medium  
System shall allow administrators to generate transaction reports for specified date ranges via batch job submission. Date range validated before submission.

---

## 4. Business Rules

### 4.1 Validation Rules Summary
The system enforces 56 comprehensive validation rules across all functional areas, organized as follows:

**Authentication Rules (2 rules)**
- User ID and password required for sign-on
- Fields must not be empty

**Transaction Processing Rules (11 rules)**
- Either Account ID or Card Number required
- Account ID and Card Number must be numeric when provided
- All transaction fields required (type, category, source, description, amount, dates, merchant)

**Account Update Rules (14 rules)**
- Account ID, balances, and limits must be numeric
- Dates must be valid CCYYMMDD format
- Month range 1-12, Year range 1950-2099
- State code 2 alphabetic characters
- Phone number US format
- SSN 9 digits
- FICO score 3 digits
- Names alphabetic only
- Status indicators Y/N

**Credit Card Rules (5 rules)**
- Card number 16 digits numeric
- Expiry date valid format
- Active status Y/N
- Embossed name max 50 characters
- CVV 3 digits numeric

**Bill Payment Rules (3 rules)**
- Payment amount required, numeric, positive (> 0)

**User Management Rules (9 rules)**
- User ID required, 8 characters, unique
- First name and last name required
- Password required, 8 characters
- User type required and valid

**Reporting Rules (4 rules)**
- Start and end dates required
- Dates must be valid format
- Start date must be before or equal to end date

### 4.2 Business Calculation Rules

**BR-CALC-001: Bill Payment Balance Calculation**  
**Rule Statement**: When customer makes bill payment, account balance is reduced by payment amount.  
**Formula**: New Account Balance = Current Account Balance - Payment Amount  
**Trigger**: Bill payment processing (COBIL00C program, line 234)  
**Business Impact**: Core financial transaction logic ensuring payments properly reflected in account balance, maintaining accurate financial records.

---

## 5. Data Requirements

### 5.1 Core Business Entities

**Account Entity** (Primary Key: 11-digit Account ID)
- Financial data: Current Balance, Credit Limit, Cash Credit Limit
- Lifecycle dates: Open Date, Expiry Date, Reissue Date
- Cycle tracking: Current Cycle Credit/Debit
- Relationships: Links to Customer via cross-reference

**Customer Entity** (Primary Key: 9-digit Customer ID)
- Personal information: First Name, Middle Name, Last Name
- Contact details: Address (3 lines), State, Country, ZIP, Phone Numbers
- Identification: SSN (9 digits), Government ID, Date of Birth
- Credit profile: FICO Score (3 digits), EFT Account ID

**Card Entity** (Primary Key: 16-character Card Number)
- Card details: CVV Code (3 digits), Embossed Name (50 char max)
- Status: Expiry Date, Active Status (Y/N)
- Relationships: Links to Account via Account ID

**Transaction Entity** (Primary Key: 16-character Transaction ID)
- Transaction data: Type Code, Category Code, Source, Description
- Financial: Amount (signed decimal with 2 decimal places)
- Merchant: Merchant ID (9 digits), Name, City, ZIP
- Timestamps: Original Timestamp, Processing Timestamp
- Relationships: Links to Card via Card Number

**User Entity** (Primary Key: 8-character User ID)
- User information: First Name, Last Name
- Security: Password (8 characters, encrypted)
- Authorization: User Type (Admin/Regular)

**Cross-Reference Entity** (Composite Key: Account ID + Card Number)
- Relationships: Account ID, Card Number, Customer ID
- Purpose: Enables navigation between accounts, customers, and cards

### 5.2 Entity Relationships
- Customer to Account: 1:N (one customer, multiple accounts)
- Account to Card: 1:N (one account, multiple cards)
- Card to Transaction: 1:N (one card, multiple transactions)
- Customer to Card: N:M via Cross-Reference entity
- Account to Transaction: 1:N indirect via Card

### 5.3 Data Quality Requirements
- **Completeness**: All mandatory fields populated before record creation
- **Accuracy**: Numeric and date fields validated for proper format
- **Consistency**: Balance calculations reconcile to transaction history
- **Integrity**: Referential integrity maintained via validation

---

## 6. Process Requirements

### 6.1 User Authentication Process
**Purpose**: Securely authenticate users and route to appropriate dashboards  
**Trigger**: User accesses system sign-on screen  
**Steps**:
1. Display sign-on screen with user ID and password fields
2. User enters credentials and presses ENTER
3. System validates credentials against user security database
4. System routes based on user type (Admin → COADM01C, Regular → COMEN01C)
5. System establishes session context

**Success Criteria**: User authenticated and viewing appropriate dashboard  
**Alternative Paths**: Invalid credentials display error (3 attempts max), account lockout after failures

### 6.2 Account Update Process
**Purpose**: Maintain accurate account information with data integrity  
**Trigger**: User selects account update function  
**Steps**:
1. User enters account ID
2. System retrieves and displays current account data
3. User modifies desired fields
4. System validates all changes (23 validation rules)
5. System displays confirmation screen
6. User confirms changes
7. System updates account record and displays success message

**Success Criteria**: Account successfully updated with all validations passed  
**Performance**: Update completes within 3 seconds

### 6.3 Bill Payment Process
**Purpose**: Enable online bill payments with immediate balance updates  
**Trigger**: User selects bill payment from menu  
**Steps**:
1. User enters account ID
2. System displays current account balance
3. User enters payment amount
4. System validates payment (3 rules: not empty, numeric, positive)
5. System calculates new balance (Current Balance - Payment Amount)
6. System displays confirmation with new balance
7. User confirms payment
8. System updates account balance and creates transaction record atomically
9. System displays success confirmation

**Success Criteria**: Payment processed, balance updated, transaction recorded  
**Critical Business Rule**: Balance calculation and transaction creation must be atomic

### 6.4 Transaction Entry Process
**Purpose**: Allow manual transaction entry for corrections and adjustments  
**Trigger**: Administrator selects add transaction  
**Steps**:
1. User enters account ID OR card number (at least one required)
2. System validates identifiers
3. User enters all transaction details
4. System validates all fields (13 validation rules)
5. System displays confirmation
6. User confirms
7. System creates transaction record and updates account balance if applicable

**Success Criteria**: Transaction created with all required information

### 6.5 User Administration Process
**Purpose**: Provide controlled mechanism for granting system access  
**Trigger**: Administrator selects add user  
**Steps**:
1. Administrator enters user information (ID, names, password, type)
2. System validates data (9 validation rules including uniqueness check)
3. System displays confirmation
4. Administrator confirms
5. System creates user record with encrypted password

**Success Criteria**: New user account created and accessible for authentication

---

## 7. User Requirements

### 7.1 User Roles

**Regular User (Customer)**
- **Functions**: View/update accounts, manage cards, browse transactions, make payments
- **Goals**: Monitor account status, make timely payments, verify transactions
- **Access**: Main user menu (COMEN01C) with customer-facing functions

**Bank Administrator**
- **Functions**: All regular user functions PLUS user management, report generation, manual transaction entry
- **Goals**: Maintain system security, support operations, generate analytics
- **Access**: Administrative menu (COADM01C) with full system access

### 7.2 Key User Interfaces

**Sign-On Screen** (All users)
- User ID and password input (8 characters each)
- Error messages for invalid credentials
- PF3 to exit

**Main Menu** (Regular users)
- Menu options for account, card, transaction, payment functions
- PF3 to exit

**Administrative Menu** (Administrators)
- All regular menu options plus user management and reporting
- PF3 to exit

**Account Screens** (All users)
- View: Display complete account details
- Update: Modify account information with validation

**Card Screens** (All users)
- List: Paginated card portfolio (PF7/PF8 for navigation)
- Detail: Complete card information
- Update: Modify card details with validation

**Transaction Screens** (All users for browsing, Admin for entry)
- List: Paginated transaction history (PF7/PF8 navigation)
- Detail: Complete transaction information
- Entry: Manual transaction addition (Admin only)

**Bill Payment Screen** (All users)
- Account ID input, current balance display
- Payment amount entry with validation
- Confirmation with new balance

**User Management Screens** (Administrators only)
- List: Paginated user directory
- Create: New user account creation
- Update: Modify user information
- Delete: Remove user accounts

**Report Screen** (Administrators only)
- Date range selection for transaction reports
- Batch job submission confirmation

### 7.3 Navigation Standards
- **PF3**: Return to previous screen (universal back)
- **PF7/PF8**: Previous/Next page (pagination)
- **PF4**: Clear screen
- **PF5**: Save changes
- **PF12**: Return to main menu
- **ENTER**: Submit/validate current action

---

## 8. Integration Requirements

### 8.1 System Architecture
**Platform**: CICS mainframe transaction processing  
**Interface**: 3270 terminal character-based interface  
**Data Storage**: VSAM (Virtual Storage Access Method) files  
**Communication**: Program-to-program transfers via CICS XCTL with communication area (COMMAREA)

### 8.2 Data Files
- **ACCTDAT**: Account master file (KSDS)
- **CUSTDAT**: Customer master file (KSDS)
- **CARDDAT**: Credit card master file (KSDS)
- **TRANSACT**: Transaction file (KSDS)
- **USRSEC**: User security file (KSDS)
- **CXACAIX**: Cross-reference index (KSDS/AIX)

### 8.3 Access Patterns
- **Direct Read**: READ with unique key (account ID, card number, user ID)
- **Browse**: STARTBR/READNEXT/READPREV/ENDBR for pagination
- **Update**: READ UPDATE followed by REWRITE
- **Insert**: WRITE new record
- **Delete**: DELETE (primarily for user records)

### 8.4 Batch Processing
**Transaction Reporting**: Administrators submit report requests to transient data queue (JOBS-TDQ). Batch job processes transaction data for specified date range offline to prevent impact on online response times.

### 8.5 Integration Limitations
- No external system integrations implemented
- No APIs or web services exposed or consumed
- No real-time external data access
- Isolated system with batch-only reporting output

---

## 9. Non-Functional Requirements

### 9.1 Security

**Authentication**
- User ID (8 characters) and Password (8 characters) required
- Credentials validated against user security file
- Passwords must be encrypted/hashed using strong algorithms
- Account lockout after 3 failed login attempts

**Authorization**
- Role-Based Access Control (RBAC): Admin vs Regular User
- Administrative functions check user type before allowing access
- Menu system presents only authorized options

**Data Protection**
- Sensitive data: PII (names, SSN, address), financial data (balances, transactions), card data (card numbers, CVV)
- Encryption required: at rest (VSAM files), in transit (terminal connections), passwords (hashed)
- Data masking: passwords during entry, consider masking card numbers and SSN

**Audit Trail**
- Log: authentication attempts, account/card updates, transactions, payments, user administration
- Record: who, what, when, where, why, result
- Retention: 7 years minimum per banking regulations
- Access: read-only for authorized security/compliance personnel

**Transaction Integrity**
- All fields validated before transaction creation
- Unique transaction IDs prevent duplicates
- Atomic updates (balance and transaction created together)
- Transactions immutable once created

### 9.2 Performance
- **Authentication**: < 2 seconds
- **Viewing operations**: < 2 seconds
- **Updates/Payments**: < 3 seconds
- **List/Browse**: < 2 seconds per page
- **Throughput**: 1000+ transactions per hour
- **Concurrent users**: 50-100 supported

### 9.3 Availability
- **Uptime**: 99.9% during business hours (8 AM - 8 PM)
- **24/7 access**: Support for customer self-service
- **Planned maintenance**: During low-usage windows
- **Monthly downtime**: < 4 hours for maintenance
- **RTO**: < 4 hours for system restoration
- **RPO**: < 15 minutes data loss acceptable

### 9.4 Compliance
- **Banking Regulations**: Federal and state banking laws compliance
- **Record Retention**: 7-year retention for financial records
- **GLBA**: Gramm-Leach-Bliley Act for customer financial information protection
- **PCI DSS**: Payment Card Industry Data Security Standard compliance
- **Audit Requirements**: Support external audits with complete audit trails

### 9.5 Usability
- **Learning curve**: New users productive within 1 hour of training
- **Error recovery**: Easy recovery with clear error messages
- **Navigation**: Intuitive with consistent function key patterns
- **Accessibility**: Full keyboard navigation, screen reader support

---

## 10. Assumptions & Constraints

### 10.1 Business Assumptions
- Customer base: Thousands to tens of thousands of accounts
- Transaction volume within current capacity limits
- Primary usage during banking hours, 24/7 self-service available
- Users receive basic training on system navigation
- Banking regulations remain relatively stable

### 10.2 Technical Constraints
- COBOL language limits modern programming paradigms
- CICS pseudo-conversational design pattern required
- 3270 character-based terminal interface only
- VSAM file structure and capabilities limitations
- Mainframe infrastructure dependency
- No web, mobile, or API access in current architecture

### 10.3 Integration Constraints
- Isolated system with limited external integration
- Batch-only reporting, no real-time external data
- No event streaming or modern integration patterns
- Point-to-point only, no ESB or message broker

### 10.4 Resource Constraints
- Limited COBOL programmer availability
- Declining mainframe expertise pool
- High mainframe operational costs (MIPS, storage, licensing)
- Limited batch processing windows

### 10.5 Regulatory Constraints
- Must comply with federal/state banking regulations
- Mandatory 7-year data retention for financial records
- Must support regulatory audits
- Privacy law compliance (GLBA)
- PCI DSS requirements for card data

### 10.6 Scope Limitations (Out of Scope)
- Interest calculations and fee computations
- Credit scoring and fraud detection
- External payment gateway integrations
- Customer onboarding and KYC
- Dispute management and chargebacks
- Rewards and loyalty programs
- Web/mobile interfaces
- RESTful APIs
- Real-time analytics

---

## 11. Business Glossary

**Account**: Credit card account with unique 11-digit ID, containing balance, credit limits, and transaction history

**Account Balance**: Current outstanding amount owed, calculated as charges minus payments

**Active Status**: Y/N indicator showing if card is currently active for transactions

**Administrator**: Bank staff with elevated privileges including user management and reporting

**Authentication**: Verifying user identity through user ID and password

**Authorization**: Determining user's permitted access based on role

**Bill Payment**: Online payment reducing account balance immediately

**Card Number**: Unique 16-digit identifier for physical credit card

**Credit Limit**: Maximum amount authorized to charge on account

**Cross-Reference**: Data structure linking accounts, customers, and cards

**Customer**: Individual with one or more credit card accounts, identified by 9-digit ID

**CVV Code**: 3-digit Card Verification Value security code

**Embossed Name**: Name physically embossed on credit card (max 50 characters)

**FICO Score**: 3-digit numerical creditworthiness representation (300-850)

**Merchant**: Business accepting credit cards, identified by ID, name, location

**Pagination**: Dividing large result sets into navigable pages

**Regular User**: Customer with standard access to view and manage own accounts

**Role-Based Access Control (RBAC)**: Security model where access determined by user role

**Session**: Period of continuous system use from login to logout

**SSN**: 9-digit Social Security Number for customer verification

**Transaction**: Record of card purchase, payment, or account activity with amount, merchant, timestamps

**User**: Individual with system access credentials and assigned role

**User Type**: Classification as Administrator or Regular User

**Validation**: Checking user input against business rules for data quality

**VSAM**: Virtual Storage Access Method, mainframe file storage system

---

## 12. Traceability Matrix

### 12.1 Requirements to Source Programs

| Requirement | Source Program | Location | Validation Rules |
|-------------|----------------|----------|-----------------|
| User Authentication | COSGN00C | Lines 118-126 | 2 rules |
| Account Viewing | COACTVWC | Complete program | N/A |
| Account Update | COACTUPC | Complete program | 23 rules |
| Card Listing | COCRDLIC | Complete program | N/A |
| Card Viewing | COCRDSLC | Complete program | N/A |
| Card Update | COCRDUPC | Complete program | 6 rules |
| Transaction Browse | COTRN00C | Complete program | N/A |
| Transaction View | COTRN01C | Complete program | N/A |
| Transaction Entry | COTRN02C | Complete program | 13 rules |
| Bill Payment | COBIL00C | Line 234 (calculation) | 3 rules |
| User Listing | COUSR00C | Complete program | N/A |
| User Creation | COUSR01C | Complete program | 9 rules |
| User Update | COUSR02C | Complete program | N/A |
| User Deletion | COUSR03C | Complete program | N/A |
| Report Generation | CORPT00C | Complete program | 4 rules |

### 12.2 Business Entities to Data Files

| Entity | VSAM File | Programs Accessing | Key Field |
|--------|-----------|-------------------|-----------|
| Account | ACCTDAT | COACTVWC, COACTUPC, COBIL00C | Account ID (11 digits) |
| Customer | CUSTDAT | Multiple programs | Customer ID (9 digits) |
| Card | CARDDAT | COCRDLIC, COCRDSLC, COCRDUPC | Card Number (16 chars) |
| Transaction | TRANSACT | COTRN00C, COTRN01C, COTRN02C, COBIL00C | Transaction ID (16 chars) |
| User | USRSEC | COUSR* programs, COSGN00C | User ID (8 chars) |
| Cross-Reference | CXACAIX | Multiple programs | Account ID + Card Number |

### 12.3 Validation Rules Coverage

**56 Total Validation Rules** distributed across:
- Authentication: 2 rules (COSGN00C)
- Transactions: 13 rules (COTRN02C)
- Accounts: 23 rules (COACTUPC)
- Cards: 6 rules (COCRDUPC)
- Bill Payment: 3 rules (COBIL00C)
- User Management: 9 rules (COUSR01C, COUSR02C)
- Reporting: 4 rules (CORPT00C)

**1 Business Calculation Rule**:
- Bill Payment Balance Calculation (COBIL00C, line 234)

### 12.4 User Roles to System Access

| Role | Menu Program | Transaction ID | Accessible Functions |
|------|--------------|----------------|---------------------|
| Regular User | COMEN01C | OMEN | Account view/update, card management, transaction browse, bill payment |
| Administrator | COADM01C | CADM | All Regular User functions + user management, transaction entry, reporting |

---

## 13. Open Questions & Risks

### 13.1 Critical Questions for Stakeholders

**Q1: Interest & Fee Calculations**  
Current system does not calculate interest or fees. Should modernized system include interest accrual, late fees, annual fees, over-limit fees? What are the business rules?

**Q2: Payment Application Logic**  
When payment is less than full balance, how should it be applied (to interest first, oldest charges first, etc.)?

**Q3: Card Replacement Process**  
What is complete workflow for card renewal, replacement due to loss/theft, or damage?

**Q4: Account Opening/Closure**  
What are complete processes for account opening and closure? What information collected? What approvals needed?

**Q5: Batch Report Details**  
What reports are generated? What is format and distribution method?

**Q6: Password Policies**  
What should password complexity and expiration policies be?

**Q7: Multi-Factor Authentication**  
Should MFA be implemented? For which user types?

**Q8: Session Timeout**  
What should timeout period be? What happens to in-progress transactions?

**Q9: Data Archival Strategy**  
What are complete retention and archival requirements beyond 7-year minimum?

**Q10: Multi-Currency Support**  
Is multi-currency support needed for international transactions?

### 13.2 Identified Risks

**RISK-001: COBOL/Mainframe Dependency** (HIGH)  
System tightly coupled to COBOL and mainframe, making modernization complex and costly. Limited COBOL expertise available.  
*Mitigation*: Plan phased modernization; maintain documentation; retain COBOL skills.

**RISK-002: Limited Scalability** (MEDIUM)  
VSAM architecture may not scale to very large data volumes efficiently.  
*Mitigation*: Monitor performance; plan database migration; implement archival.

**RISK-003: No API Layer** (MEDIUM)  
Lack of APIs prevents integration with modern systems and mobile channels.  
*Mitigation*: Prioritize API development; implement API gateway; gradual service extraction.

**RISK-004: Weak Authentication** (HIGH)  
Simple user ID/password without MFA or strong password policies creates security risk.  
*Mitigation*: Implement MFA; enforce strong passwords; monitor authentication attempts.

**RISK-005: Incomplete Business Logic** (MEDIUM)  
System lacks key banking functions (interest, fees, disputes) that may be needed.  
*Mitigation*: Document missing capabilities; prioritize development; maintain compensating controls.

**RISK-006: Data Migration Complexity** (HIGH)  
Migrating from VSAM to modern database will be complex with large data volumes and relationships.  
*Mitigation*: Plan comprehensive migration strategy; extensive testing; phased approach.

**RISK-007: Knowledge Loss** (HIGH)  
Experienced COBOL/mainframe staff retiring creates knowledge transfer risk.  
*Mitigation*: Document thoroughly; cross-train staff; capture institutional knowledge.

**RISK-008: Single Point of Failure** (HIGH)  
Single data center operation creates availability risk.  
*Mitigation*: Implement disaster recovery; consider multi-site architecture.

**RISK-009: Data Quality** (MEDIUM)  
While validation rules exist, data quality depends on consistent enforcement.  
*Mitigation*: Enforce validation; implement monitoring; periodic audits.

**RISK-010: Limited Reporting** (LOW)  
Basic reporting may not meet business intelligence needs.  
*Mitigation*: Enhance reporting; consider BI tool integration.

### 13.3 Areas Requiring Further Investigation

- Complete card lifecycle management processes
- External system integration requirements
- Statement generation and distribution
- Collections and past-due account handling
- Customer notification requirements (email/SMS)
- Mobile banking requirements
- Real-time fraud detection needs
- Regulatory reporting requirements details
- Disaster recovery and business continuity procedures
- Performance benchmarks for modernized system

---

## Document Control

**Version History**:
- Version 1.0 (October 27, 2025): Initial comprehensive BRD

**Related Documents**:
- 01.phase-1-output/business_entities.md - Detailed entity analysis
- 01.phase-1-output/validation-rules.md - Complete validation rule catalog
- 01.phase-1-output/user-stories.md - User story collection
- 01.phase-1-output/business-calculation-rules.md - Calculation rule details
- 01.phase-1-output/cardemo-screen-flow-analysis.md - Screen flow documentation
- 01.phase-1-output/analysis-summary.md - Analysis overview

**Source Code References**:
- 00.phase-1-input/ - Original COBOL source programs (18 files)

**Approval & Sign-off**:
- Business Analyst: _________________ Date: _______
- IT Manager: _________________ Date: _______
- Compliance Officer: _________________ Date: _______

---

*This Business Requirements Document represents the comprehensive business requirements extracted from the CardDemo COBOL legacy system. It is intended to serve as the authoritative reference for system understanding, modernization planning, testing, and business decision-making.*
