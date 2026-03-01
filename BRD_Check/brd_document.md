# High-Level Requirements Document: Open Bank Project API

## 1. System Overview & Purpose

### System Identification
- **System Name**: Open Bank Project API (OBP-API)
- **Application Code/ID**: OBP-API
- **Business Domain**: Financial Services / Banking API Platform

### Business Purpose
The Open Bank Project API is an open-source RESTful API platform that abstracts core banking system complexities to enable financial applications to interact with multiple banks on behalf of account holders. The system serves as a standardized interface layer between third-party financial applications and banking backends, allowing developers to build once and connect to multiple banks without learning each bank's proprietary API. The platform supports regulatory compliance requirements including PSD2 (Payment Services Directive 2) for European markets and Open Banking standards for UK markets.

### System Criticality
- **Criticality Level**: High
- **Business Impact if Unavailable**: Third-party financial applications would lose access to banking data and payment initiation capabilities. Banks would be unable to meet regulatory compliance requirements for open banking. Financial service providers would be unable to offer account aggregation, payment initiation, and other value-added services to their customers.

### System Type
- **Architecture**: REST API with Web Application components
- **Processing Model**: Mixed (HTTP request-response for API operations, scheduled tasks for background processing, event-driven for webhooks and notifications)

### Key Stakeholders
- Third-party financial application developers (fintech companies, personal finance managers, accounting software providers)
- Banks and financial institutions seeking to expose their services via standardized APIs
- Payment Service Providers (PSPs) requiring PSD2-compliant APIs
- Regulators and auditors requiring transparent access to financial data
- End customers accessing their financial data through third-party applications

---

## 2. Core Capabilities Inventory

### Category: Bank Information Management

| # | Capability Name | Description | Frequency | Volume |
|---|----------------|-------------|-----------|--------|
| 1 | Bank Listing | Retrieve a list of all banks available on the API instance with basic information | Real-time | High |
| 2 | Bank Details Retrieval | Get detailed information about a specific bank including name, logo, and website | Real-time | High |
| 3 | Bank Creation | Create a new bank entity with associated attributes and configuration | On-demand | Low |
| 4 | Bank Attribute Management | Manage custom attributes associated with banks for extended metadata | On-demand | Low |

### Category: Account Management

| # | Capability Name | Description | Frequency | Volume |
|---|----------------|-------------|-----------|--------|
| 1 | Account Listing | Retrieve all accounts accessible to a user at a specific bank | Real-time | High |
| 2 | Account Details Retrieval | Get comprehensive information about a specific account including balance and metadata | Real-time | High |
| 3 | Account Creation | Create new bank accounts with specified attributes and initial configuration | On-demand | Medium |
| 4 | Account Balance Inquiry | Retrieve current balance information for one or multiple accounts | Real-time | Very High |
| 5 | Account Label Update | Modify the display label for an account | On-demand | Low |
| 6 | Settlement Account Management | Create and manage settlement accounts for payment processing | On-demand | Low |
| 7 | Account Attribute Management | Create, update, and retrieve custom attributes for accounts | On-demand | Medium |
| 8 | Account Routing Management | Manage account routing information including IBAN and other identifiers | On-demand | Medium |
| 9 | Firehose Account Access | Bulk access to account data for authorized users with firehose permissions | Real-time | High |
| 10 | IBAN Validation | Validate and check IBAN numbers for correctness | Real-time | Medium |

### Category: Transaction Management

| # | Capability Name | Description | Frequency | Volume |
|---|----------------|-------------|-----------|--------|
| 1 | Transaction Listing | Retrieve transaction history for an account with filtering and pagination | Real-time | Very High |
| 2 | Transaction Details Retrieval | Get detailed information about a specific transaction | Real-time | High |
| 3 | Double-Entry Transaction View | View the double-entry bookkeeping representation of transactions | Real-time | Medium |
| 4 | Balancing Transaction Retrieval | Get the corresponding balancing transaction for a given transaction | Real-time | Medium |
| 5 | Transaction Tagging | Add, view, and remove tags on transactions for categorization | On-demand | Medium |

### Category: Payment Initiation

| # | Capability Name | Description | Frequency | Volume |
|---|----------------|-------------|-----------|--------|
| 1 | Account-to-Account Transfer | Initiate payments between accounts using bank and account identifiers | Real-time | High |
| 2 | SEPA Credit Transfer | Initiate SEPA payments using IBAN-based routing | Real-time | High |
| 3 | Counterparty Payment | Initiate payments to pre-registered counterparties | Real-time | High |
| 4 | Simple Transfer | Initiate payments using bank account number or IBAN directly | Real-time | Medium |
| 5 | Refund Processing | Process refund requests for previous transactions | On-demand | Medium |
| 6 | Free-Form Payment | Initiate payments with flexible parameters for special use cases | On-demand | Low |
| 7 | Card Payment | Initiate card-based payment transactions | Real-time | Medium |
| 8 | Agent Cash Withdrawal | Process cash withdrawal requests through agents | On-demand | Low |
| 9 | Transaction Request Challenge | Handle strong customer authentication challenges for payment authorization | Real-time | High |
| 10 | Transaction Request Status | Track and retrieve the status of payment requests | Real-time | High |
| 11 | Transaction Request Cancellation | Cancel pending payment requests | On-demand | Low |

### Category: Customer Management

| # | Capability Name | Description | Frequency | Volume |
|---|----------------|-------------|-----------|--------|
| 1 | Customer Creation | Create new customer records with personal and contact information | On-demand | Medium |
| 2 | Customer Details Retrieval | Get customer information by customer ID or customer number | Real-time | High |
| 3 | Customer Search | Search for customers by phone number, legal name, or other criteria | Real-time | Medium |
| 4 | Customer Update | Update customer information including email, mobile number, and identity details | On-demand | Medium |
| 5 | Customer Attribute Management | Manage custom attributes for customers | On-demand | Medium |
| 6 | Customer Address Management | Create, update, and retrieve customer addresses | On-demand | Medium |
| 7 | Tax Residence Management | Manage customer tax residence information for regulatory compliance | On-demand | Low |
| 8 | Customer-User Linking | Link customer records to user accounts for access management | On-demand | Medium |
| 9 | Firehose Customer Access | Bulk access to customer data for authorized users | Real-time | Medium |

### Category: User Management

| # | Capability Name | Description | Frequency | Volume |
|---|----------------|-------------|-----------|--------|
| 1 | User Authentication | Authenticate users via multiple methods including OAuth, Direct Login, and OpenID Connect | Real-time | Very High |
| 2 | User Creation | Create new user accounts with specified roles and permissions | On-demand | Medium |
| 3 | User Details Retrieval | Get user information by user ID, username, or email | Real-time | High |
| 4 | User Listing | Retrieve lists of users with filtering capabilities | Real-time | Medium |
| 5 | User Invitation | Send invitations to new users to join the platform | On-demand | Low |
| 6 | User Lock/Unlock | Lock or unlock user accounts for security purposes | On-demand | Low |
| 7 | User Deletion | Remove user accounts from the system | On-demand | Low |
| 8 | Bad Login Status | Track and retrieve information about failed login attempts | Real-time | Medium |
| 9 | User Synchronization | Synchronize user information with external identity providers | On-demand | Low |

### Category: Authorization & Access Control

| # | Capability Name | Description | Frequency | Volume |
|---|----------------|-------------|-----------|--------|
| 1 | Entitlement Management | Grant, revoke, and query role-based permissions for users | On-demand | Medium |
| 2 | View Management | Create, update, and delete views that control data visibility | On-demand | Medium |
| 3 | Account Access Grants | Grant and revoke user access to specific accounts through views | On-demand | Medium |
| 4 | System View Management | Manage predefined system-level views with standard permission sets | On-demand | Low |
| 5 | Custom View Management | Create and manage user-defined views for specific access patterns | On-demand | Low |
| 6 | Scope Management | Manage OAuth scopes for API access control | On-demand | Low |

### Category: Consent Management

| # | Capability Name | Description | Frequency | Volume |
|---|----------------|-------------|-----------|--------|
| 1 | Consent Creation | Create consent records for third-party access to account data | Real-time | High |
| 2 | Consent Status Management | Update and track consent status through its lifecycle | Real-time | High |
| 3 | Consent Retrieval | Get consent information by consent ID or for current user | Real-time | High |
| 4 | Consent Revocation | Revoke previously granted consents | On-demand | Medium |
| 5 | Consent Challenge Authorization | Handle strong customer authentication for consent authorization | Real-time | High |
| 6 | Consent Account Access Update | Modify the account access permissions within a consent | On-demand | Medium |

### Category: PSD2 Berlin Group Compliance

| # | Capability Name | Description | Frequency | Volume |
|---|----------------|-------------|-----------|--------|
| 1 | AIS Account List | Retrieve account list for Account Information Service | Real-time | High |
| 2 | AIS Balance Retrieval | Get account balances for Account Information Service | Real-time | High |
| 3 | AIS Transaction List | Retrieve transaction history for Account Information Service | Real-time | High |
| 4 | AIS Consent Management | Create and manage consents for account information access | Real-time | High |
| 5 | AIS Card Account Access | Access card account information including balances and transactions | Real-time | Medium |
| 6 | PIS Payment Initiation | Initiate payments through Payment Initiation Service | Real-time | High |
| 7 | PIS Periodic Payments | Initiate recurring periodic payments | On-demand | Medium |
| 8 | PIS Bulk Payments | Initiate bulk payment batches | On-demand | Low |
| 9 | PIS Payment Status | Retrieve payment initiation status | Real-time | High |
| 10 | PIS Payment Cancellation | Cancel pending payment initiations | On-demand | Medium |
| 11 | PIS Authorization Management | Manage payment authorization flows and SCA status | Real-time | High |
| 12 | PIIS Funds Confirmation | Confirm availability of funds for a specified amount | Real-time | Medium |
| 13 | Signing Basket Management | Group multiple transactions for single authorization | On-demand | Low |

### Category: Card Management

| # | Capability Name | Description | Frequency | Volume |
|---|----------------|-------------|-----------|--------|
| 1 | Credit Card Order Status | Retrieve status of credit card orders | Real-time | Medium |
| 2 | Checkbook Order Status | Retrieve status of checkbook orders | Real-time | Low |
| 3 | Physical Card Creation | Create new physical card records | On-demand | Low |
| 4 | Physical Card Update | Update physical card information | On-demand | Low |
| 5 | Card Listing | Retrieve cards associated with a bank or account | Real-time | Medium |
| 6 | Card Attribute Management | Manage custom attributes for cards | On-demand | Low |

### Category: Product & Catalog Management

| # | Capability Name | Description | Frequency | Volume |
|---|----------------|-------------|-----------|--------|
| 1 | Product Creation | Create new banking product definitions | On-demand | Low |
| 2 | Product Retrieval | Get product information and product trees | Real-time | Medium |
| 3 | Product Listing | Retrieve all products offered by a bank | Real-time | Medium |
| 4 | Product Attribute Management | Manage custom attributes for products | On-demand | Low |
| 5 | Product Collection Management | Group products into collections for catalog organization | On-demand | Low |
| 6 | Product Fee Management | Define and manage fees associated with products | On-demand | Low |

### Category: Branch & ATM Management

| # | Capability Name | Description | Frequency | Volume |
|---|----------------|-------------|-----------|--------|
| 1 | Branch Creation | Create new branch location records | On-demand | Low |
| 2 | Branch Retrieval | Get branch information and listings | Real-time | Medium |
| 3 | Branch Deletion | Remove branch records from the system | On-demand | Low |
| 4 | ATM Creation | Create new ATM location records | On-demand | Low |
| 5 | ATM Retrieval | Get ATM information and listings | Real-time | Medium |
| 6 | ATM Update | Update ATM information including services and accessibility features | On-demand | Low |
| 7 | ATM Deletion | Remove ATM records from the system | On-demand | Low |
| 8 | ATM Attribute Management | Manage custom attributes for ATMs | On-demand | Low |

### Category: Counterparty Management

| # | Capability Name | Description | Frequency | Volume |
|---|----------------|-------------|-----------|--------|
| 1 | Counterparty Creation | Create new counterparty records for payment recipients | On-demand | Medium |
| 2 | Counterparty Retrieval | Get counterparty information by ID or routing details | Real-time | High |
| 3 | Counterparty Listing | Retrieve all counterparties for an account | Real-time | Medium |
| 4 | Counterparty Limit Management | Set and manage transaction limits for counterparties | On-demand | Low |

### Category: Consumer & Application Management

| # | Capability Name | Description | Frequency | Volume |
|---|----------------|-------------|-----------|--------|
| 1 | Consumer Registration | Register new API consumer applications | On-demand | Low |
| 2 | Consumer Retrieval | Get consumer application details | Real-time | Medium |
| 3 | Consumer Listing | Retrieve all registered consumers | Real-time | Low |
| 4 | Consumer Update | Update consumer application settings including redirect URLs and certificates | On-demand | Low |
| 5 | Rate Limit Management | Set and manage API call rate limits for consumers | On-demand | Low |

### Category: Metrics & Monitoring

| # | Capability Name | Description | Frequency | Volume |
|---|----------------|-------------|-----------|--------|
| 1 | API Metrics Collection | Collect and store metrics about API usage | Real-time | Very High |
| 2 | Top APIs Report | Generate reports on most frequently used API endpoints | On-demand | Low |
| 3 | Top Consumers Report | Generate reports on highest-volume API consumers | On-demand | Low |
| 4 | Aggregate Metrics | Retrieve aggregated metrics with filtering capabilities | On-demand | Medium |
| 5 | Connector Metrics | Monitor performance of backend connector operations | Real-time | Medium |

### Category: Webhook & Notification Management

| # | Capability Name | Description | Frequency | Volume |
|---|----------------|-------------|-----------|--------|
| 1 | Account Webhook Creation | Create webhooks to receive notifications about account events | On-demand | Low |
| 2 | Account Webhook Management | Enable, disable, and retrieve account webhooks | On-demand | Low |
| 3 | Bank Account Notification | Send notifications about bank account events to registered endpoints | Event-driven | High |
| 4 | System Account Notification | Send system-level account notifications | Event-driven | Medium |

### Category: Dynamic Configuration

| # | Capability Name | Description | Frequency | Volume |
|---|----------------|-------------|-----------|--------|
| 1 | Dynamic Entity Management | Create and manage runtime-defined data entities | On-demand | Low |
| 2 | Dynamic Endpoint Management | Create and manage runtime-defined API endpoints | On-demand | Low |
| 3 | Method Routing Configuration | Configure routing of connector methods to different backends | On-demand | Low |
| 4 | Web UI Properties Management | Manage configurable web interface properties | On-demand | Low |
| 5 | Connector Method Management | Define and manage custom connector methods | On-demand | Low |

### Category: Administrative Functions

| # | Capability Name | Description | Frequency | Volume |
|---|----------------|-------------|-----------|--------|
| 1 | Database Information | Retrieve information about the database configuration | On-demand | Low |
| 2 | Adapter Information | Get information about the connected backend adapter | On-demand | Low |
| 3 | System Configuration | Manage system-wide configuration settings | On-demand | Low |
| 4 | API Collection Management | Organize API endpoints into collections for documentation | On-demand | Low |
| 5 | Regulated Entity Management | Manage regulated entity registrations for compliance | On-demand | Low |
| 6 | FX Rate Management | Create and manage foreign exchange rates | On-demand | Low |

### Category: Background Processing

| # | Capability Name | Description | Frequency | Volume |
|---|----------------|-------------|-----------|--------|
| 1 | Consent Expiration Processing | Automatically expire consents that have passed their validity date | Scheduled (Periodic) | Medium |
| 2 | Outdated Consent Rejection | Reject consents that remain unfinished beyond the allowed time | Scheduled (Periodic) | Low |
| 3 | Metrics Archival | Archive old metrics data to maintain database performance | Scheduled (Daily) | High |
| 4 | Outdated Metrics Cleanup | Delete metrics data older than the retention period | Scheduled (Daily) | Medium |
| 5 | Token and Nonce Cleanup | Remove expired authentication tokens and nonces | Scheduled (Periodic) | Medium |
| 6 | Outdated Transaction Rejection | Reject transactions that remain unprocessed beyond the allowed time | Scheduled (Periodic) | Low |

### Category: Integration & Connectivity

| # | Capability Name | Description | Frequency | Volume |
|---|----------------|-------------|-----------|--------|
| 1 | REST Backend Connector | Connect to banking backends via REST APIs | Real-time | Very High |
| 2 | Akka Actor Connector | Connect to distributed banking systems via Akka actors | Real-time | High |
| 3 | Stored Procedure Connector | Connect to databases via stored procedures | Real-time | High |
| 4 | Message Queue Connector | Connect to banking systems via message queues (Kafka, RabbitMQ) | Real-time | High |
| 5 | Blockchain Connector | Connect to blockchain networks (Cardano, Ethereum) for crypto operations | Real-time | Low |
| 6 | Local Database Connector | Direct access to OBP's local database for sandbox operations | Real-time | High |

---

### Capability Summary
- **Total Capabilities Identified**: 127
- **API Endpoints**: Approximately 500+ across all API versions (v1.2.1 through v5.1.0)
- **Background Tasks**: 6
- **Event Consumers**: Webhook-based event processing
- **Batch Jobs**: 4 (Metrics archival, consent expiration, token cleanup, transaction rejection)
- **External Integrations**: 6 connector types (REST, Akka, Stored Procedure, Kafka, RabbitMQ, Blockchain)
- **Primary Business Functions**: Bank Management, Account Management, Transaction Management, Payment Initiation, Customer Management, User Management, Authorization & Access Control, Consent Management, PSD2 Compliance, Card Management, Product Management, Branch & ATM Management, Counterparty Management, Consumer Management, Metrics & Monitoring, Webhook Management, Dynamic Configuration, Administrative Functions, Background Processing, Integration & Connectivity

---

## Open Questions & Clarifications Needed

1. **Volume Estimates**: The volume classifications (High/Medium/Low) are inferred from typical banking API usage patterns. Actual production volumes should be validated with operational data.

2. **Connector Priority**: The codebase supports multiple connector types. Clarification is needed on which connectors are actively used in production versus those available for specific deployment scenarios.

3. **Deprecated Endpoints**: Some API versions contain deprecated endpoints. A review should be conducted to identify which capabilities are being phased out.

4. **Regional Compliance**: While Berlin Group PSD2 and UK Open Banking implementations are present, clarification is needed on which regional compliance modules are actively maintained and deployed.

5. **Blockchain Integration**: The Cardano and Ethereum connectors appear to be recent additions. Clarification is needed on the production readiness and intended use cases for these integrations.

6. **Scheduled Task Configuration**: The scheduled background tasks are configurable via properties. Default intervals and retention periods should be documented for production deployments.

7. **Webhook Reliability**: The webhook implementation uses HTTP callbacks. Clarification is needed on retry policies and delivery guarantees for critical notifications.
