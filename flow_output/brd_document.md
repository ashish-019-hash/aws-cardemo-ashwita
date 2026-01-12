# High-Level Requirements Document: Open Bank Project API

## 1. System Overview & Purpose

### System Identification
- **System Name**: Open Bank Project API (OBP-API)
- **Application Code/ID**: OBP-API
- **Business Domain**: Financial Services / Open Banking Platform

### Business Purpose
The Open Bank Project API is an open-source RESTful API platform that abstracts core banking system complexities to enable financial applications to interact with multiple banks on behalf of account holders. The platform serves as a standardized interface layer between third-party developers, banks, and payment service providers, enabling secure access to banking services while maintaining regulatory compliance. The system's mission is "Bank as a Platform, Transparency as an Asset," facilitating innovation in financial services through standardized API access.

### System Criticality
- **Criticality Level**: High
- **Business Impact if Unavailable**: Complete disruption of third-party financial application integrations, inability to process PSD2-compliant transactions, loss of account information access for authorized parties, and potential regulatory non-compliance for connected financial institutions.

### System Type
- **Architecture**: REST API Platform with Multi-Protocol Backend Integration
- **Processing Model**: Mixed (HTTP request-response, Scheduled tasks, Event-driven webhooks)

### Key Stakeholders
- Third-Party Developers (Fintech applications, Personal finance managers, Accounting software providers)
- Banks and Financial Institutions
- Payment Service Providers (PSPs)
- Regulators and Auditors
- Account Holders / End Users

---

## 2. Core Capabilities Inventory

### Category: Bank Management

| # | Capability Name | Description | Frequency | Volume |
|---|----------------|-------------|-----------|--------|
| 1 | Bank Information Retrieval | Retrieve information about banks supported on the platform including identifiers, names, logos, and websites | Real-time | High |
| 2 | Bank Creation | Create new bank entities on the platform with associated attributes and configurations | On-demand | Low |
| 3 | Bank Attribute Management | Manage custom attributes associated with bank entities for extended metadata | On-demand | Low |
| 4 | Settlement Account Management | Create and manage settlement accounts for payment processing and double-entry bookkeeping | On-demand | Medium |

### Category: Account Management

| # | Capability Name | Description | Frequency | Volume |
|---|----------------|-------------|-----------|--------|
| 5 | Account Listing | Retrieve list of accounts accessible to users at specific banks with filtering options | Real-time | High |
| 6 | Account Details Retrieval | Get detailed information about specific accounts including balances and metadata | Real-time | High |
| 7 | Account Creation | Create new bank accounts with specified attributes, currencies, and ownership | On-demand | Medium |
| 8 | Account Balance Retrieval | Retrieve current and available balances for bank accounts | Real-time | Very High |
| 9 | Account Label Management | Update and manage account labels and display names | On-demand | Low |
| 10 | Account Attribute Management | Create, update, and retrieve custom attributes for accounts | On-demand | Medium |
| 11 | Account Routing Management | Manage account routing information including IBAN validation and lookup | Real-time | High |
| 12 | Firehose Account Access | Bulk access to all accounts at a bank for authorized administrative users | On-demand | Low |

### Category: Transaction Management

| # | Capability Name | Description | Frequency | Volume |
|---|----------------|-------------|-----------|--------|
| 13 | Transaction Listing | Retrieve transaction history for accounts with filtering and pagination | Real-time | Very High |
| 14 | Transaction Details Retrieval | Get detailed information about specific transactions | Real-time | High |
| 15 | Double-Entry Transaction Retrieval | Access double-entry bookkeeping records for transactions showing debit and credit sides | Real-time | Medium |
| 16 | Balancing Transaction Retrieval | Retrieve the corresponding balancing transaction for double-entry records | Real-time | Medium |
| 17 | Transaction Metadata Management | Add, update, and retrieve metadata tags and comments on transactions | On-demand | Medium |
| 18 | Transaction Firehose Access | Bulk access to all transactions for authorized administrative users | On-demand | Low |

### Category: Payment Initiation

| # | Capability Name | Description | Frequency | Volume |
|---|----------------|-------------|-----------|--------|
| 19 | SEPA Credit Transfer Initiation | Initiate SEPA credit transfers using IBAN-based routing | Real-time | High |
| 20 | Account-to-Account Transfer | Initiate transfers between accounts within the platform | Real-time | High |
| 21 | Counterparty Payment | Initiate payments to registered counterparties with pre-configured routing | Real-time | High |
| 22 | Simple Payment Transfer | Initiate payments using bank account numbers or IBANs directly | Real-time | Medium |
| 23 | Refund Processing | Process refunds for previous transactions with proper authorization | On-demand | Medium |
| 24 | Free-Form Payment | Initiate payments with flexible parameters for authorized users | On-demand | Low |
| 25 | Agent Cash Withdrawal | Process cash withdrawal requests through authorized agents | On-demand | Medium |
| 26 | Card Payment Processing | Process card-based payment transactions | Real-time | High |
| 27 | Transaction Request Challenge | Handle Strong Customer Authentication challenges for payment authorization | Real-time | High |
| 28 | Payment Cancellation | Cancel pending or scheduled payments with proper authorization | On-demand | Medium |

### Category: Standing Orders & Direct Debits

| # | Capability Name | Description | Frequency | Volume |
|---|----------------|-------------|-----------|--------|
| 29 | Standing Order Creation | Create recurring payment instructions for automatic execution | On-demand | Medium |
| 30 | Standing Order Management | Update, view, and cancel standing order instructions | On-demand | Medium |
| 31 | Direct Debit Creation | Set up direct debit mandates for automatic collection | On-demand | Medium |
| 32 | Direct Debit Management | Manage direct debit mandates including updates and cancellations | On-demand | Medium |

### Category: Customer Management

| # | Capability Name | Description | Frequency | Volume |
|---|----------------|-------------|-----------|--------|
| 33 | Customer Creation | Create new customer records with personal and contact information | On-demand | Medium |
| 34 | Customer Information Retrieval | Retrieve customer details and associated information | Real-time | High |
| 35 | Customer Search | Search for customers by various criteria including phone number and legal name | Real-time | Medium |
| 36 | Customer Attribute Management | Manage custom attributes for customer records | On-demand | Medium |
| 37 | Customer Address Management | Create, update, and retrieve customer address information | On-demand | Medium |
| 38 | Customer Account Linking | Link customers to bank accounts with specified relationships | On-demand | Medium |
| 39 | Agent Management | Create and manage agent records for cash withdrawal and other services | On-demand | Low |

### Category: User Management

| # | Capability Name | Description | Frequency | Volume |
|---|----------------|-------------|-----------|--------|
| 40 | User Information Retrieval | Retrieve user profile information by various identifiers | Real-time | High |
| 41 | User Search | Search for users by email, username, or user ID | Real-time | Medium |
| 42 | User Creation | Create new user accounts with specified roles and permissions | On-demand | Medium |
| 43 | User Invitation | Send invitations to new users to join the platform | On-demand | Low |
| 44 | User Lock/Unlock | Lock or unlock user accounts for security purposes | On-demand | Low |
| 45 | User Deletion | Remove user accounts from the system | On-demand | Low |
| 46 | Password Reset | Generate password reset links for users | On-demand | Medium |
| 47 | User Synchronization | Synchronize external user data with the platform | On-demand | Low |

### Category: Authorization & Access Control

| # | Capability Name | Description | Frequency | Volume |
|---|----------------|-------------|-----------|--------|
| 48 | View Management | Create, update, and manage custom views for account data access control | On-demand | Medium |
| 49 | System View Management | Manage system-level views with predefined permission sets | On-demand | Low |
| 50 | Account Access Granting | Grant users access to accounts through specific views | On-demand | Medium |
| 51 | Account Access Revocation | Revoke user access to accounts and views | On-demand | Medium |
| 52 | Entitlement Management | Assign and manage role-based entitlements for users | On-demand | Medium |
| 53 | Scope Management | Manage OAuth scopes for API access control | On-demand | Low |
| 54 | Permission Retrieval | Retrieve permissions and access rights for users and accounts | Real-time | High |

### Category: Consent Management

| # | Capability Name | Description | Frequency | Volume |
|---|----------------|-------------|-----------|--------|
| 55 | Consent Creation | Create consent records for account access authorization | Real-time | High |
| 56 | Consent Information Retrieval | Retrieve consent details and status | Real-time | High |
| 57 | Consent Status Management | Update consent status including acceptance, rejection, and expiration | Real-time | High |
| 58 | Consent Revocation | Revoke active consents for account access | On-demand | Medium |
| 59 | Consent Listing | List all consents for a user or bank | Real-time | Medium |
| 60 | Consent Authorization Flow | Handle multi-step consent authorization with SCA challenges | Real-time | High |

### Category: PSD2 Berlin Group Compliance

| # | Capability Name | Description | Frequency | Volume |
|---|----------------|-------------|-----------|--------|
| 61 | Account Information Service (AIS) | Provide PSD2-compliant account information access for authorized third parties | Real-time | Very High |
| 62 | Payment Initiation Service (PIS) | Enable PSD2-compliant payment initiation by authorized third parties | Real-time | High |
| 63 | Confirmation of Funds Service (PIIS) | Verify fund availability for card-based payments | Real-time | High |
| 64 | Signing Baskets | Authorize multiple transactions with a single SCA process | Real-time | Medium |
| 65 | Strong Customer Authentication (SCA) | Handle multi-factor authentication for sensitive operations | Real-time | Very High |
| 66 | Card Account Information | Retrieve card account details and transaction history | Real-time | High |

### Category: Counterparty Management

| # | Capability Name | Description | Frequency | Volume |
|---|----------------|-------------|-----------|--------|
| 67 | Counterparty Creation | Create counterparty records with routing information | On-demand | Medium |
| 68 | Counterparty Retrieval | Retrieve counterparty details and routing information | Real-time | High |
| 69 | Counterparty Metadata Management | Manage metadata associated with counterparties | On-demand | Low |
| 70 | Counterparty Limit Management | Set and manage transaction limits for counterparties | On-demand | Low |

### Category: Card Management

| # | Capability Name | Description | Frequency | Volume |
|---|----------------|-------------|-----------|--------|
| 71 | Card Creation | Create physical and virtual card records | On-demand | Medium |
| 72 | Card Information Retrieval | Retrieve card details and associated account information | Real-time | High |
| 73 | Card Update | Update card attributes and status | On-demand | Medium |
| 74 | Card Attribute Management | Manage custom attributes for card records | On-demand | Low |

### Category: Product & Branch Management

| # | Capability Name | Description | Frequency | Volume |
|---|----------------|-------------|-----------|--------|
| 75 | Product Catalog Management | Create and manage banking product definitions | On-demand | Low |
| 76 | Product Collection Management | Organize products into collections for presentation | On-demand | Low |
| 77 | Product Fee Management | Define and manage fees associated with products | On-demand | Low |
| 78 | Branch Information Management | Create and manage branch location information | On-demand | Low |
| 79 | ATM Information Management | Create and manage ATM location and service information | On-demand | Low |
| 80 | ATM Attribute Management | Manage custom attributes for ATM records including languages and services | On-demand | Low |

### Category: KYC & Compliance

| # | Capability Name | Description | Frequency | Volume |
|---|----------------|-------------|-----------|--------|
| 81 | KYC Check Management | Record and retrieve KYC verification checks | On-demand | Medium |
| 82 | KYC Document Management | Store and retrieve KYC documentation | On-demand | Medium |
| 83 | KYC Media Management | Manage media files associated with KYC processes | On-demand | Low |
| 84 | KYC Status Management | Track and update KYC verification status | On-demand | Medium |
| 85 | Tax Residence Management | Manage customer tax residence information | On-demand | Low |
| 86 | Regulated Entity Management | Manage records of regulated financial entities | On-demand | Low |

### Category: Consumer & Application Management

| # | Capability Name | Description | Frequency | Volume |
|---|----------------|-------------|-----------|--------|
| 87 | Consumer Registration | Register third-party applications as API consumers | On-demand | Low |
| 88 | Consumer Management | Update consumer application details and credentials | On-demand | Low |
| 89 | Consumer Rate Limiting | Set and manage API call limits for consumers | On-demand | Low |
| 90 | Consumer Certificate Management | Manage client certificates for secure communication | On-demand | Low |

### Category: Webhook & Notification

| # | Capability Name | Description | Frequency | Volume |
|---|----------------|-------------|-----------|--------|
| 91 | Webhook Configuration | Configure webhooks for account and transaction events | On-demand | Low |
| 92 | Account Notification Webhooks | Send notifications for account-related events | Event-driven | High |
| 93 | Transaction Notification Webhooks | Send notifications for transaction events | Event-driven | Very High |

### Category: Dynamic Configuration

| # | Capability Name | Description | Frequency | Volume |
|---|----------------|-------------|-----------|--------|
| 94 | Dynamic Entity Management | Create and manage runtime-defined data entities | On-demand | Low |
| 95 | Dynamic Endpoint Management | Create and manage runtime-defined API endpoints | On-demand | Low |
| 96 | Method Routing Configuration | Configure connector routing for different banks and methods | On-demand | Low |
| 97 | Endpoint Mapping Configuration | Map API endpoints to backend implementations | On-demand | Low |
| 98 | Web UI Properties Management | Manage runtime-configurable UI properties | On-demand | Low |

### Category: Monitoring & Metrics

| # | Capability Name | Description | Frequency | Volume |
|---|----------------|-------------|-----------|--------|
| 99 | API Metrics Collection | Collect and store API usage metrics | Real-time | Very High |
| 100 | Aggregate Metrics Retrieval | Retrieve aggregated API usage statistics | On-demand | Medium |
| 101 | System Health Monitoring | Monitor and report system health status | Real-time | Medium |
| 102 | Log Cache Access | Access cached log entries for debugging | On-demand | Low |

### Category: Scheduled Background Tasks

| # | Capability Name | Description | Frequency | Volume |
|---|----------------|-------------|-----------|--------|
| 103 | Consent Expiration Processing | Automatically expire outdated and invalid consents | Scheduled (Periodic) | Medium |
| 104 | Berlin Group Consent Cleanup | Clean up unfinished Berlin Group consent requests | Scheduled (Periodic) | Medium |
| 105 | Metrics Archival | Archive old metrics data to maintain database performance | Scheduled (Daily) | High |
| 106 | Transaction Status Cleanup | Update status of outdated transaction requests | Scheduled (Periodic) | Medium |
| 107 | Database Maintenance | Perform routine database cleanup and optimization | Scheduled (Daily) | Low |

### Category: Foreign Exchange

| # | Capability Name | Description | Frequency | Volume |
|---|----------------|-------------|-----------|--------|
| 108 | FX Rate Retrieval | Retrieve current foreign exchange rates | Real-time | Medium |
| 109 | FX Rate Management | Create and update foreign exchange rate definitions | On-demand | Low |

### Category: API Documentation & Discovery

| # | Capability Name | Description | Frequency | Volume |
|---|----------------|-------------|-----------|--------|
| 110 | API Information Retrieval | Retrieve API version and configuration information | Real-time | Medium |
| 111 | Resource Documentation | Generate and serve API documentation in Swagger/OpenAPI format | Real-time | Medium |
| 112 | API Collection Management | Organize and manage collections of API endpoints | On-demand | Low |
| 113 | Glossary Management | Manage API glossary terms and definitions | On-demand | Low |

### Category: Authentication

| # | Capability Name | Description | Frequency | Volume |
|---|----------------|-------------|-----------|--------|
| 114 | OAuth 1.0a Authentication | Support OAuth 1.0a authentication flow | Real-time | Medium |
| 115 | OAuth 2.0/OIDC Authentication | Support OAuth 2.0 and OpenID Connect authentication | Real-time | High |
| 116 | Direct Login Authentication | Support direct username/password authentication | Real-time | Medium |
| 117 | Gateway Login | Support gateway-based authentication for backend systems | Real-time | Medium |
| 118 | MTLS Client Certificate Authentication | Support mutual TLS client certificate authentication | Real-time | Medium |

### Capability Summary
- **Total Capabilities Identified**: 118
- **API Endpoints**: 500+ (across all API versions v1.2.1 through v6.0.0)
- **Background Tasks**: 5
- **Event Consumers**: Webhook-based event notifications
- **Batch Jobs**: Metrics archival, consent cleanup, transaction status updates
- **External Integrations**: Multiple backend connectors (REST, Akka, Stored Procedure, RabbitMQ, Cardano, Ethereum)
- **Primary Business Functions**: Account Management, Payment Processing, Consent Management, PSD2 Compliance, Customer Management, Authorization & Access Control

---

## Open Questions & Clarifications Needed

1. **Backend Connector Selection**: The system supports multiple backend connectors (Mapped, REST, Akka, Stored Procedure, RabbitMQ, Cardano, Ethereum). Clarification needed on which connectors are actively used in production environments and their specific use cases.

2. **Regional API Implementations**: The codebase includes implementations for Berlin Group, UK Open Banking, Australian Open Banking, Polish API, STET, Bahrain OBF, and Mexican Open Finance. Clarification needed on which regional implementations are actively deployed and maintained.

3. **Dynamic Entity Usage**: The system supports runtime-defined dynamic entities and endpoints. Clarification needed on the extent of usage and governance around dynamic configurations in production.

4. **Blockchain Integration**: The codebase includes connectors for Cardano and Ethereum blockchains. Clarification needed on the business use cases and deployment status of blockchain integrations.

5. **Webhook Delivery Guarantees**: Clarification needed on the delivery guarantees and retry mechanisms for webhook notifications.

6. **Metrics Retention Policy**: The system has configurable metrics retention periods. Clarification needed on the business requirements for metrics retention and archival.

7. **Consent Frequency Limits**: Berlin Group consents have configurable frequency limits per day. Clarification needed on the business rationale for specific limit values.

8. **Multi-tenancy Model**: Clarification needed on how the system handles multi-tenancy for banks and whether each bank requires a separate deployment or shares infrastructure.
