# High-Level Requirements Document: Open Bank Project API

## 1. System Overview & Purpose

### System Identification
- **System Name**: Open Bank Project (OBP) API
- **Application Code/ID**: OBP-API
- **Business Domain**: Financial Services / Open Banking Platform

### Business Purpose
The Open Bank Project API is an open-source RESTful API platform designed to abstract core banking system complexities and enable financial applications to interact with multiple banks on behalf of account holders. The system serves as a standardized banking interface layer that allows third-party developers to build financial applications without needing to understand each bank's proprietary systems. It enables banks to expose their services through standard APIs while maintaining control over backend systems and data access, supporting the mission of "Bank as a Platform, Transparency as an Asset."

### System Criticality
- **Criticality Level**: High
- **Business Impact if Unavailable**: Complete disruption of third-party financial application integrations, inability to process payment initiations, account information requests, and regulatory compliance operations. Banks would lose their open banking capabilities, affecting customer-facing fintech applications and potentially causing regulatory non-compliance with PSD2 and Open Banking mandates.

### System Type
- **Architecture**: REST API / Full-Stack Web Application
- **Processing Model**: Mixed (HTTP request-response, Scheduled tasks, Event-driven processing)

### Key Stakeholders
- Third-Party Developers building financial applications (fintech apps, personal finance managers, accounting software)
- Banks seeking to expose their services via standardized APIs
- Payment Service Providers (PSPs) requiring PSD2-compliant APIs for Account Information (AIS) and Payment Initiation (PIS) services
- Regulators and Auditors needing transparent access to financial data
- Account Holders granting consent for third-party access to their financial data

---

## 2. Core Capabilities Inventory

### Category: Bank & Institution Management
| # | Capability Name | Description | Frequency | Volume |
|---|----------------|-------------|-----------|--------|
| 1 | Bank Information Retrieval | Retrieve list of supported banks and detailed bank information including name, logo, and website | Real-time | High |
| 2 | Bank Creation | Create new bank entities with full configuration including routing information and settlement accounts | On-demand | Low |
| 3 | Bank Attribute Management | Create, update, retrieve, and delete custom attributes for banks | On-demand | Medium |
| 4 | Settlement Account Management | Create and manage settlement accounts for payment processing across different payment systems | On-demand | Medium |
| 5 | Branch Management | Manage bank branch information including locations and services | On-demand | Low |
| 6 | ATM Management | Manage ATM locations and attributes for bank networks | On-demand | Low |

### Category: Account Management
| # | Capability Name | Description | Frequency | Volume |
|---|----------------|-------------|-----------|--------|
| 7 | Account Listing | Retrieve list of accounts accessible to the authenticated user at a specific bank | Real-time | High |
| 8 | Account Details Retrieval | Get detailed account information including balance, metadata, and account type | Real-time | High |
| 9 | Account Creation | Create new bank accounts with specified currency, type, and ownership | On-demand | Medium |
| 10 | Account Label Update | Update the display label for bank accounts | On-demand | Low |
| 11 | Account Balance Retrieval | Get current and available balances for bank accounts | Real-time | Very High |
| 12 | Account Routing Lookup | Find accounts by routing information such as IBAN or account number | Real-time | High |
| 13 | Account Attribute Management | Create, update, and retrieve custom attributes for accounts | On-demand | Medium |
| 14 | Firehose Account Access | Bulk retrieval of all accounts at a bank for authorized administrative users | On-demand | Low |
| 15 | Account Deletion (Cascade) | Delete accounts and all associated data including transactions | On-demand | Low |

### Category: Transaction Management
| # | Capability Name | Description | Frequency | Volume |
|---|----------------|-------------|-----------|--------|
| 16 | Transaction Listing | Retrieve transaction history for accounts with filtering and pagination | Real-time | Very High |
| 17 | Transaction Details | Get detailed information about specific transactions | Real-time | High |
| 18 | Transaction Attribute Management | Create, update, and retrieve custom attributes for transactions | On-demand | Medium |
| 19 | Double-Entry Transaction View | View the double-entry bookkeeping representation of transactions | Real-time | Medium |
| 20 | Balancing Transaction Retrieval | Get the corresponding balancing transaction for double-entry accounting | Real-time | Medium |
| 21 | Historical Transaction Creation | Create historical transactions for data migration or reconciliation | On-demand | Medium |
| 22 | Transaction Tagging | Add, retrieve, and delete tags on transactions for categorization | On-demand | Medium |
| 23 | Transaction Metadata Management | Manage narrative, comments, images, and location data for transactions | On-demand | Medium |

### Category: Payment Initiation & Transaction Requests
| # | Capability Name | Description | Frequency | Volume |
|---|----------------|-------------|-----------|--------|
| 24 | SEPA Credit Transfer Initiation | Initiate SEPA credit transfer payments between accounts | Real-time | High |
| 25 | Counterparty Payment Initiation | Initiate payments to registered counterparties | Real-time | High |
| 26 | Account-to-Account Transfer | Initiate transfers between accounts at the same or different banks | Real-time | High |
| 27 | Refund Processing | Process refund transactions for previous payments | On-demand | Medium |
| 28 | Card Payment Processing | Process card-based payment transactions | Real-time | High |
| 29 | Standing Order Management | Create and manage recurring payment instructions | On-demand | Medium |
| 30 | Direct Debit Management | Create and manage direct debit mandates and collections | On-demand | Medium |
| 31 | Transaction Request Status | Retrieve status of pending and completed transaction requests | Real-time | High |
| 32 | Transaction Request Challenge | Answer security challenges for transaction authorization | Real-time | High |
| 33 | Periodic Payment Initiation | Initiate scheduled recurring payments with defined frequency | On-demand | Medium |
| 34 | Bulk Payment Initiation | Initiate multiple payments in a single batch request | On-demand | Medium |
| 35 | Payment Cancellation | Cancel pending payment requests before execution | On-demand | Low |

### Category: Customer Management
| # | Capability Name | Description | Frequency | Volume |
|---|----------------|-------------|-----------|--------|
| 36 | Customer Creation | Create new customer records with personal and contact information | On-demand | Medium |
| 37 | Customer Information Retrieval | Retrieve customer details by ID, phone number, or attributes | Real-time | High |
| 38 | Customer Attribute Management | Create, update, and retrieve custom attributes for customers | On-demand | Medium |
| 39 | Customer Address Management | Manage customer address information | On-demand | Low |
| 40 | Customer Tax Residence | Manage customer tax residence information for regulatory compliance | On-demand | Low |
| 41 | Customer-Account Linking | Link customers to their associated bank accounts | On-demand | Medium |
| 42 | Customer Deletion (Cascade) | Delete customers and all associated data | On-demand | Low |
| 43 | KYC Document Management | Manage Know Your Customer documents and verification status | On-demand | Medium |
| 44 | KYC Status Management | Track and update KYC verification status for customers | On-demand | Medium |
| 45 | KYC Check Management | Record and retrieve KYC check results | On-demand | Medium |

### Category: User & Identity Management
| # | Capability Name | Description | Frequency | Volume |
|---|----------------|-------------|-----------|--------|
| 46 | User Registration | Create new user accounts with authentication credentials | On-demand | Medium |
| 47 | User Authentication | Authenticate users via multiple methods (OAuth, Direct Login, OpenID Connect) | Real-time | Very High |
| 48 | User Profile Management | Retrieve and update user profile information | Real-time | High |
| 49 | User Search | Search for users by ID, username, or email | On-demand | Medium |
| 50 | User Invitation | Send invitations for new users to join the platform | On-demand | Low |
| 51 | User Locking | Lock user accounts for security purposes | On-demand | Low |
| 52 | User Deletion | Delete user accounts and associated data | On-demand | Low |
| 53 | Password Reset | Generate password reset URLs for users | On-demand | Medium |
| 54 | User-Customer Linking | Link user accounts to customer records | On-demand | Medium |
| 55 | User Attribute Management | Manage custom attributes for user profiles | On-demand | Low |

### Category: Authorization & Access Control
| # | Capability Name | Description | Frequency | Volume |
|---|----------------|-------------|-----------|--------|
| 56 | View-Based Access Control | Grant and revoke user access to accounts through configurable views | On-demand | High |
| 57 | Entitlement Management | Assign and manage role-based permissions for users | On-demand | Medium |
| 58 | Scope Management | Create and manage OAuth scopes for API access control | On-demand | Low |
| 59 | Consumer Management | Register and manage third-party application consumers | On-demand | Medium |
| 60 | Rate Limiting | Set and enforce API call limits per consumer | On-demand | Medium |
| 61 | Account Access Grants | Grant specific users access to specific account views | On-demand | High |
| 62 | Entitlement Request Processing | Process requests for additional entitlements from users | On-demand | Low |

### Category: Consent Management (PSD2 Compliance)
| # | Capability Name | Description | Frequency | Volume |
|---|----------------|-------------|-----------|--------|
| 63 | Consent Creation | Create consent records for third-party access to account data | Real-time | High |
| 64 | Consent Status Management | Update and track consent status through its lifecycle | Real-time | High |
| 65 | Consent Revocation | Revoke previously granted consents | On-demand | Medium |
| 66 | Consent Information Retrieval | Retrieve details of existing consents | Real-time | High |
| 67 | Consent Authorization Flow | Manage Strong Customer Authentication (SCA) for consent authorization | Real-time | High |
| 68 | Consent User Addition | Add additional users to existing consents | On-demand | Low |

### Category: Berlin Group PSD2 - Account Information Service (AIS)
| # | Capability Name | Description | Frequency | Volume |
|---|----------------|-------------|-----------|--------|
| 69 | PSD2 Account List | Retrieve list of payment accounts per Berlin Group specification | Real-time | High |
| 70 | PSD2 Account Details | Get detailed account information per Berlin Group specification | Real-time | High |
| 71 | PSD2 Balance Retrieval | Get account balances per Berlin Group specification | Real-time | Very High |
| 72 | PSD2 Transaction List | Retrieve transaction history per Berlin Group specification | Real-time | Very High |
| 73 | PSD2 Transaction Details | Get individual transaction details per Berlin Group specification | Real-time | High |
| 74 | PSD2 Card Account Access | Access card account information per Berlin Group specification | Real-time | Medium |
| 75 | PSD2 Card Account Balances | Retrieve card account balances | Real-time | Medium |
| 76 | PSD2 Card Account Transactions | Retrieve card account transaction history | Real-time | Medium |

### Category: Berlin Group PSD2 - Payment Initiation Service (PIS)
| # | Capability Name | Description | Frequency | Volume |
|---|----------------|-------------|-----------|--------|
| 77 | PSD2 Payment Initiation | Initiate payments per Berlin Group specification | Real-time | High |
| 78 | PSD2 Payment Status | Retrieve payment initiation status | Real-time | High |
| 79 | PSD2 Payment Information | Get details of initiated payments | Real-time | High |
| 80 | PSD2 Payment Cancellation | Cancel pending payment initiations | On-demand | Medium |
| 81 | PSD2 Payment Authorization | Manage SCA flow for payment authorization | Real-time | High |
| 82 | PSD2 Periodic Payment Initiation | Initiate recurring payments per Berlin Group specification | On-demand | Medium |
| 83 | PSD2 Bulk Payment Initiation | Initiate bulk payments per Berlin Group specification | On-demand | Medium |

### Category: Berlin Group PSD2 - Confirmation of Funds (PIIS)
| # | Capability Name | Description | Frequency | Volume |
|---|----------------|-------------|-----------|--------|
| 84 | Funds Confirmation | Confirm availability of funds for card-based payments | Real-time | High |

### Category: Berlin Group PSD2 - Signing Baskets
| # | Capability Name | Description | Frequency | Volume |
|---|----------------|-------------|-----------|--------|
| 85 | Signing Basket Creation | Create signing baskets for batch authorization | On-demand | Low |
| 86 | Signing Basket Authorization | Authorize multiple transactions with single SCA | On-demand | Low |
| 87 | Signing Basket Status | Retrieve status of signing baskets | On-demand | Low |

### Category: Counterparty Management
| # | Capability Name | Description | Frequency | Volume |
|---|----------------|-------------|-----------|--------|
| 88 | Counterparty Creation | Register new counterparties for payment purposes | On-demand | Medium |
| 89 | Counterparty Listing | Retrieve list of registered counterparties | Real-time | Medium |
| 90 | Counterparty Details | Get detailed information about specific counterparties | Real-time | Medium |
| 91 | Counterparty Deletion | Remove registered counterparties | On-demand | Low |
| 92 | Counterparty Limit Management | Set and manage transaction limits for counterparties | On-demand | Low |

### Category: Product & Catalog Management
| # | Capability Name | Description | Frequency | Volume |
|---|----------------|-------------|-----------|--------|
| 93 | Product Catalog Management | Create and manage banking product definitions | On-demand | Low |
| 94 | Product Attribute Management | Define and manage custom attributes for products | On-demand | Low |
| 95 | Product Fee Management | Configure fees associated with banking products | On-demand | Low |
| 96 | Product Collection Management | Group products into collections for organization | On-demand | Low |
| 97 | Account Application Processing | Process applications for new banking products | On-demand | Medium |

### Category: Dynamic Configuration & Extensibility
| # | Capability Name | Description | Frequency | Volume |
|---|----------------|-------------|-----------|--------|
| 98 | Dynamic Entity Management | Create runtime-defined data models without code deployment | On-demand | Low |
| 99 | Dynamic Endpoint Creation | Define custom API endpoints at runtime | On-demand | Low |
| 100 | Connector Method Routing | Configure routing of connector methods to different backends | On-demand | Low |
| 101 | Endpoint Mapping | Map external endpoints to internal implementations | On-demand | Low |
| 102 | Attribute Definition Management | Define attribute schemas for various entity types | On-demand | Low |
| 103 | Validation Rule Configuration | Configure custom validation rules for data | On-demand | Low |

### Category: API Collections & Documentation
| # | Capability Name | Description | Frequency | Volume |
|---|----------------|-------------|-----------|--------|
| 104 | API Collection Management | Create and manage collections of API endpoints | On-demand | Low |
| 105 | API Documentation Generation | Auto-generate Swagger/OpenAPI documentation | Real-time | Medium |
| 106 | Resource Documentation | Provide detailed documentation for API resources | Real-time | Medium |
| 107 | Glossary Management | Maintain glossary of banking and API terms | On-demand | Low |

### Category: Webhook & Event Notifications
| # | Capability Name | Description | Frequency | Volume |
|---|----------------|-------------|-----------|--------|
| 108 | Account Notification Webhooks | Configure webhooks for account-related events | Event-driven | Medium |
| 109 | System Notification Webhooks | Configure system-level event notifications | Event-driven | Low |
| 110 | Webhook Management | Create, update, and delete webhook configurations | On-demand | Low |

### Category: Metrics & Monitoring
| # | Capability Name | Description | Frequency | Volume |
|---|----------------|-------------|-----------|--------|
| 111 | API Metrics Collection | Collect and store API usage metrics | Real-time | Very High |
| 112 | Metrics Retrieval | Retrieve API usage statistics and performance data | On-demand | Medium |
| 113 | Metrics Archival | Archive historical metrics data for long-term storage | Scheduled (Daily) | High |
| 114 | System Health Monitoring | Monitor system health and connectivity status | Real-time | Medium |

### Category: Administrative & System Management
| # | Capability Name | Description | Frequency | Volume |
|---|----------------|-------------|-----------|--------|
| 115 | Database Information | Retrieve database configuration and status information | On-demand | Low |
| 116 | System Configuration Management | Manage system-wide configuration parameters | On-demand | Low |
| 117 | Web UI Properties Management | Configure web interface properties and branding | On-demand | Low |
| 118 | Endpoint Tag Management | Manage tags for API endpoint categorization | On-demand | Low |
| 119 | Migration Management | Execute and track database migrations | On-demand | Low |
| 120 | Cache Management | Manage API response caching | On-demand | Low |

### Category: Authentication Methods
| # | Capability Name | Description | Frequency | Volume |
|---|----------------|-------------|-----------|--------|
| 121 | OAuth 1.0a Authentication | Support OAuth 1.0a authentication flow | Real-time | High |
| 122 | Direct Login Authentication | Support direct username/password authentication | Real-time | High |
| 123 | OpenID Connect Integration | Support OpenID Connect authentication providers | Real-time | High |
| 124 | Gateway Login | Support gateway-based authentication for enterprise integration | Real-time | Medium |
| 125 | DAuth Authentication | Support distributed authentication mechanisms | Real-time | Low |

### Category: IBAN & Account Validation
| # | Capability Name | Description | Frequency | Volume |
|---|----------------|-------------|-----------|--------|
| 126 | IBAN Validation | Validate and check IBAN numbers for correctness | Real-time | High |
| 127 | Account Routing Validation | Validate account routing information | Real-time | Medium |

### Category: Scheduled Background Tasks
| # | Capability Name | Description | Frequency | Volume |
|---|----------------|-------------|-----------|--------|
| 128 | Consent Expiration Processing | Automatically expire outdated and invalid consents | Scheduled (Configurable) | Medium |
| 129 | Berlin Group Consent Cleanup | Clean up unfinished Berlin Group consent requests | Scheduled (Configurable) | Medium |
| 130 | Metrics Archive Processing | Archive and clean up old metrics data | Scheduled (Daily) | High |
| 131 | Database Cleanup | Clean up outdated database records | Scheduled (Configurable) | Medium |
| 132 | Transaction Processing | Process pending transactions in background | Scheduled (Configurable) | Medium |

### Category: Backend Integration (Connectors)
| # | Capability Name | Description | Frequency | Volume |
|---|----------------|-------------|-----------|--------|
| 133 | Local Database Connector | Direct database access for banking operations | Real-time | Very High |
| 134 | REST Backend Connector | HTTP-based communication with external banking systems | Real-time | High |
| 135 | Akka Actor Connector | Actor-based distributed system integration | Real-time | Medium |
| 136 | Stored Procedure Connector | Database stored procedure integration | Real-time | Medium |
| 137 | RabbitMQ Connector | Message queue integration for asynchronous processing | Event-driven | Medium |
| 138 | Blockchain Connectors | Integration with Ethereum and Cardano blockchains | Real-time | Low |

### Category: Regional Open Banking Standards
| # | Capability Name | Description | Frequency | Volume |
|---|----------------|-------------|-----------|--------|
| 139 | UK Open Banking Support | Support UK Open Banking API specifications | Real-time | High |
| 140 | Bahrain OBF Support | Support Bahrain Open Banking Framework | Real-time | Low |
| 141 | Polish API Support | Support Polish banking API standards | Real-time | Low |
| 142 | Australian Open Banking | Support Australian Consumer Data Right standards | Real-time | Low |

### Category: CRM & Meeting Management
| # | Capability Name | Description | Frequency | Volume |
|---|----------------|-------------|-----------|--------|
| 143 | CRM Event Management | Track customer relationship management events | On-demand | Low |
| 144 | Meeting Scheduling | Schedule and manage customer meetings | On-demand | Low |
| 145 | Video Meeting Integration | Integration with video conferencing services | On-demand | Low |

---

### Capability Summary
- **Total Capabilities Identified**: 145
- **API Endpoints**: ~500+ (across all API versions v1.2.1 through v6.0.0)
- **Background Tasks**: 5
- **Event Consumers**: Webhook-based event notifications
- **Batch Jobs**: 4 (Consent cleanup, Metrics archival, Database cleanup, Transaction processing)
- **External Integrations**: 8+ (REST backends, Akka, RabbitMQ, Stored Procedures, Blockchain)
- **Primary Business Functions**: 
  - Account Information Services
  - Payment Initiation Services
  - Customer Management
  - User & Authorization Management
  - PSD2/Open Banking Compliance
  - Multi-Backend Integration
  - API Management & Documentation

---

## Open Questions & Clarifications Needed

1. **Transaction Volume Estimates**: What are the expected peak transaction volumes for payment initiation and account information requests?

2. **Regional Deployment**: Which regional Open Banking standards (UK, Bahrain, Polish, Australian) are actively used in production deployments?

3. **Blockchain Integration Usage**: What is the current adoption level of the Ethereum and Cardano blockchain connectors?

4. **Consent Retention Policy**: What is the business requirement for consent data retention after expiration?

5. **Metrics Archival Period**: The current default is 3 years for metrics archive retention - is this aligned with regulatory requirements?

6. **Dynamic Entity Usage**: What are the primary use cases for runtime-defined dynamic entities in production?

7. **Webhook Event Types**: What specific account and system events should trigger webhook notifications?

8. **SCA Method Preferences**: Which Strong Customer Authentication methods (SMS OTP, Email, Chip OTP) are preferred for different transaction types?

---

*Document generated from source code analysis of OBP-API codebase*
*Analysis Date: December 2025*
