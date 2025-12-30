# Open Bank Project (OBP) API - High-Level Requirements Document

## Section 1: System Overview & Purpose

### System Identification
- **System Name**: Open Bank Project API (OBP-API)
- **Application Code/ID**: OBP-API
- **Business Domain**: Financial Services / Open Banking Platform

### Business Purpose
The Open Bank Project API is an open-source RESTful API platform designed to abstract core banking system complexities and enable financial applications to interact with multiple banks on behalf of account holders. The system serves as a standardized banking interface layer that allows third-party developers, fintech companies, and payment service providers to build financial applications without needing to understand each bank's proprietary systems. The platform's mission is "Bank as a Platform, Transparency as an Asset," enabling banks to expose their services through standardized APIs while maintaining control over backend systems and data access.

### System Criticality
- **Criticality Level**: High
- **Business Impact if Unavailable**: If the OBP-API platform becomes unavailable, all third-party financial applications relying on the API would lose access to banking services, preventing customers from viewing accounts, initiating payments, or accessing financial data. This would impact regulatory compliance (PSD2/Open Banking), disrupt fintech partner integrations, and potentially affect millions of end-user transactions.

### System Type
- **Architecture**: REST API / Full-Stack Web Application
- **Processing Model**: Mixed (HTTP request-response, Scheduled tasks, Event-driven via Akka actors and webhooks)

### Key Stakeholders
- Third-Party Developers and Fintech Companies building financial applications
- Banks and Financial Institutions seeking to expose services via standardized APIs
- Payment Service Providers (PSPs) requiring PSD2-compliant APIs
- Regulators and Auditors needing transparent access to financial data
- End Customers accessing banking services through third-party applications
- System Administrators managing the API platform

### Technology Stack
- **Programming Language**: Scala (functional and object-oriented paradigms)
- **Web Framework**: Lift Framework
- **Build Tool**: Maven with SBT support
- **Database Support**: PostgreSQL, H2, MySQL, MS SQL Server, Oracle
- **Message Processing**: Akka Actors, Akka Streams
- **Backend Connectors**: REST, Akka, Stored Procedures, Kafka, RabbitMQ, Cardano, Ethereum
- **Authentication**: OAuth 1.0a, OAuth 2.0, OpenID Connect, Direct Login, mTLS

### Integration Patterns
The system supports multiple backend integration patterns through its connector abstraction layer, including REST API connectors for HTTP-based backends, Akka connectors for actor-based distributed systems, stored procedure connectors for database-driven backends, Kafka connectors for message queue integration, and specialized connectors for blockchain platforms (Cardano, Ethereum). The platform also supports dynamic connector routing based on bank-specific configurations.

---

## Section 2: Core Capabilities Inventory

```json
{
  "coreCapabilities": [
    {
      "category": "Bank Management",
      "capabilities": [
        {
          "id": 1,
          "name": "Bank Information Retrieval",
          "description": "Retrieve information about banks supported on the platform including ID, name, logo, and website",
          "frequency": "Real-time",
          "volume": "High"
        },
        {
          "id": 2,
          "name": "Bank Creation and Management",
          "description": "Create and manage bank entities with associated attributes and configurations",
          "frequency": "On-demand",
          "volume": "Low"
        },
        {
          "id": 3,
          "name": "Bank Attribute Management",
          "description": "Create, update, and retrieve custom attributes associated with banks",
          "frequency": "On-demand",
          "volume": "Low"
        },
        {
          "id": 4,
          "name": "Settlement Account Management",
          "description": "Create and manage settlement accounts for double-entry bookkeeping and payment processing",
          "frequency": "On-demand",
          "volume": "Medium"
        }
      ]
    },
    {
      "category": "Account Management",
      "capabilities": [
        {
          "id": 5,
          "name": "Account Listing",
          "description": "Retrieve list of accounts at a bank that a user has access to with various detail levels",
          "frequency": "Real-time",
          "volume": "High"
        },
        {
          "id": 6,
          "name": "Account Details Retrieval",
          "description": "Get detailed information about specific accounts including balances and metadata",
          "frequency": "Real-time",
          "volume": "High"
        },
        {
          "id": 7,
          "name": "Account Creation",
          "description": "Create new bank accounts with specified parameters and ownership",
          "frequency": "On-demand",
          "volume": "Medium"
        },
        {
          "id": 8,
          "name": "Account Balance Retrieval",
          "description": "Retrieve current and available balances for bank accounts",
          "frequency": "Real-time",
          "volume": "High"
        },
        {
          "id": 9,
          "name": "Account Label Management",
          "description": "Update and manage account labels and display names",
          "frequency": "On-demand",
          "volume": "Low"
        },
        {
          "id": 10,
          "name": "Account Routing Lookup",
          "description": "Find accounts by routing information such as IBAN or account number",
          "frequency": "Real-time",
          "volume": "Medium"
        },
        {
          "id": 11,
          "name": "Firehose Account Access",
          "description": "Bulk retrieval of all accounts at a bank for authorized administrative users",
          "frequency": "On-demand",
          "volume": "High"
        },
        {
          "id": 12,
          "name": "Account Attribute Management",
          "description": "Create, update, and retrieve custom attributes associated with accounts",
          "frequency": "On-demand",
          "volume": "Medium"
        },
        {
          "id": 13,
          "name": "IBAN Validation",
          "description": "Validate and check IBAN numbers for correctness and errors",
          "frequency": "Real-time",
          "volume": "Medium"
        }
      ]
    },
    {
      "category": "Transaction Management",
      "capabilities": [
        {
          "id": 14,
          "name": "Transaction Listing",
          "description": "Retrieve list of transactions for an account with filtering and pagination",
          "frequency": "Real-time",
          "volume": "High"
        },
        {
          "id": 15,
          "name": "Transaction Details Retrieval",
          "description": "Get detailed information about specific transactions including metadata",
          "frequency": "Real-time",
          "volume": "High"
        },
        {
          "id": 16,
          "name": "Double-Entry Transaction Retrieval",
          "description": "View double-entry bookkeeping transactions showing debit and credit sides",
          "frequency": "Real-time",
          "volume": "Medium"
        },
        {
          "id": 17,
          "name": "Historical Transaction Creation",
          "description": "Create historical transaction records for data migration or reconciliation",
          "frequency": "On-demand",
          "volume": "Medium"
        },
        {
          "id": 18,
          "name": "Transaction Attribute Management",
          "description": "Create, update, and retrieve custom attributes associated with transactions",
          "frequency": "On-demand",
          "volume": "Medium"
        },
        {
          "id": 19,
          "name": "Transaction Tagging",
          "description": "Add, retrieve, and delete tags on transactions for categorization",
          "frequency": "On-demand",
          "volume": "Medium"
        }
      ]
    },
    {
      "category": "Payment Initiation",
      "capabilities": [
        {
          "id": 20,
          "name": "Transaction Request Creation",
          "description": "Initiate payment requests supporting multiple types including SEPA, counterparty, and card payments",
          "frequency": "Real-time",
          "volume": "High"
        },
        {
          "id": 21,
          "name": "Transaction Request Challenge",
          "description": "Handle Strong Customer Authentication challenges for payment authorization",
          "frequency": "Real-time",
          "volume": "High"
        },
        {
          "id": 22,
          "name": "Transaction Request Status",
          "description": "Retrieve and update status of payment transaction requests",
          "frequency": "Real-time",
          "volume": "High"
        },
        {
          "id": 23,
          "name": "Refund Processing",
          "description": "Initiate refund transaction requests for previously completed payments",
          "frequency": "On-demand",
          "volume": "Medium"
        },
        {
          "id": 24,
          "name": "Direct Debit Management",
          "description": "Create and manage direct debit instructions for recurring payments",
          "frequency": "On-demand",
          "volume": "Medium"
        },
        {
          "id": 25,
          "name": "Standing Order Management",
          "description": "Create and manage standing orders for scheduled recurring payments",
          "frequency": "On-demand",
          "volume": "Medium"
        },
        {
          "id": 26,
          "name": "Counterparty Limit Management",
          "description": "Set and manage payment limits for specific counterparties",
          "frequency": "On-demand",
          "volume": "Low"
        }
      ]
    },
    {
      "category": "Customer Management",
      "capabilities": [
        {
          "id": 27,
          "name": "Customer Creation",
          "description": "Create new customer records with personal and contact information",
          "frequency": "On-demand",
          "volume": "Medium"
        },
        {
          "id": 28,
          "name": "Customer Information Retrieval",
          "description": "Retrieve customer details including demographics and linked accounts",
          "frequency": "Real-time",
          "volume": "High"
        },
        {
          "id": 29,
          "name": "Customer Search",
          "description": "Search for customers by various criteria including phone number, legal name, and attributes",
          "frequency": "Real-time",
          "volume": "Medium"
        },
        {
          "id": 30,
          "name": "Customer Attribute Management",
          "description": "Create, update, and retrieve custom attributes associated with customers",
          "frequency": "On-demand",
          "volume": "Medium"
        },
        {
          "id": 31,
          "name": "Customer Message Management",
          "description": "Create and retrieve messages for customer communication",
          "frequency": "On-demand",
          "volume": "Medium"
        },
        {
          "id": 32,
          "name": "Agent Management",
          "description": "Create and manage agent records for customer service representatives",
          "frequency": "On-demand",
          "volume": "Low"
        }
      ]
    },
    {
      "category": "User Management",
      "capabilities": [
        {
          "id": 33,
          "name": "User Information Retrieval",
          "description": "Retrieve user profile information by ID, username, or email",
          "frequency": "Real-time",
          "volume": "High"
        },
        {
          "id": 34,
          "name": "User Creation",
          "description": "Create new user accounts with specified roles and permissions",
          "frequency": "On-demand",
          "volume": "Medium"
        },
        {
          "id": 35,
          "name": "User Invitation",
          "description": "Send invitations to new users to join the platform",
          "frequency": "On-demand",
          "volume": "Low"
        },
        {
          "id": 36,
          "name": "User Lock Management",
          "description": "Lock and unlock user accounts for security purposes",
          "frequency": "On-demand",
          "volume": "Low"
        },
        {
          "id": 37,
          "name": "User Deletion",
          "description": "Delete user accounts and associated data",
          "frequency": "On-demand",
          "volume": "Low"
        },
        {
          "id": 38,
          "name": "Password Reset",
          "description": "Generate password reset URLs for user account recovery",
          "frequency": "On-demand",
          "volume": "Medium"
        },
        {
          "id": 39,
          "name": "User Attribute Management",
          "description": "Create, update, and retrieve personal attributes associated with users",
          "frequency": "On-demand",
          "volume": "Medium"
        },
        {
          "id": 40,
          "name": "User-Customer Link Management",
          "description": "Create and manage links between users and customer records",
          "frequency": "On-demand",
          "volume": "Medium"
        }
      ]
    },
    {
      "category": "Authorization and Access Control",
      "capabilities": [
        {
          "id": 41,
          "name": "View Management",
          "description": "Create, update, and delete custom views that control data visibility",
          "frequency": "On-demand",
          "volume": "Medium"
        },
        {
          "id": 42,
          "name": "Account Access Grant",
          "description": "Grant users access to specific accounts through views",
          "frequency": "On-demand",
          "volume": "Medium"
        },
        {
          "id": 43,
          "name": "Account Access Revocation",
          "description": "Revoke user access to accounts and views",
          "frequency": "On-demand",
          "volume": "Medium"
        },
        {
          "id": 44,
          "name": "Entitlement Management",
          "description": "Retrieve and manage role-based entitlements for users",
          "frequency": "On-demand",
          "volume": "Medium"
        },
        {
          "id": 45,
          "name": "Scope Management",
          "description": "Create and manage OAuth scopes for API access control",
          "frequency": "On-demand",
          "volume": "Low"
        },
        {
          "id": 46,
          "name": "System View Permission Management",
          "description": "Add and remove permissions on system-level views",
          "frequency": "On-demand",
          "volume": "Low"
        }
      ]
    },
    {
      "category": "Consent Management (PSD2)",
      "capabilities": [
        {
          "id": 47,
          "name": "Consent Creation",
          "description": "Create consent records for third-party access to account information",
          "frequency": "Real-time",
          "volume": "High"
        },
        {
          "id": 48,
          "name": "Consent Status Management",
          "description": "Update and retrieve consent status throughout its lifecycle",
          "frequency": "Real-time",
          "volume": "High"
        },
        {
          "id": 49,
          "name": "Consent Revocation",
          "description": "Revoke active consents to terminate third-party access",
          "frequency": "On-demand",
          "volume": "Medium"
        },
        {
          "id": 50,
          "name": "Consent Information Retrieval",
          "description": "Retrieve detailed information about consents including access rights",
          "frequency": "Real-time",
          "volume": "High"
        },
        {
          "id": 51,
          "name": "Consent Authorization",
          "description": "Handle Strong Customer Authentication for consent authorization",
          "frequency": "Real-time",
          "volume": "High"
        },
        {
          "id": 52,
          "name": "VRP Consent Request",
          "description": "Create Variable Recurring Payment consent requests",
          "frequency": "On-demand",
          "volume": "Medium"
        }
      ]
    },
    {
      "category": "Consumer and Application Management",
      "capabilities": [
        {
          "id": 53,
          "name": "Consumer Registration",
          "description": "Register new API consumers (third-party applications) with credentials",
          "frequency": "On-demand",
          "volume": "Low"
        },
        {
          "id": 54,
          "name": "Consumer Information Retrieval",
          "description": "Retrieve information about registered API consumers",
          "frequency": "Real-time",
          "volume": "Medium"
        },
        {
          "id": 55,
          "name": "Consumer Configuration Update",
          "description": "Update consumer settings including redirect URLs, logos, and certificates",
          "frequency": "On-demand",
          "volume": "Low"
        },
        {
          "id": 56,
          "name": "Rate Limit Management",
          "description": "Set and manage API call rate limits per consumer",
          "frequency": "On-demand",
          "volume": "Low"
        },
        {
          "id": 57,
          "name": "Dynamic Consumer Registration",
          "description": "Support OAuth 2.0 Dynamic Client Registration for automated consumer onboarding",
          "frequency": "On-demand",
          "volume": "Low"
        }
      ]
    },
    {
      "category": "Counterparty Management",
      "capabilities": [
        {
          "id": 58,
          "name": "Counterparty Creation",
          "description": "Create explicit counterparty records for payment beneficiaries",
          "frequency": "On-demand",
          "volume": "Medium"
        },
        {
          "id": 59,
          "name": "Counterparty Retrieval",
          "description": "Retrieve counterparty information by ID or name",
          "frequency": "Real-time",
          "volume": "Medium"
        },
        {
          "id": 60,
          "name": "Counterparty Deletion",
          "description": "Delete counterparty records from the system",
          "frequency": "On-demand",
          "volume": "Low"
        }
      ]
    },
    {
      "category": "Product Management",
      "capabilities": [
        {
          "id": 61,
          "name": "Product Catalog Management",
          "description": "Create, update, and retrieve banking product definitions",
          "frequency": "On-demand",
          "volume": "Low"
        },
        {
          "id": 62,
          "name": "Product Attribute Management",
          "description": "Manage custom attributes associated with banking products",
          "frequency": "On-demand",
          "volume": "Low"
        },
        {
          "id": 63,
          "name": "Product Fee Management",
          "description": "Create, update, and retrieve fee structures for banking products",
          "frequency": "On-demand",
          "volume": "Low"
        }
      ]
    },
    {
      "category": "Branch and ATM Management",
      "capabilities": [
        {
          "id": 64,
          "name": "Branch Information Retrieval",
          "description": "Retrieve information about bank branches including location and services",
          "frequency": "Real-time",
          "volume": "Medium"
        },
        {
          "id": 65,
          "name": "ATM Management",
          "description": "Create, update, and retrieve ATM information including location and capabilities",
          "frequency": "On-demand",
          "volume": "Medium"
        },
        {
          "id": 66,
          "name": "ATM Attribute Management",
          "description": "Manage ATM attributes including supported currencies, languages, and services",
          "frequency": "On-demand",
          "volume": "Low"
        }
      ]
    },
    {
      "category": "KYC and Compliance",
      "capabilities": [
        {
          "id": 67,
          "name": "KYC Document Management",
          "description": "Store and retrieve Know Your Customer documentation",
          "frequency": "On-demand",
          "volume": "Medium"
        },
        {
          "id": 68,
          "name": "KYC Status Management",
          "description": "Track and update KYC verification status for customers",
          "frequency": "On-demand",
          "volume": "Medium"
        },
        {
          "id": 69,
          "name": "Regulated Entity Management",
          "description": "Create and manage regulated entity records for compliance tracking",
          "frequency": "On-demand",
          "volume": "Low"
        },
        {
          "id": 70,
          "name": "Tax Residence Management",
          "description": "Manage customer tax residence information for regulatory compliance",
          "frequency": "On-demand",
          "volume": "Low"
        }
      ]
    },
    {
      "category": "Dynamic Configuration",
      "capabilities": [
        {
          "id": 71,
          "name": "Dynamic Entity Management",
          "description": "Create and manage runtime-defined data models without code deployment",
          "frequency": "On-demand",
          "volume": "Low"
        },
        {
          "id": 72,
          "name": "Dynamic Endpoint Management",
          "description": "Create and manage runtime-defined API endpoints with custom logic",
          "frequency": "On-demand",
          "volume": "Low"
        },
        {
          "id": 73,
          "name": "Connector Method Management",
          "description": "Create and manage custom connector methods for backend integration",
          "frequency": "On-demand",
          "volume": "Low"
        },
        {
          "id": 74,
          "name": "Endpoint Mapping Management",
          "description": "Configure endpoint mappings for request/response transformation",
          "frequency": "On-demand",
          "volume": "Low"
        },
        {
          "id": 75,
          "name": "Method Routing Configuration",
          "description": "Configure dynamic routing of connector methods based on bank or criteria",
          "frequency": "On-demand",
          "volume": "Low"
        },
        {
          "id": 76,
          "name": "Dynamic Resource Documentation",
          "description": "Create and manage runtime API documentation for dynamic endpoints",
          "frequency": "On-demand",
          "volume": "Low"
        }
      ]
    },
    {
      "category": "API Documentation and Discovery",
      "capabilities": [
        {
          "id": 77,
          "name": "API Information Retrieval",
          "description": "Retrieve API version information and platform details",
          "frequency": "Real-time",
          "volume": "Medium"
        },
        {
          "id": 78,
          "name": "API Collection Management",
          "description": "Create and manage collections of API endpoints for organization",
          "frequency": "On-demand",
          "volume": "Low"
        },
        {
          "id": 79,
          "name": "API Tag Retrieval",
          "description": "Retrieve available API tags for endpoint categorization",
          "frequency": "Real-time",
          "volume": "Low"
        },
        {
          "id": 80,
          "name": "Endpoint Tag Management",
          "description": "Create and manage tags for API endpoints at system and bank levels",
          "frequency": "On-demand",
          "volume": "Low"
        },
        {
          "id": 81,
          "name": "OAuth2 Well-Known URIs",
          "description": "Provide OAuth2 server discovery information via well-known endpoints",
          "frequency": "Real-time",
          "volume": "Medium"
        }
      ]
    },
    {
      "category": "Validation and Schema Management",
      "capabilities": [
        {
          "id": 82,
          "name": "JSON Schema Validation",
          "description": "Create and manage JSON schema validations for API requests",
          "frequency": "On-demand",
          "volume": "Low"
        },
        {
          "id": 83,
          "name": "Authentication Type Validation",
          "description": "Configure and validate authentication type requirements for endpoints",
          "frequency": "On-demand",
          "volume": "Low"
        },
        {
          "id": 84,
          "name": "Attribute Definition Management",
          "description": "Define and manage attribute schemas for various entity types",
          "frequency": "On-demand",
          "volume": "Low"
        }
      ]
    },
    {
      "category": "Webhook and Notification",
      "capabilities": [
        {
          "id": 85,
          "name": "Account Webhook Management",
          "description": "Create and manage webhooks for account-related events",
          "frequency": "On-demand",
          "volume": "Low"
        },
        {
          "id": 86,
          "name": "System Notification Webhook",
          "description": "Configure system-level notification webhooks for event broadcasting",
          "frequency": "On-demand",
          "volume": "Low"
        },
        {
          "id": 87,
          "name": "Bank Account Notification Webhook",
          "description": "Configure bank-specific notification webhooks for account events",
          "frequency": "On-demand",
          "volume": "Low"
        }
      ]
    },
    {
      "category": "Metrics and Monitoring",
      "capabilities": [
        {
          "id": 88,
          "name": "API Metrics Retrieval",
          "description": "Retrieve API usage metrics including call counts and response times",
          "frequency": "Real-time",
          "volume": "Medium"
        },
        {
          "id": 89,
          "name": "Aggregate Metrics",
          "description": "Retrieve aggregated metrics for API performance analysis",
          "frequency": "On-demand",
          "volume": "Medium"
        },
        {
          "id": 90,
          "name": "Database Information",
          "description": "Retrieve database connection and configuration information",
          "frequency": "On-demand",
          "volume": "Low"
        },
        {
          "id": 91,
          "name": "Log Cache Access",
          "description": "Retrieve cached log entries for debugging and monitoring",
          "frequency": "On-demand",
          "volume": "Low"
        }
      ]
    },
    {
      "category": "Data Integrity and Maintenance",
      "capabilities": [
        {
          "id": 92,
          "name": "Cascade Deletion",
          "description": "Delete entities with all related data including transactions, accounts, and customers",
          "frequency": "On-demand",
          "volume": "Low"
        },
        {
          "id": 93,
          "name": "Data Integrity Checks",
          "description": "Perform integrity checks on views, account access, and currency configurations",
          "frequency": "On-demand",
          "volume": "Low"
        },
        {
          "id": 94,
          "name": "Orphaned Account Detection",
          "description": "Identify and report accounts without proper ownership or access",
          "frequency": "On-demand",
          "volume": "Low"
        }
      ]
    },
    {
      "category": "Berlin Group PSD2 Compliance",
      "capabilities": [
        {
          "id": 95,
          "name": "Account Information Service (AIS)",
          "description": "PSD2-compliant account information access including accounts, balances, and transactions",
          "frequency": "Real-time",
          "volume": "High"
        },
        {
          "id": 96,
          "name": "Payment Initiation Service (PIS)",
          "description": "PSD2-compliant payment initiation including SEPA and domestic payments",
          "frequency": "Real-time",
          "volume": "High"
        },
        {
          "id": 97,
          "name": "Confirmation of Funds (PIIS)",
          "description": "PSD2-compliant funds availability confirmation for card payments",
          "frequency": "Real-time",
          "volume": "Medium"
        },
        {
          "id": 98,
          "name": "Signing Baskets",
          "description": "Authorize multiple transactions with single Strong Customer Authentication",
          "frequency": "On-demand",
          "volume": "Medium"
        },
        {
          "id": 99,
          "name": "Card Account Information",
          "description": "Retrieve card account details, balances, and transaction history",
          "frequency": "Real-time",
          "volume": "Medium"
        }
      ]
    },
    {
      "category": "Background Processing",
      "capabilities": [
        {
          "id": 100,
          "name": "Consent Expiration Processing",
          "description": "Automatically expire consents that have passed their validity date",
          "frequency": "Scheduled (Periodic)",
          "volume": "Medium"
        },
        {
          "id": 101,
          "name": "Unfinished Consent Cleanup",
          "description": "Reject consents that remain in received status beyond timeout threshold",
          "frequency": "Scheduled (Periodic)",
          "volume": "Medium"
        },
        {
          "id": 102,
          "name": "Metrics Archival",
          "description": "Archive old API metrics data to maintain database performance",
          "frequency": "Scheduled (Daily)",
          "volume": "High"
        },
        {
          "id": 103,
          "name": "Database Cleanup",
          "description": "Clean up outdated records and maintain database health",
          "frequency": "Scheduled (Periodic)",
          "volume": "Medium"
        },
        {
          "id": 104,
          "name": "Transaction Processing",
          "description": "Process pending transactions and update statuses",
          "frequency": "Scheduled (Periodic)",
          "volume": "High"
        }
      ]
    },
    {
      "category": "Foreign Exchange",
      "capabilities": [
        {
          "id": 105,
          "name": "FX Rate Management",
          "description": "Create and manage foreign exchange rates for currency conversion",
          "frequency": "On-demand",
          "volume": "Low"
        },
        {
          "id": 106,
          "name": "Currency Information",
          "description": "Retrieve available currencies and exchange rate information",
          "frequency": "Real-time",
          "volume": "Medium"
        }
      ]
    },
    {
      "category": "Card Management",
      "capabilities": [
        {
          "id": 107,
          "name": "Physical Card Creation",
          "description": "Create physical card records with associated account and customer information",
          "frequency": "On-demand",
          "volume": "Medium"
        },
        {
          "id": 108,
          "name": "Card Information Update",
          "description": "Update card details including status and attributes",
          "frequency": "On-demand",
          "volume": "Medium"
        },
        {
          "id": 109,
          "name": "Card Attribute Management",
          "description": "Manage custom attributes associated with cards",
          "frequency": "On-demand",
          "volume": "Low"
        }
      ]
    },
    {
      "category": "Web UI Configuration",
      "capabilities": [
        {
          "id": 110,
          "name": "Web UI Properties Management",
          "description": "Configure and retrieve web UI customization properties",
          "frequency": "On-demand",
          "volume": "Low"
        },
        {
          "id": 111,
          "name": "Session Timeout Configuration",
          "description": "Retrieve suggested session timeout settings for user interfaces",
          "frequency": "Real-time",
          "volume": "Low"
        }
      ]
    }
  ]
}
```

### Capability Summary
- **Total Capabilities Identified**: 111
- **API Endpoints**: ~400+ (across all API versions v1.2.1 through v6.0.0)
- **Background Tasks**: 5 (Consent expiration, Consent cleanup, Metrics archival, Database cleanup, Transaction processing)
- **Event Consumers**: Akka actor-based event processing for webhooks and notifications
- **Batch Jobs**: Metrics archival and database maintenance jobs
- **External Integrations**: Multiple backend connectors (REST, Akka, Stored Procedures, Kafka, RabbitMQ, Cardano, Ethereum)
- **Primary Business Functions**: Account Management, Payment Initiation, Customer Management, Consent Management, Authorization, PSD2 Compliance, Dynamic Configuration

---

## Open Questions & Clarifications Needed

1. **Transaction Volume Estimates**: What are the expected daily transaction volumes for payment initiation and account information requests?

2. **Consent Retention Policy**: How long should expired consents be retained in the system before archival or deletion?

3. **Multi-Bank Deployment**: Is the system deployed as a single instance serving multiple banks, or are there separate instances per bank?

4. **Blockchain Integration Usage**: What is the current usage level of the Cardano and Ethereum connectors, and are they production-ready?

5. **Rate Limiting Thresholds**: What are the default rate limiting thresholds for API consumers, and how are they determined?

6. **Webhook Retry Policy**: What is the retry policy for failed webhook deliveries, and how are permanent failures handled?

7. **Metrics Retention Period**: What is the required retention period for API metrics before archival?

8. **Dynamic Entity Governance**: What governance processes exist for creating and managing dynamic entities and endpoints?

These questions should be resolved during SME review to ensure complete understanding of the system's operational requirements.
