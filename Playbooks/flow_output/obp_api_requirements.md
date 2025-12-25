# High-Level Requirements Document: Open Bank Project API

## 1. System Overview & Purpose

### System Identification
- **System Name**: Open Bank Project (OBP) API
- **Application Code/ID**: OBP-API
- **Business Domain**: Financial Services / Banking API Platform

### Business Purpose
The Open Bank Project API is an open-source RESTful API platform that abstracts core banking system complexities to enable financial applications to interact with multiple banks on behalf of account holders. The platform's mission is "Bank as a Platform, Transparency as an Asset." It provides a standardized interface for third-party developers, banks, and payment service providers to access banking services without needing to understand each bank's proprietary systems. The system enables regulatory compliance with PSD2, Open Banking, and other financial regulations while supporting innovation through sandbox environments and dynamic configuration capabilities.

### System Criticality
- **Criticality Level**: High
- **Business Impact if Unavailable**: Complete disruption of third-party financial application integrations, inability to process payment initiations, loss of account information access for authorized applications, and potential regulatory non-compliance for banks relying on the platform for PSD2/Open Banking requirements.

### System Type
- **Architecture**: REST API Platform with Multi-Protocol Backend Integration
- **Processing Model**: Mixed - HTTP request-response for real-time operations, scheduled background tasks for consent management and metrics archiving, event-driven processing for transaction notifications

### Key Stakeholders
- Third-Party Developers building financial applications (fintech apps, personal finance managers, accounting software)
- Banks seeking to expose their services via standardized APIs
- Payment Service Providers (PSPs) requiring PSD2-compliant APIs for Account Information (AIS) and Payment Initiation (PIS) services
- Regulators and Auditors needing transparent access to financial data
- Internal Operations teams managing API configuration and monitoring

---

## 2. Core Capabilities Inventory

```json
{
  "coreCapabilities": [
    {
      "category": "Bank Management",
      "capabilities": [
        {
          "id": 1,
          "name": "Bank Information Retrieval",
          "description": "Retrieve information about banks supported on the platform including identifiers, names, logos, and websites.",
          "frequency": "Real-time",
          "volume": "High"
        },
        {
          "id": 2,
          "name": "Bank Creation and Configuration",
          "description": "Create and configure new banks on the platform with associated attributes and settings.",
          "frequency": "On-demand",
          "volume": "Low"
        },
        {
          "id": 3,
          "name": "Bank Attribute Management",
          "description": "Manage custom attributes associated with banks for extended configuration and metadata.",
          "frequency": "On-demand",
          "volume": "Low"
        },
        {
          "id": 4,
          "name": "Settlement Account Management",
          "description": "Create and manage settlement accounts for banks to handle double-entry transaction processing.",
          "frequency": "On-demand",
          "volume": "Low"
        }
      ]
    },
    {
      "category": "Account Management",
      "capabilities": [
        {
          "id": 5,
          "name": "Account Listing",
          "description": "Retrieve lists of accounts accessible to users at specific banks with filtering and pagination.",
          "frequency": "Real-time",
          "volume": "High"
        },
        {
          "id": 6,
          "name": "Account Details Retrieval",
          "description": "Retrieve detailed information about specific accounts including balances, metadata, and routing information.",
          "frequency": "Real-time",
          "volume": "High"
        },
        {
          "id": 7,
          "name": "Account Creation",
          "description": "Create new bank accounts with specified attributes, currencies, and initial balances.",
          "frequency": "On-demand",
          "volume": "Medium"
        },
        {
          "id": 8,
          "name": "Account Balance Retrieval",
          "description": "Retrieve current and historical balance information for accounts.",
          "frequency": "Real-time",
          "volume": "High"
        },
        {
          "id": 9,
          "name": "Account Attribute Management",
          "description": "Manage custom attributes associated with accounts for extended metadata and configuration.",
          "frequency": "On-demand",
          "volume": "Medium"
        },
        {
          "id": 10,
          "name": "Account Routing Lookup",
          "description": "Find accounts by routing information such as IBAN, account number, or other routing schemes.",
          "frequency": "Real-time",
          "volume": "High"
        },
        {
          "id": 11,
          "name": "Account Label Management",
          "description": "Update and manage account labels and display names.",
          "frequency": "On-demand",
          "volume": "Low"
        },
        {
          "id": 12,
          "name": "IBAN Validation",
          "description": "Validate and check IBAN numbers for correctness and format compliance.",
          "frequency": "Real-time",
          "volume": "Medium"
        }
      ]
    },
    {
      "category": "Transaction Management",
      "capabilities": [
        {
          "id": 13,
          "name": "Transaction History Retrieval",
          "description": "Retrieve transaction history for accounts with filtering by date, amount, and other criteria.",
          "frequency": "Real-time",
          "volume": "High"
        },
        {
          "id": 14,
          "name": "Transaction Details Retrieval",
          "description": "Retrieve detailed information about specific transactions including metadata and counterparty information.",
          "frequency": "Real-time",
          "volume": "High"
        },
        {
          "id": 15,
          "name": "Double-Entry Transaction Retrieval",
          "description": "Retrieve double-entry book transaction information showing debit and credit sides of transactions.",
          "frequency": "Real-time",
          "volume": "Medium"
        },
        {
          "id": 16,
          "name": "Transaction Attribute Management",
          "description": "Manage custom attributes associated with transactions for extended metadata.",
          "frequency": "On-demand",
          "volume": "Medium"
        },
        {
          "id": 17,
          "name": "Transaction Tagging",
          "description": "Add, retrieve, and delete tags associated with transactions for categorization.",
          "frequency": "On-demand",
          "volume": "Medium"
        },
        {
          "id": 18,
          "name": "Historical Transaction Creation",
          "description": "Create historical transactions for data migration and reconciliation purposes.",
          "frequency": "On-demand",
          "volume": "Low"
        }
      ]
    },
    {
      "category": "Payment Initiation",
      "capabilities": [
        {
          "id": 19,
          "name": "SEPA Credit Transfer Initiation",
          "description": "Initiate SEPA credit transfers to counterparties using IBAN routing.",
          "frequency": "Real-time",
          "volume": "High"
        },
        {
          "id": 20,
          "name": "Account-to-Account Transfer",
          "description": "Initiate transfers between accounts within the platform using bank and account identifiers.",
          "frequency": "Real-time",
          "volume": "High"
        },
        {
          "id": 21,
          "name": "Counterparty Payment",
          "description": "Initiate payments to registered counterparties using counterparty identifiers.",
          "frequency": "Real-time",
          "volume": "High"
        },
        {
          "id": 22,
          "name": "Simple Payment Transfer",
          "description": "Initiate simple transfers using bank account numbers or IBANs directly.",
          "frequency": "Real-time",
          "volume": "High"
        },
        {
          "id": 23,
          "name": "Refund Processing",
          "description": "Process refund requests for previous transactions.",
          "frequency": "On-demand",
          "volume": "Medium"
        },
        {
          "id": 24,
          "name": "Free-Form Payment",
          "description": "Initiate payments with flexible parameters for custom payment scenarios.",
          "frequency": "On-demand",
          "volume": "Low"
        },
        {
          "id": 25,
          "name": "Agent Cash Withdrawal",
          "description": "Process cash withdrawal requests through authorized agents.",
          "frequency": "On-demand",
          "volume": "Medium"
        },
        {
          "id": 26,
          "name": "Card Payment Processing",
          "description": "Process card-based payment transactions.",
          "frequency": "Real-time",
          "volume": "High"
        },
        {
          "id": 27,
          "name": "Transaction Request Status Tracking",
          "description": "Track the status of payment initiation requests through their lifecycle.",
          "frequency": "Real-time",
          "volume": "High"
        },
        {
          "id": 28,
          "name": "Payment Challenge Management",
          "description": "Create and validate challenges for Strong Customer Authentication during payment processing.",
          "frequency": "Real-time",
          "volume": "High"
        },
        {
          "id": 29,
          "name": "Payment Cancellation",
          "description": "Cancel pending payment requests before execution.",
          "frequency": "On-demand",
          "volume": "Medium"
        },
        {
          "id": 30,
          "name": "Periodic Payment Setup",
          "description": "Configure recurring periodic payments with specified schedules.",
          "frequency": "On-demand",
          "volume": "Medium"
        },
        {
          "id": 31,
          "name": "Bulk Payment Processing",
          "description": "Process multiple payments in a single batch operation.",
          "frequency": "On-demand",
          "volume": "Medium"
        }
      ]
    },
    {
      "category": "Customer Management",
      "capabilities": [
        {
          "id": 32,
          "name": "Customer Information Retrieval",
          "description": "Retrieve customer profile information including personal details and contact information.",
          "frequency": "Real-time",
          "volume": "High"
        },
        {
          "id": 33,
          "name": "Customer Creation",
          "description": "Create new customer records with associated profile information.",
          "frequency": "On-demand",
          "volume": "Medium"
        },
        {
          "id": 34,
          "name": "Customer Attribute Management",
          "description": "Manage custom attributes associated with customers for extended profile data.",
          "frequency": "On-demand",
          "volume": "Medium"
        },
        {
          "id": 35,
          "name": "Customer Search",
          "description": "Search for customers by various criteria including phone number, legal name, and attributes.",
          "frequency": "Real-time",
          "volume": "Medium"
        },
        {
          "id": 36,
          "name": "Customer Address Management",
          "description": "Manage customer address information including multiple addresses.",
          "frequency": "On-demand",
          "volume": "Low"
        },
        {
          "id": 37,
          "name": "Customer-Account Linking",
          "description": "Manage relationships between customers and their associated accounts.",
          "frequency": "On-demand",
          "volume": "Medium"
        },
        {
          "id": 38,
          "name": "Customer Messaging",
          "description": "Send and retrieve messages to and from customers.",
          "frequency": "On-demand",
          "volume": "Medium"
        }
      ]
    },
    {
      "category": "KYC and Compliance",
      "capabilities": [
        {
          "id": 39,
          "name": "KYC Status Management",
          "description": "Manage Know Your Customer status information for customers.",
          "frequency": "On-demand",
          "volume": "Medium"
        },
        {
          "id": 40,
          "name": "KYC Document Management",
          "description": "Upload, retrieve, and manage KYC documents for customer verification.",
          "frequency": "On-demand",
          "volume": "Medium"
        },
        {
          "id": 41,
          "name": "KYC Check Management",
          "description": "Record and retrieve KYC check results and verification status.",
          "frequency": "On-demand",
          "volume": "Medium"
        },
        {
          "id": 42,
          "name": "KYC Media Management",
          "description": "Manage media files associated with KYC processes.",
          "frequency": "On-demand",
          "volume": "Low"
        },
        {
          "id": 43,
          "name": "Tax Residence Management",
          "description": "Manage customer tax residence information for regulatory compliance.",
          "frequency": "On-demand",
          "volume": "Low"
        }
      ]
    },
    {
      "category": "User and Access Management",
      "capabilities": [
        {
          "id": 44,
          "name": "User Authentication",
          "description": "Authenticate users through various methods including OAuth, OpenID Connect, and Direct Login.",
          "frequency": "Real-time",
          "volume": "High"
        },
        {
          "id": 45,
          "name": "User Information Retrieval",
          "description": "Retrieve user profile information and current user context.",
          "frequency": "Real-time",
          "volume": "High"
        },
        {
          "id": 46,
          "name": "User Creation and Management",
          "description": "Create and manage user accounts with associated roles and permissions.",
          "frequency": "On-demand",
          "volume": "Medium"
        },
        {
          "id": 47,
          "name": "User Invitation",
          "description": "Send invitations to new users to join the platform.",
          "frequency": "On-demand",
          "volume": "Low"
        },
        {
          "id": 48,
          "name": "User Lock Management",
          "description": "Lock and unlock user accounts for security purposes.",
          "frequency": "On-demand",
          "volume": "Low"
        },
        {
          "id": 49,
          "name": "Password Reset",
          "description": "Generate password reset links for users.",
          "frequency": "On-demand",
          "volume": "Low"
        },
        {
          "id": 50,
          "name": "User Attribute Management",
          "description": "Manage personal and non-personal attributes associated with users.",
          "frequency": "On-demand",
          "volume": "Medium"
        },
        {
          "id": 51,
          "name": "User-Customer Linking",
          "description": "Manage relationships between users and customer records.",
          "frequency": "On-demand",
          "volume": "Medium"
        }
      ]
    },
    {
      "category": "Authorization and Entitlements",
      "capabilities": [
        {
          "id": 52,
          "name": "Entitlement Management",
          "description": "Grant, revoke, and retrieve role-based entitlements for users.",
          "frequency": "On-demand",
          "volume": "Medium"
        },
        {
          "id": 53,
          "name": "Scope Management",
          "description": "Manage OAuth scopes for API access control.",
          "frequency": "On-demand",
          "volume": "Low"
        },
        {
          "id": 54,
          "name": "View-Based Access Control",
          "description": "Manage view-based permissions controlling field-level visibility and action authorization.",
          "frequency": "On-demand",
          "volume": "Medium"
        },
        {
          "id": 55,
          "name": "Account Access Grant Management",
          "description": "Grant and revoke user access to specific accounts through views.",
          "frequency": "On-demand",
          "volume": "Medium"
        },
        {
          "id": 56,
          "name": "Custom View Management",
          "description": "Create, update, and delete custom views for fine-grained access control.",
          "frequency": "On-demand",
          "volume": "Low"
        },
        {
          "id": 57,
          "name": "System View Permission Management",
          "description": "Manage permissions on system-defined views.",
          "frequency": "On-demand",
          "volume": "Low"
        }
      ]
    },
    {
      "category": "Consent Management",
      "capabilities": [
        {
          "id": 58,
          "name": "Consent Creation",
          "description": "Create consent records granting time-limited access to accounts and services.",
          "frequency": "Real-time",
          "volume": "High"
        },
        {
          "id": 59,
          "name": "Consent Status Management",
          "description": "Update and track consent status through its lifecycle.",
          "frequency": "Real-time",
          "volume": "High"
        },
        {
          "id": 60,
          "name": "Consent Retrieval",
          "description": "Retrieve consent information and associated permissions.",
          "frequency": "Real-time",
          "volume": "High"
        },
        {
          "id": 61,
          "name": "Consent Revocation",
          "description": "Revoke active consents to terminate access permissions.",
          "frequency": "On-demand",
          "volume": "Medium"
        },
        {
          "id": 62,
          "name": "Consent Authorisation",
          "description": "Manage Strong Customer Authentication flows for consent authorization.",
          "frequency": "Real-time",
          "volume": "High"
        },
        {
          "id": 63,
          "name": "VRP Consent Management",
          "description": "Create and manage Variable Recurring Payment consents.",
          "frequency": "On-demand",
          "volume": "Medium"
        },
        {
          "id": 64,
          "name": "Consent Expiration Processing",
          "description": "Automatically process and update expired consents.",
          "frequency": "Scheduled",
          "volume": "Medium"
        }
      ]
    },
    {
      "category": "Card Management",
      "capabilities": [
        {
          "id": 65,
          "name": "Physical Card Creation",
          "description": "Create physical card records with associated details and attributes.",
          "frequency": "On-demand",
          "volume": "Medium"
        },
        {
          "id": 66,
          "name": "Card Information Retrieval",
          "description": "Retrieve card information including card accounts, balances, and transaction history.",
          "frequency": "Real-time",
          "volume": "High"
        },
        {
          "id": 67,
          "name": "Card Attribute Management",
          "description": "Manage custom attributes associated with cards.",
          "frequency": "On-demand",
          "volume": "Low"
        },
        {
          "id": 68,
          "name": "Card Update",
          "description": "Update card details and status information.",
          "frequency": "On-demand",
          "volume": "Medium"
        }
      ]
    },
    {
      "category": "Counterparty Management",
      "capabilities": [
        {
          "id": 69,
          "name": "Counterparty Creation",
          "description": "Create counterparty records with routing information for payment processing.",
          "frequency": "On-demand",
          "volume": "Medium"
        },
        {
          "id": 70,
          "name": "Counterparty Retrieval",
          "description": "Retrieve counterparty information and associated metadata.",
          "frequency": "Real-time",
          "volume": "High"
        },
        {
          "id": 71,
          "name": "Counterparty Limit Management",
          "description": "Set and manage payment limits for counterparties.",
          "frequency": "On-demand",
          "volume": "Low"
        },
        {
          "id": 72,
          "name": "Counterparty Metadata Management",
          "description": "Manage metadata associated with counterparties including images and descriptions.",
          "frequency": "On-demand",
          "volume": "Low"
        }
      ]
    },
    {
      "category": "Product Management",
      "capabilities": [
        {
          "id": 73,
          "name": "Product Catalog Retrieval",
          "description": "Retrieve available banking products offered by banks.",
          "frequency": "Real-time",
          "volume": "Medium"
        },
        {
          "id": 74,
          "name": "Product Creation and Update",
          "description": "Create and update banking product definitions.",
          "frequency": "On-demand",
          "volume": "Low"
        },
        {
          "id": 75,
          "name": "Product Attribute Management",
          "description": "Manage custom attributes associated with products.",
          "frequency": "On-demand",
          "volume": "Low"
        },
        {
          "id": 76,
          "name": "Product Fee Management",
          "description": "Configure and manage fees associated with products.",
          "frequency": "On-demand",
          "volume": "Low"
        },
        {
          "id": 77,
          "name": "Product Collection Management",
          "description": "Organize products into collections for categorization.",
          "frequency": "On-demand",
          "volume": "Low"
        }
      ]
    },
    {
      "category": "Branch and ATM Management",
      "capabilities": [
        {
          "id": 78,
          "name": "Branch Information Retrieval",
          "description": "Retrieve branch location and service information.",
          "frequency": "Real-time",
          "volume": "Medium"
        },
        {
          "id": 79,
          "name": "ATM Information Retrieval",
          "description": "Retrieve ATM location, services, and availability information.",
          "frequency": "Real-time",
          "volume": "Medium"
        },
        {
          "id": 80,
          "name": "ATM Creation and Update",
          "description": "Create and update ATM records with location and service details.",
          "frequency": "On-demand",
          "volume": "Low"
        },
        {
          "id": 81,
          "name": "ATM Attribute Management",
          "description": "Manage ATM attributes including supported currencies, languages, and accessibility features.",
          "frequency": "On-demand",
          "volume": "Low"
        }
      ]
    },
    {
      "category": "Standing Orders and Direct Debits",
      "capabilities": [
        {
          "id": 82,
          "name": "Standing Order Creation",
          "description": "Create standing orders for recurring payments.",
          "frequency": "On-demand",
          "volume": "Medium"
        },
        {
          "id": 83,
          "name": "Standing Order Management",
          "description": "Update and cancel standing orders.",
          "frequency": "On-demand",
          "volume": "Medium"
        },
        {
          "id": 84,
          "name": "Direct Debit Creation",
          "description": "Create direct debit mandates for automatic payment collection.",
          "frequency": "On-demand",
          "volume": "Medium"
        },
        {
          "id": 85,
          "name": "Direct Debit Management",
          "description": "Update and cancel direct debit mandates.",
          "frequency": "On-demand",
          "volume": "Medium"
        }
      ]
    },
    {
      "category": "API Consumer Management",
      "capabilities": [
        {
          "id": 86,
          "name": "Consumer Registration",
          "description": "Register third-party applications as API consumers with credentials.",
          "frequency": "On-demand",
          "volume": "Low"
        },
        {
          "id": 87,
          "name": "Consumer Information Retrieval",
          "description": "Retrieve consumer application details and configuration.",
          "frequency": "Real-time",
          "volume": "Medium"
        },
        {
          "id": 88,
          "name": "Consumer Configuration Update",
          "description": "Update consumer settings including redirect URLs, certificates, and logos.",
          "frequency": "On-demand",
          "volume": "Low"
        },
        {
          "id": 89,
          "name": "Rate Limit Management",
          "description": "Configure and manage API rate limits for consumers.",
          "frequency": "On-demand",
          "volume": "Low"
        },
        {
          "id": 90,
          "name": "Dynamic Consumer Registration",
          "description": "Support dynamic client registration for OAuth clients.",
          "frequency": "On-demand",
          "volume": "Low"
        }
      ]
    },
    {
      "category": "Dynamic API Configuration",
      "capabilities": [
        {
          "id": 91,
          "name": "Dynamic Entity Management",
          "description": "Create and manage runtime-defined data entities without code deployment.",
          "frequency": "On-demand",
          "volume": "Low"
        },
        {
          "id": 92,
          "name": "Dynamic Endpoint Management",
          "description": "Create and manage runtime-defined API endpoints with custom logic.",
          "frequency": "On-demand",
          "volume": "Low"
        },
        {
          "id": 93,
          "name": "Connector Method Management",
          "description": "Configure and manage connector method implementations.",
          "frequency": "On-demand",
          "volume": "Low"
        },
        {
          "id": 94,
          "name": "Method Routing Configuration",
          "description": "Configure routing rules for connector method calls based on bank and parameters.",
          "frequency": "On-demand",
          "volume": "Low"
        },
        {
          "id": 95,
          "name": "Endpoint Mapping Management",
          "description": "Configure mappings between API endpoints and backend implementations.",
          "frequency": "On-demand",
          "volume": "Low"
        },
        {
          "id": 96,
          "name": "Dynamic Resource Documentation",
          "description": "Create and manage runtime API documentation.",
          "frequency": "On-demand",
          "volume": "Low"
        }
      ]
    },
    {
      "category": "API Documentation and Discovery",
      "capabilities": [
        {
          "id": 97,
          "name": "API Documentation Retrieval",
          "description": "Retrieve comprehensive API documentation including endpoints, parameters, and examples.",
          "frequency": "Real-time",
          "volume": "Medium"
        },
        {
          "id": 98,
          "name": "API Collection Management",
          "description": "Create and manage collections of API endpoints for organization and sharing.",
          "frequency": "On-demand",
          "volume": "Low"
        },
        {
          "id": 99,
          "name": "Glossary Retrieval",
          "description": "Retrieve API glossary terms and definitions.",
          "frequency": "Real-time",
          "volume": "Low"
        },
        {
          "id": 100,
          "name": "API Version Information",
          "description": "Retrieve information about available API versions and their capabilities.",
          "frequency": "Real-time",
          "volume": "Low"
        }
      ]
    },
    {
      "category": "Metrics and Monitoring",
      "capabilities": [
        {
          "id": 101,
          "name": "API Metrics Collection",
          "description": "Collect and store metrics about API usage including response times and error rates.",
          "frequency": "Real-time",
          "volume": "High"
        },
        {
          "id": 102,
          "name": "Metrics Retrieval",
          "description": "Retrieve API usage metrics for analysis and reporting.",
          "frequency": "Real-time",
          "volume": "Medium"
        },
        {
          "id": 103,
          "name": "Aggregate Metrics Retrieval",
          "description": "Retrieve aggregated metrics summaries for dashboard and reporting.",
          "frequency": "Real-time",
          "volume": "Medium"
        },
        {
          "id": 104,
          "name": "Metrics Archival",
          "description": "Archive historical metrics data for long-term storage and compliance.",
          "frequency": "Scheduled",
          "volume": "High"
        },
        {
          "id": 105,
          "name": "System Health Monitoring",
          "description": "Monitor and report system health status including database and service connectivity.",
          "frequency": "Real-time",
          "volume": "Medium"
        }
      ]
    },
    {
      "category": "Validation and Security",
      "capabilities": [
        {
          "id": 106,
          "name": "JSON Schema Validation",
          "description": "Configure and apply JSON schema validation rules for API requests.",
          "frequency": "Real-time",
          "volume": "High"
        },
        {
          "id": 107,
          "name": "Authentication Type Validation",
          "description": "Configure validation rules for authentication methods by endpoint.",
          "frequency": "Real-time",
          "volume": "High"
        },
        {
          "id": 108,
          "name": "Request Signature Verification",
          "description": "Verify digital signatures on API requests for security.",
          "frequency": "Real-time",
          "volume": "High"
        },
        {
          "id": 109,
          "name": "MTLS Certificate Validation",
          "description": "Validate mutual TLS client certificates for secure communication.",
          "frequency": "Real-time",
          "volume": "High"
        },
        {
          "id": 110,
          "name": "Login Attempt Tracking",
          "description": "Track and manage login attempts for security monitoring.",
          "frequency": "Real-time",
          "volume": "High"
        }
      ]
    },
    {
      "category": "Regulatory Compliance - PSD2 Berlin Group",
      "capabilities": [
        {
          "id": 111,
          "name": "Account Information Service (AIS)",
          "description": "Provide PSD2-compliant account information access including account lists, balances, and transactions.",
          "frequency": "Real-time",
          "volume": "High"
        },
        {
          "id": 112,
          "name": "Payment Initiation Service (PIS)",
          "description": "Provide PSD2-compliant payment initiation including SEPA and periodic payments.",
          "frequency": "Real-time",
          "volume": "High"
        },
        {
          "id": 113,
          "name": "Confirmation of Funds Service (PIIS)",
          "description": "Provide PSD2-compliant confirmation of funds availability for card payments.",
          "frequency": "Real-time",
          "volume": "Medium"
        },
        {
          "id": 114,
          "name": "Signing Basket Management",
          "description": "Support authorization of multiple transactions with single SCA process.",
          "frequency": "Real-time",
          "volume": "Medium"
        },
        {
          "id": 115,
          "name": "Strong Customer Authentication (SCA)",
          "description": "Implement multi-factor authentication flows for sensitive operations.",
          "frequency": "Real-time",
          "volume": "High"
        }
      ]
    },
    {
      "category": "Foreign Exchange",
      "capabilities": [
        {
          "id": 116,
          "name": "FX Rate Retrieval",
          "description": "Retrieve foreign exchange rates for currency conversion.",
          "frequency": "Real-time",
          "volume": "Medium"
        },
        {
          "id": 117,
          "name": "FX Rate Management",
          "description": "Create and update foreign exchange rate configurations.",
          "frequency": "On-demand",
          "volume": "Low"
        }
      ]
    },
    {
      "category": "Webhook and Notification",
      "capabilities": [
        {
          "id": 118,
          "name": "Webhook Configuration",
          "description": "Configure webhooks for account and transaction notifications.",
          "frequency": "On-demand",
          "volume": "Low"
        },
        {
          "id": 119,
          "name": "System Notification Webhook",
          "description": "Configure system-level notification webhooks for events.",
          "frequency": "On-demand",
          "volume": "Low"
        },
        {
          "id": 120,
          "name": "Bank Account Notification Webhook",
          "description": "Configure bank-level account notification webhooks.",
          "frequency": "On-demand",
          "volume": "Low"
        }
      ]
    },
    {
      "category": "Agent Management",
      "capabilities": [
        {
          "id": 121,
          "name": "Agent Creation",
          "description": "Create agent records for authorized representatives.",
          "frequency": "On-demand",
          "volume": "Low"
        },
        {
          "id": 122,
          "name": "Agent Information Retrieval",
          "description": "Retrieve agent details and status information.",
          "frequency": "Real-time",
          "volume": "Medium"
        },
        {
          "id": 123,
          "name": "Agent Status Management",
          "description": "Update agent status and authorization levels.",
          "frequency": "On-demand",
          "volume": "Low"
        }
      ]
    },
    {
      "category": "Regulated Entity Management",
      "capabilities": [
        {
          "id": 124,
          "name": "Regulated Entity Registration",
          "description": "Register regulated entities such as payment service providers.",
          "frequency": "On-demand",
          "volume": "Low"
        },
        {
          "id": 125,
          "name": "Regulated Entity Retrieval",
          "description": "Retrieve information about registered regulated entities.",
          "frequency": "Real-time",
          "volume": "Low"
        },
        {
          "id": 126,
          "name": "Regulated Entity Attribute Management",
          "description": "Manage attributes associated with regulated entities.",
          "frequency": "On-demand",
          "volume": "Low"
        }
      ]
    },
    {
      "category": "Background Processing",
      "capabilities": [
        {
          "id": 127,
          "name": "Consent Expiration Processing",
          "description": "Automatically process and update expired Berlin Group and OBP consents.",
          "frequency": "Scheduled",
          "volume": "Medium"
        },
        {
          "id": 128,
          "name": "Unfinished Consent Cleanup",
          "description": "Automatically reject consents that remain unfinished beyond configured time limits.",
          "frequency": "Scheduled",
          "volume": "Medium"
        },
        {
          "id": 129,
          "name": "Metrics Data Archival",
          "description": "Archive old metrics data to archive tables and clean up primary metrics storage.",
          "frequency": "Scheduled",
          "volume": "High"
        },
        {
          "id": 130,
          "name": "Database Cleanup",
          "description": "Clean up outdated data from database tables for performance optimization.",
          "frequency": "Scheduled",
          "volume": "Medium"
        }
      ]
    },
    {
      "category": "System Configuration",
      "capabilities": [
        {
          "id": 131,
          "name": "Web UI Properties Management",
          "description": "Configure web UI properties for customization and branding.",
          "frequency": "On-demand",
          "volume": "Low"
        },
        {
          "id": 132,
          "name": "Endpoint Tag Management",
          "description": "Configure tags for API endpoints for categorization and filtering.",
          "frequency": "On-demand",
          "volume": "Low"
        },
        {
          "id": 133,
          "name": "Database Information Retrieval",
          "description": "Retrieve database configuration and status information.",
          "frequency": "On-demand",
          "volume": "Low"
        },
        {
          "id": 134,
          "name": "Attribute Definition Management",
          "description": "Define and manage attribute schemas for various entity types.",
          "frequency": "On-demand",
          "volume": "Low"
        }
      ]
    },
    {
      "category": "CRM Integration",
      "capabilities": [
        {
          "id": 135,
          "name": "CRM Event Management",
          "description": "Manage customer relationship management events and interactions.",
          "frequency": "On-demand",
          "volume": "Medium"
        },
        {
          "id": 136,
          "name": "Meeting Management",
          "description": "Schedule and manage customer meetings.",
          "frequency": "On-demand",
          "volume": "Low"
        }
      ]
    }
  ]
}
```

### Capability Summary
- **Total Capabilities Identified**: 136
- **API Endpoints**: Approximately 1,060 across all API versions
- **Background Tasks**: 4 (Consent Expiration, Unfinished Consent Cleanup, Metrics Archival, Database Cleanup)
- **External Integrations**: Multiple backend connector types (REST, Akka, Stored Procedures, RabbitMQ, Blockchain)
- **Primary Business Functions**: Bank Management, Account Management, Transaction Management, Payment Initiation, Customer Management, Consent Management, Regulatory Compliance (PSD2/Berlin Group), API Management, Metrics and Monitoring

---

## Open Questions & Clarifications Needed

1. **Transaction Processing Volume**: What are the expected peak transaction volumes for payment initiation services?

2. **Consent Expiration Timing**: What are the specific business requirements for consent expiration intervals across different regulatory frameworks?

3. **Backend Connector Selection**: Which backend connector types (REST, Akka, Stored Procedures, RabbitMQ, Blockchain) are actively used in production environments?

4. **Multi-Bank Support**: How many banks are typically supported in a single deployment, and what are the performance implications?

5. **Regulatory Compliance Scope**: Beyond PSD2/Berlin Group, what other regulatory frameworks (UK Open Banking, Australian CDR) are actively supported and require migration consideration?

6. **Dynamic Entity Usage**: What is the extent of dynamic entity and endpoint usage in production, and how should these be handled during migration?

7. **Metrics Retention**: What are the specific retention requirements for API metrics data for compliance and audit purposes?

These questions should be addressed during SME review to ensure complete understanding of business requirements for the migration effort.
