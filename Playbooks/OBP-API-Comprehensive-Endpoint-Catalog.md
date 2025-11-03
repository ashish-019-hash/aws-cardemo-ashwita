# OBP-API Comprehensive Endpoint Catalog

**Complete API Endpoint Extraction from Scala OBP-API Codebase**

This document contains ALL extracted API endpoints following the comprehensive extraction methodology.

## Summary

- **Total Endpoints**: 769
- **API Contexts**: 15

### Endpoints by API Context

- **Australian Open Banking (v1_0_0)**: 21 endpoints
- **Bahrain OBF (v1_0_0)**: 57 endpoints
- **Berlin Group PSD2 (v1_3)**: 40 endpoints
- **Mexican Open Finance (MxOF)**: 1 endpoints
- **Polish API (v2_1_1_1)**: 26 endpoints
- **STET (French) (v1_4)**: 11 endpoints
- **UK Open Banking (v2_0_0)**: 5 endpoints
- **UK Open Banking (v3_1_0)**: 66 endpoints
- **v1_4_0**: 2 endpoints
- **v2_2_0**: 18 endpoints
- **v3_0_0**: 45 endpoints
- **v3_1_0**: 93 endpoints
- **v4_0_0**: 247 endpoints
- **v5_0_0**: 35 endpoints
- **v5_1_0**: 102 endpoints

---

## API Context: Australian Open Banking (v1_0_0)

**Total Endpoints**: 21

### GET Endpoints (18)

#### `GET /banking/accounts`

- **Endpoint Name**: `listAccounts`
- **Summary**: Get Accounts
- **Description**: ${mockedDataText(false)} Obtain a list of accounts
- **Source Files**: BankingApi.scala

#### `GET /banking/accounts/ACCOUNT_ID`

- **Endpoint Name**: `getAccountDetail`
- **Summary**: Get Account Detail
- **Description**: ${mockedDataText(true)} Obtain detailed information on a single account
- **Path Parameters**: ACCOUNT_ID
- **Error Codes**: UnknownError
- **Source Files**: AccountsApi.scala, BankingApi.scala

#### `GET /banking/accounts/ACCOUNT_ID/balance`

- **Endpoint Name**: `listBalance`
- **Summary**: Get Account Balance
- **Description**: ${mockedDataText(false)} Obtain the balance for a single specified account
- **Path Parameters**: ACCOUNT_ID
- **Source Files**: BankingApi.scala

#### `GET /banking/accounts/ACCOUNT_ID/direct-debits`

- **Endpoint Name**: `listDirectDebits`
- **Summary**: Get Direct Debits For Account
- **Description**: ${mockedDataText(true)} Obtain direct debit authorisations for a specific account
- **Path Parameters**: ACCOUNT_ID
- **Source Files**: DirectDebitsApi.scala, BankingApi.scala

#### `GET /banking/accounts/ACCOUNT_ID/payments/scheduled`

- **Endpoint Name**: `listScheduledPayments`
- **Summary**: Get Scheduled Payments for Account
- **Description**: ${mockedDataText(true)} Obtain scheduled, outgoing payments for a specific account
- **Path Parameters**: ACCOUNT_ID
- **Source Files**: ScheduledPaymentsApi.scala, BankingApi.scala

#### `GET /banking/accounts/ACCOUNT_ID/transactions`

- **Endpoint Name**: `getTransactions`
- **Summary**: Get Transactions For Account
- **Description**: ${mockedDataText(true)} Obtain transactions for a specific account. Some general notes that apply to all end points that retrieve transactions: - Where multiple transactions are returned, transactions should be ordered according to effective date in descending order - As the date and time for a transaction can alter depending on status and transaction type two separate date/times are included in the payload. There are still some scenarios where neither of these time stamps is available. For the 
- **Path Parameters**: ACCOUNT_ID
- **Source Files**: AccountsApi.scala, BankingApi.scala

#### `GET /banking/accounts/ACCOUNT_ID/transactions/TRANSACTION_ID`

- **Endpoint Name**: `getTransactionDetail`
- **Summary**: Get Transaction Detail
- **Description**: ${mockedDataText(true)} Obtain detailed information on a transaction for a specific account
- **Path Parameters**: ACCOUNT_ID, TRANSACTION_ID
- **Source Files**: AccountsApi.scala, BankingApi.scala

#### `GET /banking/accounts/balances`

- **Endpoint Name**: `listBalancesBulk`
- **Summary**: Get Bulk Balances
- **Description**: ${mockedDataText(true)} Obtain balances for multiple, filtered accounts
- **Source Files**: AccountsApi.scala, BankingApi.scala

#### `GET /banking/accounts/direct-debits`

- **Endpoint Name**: `listDirectDebitsBulk`
- **Summary**: Get Bulk Direct Debits
- **Description**: ${mockedDataText(true)} Obtain direct debit authorisations for multiple, filtered accounts
- **Source Files**: DirectDebitsApi.scala, BankingApi.scala

#### `GET /banking/payees`

- **Endpoint Name**: `listPayees`
- **Summary**: Get Payees
- **Description**: ${mockedDataText(true)} Obtain a list of pre-registered payees
- **Source Files**: BankingApi.scala, PayeesApi.scala

#### `GET /banking/payees/PAYEE_ID`

- **Endpoint Name**: `getPayeeDetail`
- **Summary**: Get Payee Detail
- **Description**: ${mockedDataText(true)} Obtain detailed information on a single payee
- **Path Parameters**: PAYEE_ID
- **Error Codes**: UnknownError
- **Source Files**: BankingApi.scala, PayeesApi.scala

#### `GET /banking/payments/scheduled`

- **Endpoint Name**: `listScheduledPaymentsBulk`
- **Summary**: Get Scheduled Payments Bulk
- **Description**: ${mockedDataText(true)} Obtain scheduled payments for multiple, filtered accounts that are the source of funds for the payments
- **Source Files**: ScheduledPaymentsApi.scala, BankingApi.scala

#### `GET /banking/products`

- **Endpoint Name**: `listProducts`
- **Summary**: Get Products
- **Source Files**: ProductsApi.scala, BankingApi.scala

#### `GET /banking/products/PRODUCT_ID`

- **Endpoint Name**: `getProductDetail`
- **Summary**: Get Product Detail
- **Description**: ${mockedDataText(true)} Obtain detailed information on a single product offered openly to the market
- **Path Parameters**: PRODUCT_ID
- **Error Codes**: UnknownError
- **Source Files**: ProductsApi.scala, BankingApi.scala

#### `GET /common/customer`

- **Endpoint Name**: `getCustomer`
- **Summary**: Get Customer
- **Description**: ${mockedDataText(true)} Obtain basic information on the customer that has authorised the current session
- **Source Files**: CommonApi.scala, CustomerApi.scala

#### `GET /common/customer/detail`

- **Endpoint Name**: `getCustomerDetail`
- **Summary**: Get Customer Detail
- **Description**: ${mockedDataText(true)} Obtain detailed information on the authorised customer within the current session.
- **Error Codes**: UnknownError
- **Source Files**: CommonApi.scala, CustomerApi.scala

#### `GET /discovery/outages`

- **Endpoint Name**: `getOutages`
- **Summary**: Get Outages
- **Description**: ${mockedDataText(true)} Obtain a list of scheduled outages for the implementation
- **Error Codes**: UnknownError
- **Source Files**: DiscoveryApi.scala, CommonApi.scala

#### `GET /discovery/status`

- **Endpoint Name**: `getStatus`
- **Summary**: Get Status
- **Description**: ${mockedDataText(true)} Obtain a health check status for the implementation
- **Source Files**: DiscoveryApi.scala, CommonApi.scala

### POST Endpoints (3)

#### `POST /banking/accounts/balances`

- **Endpoint Name**: `listBalancesSpecificAccounts`
- **Summary**: Get Balances For Specific Accounts
- **Description**: ${mockedDataText(true)} Obtain balances for a specified list of accounts
- **Source Files**: AccountsApi.scala, BankingApi.scala

#### `POST /banking/accounts/direct-debits`

- **Endpoint Name**: `listDirectDebitsSpecificAccounts`
- **Summary**: Get Direct Debits For Specific Accounts
- **Description**: ${mockedDataText(true)} Obtain direct debit authorisations for a specified list of accounts
- **Source Files**: DirectDebitsApi.scala, BankingApi.scala

#### `POST /banking/payments/scheduled`

- **Endpoint Name**: `listScheduledPaymentsSpecificAccounts`
- **Summary**: Get Scheduled Payments For Specific Accounts
- **Description**: ${mockedDataText(true)} Obtain scheduled payments for a specified list of accounts
- **Source Files**: ScheduledPaymentsApi.scala, BankingApi.scala


---

## API Context: Bahrain OBF (v1_0_0)

**Total Endpoints**: 57

### GET Endpoints (43)

#### `GET /account-access-consents/CONSENT_ID`

- **Endpoint Name**: `accountAccessConsentsConsentIdGet`
- **Summary**: Get Account Access Consents by ConsentId
- **Description**: ${mockedDataText(true)}
- **Route Pattern**: `"account-access-consents" :: consentId :: Nil`
- **Path Parameters**: CONSENT_ID
- **Source Files**: AccountAccessConsentsApi.scala

#### `GET /accounts`

- **Endpoint Name**: `accountsGet`
- **Summary**: Get Accounts
- **Description**: ${mockedDataText(true)}
- **Route Pattern**: `case "accounts" :: Nil JsonGet`
- **Source Files**: AccountsApi.scala

#### `GET /accounts/ACCOUNT_ID`

- **Endpoint Name**: `accountsAccountIdGet`
- **Summary**: Get Account by AccountId
- **Description**: ${mockedDataText(true)}
- **Route Pattern**: `"accounts" :: accountId :: Nil`
- **Path Parameters**: ACCOUNT_ID
- **Error Codes**: UnknownError
- **Source Files**: AccountsApi.scala

#### `GET /accounts/ACCOUNT_ID/balances`

- **Endpoint Name**: `accountsAccountIdBalancesGet`
- **Summary**: Get Accounts Balances by AccountId
- **Description**: ${mockedDataText(true)}
- **Route Pattern**: `"accounts" :: accountId:: "balances" :: Nil`
- **Path Parameters**: ACCOUNT_ID
- **Source Files**: BalancesApi.scala

#### `GET /accounts/ACCOUNT_ID/beneficiaries`

- **Endpoint Name**: `accountsAccountIdBeneficiariesGet`
- **Summary**: Get Accounts Beneficiaries by AccountId
- **Description**: ${mockedDataText(true)}
- **Route Pattern**: `"accounts" :: accountId:: "beneficiaries" :: Nil`
- **Path Parameters**: ACCOUNT_ID
- **Error Codes**: UnknownError
- **Source Files**: BeneficiariesApi.scala

#### `GET /accounts/ACCOUNT_ID/direct-debits`

- **Endpoint Name**: `accountsAccountIdDirectDebitsGet`
- **Summary**: Get Accounts Direct Debits by AccountId
- **Description**: ${mockedDataText(true)}
- **Route Pattern**: `"accounts" :: accountId:: "direct-debits" :: Nil`
- **Path Parameters**: ACCOUNT_ID
- **Source Files**: DirectDebitsApi.scala

#### `GET /accounts/ACCOUNT_ID/future-dated-payments`

- **Endpoint Name**: `accountsAccountIdFutureDatedPaymentsGet`
- **Summary**: Get Accounts Future Dated Payments by AccountId
- **Description**: ${mockedDataText(true)}
- **Route Pattern**: `"accounts" :: accountId:: "future-dated-payments" :: Nil`
- **Path Parameters**: ACCOUNT_ID
- **Source Files**: FutureDatedPaymentsApi.scala

#### `GET /accounts/ACCOUNT_ID/offers`

- **Endpoint Name**: `accountsAccountIdOffersGet`
- **Summary**: Get Accounts Offers by AccountId
- **Description**: ${mockedDataText(true)}
- **Route Pattern**: `"accounts" :: accountId:: "offers" :: Nil`
- **Path Parameters**: ACCOUNT_ID
- **Source Files**: OffersApi.scala

#### `GET /accounts/ACCOUNT_ID/parties`

- **Endpoint Name**: `accountsAccountIdPartiesGet`
- **Summary**: Get Accounts Parties by AccountId
- **Description**: ${mockedDataText(true)}
- **Route Pattern**: `"accounts" :: accountId:: "parties" :: Nil`
- **Path Parameters**: ACCOUNT_ID
- **Source Files**: PartiesApi.scala

#### `GET /accounts/ACCOUNT_ID/party`

- **Endpoint Name**: `accountsAccountIdPartyGet`
- **Summary**: Get Accounts Party by AccountId
- **Description**: ${mockedDataText(true)}
- **Route Pattern**: `"accounts" :: accountId:: "party" :: Nil`
- **Path Parameters**: ACCOUNT_ID
- **Source Files**: PartiesApi.scala

#### `GET /accounts/ACCOUNT_ID/standing-orders`

- **Endpoint Name**: `accountsAccountIdStandingOrdersGet`
- **Summary**: Get Accounts Standing Orders by AccountId
- **Description**: ${mockedDataText(true)}
- **Route Pattern**: `"accounts" :: accountId:: "standing-orders" :: Nil`
- **Path Parameters**: ACCOUNT_ID
- **Error Codes**: UnknownError
- **Source Files**: StandingOrdersApi.scala

#### `GET /accounts/ACCOUNT_ID/statements`

- **Endpoint Name**: `accountsAccountIdStatementsGet`
- **Summary**: Get Accounts Statements by AccountId
- **Description**: ${mockedDataText(true)}
- **Route Pattern**: `"accounts" :: accountId:: "statements" :: Nil`
- **Path Parameters**: ACCOUNT_ID
- **Error Codes**: UnknownError
- **Source Files**: StatementsApi.scala

#### `GET /accounts/ACCOUNT_ID/statements/STATEMENT_ID`

- **Endpoint Name**: `accountsAccountIdStatementsStatementIdGet`
- **Summary**: Get Accounts Statement by AccountId and StatementId
- **Description**: ${mockedDataText(true)}
- **Route Pattern**: `"accounts" :: accountId:: "statements" :: statementId :: Nil`
- **Path Parameters**: ACCOUNT_ID, STATEMENT_ID
- **Source Files**: StatementsApi.scala

#### `GET /accounts/ACCOUNT_ID/statements/STATEMENT_ID/file`

- **Endpoint Name**: `accountsAccountIdStatementsStatementIdFileGet`
- **Summary**: Get Accounts Statements File by AccountId and StatementId
- **Description**: ${mockedDataText(true)}
- **Route Pattern**: `"accounts" :: accountId:: "statements" :: statementId:: "file" :: Nil`
- **Path Parameters**: ACCOUNT_ID, STATEMENT_ID
- **Error Codes**: UnknownError
- **Source Files**: StatementsApi.scala

#### `GET /accounts/ACCOUNT_ID/statements/STATEMENT_ID/transactions`

- **Endpoint Name**: `accountsAccountIdStatementsStatementIdTransactionsGet`
- **Summary**: Get Accounts Statement Tranactions by AccountId and StatementId
- **Description**: ${mockedDataText(true)}
- **Route Pattern**: `"accounts" :: accountId:: "statements" :: statementId:: "transactions" :: Nil`
- **Path Parameters**: ACCOUNT_ID, STATEMENT_ID
- **Source Files**: StatementsApi.scala

#### `GET /accounts/ACCOUNT_ID/supplementary-account-info`

- **Endpoint Name**: `accountsAccountIdSupplementaryAccountInfoGet`
- **Summary**: Get Accounts Supplementary Account Info by AccountId
- **Description**: ${mockedDataText(true)}
- **Route Pattern**: `"accounts" :: accountId:: "supplementary-account-info" :: Nil`
- **Path Parameters**: ACCOUNT_ID
- **Source Files**: SupplementaryAccountInfoApi.scala

#### `GET /accounts/ACCOUNT_ID/transactions`

- **Endpoint Name**: `accountsAccountIdTransactionsGet`
- **Summary**: Get Accounts Trabnsactions by AccountId
- **Description**: ${mockedDataText(true)}
- **Route Pattern**: `"accounts" :: accountId:: "transactions" :: Nil`
- **Path Parameters**: ACCOUNT_ID
- **Error Codes**: UnknownError
- **Source Files**: TransactionsApi.scala

#### `GET /balances`

- **Endpoint Name**: `balancesGet`
- **Summary**: Get Balances
- **Description**: ${mockedDataText(true)}
- **Route Pattern**: `case "balances" :: Nil JsonGet`
- **Source Files**: BalancesApi.scala

#### `GET /beneficiaries`

- **Endpoint Name**: `beneficiariesGet`
- **Summary**: Get Beneficiaries
- **Description**: ${mockedDataText(true)}
- **Route Pattern**: `case "beneficiaries" :: Nil JsonGet`
- **Source Files**: BeneficiariesApi.scala

#### `GET /direct-debits`

- **Endpoint Name**: `directDebitsGet`
- **Summary**: Get Direct Debits
- **Description**: ${mockedDataText(true)}
- **Route Pattern**: `case "direct-debits" :: Nil JsonGet`
- **Source Files**: DirectDebitsApi.scala

#### `GET /domestic-future-dated-payment-cancellation-consents/CONSENT_ID`

- **Endpoint Name**: `domesticFutureDatedPaymentCancellationConsentsConsentIdGet`
- **Summary**: Get Domestic Future Dated Payment Cancellation Consents by ConsentId
- **Description**: ${mockedDataText(true)}
- **Route Pattern**: `"domestic-future-dated-payment-cancellation-consents" :: consentId :: Nil`
- **Path Parameters**: CONSENT_ID
- **Source Files**: DomesticFutureDatedPaymentConsentsApi.scala

#### `GET /domestic-future-dated-payment-consents/CONSENT_ID`

- **Endpoint Name**: `domesticFutureDatedPaymentConsentsConsentIdGet`
- **Summary**: Get Domestic Future Dated Payment Consents by ConsentId
- **Description**: ${mockedDataText(true)}
- **Route Pattern**: `"domestic-future-dated-payment-consents" :: consentId :: Nil`
- **Path Parameters**: CONSENT_ID
- **Source Files**: DomesticFutureDatedPaymentConsentsApi.scala

#### `GET /domestic-future-dated-payments/DOMESTIC_FUTURE_DATED_PAYMENT_ID`

- **Endpoint Name**: `domesticFutureDatedPaymentsDomesticFutureDatedPaymentIdGet`
- **Summary**: Get Domestic Future Dated Payments by DomesticFutureDatedPaymentId
- **Description**: ${mockedDataText(true)}
- **Route Pattern**: `"domestic-future-dated-payments" :: domesticFutureDatedPaymentId :: Nil`
- **Path Parameters**: DOMESTIC_FUTURE_DATED_PAYMENT_ID
- **Source Files**: DomesticFutureDatedPaymentsApi.scala

#### `GET /domestic-future-dated-payments/DOMESTIC_FUTURE_DATED_PAYMENT_ID/payment-details`

- **Endpoint Name**: `domesticFutureDatedPaymentsDomesticFutureDatedPaymentIdPaymentDetailsGet`
- **Summary**: Get Domestic Future Dated Payment Details by DomesticFutureDatedPaymentId
- **Description**: ${mockedDataText(true)}
- **Route Pattern**: `"domestic-future-dated-payments" :: domesticFutureDatedPaymentId:: "payment-details" :: Nil`
- **Path Parameters**: DOMESTIC_FUTURE_DATED_PAYMENT_ID
- **Source Files**: DomesticFutureDatedPaymentsApi.scala

#### `GET /domestic-payment-consents/CONSENT_ID`

- **Endpoint Name**: `domesticPaymentConsentsConsentIdGet`
- **Summary**: Get Domestic Payment Consents by ConsentId
- **Description**: ${mockedDataText(true)}
- **Route Pattern**: `"domestic-payment-consents" :: consentId :: Nil`
- **Path Parameters**: CONSENT_ID
- **Source Files**: DomesticPaymentsConsentsApi.scala

#### `GET /domestic-payment-consents/CONSENT_ID/funds-confirmation`

- **Endpoint Name**: `domesticPaymentConsentsConsentIdFundsConfirmationGet`
- **Summary**: Get Domestic Payment Consents Funds Confirmation by ConsentId
- **Description**: ${mockedDataText(true)}
- **Route Pattern**: `"domestic-payment-consents" :: consentId:: "funds-confirmation" :: Nil`
- **Path Parameters**: CONSENT_ID
- **Source Files**: DomesticPaymentsConsentsApi.scala

#### `GET /domestic-payments/DOMESTIC_PAYMENT_ID`

- **Endpoint Name**: `domesticPaymentsDomesticPaymentIdGet`
- **Summary**: Get Domestic Payments by DomesticPaymentId
- **Description**: ${mockedDataText(true)}
- **Route Pattern**: `"domestic-payments" :: domesticPaymentId :: Nil`
- **Path Parameters**: DOMESTIC_PAYMENT_ID
- **Source Files**: DomesticPaymentsApi.scala

#### `GET /domestic-payments/DOMESTIC_PAYMENT_ID/payment-details`

- **Endpoint Name**: `domesticPaymentsDomesticPaymentIdPaymentDetailsGet`
- **Summary**: Get Domestic Payment details by DomesticPaymentId
- **Description**: ${mockedDataText(true)}
- **Route Pattern**: `"domestic-payments" :: domesticPaymentId:: "payment-details" :: Nil`
- **Path Parameters**: DOMESTIC_PAYMENT_ID
- **Source Files**: DomesticPaymentsApi.scala

#### `GET /file-payment-consents/CONSENT_ID`

- **Endpoint Name**: `filePaymentConsentsConsentIdGet`
- **Summary**: Get File Payment Consents by ConsentId
- **Description**: ${mockedDataText(true)}
- **Route Pattern**: `"file-payment-consents" :: consentId :: Nil`
- **Path Parameters**: CONSENT_ID
- **Source Files**: FilePaymentConsentsApi.scala

#### `GET /file-payment-consents/CONSENT_ID/file`

- **Endpoint Name**: `filePaymentConsentsConsentIdFileGet`
- **Summary**: Get File Payment Consents File by ConsentId
- **Description**: ${mockedDataText(true)}
- **Route Pattern**: `"file-payment-consents" :: consentId:: "file" :: Nil`
- **Path Parameters**: CONSENT_ID
- **Error Codes**: UnknownError
- **Source Files**: FilePaymentConsentsApi.scala

#### `GET /file-payments/FILE_PAYMENT_ID`

- **Endpoint Name**: `filePaymentsFilePaymentIdGet`
- **Summary**: Get File Payments by FilePaymentId
- **Description**: ${mockedDataText(true)}
- **Route Pattern**: `"file-payments" :: filePaymentId :: Nil`
- **Path Parameters**: FILE_PAYMENT_ID
- **Source Files**: FilePaymentsApi.scala

#### `GET /file-payments/FILE_PAYMENT_ID/payment-details`

- **Endpoint Name**: `filePaymentsFilePaymentIdPaymentDetailsGet`
- **Summary**: Get File Payment Details by FilePaymentId
- **Description**: ${mockedDataText(true)}
- **Route Pattern**: `"file-payments" :: filePaymentId:: "payment-details" :: Nil`
- **Path Parameters**: FILE_PAYMENT_ID
- **Source Files**: FilePaymentsApi.scala

#### `GET /file-payments/FILE_PAYMENT_ID/report-file`

- **Endpoint Name**: `filePaymentsFilePaymentIdReportFileGet`
- **Summary**: Get File Payments Report File by FilePaymentId
- **Description**: ${mockedDataText(true)}
- **Route Pattern**: `"file-payments" :: filePaymentId:: "report-file" :: Nil`
- **Path Parameters**: FILE_PAYMENT_ID
- **Source Files**: FilePaymentsApi.scala

#### `GET /future-dated-payments`

- **Endpoint Name**: `futureDatedPaymentsGet`
- **Summary**: Get Future Dated Payments
- **Description**: ${mockedDataText(true)}
- **Route Pattern**: `case "future-dated-payments" :: Nil JsonGet`
- **Source Files**: FutureDatedPaymentsApi.scala

#### `GET /international-payment-consents/CONSENT_ID`

- **Endpoint Name**: `internationalPaymentConsentsConsentIdGet`
- **Summary**: Get International Payment Consents by ConsentId
- **Description**: ${mockedDataText(true)}
- **Route Pattern**: `"international-payment-consents" :: consentId :: Nil`
- **Path Parameters**: CONSENT_ID
- **Source Files**: InternationalPaymentConsentsApi.scala

#### `GET /international-payment-consents/CONSENT_ID/funds-confirmation`

- **Endpoint Name**: `internationalPaymentConsentsConsentIdFundsConfirmationGet`
- **Summary**: Get International Payment Consents Funds Confirmation by ConsentId
- **Description**: ${mockedDataText(true)}
- **Route Pattern**: `"international-payment-consents" :: consentId:: "funds-confirmation" :: Nil`
- **Path Parameters**: CONSENT_ID
- **Source Files**: InternationalPaymentConsentsApi.scala

#### `GET /international-payments/INTERNATIONAL_PAYMENT_ID`

- **Endpoint Name**: `internationalPaymentsInternationalPaymentIdGet`
- **Summary**: Get International Payments by InternationalPaymentId
- **Description**: ${mockedDataText(true)}
- **Path Parameters**: INTERNATIONAL_PAYMENT_ID
- **Source Files**: InternationalPaymentsApi.scala

#### `GET /international-payments/INTERNATIONAL_PAYMENT_ID/payment-details`

- **Endpoint Name**: `internationalPaymentsInternationalPaymentIdPaymentDetailsGet`
- **Summary**: Get International Payment Details by InternationalPaymentId
- **Description**: ${mockedDataText(true)}
- **Route Pattern**: `"international-payments" :: internationalPaymentId:: "payment-details" :: Nil`
- **Path Parameters**: INTERNATIONAL_PAYMENT_ID
- **Source Files**: InternationalPaymentsApi.scala

#### `GET /offers`

- **Endpoint Name**: `offersGet`
- **Summary**: Get Offers
- **Description**: ${mockedDataText(true)}
- **Route Pattern**: `case "offers" :: Nil JsonGet`
- **Source Files**: OffersApi.scala

#### `GET /party`

- **Endpoint Name**: `partyGet`
- **Summary**: Get Party
- **Description**: ${mockedDataText(true)}
- **Route Pattern**: `case "party" :: Nil JsonGet`
- **Source Files**: PartiesApi.scala

#### `GET /standing-orders`

- **Endpoint Name**: `standingOrdersGet`
- **Summary**: Get Standing Orders
- **Description**: ${mockedDataText(true)}
- **Route Pattern**: `case "standing-orders" :: Nil JsonGet`
- **Source Files**: StandingOrdersApi.scala

#### `GET /statements`

- **Endpoint Name**: `statementsGet`
- **Summary**: Get Statements
- **Description**: ${mockedDataText(true)}
- **Route Pattern**: `case "statements" :: Nil JsonGet`
- **Source Files**: StatementsApi.scala

#### `GET /transactions`

- **Endpoint Name**: `transactionsGet`
- **Summary**: Get Transactions
- **Description**: ${mockedDataText(true)}
- **Route Pattern**: `case "transactions" :: Nil JsonGet`
- **Source Files**: TransactionsApi.scala

### POST Endpoints (12)

#### `POST /account-access-consents`

- **Endpoint Name**: `accountAccessConsentsPost`
- **Summary**: Create Account Access Consents
- **Description**: ${mockedDataText(true)}
- **Route Pattern**: `case "account-access-consents" :: Nil JsonPost`
- **Source Files**: AccountAccessConsentsApi.scala

#### `POST /domestic-future-dated-payment-cancellation-consents`

- **Endpoint Name**: `domesticFutureDatedPaymentCancellationConsentsPost`
- **Summary**: Create Domestic Future Dated Payment Cancellation Consents
- **Description**: ${mockedDataText(true)}
- **Route Pattern**: `case "domestic-future-dated-payment-cancellation-consents" :: Nil JsonPost`
- **Source Files**: DomesticFutureDatedPaymentConsentsApi.scala

#### `POST /domestic-future-dated-payment-consents`

- **Endpoint Name**: `domesticFutureDatedPaymentConsentsPost`
- **Summary**: Create Domestic Future Dated Payment Consents
- **Description**: ${mockedDataText(true)}
- **Source Files**: DomesticFutureDatedPaymentConsentsApi.scala

#### `POST /domestic-future-dated-payments`

- **Endpoint Name**: `domesticFutureDatedPaymentsPost`
- **Summary**: Create Domestic Future Dated Payments
- **Description**: ${mockedDataText(true)}
- **Route Pattern**: `case "domestic-future-dated-payments" :: Nil JsonPost`
- **Source Files**: DomesticFutureDatedPaymentsApi.scala

#### `POST /domestic-payment-consents`

- **Endpoint Name**: `domesticPaymentConsentsPost`
- **Summary**: Create Domestic Payment Consents
- **Description**: ${mockedDataText(true)}
- **Source Files**: DomesticPaymentsConsentsApi.scala

#### `POST /domestic-payments`

- **Endpoint Name**: `domesticPaymentsPost`
- **Summary**: Create Domestic Payments
- **Description**: ${mockedDataText(true)}
- **Route Pattern**: `case "domestic-payments" :: Nil JsonPost`
- **Source Files**: DomesticPaymentsApi.scala

#### `POST /event-notifications`

- **Endpoint Name**: `eventNotificationsPost`
- **Summary**: The ASPSP to send an event-notification resource to a TPP
- **Description**: ${mockedDataText(true)}
- **Route Pattern**: `case "event-notifications" :: Nil JsonPost`
- **Source Files**: EventNotificationApi.scala

#### `POST /file-payment-consents`

- **Endpoint Name**: `filePaymentConsentsPost`
- **Summary**: Create File Payment Consents
- **Description**: ${mockedDataText(true)}
- **Route Pattern**: `case "file-payment-consents" :: Nil JsonPost`
- **Source Files**: FilePaymentConsentsApi.scala

#### `POST /file-payment-consents/CONSENT_ID/file`

- **Endpoint Name**: `filePaymentConsentsConsentIdFilePost`
- **Summary**: Create File Payment Consents File by ConsentId
- **Description**: ${mockedDataText(true)}
- **Route Pattern**: `"file-payment-consents" :: consentId:: "file" :: Nil`
- **Path Parameters**: CONSENT_ID
- **Source Files**: FilePaymentConsentsApi.scala

#### `POST /file-payments`

- **Endpoint Name**: `filePaymentsPost`
- **Summary**: Create File Payments
- **Description**: ${mockedDataText(true)}
- **Route Pattern**: `case "file-payments" :: Nil JsonPost`
- **Source Files**: FilePaymentsApi.scala

#### `POST /international-payment-consents`

- **Endpoint Name**: `internationalPaymentConsentsPost`
- **Summary**: Create International Payment Consents
- **Description**: ${mockedDataText(true)}
- **Source Files**: InternationalPaymentConsentsApi.scala

#### `POST /international-payments`

- **Endpoint Name**: `internationalPaymentsPost`
- **Summary**: Create International Payments
- **Description**: ${mockedDataText(true)}
- **Source Files**: InternationalPaymentsApi.scala

### PATCH Endpoints (2)

#### `PATCH /account-access-consents/CONSENT_ID`

- **Endpoint Name**: `accountAccessConsentsConsentIdPatch`
- **Summary**: Update Account Access Consent Status by ConsentId
- **Description**: ${mockedDataText(true)}
- **Route Pattern**: `"account-access-consents" :: consentId :: Nil`
- **Path Parameters**: CONSENT_ID
- **Source Files**: AccountAccessConsentsApi.scala

#### `PATCH /domestic-future-dated-payments/DOMESTIC_FUTURE_DATED_PAYMENT_ID`

- **Endpoint Name**: `domesticFutureDatedPaymentsDomesticFutureDatedPaymentIdPatch`
- **Summary**: Patch Domestic Future Dated Payments by DomesticFutureDatedPaymentId
- **Description**: ${mockedDataText(true)}
- **Route Pattern**: `"domestic-future-dated-payments" :: domesticFutureDatedPaymentId :: Nil`
- **Path Parameters**: DOMESTIC_FUTURE_DATED_PAYMENT_ID
- **Source Files**: DomesticFutureDatedPaymentsApi.scala


---

## API Context: Berlin Group PSD2 (v1_3)

**Total Endpoints**: 40

### GET Endpoints (23)

#### `GET /PAYMENT_SERVICE/PAYMENT_PRODUCT/PAYMENTID`

- **Endpoint Name**: `getPaymentInformation`
- **Summary**: Get Payment Information
- **Description**: ${mockedDataText(false)} Returns the content of a payment object
- **Path Parameters**: PAYMENT_SERVICE, PAYMENTID, PAYMENT_PRODUCT
- **Source Files**: PaymentInitiationServicePISApi.scala

#### `GET /PAYMENT_SERVICE/PAYMENT_PRODUCT/PAYMENTID/authorisations`

- **Endpoint Name**: `getPaymentInitiationAuthorisation`
- **Summary**: Get Payment Initiation Authorisation Sub-Resources Request
- **Description**: ${mockedDataText(false)} Read a list of all authorisation subresources IDs which have been created. This function returns an array of hyperlinks to all generated authorisation sub-resources.
- **Path Parameters**: PAYMENT_SERVICE, PAYMENTID, PAYMENT_PRODUCT
- **Source Files**: PaymentInitiationServicePISApi.scala

#### `GET /PAYMENT_SERVICE/PAYMENT_PRODUCT/PAYMENTID/cancellation-authorisations`

- **Endpoint Name**: `getPaymentInitiationCancellationAuthorisationInformation`
- **Summary**: Get Cancellation Authorisation Sub-Resources Request
- **Description**: ${mockedDataText(false)} Retrieve a list of all created cancellation authorisation sub-resources.
- **Path Parameters**: PAYMENT_SERVICE, PAYMENTID, PAYMENT_PRODUCT
- **Error Codes**: checkPaymentProductError, checkPaymentServerTypeError, UnknownError
- **Source Files**: PaymentInitiationServicePISApi.scala

#### `GET /PAYMENT_SERVICE/PAYMENT_PRODUCT/PAYMENTID/cancellation-authorisations/CANCELLATIONID`

- **Endpoint Name**: `getPaymentCancellationScaStatus`
- **Summary**: Read the SCA status of the payment cancellation's authorisation.
- **Description**: ${mockedDataText(false)} This method returns the SCA status of a payment initiation's authorisation sub-resource.
- **Path Parameters**: CANCELLATIONID, PAYMENT_SERVICE, PAYMENTID, PAYMENT_PRODUCT
- **Source Files**: PaymentInitiationServicePISApi.scala

#### `GET /PAYMENT_SERVICE/PAYMENT_PRODUCT/PAYMENT_ID/authorisations/AUTHORISATION_ID`

- **Endpoint Name**: `getPaymentInitiationScaStatus`
- **Summary**: Read the SCA Status of the payment authorisation
- **Description**: ${mockedDataText(false)} This method returns the SCA status of a payment initiation's authorisation sub-resource.
- **Path Parameters**: PAYMENT_ID, PAYMENT_SERVICE, AUTHORISATION_ID, PAYMENT_PRODUCT
- **Source Files**: PaymentInitiationServicePISApi.scala

#### `GET /PAYMENT_SERVICE/PAYMENT_PRODUCT/PAYMENT_ID/status`

- **Endpoint Name**: `getPaymentInitiationStatus`
- **Summary**: Payment initiation status request
- **Description**: ${mockedDataText(false)} Check the transaction status of a payment initiation.
- **Path Parameters**: PAYMENT_ID, PAYMENT_SERVICE, PAYMENT_PRODUCT
- **Source Files**: PaymentInitiationServicePISApi.scala

#### `GET /accounts`

- **Endpoint Name**: `getAccountList`
- **Summary**: Read Account List
- **Description**: ${mockedDataText(false)} Read the identifiers of the available payment account together with booking balance information, depending on the consent granted. It is assumed that a consent of the PSU to this access is already given and stored on the ASPSP system. The addressed list of accounts depends then on the PSU ID and the stored consent addressed by consentId, respectively the OAuth2 access token. Returns all identifiers of the accounts, to which an account access has been granted to through t
- **Route Pattern**: `case "accounts" :: Nil JsonGet`
- **Source Files**: AccountInformationServiceAISApi.scala

#### `GET /accounts/ACCOUNT_ID`

- **Endpoint Name**: `getAccountDetails`
- **Summary**: Read Account Details
- **Description**: ${mockedDataText(false)} Reads details about an account, with balances where required. It is assumed that a consent of the PSU to this access is already given and stored on the ASPSP system. The addressed details of this account depends then on the stored consent addressed by consentId, respectively the OAuth2 access token. **NOTE:** The account-id can represent a multicurrency account. In this case the currency code is set to "XXX". Give detailed information about the addressed account. Give de
- **Route Pattern**: `"accounts" :: accountId :: Nil`
- **Path Parameters**: ACCOUNT_ID
- **Source Files**: AccountInformationServiceAISApi.scala

#### `GET /accounts/ACCOUNT_ID/balances`

- **Endpoint Name**: `getBalances`
- **Summary**: Read Balance
- **Description**: ${mockedDataText(false)} Reads account data from a given account addressed by "account-id". **Remark:** This account-id can be a tokenised identification due to data protection reason since the path information might be logged on intermediary servers within the ASPSP sphere. This account-id then can be retrieved by the "GET Account List" call. The account-id is constant at least throughout the lifecycle of a given consent.
- **Route Pattern**: `"accounts" :: AccountId(accountId):: "balances" :: Nil`
- **Path Parameters**: ACCOUNT_ID
- **Request Body**: createAccountBalanceJSON
- **Source Files**: AccountInformationServiceAISApi.scala

#### `GET /accounts/ACCOUNT_ID/transactions`

- **Endpoint Name**: `getTransactionList`
- **Summary**: Read transaction list of an account
- **Description**: ${mockedDataText(false)} Read transaction reports or transaction lists of a given account addressed by "account-id", depending on the steering parameter "bookingStatus" together with balances. For a given account, additional parameters are e.g. the attributes "dateFrom" and "dateTo". The ASPSP might add balance information, if transaction lists without balances are not supported.
- **Route Pattern**: `"accounts" :: AccountId(accountId):: "transactions" :: Nil`
- **Path Parameters**: ACCOUNT_ID
- **Source Files**: AccountInformationServiceAISApi.scala

#### `GET /accounts/ACCOUNT_ID/transactions/TRANSACTIONID`

- **Endpoint Name**: `getTransactionDetails`
- **Summary**: Read Transaction Details
- **Description**: ${mockedDataText(false)} Reads transaction details from a given transaction addressed by "transactionId" on a given account addressed by "account-id". This call is only available on transactions as reported in a JSON format. **Remark:** Please note that the PATH might be already given in detail by the corresponding entry of the response of the "Read Transaction List" call within the _links subfield.
- **Route Pattern**: `"accounts" :: accountId :: "transactions" :: transactionId :: Nil`
- **Path Parameters**: ACCOUNT_ID, TRANSACTIONID
- **Source Files**: AccountInformationServiceAISApi.scala

#### `GET /card-accounts`

- **Endpoint Name**: `getCardAccounts`
- **Summary**: Reads a list of card accounts
- **Description**: ${mockedDataText(false)} Reads a list of card accounts with additional information, e.g. balance information. It is assumed that a consent of the PSU to this access is already given and stored on the ASPSP system. The addressed list of card accounts depends then on the PSU ID and the stored consent addressed by consentId, respectively the OAuth2 access token.
- **Route Pattern**: `case "card-accounts" :: Nil JsonGet`
- **Source Files**: AccountInformationServiceAISApi.scala

#### `GET /card-accounts/ACCOUNT_ID`

- **Endpoint Name**: `readCardAccount`
- **Summary**: Reads details about a card account
- **Description**: ${mockedDataText(false)} Reads details about a card account. It is assumed that a consent of the PSU to this access is already given and stored on the ASPSP system. The addressed details of this account depends then on the stored consent addressed by consentId, respectively the OAuth2 access token.
- **Route Pattern**: `"card-accounts" :: accountId :: Nil`
- **Path Parameters**: ACCOUNT_ID
- **Source Files**: AccountInformationServiceAISApi.scala

#### `GET /card-accounts/ACCOUNT_ID/balances`

- **Endpoint Name**: `getCardAccountBalances`
- **Summary**: Read card account balances
- **Description**: ${mockedDataText(false)} Reads balance data from a given card account addressed by "account-id". Remark: This account-id can be a tokenised identification due to data protection reason since the path information might be logged on intermediary servers within the ASPSP sphere. This account-id then can be retrieved by the "GET Card Account List" call
- **Route Pattern**: `"card-accounts" :: AccountId(accountId) :: "balances" :: Nil`
- **Path Parameters**: ACCOUNT_ID
- **Request Body**: createCardAccountBalanceJSON
- **Source Files**: AccountInformationServiceAISApi.scala

#### `GET /card-accounts/ACCOUNT_ID/transactions`

- **Endpoint Name**: `getCardAccountTransactionList`
- **Summary**: Read transaction list of a card account
- **Description**: ${mockedDataText(false)} Reads account data from a given card account addressed by "account-id".
- **Route Pattern**: `"card-accounts" :: AccountId(accountId):: "transactions" :: Nil`
- **Path Parameters**: ACCOUNT_ID
- **Source Files**: AccountInformationServiceAISApi.scala

#### `GET /consents/CONSENTID`

- **Endpoint Name**: `getConsentInformation`
- **Summary**: Get Consent Request
- **Description**: ${mockedDataText(false)} Returns the content of an account information consent object. This is returning the data for the TPP especially in cases, where the consent was directly managed between ASPSP and PSU e.g. in a re-direct SCA Approach.
- **Route Pattern**: `"consents" :: consentId :: Nil`
- **Path Parameters**: CONSENTID
- **Source Files**: AccountInformationServiceAISApi.scala

#### `GET /consents/CONSENTID/authorisations`

- **Endpoint Name**: `getConsentAuthorisation`
- **Summary**: Get Consent Authorisation Sub-Resources Request
- **Description**: ${mockedDataText(false)} Return a list of all authorisation subresources IDs which have been created. This function returns an array of hyperlinks to all generated authorisation sub-resources.
- **Route Pattern**: `"consents" :: consentId :: "authorisations" :: Nil`
- **Path Parameters**: CONSENTID
- **Source Files**: AccountInformationServiceAISApi.scala

#### `GET /consents/CONSENTID/authorisations/AUTHORISATIONID`

- **Endpoint Name**: `getConsentScaStatus`
- **Summary**: Read the SCA status of the consent authorisation
- **Description**: ${mockedDataText(false)} This method returns the SCA status of a consent initiation's authorisation sub-resource.
- **Route Pattern**: `"consents" :: consentId:: "authorisations" :: authorisationId :: Nil`
- **Path Parameters**: AUTHORISATIONID, CONSENTID
- **Error Codes**: UnknownError, ConsentNotFound
- **Source Files**: AccountInformationServiceAISApi.scala

#### `GET /consents/CONSENTID/status`

- **Endpoint Name**: `getConsentStatus`
- **Summary**: Consent status request
- **Description**: ${mockedDataText(false)} Read the status of an account information consent resource.
- **Route Pattern**: `"consents" :: consentId:: "status" :: Nil`
- **Path Parameters**: CONSENTID
- **Source Files**: AccountInformationServiceAISApi.scala

#### `GET /signing-baskets/BASKETID`

- **Endpoint Name**: `getSigningBasket`
- **Summary**: Returns the content of an signing basket object.
- **Description**: ${mockedDataText(false)} Returns the content of an signing basket object.
- **Route Pattern**: `"signing-baskets" :: basketid :: Nil`
- **Path Parameters**: BASKETID
- **Error Codes**: UnknownError
- **Source Files**: SigningBasketsApi.scala

#### `GET /signing-baskets/BASKETID/authorisations`

- **Endpoint Name**: `getSigningBasketAuthorisation`
- **Summary**: Get Signing Basket Authorisation Sub-Resources Request
- **Description**: ${mockedDataText(false)} Read a list of all authorisation subresources IDs which have been created. This function returns an array of hyperlinks to all generated authorisation sub-resources.
- **Route Pattern**: `"signing-baskets" :: basketid:: "authorisations" :: Nil`
- **Path Parameters**: BASKETID
- **Error Codes**: UnknownError
- **Source Files**: SigningBasketsApi.scala

#### `GET /signing-baskets/BASKETID/authorisations/AUTHORISATIONID`

- **Endpoint Name**: `getSigningBasketScaStatus`
- **Summary**: Read the SCA status of the signing basket authorisation
- **Description**: ${mockedDataText(false)} This method returns the SCA status of a signing basket's authorisation sub-resource.
- **Route Pattern**: `"signing-baskets" :: basketId:: "authorisations" :: authorisationId :: Nil`
- **Path Parameters**: AUTHORISATIONID, BASKETID
- **Error Codes**: UnknownError, ConsentNotFound
- **Source Files**: SigningBasketsApi.scala

#### `GET /signing-baskets/BASKETID/status`

- **Endpoint Name**: `getSigningBasketStatus`
- **Summary**: Read the status of the signing basket
- **Description**: ${mockedDataText(false)} Returns the status of a signing basket object.
- **Route Pattern**: `"signing-baskets" :: basketid:: "status" :: Nil`
- **Path Parameters**: BASKETID
- **Source Files**: SigningBasketsApi.scala

### POST Endpoints (10)

#### `POST /PAYMENT_SERVICE/PAYMENT_PRODUCT/PAYMENT_ID/authorisations`

- **Endpoint Name**: `startPaymentAuthorisationUpdatePsuAuthentication`
- **Summary**: Start the authorisation process for a payment initiation (selectPsuAuthenticationMethod)
- **Description**: { "scaStatus": "finalised", "_links":{ "status": {"href":"/v1/payments/sepa-credit-transfers/qwer3456tzui7890/status"} } }
- **Path Parameters**: PAYMENT_ID, PAYMENT_SERVICE, PAYMENT_PRODUCT
- **Source Files**: PaymentInitiationServicePISApi.scala

#### `POST /PAYMENT_SERVICE/PAYMENT_PRODUCT/PAYMENT_ID/cancellation-authorisations`

- **Endpoint Name**: `startPaymentInitiationCancellationAuthorisationTransactionAuthorisation`
- **Summary**: Start the authorisation process for the cancellation of the addressed payment (transactionAuthorisat
- **Description**: { "psuData": { "password": "start12" } }
- **Path Parameters**: PAYMENT_ID, PAYMENT_SERVICE, PAYMENT_PRODUCT
- **Source Files**: PaymentInitiationServicePISApi.scala

#### `POST /bulk-payments/PAYMENT_PRODUCT`

- **Endpoint Name**: `initiateBulkPayments`
- **Summary**: Payment initiation request(bulk-payments)
- **Description**: { "batchBookingPreferred": "true", "debtorAccount": { "iban": "DE40100100103307118608" }, "paymentInformationId": "my-bulk-identification-1234", "requestedExecutionDate": "2018-08-01", "payments": [ { "instructedAmount": { "currency": "EUR", "amount": "123.50" }, "creditorName": "Merchant123", "creditorAccount": { "iban": "DE02100100109307118603" }, "remittanceInformationUnstructured": "Ref Number Merchant 1" }, { "instructedAmount": { "currency": "EUR", "amount": "34.10" }, "creditorName": "Mer
- **Route Pattern**: `"bulk-payments" :: paymentProduct :: Nil`
- **Path Parameters**: PAYMENT_PRODUCT
- **Source Files**: PaymentInitiationServicePISApi.scala

#### `POST /consents`

- **Endpoint Name**: `createConsent`
- **Summary**: Create consent
- **Description**: ${mockedDataText(false)} This method create a consent resource, defining access rights to dedicated accounts of a given PSU-ID. These accounts are addressed explicitly in the method as parameters as a core function. **Side Effects** When this Consent Request is a request where the "recurringIndicator" equals "true", and if it exists already a former consent for recurring access on account information for the addressed PSU, then the former consent automatically expires as soon as the new consent 
- **Route Pattern**: `case "consents" :: Nil JsonPost`
- **Source Files**: AccountInformationServiceAISApi.scala

#### `POST /consents/CONSENTID/authorisations`

- **Endpoint Name**: `startConsentAuthorisationTransactionAuthorisation`
- **Summary**: Start the authorisation process for a consent(selectPsuAuthenticationMethod)
- **Description**: { "psuData": { "password": "start12" } }
- **Route Pattern**: `"consents" :: consentId :: "authorisations" :: Nil`
- **Path Parameters**: CONSENTID
- **Source Files**: AccountInformationServiceAISApi.scala

#### `POST /funds-confirmations`

- **Endpoint Name**: `checkAvailabilityOfFunds`
- **Summary**: Confirmation of Funds Request
- **Description**: ${mockedDataText(false)} Creates a confirmation of funds request at the ASPSP. Checks whether a specific amount is available at point of time of the request on an account linked to a given tuple card issuer(TPP)/card number, or addressed by IBAN and TPP respectively. If the related extended services are used a conditional Consent-ID is contained in the header. This field is contained but commented out in this specification.
- **Route Pattern**: `case "funds-confirmations" ::  Nil JsonPost`
- **Source Files**: ConfirmationOfFundsServicePIISApi.scala

#### `POST /payments/PAYMENT_PRODUCT`

- **Endpoint Name**: `initiatePayments`
- **Summary**: Payment initiation request(payments)
- **Description**: { "debtorAccount": { "iban": "DE123456987480123" }, "instructedAmount": { "currency": "EUR", "amount": "100" }, "creditorAccount": { "iban": "UK12 1234 5123 4517 2948 6166 077" }, "creditorName": "70charname" }
- **Route Pattern**: `"payments" :: paymentProduct :: Nil`
- **Path Parameters**: PAYMENT_PRODUCT
- **Source Files**: PaymentInitiationServicePISApi.scala

#### `POST /periodic-payments/PAYMENT_PRODUCT`

- **Endpoint Name**: `initiatePeriodicPayments`
- **Summary**: Payment initiation request(periodic-payments)
- **Description**: { "instructedAmount": { "currency": "EUR", "amount": "123" }, "debtorAccount": { "iban": "DE40100100103307118608" }, "creditorName": "Merchant123", "creditorAccount": { "iban": "DE23100120020123456789" }, "remittanceInformationUnstructured": "Ref Number Abonnement", "startDate": "2018-03-01", "executionRule": "preceding", "frequency": "Monthly", "dayOfExecution": "01" }
- **Route Pattern**: `"periodic-payments" :: paymentProduct :: Nil`
- **Path Parameters**: PAYMENT_PRODUCT
- **Source Files**: PaymentInitiationServicePISApi.scala

#### `POST /signing-baskets`

- **Endpoint Name**: `createSigningBasket`
- **Summary**: Create a signing basket resource
- **Description**: ${mockedDataText(false)} Create a signing basket resource for authorising several transactions with one SCA method. The resource identifications of these transactions are contained in the payload of this access method
- **Route Pattern**: `case "signing-baskets" :: Nil JsonPost`
- **Source Files**: SigningBasketsApi.scala

#### `POST /signing-baskets/BASKETID/authorisations`

- **Endpoint Name**: `startSigningBasketAuthorisation`
- **Summary**: Start the authorisation process for a signing basket
- **Description**: ${mockedDataText(false)} Create an authorisation sub-resource and start the authorisation process of a signing basket. The message might in addition transmit authentication and authorisation related data. This method is iterated n times for a n times SCA authorisation in a corporate context, each creating an own authorisation sub-endpoint for the corresponding PSU authorising the signing-baskets. The ASPSP might make the usage of this access method unnecessary in case of only one SCA process nee
- **Route Pattern**: `"signing-baskets" :: basketId :: "authorisations" :: Nil`
- **Path Parameters**: BASKETID
- **Source Files**: SigningBasketsApi.scala

### PUT Endpoints (4)

#### `PUT /PAYMENT_SERVICE/PAYMENT_PRODUCT/PAYMENT_ID/authorisations/AUTHORISATION_ID`

- **Endpoint Name**: `updatePaymentPsuDataTransactionAuthorisation`
- **Summary**: Update PSU data for payment initiation (selectPsuAuthenticationMethod)
- **Description**: {"confirmationCode":"confirmationCode"}
- **Path Parameters**: PAYMENT_ID, PAYMENT_SERVICE, AUTHORISATION_ID, PAYMENT_PRODUCT
- **Source Files**: PaymentInitiationServicePISApi.scala

#### `PUT /PAYMENT_SERVICE/PAYMENT_PRODUCT/PAYMENT_ID/cancellation-authorisations/AUTHORISATION_ID`

- **Endpoint Name**: `updatePaymentCancellationPsuDataTransactionAuthorisation`
- **Summary**: Update PSU Data for payment initiation cancellation (selectPsuAuthenticationMethod)
- **Description**: {"confirmationCode":"confirmationCode"}
- **Path Parameters**: PAYMENT_ID, PAYMENT_SERVICE, AUTHORISATION_ID, PAYMENT_PRODUCT
- **Source Files**: PaymentInitiationServicePISApi.scala

#### `PUT /consents/CONSENTID/authorisations/AUTHORISATIONID`

- **Endpoint Name**: `updateConsentsPsuDataTransactionAuthorisation`
- **Summary**: Update PSU Data for consents (selectPsuAuthenticationMethod)
- **Description**: { "authenticationMethodId": "myAuthenticationID" }
- **Route Pattern**: `"consents" :: consentId :: "authorisations" :: authorisationId :: Nil`
- **Path Parameters**: AUTHORISATIONID, CONSENTID
- **Source Files**: AccountInformationServiceAISApi.scala

#### `PUT /signing-baskets/BASKETID/authorisations/AUTHORISATIONID`

- **Endpoint Name**: `updateSigningBasketPsuData`
- **Summary**: Update PSU Data for signing basket
- **Description**: ${mockedDataText(false)} This method update PSU data on the signing basket resource if needed. It may authorise a igning basket within the Embedded SCA Approach where needed. Independently from the SCA Approach it supports e.g. the selection of the authentication method and a non-SCA PSU authentication. This methods updates PSU data on the cancellation authorisation resource if needed. There are several possible Update PSU Data requests in the context of a consent request if needed, which depend
- **Route Pattern**: `"signing-baskets" :: basketId:: "authorisations" :: authorisationId :: Nil`
- **Path Parameters**: AUTHORISATIONID, BASKETID
- **Source Files**: SigningBasketsApi.scala

### DELETE Endpoints (3)

#### `DELETE /PAYMENT_SERVICE/PAYMENT_PRODUCT/PAYMENTID`

- **Endpoint Name**: `cancelPayment`
- **Summary**: Payment Cancellation Request
- **Description**: ${mockedDataText(false)} This method initiates the cancellation of a payment. Depending on the payment-service, the payment-product and the ASPSP's implementation, this TPP call might be sufficient to cancel a payment. If an authorisation of the payment cancellation is mandated by the ASPSP, a corresponding hyperlink will be contained in the response message. Cancels the addressed payment with resource identification paymentId if applicable to the payment-service, payment-product and received in
- **Path Parameters**: PAYMENT_SERVICE, PAYMENTID, PAYMENT_PRODUCT
- **Source Files**: PaymentInitiationServicePISApi.scala

#### `DELETE /consents/CONSENTID`

- **Endpoint Name**: `deleteConsent`
- **Summary**: Delete Consent
- **Description**: ${mockedDataText(false)} The TPP can delete an account information consent object if needed.
- **Route Pattern**: `"consents" :: consentId :: Nil`
- **Path Parameters**: CONSENTID
- **Source Files**: AccountInformationServiceAISApi.scala

#### `DELETE /signing-baskets/BASKETID`

- **Endpoint Name**: `deleteSigningBasket`
- **Summary**: Delete the signing basket
- **Description**: ${mockedDataText(false)} Delete the signing basket structure as long as no (partial) authorisation has yet been applied. The undlerying transactions are not affected by this deletion. Remark: The signing basket as such is not deletable after a first (partial) authorisation has been applied. Nevertheless, single transactions might be cancelled on an individual basis on the XS2A interface.
- **Route Pattern**: `"signing-baskets" :: basketid :: Nil`
- **Path Parameters**: BASKETID
- **Error Codes**: UnknownError
- **Source Files**: SigningBasketsApi.scala


---

## API Context: Mexican Open Finance (MxOF)

**Total Endpoints**: 1

### GET Endpoints (1)

#### `GET /atms`

- **Endpoint Name**: `getMxAtms`
- **Summary**: Get ATMS
- **Description**: ${mockedDataText(false)} Gets a list of all ATM objects.
- **Route Pattern**: `case "atms" :: Nil JsonGet`
- **Source Files**: APIMethods_AtmsApi.scala


---

## API Context: Polish API (v2_1_1_1)

**Total Endpoints**: 26

### POST Endpoints (26)

#### `POST /accounts/v2_1_1.1/deleteConsent`

- **Endpoint Name**: `deleteConsent`
- **Summary**: Removes consent
- **Description**: ${mockedDataText(true)} Removes consent
- **Path Parameters**: C, _
- **Error Codes**: UnknownError
- **Source Files**: AISApi.scala

#### `POST /accounts/v2_1_1.1/getAccount`

- **Endpoint Name**: `getAccount`
- **Summary**: Get detailed information about user payment account
- **Description**: ${mockedDataText(true)} User identification based on access token
- **Path Parameters**: A, _
- **Source Files**: AISApi.scala

#### `POST /accounts/v2_1_1.1/getAccounts`

- **Endpoint Name**: `getAccounts`
- **Summary**: Get information about all user's payment account
- **Description**: ${mockedDataText(true)} User identification based on access token
- **Path Parameters**: A, _
- **Source Files**: AISApi.scala

#### `POST /accounts/v2_1_1.1/getHolds`

- **Endpoint Name**: `getHolds`
- **Summary**: Get list of user's holded operations
- **Description**: ${mockedDataText(true)}
- **Path Parameters**: H, _
- **Source Files**: AISApi.scala

#### `POST /accounts/v2_1_1.1/getTransactionDetail`

- **Endpoint Name**: `getTransactionDetail`
- **Summary**: Get detailed information about user's single transaction
- **Description**: ${mockedDataText(true)}
- **Path Parameters**: D, T, _
- **Source Files**: AISApi.scala

#### `POST /accounts/v2_1_1.1/getTransactionsCancelled`

- **Endpoint Name**: `getTransactionsCancelled`
- **Summary**: Get list of user cancelled transactions
- **Description**: ${mockedDataText(true)}
- **Path Parameters**: T, C, _
- **Error Codes**: UnknownError
- **Source Files**: AISApi.scala

#### `POST /accounts/v2_1_1.1/getTransactionsDone`

- **Endpoint Name**: `getTransactionsDone`
- **Summary**: Get list of user done transactions
- **Description**: ${mockedDataText(true)}
- **Path Parameters**: D, T, _
- **Error Codes**: UnknownError
- **Source Files**: AISApi.scala

#### `POST /accounts/v2_1_1.1/getTransactionsPending`

- **Endpoint Name**: `getTransactionsPending`
- **Summary**: Get list of user's pending transactions
- **Description**: ${mockedDataText(true)}
- **Path Parameters**: T, P, _
- **Error Codes**: UnknownError
- **Source Files**: AISApi.scala

#### `POST /accounts/v2_1_1.1/getTransactionsRejected`

- **Endpoint Name**: `getTransactionsRejected`
- **Summary**: Get list of user's rejected transactions
- **Description**: ${mockedDataText(true)}
- **Path Parameters**: R, T, _
- **Error Codes**: UnknownError
- **Source Files**: AISApi.scala

#### `POST /accounts/v2_1_1.1/getTransactionsScheduled`

- **Endpoint Name**: `getTransactionsScheduled`
- **Summary**: Get list of user scheduled transactions
- **Description**: ${mockedDataText(true)}
- **Path Parameters**: S, T, _
- **Source Files**: AISApi.scala

#### `POST /auth/v2_1_1.1/authorize`

- **Endpoint Name**: `authorize`
- **Summary**: Requests OAuth2 authorization code
- **Description**: ${mockedDataText(true)} Requests OAuth2 authorization code
- **Path Parameters**: _
- **Source Files**: ASApi.scala

#### `POST /auth/v2_1_1.1/authorizeExt`

- **Endpoint Name**: `authorizeExt`
- **Summary**: Requests OAuth2 authorization code based on One-time authorization code issued by External Authoriza
- **Description**: ${mockedDataText(true)} Requests OAuth2 authorization code based One-time authorization code issued by External Authorization Tool. Authorization code will be delivered to TPP as callback request from ASPSP if PSU authentication is confirmed by EAT. Callback function must provide similar notification also in case of unsuccessful authentication or its abandonment.
- **Path Parameters**: _, E
- **Source Files**: ASApi.scala

#### `POST /auth/v2_1_1.1/token`

- **Endpoint Name**: `token`
- **Summary**: Requests OAuth2 access token value
- **Description**: ${mockedDataText(true)} Requests OAuth2 access token value
- **Path Parameters**: _
- **Source Files**: ASApi.scala

#### `POST /confirmation/v2_1_1.1/getConfirmationOfFunds`

- **Endpoint Name**: `getConfirmationOfFunds`
- **Summary**: Confirmation of the availability of funds
- **Description**: ${mockedDataText(true)} Confirming the availability on the payers account of the amount necessary to execute the payment transaction, as defined in Art. 65 PSD2.
- **Path Parameters**: O, C, _, F
- **Source Files**: CAFApi.scala

#### `POST /payments/v2_1_1.1/EEA`

- **Endpoint Name**: `eEA`
- **Summary**: Initiate SEPA foreign transfers
- **Description**: ${mockedDataText(true)}
- **Path Parameters**: _, EEA
- **Source Files**: PISApi.scala

#### `POST /payments/v2_1_1.1/bundle`

- **Endpoint Name**: `bundle`
- **Summary**: Initiate many transfers as bundle
- **Description**: ${mockedDataText(true)}
- **Path Parameters**: _
- **Source Files**: PISApi.scala

#### `POST /payments/v2_1_1.1/cancelPayments`

- **Endpoint Name**: `cancelPayments`
- **Summary**: Cancelation of future dated payment
- **Description**: ${mockedDataText(true)}
- **Path Parameters**: P, _
- **Source Files**: PISApi.scala

#### `POST /payments/v2_1_1.1/cancelRecurringPayment`

- **Endpoint Name**: `cancelRecurringPayment`
- **Summary**: Cancelation of recurring payment
- **Description**: ${mockedDataText(true)}
- **Path Parameters**: R, P, _
- **Source Files**: PISApi.scala

#### `POST /payments/v2_1_1.1/domestic`

- **Endpoint Name**: `domestic`
- **Summary**: Initiate domestic transfer
- **Description**: ${mockedDataText(true)}
- **Path Parameters**: _
- **Source Files**: PISApi.scala

#### `POST /payments/v2_1_1.1/getBundle`

- **Endpoint Name**: `getBundle`
- **Summary**: Get the status of bundle of payments
- **Description**: ${mockedDataText(true)}
- **Path Parameters**: B, _
- **Source Files**: PISApi.scala

#### `POST /payments/v2_1_1.1/getMultiplePayments`

- **Endpoint Name**: `getMultiplePayments`
- **Summary**: Get the status of multiple payments
- **Description**: ${mockedDataText(true)}
- **Path Parameters**: M, P, _
- **Source Files**: PISApi.scala

#### `POST /payments/v2_1_1.1/getPayment`

- **Endpoint Name**: `getPayment`
- **Summary**: Get the status of payment
- **Description**: ${mockedDataText(true)}
- **Path Parameters**: P, _
- **Error Codes**: UnknownError
- **Source Files**: PISApi.scala

#### `POST /payments/v2_1_1.1/getRecurringPayment`

- **Endpoint Name**: `getRecurringPayment`
- **Summary**: Get the status of recurring payment
- **Description**: ${mockedDataText(true)}
- **Path Parameters**: R, P, _
- **Source Files**: PISApi.scala

#### `POST /payments/v2_1_1.1/nonEEA`

- **Endpoint Name**: `nonEEA`
- **Summary**: Initiate non SEPA foreign transfers
- **Description**: ${mockedDataText(true)}
- **Path Parameters**: _, EEA
- **Source Files**: PISApi.scala

#### `POST /payments/v2_1_1.1/recurring`

- **Endpoint Name**: `recurring`
- **Summary**: Defines new recurring payment
- **Description**: ${mockedDataText(true)}
- **Path Parameters**: _
- **Source Files**: PISApi.scala

#### `POST /payments/v2_1_1.1/tax`

- **Endpoint Name**: `tax`
- **Summary**: Initiate tax transfer
- **Description**: ${mockedDataText(true)}
- **Path Parameters**: _
- **Source Files**: PISApi.scala


---

## API Context: STET (French) (v1_4)

**Total Endpoints**: 11

### GET Endpoints (6)

#### `GET /accounts`

- **Endpoint Name**: `accountsGet`
- **Summary**: Retrieval of the PSU accounts (AISP)
- **Description**: ${mockedDataText(false)} ### Description This call returns all payment accounts that are relevant the PSU on behalf of whom the AISP is connected. Thanks to HYPERMEDIA, each account is returned with the links aiming to ease access to the relevant transactions and balances. The result may be subject to pagination (i.e. retrieving a partial result in case of having too many results) through a set of pages by the ASPSP. Thereafter, the AISP may ask for the first, next, previous or last page of resu
- **Route Pattern**: `case "accounts" :: Nil JsonGet`
- **Source Files**: AISPApi.scala

#### `GET /accounts/ACCOUNTRESOURCEID/balances`

- **Endpoint Name**: `accountsBalancesGet`
- **Summary**: Retrieval of an account balances report (AISP)
- **Description**: ${mockedDataText(false)} ### Description This call returns a set of balances for a given PSU account that is specified by the AISP through an account resource Identification ### Prerequisites * The TPP has been registered by the Registration Authority for the AISP role * The TPP and the PSU have a contract that has been enrolled by the ASPSP * At this step, the ASPSP has delivered an OAUTH2 "Authorization Code" or "Resource Owner Password" access token to the TPP (cf. § 3.4.2). * The TPP and the
- **Route Pattern**: `"accounts" :: accountresourceid:: "balances" :: Nil`
- **Path Parameters**: ACCOUNTRESOURCEID
- **Source Files**: AISPApi.scala

#### `GET /accounts/ACCOUNTRESOURCEID/transactions`

- **Endpoint Name**: `accountsTransactionsGet`
- **Summary**: Retrieval of an account transaction set (AISP)
- **Description**: ${mockedDataText(false)} ### Description This call returns transactions for an account for a given PSU account that is specified by the AISP through an account resource identification. The request may use some filter parameter in order to restrict the query * on a given imputation date range * past a given incremental technical identification The result may be subject to pagination (i.e. retrieving a partial result in case of having too many results) through a set of pages by the ASPSP. Thereaft
- **Route Pattern**: `"accounts" :: accountresourceid:: "transactions" :: Nil`
- **Path Parameters**: ACCOUNTRESOURCEID
- **Source Files**: AISPApi.scala

#### `GET /end-user-identity`

- **Endpoint Name**: `endUserIdentityGet`
- **Summary**: Retrieval of the identity of the end-user (AISP)
- **Description**: ${mockedDataText(true)} ### Description This call returns the identity of the PSU (end-user). ### Prerequisites * The TPP has been registered by the Registration Authority for the AISP role. * The TPP and the PSU have a contract that has been enrolled by the ASPSP * The TPP and the ASPSP have successfully processed a mutual check and authentication * The TPP has presented its OAUTH2 "Authorization Code" or "Resource Owner Password" access token which allows the ASPSP to identify the relevant PSU
- **Route Pattern**: `case "end-user-identity" :: Nil JsonGet`
- **Source Files**: AISPApi.scala

#### `GET /payment-requests/PAYMENTREQUESTRESOURCEID`

- **Endpoint Name**: `paymentRequestsGet`
- **Summary**: Retrieval of a payment request (PISP)
- **Description**: ${mockedDataText(true)} ### Description The following use cases can be applied: * retrieval of a payment request on behalf of a merchant * retrieval of a transfer request on behalf of the account's owner * retrieval of a standing-order request on behalf of the account's owner The PISP has sent a Request through a POST command. The ASPSP has registered the Request, updated if necessary the relevant identifiers in order to avoid duplicates and returned the location of the updated Request. The PISP
- **Route Pattern**: `"payment-requests" :: paymentrequestresourceid :: Nil`
- **Path Parameters**: PAYMENTREQUESTRESOURCEID
- **Source Files**: PISPApi.scala

#### `GET /trusted-beneficiaries`

- **Endpoint Name**: `trustedBeneficiariesGet`
- **Summary**: Retrieval of the trusted beneficiaries list (AISP)
- **Description**: ${mockedDataText(true)} ### Description This call returns all trusted beneficiaries that have been set by the PSU. Those beneficiaries can benefit from an SCA exemption during payment initiation. The result may be subject to pagination (i.e. retrieving a partial result in case of having too many results) through a set of pages by the ASPSP. Thereafter, the AISP may ask for the first, next, previous or last page of results. ### Prerequisites * The TPP has been registered by the Registration Autho
- **Route Pattern**: `case "trusted-beneficiaries" :: Nil JsonGet`
- **Source Files**: AISPApi.scala

### POST Endpoints (3)

#### `POST /funds-confirmations`

- **Endpoint Name**: `fundsConfirmationsPost`
- **Summary**: Payment coverage check request (CBPII)
- **Description**: ${mockedDataText(true)} ### Description The CBPII can ask an ASPSP to check if a given amount can be covered by the liquidity that is available on a PSU cash account or payment card. ### Prerequisites * The TPP has been registered by the Registration Authority for the CBPII role * The TPP and the PSU have a contract that has been registered by the ASPSP * The TPP and the ASPSP have successfully processed a mutual check and authentication * The TPP has presented its OAUTH2 "Authorization Code", "
- **Route Pattern**: `case "funds-confirmations" :: Nil JsonPost`
- **Source Files**: CBPIIApi.scala

#### `POST /payment-requests`

- **Endpoint Name**: `paymentRequestsPost`
- **Summary**: Payment request initiation (PISP)
- **Description**: ${mockedDataText(true)} ### Description The following use cases can be applied: * payment request on behalf of a merchant * transfer request on behalf of the account's owner * standing-order request on behalf of the account's owner #### Data content A payment request or a transfer request might embed several payment instructions having * one single execution date or multiple execution dates * one single beneficiary or multiple beneficiaries Having at the same time multiple beneficiaries and mult
- **Source Files**: PISPApi.scala

#### `POST /payment-requests/PAYMENTREQUESTRESOURCEID/confirmation`

- **Endpoint Name**: `paymentRequestConfirmationPost`
- **Summary**: Confirmation of a payment request or a modification request (PISP)
- **Description**: ${mockedDataText(true)} ### Description The PISP confirms one of the following requests * payment request on behalf of a merchant * transfer request on behalf of the account's owner * standing-order request on behalf of the account's owner The ASPSP answers with a status of the relevant request and the subsequent Credit Transfer. ### Prerequisites * The TPP has been registered by the Registration Authority for the PISP role * The TPP was provided with an OAUTH2 "Client Credential" access token b
- **Route Pattern**: `"payment-requests" :: paymentrequestresourceid:: "confirmation" :: Nil`
- **Path Parameters**: PAYMENTREQUESTRESOURCEID
- **Source Files**: PISPApi.scala

### PUT Endpoints (2)

#### `PUT /consents`

- **Endpoint Name**: `consentsPut`
- **Summary**: Forwarding the PSU consent (AISP)
- **Description**: ${mockedDataText(true)} ### Description In the mixed detailed consent on accounts * the AISP captures the consent of the PSU * then it forwards this consent to the ASPSP This consent replaces any prior consent that was previously sent by the AISP. ### Prerequisites * The TPP has been registered by the Registration Authority for the AISP role. * The TPP and the PSU have a contract that has been enrolled by the ASPSP * The TPP and the ASPSP have successfully processed a mutual check and authentica
- **Route Pattern**: `case "consents" :: Nil JsonPut`
- **Source Files**: AISPApi.scala

#### `PUT /payment-requests/PAYMENTREQUESTRESOURCEID`

- **Endpoint Name**: `paymentRequestPut`
- **Summary**: Modification of a Payment/Transfer Request (PISP)
- **Description**: ${mockedDataText(true)} ### Description The PISP sent a Payment/Transfer Request through a POST command. The ASPSP registered the Payment/Transfer Request, updated if necessary the relevant identifiers in order to avoid duplicates and returned the location of the updated Request. The PISP got the Payment/Transfer Request that has been updated with the resource identifiers, and eventually the status of the Payment/Transfer Request and the status of the subsequent credit transfer. The PISP request
- **Path Parameters**: PAYMENTREQUESTRESOURCEID
- **Source Files**: PISPApi.scala


---

## API Context: UK Open Banking (v2_0_0)

**Total Endpoints**: 5

### GET Endpoints (5)

#### `GET /accounts`

- **Endpoint Name**: `getAccountList`
- **Summary**: UK Open Banking: Get Account List
- **Description**:  Reads a list of bank accounts, with balances where required. It is assumed that a consent of the PSU to this access is already given and stored on the ASPSP system. ${userAuthenticationMessage(true)} This call is work in progress - Experimental! 
- **Route Pattern**: `case "accounts" :: Nil JsonGet`
- **Tags**: UKOpenBanking, Account, PrivateData
- **Request Body**: SwaggerDefinitionsJSON
- **Error Codes**: UnknownError
- **Source Files**: APIMethods_UKOpenBanking_200.scala

#### `GET /accounts/ACCOUNT_ID`

- **Endpoint Name**: `getAccount`
- **Summary**: UK Open Banking: Get Account
- **Description**:  Reads a bank account, with balances where required. It is assumed that a consent of the PSU to this access is already given and stored on the ASPSP system. ${userAuthenticationMessage(true)} This call is work in progress - Experimental! 
- **Route Pattern**: `"accounts" :: AccountId(accountId) :: Nil`
- **Tags**: UKOpenBanking, Account, PrivateData
- **Path Parameters**: ACCOUNT_ID
- **Request Body**: SwaggerDefinitionsJSON
- **Error Codes**: UnknownError
- **Source Files**: APIMethods_UKOpenBanking_200.scala

#### `GET /accounts/ACCOUNT_ID/balances`

- **Endpoint Name**: `getAccountBalances`
- **Summary**: UK Open Banking: Get Account Balances
- **Description**:  An AISP may retrieve the account balance information resource for a specific AccountId (which is retrieved in the call to GET /accounts). ${userAuthenticationMessage(true)} This call is work in progress - Experimental! 
- **Route Pattern**: `"accounts" :: AccountId(accountId) :: "balances" :: Nil`
- **Tags**: UKOpenBanking, Account, PrivateData
- **Path Parameters**: ACCOUNT_ID
- **Request Body**: SwaggerDefinitionsJSON
- **Error Codes**: UnknownError
- **Source Files**: APIMethods_UKOpenBanking_200.scala

#### `GET /accounts/ACCOUNT_ID/transactions`

- **Endpoint Name**: `getAccountTransactions`
- **Summary**: UK Open Banking: Get Account Transactions
- **Description**:  Reads account data from a given account addressed by “account-id”. ${userAuthenticationMessage(true)} This call is work in progress - Experimental! 
- **Route Pattern**: `"accounts" :: AccountId(accountId) :: "transactions" :: Nil`
- **Tags**: Psd2, UKOpenBanking, Transaction, PrivateData
- **Path Parameters**: ACCOUNT_ID
- **Request Body**: SwaggerDefinitionsJSON
- **Error Codes**: UnknownError
- **Source Files**: APIMethods_UKOpenBanking_200.scala

#### `GET /balances`

- **Endpoint Name**: `getBalances`
- **Summary**: UK Open Banking: Get Balances
- **Description**:  If an ASPSP has implemented the bulk retrieval endpoints - an AISP may optionally retrieve the account information resources in bulk. This will retrieve the resources for all authorised accounts linked to the account-request. ${userAuthenticationMessage(true)} This call is work in progress - Experimental! 
- **Route Pattern**: `case "balances" :: Nil JsonGet`
- **Tags**: UKOpenBanking, Account, PrivateData
- **Request Body**: SwaggerDefinitionsJSON
- **Error Codes**: UnknownError
- **Source Files**: APIMethods_UKOpenBanking_200.scala


---

## API Context: UK Open Banking (v3_1_0)

**Total Endpoints**: 66

### GET Endpoints (46)

#### `GET /account-access-consents/CONSENT_ID`

- **Endpoint Name**: `getAccountAccessConsentsConsentId`
- **Summary**: Get Account Access Consents
- **Description**:  ${mockedDataText(false)} Get Account Access Consents 
- **Route Pattern**: `"account-access-consents" :: consentId :: Nil`
- **Path Parameters**: CONSENT_ID
- **Source Files**: AccountAccessApi.scala

#### `GET /accounts`

- **Endpoint Name**: `getAccounts`
- **Summary**: Get Accounts
- **Route Pattern**: `case "accounts" :: Nil JsonGet`
- **Source Files**: AccountsApi.scala

#### `GET /accounts/ACCOUNTID/beneficiaries`

- **Endpoint Name**: `getAccountsAccountIdBeneficiaries`
- **Summary**: Get Beneficiaries
- **Description**: ${mockedDataText(true)}
- **Route Pattern**: `"accounts" :: accountid:: "beneficiaries" :: Nil`
- **Path Parameters**: ACCOUNTID
- **Source Files**: BeneficiariesApi.scala

#### `GET /accounts/ACCOUNTID/direct-debits`

- **Endpoint Name**: `getAccountsAccountIdDirectDebits`
- **Summary**: Get Direct Debits
- **Description**: ${mockedDataText(true)}
- **Route Pattern**: `"accounts" :: accountid:: "direct-debits" :: Nil`
- **Path Parameters**: ACCOUNTID
- **Source Files**: DirectDebitsApi.scala

#### `GET /accounts/ACCOUNTID/offers`

- **Endpoint Name**: `getAccountsAccountIdOffers`
- **Summary**: Get Offers
- **Description**: ${mockedDataText(true)}
- **Route Pattern**: `"accounts" :: accountid:: "offers" :: Nil`
- **Path Parameters**: ACCOUNTID
- **Source Files**: OffersApi.scala

#### `GET /accounts/ACCOUNTID/party`

- **Endpoint Name**: `getAccountsAccountIdParty`
- **Summary**: Get Party
- **Description**: ${mockedDataText(true)}
- **Route Pattern**: `"accounts" :: accountid:: "party" :: Nil`
- **Path Parameters**: ACCOUNTID
- **Source Files**: PartysApi.scala

#### `GET /accounts/ACCOUNTID/product`

- **Endpoint Name**: `getAccountsAccountIdProduct`
- **Summary**: Get Products
- **Description**: ${mockedDataText(true)}
- **Route Pattern**: `"accounts" :: accountid:: "product" :: Nil`
- **Path Parameters**: ACCOUNTID
- **Source Files**: ProductsApi.scala

#### `GET /accounts/ACCOUNTID/scheduled-payments`

- **Endpoint Name**: `getAccountsAccountIdScheduledPayments`
- **Summary**: Get Scheduled Payments
- **Description**: ${mockedDataText(true)}
- **Route Pattern**: `"accounts" :: accountid:: "scheduled-payments" :: Nil`
- **Path Parameters**: ACCOUNTID
- **Source Files**: ScheduledPaymentsApi.scala

#### `GET /accounts/ACCOUNTID/standing-orders`

- **Endpoint Name**: `getAccountsAccountIdStandingOrders`
- **Summary**: Get Standing Orders
- **Description**: ${mockedDataText(true)}
- **Route Pattern**: `"accounts" :: accountid:: "standing-orders" :: Nil`
- **Path Parameters**: ACCOUNTID
- **Source Files**: StandingOrdersApi.scala

#### `GET /accounts/ACCOUNTID/statements`

- **Endpoint Name**: `getAccountsAccountIdStatements`
- **Summary**: Get Statements
- **Description**: ${mockedDataText(true)}
- **Path Parameters**: ACCOUNTID
- **Source Files**: StatementsApi.scala

#### `GET /accounts/ACCOUNTID/statements/STATEMENTID`

- **Endpoint Name**: `getAccountsAccountIdStatementsStatementId`
- **Summary**: Get Statements
- **Description**: ${mockedDataText(true)}
- **Path Parameters**: ACCOUNTID, STATEMENTID
- **Source Files**: StatementsApi.scala

#### `GET /accounts/ACCOUNTID/statements/STATEMENTID/file`

- **Endpoint Name**: `getAccountsAccountIdStatementsStatementIdFile`
- **Summary**: Get Statements
- **Description**: ${mockedDataText(true)}
- **Route Pattern**: `"accounts" :: accountid:: "statements" :: statementid:: "file" :: Nil`
- **Path Parameters**: ACCOUNTID, STATEMENTID
- **Source Files**: StatementsApi.scala

#### `GET /accounts/ACCOUNTID/statements/STATEMENTID/transactions`

- **Endpoint Name**: `getAccountsAccountIdStatementsStatementIdTransactions`
- **Summary**: Get Transactions
- **Description**: ${mockedDataText(true)}
- **Path Parameters**: ACCOUNTID, STATEMENTID
- **Source Files**: TransactionsApi.scala, StatementsApi.scala

#### `GET /accounts/ACCOUNT_ID`

- **Endpoint Name**: `getAccountsAccountId`
- **Summary**: Get Accounts
- **Route Pattern**: `"accounts" :: AccountId(accountId) :: Nil`
- **Path Parameters**: ACCOUNT_ID
- **Source Files**: AccountsApi.scala

#### `GET /accounts/ACCOUNT_ID/balances`

- **Endpoint Name**: `getAccountsAccountIdBalances`
- **Summary**: Get Balances
- **Route Pattern**: `"accounts" :: AccountId(accountId):: "balances" :: Nil`
- **Path Parameters**: ACCOUNT_ID
- **Source Files**: BalancesApi.scala

#### `GET /accounts/ACCOUNT_ID/transactions`

- **Endpoint Name**: `getAccountsAccountIdTransactions`
- **Summary**: Get Transactions
- **Path Parameters**: ACCOUNT_ID
- **Source Files**: TransactionsApi.scala

#### `GET /balances`

- **Endpoint Name**: `getBalances`
- **Summary**: Get Balances
- **Route Pattern**: `case "balances" :: Nil JsonGet`
- **Source Files**: BalancesApi.scala

#### `GET /beneficiaries`

- **Endpoint Name**: `getBeneficiaries`
- **Summary**: Get Beneficiaries
- **Description**: ${mockedDataText(true)}
- **Route Pattern**: `case "beneficiaries" :: Nil JsonGet`
- **Source Files**: BeneficiariesApi.scala

#### `GET /direct-debits`

- **Endpoint Name**: `getDirectDebits`
- **Summary**: Get Direct Debits
- **Description**: ${mockedDataText(true)}
- **Route Pattern**: `case "direct-debits" :: Nil JsonGet`
- **Source Files**: DirectDebitsApi.scala

#### `GET /domestic-payment-consents/CONSENTID`

- **Endpoint Name**: `getDomesticPaymentConsentsConsentId`
- **Summary**: Get Domestic Payment Consents
- **Description**: ${mockedDataText(true)}
- **Route Pattern**: `"domestic-payment-consents" :: consentid :: Nil`
- **Path Parameters**: CONSENTID
- **Source Files**: DomesticPaymentsApi.scala

#### `GET /domestic-payment-consents/CONSENTID/funds-confirmation`

- **Endpoint Name**: `getDomesticPaymentConsentsConsentIdFundsConfirmation`
- **Summary**: Get Domestic Payment Consents Funds Confirmation
- **Description**: ${mockedDataText(true)}
- **Route Pattern**: `"domestic-payment-consents" :: consentid:: "funds-confirmation" :: Nil`
- **Path Parameters**: CONSENTID
- **Source Files**: DomesticPaymentsApi.scala

#### `GET /domestic-payments/DOMESTICPAYMENTID`

- **Endpoint Name**: `getDomesticPaymentsDomesticPaymentId`
- **Summary**: Get Domestic Payments
- **Description**: ${mockedDataText(true)}
- **Route Pattern**: `"domestic-payments" :: domesticpaymentid :: Nil`
- **Path Parameters**: DOMESTICPAYMENTID
- **Source Files**: DomesticPaymentsApi.scala

#### `GET /domestic-scheduled-payment-consents/CONSENTID`

- **Endpoint Name**: `getDomesticScheduledPaymentConsentsConsentId`
- **Summary**: Get Domestic Scheduled Payment Consents
- **Description**: ${mockedDataText(true)}
- **Route Pattern**: `"domestic-scheduled-payment-consents" :: consentid :: Nil`
- **Path Parameters**: CONSENTID
- **Source Files**: DomesticScheduledPaymentsApi.scala

#### `GET /domestic-scheduled-payments/DOMESTICSCHEDULEDPAYMENTID`

- **Endpoint Name**: `getDomesticScheduledPaymentsDomesticScheduledPaymentId`
- **Summary**: Get Domestic Scheduled Payments
- **Description**: ${mockedDataText(true)}
- **Route Pattern**: `"domestic-scheduled-payments" :: domesticscheduledpaymentid :: Nil`
- **Path Parameters**: DOMESTICSCHEDULEDPAYMENTID
- **Source Files**: DomesticScheduledPaymentsApi.scala

#### `GET /domestic-standing-order-consents/CONSENTID`

- **Endpoint Name**: `getDomesticStandingOrderConsentsConsentId`
- **Summary**: Get Domestic Standing Order Consents
- **Description**: ${mockedDataText(true)}
- **Route Pattern**: `"domestic-standing-order-consents" :: consentid :: Nil`
- **Path Parameters**: CONSENTID
- **Source Files**: DomesticStandingOrdersApi.scala

#### `GET /domestic-standing-orders/DOMESTICSTANDINGORDERID`

- **Endpoint Name**: `getDomesticStandingOrdersDomesticStandingOrderId`
- **Summary**: Get Domestic Standing Orders
- **Description**: ${mockedDataText(true)}
- **Route Pattern**: `"domestic-standing-orders" :: domesticstandingorderid :: Nil`
- **Path Parameters**: DOMESTICSTANDINGORDERID
- **Source Files**: DomesticStandingOrdersApi.scala

#### `GET /file-payment-consents/CONSENTID`

- **Endpoint Name**: `getFilePaymentConsentsConsentId`
- **Summary**: Get File Payment Consents
- **Description**: ${mockedDataText(true)}
- **Route Pattern**: `"file-payment-consents" :: consentid :: Nil`
- **Path Parameters**: CONSENTID
- **Source Files**: FilePaymentsApi.scala

#### `GET /file-payment-consents/CONSENTID/file`

- **Endpoint Name**: `getFilePaymentConsentsConsentIdFile`
- **Summary**: Get File Payment Consents
- **Description**: ${mockedDataText(true)}
- **Route Pattern**: `"file-payment-consents" :: consentid:: "file" :: Nil`
- **Path Parameters**: CONSENTID
- **Source Files**: FilePaymentsApi.scala

#### `GET /file-payments/FILEPAYMENTID`

- **Endpoint Name**: `getFilePaymentsFilePaymentId`
- **Summary**: Get File Payments
- **Description**: ${mockedDataText(true)}
- **Route Pattern**: `"file-payments" :: filepaymentid :: Nil`
- **Path Parameters**: FILEPAYMENTID
- **Source Files**: FilePaymentsApi.scala

#### `GET /file-payments/FILEPAYMENTID/report-file`

- **Endpoint Name**: `getFilePaymentsFilePaymentIdReportFile`
- **Summary**: Get File Payments
- **Description**: ${mockedDataText(true)}
- **Route Pattern**: `"file-payments" :: filepaymentid:: "report-file" :: Nil`
- **Path Parameters**: FILEPAYMENTID
- **Source Files**: FilePaymentsApi.scala

#### `GET /funds-confirmation-consents/CONSENTID`

- **Endpoint Name**: `getFundsConfirmationConsentsConsentId`
- **Summary**: Get Funds Confirmation Consent
- **Description**: ${mockedDataText(true)}
- **Route Pattern**: `"funds-confirmation-consents" :: consentid :: Nil`
- **Path Parameters**: CONSENTID
- **Source Files**: FundsConfirmationsApi.scala

#### `GET /international-payment-consents/CONSENTID`

- **Endpoint Name**: `getInternationalPaymentConsentsConsentId`
- **Summary**: Get International Payment Consents
- **Description**: ${mockedDataText(true)}
- **Path Parameters**: CONSENTID
- **Source Files**: InternationalPaymentsApi.scala

#### `GET /international-payment-consents/CONSENTID/funds-confirmation`

- **Endpoint Name**: `getInternationalPaymentConsentsConsentIdFundsConfirmation`
- **Summary**: Get International Payment Consents Funds Confirmation
- **Description**: ${mockedDataText(true)}
- **Route Pattern**: `"international-payment-consents" :: consentid:: "funds-confirmation" :: Nil`
- **Path Parameters**: CONSENTID
- **Source Files**: InternationalPaymentsApi.scala

#### `GET /international-payments/INTERNATIONALPAYMENTID`

- **Endpoint Name**: `getInternationalPaymentsInternationalPaymentId`
- **Summary**: Get International Payments
- **Description**: ${mockedDataText(true)}
- **Path Parameters**: INTERNATIONALPAYMENTID
- **Source Files**: InternationalPaymentsApi.scala

#### `GET /international-scheduled-payment-consents/CONSENTID`

- **Endpoint Name**: `getInternationalScheduledPaymentConsentsConsentId`
- **Summary**: Get International Scheduled Payment Consents
- **Description**: ${mockedDataText(true)}
- **Path Parameters**: CONSENTID
- **Source Files**: InternationalScheduledPaymentsApi.scala

#### `GET /international-scheduled-payment-consents/CONSENTID/funds-confirmation`

- **Endpoint Name**: `getInternationalScheduledPaymentConsentsConsentIdFundsConfirmation`
- **Summary**: Get International Scheduled Payment Consents Funds Confirmation
- **Description**: ${mockedDataText(true)}
- **Route Pattern**: `"international-scheduled-payment-consents" :: consentid:: "funds-confirmation" :: Nil`
- **Path Parameters**: CONSENTID
- **Source Files**: InternationalScheduledPaymentsApi.scala

#### `GET /international-scheduled-payments/INTERNATIONALSCHEDULEDPAYMENTID`

- **Endpoint Name**: `getInternationalScheduledPaymentsInternationalScheduledPaymentId`
- **Summary**: Get International Scheduled Payments
- **Description**: ${mockedDataText(true)}
- **Path Parameters**: INTERNATIONALSCHEDULEDPAYMENTID
- **Source Files**: InternationalScheduledPaymentsApi.scala

#### `GET /international-standing-order-consents/CONSENTID`

- **Endpoint Name**: `getInternationalStandingOrderConsentsConsentId`
- **Summary**: Get International Standing Order Consents
- **Description**: ${mockedDataText(true)}
- **Route Pattern**: `"international-standing-order-consents" :: consentid :: Nil`
- **Path Parameters**: CONSENTID
- **Source Files**: InternationalStandingOrdersApi.scala

#### `GET /international-standing-orders/INTERNATIONALSTANDINGORDERPAYMENTID`

- **Endpoint Name**: `getInternationalStandingOrdersInternationalStandingOrderPaymentId`
- **Summary**: Get International Standing Orders
- **Description**: ${mockedDataText(true)}
- **Route Pattern**: `"international-standing-orders" :: internationalstandingorderpaymentid :: Nil`
- **Path Parameters**: INTERNATIONALSTANDINGORDERPAYMENTID
- **Source Files**: InternationalStandingOrdersApi.scala

#### `GET /offers`

- **Endpoint Name**: `getOffers`
- **Summary**: Get Offers
- **Description**: ${mockedDataText(true)}
- **Route Pattern**: `case "offers" :: Nil JsonGet`
- **Source Files**: OffersApi.scala

#### `GET /party`

- **Endpoint Name**: `getParty`
- **Summary**: Get Party
- **Description**: ${mockedDataText(true)}
- **Route Pattern**: `case "party" :: Nil JsonGet`
- **Source Files**: PartysApi.scala

#### `GET /products`

- **Endpoint Name**: `getProducts`
- **Summary**: Get Products
- **Description**: ${mockedDataText(true)}
- **Route Pattern**: `case "products" :: Nil JsonGet`
- **Source Files**: ProductsApi.scala

#### `GET /scheduled-payments`

- **Endpoint Name**: `getScheduledPayments`
- **Summary**: Get Scheduled Payments
- **Description**: ${mockedDataText(true)}
- **Route Pattern**: `case "scheduled-payments" :: Nil JsonGet`
- **Source Files**: ScheduledPaymentsApi.scala

#### `GET /standing-orders`

- **Endpoint Name**: `getStandingOrders`
- **Summary**: Get Standing Orders
- **Description**: ${mockedDataText(true)}
- **Route Pattern**: `case "standing-orders" :: Nil JsonGet`
- **Source Files**: StandingOrdersApi.scala

#### `GET /statements`

- **Endpoint Name**: `getStatements`
- **Summary**: Get Statements
- **Description**: ${mockedDataText(true)}
- **Source Files**: StatementsApi.scala

#### `GET /transactions`

- **Endpoint Name**: `getTransactions`
- **Summary**: Get Transactions
- **Source Files**: TransactionsApi.scala

### POST Endpoints (18)

#### `POST /account-access-consents`

- **Endpoint Name**: `createAccountAccessConsents`
- **Summary**: Create Account Access Consents
- **Description**: ${mockedDataText(false)} Create Account Access Consents 
- **Route Pattern**: `case "account-access-consents" :: Nil JsonPost`
- **Source Files**: AccountAccessApi.scala

#### `POST /domestic-payment-consents`

- **Endpoint Name**: `createDomesticPaymentConsents`
- **Summary**: Create Domestic Payment Consents
- **Description**: ${mockedDataText(true)}
- **Route Pattern**: `case "domestic-payment-consents" :: Nil JsonPost`
- **Source Files**: DomesticPaymentsApi.scala

#### `POST /domestic-payments`

- **Endpoint Name**: `createDomesticPayments`
- **Summary**: Create Domestic Payments
- **Description**: ${mockedDataText(true)}
- **Route Pattern**: `case "domestic-payments" :: Nil JsonPost`
- **Source Files**: DomesticPaymentsApi.scala

#### `POST /domestic-scheduled-payment-consents`

- **Endpoint Name**: `createDomesticScheduledPaymentConsents`
- **Summary**: Create Domestic Scheduled Payment Consents
- **Description**: ${mockedDataText(true)}
- **Route Pattern**: `case "domestic-scheduled-payment-consents" :: Nil JsonPost`
- **Source Files**: DomesticScheduledPaymentsApi.scala

#### `POST /domestic-scheduled-payments`

- **Endpoint Name**: `createDomesticScheduledPayments`
- **Summary**: Create Domestic Scheduled Payments
- **Description**: ${mockedDataText(true)}
- **Route Pattern**: `case "domestic-scheduled-payments" :: Nil JsonPost`
- **Source Files**: DomesticScheduledPaymentsApi.scala

#### `POST /domestic-standing-order-consents`

- **Endpoint Name**: `createDomesticStandingOrderConsents`
- **Summary**: Create Domestic Standing Order Consents
- **Description**: ${mockedDataText(true)}
- **Route Pattern**: `case "domestic-standing-order-consents" :: Nil JsonPost`
- **Source Files**: DomesticStandingOrdersApi.scala

#### `POST /domestic-standing-orders`

- **Endpoint Name**: `createDomesticStandingOrders`
- **Summary**: Create Domestic Standing Orders
- **Description**: ${mockedDataText(true)}
- **Route Pattern**: `case "domestic-standing-orders" :: Nil JsonPost`
- **Source Files**: DomesticStandingOrdersApi.scala

#### `POST /file-payment-consents`

- **Endpoint Name**: `createFilePaymentConsents`
- **Summary**: Create File Payment Consents
- **Description**: ${mockedDataText(true)}
- **Route Pattern**: `case "file-payment-consents" :: Nil JsonPost`
- **Source Files**: FilePaymentsApi.scala

#### `POST /file-payment-consents/CONSENTID/file`

- **Endpoint Name**: `createFilePaymentConsentsConsentIdFile`
- **Summary**: Create File Payment Consents
- **Description**: ${mockedDataText(true)}
- **Route Pattern**: `"file-payment-consents" :: consentid:: "file" :: Nil`
- **Path Parameters**: CONSENTID
- **Source Files**: FilePaymentsApi.scala

#### `POST /file-payments`

- **Endpoint Name**: `createFilePayments`
- **Summary**: Create File Payments
- **Description**: ${mockedDataText(true)}
- **Route Pattern**: `case "file-payments" :: Nil JsonPost`
- **Source Files**: FilePaymentsApi.scala

#### `POST /funds-confirmation-consents`

- **Endpoint Name**: `createFundsConfirmationConsents`
- **Summary**: Create Funds Confirmation Consent
- **Description**: ${mockedDataText(true)}
- **Route Pattern**: `case "funds-confirmation-consents" :: Nil JsonPost`
- **Source Files**: FundsConfirmationsApi.scala

#### `POST /funds-confirmations`

- **Endpoint Name**: `createFundsConfirmations`
- **Summary**: Create Funds Confirmation
- **Description**: ${mockedDataText(true)}
- **Route Pattern**: `case "funds-confirmations" :: Nil JsonPost`
- **Source Files**: FundsConfirmationsApi.scala

#### `POST /international-payment-consents`

- **Endpoint Name**: `createInternationalPaymentConsents`
- **Summary**: Create International Payment Consents
- **Description**: ${mockedDataText(true)}
- **Source Files**: InternationalPaymentsApi.scala

#### `POST /international-payments`

- **Endpoint Name**: `createInternationalPayments`
- **Summary**: Create International Payments
- **Description**: ${mockedDataText(true)}
- **Route Pattern**: `case "international-payments" :: Nil JsonPost`
- **Source Files**: InternationalPaymentsApi.scala

#### `POST /international-scheduled-payment-consents`

- **Endpoint Name**: `createInternationalScheduledPaymentConsents`
- **Summary**: Create International Scheduled Payment Consents
- **Description**: ${mockedDataText(true)}
- **Source Files**: InternationalScheduledPaymentsApi.scala

#### `POST /international-scheduled-payments`

- **Endpoint Name**: `createInternationalScheduledPayments`
- **Summary**: Create International Scheduled Payments
- **Description**: ${mockedDataText(true)}
- **Source Files**: InternationalScheduledPaymentsApi.scala

#### `POST /international-standing-order-consents`

- **Endpoint Name**: `createInternationalStandingOrderConsents`
- **Summary**: Create International Standing Order Consents
- **Description**: ${mockedDataText(true)}
- **Route Pattern**: `case "international-standing-order-consents" :: Nil JsonPost`
- **Source Files**: InternationalStandingOrdersApi.scala

#### `POST /international-standing-orders`

- **Endpoint Name**: `createInternationalStandingOrders`
- **Summary**: Create International Standing Orders
- **Description**: ${mockedDataText(true)}
- **Route Pattern**: `case "international-standing-orders" :: Nil JsonPost`
- **Source Files**: InternationalStandingOrdersApi.scala

### DELETE Endpoints (2)

#### `DELETE /account-access-consents/CONSENT_ID`

- **Endpoint Name**: `deleteAccountAccessConsentsConsentId`
- **Summary**: Delete Account Access Consents
- **Description**: ${mockedDataText(false)} Delete Account Access Consents 
- **Route Pattern**: `"account-access-consents" :: consentId :: Nil`
- **Path Parameters**: CONSENT_ID
- **Source Files**: AccountAccessApi.scala

#### `DELETE /funds-confirmation-consents/CONSENTID`

- **Endpoint Name**: `deleteFundsConfirmationConsentsConsentId`
- **Summary**: Delete Funds Confirmation Consent
- **Description**: ${mockedDataText(true)}
- **Route Pattern**: `"funds-confirmation-consents" :: consentid :: Nil`
- **Path Parameters**: CONSENTID
- **Error Codes**: UnknownError
- **Source Files**: FundsConfirmationsApi.scala


---

## API Context: v1_4_0

**Total Endpoints**: 2

### GET Endpoints (1)

#### `GET /dummy`

- **Endpoint Name**: `testResourceDoc`
- **Summary**: I am only a test resource Doc
- **Description**:  #This should be H1 ##This should be H2 ###This should be H3 ####This should be H4 Here is a list with two items: * One * Two There are underscores by them selves _ There are _underscores_ around a word There are underscores_in_words There are 'underscores_in_words_inside_quotes' There are (underscores_in_words_in_brackets) _etc_...
- **Route Pattern**: `case "dummy" :: Nil JsonGet`
- **Tags**: OldStyle, Documentation
- **Request Body**: apiInfoJSON
- **Error Codes**: UnknownError
- **Source Files**: APIMethods140.scala

### POST Endpoints (1)

#### `POST /banks/BANK_ID/customer/CUSTOMER_ID/messages`

- **Endpoint Name**: `addCustomerMessage`
- **Summary**: Create Customer Message
- **Description**: Returns information about branches for a single bank specified by BANK_ID including: * Name * Address * Geo Location * License the data under this endpoint is released under ${urlParametersDocument(false, false)} You can use the url query parameters *limit* and *offset* for pagination ${userAuthenticationMessage(!getBranchesIsPublic)}
- **Route Pattern**: `"banks" :: BankId(bankId) :: "customer" :: customerId ::  "messages" :: Nil`
- **Tags**: Person, Message, Customer
- **Path Parameters**: BANK_ID, CUSTOMER_ID
- **Error Codes**: UnknownError
- **Source Files**: APIMethods140.scala


---

## API Context: v2_2_0

**Total Endpoints**: 18

### GET Endpoints (8)

#### `GET /banks/BANK_ID/accounts/ACCOUNT_ID/VIEW_ID/counterparties`

- **Endpoint Name**: `getExplicitCounterpartiesForAccount`
- **Summary**: Get Counterparties (Explicit)
- **Description**: This endpoints gets the explicit Counterparties on an Account / View. For a general introduction to Counterparties in OBP, see ${Glossary.getGlossaryItemLink("Counterparties")} ${userAuthenticationMessage(true)} 
- **Route Pattern**: `"banks" :: BankId(bankId) :: "accounts" :: AccountId(accountId) :: ViewId(viewId) :: "counterparties" :: Nil`
- **Tags**: Psd2, Account, Counterparty, PSD2PIS
- **Path Parameters**: BANK_ID, ACCOUNT_ID, VIEW_ID
- **Error Codes**: BankAccountNotFound, ViewNotFound, UnknownError
- **Source Files**: APIMethods220.scala

#### `GET /banks/BANK_ID/accounts/ACCOUNT_ID/VIEW_ID/counterparties/COUNTERPARTY_ID`

- **Endpoint Name**: `getExplicitCounterpartyById`
- **Summary**: Get Counterparty by Counterparty Id (Explicit)
- **Description**: Information returned about the Counterparty specified by COUNTERPARTY_ID: ${userAuthenticationMessage(true)} 
- **Route Pattern**: `"banks" :: BankId(bankId) :: "accounts" :: AccountId(accountId) :: ViewId(viewId) :: "counterparties" :: CounterpartyId(counterpartyId) :: Nil`
- **Tags**: Psd2, CounterpartyMetaData, Counterparty, PSD2PIS
- **Path Parameters**: BANK_ID, COUNTERPARTY_ID, ACCOUNT_ID, VIEW_ID
- **Request Body**: createCounterpartyWithMetadataJSON
- **Error Codes**: BankNotFound, UnknownError
- **Source Files**: APIMethods220.scala

#### `GET /banks/BANK_ID/accounts/ACCOUNT_ID/views`

- **Endpoint Name**: `getViewsForBankAccount`
- **Summary**: Get Views for Account
- **Description**: #Views Views in Open Bank Project provide a mechanism for fine grained access control and delegation to Accounts and Transactions. Account holders use the 'owner' view by default. Delegated access is made through other views for example 'accountants', 'share-holders' or 'tagging-application'. Views can be created via the API and each view has a list of entitlements. Views on accounts and transactions filter the underlying data to redact certain fields for certain users. For instance the balance 
- **Route Pattern**: `"banks" :: BankId(bankId) :: "accounts" :: AccountId(accountId) :: "views" :: Nil`
- **Path Parameters**: BANK_ID, ACCOUNT_ID
- **Source Files**: APIMethods220.scala

#### `GET /banks/BANK_ID/fx/FROM_CURRENCY_CODE/TO_CURRENCY_CODE`

- **Endpoint Name**: `getCurrentFxRate`
- **Summary**: Get Current FxRate
- **Description**: Get the latest FX rate specified by BANK_ID, FROM_CURRENCY_CODE and TO_CURRENCY_CODE OBP may try different sources of FX rate information depending on the Connector in operation. For example we want to convert EUR => USD: OBP will: 1st try - Connector (database, core banking system or external FX service) 2nd try part 1 - fallbackexchangerates/eur.json 2nd try part 2 - fallbackexchangerates/usd.json (the inverse rate is used) 3rd try - Hardcoded map of FX rates. ![FX Flow](https://user-images.gi
- **Route Pattern**: `"banks" :: BankId(bankId) :: "fx" :: fromCurrencyCode :: toCurrencyCode :: Nil`
- **Tags**: Fx
- **Path Parameters**: BANK_ID, FROM_CURRENCY_CODE, TO_CURRENCY_CODE
- **Request Body**: fXRateJSON
- **Error Codes**: UnknownError
- **Source Files**: APIMethods220.scala

#### `GET /config`

- **Endpoint Name**: `config`
- **Summary**: Get API Configuration
- **Description**: Returns information about: * API Config * Akka ports * Elastic search ports * Cached function
- **Route Pattern**: `case "config" :: Nil JsonGet`
- **Required Roles**: GetConfig
- **Request Body**: configurationJSON
- **Source Files**: APIMethods220.scala

#### `GET /management/connector/metrics`

- **Endpoint Name**: `getConnectorMetrics`
- **Summary**: Get Connector Metrics
- **Description**: Get the all metrics require CanGetConnectorMetrics role Filters Part 1.*filtering* (no wilde cards etc.) parameters to GET /management/connector/metrics Should be able to filter on the following metrics fields eg: /management/connector/metrics?from_date=$DateWithMsExampleString&to_date=$DateWithMsExampleString&limit=50&offset=2 1 from_date (defaults to one week before current date): eg:from_date=$DateWithMsExampleString 2 to_date (defaults to current date) eg:to_date=$DateWithMsExampleString 3 l
- **Route Pattern**: `"management" :: "connector" :: "metrics" :: Nil`
- **Tags**: Api, Metric
- **Required Roles**: GetConnectorMetrics
- **Error Codes**: UnknownError
- **Source Files**: APIMethods220.scala

#### `GET /message-docs/CONNECTOR`

- **Endpoint Name**: `getMessageDocs`
- **Summary**: Get Message Docs
- **Description**: These message docs provide example messages sent by OBP to the (RabbitMq) message queue for processing by the Core Banking / Payment system Adapter - together with an example expected response and possible error codes. Integrators can use these messages to build Adapters that provide core banking services to OBP. Note: API Explorer provides a Message Docs page where these messages are displayed. `CONNECTOR`: rest_vMar2019, stored_procedure_vDec2019 ...
- **Route Pattern**: `"message-docs" :: connector :: Nil`
- **Tags**: Api, Documentation
- **Path Parameters**: CONNECTOR
- **Request Body**: bankJSONV220
- **Response Body**: bankJSONV220
- **Error Codes**: UnknownError
- **Source Files**: APIMethods220.scala

#### `GET /root`

- **Endpoint Name**: `root`
- **Summary**: Get API Info (root)
- **Description**: Returns information about: * API version * Hosted by information * Git Commit
- **Request Body**: apiInfoJSON
- **Source Files**: APIMethods220.scala

### POST Endpoints (6)

#### `POST /banks`

- **Endpoint Name**: `createBank`
- **Summary**: Create Bank
- **Description**: Create a new bank (Authenticated access). ${userAuthenticationMessage(true) } 
- **Route Pattern**: `case "banks" :: Nil JsonPost`
- **Tags**: OldStyle, Bank
- **Required Roles**: CreateBank
- **Request Body**: bankJSONV220
- **Response Body**: bankJSONV220
- **Error Codes**: UnknownError
- **Source Files**: APIMethods220.scala

#### `POST /banks/BANK_ID/accounts/ACCOUNT_ID/VIEW_ID/counterparties`

- **Endpoint Name**: `createCounterparty`
- **Summary**: Create Counterparty (Explicit)
- **Description**: Create Counterparty (Explicit) for an Account. In OBP, there are two types of Counterparty. * Explicit Counterparties (those here) which we create explicitly and are used in COUNTERPARTY Transaction Requests * Implicit Counterparties (AKA Other Accounts) which are generated automatically from the other sides of Transactions. Explicit Counterparties are created for the account / view They are how the user of the view (e.g. account owner) refers to the other side of the transaction name : the huma
- **Route Pattern**: `"banks" :: BankId(bankId) :: "accounts" :: AccountId(accountId) :: ViewId(viewId) :: "counterparties" :: Nil`
- **Path Parameters**: BANK_ID, ACCOUNT_ID, VIEW_ID
- **Source Files**: APIMethods220.scala

#### `POST /banks/BANK_ID/accounts/ACCOUNT_ID/views`

- **Endpoint Name**: `createViewForBankAccount`
- **Summary**: Create View
- **Description**: #Create a view on bank account ${userAuthenticationMessage(true)} and the user needs to have access to the owner view. The 'alias' field in the JSON can take one of three values: * _public_: to use the public alias if there is one specified for the other account. * _private_: to use the private alias if there is one specified for the other account. * _''(empty string)_: to use no alias; the view shows the real name of the other account. The 'hide_metadata_if_alias_used' field in the JSON can tak
- **Route Pattern**: `"banks" :: BankId(bankId) :: "accounts" :: AccountId(accountId) :: "views" :: Nil`
- **Tags**: OldStyle, Account, View
- **Path Parameters**: BANK_ID, ACCOUNT_ID
- **Request Body**: viewJSONV220
- **Response Body**: viewJSONV220
- **Error Codes**: BankAccountNotFound, UnknownError
- **Source Files**: APIMethods220.scala

#### `POST /banks/BANK_ID/atms`

- **Endpoint Name**: `createAtm`
- **Summary**: Create ATM
- **Description**: Create ATM for the Bank. ${userAuthenticationMessage(true) } 
- **Route Pattern**: `"banks" :: BankId(bankId) :: "atms" :: Nil`
- **Tags**: ATM
- **Required Roles**: CreateAtm, CreateAtmAtAnyBank
- **Path Parameters**: BANK_ID
- **Error Codes**: BankNotFound, UnknownError
- **Source Files**: APIMethods220.scala

#### `POST /banks/BANK_ID/branches`

- **Endpoint Name**: `createBranch`
- **Summary**: Create Branch
- **Description**: Create Branch for the Bank. ${userAuthenticationMessage(true) } 
- **Route Pattern**: `"banks" :: BankId(bankId) :: "branches" :: Nil`
- **Tags**: OpenData, Branch
- **Required Roles**: CreateBranch, CreateBranchAtAnyBank
- **Path Parameters**: BANK_ID
- **Error Codes**: BankNotFound, UnknownError
- **Source Files**: APIMethods220.scala

#### `POST /management/consumers`

- **Endpoint Name**: `createConsumer`
- **Summary**: Post a Consumer
- **Description**: Create a Consumer (Authenticated access). 
- **Route Pattern**: `"management" :: "consumers" :: Nil`
- **Tags**: OldStyle, Consumer
- **Required Roles**: CreateConsumer
- **Request Body**: ConsumerPostJSON
- **Error Codes**: UnknownError
- **Source Files**: APIMethods220.scala

### PUT Endpoints (4)

#### `PUT /banks/BANK_ID/accounts/ACCOUNT_ID`

- **Endpoint Name**: `createAccount`
- **Summary**: Create Account
- **Description**: Create Account at bank specified by BANK_ID with Id specified by ACCOUNT_ID. The User can create an Account for themself or an Account for another User if they have CanCreateAccount role. If USER_ID is not specified the account will be owned by the logged in User. The type field should be a product_code from Product. Note: The Amount must be zero.
- **Route Pattern**: `"banks" :: BankId(bankId) :: "accounts" :: AccountId(accountId) :: Nil`
- **Tags**: Account, Onboarding
- **Required Roles**: CreateAccount
- **Path Parameters**: BANK_ID, ACCOUNT_ID
- **Request Body**: createAccountJSONV220
- **Response Body**: createAccountJSONV220
- **Error Codes**: UserNotFound, BankNotFound, UnknownError
- **Source Files**: APIMethods220.scala

#### `PUT /banks/BANK_ID/accounts/ACCOUNT_ID/views/VIEW_ID`

- **Endpoint Name**: `updateViewForBankAccount`
- **Summary**: Update View
- **Description**: Update an existing view on a bank account ${userAuthenticationMessage(true)} and the user needs to have access to the owner view. The json sent is the same as during view creation (above), with one difference: the 'name' field of a view is not editable (it is only set when a view is created)
- **Route Pattern**: `"banks" :: BankId(bankId) :: "accounts" :: AccountId(accountId) :: "views" :: ViewId(viewId) :: Nil`
- **Tags**: OldStyle, Account, View
- **Path Parameters**: BANK_ID, ACCOUNT_ID, VIEW_ID
- **Request Body**: viewJSONV220
- **Response Body**: viewJSONV220
- **Error Codes**: BankAccountNotFound, UnknownError
- **Source Files**: APIMethods220.scala

#### `PUT /banks/BANK_ID/fx`

- **Endpoint Name**: `createFx`
- **Summary**: Create Fx
- **Description**: Create or Update Fx for the Bank. Example: “from_currency_code”:“EUR”, “to_currency_code”:“USD”, “conversion_value”: 1.136305, “inverse_conversion_value”: 1 / 1.136305 = 0.8800454103431737, Thus 1 Euro = 1.136305 US Dollar and 1 US Dollar = 0.8800 Euro ${userAuthenticationMessage(true) } 
- **Route Pattern**: `"banks" :: BankId(bankId) :: "fx" :: Nil`
- **Tags**: Fx
- **Required Roles**: CreateFxRateAtAnyBank, CreateFxRate
- **Path Parameters**: BANK_ID
- **Error Codes**: BankNotFound, UnknownError
- **Source Files**: APIMethods220.scala

#### `PUT /banks/BANK_ID/products`

- **Endpoint Name**: `createProduct`
- **Summary**: Create Product
- **Description**: Create or Update Product for the Bank. ${userAuthenticationMessage(true) } 
- **Route Pattern**: `"banks" :: BankId(bankId) :: "products" :: Nil`
- **Tags**: Product
- **Required Roles**: CreateProductAtAnyBank, CreateProduct
- **Path Parameters**: BANK_ID
- **Error Codes**: BankNotFound, UnknownError
- **Source Files**: APIMethods220.scala


---

## API Context: v3_0_0

**Total Endpoints**: 45

### GET Endpoints (34)

#### `GET /api/glossary`

- **Endpoint Name**: `getApiGlossary`
- **Summary**: Get Glossary of the API
- **Description**: Get API Glossary Returns the glossary of the API 
- **Route Pattern**: `"api" :: "glossary" :: Nil`
- **Error Codes**: UnknownError
- **Source Files**: APIMethods300.scala

#### `GET /banks`

- **Endpoint Name**: `getBanks`
- **Summary**: Get Banks
- **Description**: Get banks on this API instance Returns a list of banks supported on this server: * ID used as parameter in URLs * Short and full name of bank * Logo URL * Website
- **Route Pattern**: `case "banks" :: Nil JsonGet`
- **Request Body**: banksJSON
- **Source Files**: APIMethods300.scala

#### `GET /banks/BANK_ID`

- **Endpoint Name**: `bankById`
- **Summary**: Get Bank
- **Description**: Get the bank specified by BANK_ID Returns information about a single bank specified by BANK_ID including: * Short and full name of bank * Logo URL * Website
- **Route Pattern**: `"banks" :: BankId(bankId) :: Nil`
- **Path Parameters**: BANK_ID
- **Request Body**: createBankJSON400
- **Source Files**: APIMethods300.scala

#### `GET /banks/BANK_ID/accounts-held`

- **Endpoint Name**: `getAccountsHeld`
- **Summary**: Get Accounts Held
- **Description**: Get Accounts held by the current User if even the User has not been assigned the owner View yet. Can be used to onboard the account to the API - since all other account and transaction endpoints require views to be assigned. ${accountTypeFilterText("/banks/BANK_ID/accounts-held")} 
- **Route Pattern**: `"banks" :: BankId(bankId) :: "accounts-held" :: Nil`
- **Tags**: Psd2, Account, PSD2AIS, View
- **Path Parameters**: BANK_ID
- **Request Body**: createCoreAccountsByCoreAccountsJSON
- **Error Codes**: UnknownError
- **Source Files**: APIMethods300.scala

#### `GET /banks/BANK_ID/accounts/ACCOUNT_ID/VIEW_ID/account`

- **Endpoint Name**: `getPrivateAccountById`
- **Summary**: Get Account by Id (Full)
- **Description**: Information returned about an account specified by ACCOUNT_ID as moderated by the view (VIEW_ID): * Number * Owners * Type * Balance * IBAN * Available views (sorted by short_name) More details about the data moderation by the view [here](#1_2_1-getViewsForBankAccount). PSD2 Context: PSD2 requires customers to have access to their account information via third party applications. This call provides balance and other account information via delegated authentication using OAuth. Authentication is 
- **Route Pattern**: `"banks" :: BankId(bankId) :: "accounts" :: AccountId(accountId) :: ViewId(viewId) :: "account" :: Nil`
- **Path Parameters**: BANK_ID, ACCOUNT_ID, VIEW_ID
- **Request Body**: createCoreBankAccountJSON
- **Source Files**: APIMethods300.scala

#### `GET /banks/BANK_ID/accounts/ACCOUNT_ID/VIEW_ID/other_accounts`

- **Endpoint Name**: `getOtherAccountsForBankAccount`
- **Summary**: Get Other Accounts of one Account
- **Description**: Returns data about all the other accounts that have shared at least one transaction with the ACCOUNT_ID at BANK_ID. ${userAuthenticationMessage(false)} Authentication is required if the view VIEW_ID is not public.
- **Route Pattern**: `"banks" :: BankId(bankId) :: "accounts" :: AccountId(accountId) :: ViewId(viewId) :: "other_accounts" :: Nil`
- **Tags**: Account, Counterparty
- **Path Parameters**: BANK_ID, ACCOUNT_ID, VIEW_ID, _
- **Error Codes**: BankAccountNotFound, ViewNotFound, UnknownError
- **Source Files**: APIMethods300.scala

#### `GET /banks/BANK_ID/accounts/ACCOUNT_ID/VIEW_ID/other_accounts/OTHER_ACCOUNT_ID`

- **Endpoint Name**: `getOtherAccountByIdForBankAccount`
- **Summary**: Get Other Account by Id
- **Description**: Returns data about the Other Account that has shared at least one transaction with ACCOUNT_ID at BANK_ID. ${userAuthenticationMessage(false)} Authentication is required if the view is not public.
- **Route Pattern**: `"banks" :: BankId(bankId) :: "accounts" :: AccountId(accountId) :: ViewId(viewId) :: "other_accounts":: other_account_id :: Nil`
- **Tags**: Account, Counterparty
- **Path Parameters**: BANK_ID, VIEW_ID, OTHER_ACCOUNT_ID, ACCOUNT_ID, _
- **Error Codes**: BankAccountNotFound, ViewNotFound, UnknownError
- **Source Files**: APIMethods300.scala

#### `GET /banks/BANK_ID/accounts/ACCOUNT_ID/VIEW_ID/transactions`

- **Endpoint Name**: `getTransactionsForBankAccount`
- **Summary**: Get Transactions for Account (Full)
- **Description**: Returns transactions list of the account specified by ACCOUNT_ID and [moderated](#1_2_1-getViewsForBankAccount) by the view (VIEW_ID). ${userAuthenticationMessage(false)} Authentication is required if the view is not public. ${urlParametersDocument(true, true)} 
- **Route Pattern**: `"banks" :: BankId(bankId) :: "accounts" :: AccountId(accountId) :: ViewId(viewId) :: "transactions" :: Nil`
- **Tags**: Account, Transaction
- **Path Parameters**: BANK_ID, ACCOUNT_ID, VIEW_ID
- **Error Codes**: FilterDateFormatError, FilterOffersetError, BankAccountNotFound, ViewNotFound, UnknownError, FilterSortDirectionError, FilterLimitError
- **Source Files**: APIMethods300.scala

#### `GET /banks/BANK_ID/accounts/ACCOUNT_ID/permissions/PROVIDER/PROVIDER_ID`

- **Endpoint Name**: `getPermissionForUserForBankAccount`
- **Summary**: Get Account access for User
- **Description**: Returns the list of the views at BANK_ID for account ACCOUNT_ID that a user identified by PROVIDER_ID at their provider PROVIDER has access to. All url parameters must be [%-encoded](http://en.wikipedia.org/wiki/Percent-encoding), which is often especially relevant for USER_ID and PROVIDER. ${userAuthenticationMessage(true)} The user needs to have access to the owner view.
- **Route Pattern**: `"banks" :: BankId(bankId) :: "accounts" :: AccountId(accountId) :: "permissions" :: provider :: providerId :: Nil`
- **Tags**: Account, View, User
- **Path Parameters**: BANK_ID, ACCOUNT_ID, PROVIDER, PROVIDER_ID
- **Error Codes**: AccountNotFound, BankNotFound, UnknownError
- **Source Files**: APIMethods300.scala

#### `GET /banks/BANK_ID/accounts/ACCOUNT_ID/views`

- **Endpoint Name**: `getViewsForBankAccount`
- **Summary**: Get Views for Account
- **Description**: #Views Views in Open Bank Project provide a mechanism for fine grained access control and delegation to Accounts and Transactions. Account holders use the 'owner' view by default. Delegated access is made through other views for example 'accountants', 'share-holders' or 'tagging-application'. Views can be created via the API and each view has a list of entitlements. Views on accounts and transactions filter the underlying data to redact certain fields for certain users. For instance the balance 
- **Route Pattern**: `"banks" :: BankId(bankId) :: "accounts" :: AccountId(accountId) :: "views" :: Nil`
- **Path Parameters**: BANK_ID, ACCOUNT_ID
- **Source Files**: APIMethods300.scala

#### `GET /banks/BANK_ID/accounts/account_ids/private`

- **Endpoint Name**: `getPrivateAccountIdsbyBankId`
- **Summary**: Get Accounts at Bank (IDs only)
- **Description**: Returns only the list of accounts ids at BANK_ID that the user has access to. Each account must have at least one private View. For each account the API returns its account ID. If you want to see more information on the Views, use the Account Detail call. ${accountTypeFilterText("/banks/BANK_ID/accounts/account_ids/private")} ${userAuthenticationMessage(true)}
- **Tags**: Psd2, Account, PSD2AIS
- **Path Parameters**: BANK_ID, _
- **Error Codes**: BankNotFound, UnknownError
- **Source Files**: APIMethods300.scala

#### `GET /banks/BANK_ID/accounts/private`

- **Endpoint Name**: `privateAccountsAtOneBank`
- **Summary**: Get Accounts at Bank (Minimal)
- **Description**: Returns the minimal list of private accounts at BANK_ID that the user has access to. For each account, the API returns the ID, routing addresses and the views available to the current user. If you want to see more information on the Views, use the Account Detail call. ${accountTypeFilterText("/banks/BANK_ID/accounts/private")} ${userAuthenticationMessage(true)}
- **Route Pattern**: `"banks" :: BankId(bankId) :: "accounts" :: "private" :: Nil`
- **Tags**: Psd2, Account, PSD2AIS
- **Path Parameters**: BANK_ID
- **Request Body**: createCoreAccountsByCoreAccountsJSON
- **Error Codes**: BankNotFound, UnknownError
- **Source Files**: APIMethods300.scala

#### `GET /banks/BANK_ID/adapter`

- **Endpoint Name**: `getAdapterInfoForBank`
- **Summary**: Get Adapter Info for a bank
- **Description**: Get basic information about the Adapter listening on behalf of this bank. ${userAuthenticationMessage(true)} 
- **Route Pattern**: `"banks" :: BankId(bankId) :: "adapter" :: Nil`
- **Tags**: Api
- **Required Roles**: GetAdapterInfoAtOneBank
- **Path Parameters**: BANK_ID
- **Error Codes**: UnknownError
- **Source Files**: APIMethods300.scala

#### `GET /banks/BANK_ID/atms`

- **Endpoint Name**: `getAtms`
- **Summary**: Get Bank ATMS
- **Description**: Returns information about ATMs for a single bank specified by BANK_ID including: * Address * Geo Location * License the data under this endpoint is released under Pagination: By default, 100 records are returned. You can use the url query parameters *limit* and *offset* for pagination ${userAuthenticationMessage(!getAtmsIsPublic)}
- **Route Pattern**: `"banks" :: BankId(bankId) :: "atms" :: Nil`
- **Tags**: ATM
- **Path Parameters**: BANK_ID
- **Error Codes**: BankNotFound, UnknownError
- **Source Files**: APIMethods300.scala

#### `GET /banks/BANK_ID/atms/ATM_ID`

- **Endpoint Name**: `getAtm`
- **Summary**: Get Bank ATM
- **Description**: Returns information about ATM for a single bank specified by BANK_ID and ATM_ID including: * Address * Geo Location * License the data under this endpoint is released under ${userAuthenticationMessage(!getAtmsIsPublic)}
- **Route Pattern**: `"banks" :: BankId(bankId) :: "atms" :: AtmId(atmId) :: Nil`
- **Tags**: ATM
- **Path Parameters**: BANK_ID, ATM_ID
- **Error Codes**: BankNotFound, AtmNotFound, UnknownError
- **Source Files**: APIMethods300.scala

#### `GET /banks/BANK_ID/branches`

- **Endpoint Name**: `getBranches`
- **Summary**: Get Branches for a Bank
- **Description**: Returns information about branches for a single bank specified by BANK_ID including: * Name * Address * Geo Location * License the data under this endpoint is released under * Structured opening hours * Accessible flag * Branch Type * More Info Pagination: By default, 50 records are returned. You can use the url query parameters *limit* and *offset* for pagination You can also use the follow url query parameters: - city - string, find Branches those in this city, optional - withinMetersOf - numb
- **Route Pattern**: `"banks" :: BankId(bankId) :: "branches" :: Nil`
- **Tags**: Bank, Branch
- **Path Parameters**: BANK_ID
- **Error Codes**: BranchesNotFound, BankNotFound, UnknownError
- **Source Files**: APIMethods300.scala

#### `GET /banks/BANK_ID/branches/BRANCH_ID`

- **Endpoint Name**: `getBranch`
- **Summary**: Get Branch
- **Description**: Returns information about a single Branch specified by BANK_ID and BRANCH_ID including: * Name * Address * Geo Location * License the data under this endpoint is released under. ${userAuthenticationMessage(!getBranchesIsPublic)}
- **Route Pattern**: `"banks" :: BankId(bankId) :: "branches" :: BranchId(branchId) :: Nil`
- **Tags**: Bank, Branch
- **Path Parameters**: BANK_ID, BRANCH_ID
- **Error Codes**: BranchNotFound, UnknownError
- **Source Files**: APIMethods300.scala

#### `GET /banks/BANK_ID/firehose/accounts/ACCOUNT_ID/views/VIEW_ID/transactions`

- **Endpoint Name**: `getFirehoseTransactionsForBankAccount`
- **Summary**: Get Firehose Transactions for Account
- **Description**:  Get Transactions for an Account that has a firehose View. Allows bulk access to an account's transactions. User must have the CanUseFirehoseAtAnyBank Role To find ACCOUNT_IDs, use the getFirehoseAccountsAtOneBank call. For VIEW_ID try 'owner' ${urlParametersDocument(true, true)} ${userAuthenticationMessage(true)} 
- **Route Pattern**: `"banks" :: BankId(bankId):: "firehose" :: "accounts" ::  AccountId(accountId) :: "views" :: ViewId(viewId) :: "transactions" :: Nil`
- **Tags**: FirehoseData, AccountFirehose, Transaction, TransactionFirehose
- **Required Roles**: UseAccountFirehose, UseAccountFirehoseAtAnyBank
- **Path Parameters**: BANK_ID, ACCOUNT_ID, VIEW_ID
- **Error Codes**: UnknownError
- **Source Files**: APIMethods300.scala

#### `GET /banks/BANK_ID/firehose/accounts/views/VIEW_ID`

- **Endpoint Name**: `getFirehoseAccountsAtOneBank`
- **Summary**: Get Firehose Accounts at Bank
- **Description**:  Get Accounts which have a firehose view assigned to them. This endpoint allows bulk access to accounts. Requires the CanUseFirehoseAtAnyBank Role To be shown on the list, each Account must have a firehose View linked to it. A firehose view has is_firehose = true For VIEW_ID try 'owner' optional request parameters for filter with attributes URL params example: /banks/some-bank-id/firehose/accounts/views/owner?&limit=50&offset=1 to invalid Browser cache, add timestamp query parameter as follow, t
- **Tags**: FirehoseData, AccountFirehose, Account
- **Required Roles**: UseAccountFirehose, UseAccountFirehoseAtAnyBank
- **Path Parameters**: BANK_ID, VIEW_ID
- **Error Codes**: UnknownError
- **Source Files**: APIMethods300.scala

#### `GET /banks/BANK_ID/public/accounts/ACCOUNT_ID/VIEW_ID/account`

- **Endpoint Name**: `getPublicAccountById`
- **Summary**: Get Public Account by Id
- **Description**:  Returns information about an account that has a public view. The account is specified by ACCOUNT_ID. The information is moderated by the view specified by VIEW_ID. * Number * Owners * Type * Balance * Routing PSD2 Context: PSD2 requires customers to have access to their account information via third party applications. This call provides balance and other account information via delegated authentication using OAuth. ${userAuthenticationMessage(false)} 
- **Route Pattern**: `"banks" :: BankId(bankId) :: "public" :: "accounts" :: AccountId(accountId) :: ViewId(viewId) :: "account" :: Nil`
- **Path Parameters**: BANK_ID, ACCOUNT_ID, VIEW_ID
- **Request Body**: createCoreBankAccountJSON
- **Source Files**: APIMethods300.scala

#### `GET /consumers/CONSUMER_ID/scopes`

- **Endpoint Name**: `getScopes`
- **Summary**: Get Scopes for Consumer
- **Description**: Get all the scopes for an consumer specified by CONSUMER_ID ${userAuthenticationMessage(true)} 
- **Route Pattern**: `"consumers" :: consumerId :: "scopes" :: Nil`
- **Tags**: Scope, Consumer
- **Path Parameters**: CONSUMER_ID
- **Request Body**: createScopeJSONs
- **Error Codes**: EntitlementNotFound, UnknownError
- **Source Files**: APIMethods300.scala

#### `GET /entitlement-requests`

- **Endpoint Name**: `getAllEntitlementRequests`
- **Summary**: Get all Entitlement Requests
- **Description**:  Get all Entitlement Requests ${userAuthenticationMessage(true)}
- **Route Pattern**: `case "entitlement-requests" :: Nil JsonGet`
- **Tags**: Entitlement, Role, User
- **Required Roles**: GetEntitlementRequestsAtAnyBank
- **Request Body**: entitlementRequestsJSON
- **Error Codes**: UnknownError
- **Source Files**: APIMethods300.scala

#### `GET /management/aggregate-metrics`

- **Endpoint Name**: `getAggregateMetrics`
- **Summary**: Get Aggregate Metrics
- **Description**: Returns aggregate metrics on api usage eg. total count, response time (in ms), etc. Should be able to filter on the following fields eg: /management/aggregate-metrics?from_date=$DateWithMsExampleString&to_date=$DateWithMsExampleString&consumer_id=5 &user_id=66214b8e-259e-44ad-8868-3eb47be70646&implemented_by_partial_function=getTransactionsForBankAccount &implemented_in_version=v3.0.0&url=/obp/v3.0.0/banks/gh.29.uk/accounts/8ca8a7e4-6d02-48e3-a029-0b2bf89de9f0/owner/transactions &verb=GET&anon=f
- **Route Pattern**: `"management" :: "aggregate-metrics" :: Nil`
- **Source Files**: APIMethods300.scala

#### `GET /my/accounts`

- **Endpoint Name**: `corePrivateAccountsAllBanks`
- **Summary**: Get Accounts at all Banks (private)
- **Description**: Returns the list of accounts containing private views for the user. Each account lists the views available to the user. ${accountTypeFilterText("/my/accounts")} ${userAuthenticationMessage(true)} 
- **Route Pattern**: `"my" :: "accounts" :: Nil`
- **Tags**: Psd2, Account, PSD2AIS, PrivateData
- **Request Body**: createCoreAccountsByCoreAccountsJSON
- **Error Codes**: UnknownError
- **Source Files**: APIMethods300.scala

#### `GET /my/banks/BANK_ID/accounts/ACCOUNT_ID/account`

- **Endpoint Name**: `getCoreAccountById`
- **Summary**: Get Account by Id (Core)
- **Description**: Information returned about the account specified by ACCOUNT_ID: * Number - The human readable account number given by the bank that identifies the account. * Label - A label given by the owner of the account * Owners - Users that own this account * Type - The type of account * Balance - Currency and Value * Account Routings - A list that might include IBAN or national account identifiers * Account Rules - A list that might include Overdraft and other bank specific rules This call returns the own
- **Route Pattern**: `"my" :: "banks" :: BankId(bankId) :: "accounts" :: AccountId(accountId) :: "account" :: Nil`
- **Path Parameters**: BANK_ID, ACCOUNT_ID
- **Source Files**: APIMethods300.scala

#### `GET /my/banks/BANK_ID/accounts/ACCOUNT_ID/transactions`

- **Endpoint Name**: `getCoreTransactionsForBankAccount`
- **Summary**: Get Transactions for Account (Core)
- **Description**: Returns transactions list (Core info) of the account specified by ACCOUNT_ID. ${userAuthenticationMessage(true)} ${urlParametersDocument(true, true)} 
- **Route Pattern**: `"my" :: "banks" :: BankId(bankId) :: "accounts" :: AccountId(accountId) :: "transactions" :: Nil`
- **Tags**: Psd2, Account, Transaction, PSD2AIS
- **Path Parameters**: BANK_ID, ACCOUNT_ID
- **Error Codes**: FilterDateFormatError, FilterOffersetError, BankAccountNotFound, ViewNotFound, UnknownError, FilterSortDirectionError, FilterLimitError
- **Source Files**: APIMethods300.scala

#### `GET /my/entitlement-requests`

- **Endpoint Name**: `getEntitlementRequestsForCurrentUser`
- **Summary**: Get Entitlement Requests for the current User
- **Description**: Get Entitlement Requests for the current User. ${userAuthenticationMessage(true)} 
- **Route Pattern**: `"my" :: "entitlement-requests" :: Nil`
- **Tags**: Entitlement, Role, User
- **Request Body**: entitlementRequestsJSON
- **Error Codes**: UnknownError
- **Source Files**: APIMethods300.scala

#### `GET /my/entitlements`

- **Endpoint Name**: `getEntitlementsForCurrentUser`
- **Summary**: Get Entitlements for the current User
- **Description**: Get Entitlements for the current User. ${userAuthenticationMessage(true)} 
- **Route Pattern**: `"my" :: "entitlements" :: Nil`
- **Tags**: Entitlement, Role, User
- **Request Body**: entitlementJSONs
- **Response Body**: entitlementJSONs
- **Error Codes**: UnknownError
- **Source Files**: APIMethods300.scala

#### `GET /users`

- **Endpoint Name**: `getUsers`
- **Summary**: Get all Users
- **Description**: Get all users ${userAuthenticationMessage(true)} CanGetAnyUser entitlement is required, ${urlParametersDocument(false, false)} * locked_status (if null ignore) 
- **Route Pattern**: `case "users" :: Nil JsonGet`
- **Tags**: User
- **Required Roles**: GetAnyUser
- **Request Body**: createUserJSONs
- **Error Codes**: UnknownError
- **Source Files**: APIMethods300.scala

#### `GET /users/USER_ID/entitlement-requests`

- **Endpoint Name**: `getEntitlementRequests`
- **Summary**: Get Entitlement Requests for a User
- **Description**: Get Entitlement Requests for a User. ${userAuthenticationMessage(true)} 
- **Route Pattern**: `"users" :: userId :: "entitlement-requests" :: Nil`
- **Tags**: Entitlement, Role, User
- **Required Roles**: GetEntitlementRequestsAtAnyBank
- **Path Parameters**: USER_ID
- **Request Body**: entitlementRequestsJSON
- **Error Codes**: UnknownError
- **Source Files**: APIMethods300.scala

#### `GET /users/current/customers`

- **Endpoint Name**: `getCustomersForUser`
- **Summary**: Get Customers for Current User
- **Description**: Gets all Customers that are linked to a User. ${userAuthenticationMessage(true)} 
- **Route Pattern**: `"users" :: "current" :: "customers" :: Nil`
- **Tags**: Customer, User
- **Error Codes**: UserCustomerLinksNotFound, UnknownError
- **Source Files**: APIMethods300.scala

#### `GET /users/email/EMAIL/terminator`

- **Endpoint Name**: `getUser`
- **Summary**: Get Users by Email Address
- **Description**: Get users by email address ${userAuthenticationMessage(true)} CanGetAnyUser entitlement is required, 
- **Route Pattern**: `"users" :: "email" :: email :: "terminator" :: Nil`
- **Tags**: User
- **Required Roles**: GetAnyUser
- **Path Parameters**: EMAIL
- **Request Body**: createUserJSONs
- **Error Codes**: UserNotFound, UnknownError
- **Source Files**: APIMethods300.scala

#### `GET /users/user_id/USER_ID`

- **Endpoint Name**: `getUserByUserId`
- **Summary**: Get User by USER_ID
- **Description**: Get user by USER_ID ${userAuthenticationMessage(true)} CanGetAnyUser entitlement is required, 
- **Route Pattern**: `"users" :: "user_id" :: userId :: Nil`
- **Tags**: User
- **Required Roles**: GetAnyUser
- **Path Parameters**: USER_ID, _
- **Request Body**: createUserJSON
- **Error Codes**: UserNotFound, UnknownError
- **Source Files**: APIMethods300.scala

#### `GET /users/username/USERNAME`

- **Endpoint Name**: `getUserByUsername`
- **Summary**: Get User by USERNAME
- **Description**: Get user by USERNAME ${userAuthenticationMessage(true)} CanGetAnyUser entitlement is required, 
- **Route Pattern**: `"users" :: "username" :: username :: Nil`
- **Tags**: User
- **Required Roles**: GetAnyUser
- **Path Parameters**: USERNAME
- **Request Body**: createUserJSON
- **Error Codes**: UserNotFound, UnknownError
- **Source Files**: APIMethods300.scala

### POST Endpoints (7)

#### `POST /banks/BANK_ID/accounts/ACCOUNT_ID/views`

- **Endpoint Name**: `createViewForBankAccount`
- **Summary**: Create Custom View
- **Description**: Create a custom view on bank account ${userAuthenticationMessage(true)} and the user needs to have access to the owner view. The 'alias' field in the JSON can take one of three values: * _public_: to use the public alias if there is one specified for the other account. * _private_: to use the private alias if there is one specified for the other account. * _''(empty string)_: to use no alias; the view shows the real name of the other account. The 'hide_metadata_if_alias_used' field in the JSON c
- **Route Pattern**: `"banks" :: BankId(bankId) :: "accounts" :: AccountId(accountId) :: "views" :: Nil`
- **Tags**: Account, View
- **Path Parameters**: BANK_ID, ACCOUNT_ID
- **Request Body**: SwaggerDefinitionsJSON
- **Error Codes**: BankAccountNotFound, UnknownError
- **Source Files**: APIMethods300.scala

#### `POST /banks/BANK_ID/atms`

- **Endpoint Name**: `createAtm`
- **Summary**: Create ATM
- **Description**: Create ATM for the Bank. ${userAuthenticationMessage(true) } 
- **Route Pattern**: `"banks" :: BankId(bankId) :: "atms" :: Nil`
- **Tags**: ATM
- **Required Roles**: CreateAtm, CreateAtmAtAnyBank
- **Path Parameters**: BANK_ID
- **Error Codes**: BankNotFound, UnknownError
- **Source Files**: APIMethods300.scala

#### `POST /banks/BANK_ID/branches`

- **Endpoint Name**: `createBranch`
- **Summary**: Create Branch
- **Description**: Create Branch for the Bank. ${userAuthenticationMessage(true) } 
- **Route Pattern**: `"banks" :: BankId(bankId) :: "branches" :: Nil`
- **Tags**: Branch
- **Required Roles**: CreateBranch, CreateBranchAtAnyBank
- **Path Parameters**: BANK_ID
- **Error Codes**: BankNotFound, UnknownError
- **Source Files**: APIMethods300.scala

#### `POST /consumers/CONSUMER_ID/scopes`

- **Endpoint Name**: `addScope`
- **Summary**: Create Scope for a Consumer
- **Description**: Create Scope. Grant Role to Consumer. Scopes are used to grant System or Bank level roles to the Consumer (App). (For Account level privileges, see Views) For a System level Role (.e.g CanGetAnyUser), set bank_id to an empty string i.e. "bank_id":"" For a Bank level Role (e.g. CanCreateAccount), set bank_id to a valid value e.g. "bank_id":"my-bank-id" 
- **Route Pattern**: `"consumers" :: consumerId :: "scopes" :: Nil`
- **Tags**: Scope, Consumer
- **Required Roles**: CreateScopeAtOneBank, CreateScopeAtAnyBank
- **Path Parameters**: CONSUMER_ID
- **Request Body**: SwaggerDefinitionsJSON
- **Error Codes**: ConsumerNotFound, UnknownError
- **Source Files**: APIMethods300.scala

#### `POST /entitlement-requests`

- **Endpoint Name**: `addEntitlementRequest`
- **Summary**: Create Entitlement Request for current User
- **Description**: Create Entitlement Request. Any logged in User can use this endpoint to request an Entitlement Entitlements are used to grant System or Bank level roles to Users. (For Account level privileges, see Views) For a System level Role (.e.g CanGetAnyUser), set bank_id to an empty string i.e. "bank_id":"" For a Bank level Role (e.g. CanCreateAccount), set bank_id to a valid value e.g. "bank_id":"my-bank-id" ${userAuthenticationMessage(true)} 
- **Route Pattern**: `case "entitlement-requests" :: Nil JsonPost`
- **Tags**: Entitlement, Role, User
- **Request Body**: SwaggerDefinitionsJSON
- **Error Codes**: UserNotFound, UnknownError
- **Source Files**: APIMethods300.scala

#### `POST /search/warehouse/INDEX`

- **Endpoint Name**: `dataWarehouseSearch`
- **Summary**: Data Warehouse Search
- **Description**:  Search the data warehouse and get row level results. ${userAuthenticationMessage(true)} CanSearchWarehouse entitlement is required. You can request the Role below. Elastic (search) is used in the background. See links below for syntax. Examples of usage: POST /search/warehouse/THE_INDEX_YOU_WANT_TO_USE POST /search/warehouse/INDEX1,INDEX2 POST /search/warehouse/ALL { Any valid elasticsearch query DSL in the body } [Elasticsearch query DSL](https://www.elastic.co/guide/en/elasticsearch/reference
- **Route Pattern**: `"search" :: "warehouse" :: index :: Nil`
- **Tags**: SearchWarehouse
- **Required Roles**: SearchWarehouse
- **Path Parameters**: INDEX
- **Error Codes**: UnknownError
- **Source Files**: APIMethods300.scala

#### `POST /search/warehouse/statistics/INDEX/FIELD`

- **Endpoint Name**: `dataWarehouseStatistics`
- **Summary**: Data Warehouse Statistics
- **Description**:  Search the data warehouse and get statistical aggregations over a warehouse field Does a stats aggregation over some numeric field: https://www.elastic.co/guide/en/elasticsearch/reference/current/search-aggregations-metrics-stats-aggregation.html ${userAuthenticationMessage(true)} CanSearchWarehouseStats Role is required. You can request this below. Elastic (search) is used in the background. See links below for syntax. Examples of usage: POST /search/warehouse/statistics/INDEX/FIELD POST /sear
- **Route Pattern**: `"search" :: "warehouse" :: "statistics" :: index :: field :: Nil`
- **Tags**: SearchWarehouse
- **Required Roles**: SearchWarehouseStatistics
- **Path Parameters**: FIELD, INDEX
- **Error Codes**: UnknownError
- **Source Files**: APIMethods300.scala

### PUT Endpoints (2)

#### `PUT /banks/BANK_ID/accounts/ACCOUNT_ID/views/VIEW_ID`

- **Endpoint Name**: `updateViewForBankAccount`
- **Summary**: Update Custom View
- **Description**: Update an existing custom view on a bank account ${userAuthenticationMessage(true)} and the user needs to have access to the owner view. The json sent is the same as during view creation (above), with one difference: the 'name' field of a view is not editable (it is only set when a view is created)
- **Route Pattern**: `"banks" :: BankId(bankId) :: "accounts" :: AccountId(accountId) :: "views" :: ViewId(viewId) :: Nil`
- **Tags**: Account, View
- **Path Parameters**: BANK_ID, ACCOUNT_ID, VIEW_ID
- **Request Body**: UpdateViewJSON
- **Error Codes**: BankAccountNotFound, UnknownError
- **Source Files**: APIMethods300.scala

#### `PUT /banks/BANK_ID/branches/BRANCH_ID`

- **Endpoint Name**: `updateBranch`
- **Summary**: Update Branch
- **Description**: Update an existing branch for a bank account (Authenticated access). ${userAuthenticationMessage(true) } 
- **Tags**: Branch
- **Required Roles**: UpdateBranch
- **Path Parameters**: BANK_ID, BRANCH_ID
- **Error Codes**: BankNotFound, UnknownError
- **Source Files**: APIMethods300.scala

### DELETE Endpoints (2)

#### `DELETE /consumers/CONSUMER_ID/scope/SCOPE_ID`

- **Endpoint Name**: `deleteScope`
- **Summary**: Delete Consumer Scope
- **Description**: Delete Consumer Scope specified by SCOPE_ID for an consumer specified by CONSUMER_ID Authentication is required and the user needs to be a Super Admin. Super Admins are listed in the Props file. 
- **Route Pattern**: `"consumers" :: consumerId :: "scope" :: scopeId :: Nil`
- **Tags**: Scope, Consumer
- **Path Parameters**: CONSUMER_ID, SCOPE_ID
- **Error Codes**: EntitlementNotFound, UnknownError
- **Source Files**: APIMethods300.scala

#### `DELETE /entitlement-requests/ENTITLEMENT_REQUEST_ID`

- **Endpoint Name**: `deleteEntitlementRequest`
- **Summary**: Delete Entitlement Request
- **Description**: Delete the Entitlement Request specified by ENTITLEMENT_REQUEST_ID for a user specified by USER_ID ${userAuthenticationMessage(true)}
- **Route Pattern**: `"entitlement-requests" :: entitlementRequestId :: Nil`
- **Tags**: Entitlement, Role, User
- **Required Roles**: DeleteEntitlementRequestsAtAnyBank
- **Path Parameters**: ENTITLEMENT_REQUEST_ID
- **Request Body**: entitlementJSONs
- **Response Body**: entitlementJSONs
- **Error Codes**: UnknownError
- **Source Files**: APIMethods300.scala


---

## API Context: v3_1_0

**Total Endpoints**: 93

### GET Endpoints (37)

#### `GET /adapter`

- **Endpoint Name**: `getAdapterInfo`
- **Summary**: Get Adapter Info
- **Description**: Get basic information about the Adapter. ${userAuthenticationMessage(true)} 
- **Route Pattern**: `case "adapter" :: Nil JsonGet`
- **Tags**: Api
- **Required Roles**: GetAdapterInfo
- **Error Codes**: UnknownError
- **Source Files**: APIMethods310.scala

#### `GET /banks/BANK_ID/account-applications`

- **Endpoint Name**: `getAccountApplications`
- **Summary**: Get Account Applications
- **Description**: Get the Account Applications. ${userAuthenticationMessage(true)} 
- **Route Pattern**: `"banks" :: BankId(bankId) ::"account-applications" :: Nil`
- **Tags**: Account, AccountApplication
- **Path Parameters**: BANK_ID
- **Error Codes**: UnknownError
- **Source Files**: APIMethods310.scala

#### `GET /banks/BANK_ID/account-applications/ACCOUNT_APPLICATION_ID`

- **Endpoint Name**: `getAccountApplication`
- **Summary**: Get Account Application by Id
- **Description**: Get the Account Application. ${userAuthenticationMessage(true)} 
- **Route Pattern**: `"banks" :: BankId(bankId) ::"account-applications":: accountApplicationId :: Nil`
- **Tags**: Account, AccountApplication
- **Path Parameters**: BANK_ID, ACCOUNT_APPLICATION_ID
- **Error Codes**: UnknownError
- **Source Files**: APIMethods310.scala

#### `GET /banks/BANK_ID/accounts/ACCOUNT_ID/VIEW_ID/account`

- **Endpoint Name**: `getPrivateAccountByIdFull`
- **Summary**: Get Account by Id (Full)
- **Description**: Information returned about an account specified by ACCOUNT_ID as moderated by the view (VIEW_ID): * Number * Owners * Type * Balance * IBAN * Available views (sorted by short_name) More details about the data moderation by the view [here](#1_2_1-getViewsForBankAccount). PSD2 Context: PSD2 requires customers to have access to their account information via third party applications. This call provides balance and other account information via delegated authentication using OAuth. Authentication is 
- **Route Pattern**: `"banks" :: BankId(bankId) :: "accounts" :: AccountId(accountId) :: ViewId(viewId) :: "account" :: Nil`
- **Path Parameters**: BANK_ID, ACCOUNT_ID, VIEW_ID
- **Request Body**: moderatedAccountJSON310
- **Response Body**: moderatedAccountJSON310
- **Source Files**: APIMethods310.scala

#### `GET /banks/BANK_ID/accounts/ACCOUNT_ID/VIEW_ID/checkbook/orders`

- **Endpoint Name**: `getCheckbookOrders`
- **Summary**: Get Checkbook orders
- **Description**: ${mockedDataText(false)}Get all checkbook orders
- **Route Pattern**: `"banks" :: BankId(bankId) :: "accounts" :: AccountId(accountId) :: ViewId(viewId) :: "checkbook"  :: "orders" :: Nil`
- **Path Parameters**: BANK_ID, ACCOUNT_ID, VIEW_ID
- **Source Files**: APIMethods310.scala

#### `GET /banks/BANK_ID/accounts/ACCOUNT_ID/VIEW_ID/credit_cards/orders`

- **Endpoint Name**: `getStatusOfCreditCardOrder`
- **Summary**: Get status of Credit Card order 
- **Description**: ${mockedDataText(false)}Get status of Credit Card orders Get all orders 
- **Route Pattern**: `"banks" :: BankId(bankId) :: "accounts" :: AccountId(accountId) :: ViewId(viewId) :: "credit_cards"  :: "orders" :: Nil`
- **Path Parameters**: BANK_ID, ACCOUNT_ID, VIEW_ID, _
- **Source Files**: APIMethods310.scala

#### `GET /banks/BANK_ID/accounts/ACCOUNT_ID/VIEW_ID/funds-available`

- **Endpoint Name**: `checkFundsAvailable`
- **Summary**: Check Available Funds
- **Description**: Check Available Funds Mandatory URL parameters: * amount=NUMBER * currency=STRING 
- **Route Pattern**: `"banks" :: BankId(bankId) :: "accounts" :: AccountId(accountId) :: ViewId(viewId) :: "funds-available" :: Nil`
- **Path Parameters**: BANK_ID, ACCOUNT_ID, VIEW_ID
- **Source Files**: APIMethods310.scala

#### `GET /banks/BANK_ID/accounts/ACCOUNT_ID/VIEW_ID/transaction-requests`

- **Endpoint Name**: `getTransactionRequests`
- **Summary**: Get Transaction Requests.
- **Description**: Returns transaction requests for account specified by ACCOUNT_ID at bank specified by BANK_ID. The VIEW_ID specified must be 'owner' and the user must have access to this view. Version 2.0.0 now returns charge information. Transaction Requests serve to initiate transactions that may or may not proceed. They contain information including: * Transaction Request Id * Type * Status (INITIATED, COMPLETED) * Challenge (in order to confirm the request) * From Bank / Account * Details including Currency
- **Route Pattern**: `"banks" :: BankId(bankId) :: "accounts" :: AccountId(accountId) :: ViewId(viewId) :: "transaction-requests" :: Nil`
- **Tags**: PSD2PIS, TransactionRequest
- **Path Parameters**: BANK_ID, ACCOUNT_ID, VIEW_ID
- **Request Body**: transactionRequestWithChargeJSONs210
- **Response Body**: transactionRequestWithChargeJSONs210
- **Error Codes**: BankNotFound, BankAccountNotFound, UnknownError
- **Source Files**: APIMethods310.scala

#### `GET /banks/BANK_ID/accounts/ACCOUNT_ID/VIEW_ID/transactions/TRANSACTION_ID/transaction`

- **Endpoint Name**: `getTransactionByIdForBankAccount`
- **Summary**: Get Transaction by Id
- **Description**: Returns one transaction specified by TRANSACTION_ID of the account ACCOUNT_ID and [moderated](#1_2_1-getViewsForBankAccount) by the view (VIEW_ID). ${userAuthenticationMessage(false)} Authentication is required if the view is not public. 
- **Route Pattern**: `"banks" :: BankId(bankId) :: "accounts" :: AccountId(accountId) :: ViewId(viewId) :: "transactions" :: TransactionId(transactionId) :: "transaction" :: Nil`
- **Tags**: Transaction
- **Path Parameters**: BANK_ID, ACCOUNT_ID, TRANSACTION_ID, VIEW_ID
- **Request Body**: createTransactionJSON
- **Error Codes**: BankAccountNotFound, ViewNotFound, UnknownError
- **Source Files**: APIMethods310.scala

#### `GET /banks/BANK_ID/balances`

- **Endpoint Name**: `getBankAccountsBalances`
- **Summary**: Get Accounts Balances
- **Description**: Get the Balances for the Accounts of the current User at one bank.
- **Route Pattern**: `"banks" :: BankId(bankId) :: "balances" :: Nil`
- **Tags**: Consumer
- **Required Roles**: DisableConsumers, EnableConsumers
- **Path Parameters**: BANK_ID
- **Request Body**: putEnabledJSON
- **Error Codes**: UnknownError
- **Source Files**: APIMethods310.scala

#### `GET /banks/BANK_ID/customers/CUSTOMER_ID`

- **Endpoint Name**: `getCustomerByCustomerId`
- **Summary**: Get Customer by CUSTOMER_ID
- **Description**: Gets the Customer specified by CUSTOMER_ID. ${userAuthenticationMessage(true)} 
- **Route Pattern**: `"banks" :: BankId(bankId) :: "customers" :: customerId :: Nil`
- **Tags**: Customer
- **Required Roles**: GetCustomer
- **Path Parameters**: BANK_ID, CUSTOMER_ID
- **Error Codes**: UserCustomerLinksNotFound, UnknownError
- **Source Files**: APIMethods310.scala

#### `GET /banks/BANK_ID/customers/CUSTOMER_ID/addresses`

- **Endpoint Name**: `getCustomerAddresses`
- **Summary**: Get Customer Addresses
- **Description**: Get the Addresses of the Customer specified by CUSTOMER_ID. ${userAuthenticationMessage(true)} 
- **Route Pattern**: `"banks" :: BankId(bankId) :: "customers" :: customerId :: "addresses" :: Nil`
- **Tags**: Customer, Kyc
- **Required Roles**: GetCustomerAddress
- **Path Parameters**: BANK_ID, CUSTOMER_ID
- **Error Codes**: UnknownError
- **Source Files**: APIMethods310.scala

#### `GET /banks/BANK_ID/customers/CUSTOMER_ID/tax-residences`

- **Endpoint Name**: `getTaxResidence`
- **Summary**: Get Tax Residences of Customer
- **Description**: Get the Tax Residences of the Customer specified by CUSTOMER_ID. ${userAuthenticationMessage(true)} 
- **Route Pattern**: `"banks" :: BankId(bankId) :: "customers" :: customerId :: "tax-residences" :: Nil`
- **Tags**: Customer, Kyc
- **Path Parameters**: BANK_ID, CUSTOMER_ID
- **Error Codes**: UnknownError
- **Source Files**: APIMethods310.scala

#### `GET /banks/BANK_ID/firehose/customers`

- **Endpoint Name**: `getFirehoseCustomers`
- **Summary**: Get Firehose Customers
- **Description**:  Get Customers that has a firehose View. Allows bulk access to customers. User must have the CanUseFirehoseAtAnyBank Role ${urlParametersDocument(true, true)} ${userAuthenticationMessage(true)} 
- **Route Pattern**: `"banks" :: BankId(bankId):: "firehose" ::  "customers" :: Nil`
- **Tags**: FirehoseData, Customer
- **Required Roles**: UseCustomerFirehoseAtAnyBank
- **Path Parameters**: BANK_ID
- **Request Body**: customerJSONs
- **Response Body**: customerJSONs
- **Error Codes**: UnknownError
- **Source Files**: APIMethods310.scala

#### `GET /banks/BANK_ID/meetings`

- **Endpoint Name**: `getMeetings`
- **Summary**: Get Meetings
- **Description**: Meetings contain meta data about, and are used to facilitate, video conferences / chats etc. The actual conference/chats are handled by external services. Login is required. This call is **experimental** and will require further authorisation in the future.
- **Route Pattern**: `"banks" :: BankId(bankId) :: "meetings" :: Nil`
- **Tags**: Meeting, Customer, Experimental
- **Path Parameters**: BANK_ID
- **Error Codes**: BankNotFound, UnknownError
- **Source Files**: APIMethods310.scala

#### `GET /banks/BANK_ID/my/consents`

- **Endpoint Name**: `getConsents`
- **Summary**: Get Consents
- **Description**:  This endpoint gets the Consents that the current User created. ${userAuthenticationMessage(true)} 
- **Route Pattern**: `"banks" :: BankId(bankId) :: "my" :: "consents" :: Nil`
- **Tags**: Psd2, Consent, PSD2AIS
- **Path Parameters**: BANK_ID
- **Error Codes**: BankNotFound, UnknownError
- **Source Files**: APIMethods310.scala

#### `GET /banks/BANK_ID/product-collections/COLLECTION_CODE`

- **Endpoint Name**: `getProductCollection`
- **Summary**: Get Product Collection
- **Description**: Returns information about the financial Product Collection specified by BANK_ID and COLLECTION_CODE: 
- **Route Pattern**: `"banks" :: BankId(bankId) :: "product-collections" :: collectionCode :: Nil`
- **Tags**: ProductCollection, Product
- **Path Parameters**: BANK_ID, COLLECTION_CODE
- **Error Codes**: BankNotFound, UnknownError
- **Source Files**: APIMethods310.scala

#### `GET /banks/BANK_ID/product-tree/PRODUCT_CODE`

- **Endpoint Name**: `getProductTree`
- **Summary**: Get Product Tree
- **Description**: Returns information about a particular financial product specified by BANK_ID and PRODUCT_CODE and it's parent product(s) recursively as specified by parent_product_code. Each product includes the following information. * Name * Code * Parent Product Code * Category * Family * Super Family * More info URL * Description * Terms and Conditions * License: The licence under which this product data is released. Licence can be an Open Data licence such as Open Data Commons Public Domain Dedication and
- **Route Pattern**: `"banks" :: BankId(bankId) :: "product-tree" :: ProductCode(productCode) :: Nil`
- **Tags**: Product
- **Path Parameters**: BANK_ID, PRODUCT_CODE
- **Error Codes**: UnknownError, ProductNotFound
- **Source Files**: APIMethods310.scala

#### `GET /banks/BANK_ID/products/PRODUCT_CODE/attributes/PRODUCT_ATTRIBUTE_ID`

- **Endpoint Name**: `getProductAttribute`
- **Summary**: Get Product Attribute
- **Description**: Get Product Attribute $productAttributeGeneralInfo Get one product attribute by its id. ${userAuthenticationMessage(true)} 
- **Route Pattern**: `"banks" :: bankId :: "products" :: productCode:: "attributes" :: productAttributeId :: Nil`
- **Tags**: Product
- **Required Roles**: GetProductAttribute
- **Path Parameters**: BANK_ID, PRODUCT_ATTRIBUTE_ID, PRODUCT_CODE
- **Error Codes**: UnknownError
- **Source Files**: APIMethods310.scala

#### `GET /config`

- **Endpoint Name**: `config`
- **Summary**: Get API Configuration
- **Description**: Returns information about: * The default bank_id * Akka configuration * Elastic Search configuration * Cached functions
- **Route Pattern**: `case "config" :: Nil JsonGet`
- **Required Roles**: GetConfig
- **Request Body**: configurationJSON
- **Error Codes**: UnknownError
- **Source Files**: APIMethods310.scala

#### `GET /connector/loopback`

- **Endpoint Name**: `getObpConnectorLoopback`
- **Summary**: Get Connector Status (Loopback)
- **Description**: This endpoint makes a call to the Connector to check the backend transport is reachable. (WIP) ${userAuthenticationMessage(true)} 
- **Route Pattern**: `"connector" :: "loopback" :: Nil`
- **Tags**: Api
- **Error Codes**: UnknownError
- **Source Files**: APIMethods310.scala

#### `GET /entitlements`

- **Endpoint Name**: `getAllEntitlements`
- **Summary**: Get all Entitlements
- **Description**:  Login is required. Possible filter on the role field: eg: /entitlements?role=${canGetCustomer.toString} 
- **Route Pattern**: `case "entitlements" :: Nil JsonGet`
- **Tags**: Entitlement, Role
- **Request Body**: entitlementJSONs
- **Response Body**: entitlementJSONs
- **Error Codes**: UnknownError
- **Source Files**: APIMethods310.scala

#### `GET /management/banks/BANK_ID/account-web-hooks`

- **Endpoint Name**: `getAccountWebhooks`
- **Summary**: Get Account Webhooks
- **Description**: Get Account Webhooks. Possible custom URL parameters for pagination: ${urlParametersDocument(false, false)} * account_id=STRING (if null ignore) * user_id=STRING (if null ignore) 
- **Route Pattern**: `"management" :: "banks" :: BankId(bankId) ::"account-web-hooks" :: Nil`
- **Required Roles**: GetWebhooks
- **Path Parameters**: BANK_ID
- **Request Body**: configurationJSON
- **Source Files**: APIMethods310.scala

#### `GET /management/banks/BANK_ID/cards`

- **Endpoint Name**: `getCardsForBank`
- **Summary**: Get Cards for the specified bank
- **Description**: Should be able to filter on the following fields eg:/management/banks/BANK_ID/cards?customer_id=66214b8e-259e-44ad-8868-3eb47be70646&account_id=8ca8a7e4-6d02-48e3-a029-0b2bf89de9f0 1 customer_id should be valid customer_id, otherwise, it will return an empty card list. 2 account_id should be valid account_id , otherwise, it will return an empty card list. ${userAuthenticationMessage(true)}
- **Route Pattern**: `"management" :: "banks" :: BankId(bankId) :: "cards" :: Nil`
- **Tags**: Card
- **Path Parameters**: BANK_ID
- **Error Codes**: BankNotFound, UnknownError
- **Source Files**: APIMethods310.scala

#### `GET /management/banks/BANK_ID/cards/CARD_ID`

- **Endpoint Name**: `getCardForBank`
- **Summary**: Get Card By Id
- **Description**:  This will the datails of the card. It shows the account infomation which linked the the card. Also shows the card attributes of the card. 
- **Route Pattern**: `"management" :: "banks" :: BankId(bankId) :: "cards" :: cardId :: Nil`
- **Tags**: Card
- **Required Roles**: GetCardsForBank
- **Path Parameters**: BANK_ID, CARD_ID
- **Error Codes**: BankNotFound, UnknownError
- **Source Files**: APIMethods310.scala

#### `GET /management/consumers`

- **Endpoint Name**: `getConsumers`
- **Summary**: Get Consumers
- **Description**: Get the all Consumers. ${userAuthenticationMessage(true)} ${urlParametersDocument(true, true)} 
- **Route Pattern**: `"management" :: "consumers" :: Nil`
- **Tags**: Consumer
- **Required Roles**: GetConsumers
- **Error Codes**: UnknownError
- **Source Files**: APIMethods310.scala

#### `GET /management/consumers/CONSUMER_ID`

- **Endpoint Name**: `getConsumer`
- **Summary**: Get Consumer
- **Description**: Get the Consumer specified by CONSUMER_ID. 
- **Route Pattern**: `"management" :: "consumers" :: consumerId :: Nil`
- **Tags**: Consumer
- **Required Roles**: GetConsumers
- **Path Parameters**: CONSUMER_ID
- **Request Body**: consumerJSON
- **Error Codes**: ConsumerNotFound, UnknownError
- **Source Files**: APIMethods310.scala

#### `GET /management/consumers/CONSUMER_ID/consumer/call-limits`

- **Endpoint Name**: `getCallsLimit`
- **Summary**: Get Call Limits for a Consumer
- **Description**:  Get Calls limits per Consumer. ${userAuthenticationMessage(true)} 
- **Route Pattern**: `"management" :: "consumers" :: consumerId :: "consumer" :: "call-limits" :: Nil`
- **Tags**: Consumer
- **Required Roles**: SetCallLimits
- **Path Parameters**: CONSUMER_ID
- **Error Codes**: UpdateConsumerError, ConsumerNotFound, UnknownError
- **Source Files**: APIMethods310.scala

#### `GET /management/method_routings`

- **Endpoint Name**: `getMethodRoutings`
- **Summary**: Get MethodRoutings
- **Description**: Get the all MethodRoutings. Query url parameters: * method_name: filter with method_name * active: if active = true, it will show all the webui_ props. Even if they are set yet, we will return all the default webui_ props eg: ${getObpApiRoot}/v3.1.0/management/method_routings?active=true ${getObpApiRoot}/v3.1.0/management/method_routings?method_name=getBank 
- **Required Roles**: GetMethodRoutings
- **Path Parameters**: _
- **Source Files**: APIMethods310.scala

#### `GET /management/metrics/top-apis`

- **Endpoint Name**: `getTopAPIs`
- **Summary**: Get Top APIs
- **Description**: Get metrics about the most popular APIs. e.g.: total count, response time (in ms), etc. Should be able to filter on the following fields eg: /management/metrics/top-apis?from_date=$epochTimeString&to_date=$DefaultToDateString&consumer_id=5 &user_id=66214b8e-259e-44ad-8868-3eb47be70646&implemented_by_partial_function=getTransactionsForBankAccount &implemented_in_version=v3.0.0&url=/obp/v3.0.0/banks/gh.29.uk/accounts/8ca8a7e4-6d02-48e3-a029-0b2bf89de9f0/owner/transactions &verb=GET&anon=false&app_
- **Route Pattern**: `"management" :: "metrics" :: "top-apis" :: Nil`
- **Source Files**: APIMethods310.scala

#### `GET /management/metrics/top-consumers`

- **Endpoint Name**: `getMetricsTopConsumers`
- **Summary**: Get Top Consumers
- **Description**: Get metrics about the top consumers of the API usage e.g. total count, consumer_id and app_name. Should be able to filter on the following fields e.g.: /management/metrics/top-consumers?from_date=$epochTimeString&to_date=$DefaultToDateString&consumer_id=5 &user_id=66214b8e-259e-44ad-8868-3eb47be70646&implemented_by_partial_function=getTransactionsForBankAccount &implemented_in_version=v3.0.0&url=/obp/v3.0.0/banks/gh.29.uk/accounts/8ca8a7e4-6d02-48e3-a029-0b2bf89de9f0/owner/transactions &verb=GET
- **Route Pattern**: `"management" :: "metrics" :: "top-consumers" :: Nil`
- **Source Files**: APIMethods310.scala

#### `GET /management/users/current/consumers`

- **Endpoint Name**: `getConsumersForCurrentUser`
- **Summary**: Get Consumers (logged in User)
- **Description**: Get the Consumers for logged in User. 
- **Route Pattern**: `"management" :: "users" :: "current" :: "consumers" :: Nil`
- **Tags**: Consumer
- **Required Roles**: GetConsumers
- **Error Codes**: UnknownError
- **Source Files**: APIMethods310.scala

#### `GET /management/webui_props`

- **Endpoint Name**: `getWebUiProps`
- **Summary**: Get WebUiProps
- **Description**:  Get the all WebUiProps key values, those props key with "webui_" can be stored in DB, this endpoint get all from DB. url query parameter: active: It must be a boolean string. and If active = true, it will show combination of explicit (inserted) + implicit (default) method_routings. eg: ${getObpApiRoot}/v3.1.0/management/webui_props ${getObpApiRoot}/v3.1.0/management/webui_props?active=true 
- **Required Roles**: GetWebUiProps
- **Path Parameters**: _
- **Source Files**: APIMethods310.scala

#### `GET /message-docs/CONNECTOR/swagger2.0`

- **Endpoint Name**: `getMessageDocsSwagger`
- **Summary**: Get Message Docs Swagger
- **Description**:  This endpoint provides example message docs in swagger format. It is only relavent for REST Connectors. This endpoint can be used by the developer building a REST Adapter that connects to the Core Banking System (CBS). That is, the Adapter developer can use the Swagger surfaced here to build the REST APIs that the OBP REST connector will call to consume CBS services. i.e.: OBP API (Core OBP API code) -> OBP REST Connector (OBP REST Connector code) -> OBP REST Adapter (Adapter developer code) ->
- **Route Pattern**: `"message-docs" :: restConnectorVersion ::"swagger2.0" :: Nil`
- **Tags**: Api, Documentation
- **Path Parameters**: CONNECTOR
- **Error Codes**: UnknownError
- **Source Files**: APIMethods310.scala

#### `GET /rate-limiting`

- **Endpoint Name**: `getRateLimitingInfo`
- **Summary**: Get Rate Limiting Info
- **Description**: Get information about the Rate Limiting setup on this OBP Instance such as: Is rate limiting enabled and active? What backend is used to keep track of the API calls (e.g. REDIS). Note: Rate limiting can be set at the Consumer level and also for anonymous calls. See the consumer rate limits / call limits endpoints. ${userAuthenticationMessage(true)} 
- **Route Pattern**: `case "rate-limiting" :: Nil JsonGet`
- **Tags**: Api, RateLimits
- **Error Codes**: UnknownError
- **Source Files**: APIMethods310.scala

#### `GET /users/USERNAME/lock-status`

- **Endpoint Name**: `getBadLoginStatus`
- **Summary**: Get User Lock Status
- **Description**:  Get User Login Status. ${userAuthenticationMessage(true)} 
- **Route Pattern**: `"users" :: username::  "lock-status" :: Nil`
- **Tags**: User
- **Required Roles**: ReadUserLockedStatus
- **Path Parameters**: USERNAME
- **Error Codes**: UserNotFound, UnknownError
- **Source Files**: APIMethods310.scala

#### `GET /users/USER_ID/auth-context`

- **Endpoint Name**: `getUserAuthContexts`
- **Summary**: Get User Auth Contexts
- **Description**: Get User Auth Contexts for a User. ${userAuthenticationMessage(true)} 
- **Route Pattern**: `"users" :: userId :: "auth-context" :: Nil`
- **Tags**: User
- **Path Parameters**: USER_ID
- **Error Codes**: UnknownError
- **Source Files**: APIMethods310.scala

### POST Endpoints (23)

#### `POST /banks/BANK_ID/account-applications`

- **Endpoint Name**: `createAccountApplication`
- **Summary**: Create Account Application
- **Description**: Create Account Application ${userAuthenticationMessage(true)} 
- **Route Pattern**: `"banks" :: BankId(bankId) :: "account-applications" :: Nil`
- **Tags**: Account, AccountApplication
- **Path Parameters**: BANK_ID
- **Error Codes**: UnknownError
- **Source Files**: APIMethods310.scala

#### `POST /banks/BANK_ID/account-web-hooks`

- **Endpoint Name**: `createAccountWebhook`
- **Summary**: Create an Account Webhook
- **Description**: Create an Account Webhook $accountWebHookInfo 
- **Route Pattern**: `"banks" :: BankId(bankId) :: "account-web-hooks" :: Nil`
- **Required Roles**: CreateWebhook
- **Path Parameters**: BANK_ID
- **Source Files**: APIMethods310.scala

#### `POST /banks/BANK_ID/accounts/ACCOUNT_ID/products/PRODUCT_CODE/attribute`

- **Endpoint Name**: `createAccountAttribute`
- **Summary**: Create Account Attribute
- **Description**: Create Account Attribute $accountAttributeGeneralInfo Typical account attributes might be: ISIN (for International bonds) VKN (for German bonds) REDCODE (markit short code for credit derivative) LOAN_ID (e.g. used for Anacredit reporting) ISSUE_DATE (When the bond was issued in the market) MATURITY_DATE (End of life time of a product) TRADABLE See [FPML](http://www.fpml.org/) for more examples. The type field must be one of "STRING", "INTEGER", "DOUBLE" or DATE_WITH_DAY" ${userAuthenticationMess
- **Route Pattern**: `"banks" :: bankId :: "accounts" :: accountId :: "products" :: productCode :: "attribute" :: Nil`
- **Tags**: Account
- **Required Roles**: CreateAccountAttributeAtOneBank
- **Path Parameters**: BANK_ID, ACCOUNT_ID, PRODUCT_CODE
- **Error Codes**: UnknownError
- **Source Files**: APIMethods310.scala

#### `POST /banks/BANK_ID/consents/CONSENT_ID/challenge`

- **Endpoint Name**: `answerConsentChallenge`
- **Summary**: Answer Consent Challenge
- **Description**:  $generalObpConsentText This endpoint is used to confirm a Consent previously created. The User must supply a code that was sent out of band (OOB) for example via an SMS. ${userAuthenticationMessage(true)} 
- **Route Pattern**: `"banks" :: BankId(bankId) :: "consents"  :: consentId :: "challenge" :: Nil`
- **Path Parameters**: BANK_ID, CONSENT_ID
- **Source Files**: APIMethods310.scala

#### `POST /banks/BANK_ID/customers`

- **Endpoint Name**: `createCustomer`
- **Summary**: Create Customer
- **Description**:  The Customer resource stores the customer number (which is set by the backend), legal name, email, phone number, their date of birth, relationship status, education attained, a url for a profile image, KYC status etc. Dates need to be in the format 2013-01-21T23:08:00Z Note: If you need to set a specific customer number, use the Update Customer Number endpoint after this call. ${userAuthenticationMessage(true)} 
- **Route Pattern**: `"banks" :: BankId(bankId) :: "customers" :: Nil`
- **Tags**: Person, Customer
- **Required Roles**: CreateCustomer, CreateCustomerAtAnyBank
- **Path Parameters**: BANK_ID
- **Error Codes**: UserNotFound, BankNotFound, CreateCustomerError, UnknownError
- **Source Files**: APIMethods310.scala

#### `POST /banks/BANK_ID/customers/CUSTOMER_ID/address`

- **Endpoint Name**: `createCustomerAddress`
- **Summary**: Create Address
- **Description**: Create an Address for a Customer specified by CUSTOMER_ID. ${userAuthenticationMessage(true)} 
- **Route Pattern**: `"banks" :: BankId(bankId) :: "customers" :: customerId :: "address" :: Nil`
- **Tags**: Customer
- **Required Roles**: CreateCustomerAddress
- **Path Parameters**: BANK_ID, CUSTOMER_ID
- **Error Codes**: UnknownError
- **Source Files**: APIMethods310.scala

#### `POST /banks/BANK_ID/customers/CUSTOMER_ID/tax-residence`

- **Endpoint Name**: `createTaxResidence`
- **Summary**: Create Tax Residence
- **Description**: Create a Tax Residence for a Customer specified by CUSTOMER_ID. ${userAuthenticationMessage(true)} 
- **Route Pattern**: `"banks" :: BankId(bankId) :: "customers" :: customerId :: "tax-residence" :: Nil`
- **Tags**: Customer, Kyc
- **Required Roles**: CreateTaxResidence
- **Path Parameters**: BANK_ID, CUSTOMER_ID
- **Error Codes**: UnknownError
- **Source Files**: APIMethods310.scala

#### `POST /banks/BANK_ID/customers/customer-number`

- **Endpoint Name**: `getCustomerByCustomerNumber`
- **Summary**: Get Customer by CUSTOMER_NUMBER
- **Description**: Gets the Customer specified by CUSTOMER_NUMBER. ${userAuthenticationMessage(true)} 
- **Route Pattern**: `"banks" :: BankId(bankId) :: "customers" :: "customer-number" :: Nil`
- **Tags**: Customer, Kyc
- **Required Roles**: GetCustomer
- **Path Parameters**: BANK_ID
- **Error Codes**: UserCustomerLinksNotFound, UnknownError
- **Source Files**: APIMethods310.scala

#### `POST /banks/BANK_ID/meetings`

- **Endpoint Name**: `createMeeting`
- **Summary**: Create Meeting (video conference/call)
- **Description**: Create Meeting: Initiate a video conference/call with the bank. The Meetings resource contains meta data about video/other conference sessions provider_id determines the provider of the meeting / video chat service. MUST be url friendly (no spaces). purpose_id explains the purpose of the chat. onboarding mortgage complaint etc. MUST be url friendly (no spaces). Login is required. This call is **experimental**. Currently staff_user_id is not set. Further calls will be needed to correctly set this
- **Route Pattern**: `"banks" :: BankId(bankId) :: "meetings" :: Nil`
- **Tags**: Meeting, Customer, Experimental
- **Path Parameters**: BANK_ID
- **Error Codes**: BankNotFound, UnknownError
- **Source Files**: APIMethods310.scala

#### `POST /banks/BANK_ID/my/consents/EMAIL`

- **Endpoint Name**: `createConsentEmail`
- **Summary**: Create Consent (EMAIL)
- **Description**:  This endpoint starts the process of creating a Consent. The Consent is created in an ${ConsentStatus.INITIATED} state. A One Time Password (OTP) (AKA security challenge) is sent Out of Band (OOB) to the User via the transport defined in SCA_METHOD SCA_METHOD is typically "SMS","EMAIL" or "IMPLICIT". "EMAIL" is used for testing purposes. OBP mapped mode "IMPLICIT" is "EMAIL". Other mode, bank can decide it in the connector method 'getConsentImplicitSCA'. When the Consent is created, OBP (or a ba
- **Path Parameters**: BANK_ID, EMAIL
- **Source Files**: APIMethods310.scala

#### `POST /banks/BANK_ID/my/consents/IMPLICIT`

- **Endpoint Name**: `createConsentImplicit`
- **Summary**: Create Consent (IMPLICIT)
- **Description**:  This endpoint starts the process of creating a Consent. The Consent is created in an ${ConsentStatus.INITIATED} state. A One Time Password (OTP) (AKA security challenge) is sent Out of Band (OOB) to the User via the transport defined in SCA_METHOD SCA_METHOD is typically "SMS","EMAIL" or "IMPLICIT". "EMAIL" is used for testing purposes. OBP mapped mode "IMPLICIT" is "EMAIL". Other mode, bank can decide it in the connector method 'getConsentImplicitSCA'. When the Consent is created, OBP (or a ba
- **Path Parameters**: BANK_ID, IMPLICIT
- **Source Files**: APIMethods310.scala

#### `POST /banks/BANK_ID/my/consents/SMS`

- **Endpoint Name**: `createConsentSms`
- **Summary**: Create Consent (SMS)
- **Description**:  This endpoint starts the process of creating a Consent. The Consent is created in an ${ConsentStatus.INITIATED} state. A One Time Password (OTP) (AKA security challenge) is sent Out of Band (OOB) to the User via the transport defined in SCA_METHOD SCA_METHOD is typically "SMS","EMAIL" or "IMPLICIT". "EMAIL" is used for testing purposes. OBP mapped mode "IMPLICIT" is "EMAIL". Other mode, bank can decide it in the connector method 'getConsentImplicitSCA'. When the Consent is created, OBP (or a ba
- **Path Parameters**: BANK_ID, SMS
- **Source Files**: APIMethods310.scala

#### `POST /banks/BANK_ID/products/PRODUCT_CODE/attribute`

- **Endpoint Name**: `createProductAttribute`
- **Summary**: Create Product Attribute
- **Description**: Create Product Attribute $productAttributeGeneralInfo Typical product attributes might be: ISIN (for International bonds) VKN (for German bonds) REDCODE (markit short code for credit derivative) LOAN_ID (e.g. used for Anacredit reporting) ISSUE_DATE (When the bond was issued in the market) MATURITY_DATE (End of life time of a product) TRADABLE See [FPML](http://www.fpml.org/) for more examples. The type field must be one of "STRING", "INTEGER", "DOUBLE" or DATE_WITH_DAY" ${userAuthenticationMess
- **Route Pattern**: `"banks" :: bankId :: "products" :: productCode:: "attribute" :: Nil`
- **Tags**: Product
- **Required Roles**: CreateProductAttribute
- **Path Parameters**: BANK_ID, PRODUCT_CODE
- **Error Codes**: UnknownError
- **Source Files**: APIMethods310.scala

#### `POST /banks/BANK_ID/users/current/auth-context-updates/AUTH_CONTEXT_UPDATE_ID/challenge`

- **Endpoint Name**: `answerUserAuthContextUpdateChallenge`
- **Summary**: Answer Auth Context Update Challenge
- **Description**:  Answer Auth Context Update Challenge. 
- **Route Pattern**: `"banks" :: BankId(bankId) :: "users" :: "current" ::"auth-context-updates"  :: authContextUpdateId :: "challenge" :: Nil`
- **Path Parameters**: BANK_ID, AUTH_CONTEXT_UPDATE_ID
- **Source Files**: APIMethods310.scala

#### `POST /banks/BANK_ID/users/current/auth-context-updates/SCA_METHOD`

- **Endpoint Name**: `createUserAuthContextUpdateRequest`
- **Summary**: Create User Auth Context Update Request
- **Description**: Create User Auth Context Update Request. ${userAuthenticationMessage(true)} A One Time Password (OTP) (AKA security challenge) is sent Out of Band (OOB) to the User via the transport defined in SCA_METHOD SCA_METHOD is typically "SMS" or "EMAIL". "EMAIL" is used for testing purposes. 
- **Route Pattern**: `"banks" :: BankId(bankId) :: "users" :: "current" ::"auth-context-updates" :: scaMethod :: Nil`
- **Tags**: User
- **Path Parameters**: BANK_ID, SCA_METHOD
- **Error Codes**: CreateUserAuthContextError, UnknownError
- **Source Files**: APIMethods310.scala

#### `POST /management/banks/BANK_ID/cards`

- **Endpoint Name**: `addCardForBank`
- **Summary**: Create Card
- **Description**: Create Card at bank specified by BANK_ID . ${userAuthenticationMessage(true)} 
- **Route Pattern**: `"management" :: "banks" :: BankId(bankId) :: "cards" :: Nil`
- **Tags**: Card
- **Required Roles**: CreateCardsForBank
- **Path Parameters**: BANK_ID
- **Error Codes**: UnknownError
- **Source Files**: APIMethods310.scala

#### `POST /management/banks/BANK_ID/cards/CARD_ID/attribute`

- **Endpoint Name**: `createCardAttribute`
- **Summary**: Create Card Attribute
- **Description**: Create Card Attribute Card Attributes are used to describe a financial Product with a list of typed key value pairs. Each Card Attribute is linked to its Card by CARD_ID The type field must be one of "STRING", "INTEGER", "DOUBLE" or DATE_WITH_DAY" ${userAuthenticationMessage(true)} 
- **Tags**: Card
- **Path Parameters**: BANK_ID, CARD_ID
- **Error Codes**: UnknownError
- **Source Files**: APIMethods310.scala

#### `POST /management/historical/transactions `

- **Endpoint Name**: `saveHistoricalTransaction`
- **Summary**: Save Historical Transactions 
- **Description**:  Import the historical transactions. The fields bank_id, account_id, counterparty_id in the json body are all optional ones. It support transfer money from account to account, account to counterparty and counterparty to counterparty Both bank_id + account_id and counterparty_id can identify the account, so OBP only need one of them to make the payment. So: When you need the account to account, just omit counterparty_id field.eg: { "from": { "bank_id": "gh.29.uk", "account_id": "1ca8a7e4-6d02-48e
- **Route Pattern**: `"management" :: "historical" :: "transactions" :: Nil`
- **Source Files**: APIMethods310.scala

#### `POST /management/method_routings`

- **Endpoint Name**: `createMethodRouting`
- **Summary**: Create MethodRouting
- **Description**: Create a MethodRouting. ${userAuthenticationMessage(true)} Explanation of Fields: * method_name is required String value, current supported value: $supportedConnectorNames * connector_name is required String value * is_bank_id_exact_match is required boolean value, if bank_id_pattern is exact bank_id value, this value is true; if bank_id_pattern is null or a regex, this value is false * bank_id_pattern is optional String value, it can be null, a exact bank_id or a regex * parameters is optional 
- **Route Pattern**: `"management" :: "method_routings" :: Nil`
- **Path Parameters**: _
- **Source Files**: APIMethods310.scala

#### `POST /management/webui_props`

- **Endpoint Name**: `createWebUiProps`
- **Summary**: Create WebUiProps
- **Description**: Create a WebUiProps. ${userAuthenticationMessage(true)} Explaination of Fields: * name is required String value * value is required String value The line break and double quotations should do escape, example: ``` {"name": "webui_some", "value": "this value have "line break" and double quotations."} ``` should do escape like this: ``` {"name": "webui_some", "value": "this value\\nhave \\"line break\\" and double quotations."} ``` Insert image examples: ``` // set width=100 and height=50 {"name": 
- **Route Pattern**: `"management" :: "webui_props" :: Nil`
- **Path Parameters**: _
- **Error Codes**: UnknownError
- **Source Files**: APIMethods310.scala

#### `POST /system-views`

- **Endpoint Name**: `createSystemView`
- **Summary**: Create System View
- **Description**: Create a system view ${userAuthenticationMessage(true)} and the user needs to have access to the $canCreateSystemView entitlement. The 'alias' field in the JSON can take one of two values: * _public_: to use the public alias if there is one specified for the other account. * _private_: to use the private alias if there is one specified for the other account. * _''(empty string)_: to use no alias; the view shows the real name of the other account. The 'hide_metadata_if_alias_used' field in the JS
- **Route Pattern**: `case "system-views" :: Nil JsonPost`
- **Tags**: SystemView
- **Required Roles**: CreateSystemView
- **Request Body**: SwaggerDefinitionsJSON
- **Error Codes**: UnknownError
- **Source Files**: APIMethods310.scala

#### `POST /users/USER_ID/auth-context`

- **Endpoint Name**: `createUserAuthContext`
- **Summary**: Create User Auth Context
- **Description**: Create User Auth Context. These key value pairs will be propagated over connector to adapter. Normally used for mapping OBP user and Bank User/Customer. ${userAuthenticationMessage(true)} 
- **Route Pattern**: `"users" :: userId ::"auth-context" :: Nil`
- **Tags**: User
- **Required Roles**: CreateUserAuthContext
- **Path Parameters**: USER_ID
- **Error Codes**: CreateUserAuthContextError, UnknownError
- **Source Files**: APIMethods310.scala

#### `POST /users/USER_ID/refresh`

- **Endpoint Name**: `refreshUser`
- **Summary**: Refresh User
- **Description**: The endpoint is used for updating the accounts, views, account holders for the user. As to the Json body, you can leave it as Empty. This call will get data from backend, no need to prepare the json body in api side. ${userAuthenticationMessage(true)} 
- **Route Pattern**: `"users" :: userId :: "refresh" :: Nil`
- **Tags**: User
- **Required Roles**: RefreshUser
- **Path Parameters**: USER_ID
- **Error Codes**: UnknownError
- **Source Files**: APIMethods310.scala

### PUT Endpoints (24)

#### `PUT /banks/BANK_ID/account-applications/ACCOUNT_APPLICATION_ID`

- **Endpoint Name**: `updateAccountApplicationStatus`
- **Summary**: Update Account Application Status
- **Description**: Update an Account Application status ${userAuthenticationMessage(true)} 
- **Route Pattern**: `"banks" :: BankId(bankId) ::"account-applications" :: accountApplicationId :: Nil`
- **Tags**: Account, AccountApplication
- **Path Parameters**: BANK_ID, ACCOUNT_APPLICATION_ID
- **Error Codes**: UnknownError
- **Source Files**: APIMethods310.scala

#### `PUT /banks/BANK_ID/account-web-hooks`

- **Endpoint Name**: `enableDisableAccountWebhook`
- **Summary**: Enable/Disable an Account Webhook
- **Description**: Enable/Disable an Account Webhook $accountWebHookInfo 
- **Route Pattern**: `"banks" :: BankId(bankId) :: "account-web-hooks" :: Nil`
- **Required Roles**: UpdateWebhook
- **Path Parameters**: BANK_ID
- **Source Files**: APIMethods310.scala

#### `PUT /banks/BANK_ID/accounts/ACCOUNT_ID`

- **Endpoint Name**: `createAccount`
- **Summary**: Create Account
- **Description**: Create Account at bank specified by BANK_ID with Id specified by ACCOUNT_ID. The User can create an Account for themself - or - the User that has the USER_ID specified in the POST body. If the PUT body USER_ID *is* specified, the logged in user must have the Role canCreateAccount. Once created, the Account will be owned by the User specified by USER_ID. If the PUT body USER_ID is *not* specified, the account will be owned by the logged in User. The 'product_code' field SHOULD be a product_code f
- **Route Pattern**: `"banks" :: BankId(bankId) :: "accounts" :: AccountId(accountId) :: Nil`
- **Tags**: Account, Onboarding
- **Required Roles**: CreateAccount
- **Path Parameters**: BANK_ID, ACCOUNT_ID
- **Error Codes**: UserNotFound, BankNotFound, UnknownError
- **Source Files**: APIMethods310.scala

#### `PUT /banks/BANK_ID/accounts/ACCOUNT_ID/products/PRODUCT_CODE/attributes/ACCOUNT_ATTRIBUTE_ID`

- **Endpoint Name**: `updateAccountAttribute`
- **Summary**: Update Account Attribute
- **Description**: Update Account Attribute $accountAttributeGeneralInfo Typical account attributes might be: ISIN (for International bonds) VKN (for German bonds) REDCODE (markit short code for credit derivative) LOAN_ID (e.g. used for Anacredit reporting) ISSUE_DATE (When the bond was issued in the market) MATURITY_DATE (End of life time of a product) TRADABLE See [FPML](http://www.fpml.org/) for more examples. ${userAuthenticationMessage(true)} 
- **Route Pattern**: `"banks" :: bankId :: "accounts" :: accountId :: "products" :: productCode :: "attributes" :: accountAttributeId :: Nil`
- **Tags**: Account
- **Required Roles**: UpdateAccountAttribute
- **Path Parameters**: BANK_ID, ACCOUNT_ID, PRODUCT_CODE, ACCOUNT_ATTRIBUTE_ID
- **Error Codes**: UnknownError
- **Source Files**: APIMethods310.scala

#### `PUT /banks/BANK_ID/customers/CUSTOMER_ID/addresses/CUSTOMER_ADDRESS_ID`

- **Endpoint Name**: `updateCustomerAddress`
- **Summary**: Update the Address of a Customer
- **Description**: Update an Address of the Customer specified by CUSTOMER_ADDRESS_ID. ${userAuthenticationMessage(true)} 
- **Route Pattern**: `"banks" :: BankId(bankId) :: "customers" :: customerId :: "addresses" :: customerAddressId :: Nil`
- **Tags**: Customer
- **Path Parameters**: BANK_ID, CUSTOMER_ADDRESS_ID, CUSTOMER_ID
- **Error Codes**: UnknownError
- **Source Files**: APIMethods310.scala

#### `PUT /banks/BANK_ID/customers/CUSTOMER_ID/branch`

- **Endpoint Name**: `updateCustomerBranch`
- **Summary**: Update the Branch of a Customer
- **Description**: Update the Branch of the Customer specified by CUSTOMER_ID. ${userAuthenticationMessage(true)} 
- **Route Pattern**: `"banks" :: BankId(bankId) :: "customers" :: customerId :: "branch" :: Nil`
- **Tags**: Customer
- **Path Parameters**: BANK_ID, CUSTOMER_ID
- **Error Codes**: UnknownError
- **Source Files**: APIMethods310.scala

#### `PUT /banks/BANK_ID/customers/CUSTOMER_ID/credit-limit`

- **Endpoint Name**: `updateCustomerCreditLimit`
- **Summary**: Update the credit limit of a Customer
- **Description**: Update the credit limit of the Customer specified by CUSTOMER_ID. ${userAuthenticationMessage(true)} 
- **Route Pattern**: `"banks" :: BankId(bankId) :: "customers" :: customerId :: "credit-limit" :: Nil`
- **Tags**: Customer
- **Path Parameters**: BANK_ID, CUSTOMER_ID
- **Error Codes**: UnknownError
- **Source Files**: APIMethods310.scala

#### `PUT /banks/BANK_ID/customers/CUSTOMER_ID/credit-rating-and-source`

- **Endpoint Name**: `updateCustomerCreditRatingAndSource`
- **Summary**: Update the credit rating and source of a Customer
- **Description**: Update the credit rating and source of the Customer specified by CUSTOMER_ID. ${userAuthenticationMessage(true)} 
- **Route Pattern**: `"banks" :: BankId(bankId) :: "customers" :: customerId :: "credit-rating-and-source" :: Nil`
- **Tags**: Customer
- **Path Parameters**: BANK_ID, CUSTOMER_ID
- **Error Codes**: UnknownError
- **Source Files**: APIMethods310.scala

#### `PUT /banks/BANK_ID/customers/CUSTOMER_ID/data`

- **Endpoint Name**: `updateCustomerData`
- **Summary**: Update the other data of a Customer
- **Description**: Update the other data of the Customer specified by CUSTOMER_ID. ${userAuthenticationMessage(true)} 
- **Route Pattern**: `"banks" :: BankId(bankId) :: "customers" :: customerId :: "data" :: Nil`
- **Tags**: Customer
- **Path Parameters**: BANK_ID, CUSTOMER_ID
- **Error Codes**: UnknownError
- **Source Files**: APIMethods310.scala

#### `PUT /banks/BANK_ID/customers/CUSTOMER_ID/email`

- **Endpoint Name**: `updateCustomerEmail`
- **Summary**: Update the email of a Customer
- **Description**: Update an email of the Customer specified by CUSTOMER_ID. ${userAuthenticationMessage(true)} 
- **Route Pattern**: `"banks" :: BankId(bankId) :: "customers" :: customerId :: "email" :: Nil`
- **Tags**: Customer
- **Path Parameters**: BANK_ID, CUSTOMER_ID
- **Error Codes**: UnknownError
- **Source Files**: APIMethods310.scala

#### `PUT /banks/BANK_ID/customers/CUSTOMER_ID/identity`

- **Endpoint Name**: `updateCustomerIdentity`
- **Summary**: Update the identity data of a Customer
- **Description**: Update the identity data of the Customer specified by CUSTOMER_ID. ${userAuthenticationMessage(true)} 
- **Route Pattern**: `"banks" :: BankId(bankId) :: "customers" :: customerId :: "identity" :: Nil`
- **Tags**: Customer
- **Path Parameters**: BANK_ID, CUSTOMER_ID
- **Error Codes**: UnknownError
- **Source Files**: APIMethods310.scala

#### `PUT /banks/BANK_ID/customers/CUSTOMER_ID/mobile-number`

- **Endpoint Name**: `updateCustomerMobileNumber`
- **Summary**: Update the mobile number of a Customer
- **Description**: Update the mobile number of the Customer specified by CUSTOMER_ID. ${userAuthenticationMessage(true)} 
- **Route Pattern**: `"banks" :: BankId(bankId) :: "customers" :: customerId :: "mobile-number" :: Nil`
- **Tags**: Customer
- **Path Parameters**: BANK_ID, CUSTOMER_ID
- **Error Codes**: UnknownError
- **Source Files**: APIMethods310.scala

#### `PUT /banks/BANK_ID/customers/CUSTOMER_ID/number`

- **Endpoint Name**: `updateCustomerNumber`
- **Summary**: Update the number of a Customer
- **Description**: Update the number of the Customer specified by CUSTOMER_ID. ${userAuthenticationMessage(true)} 
- **Route Pattern**: `"banks" :: BankId(bankId) :: "customers" :: customerId :: "number" :: Nil`
- **Tags**: Customer
- **Path Parameters**: BANK_ID, CUSTOMER_ID
- **Error Codes**: UnknownError
- **Source Files**: APIMethods310.scala

#### `PUT /banks/BANK_ID/product-collections/COLLECTION_CODE`

- **Endpoint Name**: `createProductCollection`
- **Summary**: Create Product Collection
- **Description**: Create or Update a Product Collection at the Bank. Use Product Collections to create Product "Baskets", "Portfolios", "Indices", "Collections", "Underlyings-lists", "Buckets" etc. etc. There is a many to many relationship between Products and Product Collections: * A Product can exist in many Collections * A Collection can contain many Products. A collection has collection code, one parent Product and one or more child Products. $productHiearchyAndCollectionNote ${userAuthenticationMessage(true)
- **Route Pattern**: `"banks" :: BankId(bankId) :: "product-collections" :: collectionCode :: Nil`
- **Tags**: ProductCollection, Product
- **Required Roles**: MaintainProductCollection
- **Path Parameters**: BANK_ID, COLLECTION_CODE
- **Error Codes**: BankNotFound, UnknownError
- **Source Files**: APIMethods310.scala

#### `PUT /banks/BANK_ID/products/PRODUCT_CODE`

- **Endpoint Name**: `createProduct`
- **Summary**: Create Product
- **Description**: Create or Update Product for the Bank. Typical Super Family values / Asset classes are: Debt Equity FX Commodity Derivative $productHiearchyAndCollectionNote ${userAuthenticationMessage(true) } 
- **Route Pattern**: `"banks" :: BankId(bankId) :: "products" :: ProductCode(productCode) :: Nil`
- **Tags**: Product
- **Required Roles**: CreateProductAtAnyBank, CreateProduct
- **Path Parameters**: BANK_ID, PRODUCT_CODE
- **Error Codes**: BankNotFound, UnknownError
- **Source Files**: APIMethods310.scala

#### `PUT /banks/BANK_ID/products/PRODUCT_CODE/attributes/PRODUCT_ATTRIBUTE_ID`

- **Endpoint Name**: `updateProductAttribute`
- **Summary**: Update Product Attribute
- **Description**: Update Product Attribute. $productAttributeGeneralInfo Update one Product Attribute by its id. ${userAuthenticationMessage(true)} 
- **Route Pattern**: `"banks" :: bankId :: "products" :: productCode:: "attributes" :: productAttributeId :: Nil`
- **Tags**: Product
- **Required Roles**: UpdateProductAttribute
- **Path Parameters**: BANK_ID, PRODUCT_ATTRIBUTE_ID, PRODUCT_CODE
- **Error Codes**: UnknownError
- **Source Files**: APIMethods310.scala

#### `PUT /management/banks/BANK_ID/accounts/ACCOUNT_ID`

- **Endpoint Name**: `updateAccount`
- **Summary**: Update Account
- **Description**: Update the account. ${userAuthenticationMessage(true)} 
- **Route Pattern**: `"management" :: "banks" :: BankId(bankId) :: "accounts" :: AccountId(accountId) :: Nil`
- **Tags**: Account
- **Required Roles**: UpdateAccount
- **Path Parameters**: BANK_ID, ACCOUNT_ID
- **Error Codes**: BankAccountNotFound, UnknownError
- **Source Files**: APIMethods310.scala

#### `PUT /management/banks/BANK_ID/cards/CARD_ID`

- **Endpoint Name**: `updatedCardForBank`
- **Summary**: Update Card
- **Description**: Update Card at bank specified by CARD_ID . ${userAuthenticationMessage(true)} 
- **Route Pattern**: `"management" :: "banks" :: BankId(bankId) :: "cards" :: cardId :: Nil`
- **Tags**: Card
- **Required Roles**: UpdateCardsForBank
- **Path Parameters**: BANK_ID, CARD_ID
- **Error Codes**: UnknownError
- **Source Files**: APIMethods310.scala

#### `PUT /management/banks/BANK_ID/cards/CARD_ID/attributes/CARD_ATTRIBUTE_ID`

- **Endpoint Name**: `updateCardAttribute`
- **Summary**: Update Card Attribute
- **Description**: Update Card Attribute Card Attributes are used to describe a financial Product with a list of typed key value pairs. Each Card Attribute is linked to its Card by CARD_ID ${userAuthenticationMessage(true)} 
- **Tags**: Card
- **Path Parameters**: BANK_ID, CARD_ID, CARD_ATTRIBUTE_ID
- **Error Codes**: UnknownError
- **Source Files**: APIMethods310.scala

#### `PUT /management/consumers/CONSUMER_ID`

- **Endpoint Name**: `enableDisableConsumers`
- **Summary**: Enable or Disable Consumers
- **Description**: Enable/Disable a Consumer specified by CONSUMER_ID. 
- **Route Pattern**: `"management" :: "consumers" :: consumerId :: Nil`
- **Tags**: Consumer
- **Required Roles**: DisableConsumers, EnableConsumers
- **Path Parameters**: CONSUMER_ID
- **Request Body**: putEnabledJSON
- **Error Codes**: UnknownError
- **Source Files**: APIMethods310.scala

#### `PUT /management/consumers/CONSUMER_ID/consumer/call-limits`

- **Endpoint Name**: `callsLimit`
- **Summary**: Set Rate Limiting (call limits) per Consumer
- **Description**:  Set the API rate limiting (call limits) per Consumer: Call limits can be set: Per Second Per Minute Per Hour Per Week Per Month ${userAuthenticationMessage(true)} 
- **Route Pattern**: `"management" :: "consumers" :: consumerId :: "consumer" :: "call-limits" :: Nil`
- **Tags**: Consumer
- **Required Roles**: SetCallLimits
- **Path Parameters**: CONSUMER_ID
- **Error Codes**: UpdateConsumerError, ConsumerNotFound, UnknownError
- **Source Files**: APIMethods310.scala

#### `PUT /management/method_routings/METHOD_ROUTING_ID`

- **Endpoint Name**: `updateMethodRouting`
- **Summary**: Update MethodRouting
- **Description**: Update a MethodRouting. ${userAuthenticationMessage(true)} Explaination of Fields: * method_name is required String value, current supported value: $supportedConnectorNames * connector_name is required String value * is_bank_id_exact_match is required boolean value, if bank_id_pattern is exact bank_id value, this value is true; if bank_id_pattern is null or a regex, this value is false * bank_id_pattern is optional String value, it can be null, a exact bank_id or a regex * parameters is optional
- **Route Pattern**: `"management" :: "method_routings" :: methodRoutingId :: Nil`
- **Path Parameters**: METHOD_ROUTING_ID, _
- **Source Files**: APIMethods310.scala

#### `PUT /system-views/VIEW_ID`

- **Endpoint Name**: `updateSystemView`
- **Summary**: Update System View
- **Description**: Update an existing view on a bank account ${userAuthenticationMessage(true)} and the user needs to have access to the owner view. The json sent is the same as during view creation (above), with one difference: the 'name' field of a view is not editable (it is only set when a view is created)
- **Route Pattern**: `"system-views" :: viewId :: Nil`
- **Tags**: SystemView
- **Required Roles**: UpdateSystemView
- **Path Parameters**: VIEW_ID
- **Request Body**: UpdateViewJSON
- **Error Codes**: BankAccountNotFound, UnknownError
- **Source Files**: APIMethods310.scala

#### `PUT /users/USERNAME/lock-status`

- **Endpoint Name**: `unlockUser`
- **Summary**: Unlock the user
- **Description**:  Unlock a User. (Perhaps the user was locked due to multiple failed login attempts) ${userAuthenticationMessage(true)} 
- **Route Pattern**: `"users" :: username::  "lock-status" :: Nil`
- **Tags**: User
- **Required Roles**: UnlockUser
- **Path Parameters**: USERNAME
- **Error Codes**: UserNotFound, UnknownError
- **Source Files**: APIMethods310.scala

### DELETE Endpoints (9)

#### `DELETE /banks/BANK_ID/branches/BRANCH_ID`

- **Endpoint Name**: `deleteBranch`
- **Summary**: Delete Branch
- **Description**: Delete Branch from given Bank. ${userAuthenticationMessage(true) } 
- **Route Pattern**: `"banks" :: BankId(bankId) :: "branches" :: BranchId(branchId) :: Nil`
- **Tags**: Branch
- **Required Roles**: DeleteBranchAtAnyBank, DeleteBranch
- **Path Parameters**: BANK_ID, BRANCH_ID
- **Error Codes**: BankNotFound, UnknownError
- **Source Files**: APIMethods310.scala

#### `DELETE /banks/BANK_ID/customers/CUSTOMER_ID/addresses/CUSTOMER_ADDRESS_ID`

- **Endpoint Name**: `deleteCustomerAddress`
- **Summary**: Delete Customer Address
- **Description**: Delete an Address of the Customer specified by CUSTOMER_ADDRESS_ID. ${userAuthenticationMessage(true)} 
- **Route Pattern**: `"banks" :: BankId(bankId) :: "customers" :: customerId :: "addresses" :: customerAddressId :: Nil`
- **Tags**: Customer, Kyc
- **Required Roles**: DeleteCustomerAddress
- **Path Parameters**: BANK_ID, CUSTOMER_ADDRESS_ID, CUSTOMER_ID
- **Error Codes**: UnknownError
- **Source Files**: APIMethods310.scala

#### `DELETE /banks/BANK_ID/customers/CUSTOMER_ID/tax_residencies/TAX_RESIDENCE_ID`

- **Endpoint Name**: `deleteTaxResidence`
- **Summary**: Delete Tax Residence
- **Description**: Delete a Tax Residence of the Customer specified by TAX_RESIDENCE_ID. ${userAuthenticationMessage(true)} 
- **Route Pattern**: `"banks" :: BankId(bankId) :: "customers" :: customerId :: "tax_residencies" :: taxResidenceId :: Nil`
- **Tags**: Customer, Kyc
- **Path Parameters**: BANK_ID, CUSTOMER_ID, _, TAX_RESIDENCE_ID
- **Request Body**: entitlementJSONs
- **Response Body**: entitlementJSONs
- **Error Codes**: UnknownError
- **Source Files**: APIMethods310.scala

#### `DELETE /banks/BANK_ID/products/PRODUCT_CODE/attributes/PRODUCT_ATTRIBUTE_ID`

- **Endpoint Name**: `deleteProductAttribute`
- **Summary**: Delete Product Attribute
- **Description**: Delete Product Attribute $productAttributeGeneralInfo Delete a Product Attribute by its id. ${userAuthenticationMessage(true)} 
- **Route Pattern**: `"banks" :: bankId :: "products" :: productCode:: "attributes" :: productAttributeId :: Nil`
- **Tags**: Product
- **Required Roles**: UpdateProductAttribute
- **Path Parameters**: BANK_ID, PRODUCT_ATTRIBUTE_ID, PRODUCT_CODE
- **Error Codes**: BankNotFound, UnknownError
- **Source Files**: APIMethods310.scala

#### `DELETE /management/banks/BANK_ID/cards/CARD_ID`

- **Endpoint Name**: `deleteCardForBank`
- **Summary**: Delete Card
- **Description**: Delete a Card at bank specified by CARD_ID . ${userAuthenticationMessage(true)} 
- **Tags**: Card
- **Required Roles**: CreateCardsForBank
- **Path Parameters**: BANK_ID, CARD_ID
- **Error Codes**: UnknownError
- **Source Files**: APIMethods310.scala

#### `DELETE /management/method_routings/METHOD_ROUTING_ID`

- **Endpoint Name**: `deleteMethodRouting`
- **Summary**: Delete MethodRouting
- **Description**: Delete a MethodRouting specified by METHOD_ROUTING_ID. ${userAuthenticationMessage(true)} 
- **Route Pattern**: `"management" :: "method_routings" :: methodRoutingId :: Nil`
- **Tags**: MethodRouting, Api
- **Required Roles**: DeleteMethodRouting
- **Path Parameters**: METHOD_ROUTING_ID, _
- **Error Codes**: UnknownError
- **Source Files**: APIMethods310.scala

#### `DELETE /management/webui_props/WEB_UI_PROPS_ID`

- **Endpoint Name**: `deleteWebUiProps`
- **Summary**: Delete WebUiProps
- **Description**: Delete a WebUiProps specified by WEB_UI_PROPS_ID. ${userAuthenticationMessage(true)} 
- **Route Pattern**: `"management" :: "webui_props" :: webUiPropsId :: Nil`
- **Tags**: WebUiProps
- **Required Roles**: DeleteWebUiProps
- **Path Parameters**: WEB_UI_PROPS_ID, _
- **Error Codes**: UnknownError
- **Source Files**: APIMethods310.scala

#### `DELETE /users/USER_ID/auth-context`

- **Endpoint Name**: `deleteUserAuthContexts`
- **Summary**: Delete User's Auth Contexts
- **Description**: Delete the Auth Contexts of a User specified by USER_ID. ${userAuthenticationMessage(true)} 
- **Route Pattern**: `"users" :: userId :: "auth-context" :: Nil`
- **Tags**: User
- **Required Roles**: DeleteUserAuthContext
- **Path Parameters**: USER_ID
- **Error Codes**: UnknownError
- **Source Files**: APIMethods310.scala

#### `DELETE /users/USER_ID/auth-context/USER_AUTH_CONTEXT_ID`

- **Endpoint Name**: `deleteUserAuthContextById`
- **Summary**: Delete User Auth Context
- **Description**: Delete a User AuthContext of the User specified by USER_AUTH_CONTEXT_ID. ${userAuthenticationMessage(true)} 
- **Route Pattern**: `"users" :: userId :: "auth-context" :: userAuthContextId :: Nil`
- **Tags**: User
- **Required Roles**: DeleteUserAuthContext
- **Path Parameters**: USER_ID, USER_AUTH_CONTEXT_ID
- **Error Codes**: UnknownError
- **Source Files**: APIMethods310.scala


---

## API Context: v4_0_0

**Total Endpoints**: 247

### GET Endpoints (104)

#### `GET /api-collections/API_COLLECTION_ID/api-collection-endpoints`

- **Endpoint Name**: `getApiCollectionEndpoints`
- **Summary**: Get Api Collection Endpoints
- **Description**: Get Api Collection Endpoints By API_COLLECTION_ID. ${userAuthenticationMessage(false)} 
- **Route Pattern**: `"api-collections" :: apiCollectionId :: "api-collection-endpoints" :: Nil`
- **Tags**: ApiCollection
- **Path Parameters**: API_COLLECTION_ID
- **Error Codes**: UnknownError
- **Source Files**: APIMethods400.scala

#### `GET /api-collections/featured`

- **Endpoint Name**: `getFeaturedApiCollections`
- **Summary**: Get Featured Api Collections
- **Description**: Get Featured Api Collections. ${userAuthenticationMessage(false)} 
- **Route Pattern**: `"api-collections" :: "featured" :: Nil`
- **Tags**: ApiCollection
- **Error Codes**: UnknownError
- **Source Files**: APIMethods400.scala

#### `GET /api-collections/sharable/API_COLLECTION_ID`

- **Endpoint Name**: `getSharableApiCollectionById`
- **Summary**: Get Sharable Api Collection By Id
- **Description**: Get Sharable Api Collection By Id. ${userAuthenticationMessage(false)} 
- **Route Pattern**: `"api-collections" :: "sharable" :: apiCollectionId :: Nil`
- **Tags**: ApiCollection
- **Path Parameters**: API_COLLECTION_ID
- **Error Codes**: UnknownError
- **Source Files**: APIMethods400.scala

#### `GET /api/versions`

- **Endpoint Name**: `getScannedApiVersions`
- **Summary**: Get scanned API Versions
- **Description**: Get all the scanned API Versions.
- **Route Pattern**: `"api" :: "versions" :: Nil`
- **Source Files**: APIMethods400.scala

#### `GET /banks`

- **Endpoint Name**: `getBanks`
- **Summary**: Get Banks
- **Description**: Get banks on this API instance Returns a list of banks supported on this server: * ID used as parameter in URLs * Short and full name of bank * Logo URL * Website
- **Route Pattern**: `"banks" :: BankId(bankId) :: Nil`
- **Request Body**: banksJSON400
- **Response Body**: banksJSON400
- **Source Files**: APIMethods400.scala

#### `GET /banks/BANK_ID`

- **Endpoint Name**: `getBank`
- **Summary**: Get Bank
- **Description**: Get the bank specified by BANK_ID Returns information about a single bank specified by BANK_ID including: * Short and full name of bank * Logo URL * Website
- **Route Pattern**: `"banks" :: BankId(bankId) :: Nil`
- **Path Parameters**: BANK_ID
- **Request Body**: createBankJSON400
- **Source Files**: APIMethods400.scala

#### `GET /banks/BANK_ID/accounts`

- **Endpoint Name**: `getPrivateAccountsAtOneBank`
- **Summary**: Get Accounts at Bank
- **Description**:  Returns the list of accounts at BANK_ID that the user has access to. For each account the API returns the account ID and the views available to the user.. Each account must have at least one private View. optional request parameters for filter with attributes URL params example: /banks/some-bank-id/accounts?&limit=50&offset=1 
- **Route Pattern**: `"banks" :: BankId(bankId) :: "accounts" :: Nil`
- **Tags**: PublicData, Account, PrivateData
- **Path Parameters**: BANK_ID
- **Request Body**: basicAccountsJSON
- **Error Codes**: BankNotFound, UnknownError
- **Source Files**: APIMethods400.scala

#### `GET /banks/BANK_ID/accounts/ACCOUNT_ID/VIEW_ID/account`

- **Endpoint Name**: `getPrivateAccountByIdFull`
- **Summary**: Get Account by Id (Full)
- **Description**: Information returned about an account specified by ACCOUNT_ID as moderated by the view (VIEW_ID): * Number * Owners * Type * Balance * IBAN * Available views (sorted by short_name) More details about the data moderation by the view [here](#1_2_1-getViewsForBankAccount). PSD2 Context: PSD2 requires customers to have access to their account information via third party applications. This call provides balance and other account information via delegated authentication using OAuth. Authentication is 
- **Route Pattern**: `"banks" :: BankId(bankId) :: "accounts" :: AccountId(accountId) :: ViewId(viewId) :: "account" :: Nil`
- **Path Parameters**: BANK_ID, ACCOUNT_ID, VIEW_ID
- **Request Body**: moderatedAccountJSON400
- **Response Body**: moderatedAccountJSON400
- **Source Files**: APIMethods400.scala

#### `GET /banks/BANK_ID/accounts/ACCOUNT_ID/VIEW_ID/transaction-requests/TRANSACTION_REQUEST_ID`

- **Endpoint Name**: `getTransactionRequest`
- **Summary**: Get Transaction Request.
- **Description**: Returns transaction request for transaction specified by TRANSACTION_REQUEST_ID and for account specified by ACCOUNT_ID at bank specified by BANK_ID. The VIEW_ID specified must be 'owner' and the user must have access to this view. Version 2.0.0 now returns charge information. Transaction Requests serve to initiate transactions that may or may not proceed. They contain information including: * Transaction Request Id * Type * Status (INITIATED, COMPLETED) * Challenge (in order to confirm the requ
- **Route Pattern**: `"banks" :: BankId(bankId) :: "accounts" :: AccountId(accountId) :: ViewId(viewId) :: "transaction-requests" :: TransactionRequestId(requestId) :: Nil`
- **Path Parameters**: BANK_ID, ACCOUNT_ID, TRANSACTION_REQUEST_ID, VIEW_ID
- **Request Body**: transactionRequestWithChargeJSON210
- **Response Body**: transactionRequestWithChargeJSON210
- **Error Codes**: BankNotFound, BankAccountNotFound, UnknownError
- **Source Files**: APIMethods400.scala

#### `GET /banks/BANK_ID/accounts/ACCOUNT_ID/VIEW_ID/transactions/TRANSACTION_ID/double-entry-transaction`

- **Endpoint Name**: `getDoubleEntryTransaction`
- **Summary**: Get Double Entry Transaction
- **Description**: Get Double Entry Transaction This endpoint can be used to see the double entry transactions. It returns the `bank_id`, `account_id` and `transaction_id` for the debit end the credit transaction. The other side account can be a settlement account or an OBP account. The endpoint also provide the `transaction_request` object which contains the `bank_id`, `account_id` and `transaction_request_id` of the transaction request at the origin of the transaction. Please note that if none transaction reques
- **Route Pattern**: `"banks" :: BankId(bankId) :: "accounts" :: AccountId(accountId) :: ViewId(viewId) :: "transactions" :: TransactionId(transactionId) :: "double-entry-transaction" :: Nil`
- **Tags**: Transaction
- **Required Roles**: GetDoubleEntryTransactionAtAnyBank, GetDoubleEntryTransactionAtOneBank
- **Path Parameters**: BANK_ID, ACCOUNT_ID, TRANSACTION_ID, VIEW_ID
- **Error Codes**: BankNotFound, BankAccountNotFound, UnknownError
- **Source Files**: APIMethods400.scala

#### `GET /banks/BANK_ID/accounts/ACCOUNT_ID/balances`

- **Endpoint Name**: `getBankAccountBalancesForCurrentUser`
- **Summary**: Get Account Balances
- **Description**: Get the Balances for one Account of the current User at one bank.
- **Route Pattern**: `"banks" :: BankId(bankId) :: "accounts" :: AccountId(accountId) :: "balances" :: Nil`
- **Path Parameters**: BANK_ID, ACCOUNT_ID
- **Source Files**: APIMethods400.scala

#### `GET /banks/BANK_ID/accounts/ACCOUNT_ID/transaction-requests/TRANSACTION_REQUEST_ID/attributes`

- **Endpoint Name**: `getTransactionRequestAttributes`
- **Summary**: Get Transaction Request Attributes
- **Description**: Get Transaction Request Attributes ${userAuthenticationMessage(true)} 
- **Route Pattern**: `"banks" :: BankId(bankId) :: "accounts" :: AccountId(accountId) :: "transaction-requests" :: TransactionRequestId(transactionRequestId) :: "attributes" :: Nil`
- **Tags**: TransactionRequest
- **Required Roles**: GetTransactionRequestAttributesAtOneBank
- **Path Parameters**: BANK_ID, ACCOUNT_ID, TRANSACTION_REQUEST_ID
- **Error Codes**: BankNotFound, BankAccountNotFound, UnknownError
- **Source Files**: APIMethods400.scala

#### `GET /banks/BANK_ID/accounts/ACCOUNT_ID/transaction-requests/TRANSACTION_REQUEST_ID/attributes/ATTRIBUTE_ID`

- **Endpoint Name**: `getTransactionRequestAttributeById`
- **Summary**: Get Transaction Request Attribute By Id
- **Description**: Get Transaction Request Attribute By Id ${userAuthenticationMessage(true)} 
- **Route Pattern**: `"banks" :: BankId(bankId) :: "accounts" :: AccountId(accountId) ::  "transaction-requests" :: TransactionRequestId(transactionRequestId) :: "attributes" :: transactionRequestAttributeId :: Nil`
- **Tags**: TransactionRequest
- **Required Roles**: GetTransactionRequestAttributeAtOneBank
- **Path Parameters**: BANK_ID, ATTRIBUTE_ID, ACCOUNT_ID, TRANSACTION_REQUEST_ID
- **Error Codes**: BankNotFound, BankAccountNotFound, UnknownError
- **Source Files**: APIMethods400.scala

#### `GET /banks/BANK_ID/accounts/ACCOUNT_ID/transactions/TRANSACTION_ID/attributes`

- **Endpoint Name**: `getTransactionAttributes`
- **Summary**: Get Transaction Attributes
- **Description**: Get Transaction Attributes ${userAuthenticationMessage(true)} 
- **Route Pattern**: `"banks" :: BankId(bankId) :: "accounts" :: AccountId(accountId) :: "transactions" :: TransactionId(transactionId) :: "attributes" :: Nil`
- **Tags**: Transaction
- **Required Roles**: GetTransactionAttributesAtOneBank
- **Path Parameters**: BANK_ID, ACCOUNT_ID, TRANSACTION_ID
- **Error Codes**: BankNotFound, BankAccountNotFound, UnknownError
- **Source Files**: APIMethods400.scala

#### `GET /banks/BANK_ID/accounts/ACCOUNT_ID/transactions/TRANSACTION_ID/attributes/ATTRIBUTE_ID`

- **Endpoint Name**: `getTransactionAttributeById`
- **Summary**: Get Transaction Attribute By Id
- **Description**: Get Transaction Attribute By Id ${userAuthenticationMessage(true)} 
- **Route Pattern**: `"banks" :: BankId(bankId) :: "accounts" :: AccountId(accountId) ::  "transactions" :: TransactionId(transactionId) :: "attributes" :: transactionAttributeId :: Nil`
- **Tags**: Transaction
- **Required Roles**: GetTransactionAttributeAtOneBank
- **Path Parameters**: BANK_ID, ATTRIBUTE_ID, ACCOUNT_ID, TRANSACTION_ID
- **Error Codes**: BankNotFound, BankAccountNotFound, UnknownError
- **Source Files**: APIMethods400.scala

#### `GET /banks/BANK_ID/atms`

- **Endpoint Name**: `getAtms`
- **Summary**: Get Bank ATMS
- **Description**: Returns information about ATMs for a single bank specified by BANK_ID including: * Address * Geo Location * License the data under this endpoint is released under Pagination: By default, 100 records are returned. You can use the url query parameters *limit* and *offset* for pagination ${userAuthenticationMessage(!getAtmsIsPublic)}
- **Route Pattern**: `"banks" :: BankId(bankId) :: "atms" :: Nil`
- **Tags**: ATM
- **Path Parameters**: BANK_ID
- **Error Codes**: BankNotFound, UnknownError
- **Source Files**: APIMethods400.scala

#### `GET /banks/BANK_ID/atms/ATM_ID`

- **Endpoint Name**: `getAtm`
- **Summary**: Get Bank ATM
- **Description**: Returns information about ATM for a single bank specified by BANK_ID and ATM_ID including: * Address * Geo Location * License the data under this endpoint is released under ${userAuthenticationMessage(!getAtmsIsPublic)} 
- **Route Pattern**: `"banks" :: BankId(bankId) :: "atms" :: AtmId(atmId) :: Nil`
- **Tags**: ATM
- **Path Parameters**: BANK_ID, ATM_ID
- **Request Body**: CREATE_LOCALISED_RESOURCE_DOC_JSON_TTL
- **Error Codes**: UnknownError
- **Source Files**: APIMethods400.scala

#### `GET /banks/BANK_ID/attribute-definitions/account`

- **Endpoint Name**: `getAccountAttributeDefinition`
- **Summary**: Get Account Attribute Definition
- **Description**: Get Account Attribute Definition ${userAuthenticationMessage(true)} 
- **Route Pattern**: `"banks" :: BankId(bankId) :: "attribute-definitions" :: "account" :: Nil`
- **Tags**: Account
- **Required Roles**: GetAccountAttributeDefinitionAtOneBank
- **Path Parameters**: BANK_ID
- **Error Codes**: BankNotFound, UnknownError
- **Source Files**: APIMethods400.scala

#### `GET /banks/BANK_ID/attribute-definitions/card`

- **Endpoint Name**: `getCardAttributeDefinition`
- **Summary**: Get Card Attribute Definition
- **Description**: Get Card Attribute Definition ${userAuthenticationMessage(true)} 
- **Route Pattern**: `"banks" :: BankId(bankId) :: "attribute-definitions" :: "card" :: Nil`
- **Tags**: Card
- **Required Roles**: GetCardAttributeDefinitionAtOneBank
- **Path Parameters**: BANK_ID
- **Error Codes**: BankNotFound, UnknownError
- **Source Files**: APIMethods400.scala

#### `GET /banks/BANK_ID/attribute-definitions/customer`

- **Endpoint Name**: `getCustomerAttributeDefinition`
- **Summary**: Get Customer Attribute Definition
- **Description**: Get Customer Attribute Definition ${userAuthenticationMessage(true)} 
- **Route Pattern**: `"banks" :: BankId(bankId) :: "attribute-definitions" :: "customer" :: Nil`
- **Tags**: Customer
- **Required Roles**: GetCustomerAttributeDefinitionAtOneBank
- **Path Parameters**: BANK_ID
- **Error Codes**: BankNotFound, UnknownError
- **Source Files**: APIMethods400.scala

#### `GET /banks/BANK_ID/attribute-definitions/product`

- **Endpoint Name**: `getProductAttributeDefinition`
- **Summary**: Get Product Attribute Definition
- **Description**: Get Product Attribute Definition ${userAuthenticationMessage(true)} 
- **Route Pattern**: `"banks" :: BankId(bankId) :: "attribute-definitions" :: "product" :: Nil`
- **Tags**: Product
- **Required Roles**: GetProductAttributeDefinitionAtOneBank
- **Path Parameters**: BANK_ID
- **Error Codes**: BankNotFound, UnknownError
- **Source Files**: APIMethods400.scala

#### `GET /banks/BANK_ID/attribute-definitions/transaction`

- **Endpoint Name**: `getTransactionAttributeDefinition`
- **Summary**: Get Transaction Attribute Definition
- **Description**: Get Transaction Attribute Definition ${userAuthenticationMessage(true)} 
- **Route Pattern**: `"banks" :: BankId(bankId) :: "attribute-definitions" :: "transaction" :: Nil`
- **Tags**: Transaction
- **Required Roles**: GetTransactionAttributeDefinitionAtOneBank
- **Path Parameters**: BANK_ID
- **Error Codes**: BankNotFound, UnknownError
- **Source Files**: APIMethods400.scala

#### `GET /banks/BANK_ID/attribute-definitions/transaction-request`

- **Endpoint Name**: `getTransactionRequestAttributeDefinition`
- **Summary**: Get Transaction Request Attribute Definition
- **Description**: Get Transaction Request Attribute Definition ${userAuthenticationMessage(true)} 
- **Route Pattern**: `"banks" :: BankId(bankId) :: "attribute-definitions" :: "transaction-request" :: Nil`
- **Tags**: TransactionRequest
- **Required Roles**: GetTransactionRequestAttributeDefinitionAtOneBank
- **Path Parameters**: BANK_ID
- **Error Codes**: BankNotFound, UnknownError
- **Source Files**: APIMethods400.scala

#### `GET /banks/BANK_ID/attributes`

- **Endpoint Name**: `getBankAttributes`
- **Summary**: Get Bank Attributes
- **Description**: Get Bank Attributes ${userAuthenticationMessage(true)} 
- **Route Pattern**: `"banks" :: BankId(bankId) :: "attributes" :: Nil`
- **Tags**: Bank
- **Required Roles**: GetBankAttribute
- **Path Parameters**: BANK_ID
- **Error Codes**: BankNotFound, UnknownError
- **Source Files**: APIMethods400.scala

#### `GET /banks/BANK_ID/attributes/BANK_ATTRIBUTE_ID`

- **Endpoint Name**: `getBankAttribute`
- **Summary**: Get Bank Attribute By BANK_ATTRIBUTE_ID
- **Description**: Get Bank Attribute By BANK_ATTRIBUTE_ID ${userAuthenticationMessage(true)} 
- **Route Pattern**: `"banks" :: BankId(bankId) :: "attributes" :: bankAttributeId :: Nil`
- **Tags**: Bank
- **Required Roles**: GetBankAttribute
- **Path Parameters**: BANK_ID, BANK_ATTRIBUTE_ID
- **Error Codes**: BankNotFound, UnknownError
- **Source Files**: APIMethods400.scala

#### `GET /banks/BANK_ID/balances`

- **Endpoint Name**: `getBankAccountsBalancesForCurrentUser`
- **Summary**: Get Accounts Balances
- **Description**: Get the Balances for the Accounts of the current User at one bank.
- **Route Pattern**: `"banks" :: BankId(bankId) :: "balances" :: Nil`
- **Path Parameters**: BANK_ID
- **Source Files**: APIMethods400.scala

#### `GET /banks/BANK_ID/customers`

- **Endpoint Name**: `getCustomersByAttributes`
- **Summary**: Get Customers by ATTRIBUTES
- **Description**: Gets the Customers specified by attributes URL params example: /banks/some-bank-id/customers?name=John&age=8 URL params example: /banks/some-bank-id/customers?&limit=50&offset=1 
- **Route Pattern**: `"banks" :: BankId(bankId) :: "customers" :: Nil`
- **Required Roles**: GetCustomer
- **Path Parameters**: BANK_ID
- **Source Files**: APIMethods400.scala

#### `GET /banks/BANK_ID/customers/CUSTOMER_ID/attributes`

- **Endpoint Name**: `getCustomerAttributes`
- **Summary**: Get Customer Attributes
- **Description**: Get Customer Attributes ${userAuthenticationMessage(true)} 
- **Route Pattern**: `"banks" :: bankId :: "customers" :: customerId :: "attributes" :: Nil`
- **Tags**: Customer
- **Required Roles**: GetCustomerAttributesAtAnyBank, GetCustomerAttributesAtOneBank
- **Path Parameters**: BANK_ID, CUSTOMER_ID
- **Error Codes**: BankNotFound, UnknownError
- **Source Files**: APIMethods400.scala

#### `GET /banks/BANK_ID/customers/CUSTOMER_ID/attributes/ATTRIBUTE_ID`

- **Endpoint Name**: `getCustomerAttributeById`
- **Summary**: Get Customer Attribute By Id
- **Description**: Get Customer Attribute By Id ${userAuthenticationMessage(true)} 
- **Tags**: Customer
- **Required Roles**: GetCustomerAttributeAtOneBank, GetCustomerAttributeAtAnyBank
- **Path Parameters**: BANK_ID, ATTRIBUTE_ID, CUSTOMER_ID
- **Error Codes**: BankNotFound, UnknownError
- **Source Files**: APIMethods400.scala

#### `GET /banks/BANK_ID/customers/CUSTOMER_ID/correlated-users`

- **Endpoint Name**: `getCorrelatedUsersInfoByCustomerId`
- **Summary**: Get Correlated User Info by Customer
- **Description**: Get Correlated User Info by CUSTOMER_ID ${userAuthenticationMessage(true)} 
- **Route Pattern**: `"banks" :: BankId(bankId) :: "customers" :: customerId :: "correlated-users" :: Nil`
- **Tags**: Customer
- **Required Roles**: GetCorrelatedUsersInfoAtAnyBank, GetCorrelatedUsersInfo
- **Path Parameters**: BANK_ID, CUSTOMER_ID
- **Error Codes**: BankNotFound, UnknownError
- **Source Files**: APIMethods400.scala

#### `GET /banks/BANK_ID/customers/CUSTOMER_ID/messages`

- **Endpoint Name**: `getCustomerMessages`
- **Summary**: Get Customer Messages for a Customer
- **Description**: Get messages for the customer specified by CUSTOMER_ID ${userAuthenticationMessage(true)}
- **Route Pattern**: `"banks" :: BankId(bankId) :: "customers" :: customerId :: "messages" :: Nil`
- **Tags**: Message, Customer
- **Required Roles**: GetCustomerMessages
- **Path Parameters**: BANK_ID, CUSTOMER_ID
- **Error Codes**: BankNotFound, UnknownError
- **Source Files**: APIMethods400.scala

#### `GET /banks/BANK_ID/entitlements`

- **Endpoint Name**: `getEntitlementsForBank`
- **Summary**: Get Entitlements for One Bank
- **Description**:  
- **Route Pattern**: `"banks" :: bankId :: "entitlements" :: Nil`
- **Tags**: Entitlement, Role, User
- **Required Roles**: GetEntitlementsForOneBank, GetEntitlementsForAnyBank
- **Path Parameters**: BANK_ID
- **Request Body**: createEntitlementJSONs
- **Error Codes**: UnknownError
- **Source Files**: APIMethods400.scala

#### `GET /banks/BANK_ID/firehose/accounts/views/VIEW_ID`

- **Endpoint Name**: `getFirehoseAccountsAtOneBank`
- **Summary**: Get Firehose Accounts at Bank
- **Description**:  Get Accounts which have a firehose view assigned to them. This endpoint allows bulk access to accounts. Requires the CanUseFirehoseAtAnyBank Role To be shown on the list, each Account must have a firehose View linked to it. A firehose view has is_firehose = true For VIEW_ID try 'owner' optional request parameters for filter with attributes URL params example: /banks/some-bank-id/firehose/accounts/views/owner?&limit=50&offset=1 to invalid Browser cache, add timestamp query parameter as follow, t
- **Tags**: FirehoseData, AccountFirehose, Account
- **Required Roles**: UseAccountFirehose, UseAccountFirehoseAtAnyBank
- **Path Parameters**: BANK_ID, VIEW_ID
- **Error Codes**: BankNotFound
- **Source Files**: APIMethods400.scala

#### `GET /banks/BANK_ID/my/consent-infos`

- **Endpoint Name**: `getConsentInfosByBank`
- **Summary**: Get My Consents Info At Bank
- **Description**:  This endpoint gets the Consents that the current User created at bank. ${userAuthenticationMessage(true)} 
- **Route Pattern**: `"banks" :: BankId(bankId) :: "my" :: "consent-infos" :: Nil`
- **Tags**: Psd2, Consent, PSD2AIS
- **Path Parameters**: BANK_ID
- **Error Codes**: BankNotFound, UnknownError
- **Source Files**: APIMethods400.scala

#### `GET /banks/BANK_ID/my/consents`

- **Endpoint Name**: `getConsents`
- **Summary**: Get Consents
- **Description**:  This endpoint gets the Consents that the current User created. ${userAuthenticationMessage(true)} 
- **Route Pattern**: `"banks" :: BankId(bankId) :: "my" :: "consents" :: Nil`
- **Tags**: Psd2, Consent, PSD2AIS
- **Path Parameters**: BANK_ID
- **Error Codes**: BankNotFound, UnknownError
- **Source Files**: APIMethods400.scala

#### `GET /banks/BANK_ID/products/PRODUCT_CODE`

- **Endpoint Name**: `getProduct`
- **Summary**: Get Bank Product
- **Description**: Returns information about a financial Product offered by the bank specified by BANK_ID and PRODUCT_CODE including: * Name * Code * Parent Product Code * More info URL * Description * Terms and Conditions * Description * Meta * Attributes * Fees ${userAuthenticationMessage(!getProductsIsPublic)}
- **Route Pattern**: `"banks" :: BankId(bankId) :: "products" :: ProductCode(productCode) :: Nil`
- **Tags**: Product
- **Path Parameters**: BANK_ID, PRODUCT_CODE
- **Error Codes**: BankNotFound, UnknownError, ProductNotFound
- **Source Files**: APIMethods400.scala

#### `GET /banks/BANK_ID/products/PRODUCT_CODE/attributes/PRODUCT_ATTRIBUTE_ID`

- **Endpoint Name**: `getProductAttribute`
- **Summary**: Get Product Attribute
- **Description**: Get Product Attribute $productAttributeGeneralInfo Get one product attribute by its id. ${userAuthenticationMessage(true)} 
- **Route Pattern**: `"banks" :: bankId :: "products" :: productCode:: "attributes" :: productAttributeId :: Nil`
- **Tags**: Product
- **Required Roles**: UpdateProductAttribute
- **Path Parameters**: BANK_ID, PRODUCT_ATTRIBUTE_ID, PRODUCT_CODE
- **Error Codes**: UnknownError
- **Source Files**: APIMethods400.scala

#### `GET /banks/BANK_ID/products/PRODUCT_CODE/fees`

- **Endpoint Name**: `getProductFees`
- **Summary**: Get Product Fees
- **Description**: Get Product Fees ${userAuthenticationMessage(false)} 
- **Route Pattern**: `"banks" :: bankId :: "products" :: productCode:: "fees" :: Nil`
- **Tags**: Product
- **Path Parameters**: BANK_ID, PRODUCT_CODE
- **Error Codes**: BankNotFound, UnknownError
- **Source Files**: APIMethods400.scala

#### `GET /banks/BANK_ID/products/PRODUCT_CODE/fees/PRODUCT_FEE_ID`

- **Endpoint Name**: `getProductFee`
- **Summary**: Get Product Fee
- **Description**: Get Product Fee Get one product fee by its id. ${userAuthenticationMessage(false)} 
- **Route Pattern**: `"banks" :: bankId :: "products" :: productCode:: "fees" :: productFeeId :: Nil`
- **Tags**: Product
- **Path Parameters**: BANK_ID, PRODUCT_FEE_ID, PRODUCT_CODE
- **Error Codes**: BankNotFound, UnknownError
- **Source Files**: APIMethods400.scala

#### `GET /banks/BANK_ID/settlement-accounts`

- **Endpoint Name**: `getSettlementAccounts`
- **Summary**: Get Settlement accounts at Bank
- **Description**: Get settlement accounts on this API instance Returns a list of settlement accounts at this Bank Note: a settlement account is considered as a bank account. So you can update it and add account attributes to it using the regular account endpoints 
- **Route Pattern**: `"banks" :: BankId(bankId) :: "settlement-accounts" :: Nil`
- **Tags**: Psd2, Bank
- **Required Roles**: GetSettlementAccountAtOneBank
- **Path Parameters**: BANK_ID
- **Error Codes**: BankNotFound, UnknownError
- **Source Files**: APIMethods400.scala

#### `GET /banks/BANK_ID/user-invitations`

- **Endpoint Name**: `getUserInvitations`
- **Summary**: Get User Invitations
- **Description**: Get User Invitations ${userAuthenticationMessage(true)} 
- **Route Pattern**: `"banks" :: BankId(bankId) :: "user-invitations" :: Nil`
- **Tags**: UserInvitation
- **Required Roles**: GetUserInvitation
- **Path Parameters**: BANK_ID
- **Error Codes**: BankNotFound, UnknownError
- **Source Files**: APIMethods400.scala

#### `GET /banks/BANK_ID/user-invitations/SECRET_LINK`

- **Endpoint Name**: `getUserInvitation`
- **Summary**: Get User Invitation
- **Description**: Get User Invitation ${userAuthenticationMessage(true)} 
- **Route Pattern**: `"banks" :: BankId(bankId) :: "user-invitations" :: secretLink :: Nil`
- **Tags**: UserInvitation
- **Required Roles**: GetUserInvitation
- **Path Parameters**: BANK_ID, SECRET_LINK
- **Error Codes**: BankNotFound, UnknownError
- **Source Files**: APIMethods400.scala

#### `GET /banks/BANK_ID/user_customer_links/customers/CUSTOMER_ID`

- **Endpoint Name**: `getUserCustomerLinksByCustomerId`
- **Summary**: Get User Customer Links by Customer
- **Description**: Get User Customer Links by CUSTOMER_ID ${userAuthenticationMessage(true)} 
- **Route Pattern**: `"banks" :: BankId(bankId) :: "user_customer_links" :: "customers" :: customerId :: Nil`
- **Tags**: Customer
- **Required Roles**: GetUserCustomerLink
- **Path Parameters**: BANK_ID, CUSTOMER_ID, _
- **Request Body**: createUserCustomerLinkJSONs
- **Error Codes**: BankNotFound, UnknownError
- **Source Files**: APIMethods400.scala

#### `GET /banks/BANK_ID/user_customer_links/users/USER_ID`

- **Endpoint Name**: `getUserCustomerLinksByUserId`
- **Summary**: Get User Customer Links by User
- **Description**: Get User Customer Links by USER_ID ${userAuthenticationMessage(true)} 
- **Route Pattern**: `"banks" :: BankId(bankId) :: "user_customer_links" :: "users" :: userId :: Nil`
- **Tags**: Customer
- **Required Roles**: GetUserCustomerLink
- **Path Parameters**: BANK_ID, USER_ID, _
- **Request Body**: createUserCustomerLinkJSONs
- **Error Codes**: BankNotFound, UnknownError
- **Source Files**: APIMethods400.scala

#### `GET /consumers/CONSUMER_ID/scopes`

- **Endpoint Name**: `getScopes`
- **Summary**: Get Scopes for Consumer
- **Description**: Get all the scopes for an consumer specified by CONSUMER_ID ${userAuthenticationMessage(true)} 
- **Route Pattern**: `"consumers" :: uuidOfConsumer :: "scopes" :: Nil`
- **Tags**: Scope, Consumer
- **Path Parameters**: CONSUMER_ID
- **Request Body**: createScopeJSONs
- **Error Codes**: EntitlementNotFound, ConsumerNotFound, UnknownError
- **Source Files**: APIMethods400.scala

#### `GET /customers`

- **Endpoint Name**: `getCustomersAtAnyBank`
- **Summary**: Get Customers at Any Bank
- **Description**: Get Customers at Any Bank. ${userAuthenticationMessage(true)} 
- **Route Pattern**: `case "customers" :: Nil JsonGet`
- **Tags**: Customer, User
- **Required Roles**: GetCustomersAtAnyBank
- **Error Codes**: UserCustomerLinksNotFound, UnknownError
- **Source Files**: APIMethods400.scala

#### `GET /customers-minimal`

- **Endpoint Name**: `getCustomersMinimalAtAnyBank`
- **Summary**: Get Customers Minimal at Any Bank
- **Description**: Get Customers Minimal at Any Bank. ${userAuthenticationMessage(true)} 
- **Route Pattern**: `case "customers-minimal" :: Nil JsonGet`
- **Tags**: Customer, User
- **Required Roles**: GetCustomersMinimalAtAnyBank
- **Error Codes**: UserCustomerLinksNotFound, UnknownError
- **Source Files**: APIMethods400.scala

#### `GET /customers/CUSTOMER_ID/accounts-minimal`

- **Endpoint Name**: `getAccountsMinimalByCustomerId`
- **Summary**: Get Accounts Minimal for a Customer
- **Description**: Get Accounts Minimal by CUSTOMER_ID ${userAuthenticationMessage(true)} 
- **Route Pattern**: `"customers" :: customerId :: "accounts-minimal" :: Nil`
- **Tags**: Account
- **Required Roles**: GetAccountsMinimalForCustomerAtAnyBank
- **Path Parameters**: CUSTOMER_ID
- **Error Codes**: CustomerNotFound, UnknownError
- **Source Files**: APIMethods400.scala

#### `GET /database/info`

- **Endpoint Name**: `getMapperDatabaseInfo`
- **Summary**: Get Mapper Database Info
- **Description**: Get basic information about the Mapper Database. ${userAuthenticationMessage(true)} 
- **Route Pattern**: `"database" :: "info" :: Nil`
- **Tags**: Api
- **Required Roles**: GetDatabaseInfo
- **Error Codes**: UnknownError
- **Source Files**: APIMethods400.scala

#### `GET /development/call_context`

- **Endpoint Name**: `getCallContext`
- **Summary**: Get the Call Context of a current call
- **Description**: Get the Call Context of the current call. 
- **Route Pattern**: `"development" :: "call_context" :: Nil`
- **Tags**: Api
- **Required Roles**: GetCallContext
- **Path Parameters**: _
- **Error Codes**: UnknownError
- **Source Files**: APIMethods400.scala

#### `GET /development/echo/jws-verified-request-jws-signed-response`

- **Endpoint Name**: `verifyRequestSignResponse`
- **Summary**: Verify Request and Sign Response of a current call
- **Description**: Verify Request and Sign Response of a current call. 
- **Route Pattern**: `"development" :: "echo":: "jws-verified-request-jws-signed-response" :: Nil`
- **Tags**: Api
- **Error Codes**: UnknownError
- **Source Files**: APIMethods400.scala

#### `GET /endpoints/authentication-type-validations`

- **Endpoint Name**: `getAllAuthenticationTypeValidationsPublic`
- **Summary**: Get all Authentication Type Validations - public
- **Description**: Get all Authentication Type Validations - public. 
- **Required Roles**: CreateConnectorMethod
- **Error Codes**: UnknownError
- **Source Files**: APIMethods400.scala

#### `GET /endpoints/json-schema-validations`

- **Endpoint Name**: `getAllJsonSchemaValidationsPublic`
- **Summary**: Get all JSON Schema Validations - public
- **Description**: Get all JSON Schema Validations - public. 
- **Tags**: JsonSchemaValidation
- **Required Roles**: CreateAuthenticationTypeValidation
- **Error Codes**: UnknownError
- **Source Files**: APIMethods400.scala

#### `GET /management/authentication-type-validations`

- **Endpoint Name**: `getAllAuthenticationTypeValidations`
- **Summary**: Get all Authentication Type Validations
- **Description**: Get all Authentication Type Validations. 
- **Required Roles**: GetAuthenticationTypeValidation
- **Source Files**: APIMethods400.scala

#### `GET /management/authentication-type-validations/OPERATION_ID`

- **Endpoint Name**: `getAuthenticationTypeValidation`
- **Summary**: Get an Authentication Type Validation
- **Description**: Get an Authentication Type Validation by operation_id. 
- **Route Pattern**: `"management" :: "authentication-type-validations" :: operationId :: Nil`
- **Tags**: AuthenticationTypeValidation
- **Required Roles**: GetAuthenticationTypeValidation
- **Path Parameters**: OPERATION_ID
- **Error Codes**: UnknownError
- **Source Files**: APIMethods400.scala

#### `GET /management/banks/BANK_ID/accounts/ACCOUNT_ID/VIEW_ID/counterparties`

- **Endpoint Name**: `getCounterpartiesForAnyAccount`
- **Summary**: Get Counterparties for any account (Explicit)
- **Description**: This is a management endpoint that gets the Counterparties that have been explicitly created for an Account / View. For a general introduction to Counterparties in OBP, see ${Glossary.getGlossaryItemLink("Counterparties")} ${userAuthenticationMessage(true)} 
- **Route Pattern**: `"management" :: "banks" :: BankId(bankId) :: "accounts" :: AccountId(accountId) :: ViewId(viewId) :: "counterparties" :: Nil`
- **Tags**: Psd2, Account, Counterparty, PSD2PIS
- **Required Roles**: GetCounterpartiesAtAnyBank, GetCounterparties
- **Path Parameters**: BANK_ID, ACCOUNT_ID, VIEW_ID
- **Error Codes**: BankNotFound, BankAccountNotFound, UnknownError
- **Source Files**: APIMethods400.scala

#### `GET /management/banks/BANK_ID/accounts/ACCOUNT_ID/VIEW_ID/counterparties/COUNTERPARTY_ID`

- **Endpoint Name**: `getCounterpartyByIdForAnyAccount`
- **Summary**: Get Counterparty by Id for any account (Explicit)
- **Description**: This is a management endpoint that gets information about any single explicitly created Counterparty on an Account / View specified by its COUNTERPARTY_ID", For a general introduction to Counterparties in OBP, see ${Glossary.getGlossaryItemLink("Counterparties")} ${userAuthenticationMessage(true)} 
- **Route Pattern**: `"management" :: "banks" :: BankId(bankId) :: "accounts" :: AccountId(accountId) :: ViewId(viewId):: "counterparties" :: CounterpartyId(counterpartyId) :: Nil`
- **Tags**: Account, Counterparty
- **Required Roles**: GetCounterpartyAtAnyBank, GetCounterparty
- **Path Parameters**: BANK_ID, COUNTERPARTY_ID, ACCOUNT_ID, VIEW_ID
- **Error Codes**: BankNotFound, BankAccountNotFound, ViewNotFound, UnknownError
- **Source Files**: APIMethods400.scala

#### `GET /management/banks/BANK_ID/accounts/ACCOUNT_ID/VIEW_ID/counterparty-names/COUNTERPARTY_NAME`

- **Endpoint Name**: `getCounterpartyByNameForAnyAccount`
- **Summary**: Get Counterparty by name for any account (Explicit) 
- **Description**: This is a management endpoint that allows the retrieval of any Counterparty on an Account / View by its Name. For a general introduction to Counterparties in OBP, see ${Glossary.getGlossaryItemLink("Counterparties")} ${userAuthenticationMessage(true)} 
- **Route Pattern**: `"management" :: "banks" :: BankId(bankId) :: "accounts" :: AccountId(accountId) :: ViewId(viewId):: "counterparty-names" :: counterpartyName :: Nil`
- **Tags**: Account, Counterparty
- **Required Roles**: GetCounterpartyAtAnyBank, GetCounterparty
- **Path Parameters**: BANK_ID, ACCOUNT_ID, COUNTERPARTY_NAME, VIEW_ID
- **Error Codes**: BankNotFound, BankAccountNotFound, ViewNotFound, UnknownError
- **Source Files**: APIMethods400.scala

#### `GET /management/banks/BANK_ID/dynamic-endpoints`

- **Endpoint Name**: `getBankLevelDynamicEndpoints`
- **Summary**: Get Bank Level Dynamic Endpoints
- **Description**:  Get Bank Level Dynamic Endpoints. 
- **Route Pattern**: `"management" :: "banks" :: bankId :: "dynamic-endpoints" :: Nil`
- **Required Roles**: GetDynamicEndpoints, GetBankLevelDynamicEndpoints
- **Path Parameters**: BANK_ID
- **Source Files**: APIMethods400.scala

#### `GET /management/banks/BANK_ID/dynamic-endpoints/DYNAMIC_ENDPOINT_ID`

- **Endpoint Name**: `getBankLevelDynamicEndpoint`
- **Summary**:  Get Bank Level Dynamic Endpoint
- **Description**: Get a Bank Level Dynamic Endpoint. 
- **Route Pattern**: `"management" :: "banks" :: bankId :: "dynamic-endpoints" :: dynamicEndpointId :: Nil`
- **Tags**: Api, ManageDynamicEndpoint
- **Required Roles**: GetBankLevelDynamicEndpoint, GetDynamicEndpoint
- **Path Parameters**: BANK_ID, DYNAMIC_ENDPOINT_ID
- **Error Codes**: BankNotFound, DynamicEndpointNotFound, UnknownError
- **Source Files**: APIMethods400.scala

#### `GET /management/banks/BANK_ID/dynamic-entities`

- **Endpoint Name**: `getBankLevelDynamicEntities`
- **Summary**: Get Bank Level Dynamic Entities
- **Description**: Get all the bank level Dynamic Entities for one bank.
- **Route Pattern**: `"management" :: "banks" :: bankId :: "dynamic-entities" :: Nil`
- **Required Roles**: GetBankLevelDynamicEntities
- **Path Parameters**: BANK_ID
- **Source Files**: APIMethods400.scala

#### `GET /management/banks/BANK_ID/dynamic-message-docs`

- **Endpoint Name**: `getAllBankLevelDynamicMessageDocs`
- **Summary**: Get all Bank Level Dynamic Message Docs
- **Description**: Get all Bank Level Dynamic Message Docs. 
- **Route Pattern**: `"management" :: "banks" :: bankId :: "dynamic-message-docs" :: Nil`
- **Tags**: DynamicMessageDoc
- **Required Roles**: GetAllDynamicMessageDocs
- **Path Parameters**: BANK_ID
- **Error Codes**: BankNotFound, UnknownError
- **Source Files**: APIMethods400.scala

#### `GET /management/banks/BANK_ID/dynamic-message-docs/DYNAMIC_MESSAGE_DOC_ID`

- **Endpoint Name**: `getBankLevelDynamicMessageDoc`
- **Summary**: Get Bank Level Dynamic Message Doc
- **Description**: Get a Bank Level Dynamic Message Doc by DYNAMIC_MESSAGE_DOC_ID. 
- **Route Pattern**: `"management" :: "banks" :: bankId :: "dynamic-message-docs" :: dynamicMessageDocId :: Nil`
- **Tags**: DynamicMessageDoc
- **Required Roles**: GetBankLevelDynamicMessageDoc
- **Path Parameters**: BANK_ID, DYNAMIC_MESSAGE_DOC_ID
- **Error Codes**: BankNotFound, UnknownError
- **Source Files**: APIMethods400.scala

#### `GET /management/banks/BANK_ID/dynamic-resource-docs`

- **Endpoint Name**: `getAllBankLevelDynamicResourceDocs`
- **Summary**: Get all Bank Level Dynamic Resource Docs
- **Description**: Get all Bank Level Dynamic Resource Docs. 
- **Route Pattern**: `"management" :: "banks" :: bankId :: "dynamic-resource-docs" :: Nil`
- **Tags**: DynamicResourceDoc
- **Required Roles**: GetAllBankLevelDynamicResourceDocs
- **Path Parameters**: BANK_ID
- **Error Codes**: BankNotFound, UnknownError
- **Source Files**: APIMethods400.scala

#### `GET /management/banks/BANK_ID/dynamic-resource-docs/DYNAMIC-RESOURCE-DOC-ID`

- **Endpoint Name**: `getBankLevelDynamicResourceDoc`
- **Summary**: Get Bank Level Dynamic Resource Doc by Id
- **Description**: Get a Bank Level Dynamic Resource Doc by DYNAMIC-RESOURCE-DOC-ID. 
- **Route Pattern**: `"management" :: "banks" :: bankId :: "dynamic-resource-docs" :: dynamicResourceDocId :: Nil`
- **Tags**: DynamicResourceDoc
- **Required Roles**: GetBankLevelDynamicResourceDoc
- **Path Parameters**: BANK_ID, DYNAMIC, RESOURCE, ID, DOC
- **Error Codes**: BankNotFound, UnknownError
- **Source Files**: APIMethods400.scala

#### `GET /management/banks/BANK_ID/endpoint-mappings`

- **Endpoint Name**: `getAllBankLevelEndpointMappings`
- **Summary**: Get all Bank Level Endpoint Mappings
- **Description**: Get all Bank Level Endpoint Mappings. 
- **Route Pattern**: `"management" :: "banks" :: bankId :: "endpoint-mappings" :: Nil`
- **Tags**: EndpointMapping
- **Required Roles**: GetAllBankLevelEndpointMappings, GetAllEndpointMappings
- **Path Parameters**: BANK_ID
- **Error Codes**: BankNotFound, UnknownError
- **Source Files**: APIMethods400.scala

#### `GET /management/banks/BANK_ID/endpoint-mappings/ENDPOINT_MAPPING_ID`

- **Endpoint Name**: `getBankLevelEndpointMapping`
- **Summary**: Get Bank Level Endpoint Mapping
- **Description**: Get an Bank Level Endpoint Mapping by ENDPOINT_MAPPING_ID. 
- **Route Pattern**: `"management" :: "banks" :: bankId :: "endpoint-mappings" :: endpointMappingId :: Nil`
- **Tags**: EndpointMapping
- **Required Roles**: GetEndpointMapping, GetBankLevelEndpointMapping
- **Path Parameters**: BANK_ID, ENDPOINT_MAPPING_ID
- **Error Codes**: BankNotFound, UnknownError
- **Source Files**: APIMethods400.scala

#### `GET /management/banks/BANK_ID/endpoints/OPERATION_ID/tags`

- **Endpoint Name**: `getBankLevelEndpointTags`
- **Summary**: Get Bank Level Endpoint Tags
- **Description**: Get Bank Level Endpoint Tags.
- **Tags**: Api
- **Required Roles**: GetBankLevelEndpointTag
- **Path Parameters**: BANK_ID, OPERATION_ID
- **Error Codes**: BankNotFound, UnknownError
- **Source Files**: APIMethods400.scala

#### `GET /management/banks/BANK_ID/fast-firehose/accounts`

- **Endpoint Name**: `getFastFirehoseAccountsAtOneBank`
- **Summary**: Get Fast Firehose Accounts at Bank
- **Description**:  This endpoint allows bulk access to accounts. optional pagination parameters for filter with accounts ${urlParametersDocument(true, false)} ${userAuthenticationMessage(true)} 
- **Tags**: FirehoseData, AccountFirehose, Account
- **Required Roles**: UseAccountFirehose, UseAccountFirehoseAtAnyBank
- **Path Parameters**: BANK_ID
- **Request Body**: createFirehoseBankAccountJSON
- **Error Codes**: BankNotFound
- **Source Files**: APIMethods400.scala

#### `GET /management/connector-methods`

- **Endpoint Name**: `getAllConnectorMethods`
- **Summary**: Get all Connector Methods
- **Description**: Get all Connector Methods. 
- **Route Pattern**: `"management" :: "connector-methods" :: Nil`
- **Tags**: ConnectorMethod
- **Required Roles**: GetAllConnectorMethods
- **Error Codes**: UnknownError
- **Source Files**: APIMethods400.scala

#### `GET /management/connector-methods/CONNECTOR_METHOD_ID`

- **Endpoint Name**: `getConnectorMethod`
- **Summary**: Get Connector Method by Id
- **Description**: Get an internal connector by CONNECTOR_METHOD_ID. 
- **Route Pattern**: `"management" :: "connector-methods" :: connectorMethodId :: Nil`
- **Tags**: ConnectorMethod
- **Required Roles**: GetConnectorMethod
- **Path Parameters**: CONNECTOR_METHOD_ID
- **Error Codes**: UnknownError
- **Source Files**: APIMethods400.scala

#### `GET /management/dynamic-endpoints`

- **Endpoint Name**: `getDynamicEndpoints`
- **Summary**:  Get Dynamic Endpoints
- **Description**:  Get Dynamic Endpoints. 
- **Route Pattern**: `"management" :: "dynamic-endpoints" :: Nil`
- **Required Roles**: GetDynamicEndpoints
- **Source Files**: APIMethods400.scala

#### `GET /management/dynamic-endpoints/DYNAMIC_ENDPOINT_ID`

- **Endpoint Name**: `getDynamicEndpoint`
- **Summary**: Get Dynamic Endpoint
- **Description**: Get a Dynamic Endpoint. Get one DynamicEndpoint, 
- **Route Pattern**: `"management" :: "dynamic-endpoints" :: dynamicEndpointId :: Nil`
- **Tags**: Api, ManageDynamicEndpoint
- **Required Roles**: GetDynamicEndpoint
- **Path Parameters**: DYNAMIC_ENDPOINT_ID
- **Error Codes**: DynamicEndpointNotFound, UnknownError
- **Source Files**: APIMethods400.scala

#### `GET /management/dynamic-message-docs`

- **Endpoint Name**: `getAllDynamicMessageDocs`
- **Summary**: Get all Dynamic Message Docs
- **Description**: Get all Dynamic Message Docs. 
- **Route Pattern**: `"management" :: "dynamic-message-docs" :: Nil`
- **Tags**: DynamicMessageDoc
- **Required Roles**: GetAllDynamicMessageDocs
- **Error Codes**: UnknownError
- **Source Files**: APIMethods400.scala

#### `GET /management/dynamic-message-docs/DYNAMIC_MESSAGE_DOC_ID`

- **Endpoint Name**: `getDynamicMessageDoc`
- **Summary**: Get Dynamic Message Doc
- **Description**: Get a Dynamic Message Doc by DYNAMIC_MESSAGE_DOC_ID. 
- **Route Pattern**: `"management" :: "dynamic-message-docs" :: dynamicMessageDocId :: Nil`
- **Tags**: DynamicMessageDoc
- **Required Roles**: GetDynamicMessageDoc
- **Path Parameters**: DYNAMIC_MESSAGE_DOC_ID
- **Error Codes**: UnknownError
- **Source Files**: APIMethods400.scala

#### `GET /management/dynamic-resource-docs`

- **Endpoint Name**: `getAllDynamicResourceDocs`
- **Summary**: Get all Dynamic Resource Docs
- **Description**: Get all Dynamic Resource Docs. 
- **Route Pattern**: `"management" :: "dynamic-resource-docs" :: Nil`
- **Tags**: DynamicResourceDoc
- **Required Roles**: GetAllDynamicResourceDocs
- **Error Codes**: UnknownError
- **Source Files**: APIMethods400.scala

#### `GET /management/dynamic-resource-docs/DYNAMIC-RESOURCE-DOC-ID`

- **Endpoint Name**: `getDynamicResourceDoc`
- **Summary**: Get Dynamic Resource Doc by Id
- **Description**: Get a Dynamic Resource Doc by DYNAMIC-RESOURCE-DOC-ID. 
- **Route Pattern**: `"management" :: "dynamic-resource-docs" :: dynamicResourceDocId :: Nil`
- **Tags**: DynamicResourceDoc
- **Required Roles**: GetDynamicResourceDoc
- **Path Parameters**: DOC, RESOURCE, ID, DYNAMIC
- **Error Codes**: UnknownError
- **Source Files**: APIMethods400.scala

#### `GET /management/endpoint-mappings`

- **Endpoint Name**: `getAllEndpointMappings`
- **Summary**: Get all Endpoint Mappings
- **Description**: Get all Endpoint Mappings. 
- **Route Pattern**: `"management" :: "endpoint-mappings" :: Nil`
- **Tags**: EndpointMapping
- **Required Roles**: GetAllEndpointMappings
- **Error Codes**: UnknownError
- **Source Files**: APIMethods400.scala

#### `GET /management/endpoint-mappings/ENDPOINT_MAPPING_ID`

- **Endpoint Name**: `getEndpointMapping`
- **Summary**: Get Endpoint Mapping by Id
- **Description**: Get an Endpoint Mapping by ENDPOINT_MAPPING_ID. 
- **Route Pattern**: `"management" :: "endpoint-mappings" :: endpointMappingId :: Nil`
- **Tags**: EndpointMapping
- **Required Roles**: GetEndpointMapping
- **Path Parameters**: ENDPOINT_MAPPING_ID
- **Error Codes**: UnknownError
- **Source Files**: APIMethods400.scala

#### `GET /management/endpoints/OPERATION_ID/tags`

- **Endpoint Name**: `getSystemLevelEndpointTags`
- **Summary**: Get System Level Endpoint Tags
- **Description**: Get System Level Endpoint Tags.
- **Route Pattern**: `"management" :: "endpoints" :: operationId :: "tags" :: Nil`
- **Tags**: Api
- **Required Roles**: GetSystemLevelEndpointTag
- **Path Parameters**: OPERATION_ID
- **Error Codes**: UnknownError
- **Source Files**: APIMethods400.scala

#### `GET /management/json-schema-validations`

- **Endpoint Name**: `getAllJsonSchemaValidations`
- **Summary**: Get all JSON Schema Validations
- **Description**: Get all JSON Schema Validations. 
- **Tags**: JsonSchemaValidation
- **Required Roles**: GetJsonSchemaValidation
- **Error Codes**: UnknownError
- **Source Files**: APIMethods400.scala

#### `GET /management/json-schema-validations/OPERATION_ID`

- **Endpoint Name**: `getJsonSchemaValidation`
- **Summary**: Get a JSON Schema Validation
- **Description**: Get a JSON Schema Validation by operation_id. 
- **Route Pattern**: `"management" :: "json-schema-validations" :: operationId :: Nil`
- **Tags**: JsonSchemaValidation
- **Required Roles**: GetJsonSchemaValidation
- **Path Parameters**: OPERATION_ID
- **Error Codes**: UnknownError
- **Source Files**: APIMethods400.scala

#### `GET /management/system-dynamic-entities`

- **Endpoint Name**: `getSystemDynamicEntities`
- **Summary**: Get System Dynamic Entities
- **Description**: Get all System Dynamic Entities
- **Route Pattern**: `"management" :: "system-dynamic-entities" :: Nil`
- **Required Roles**: GetSystemLevelDynamicEntities
- **Source Files**: APIMethods400.scala

#### `GET /my/api-collection-ids/API_COLLECTION_ID/api-collection-endpoints`

- **Endpoint Name**: `getMyApiCollectionEndpointsById`
- **Summary**: Get My Api Collection Endpoints By Id
- **Description**: Get Api Collection Endpoints By API_COLLECTION_ID. ${userAuthenticationMessage(true)} 
- **Tags**: ApiCollection
- **Path Parameters**: API_COLLECTION_ID
- **Error Codes**: UserNotFound, UnknownError
- **Source Files**: APIMethods400.scala

#### `GET /my/api-collections`

- **Endpoint Name**: `getMyApiCollections`
- **Summary**: Get My Api Collections
- **Description**: Get all the apiCollections for logged in user. ${userAuthenticationMessage(true)} 
- **Route Pattern**: `"my" :: "api-collections" :: Nil`
- **Tags**: ApiCollection
- **Error Codes**: UnknownError
- **Source Files**: APIMethods400.scala

#### `GET /my/api-collections/API_COLLECTION_ID`

- **Endpoint Name**: `getMyApiCollectionById`
- **Summary**: Get My Api Collection By Id
- **Description**: Get Api Collection By API_COLLECTION_ID. ${userAuthenticationMessage(true)} 
- **Route Pattern**: `"my" :: "api-collections" :: apiCollectionId :: Nil`
- **Tags**: ApiCollection
- **Path Parameters**: API_COLLECTION_ID
- **Error Codes**: UserNotFound, UnknownError
- **Source Files**: APIMethods400.scala

#### `GET /my/api-collections/API_COLLECTION_NAME/api-collection-endpoints`

- **Endpoint Name**: `getMyApiCollectionEndpoints`
- **Summary**: Get My Api Collection Endpoints
- **Description**: Get Api Collection Endpoints By API_COLLECTION_NAME. ${userAuthenticationMessage(true)} 
- **Tags**: ApiCollection
- **Path Parameters**: API_COLLECTION_NAME
- **Error Codes**: UserNotFound, UnknownError
- **Source Files**: APIMethods400.scala

#### `GET /my/api-collections/API_COLLECTION_NAME/api-collection-endpoints/OPERATION_ID`

- **Endpoint Name**: `getMyApiCollectionEndpoint`
- **Summary**: Get My Api Collection Endpoint
- **Description**: Get Api Collection Endpoint By API_COLLECTION_NAME and OPERATION_ID. ${userAuthenticationMessage(false)} 
- **Route Pattern**: `"my" :: "api-collections" :: apiCollectionName :: "api-collection-endpoints" :: operationId :: Nil`
- **Tags**: ApiCollection
- **Path Parameters**: OPERATION_ID, API_COLLECTION_NAME
- **Error Codes**: UserNotFound, UnknownError
- **Source Files**: APIMethods400.scala

#### `GET /my/api-collections/name/API_COLLECTION_NAME`

- **Endpoint Name**: `getMyApiCollectionByName`
- **Summary**: Get My Api Collection By Name
- **Description**: Get Api Collection By API_COLLECTION_NAME. ${userAuthenticationMessage(true)} 
- **Route Pattern**: `"my" :: "api-collections" :: "name" ::apiCollectionName :: Nil`
- **Tags**: ApiCollection
- **Path Parameters**: API_COLLECTION_NAME
- **Error Codes**: UserNotFound, UnknownError
- **Source Files**: APIMethods400.scala

#### `GET /my/banks/BANK_ID/accounts/ACCOUNT_ID/account`

- **Endpoint Name**: `getCoreAccountById`
- **Summary**: Get Account by Id (Core)
- **Description**: Information returned about the account specified by ACCOUNT_ID: * Number - The human readable account number given by the bank that identifies the account. * Label - A label given by the owner of the account * Owners - Users that own this account * Type - The type of account * Balance - Currency and Value * Account Routings - A list that might include IBAN or national account identifiers * Account Rules - A list that might include Overdraft and other bank specific rules * Tags - A list of Tags a
- **Route Pattern**: `"my" :: "banks" :: BankId(bankId) :: "accounts" :: AccountId(accountId) :: "account" :: Nil`
- **Path Parameters**: BANK_ID, ACCOUNT_ID
- **Source Files**: APIMethods400.scala

#### `GET /my/consent-infos`

- **Endpoint Name**: `getConsentInfos`
- **Summary**: Get My Consents Info
- **Description**:  This endpoint gets the Consents that the current User created. ${userAuthenticationMessage(true)} 
- **Route Pattern**: `"my" :: "consent-infos" :: Nil`
- **Tags**: Psd2, Consent, PSD2AIS
- **Error Codes**: BankNotFound, UnknownError
- **Source Files**: APIMethods400.scala

#### `GET /my/correlated-entities`

- **Endpoint Name**: `getMyCorrelatedEntities`
- **Summary**: Get Correlated Entities for the current User
- **Description**: Correlated Entities are users and customers linked to the currently authenticated user via User-Customer-Links ${userAuthenticationMessage(true)} 
- **Route Pattern**: `"my" :: "correlated-entities" :: Nil`
- **Tags**: Customer
- **Error Codes**: BankNotFound, UnknownError
- **Source Files**: APIMethods400.scala

#### `GET /my/dynamic-endpoints`

- **Endpoint Name**: `getMyDynamicEndpoints`
- **Summary**: Get My Dynamic Endpoints
- **Description**: Get My Dynamic Endpoints.
- **Route Pattern**: `"my" :: "dynamic-endpoints" :: Nil`
- **Source Files**: APIMethods400.scala

#### `GET /my/dynamic-entities`

- **Endpoint Name**: `getMyDynamicEntities`
- **Summary**: Get My Dynamic Entities
- **Description**: Get all my Dynamic Entities.
- **Route Pattern**: `"my" :: "dynamic-entities" :: Nil`
- **Source Files**: APIMethods400.scala

#### `GET /my/spaces`

- **Endpoint Name**: `getMySpaces`
- **Summary**: Get My Spaces
- **Description**: Get My Spaces.
- **Route Pattern**: `"my" :: "spaces" :: Nil`
- **Tags**: User
- **Error Codes**: UnknownError
- **Source Files**: APIMethods400.scala

#### `GET /my/user/attributes`

- **Endpoint Name**: `getMyPersonalUserAttributes`
- **Summary**: Get My Personal User Attributes
- **Description**: Get My Personal User Attributes. ${userAuthenticationMessage(true)} 
- **Route Pattern**: `"my" :: "user" :: "attributes" :: Nil`
- **Tags**: User
- **Error Codes**: UnknownError
- **Source Files**: APIMethods400.scala

#### `GET /transactions/TRANSACTION_ID/balancing-transaction`

- **Endpoint Name**: `getBalancingTransaction`
- **Summary**: Get Balancing Transaction
- **Description**: Get Balancing Transaction ${userAuthenticationMessage(true)} 
- **Route Pattern**: `"transactions" :: TransactionId(transactionId) :: "balancing-transaction" :: Nil`
- **Tags**: Transaction
- **Path Parameters**: TRANSACTION_ID
- **Error Codes**: UnknownError
- **Source Files**: APIMethods400.scala

#### `GET /users`

- **Endpoint Name**: `getUsers`
- **Summary**: Get all Users
- **Description**: Get all users ${userAuthenticationMessage(true)} CanGetAnyUser entitlement is required, ${urlParametersDocument(false, false)} * locked_status (if null ignore) 
- **Route Pattern**: `case "users" :: Nil JsonGet`
- **Tags**: User
- **Required Roles**: GetAnyUser
- **Error Codes**: UnknownError
- **Source Files**: APIMethods400.scala

#### `GET /users/USER_ID/api-collections`

- **Endpoint Name**: `getApiCollectionsForUser`
- **Summary**: Get Api Collections for User
- **Description**: Get Api Collections for User. ${userAuthenticationMessage(true)} 
- **Route Pattern**: `"users" :: userId :: "api-collections" :: Nil`
- **Tags**: ApiCollection
- **Path Parameters**: USER_ID
- **Error Codes**: UserNotFound, UnknownError
- **Source Files**: APIMethods400.scala

#### `GET /users/USER_ID/attributes`

- **Endpoint Name**: `getUserWithAttributes`
- **Summary**: Get User with Attributes by USER_ID
- **Description**: Get User Attributes for the user defined via USER_ID. ${userAuthenticationMessage(true)} 
- **Route Pattern**: `"users" :: userId :: "attributes" :: Nil`
- **Tags**: User
- **Path Parameters**: USER_ID
- **Error Codes**: UnknownError
- **Source Files**: APIMethods400.scala

#### `GET /users/current/user_id`

- **Endpoint Name**: `getCurrentUserId`
- **Summary**: Get User Id (Current)
- **Description**: Get the USER_ID of the logged in user ${userAuthenticationMessage(true)}
- **Route Pattern**: `"users" :: "current" :: "user_id" :: Nil`
- **Tags**: User
- **Required Roles**: GetAnyUser
- **Path Parameters**: _
- **Error Codes**: UnknownError
- **Source Files**: APIMethods400.scala

#### `GET /users/email/EMAIL/terminator`

- **Endpoint Name**: `getUsersByEmail`
- **Summary**: Get Users by Email Address
- **Description**: Get users by email address ${userAuthenticationMessage(true)} CanGetAnyUser entitlement is required, 
- **Route Pattern**: `"users" :: "email" :: email :: "terminator" :: Nil`
- **Tags**: User
- **Required Roles**: GetAnyUser
- **Path Parameters**: EMAIL
- **Error Codes**: UserNotFound, UnknownError
- **Source Files**: APIMethods400.scala

#### `GET /users/user_id/USER_ID`

- **Endpoint Name**: `getUserByUserId`
- **Summary**: Get User by USER_ID
- **Description**: Get user by USER_ID ${userAuthenticationMessage(true)} CanGetAnyUser entitlement is required, 
- **Route Pattern**: `"users" :: "user_id" :: userId :: Nil`
- **Tags**: User
- **Required Roles**: GetAnyUser
- **Path Parameters**: USER_ID, _
- **Request Body**: createUserInfoJSON
- **Error Codes**: UserNotFound, UnknownError
- **Source Files**: APIMethods400.scala

#### `GET /users/username/USERNAME`

- **Endpoint Name**: `getUserByUsername`
- **Summary**: Get User by USERNAME
- **Description**: Get user by USERNAME ${userAuthenticationMessage(true)} CanGetAnyUser entitlement is required, 
- **Route Pattern**: `"users" :: "username" :: username :: Nil`
- **Tags**: User
- **Required Roles**: GetAnyUser
- **Path Parameters**: USERNAME
- **Request Body**: createUserInfoJSON
- **Error Codes**: UserNotFound, UnknownError
- **Source Files**: APIMethods400.scala

### POST Endpoints (63)

#### `POST /account/check/scheme/iban`

- **Endpoint Name**: `ibanChecker`
- **Summary**: Validate and check IBAN
- **Description**: Validate and check IBAN for errors 
- **Route Pattern**: `"account" :: "check" :: "scheme" :: "iban" :: Nil`
- **Source Files**: APIMethods400.scala

#### `POST /banks`

- **Endpoint Name**: `createBank`
- **Summary**: Create Bank
- **Description**: Create a new bank (Authenticated access). The user creating this will be automatically assigned the Role CanCreateEntitlementAtOneBank. Thus the User can manage the bank they create and assign Roles to other Users. Only SANDBOX mode The settlement accounts are created specified by the bank in the POST body. Name and account id are created in accordance to the next rules: - Incoming account (name: Default incoming settlement account, Account ID: OBP_DEFAULT_INCOMING_ACCOUNT_ID, currency: EUR) - O
- **Route Pattern**: `case "banks" :: Nil JsonPost`
- **Tags**: Bank
- **Required Roles**: CreateBank
- **Error Codes**: UnknownError
- **Source Files**: APIMethods400.scala

#### `POST /banks/BANK_ID/accounts`

- **Endpoint Name**: `addAccount`
- **Summary**: Create Account (POST)
- **Description**: Create Account at bank specified by BANK_ID. The User can create an Account for themself - or - the User that has the USER_ID specified in the POST body. If the POST body USER_ID *is* specified, the logged in user must have the Role CanCreateAccount. Once created, the Account will be owned by the User specified by USER_ID. If the POST body USER_ID is *not* specified, the account will be owned by the logged in User. The 'product_code' field SHOULD be a product_code from Product. If the product_co
- **Route Pattern**: `"banks" :: BankId(bankId) :: "accounts" :: Nil`
- **Tags**: Account
- **Required Roles**: CreateAccount
- **Path Parameters**: BANK_ID
- **Error Codes**: UnknownError
- **Source Files**: APIMethods400.scala

#### `POST /banks/BANK_ID/accounts/ACCOUNT_ID/VIEW_ID/counterparties`

- **Endpoint Name**: `createExplicitCounterparty`
- **Summary**: Create Counterparty (Explicit)
- **Description**: This endpoint creates an (Explicit) Counterparty for an Account. For an introduction to Counterparties in OBP see ${Glossary.getGlossaryItemLink("Counterparties")} ${userAuthenticationMessage(true)} 
- **Route Pattern**: `"banks" :: BankId(bankId) :: "accounts" :: AccountId(accountId) :: ViewId(viewId) :: "counterparties" :: Nil`
- **Tags**: Account, Counterparty
- **Path Parameters**: BANK_ID, ACCOUNT_ID, VIEW_ID
- **Request Body**: PostCounterpartyJSON
- **Error Codes**: BankNotFound, BankAccountNotFound, ViewNotFound, UnknownError
- **Source Files**: APIMethods400.scala

#### `POST /banks/BANK_ID/accounts/ACCOUNT_ID/VIEW_ID/counterparties/COUNTERPARTY_ID`

- **Endpoint Name**: `deleteExplicitCounterparty`
- **Summary**: Delete Counterparty (Explicit)
- **Description**: This endpoint deletes the Counterparty on the Account / View specified by the COUNTERPARTY_ID. It also deletes any related Counterparty Metadata. The User calling this endpoint must have access to the View specified in the URL and that View must have the permission `can_delete_counterparty`. For a general introduction to Counterparties in OBP see ${Glossary.getGlossaryItemLink("Counterparties")} ${userAuthenticationMessage(true)} 
- **Route Pattern**: `"banks" :: BankId(bankId) :: "accounts" :: AccountId(accountId) :: ViewId(viewId) :: "counterparties" :: CounterpartyId(counterpartyId) :: Nil`
- **Tags**: Account, Counterparty
- **Path Parameters**: BANK_ID, COUNTERPARTY_ID, ACCOUNT_ID, VIEW_ID
- **Error Codes**: BankNotFound, BankAccountNotFound, UnknownError
- **Source Files**: APIMethods400.scala

#### `POST /banks/BANK_ID/accounts/ACCOUNT_ID/VIEW_ID/direct-debit`

- **Endpoint Name**: `createDirectDebit`
- **Summary**: Create Direct Debit
- **Description**: Create direct debit for an account. 
- **Route Pattern**: `"banks" :: BankId(bankId) :: "accounts" :: AccountId(accountId) :: ViewId(viewId) :: "direct-debit" :: Nil`
- **Tags**: DirectDebit, Account
- **Path Parameters**: BANK_ID, ACCOUNT_ID, VIEW_ID
- **Error Codes**: BankNotFound, BankAccountNotFound, CustomerNotFound, UserNotFound, UnknownError, CounterpartyNotFound
- **Source Files**: APIMethods400.scala

#### `POST /banks/BANK_ID/accounts/ACCOUNT_ID/VIEW_ID/metadata/tags`

- **Endpoint Name**: `addTagForViewOnAccount`
- **Summary**: Create a tag on account
- **Description**: Posts a tag about an account ACCOUNT_ID on a [view](#1_2_1-getViewsForBankAccount) VIEW_ID. ${userAuthenticationMessage(true)} Authentication is required as the tag is linked with the user.
- **Route Pattern**: `"banks" :: BankId(bankId) :: "accounts" :: AccountId(accountId) :: ViewId(viewId) :: "metadata" :: "tags" :: Nil`
- **Tags**: Account, AccountMetadata
- **Path Parameters**: BANK_ID, ACCOUNT_ID, VIEW_ID
- **Request Body**: postAccountTagJSON
- **Error Codes**: BankNotFound, BankAccountNotFound, UnknownError
- **Source Files**: APIMethods400.scala

#### `POST /banks/BANK_ID/accounts/ACCOUNT_ID/VIEW_ID/standing-order`

- **Endpoint Name**: `createStandingOrder`
- **Summary**: Create Standing Order
- **Description**: Create standing order for an account. when -> frequency = {‘YEARLY’,’MONTHLY, ‘WEEKLY’, ‘BI-WEEKLY’, DAILY’} when -> detail = { ‘FIRST_MONDAY’, ‘FIRST_DAY’, ‘LAST_DAY’}} 
- **Route Pattern**: `"banks" :: BankId(bankId) :: "accounts" :: AccountId(accountId) :: ViewId(viewId) :: "standing-order" :: Nil`
- **Tags**: StandingOrder, Account
- **Path Parameters**: BANK_ID, ACCOUNT_ID, VIEW_ID
- **Error Codes**: BankNotFound, BankAccountNotFound, CustomerNotFound, UserNotFound, UnknownError
- **Source Files**: APIMethods400.scala

#### `POST /banks/BANK_ID/accounts/ACCOUNT_ID/VIEW_ID/transaction-request-types/ACCOUNT/transaction-requests`

- **Endpoint Name**: `createTransactionRequestAccount`
- **Summary**: Create Transaction Request (ACCOUNT)
- **Description**: When using ACCOUNT, the payee is set in the request body. Money goes into the BANK_ID and ACCOUNT_ID specified in the request body. $transactionRequestGeneralText 
- **Tags**: Psd2, PSD2PIS, TransactionRequest
- **Path Parameters**: BANK_ID, ACCOUNT_ID, ACCOUNT, VIEW_ID
- **Request Body**: transactionRequestWithChargeJSON400
- **Response Body**: transactionRequestWithChargeJSON400
- **Error Codes**: AccountNotFound, BankNotFound, BankAccountNotFound, UnknownError
- **Source Files**: APIMethods400.scala

#### `POST /banks/BANK_ID/accounts/ACCOUNT_ID/VIEW_ID/transaction-request-types/ACCOUNT_OTP/transaction-requests`

- **Endpoint Name**: `createTransactionRequestAccountOtp`
- **Summary**: Create Transaction Request (ACCOUNT_OTP)
- **Description**: When using ACCOUNT, the payee is set in the request body. Money goes into the BANK_ID and ACCOUNT_ID specified in the request body. $transactionRequestGeneralText 
- **Tags**: Psd2, PSD2PIS, TransactionRequest
- **Path Parameters**: BANK_ID, ACCOUNT_ID, VIEW_ID, ACCOUNT_OTP
- **Request Body**: transactionRequestWithChargeJSON400
- **Response Body**: transactionRequestWithChargeJSON400
- **Error Codes**: AccountNotFound, BankNotFound, BankAccountNotFound, UnknownError
- **Source Files**: APIMethods400.scala

#### `POST /banks/BANK_ID/accounts/ACCOUNT_ID/VIEW_ID/transaction-request-types/AGENT_CASH_WITHDRAWAL/transaction-requests`

- **Endpoint Name**: `createTransactionRequestAgentCashWithDrawal`
- **Summary**: Create Transaction Request (AGENT_CASH_WITHDRAWAL)
- **Description**:  Either the `from` or the `to` field must be filled. Those fields refers to the information about the party that will be refunded. In case the `from` object is used, it means that the refund comes from the part that sent you a transaction. In the `from` object, you have two choices : - Use `bank_id` and `account_id` fields if the other account is registered on the OBP-API - Use the `counterparty_id` field in case the counterparty account is out of the OBP-API In case the `to` object is used, it 
- **Path Parameters**: BANK_ID, ACCOUNT_ID, VIEW_ID, AGENT_CASH_WITHDRAWAL
- **Request Body**: transactionRequestWithChargeJSON400
- **Response Body**: transactionRequestWithChargeJSON400
- **Error Codes**: AccountNotFound, BankNotFound, BankAccountNotFound, UnknownError
- **Source Files**: APIMethods400.scala

#### `POST /banks/BANK_ID/accounts/ACCOUNT_ID/VIEW_ID/transaction-request-types/REFUND/transaction-requests`

- **Endpoint Name**: `createTransactionRequestRefund`
- **Summary**: Create Transaction Request (REFUND)
- **Description**:  Either the `from` or the `to` field must be filled. Those fields refers to the information about the party that will be refunded. In case the `from` object is used, it means that the refund comes from the part that sent you a transaction. In the `from` object, you have two choices : - Use `bank_id` and `account_id` fields if the other account is registered on the OBP-API - Use the `counterparty_id` field in case the counterparty account is out of the OBP-API In case the `to` object is used, it 
- **Path Parameters**: BANK_ID, ACCOUNT_ID, VIEW_ID, REFUND
- **Request Body**: transactionRequestWithChargeJSON400
- **Response Body**: transactionRequestWithChargeJSON400
- **Error Codes**: AccountNotFound, BankNotFound, BankAccountNotFound, UnknownError
- **Source Files**: APIMethods400.scala

#### `POST /banks/BANK_ID/accounts/ACCOUNT_ID/VIEW_ID/transaction-request-types/SIMPLE/transaction-requests`

- **Endpoint Name**: `createTransactionRequestSimple`
- **Summary**: Create Transaction Request (SIMPLE)
- **Description**:  Special instructions for SIMPLE: You can transfer money to the Bank Account Number or IBAN directly. $transactionRequestGeneralText 
- **Tags**: Psd2, PSD2PIS, TransactionRequest
- **Path Parameters**: BANK_ID, ACCOUNT_ID, VIEW_ID, SIMPLE
- **Request Body**: transactionRequestWithChargeJSON400
- **Response Body**: transactionRequestWithChargeJSON400
- **Error Codes**: AccountNotFound, BankNotFound, BankAccountNotFound, UnknownError
- **Source Files**: APIMethods400.scala

#### `POST /banks/BANK_ID/accounts/ACCOUNT_ID/VIEW_ID/transaction-request-types/TRANSACTION_REQUEST_TYPE/transaction-requests/TRANSACTION_REQUEST_ID/challenge`

- **Endpoint Name**: `answerTransactionRequestChallenge`
- **Summary**: Answer Transaction Request Challenge
- **Description**: In Sandbox mode, any string that can be converted to a positive integer will be accepted as an answer. This endpoint totally depends on createTransactionRequest, it need get the following data from createTransactionRequest response body. 1)`TRANSACTION_REQUEST_TYPE` : is the same as createTransactionRequest request URL . 2)`TRANSACTION_REQUEST_ID` : is the `id` field in createTransactionRequest response body. 3) `id` : is `challenge.id` field in createTransactionRequest response body. 4) `answer
- **Path Parameters**: BANK_ID, TRANSACTION_REQUEST_ID, TRANSACTION_REQUEST_TYPE, VIEW_ID, ACCOUNT_ID
- **Source Files**: APIMethods400.scala

#### `POST /banks/BANK_ID/accounts/ACCOUNT_ID/account-access/revoke`

- **Endpoint Name**: `revokeUserAccessToView`
- **Summary**: Revoke User access to View
- **Description**: Revoke the User identified by USER_ID access to the view identified by VIEW_ID. ${userAuthenticationMessage(true)} and the user needs to be account holder. 
- **Route Pattern**: `"banks" :: BankId(bankId) :: "accounts" :: AccountId(accountId) :: "account-access" :: "revoke" :: Nil`
- **Tags**: Account, User, AccountAccess, View, OwnerRequired
- **Path Parameters**: BANK_ID, ACCOUNT_ID
- **Error Codes**: UserNotFound, SystemViewNotFound, ViewNotFound, UnknownError
- **Source Files**: APIMethods400.scala

#### `POST /banks/BANK_ID/accounts/ACCOUNT_ID/transaction-requests/TRANSACTION_REQUEST_ID/attribute`

- **Endpoint Name**: `createTransactionRequestAttribute`
- **Summary**: Create Transaction Request Attribute
- **Description**: Create Transaction Request Attribute The type field must be one of "STRING", "INTEGER", "DOUBLE" or DATE_WITH_DAY" ${userAuthenticationMessage(true)} 
- **Route Pattern**: `"banks" :: BankId(bankId) :: "accounts" :: AccountId(accountId) :: "transaction-requests" :: TransactionRequestId(transactionRequestId) :: "attribute" :: Nil`
- **Tags**: TransactionRequest
- **Required Roles**: CreateTransactionRequestAttributeAtOneBank
- **Path Parameters**: BANK_ID, ACCOUNT_ID, TRANSACTION_REQUEST_ID
- **Error Codes**: BankNotFound, BankAccountNotFound, UnknownError
- **Source Files**: APIMethods400.scala

#### `POST /banks/BANK_ID/accounts/ACCOUNT_ID/transactions/TRANSACTION_ID/attribute`

- **Endpoint Name**: `createTransactionAttribute`
- **Summary**: Create Transaction Attribute
- **Description**: Create Transaction Attribute The type field must be one of "STRING", "INTEGER", "DOUBLE" or DATE_WITH_DAY" ${userAuthenticationMessage(true)} 
- **Route Pattern**: `"banks" :: BankId(bankId) :: "accounts" :: AccountId(accountId) :: "transactions" :: TransactionId(transactionId) :: "attribute" :: Nil`
- **Tags**: Transaction
- **Required Roles**: CreateTransactionAttributeAtOneBank
- **Path Parameters**: BANK_ID, ACCOUNT_ID, TRANSACTION_ID
- **Error Codes**: BankNotFound, BankAccountNotFound, UnknownError
- **Source Files**: APIMethods400.scala

#### `POST /banks/BANK_ID/accounts/ACCOUNT_ID/user-account-access`

- **Endpoint Name**: `createUserWithAccountAccess`
- **Summary**: Create (DAuth) User with Account Access
- **Description**: This endpoint is used as part of the DAuth solution to grant access to account and transaction data to a smart contract on the blockchain. Put the smart contract address in username For provider use "dauth" This endpoint will create the (DAuth) User with username and provider if the User does not already exist. ${userAuthenticationMessage(true)} and the logged in user needs to be account holder. For information about DAuth see below: ${getGlossaryItem("DAuth")} 
- **Route Pattern**: `"banks" :: BankId(bankId) :: "accounts" :: AccountId(accountId) :: "user-account-access" :: Nil`
- **Tags**: Account, User, AccountAccess, DAuth, View, OwnerRequired
- **Path Parameters**: BANK_ID, ACCOUNT_ID
- **Source Files**: APIMethods400.scala

#### `POST /banks/BANK_ID/atms`

- **Endpoint Name**: `createAtm`
- **Summary**: Create ATM
- **Description**: Create ATM.
- **Route Pattern**: `"banks" :: BankId(bankId) :: "atms" :: Nil`
- **Tags**: ATM
- **Required Roles**: CreateAtm, CreateAtmAtAnyBank
- **Path Parameters**: BANK_ID
- **Error Codes**: UnknownError
- **Source Files**: APIMethods400.scala

#### `POST /banks/BANK_ID/attribute`

- **Endpoint Name**: `createBankAttribute`
- **Summary**: Create Bank Attribute
- **Description**: Create Bank Attribute Typical product attributes might be: ISIN (for International bonds) VKN (for German bonds) REDCODE (markit short code for credit derivative) LOAN_ID (e.g. used for Anacredit reporting) ISSUE_DATE (When the bond was issued in the market) MATURITY_DATE (End of life time of a product) TRADABLE See [FPML](http://www.fpml.org/) for more examples. The type field must be one of "STRING", "INTEGER", "DOUBLE" or DATE_WITH_DAY" ${userAuthenticationMessage(true)} 
- **Route Pattern**: `"banks" :: bankId :: "attribute" :: Nil`
- **Tags**: Bank
- **Required Roles**: CreateBankAttribute
- **Path Parameters**: BANK_ID
- **Error Codes**: UnknownError
- **Source Files**: APIMethods400.scala

#### `POST /banks/BANK_ID/customers`

- **Endpoint Name**: `createCustomer`
- **Summary**: Create Customer
- **Description**:  The Customer resource stores the customer number (which is set by the backend), legal name, email, phone number, their date of birth, relationship status, education attained, a url for a profile image, KYC status etc. Dates need to be in the format 2013-01-21T23:08:00Z Note: If you need to set a specific customer number, use the Update Customer Number endpoint after this call. ${userAuthenticationMessage(true)} 
- **Route Pattern**: `"banks" :: BankId(bankId) :: "customers" :: Nil`
- **Tags**: Person, Customer
- **Required Roles**: CreateCustomer, CreateCustomerAtAnyBank
- **Path Parameters**: BANK_ID
- **Error Codes**: UserNotFound, BankNotFound, CreateConsumerError, UnknownError
- **Source Files**: APIMethods400.scala

#### `POST /banks/BANK_ID/customers/CUSTOMER_ID/attribute`

- **Endpoint Name**: `createCustomerAttribute`
- **Summary**: Create Customer Attribute
- **Description**: Create Customer Attribute The type field must be one of "STRING", "INTEGER", "DOUBLE" or DATE_WITH_DAY" ${userAuthenticationMessage(true)} 
- **Route Pattern**: `"banks" :: bankId :: "customers" :: customerId :: "attribute" :: Nil`
- **Tags**: Customer
- **Required Roles**: CreateCustomerAttributeAtAnyBank, CreateCustomerAttributeAtOneBank
- **Path Parameters**: BANK_ID, CUSTOMER_ID
- **Error Codes**: BankNotFound, UnknownError
- **Source Files**: APIMethods400.scala

#### `POST /banks/BANK_ID/customers/CUSTOMER_ID/messages`

- **Endpoint Name**: `createCustomerMessage`
- **Summary**: Create Customer Message
- **Description**:  Create a message for the customer specified by CUSTOMER_ID ${userAuthenticationMessage(true)} 
- **Route Pattern**: `"banks" :: BankId(bankId) :: "customers" :: customerId :: "messages" :: Nil`
- **Tags**: Person, Message, Customer
- **Required Roles**: CreateCustomerMessage
- **Path Parameters**: BANK_ID, CUSTOMER_ID
- **Error Codes**: BankNotFound
- **Source Files**: APIMethods400.scala

#### `POST /banks/BANK_ID/management/historical/transactions`

- **Endpoint Name**: `createHistoricalTransactionAtBank`
- **Summary**: Create Historical Transactions 
- **Description**:  Create historical transactions at one Bank Use this endpoint to create transactions between any two accounts at the same bank. From account and to account must be at the same bank. Example: { "from_account_id": "1ca8a7e4-6d02-48e3-a029-0b2bf89de9f0", "to_account_id": "2ca8a7e4-6d02-48e3-a029-0b2bf89de9f0", "value": { "currency": "GBP", "amount": "10" }, "description": "this is for work", "posted": "2017-09-19T02:31:05Z", "completed": "2017-09-19T02:31:05Z", "type": "SANDBOX_TAN", "charge_policy
- **Route Pattern**: `"banks" :: BankId(bankId) :: "management"  :: "historical" :: "transactions" :: Nil`
- **Tags**: TransactionRequest
- **Required Roles**: CreateHistoricalTransactionAtBank
- **Path Parameters**: BANK_ID
- **Error Codes**: AccountNotFound, BankNotFound, CounterpartyNotFound, UnknownError
- **Source Files**: APIMethods400.scala

#### `POST /banks/BANK_ID/products/PRODUCT_CODE/attribute`

- **Endpoint Name**: `createProductAttribute`
- **Summary**: Create Product Attribute
- **Description**: Create Product Attribute $productAttributeGeneralInfo Typical product attributes might be: ISIN (for International bonds) VKN (for German bonds) REDCODE (markit short code for credit derivative) LOAN_ID (e.g. used for Anacredit reporting) ISSUE_DATE (When the bond was issued in the market) MATURITY_DATE (End of life time of a product) TRADABLE See [FPML](http://www.fpml.org/) for more examples. The type field must be one of "STRING", "INTEGER", "DOUBLE" or DATE_WITH_DAY" ${userAuthenticationMess
- **Route Pattern**: `"banks" :: bankId :: "products" :: productCode:: "attribute" :: Nil`
- **Tags**: Product
- **Required Roles**: CreateProductAttribute
- **Path Parameters**: BANK_ID, PRODUCT_CODE
- **Error Codes**: UnknownError
- **Source Files**: APIMethods400.scala

#### `POST /banks/BANK_ID/products/PRODUCT_CODE/fee`

- **Endpoint Name**: `createProductFee`
- **Summary**: Create Product Fee
- **Description**: Create Product Fee ${userAuthenticationMessage(true)} 
- **Route Pattern**: `"banks" :: bankId :: "products" :: productCode:: "fee" :: Nil`
- **Tags**: Product
- **Required Roles**: CreateProductFee
- **Path Parameters**: BANK_ID, PRODUCT_CODE
- **Error Codes**: BankNotFound, UnknownError
- **Source Files**: APIMethods400.scala

#### `POST /banks/BANK_ID/search/customers/mobile-phone-number`

- **Endpoint Name**: `getCustomersByCustomerPhoneNumber`
- **Summary**: Get Customers by MOBILE_PHONE_NUMBER
- **Description**: Gets the Customers specified by MOBILE_PHONE_NUMBER. There are two wildcards often used in conjunction with the LIKE operator: % - The percent sign represents zero, one, or multiple characters _ - The underscore represents a single character For example {"customer_phone_number":"%381%"} lists all numbers which contain 381 sequence 
- **Route Pattern**: `"banks" :: BankId(bankId) :: "search"  :: "customers" :: "mobile-phone-number" :: Nil`
- **Tags**: Customer, Kyc
- **Required Roles**: GetCustomer
- **Path Parameters**: BANK_ID
- **Error Codes**: UserCustomerLinksNotFound, UnknownError
- **Source Files**: APIMethods400.scala

#### `POST /banks/BANK_ID/settlement-accounts`

- **Endpoint Name**: `createSettlementAccount`
- **Summary**: Create Settlement Account
- **Description**: Create a new settlement account at a bank. The created settlement account id will be the concatenation of the payment system and the account currency. For examples: SEPA_SETTLEMENT_ACCOUNT_EUR, CARD_SETTLEMENT_ACCOUNT_USD By default, when you create a new bank, two settlements accounts are created automatically: OBP_DEFAULT_INCOMING_ACCOUNT_ID and OBP_DEFAULT_OUTGOING_ACCOUNT_ID Those two accounts have EUR as default currency. If you want to create default settlement account for a specific curre
- **Route Pattern**: `"banks" :: BankId(bankId) :: "settlement-accounts" :: Nil`
- **Path Parameters**: BANK_ID
- **Source Files**: APIMethods400.scala

#### `POST /banks/BANK_ID/user-invitation`

- **Endpoint Name**: `createUserInvitation`
- **Summary**: Create User Invitation
- **Description**: Create User Invitation. This endpoint will send an invitation email to the developers, then they can use the link to create the obp user. purpose filed only support:${UserInvitationPurpose.values.toString()}. You can customise the email details use the following webui props: when purpose == ${UserInvitationPurpose.DEVELOPER.toString} webui_developer_user_invitation_email_subject webui_developer_user_invitation_email_from webui_developer_user_invitation_email_text webui_developer_user_invitation_
- **Route Pattern**: `"banks" :: BankId(bankId) :: "user-invitation" :: Nil`
- **Tags**: Kyc, UserInvitation
- **Path Parameters**: BANK_ID
- **Error Codes**: BankNotFound, UserCustomerLinksNotFound, UnknownError
- **Source Files**: APIMethods400.scala

#### `POST /banks/BANK_ID/user-invitations`

- **Endpoint Name**: `getUserInvitationAnonymous`
- **Summary**: Get User Invitation Information
- **Description**: Get User Invitation Information. ${userAuthenticationMessage(false)} 
- **Route Pattern**: `"banks" :: BankId(bankId) :: "user-invitations" :: Nil`
- **Tags**: Kyc, UserInvitation
- **Path Parameters**: BANK_ID
- **Error Codes**: BankNotFound, UserCustomerLinksNotFound, UnknownError
- **Source Files**: APIMethods400.scala

#### `POST /banks/BANK_ID/user_customer_links`

- **Endpoint Name**: `createUserCustomerLinks`
- **Summary**: Create User Customer Link
- **Description**: Link a User to a Customer ${userAuthenticationMessage(true)} 
- **Route Pattern**: `"banks" :: BankId(bankId):: "user_customer_links" :: Nil`
- **Tags**: Customer, User
- **Required Roles**: CreateUserCustomerLinkAtAnyBank, CreateUserCustomerLink
- **Path Parameters**: BANK_ID, _
- **Error Codes**: CustomerNotFound, BankNotFound, CreateUserCustomerLinksError, UnknownError
- **Source Files**: APIMethods400.scala

#### `POST /banks/BANK_ID/web-hooks/account/notifications/on-create-transaction`

- **Endpoint Name**: `createBankAccountNotificationWebhook`
- **Summary**: Create bank level Account Notification Webhook
- **Description**: Create a notification Webhook that will fire for all accounts on the specified Bank. $generalWebHookInfo $accountNotificationWebhookInfo 
- **Route Pattern**: `"banks" :: BankId(bankId) :: "web-hooks" ::"account" ::"notifications" ::"on-create-transaction" :: Nil`
- **Required Roles**: CreateAccountNotificationWebhookAtOneBank
- **Path Parameters**: BANK_ID
- **Source Files**: APIMethods400.scala

#### `POST /consumers/CONSUMER_ID/scopes`

- **Endpoint Name**: `addScope`
- **Summary**: Create Scope for a Consumer
- **Description**: Create Scope. Grant Role to Consumer. Scopes are used to grant System or Bank level roles to the Consumer (App). (For Account level privileges, see Views) For a System level Role (.e.g CanGetAnyUser), set bank_id to an empty string i.e. "bank_id":"" For a Bank level Role (e.g. CanCreateAccount), set bank_id to a valid value e.g. "bank_id":"my-bank-id" 
- **Route Pattern**: `"consumers" :: consumerId :: "scopes" :: Nil`
- **Tags**: Scope, Consumer
- **Required Roles**: CreateScopeAtOneBank, CreateScopeAtAnyBank
- **Path Parameters**: CONSUMER_ID
- **Request Body**: SwaggerDefinitionsJSON
- **Error Codes**: ConsumerNotFound, UnknownError
- **Source Files**: APIMethods400.scala

#### `POST /management/accounts/account-routing-query`

- **Endpoint Name**: `getAccountByAccountRouting`
- **Summary**: Get Account by Account Routing
- **Description**: This endpoint returns the account (if it exists) linked with the provided scheme and address. The `bank_id` field is optional, but if it's not provided, we don't guarantee that the returned account is unique across all the banks. Example of account routing scheme: `IBAN`, "OBP", "AccountNumber", ... Example of account routing address: `DE17500105178275645584`, "321774cc-fccd-11ea-adc1-0242ac120002", "55897106215", ... 
- **Route Pattern**: `"management" :: "accounts" :: "account-routing-query" :: Nil`
- **Request Body**: moderatedAccountJSON400
- **Response Body**: moderatedAccountJSON400
- **Error Codes**: BankNotFound, BankAccountNotFound, UnknownError
- **Source Files**: APIMethods400.scala

#### `POST /management/accounts/account-routing-regex-query`

- **Endpoint Name**: `getAccountsByAccountRoutingRegex`
- **Summary**: Get Accounts by Account Routing Regex
- **Description**: This endpoint returns an array of accounts matching the provided routing scheme and the routing address regex. The `bank_id` field is optional. Example of account routing scheme: `IBAN`, `OBP`, `AccountNumber`, ... Example of account routing address regex: `DE175.*`, `55897106215-[A-Z]{3}`, ... This endpoint can be used to retrieve multiples accounts matching a same account routing address pattern. For example, if you want to link multiple accounts having different currencies, you can create an 
- **Route Pattern**: `"management" :: "accounts" :: "account-routing-regex-query" :: Nil`
- **Request Body**: moderatedAccountsJSON400
- **Response Body**: moderatedAccountsJSON400
- **Error Codes**: BankNotFound, BankAccountNotFound, UnknownError
- **Source Files**: APIMethods400.scala

#### `POST /management/authentication-type-validations/OPERATION_ID`

- **Endpoint Name**: `createAuthenticationTypeValidation`
- **Summary**: Create an Authentication Type Validation
- **Description**: Create an Authentication Type Validation. Please supply allowed authentication types. 
- **Route Pattern**: `"management" :: "authentication-type-validations" :: operationId :: Nil`
- **Tags**: AuthenticationTypeValidation
- **Required Roles**: CreateAuthenticationTypeValidation
- **Path Parameters**: OPERATION_ID
- **Error Codes**: UnknownError
- **Source Files**: APIMethods400.scala

#### `POST /management/banks/BANK_ID/accounts/ACCOUNT_ID/VIEW_ID/counterparties`

- **Endpoint Name**: `createCounterpartyForAnyAccount`
- **Summary**: Create Counterparty for any account (Explicit)
- **Description**: This is a management endpoint that allows the creation of a Counterparty on any Account. For an introduction to Counterparties in OBP, see ${Glossary.getGlossaryItemLink("Counterparties")} ${userAuthenticationMessage(true)} 
- **Route Pattern**: `"management" :: "banks" :: BankId(bankId) :: "accounts" :: AccountId(accountId) :: ViewId(viewId):: "counterparties" :: Nil`
- **Tags**: Account, Counterparty
- **Required Roles**: CreateCounterpartyAtAnyBank, CreateCounterparty
- **Path Parameters**: BANK_ID, ACCOUNT_ID, VIEW_ID
- **Error Codes**: AccountNotFound, BankNotFound, BankAccountNotFound, ViewNotFound, UnknownError
- **Source Files**: APIMethods400.scala

#### `POST /management/banks/BANK_ID/accounts/ACCOUNT_ID/direct-debit`

- **Endpoint Name**: `createDirectDebitManagement`
- **Summary**: Create Direct Debit (management)
- **Description**: Create direct debit for an account. 
- **Route Pattern**: `"management" :: "banks" :: BankId(bankId) :: "accounts" :: AccountId(accountId) :: "direct-debit" :: Nil`
- **Tags**: DirectDebit, Account
- **Required Roles**: CreateDirectDebitAtOneBank
- **Path Parameters**: BANK_ID, ACCOUNT_ID
- **Error Codes**: BankNotFound, BankAccountNotFound, CustomerNotFound, UserNotFound, UnknownError, CounterpartyNotFound
- **Source Files**: APIMethods400.scala

#### `POST /management/banks/BANK_ID/accounts/ACCOUNT_ID/standing-order`

- **Endpoint Name**: `createStandingOrderManagement`
- **Summary**: Create Standing Order (management)
- **Description**: Create standing order for an account. when -> frequency = {‘YEARLY’,’MONTHLY, ‘WEEKLY’, ‘BI-WEEKLY’, DAILY’} when -> detail = { ‘FIRST_MONDAY’, ‘FIRST_DAY’, ‘LAST_DAY’}} 
- **Route Pattern**: `"management" :: "banks" :: BankId(bankId) :: "accounts" :: AccountId(accountId) :: "standing-order" :: Nil`
- **Tags**: StandingOrder, Account
- **Required Roles**: CreateStandingOrderAtOneBank
- **Path Parameters**: BANK_ID, ACCOUNT_ID
- **Error Codes**: BankNotFound, BankAccountNotFound, CustomerNotFound, UserNotFound, UnknownError
- **Source Files**: APIMethods400.scala

#### `POST /management/banks/BANK_ID/dynamic-endpoints`

- **Endpoint Name**: `createBankLevelDynamicEndpoint`
- **Summary**: Create Bank Level Dynamic Endpoint
- **Description**: Create dynamic endpoints. Create dynamic endpoints with one json format swagger content. If the host of swagger is `dynamic_entity`, then you need link the swagger fields to the dynamic entity fields, please check `Endpoint Mapping` endpoints. If the host of swagger is `obp_mock`, every dynamic endpoint will return example response of swagger,\n when create MethodRouting for given dynamic endpoint, it will be routed to given url. 
- **Route Pattern**: `"management" :: "banks" :: BankId(bankId) ::"dynamic-endpoints" :: Nil`
- **Tags**: Api, ManageDynamicEndpoint
- **Required Roles**: CreateBankLevelDynamicEndpoint, CreateDynamicEndpoint
- **Path Parameters**: BANK_ID
- **Error Codes**: BankNotFound, UnknownError
- **Source Files**: APIMethods400.scala

#### `POST /management/banks/BANK_ID/dynamic-entities`

- **Endpoint Name**: `createBankLevelDynamicEntity`
- **Summary**: Create Bank Level Dynamic Entity
- **Description**: Create a Bank Level DynamicEntity. ${userAuthenticationMessage(true)} Create a DynamicEntity. If creation is successful, the corresponding POST, GET, PUT and DELETE (Create, Read, Update, Delete or CRUD for short) endpoints will be generated automatically The following field types are as supported: ${DynamicEntityFieldType.values.map(_.toString).mkString("[", ", ", ", reference]")} The ${DynamicEntityFieldType.DATE_WITH_DAY} format is: ${DynamicEntityFieldType.DATE_WITH_DAY.dateFormat} Reference
- **Path Parameters**: BANK_ID
- **Error Codes**: BankNotFound, UnknownError
- **Source File**: APIMethods400.scala

#### `POST /management/banks/BANK_ID/dynamic-message-docs`

- **Endpoint Name**: `createBankLevelDynamicMessageDoc`
- **Summary**: Create Bank Level Dynamic Message Doc
- **Description**: Create a Bank Level Dynamic Message Doc. 
- **Route Pattern**: `"management" :: "banks" :: bankId ::"dynamic-message-docs" :: Nil`
- **Tags**: DynamicMessageDoc
- **Required Roles**: CreateBankLevelDynamicMessageDoc
- **Path Parameters**: BANK_ID
- **Error Codes**: BankNotFound, UnknownError
- **Source Files**: APIMethods400.scala

#### `POST /management/banks/BANK_ID/dynamic-resource-docs`

- **Endpoint Name**: `createBankLevelDynamicResourceDoc`
- **Summary**: Create Bank Level Dynamic Resource Doc
- **Description**: Create a Bank Level Dynamic Resource Doc. The connector_method_body is URL-encoded format String 
- **Route Pattern**: `"management" :: "banks" :: bankId :: "dynamic-resource-docs" :: Nil`
- **Tags**: DynamicResourceDoc
- **Required Roles**: CreateBankLevelDynamicResourceDoc
- **Path Parameters**: BANK_ID
- **Error Codes**: BankNotFound, UnknownError
- **Source Files**: APIMethods400.scala

#### `POST /management/banks/BANK_ID/endpoint-mappings`

- **Endpoint Name**: `createBankLevelEndpointMapping`
- **Summary**: Create Bank Level Endpoint Mapping
- **Description**: Create an Bank Level Endpoint Mapping. Note: at moment only support the dynamic endpoints 
- **Route Pattern**: `"management" :: "banks" :: bankId :: "endpoint-mappings" :: Nil`
- **Tags**: EndpointMapping
- **Required Roles**: CreateBankLevelEndpointMapping, CreateEndpointMapping
- **Path Parameters**: BANK_ID
- **Error Codes**: BankNotFound, UnknownError
- **Source Files**: APIMethods400.scala

#### `POST /management/banks/BANK_ID/endpoints/OPERATION_ID/tags`

- **Endpoint Name**: `createBankLevelEndpointTag`
- **Summary**: Create Bank Level Endpoint Tag
- **Description**: Create Bank Level Endpoint Tag Note: Resource Docs are cached, TTL is ${CREATE_LOCALISED_RESOURCE_DOC_JSON_TTL} seconds 
- **Route Pattern**: `"management" :: "banks" :: bankId :: "endpoints" :: operationId :: "tags" :: Nil`
- **Tags**: Api
- **Required Roles**: CreateBankLevelEndpointTag
- **Path Parameters**: BANK_ID, OPERATION_ID
- **Request Body**: CREATE_LOCALISED_RESOURCE_DOC_JSON_TTL
- **Error Codes**: BankNotFound, UnknownError
- **Source Files**: APIMethods400.scala

#### `POST /management/connector-methods`

- **Endpoint Name**: `createConnectorMethod`
- **Summary**: Create Connector Method
- **Description**: Create an internal connector. The method_body is URL-encoded format String 
- **Route Pattern**: `"management" :: "connector-methods" :: Nil`
- **Tags**: ConnectorMethod
- **Required Roles**: CreateConnectorMethod
- **Error Codes**: UnknownError
- **Source Files**: APIMethods400.scala

#### `POST /management/dynamic-endpoints`

- **Endpoint Name**: `createDynamicEndpoint`
- **Summary**: Create Dynamic Endpoint
- **Description**: Create dynamic endpoints. Create dynamic endpoints with one json format swagger content. If the host of swagger is `dynamic_entity`, then you need link the swagger fields to the dynamic entity fields, please check `Endpoint Mapping` endpoints. If the host of swagger is `obp_mock`, every dynamic endpoint will return example response of swagger,\n when create MethodRouting for given dynamic endpoint, it will be routed to given url. 
- **Route Pattern**: `"management" :: "dynamic-endpoints" :: Nil`
- **Tags**: Api, ManageDynamicEndpoint
- **Required Roles**: CreateDynamicEndpoint
- **Error Codes**: UnknownError
- **Source Files**: APIMethods400.scala

#### `POST /management/dynamic-message-docs`

- **Endpoint Name**: `createDynamicMessageDoc`
- **Summary**: Create Dynamic Message Doc
- **Description**: Create a Dynamic Message Doc. 
- **Route Pattern**: `"management" :: "dynamic-message-docs" :: Nil`
- **Tags**: DynamicMessageDoc
- **Required Roles**: CreateDynamicMessageDoc
- **Error Codes**: UnknownError
- **Source Files**: APIMethods400.scala

#### `POST /management/dynamic-resource-docs`

- **Endpoint Name**: `createDynamicResourceDoc`
- **Summary**: Create Dynamic Resource Doc
- **Description**: Create a Dynamic Resource Doc. The connector_method_body is URL-encoded format String 
- **Route Pattern**: `"management" :: "dynamic-resource-docs" :: Nil`
- **Tags**: DynamicResourceDoc
- **Required Roles**: CreateDynamicResourceDoc
- **Error Codes**: UnknownError
- **Source Files**: APIMethods400.scala

#### `POST /management/dynamic-resource-docs/endpoint-code`

- **Endpoint Name**: `buildDynamicEndpointTemplate`
- **Summary**: Create Dynamic Resource Doc endpoint code
- **Description**: Create a Dynamic Resource Doc endpoint code. copy the response and past to ${nameOf(PractiseEndpoint)}, So you can have the benefits of auto compilation and debug 
- **Route Pattern**: `"management" :: "dynamic-resource-docs" :: "endpoint-code" :: Nil`
- **Tags**: DynamicResourceDoc
- **Error Codes**: UnknownError
- **Source Files**: APIMethods400.scala

#### `POST /management/endpoint-mappings`

- **Endpoint Name**: `createEndpointMapping`
- **Summary**: Create Endpoint Mapping
- **Description**: Create an Endpoint Mapping. Note: at moment only support the dynamic endpoints 
- **Route Pattern**: `"management" :: "endpoint-mappings" :: Nil`
- **Tags**: EndpointMapping
- **Required Roles**: CreateEndpointMapping
- **Error Codes**: UnknownError
- **Source Files**: APIMethods400.scala

#### `POST /management/endpoints/OPERATION_ID/tags`

- **Endpoint Name**: `createSystemLevelEndpointTag`
- **Summary**: Create System Level Endpoint Tag
- **Description**: Create System Level Endpoint Tag Note: Resource Docs are cached, TTL is ${CREATE_LOCALISED_RESOURCE_DOC_JSON_TTL} seconds 
- **Route Pattern**: `"management" :: "endpoints" :: operationId :: "tags" :: Nil`
- **Tags**: Api
- **Required Roles**: CreateSystemLevelEndpointTag
- **Path Parameters**: OPERATION_ID
- **Request Body**: CREATE_LOCALISED_RESOURCE_DOC_JSON_TTL
- **Error Codes**: UnknownError
- **Source Files**: APIMethods400.scala

#### `POST /management/json-schema-validations/OPERATION_ID`

- **Endpoint Name**: `createJsonSchemaValidation`
- **Summary**: Create a JSON Schema Validation
- **Description**: Create a JSON Schema Validation. Introduction: ${Glossary.getGlossaryItemSimple("JSON Schema Validation")} To use this endpoint, please supply a valid json-schema in the request body. Note: It might take a few minutes for the newly created JSON Schema to take effect! 
- **Route Pattern**: `"management" :: "json-schema-validations" :: operationId :: Nil`
- **Tags**: JsonSchemaValidation
- **Required Roles**: CreateJsonSchemaValidation
- **Path Parameters**: OPERATION_ID
- **Error Codes**: UnknownError
- **Source Files**: APIMethods400.scala

#### `POST /management/system-dynamic-entities`

- **Endpoint Name**: `createSystemDynamicEntity`
- **Summary**: Create System Level Dynamic Entity
- **Description**: Create a system level Dynamic Entity. ${userAuthenticationMessage(true)} Create a DynamicEntity. If creation is successful, the corresponding POST, GET, PUT and DELETE (Create, Read, Update, Delete or CRUD for short) endpoints will be generated automatically The following field types are as supported: ${DynamicEntityFieldType.values.map(_.toString).mkString("[", ", ", ", reference]")} The ${DynamicEntityFieldType.DATE_WITH_DAY} format is: ${DynamicEntityFieldType.DATE_WITH_DAY.dateFormat} Refere
- **Route Pattern**: `"management" :: "system-dynamic-entities" :: Nil`
- **Error Codes**: UnknownError
- **Source File**: APIMethods400.scala

#### `POST /management/user/reset-password-url`

- **Endpoint Name**: `resetPasswordUrl`
- **Summary**: Create password reset url
- **Description**: Create password reset url. 
- **Route Pattern**: `"management" :: "user" :: "reset-password-url" :: Nil`
- **Tags**: User
- **Required Roles**: CreateResetPasswordUrl
- **Error Codes**: UnknownError
- **Source Files**: APIMethods400.scala

#### `POST /my/api-collection-ids/API_COLLECTION_ID/api-collection-endpoints`

- **Endpoint Name**: `createMyApiCollectionEndpointById`
- **Summary**: Create My Api Collection Endpoint By Id
- **Description**: Create Api Collection Endpoint By Id. ${Glossary.getGlossaryItem("API Collections")} ${userAuthenticationMessage(true)} 
- **Route Pattern**: `"my" :: "api-collection-ids" :: apiCollectionId :: "api-collection-endpoints" :: Nil`
- **Tags**: ApiCollection
- **Path Parameters**: API_COLLECTION_ID
- **Error Codes**: UnknownError
- **Source Files**: APIMethods400.scala

#### `POST /my/api-collections`

- **Endpoint Name**: `createMyApiCollection`
- **Summary**: Create My Api Collection
- **Description**: Create Api Collection for logged in user. ${userAuthenticationMessage(true)} 
- **Route Pattern**: `"my" :: "api-collections" :: Nil`
- **Tags**: ApiCollection
- **Error Codes**: UserNotFound, UnknownError
- **Source Files**: APIMethods400.scala

#### `POST /my/api-collections/API_COLLECTION_NAME/api-collection-endpoints`

- **Endpoint Name**: `createMyApiCollectionEndpoint`
- **Summary**: Create My Api Collection Endpoint
- **Description**: Create Api Collection Endpoint. ${Glossary.getGlossaryItem("API Collections")} ${userAuthenticationMessage(true)} 
- **Route Pattern**: `"my" :: "api-collections" :: apiCollectionName :: "api-collection-endpoints" :: Nil`
- **Tags**: ApiCollection
- **Path Parameters**: API_COLLECTION_NAME
- **Error Codes**: UnknownError
- **Source Files**: APIMethods400.scala

#### `POST /my/user/attributes`

- **Endpoint Name**: `createMyPersonalUserAttribute`
- **Summary**: Create My Personal User Attribute
- **Description**: Create My Personal User Attribute The `type` field must be one of "STRING", "INTEGER", "DOUBLE" or DATE_WITH_DAY" ${userAuthenticationMessage(true)} 
- **Route Pattern**: `"my" :: "user" :: "attributes" :: Nil`
- **Tags**: User
- **Error Codes**: UnknownError
- **Source Files**: APIMethods400.scala

#### `POST /transaction-request-types/CARD/transaction-requests`

- **Endpoint Name**: `createTransactionRequestCard`
- **Summary**: Create Transaction Request (CARD)
- **Description**:  When using CARD, the payee is set in the request body . Money goes into the Counterparty in the request body. $transactionRequestGeneralText 
- **Route Pattern**: `"transaction-request-types" :: "CARD" :: "transaction-requests" :: Nil`
- **Tags**: Psd2, PSD2PIS, TransactionRequest
- **Path Parameters**: CARD
- **Request Body**: transactionRequestWithChargeJSON400
- **Response Body**: transactionRequestWithChargeJSON400
- **Error Codes**: AccountNotFound, BankNotFound, BankAccountNotFound, UnknownError
- **Source Files**: APIMethods400.scala

#### `POST /user-entitlements`

- **Endpoint Name**: `createUserWithRoles`
- **Summary**: Create (DAuth) User with Roles
- **Description**:  This endpoint is used as part of the DAuth solution to grant Entitlements for Roles to a smart contract on the blockchain. Put the smart contract address in username For provider use "dauth" This endpoint will create the User with username and provider if the User does not already exist. Then it will create Entitlements i.e. grant Roles to the User. Entitlements are used to grant System or Bank level roles to Users. (For Account level privileges, see Views) i.e. Entitlements are used to create 
- **Route Pattern**: `case "user-entitlements" :: Nil JsonPost`
- **Error Codes**: UnknownError
- **Source Files**: APIMethods400.scala

#### `POST /users/USERNAME/locks`

- **Endpoint Name**: `lockUser`
- **Summary**: Lock the user
- **Description**:  Lock a User. ${userAuthenticationMessage(true)} 
- **Route Pattern**: `"users" :: username ::  "locks" :: Nil`
- **Tags**: User
- **Required Roles**: LockUser
- **Path Parameters**: USERNAME
- **Error Codes**: UserNotFound, UnknownError
- **Source Files**: APIMethods400.scala

#### `POST /web-hooks/account/notifications/on-create-transaction`

- **Endpoint Name**: `createSystemAccountNotificationWebhook`
- **Summary**: Create system level Account Notification Webhook
- **Description**:  Create a notification Webhook that will fire for all accounts on the system. $generalWebHookInfo $accountNotificationWebhookInfo 
- **Required Roles**: CreateSystemAccountNotificationWebhook
- **Source Files**: APIMethods400.scala

### PUT Endpoints (41)

#### `PUT /banks/BANK_ID/accounts/ACCOUNT_ID/transaction-requests/TRANSACTION_REQUEST_ID/attributes/ATTRIBUTE_ID`

- **Endpoint Name**: `updateTransactionRequestAttribute`
- **Summary**: Update Transaction Request Attribute
- **Description**: Update Transaction Request Attribute ${userAuthenticationMessage(true)} 
- **Route Pattern**: `"banks" :: BankId(bankId) :: "accounts" :: AccountId(accountId) :: "transaction-requests" :: TransactionRequestId(transactionRequestId) :: "attributes" :: transactionRequestAttributeId :: Nil`
- **Tags**: TransactionRequest
- **Required Roles**: UpdateTransactionRequestAttributeAtOneBank
- **Path Parameters**: BANK_ID, ATTRIBUTE_ID, ACCOUNT_ID, TRANSACTION_REQUEST_ID
- **Error Codes**: BankNotFound, BankAccountNotFound, UnknownError
- **Source Files**: APIMethods400.scala

#### `PUT /banks/BANK_ID/accounts/ACCOUNT_ID/transactions/TRANSACTION_ID/attributes/ACCOUNT_ATTRIBUTE_ID`

- **Endpoint Name**: `updateTransactionAttribute`
- **Summary**: Update Transaction Attribute
- **Description**: Update Transaction Attribute ${userAuthenticationMessage(true)} 
- **Route Pattern**: `"banks" :: BankId(bankId) :: "accounts" :: AccountId(accountId) :: "transactions" :: TransactionId(transactionId) :: "attributes" :: transactionAttributeId :: Nil`
- **Tags**: Transaction
- **Required Roles**: UpdateTransactionAttributeAtOneBank
- **Path Parameters**: BANK_ID, ACCOUNT_ID, TRANSACTION_ID, ACCOUNT_ATTRIBUTE_ID
- **Error Codes**: BankNotFound, BankAccountNotFound, UnknownError
- **Source Files**: APIMethods400.scala

#### `PUT /banks/BANK_ID/atms/ATM_ID`

- **Endpoint Name**: `updateAtm`
- **Summary**: UPDATE ATM
- **Description**: Update ATM.
- **Route Pattern**: `"banks" :: BankId(bankId) :: "atms" :: AtmId(atmId) :: Nil`
- **Tags**: ATM
- **Required Roles**: UpdateAtm, UpdateAtmAtAnyBank
- **Path Parameters**: BANK_ID, ATM_ID
- **Error Codes**: UnknownError
- **Source Files**: APIMethods400.scala

#### `PUT /banks/BANK_ID/atms/ATM_ID/accessibility-features`

- **Endpoint Name**: `updateAtmAccessibilityFeatures`
- **Summary**: Update ATM Accessibility Features
- **Description**: Update ATM Accessibility Features. 
- **Route Pattern**: `"banks" :: BankId(bankId) :: "atms" :: AtmId(atmId) :: "accessibility-features" :: Nil`
- **Tags**: ATM
- **Path Parameters**: BANK_ID, ATM_ID
- **Error Codes**: UnknownError
- **Source Files**: APIMethods400.scala

#### `PUT /banks/BANK_ID/atms/ATM_ID/location-categories`

- **Endpoint Name**: `updateAtmLocationCategories`
- **Summary**: Update ATM Location Categories
- **Description**: Update ATM Location Categories. 
- **Route Pattern**: `"banks" :: BankId(bankId) :: "atms" :: AtmId(atmId) :: "location-categories" :: Nil`
- **Tags**: ATM
- **Path Parameters**: BANK_ID, ATM_ID
- **Error Codes**: UnknownError
- **Source Files**: APIMethods400.scala

#### `PUT /banks/BANK_ID/atms/ATM_ID/notes`

- **Endpoint Name**: `updateAtmNotes`
- **Summary**: Update ATM Notes
- **Description**: Update ATM Notes. 
- **Route Pattern**: `"banks" :: BankId(bankId) :: "atms" :: AtmId(atmId) :: "notes" :: Nil`
- **Tags**: ATM
- **Path Parameters**: BANK_ID, ATM_ID
- **Error Codes**: UnknownError
- **Source Files**: APIMethods400.scala

#### `PUT /banks/BANK_ID/atms/ATM_ID/services`

- **Endpoint Name**: `updateAtmServices`
- **Summary**: Update ATM Services
- **Description**: Update ATM Services. 
- **Route Pattern**: `"banks" :: BankId(bankId) :: "atms" :: AtmId(atmId) :: "services" :: Nil`
- **Tags**: ATM
- **Path Parameters**: BANK_ID, ATM_ID
- **Error Codes**: UnknownError
- **Source Files**: APIMethods400.scala

#### `PUT /banks/BANK_ID/atms/ATM_ID/supported-currencies`

- **Endpoint Name**: `updateAtmSupportedCurrencies`
- **Summary**: Update ATM Supported Currencies
- **Description**: Update ATM Supported Currencies. 
- **Route Pattern**: `"banks" :: BankId(bankId) :: "atms" :: AtmId(atmId) :: "supported-currencies" :: Nil`
- **Tags**: ATM
- **Path Parameters**: BANK_ID, ATM_ID
- **Error Codes**: UnknownError
- **Source Files**: APIMethods400.scala

#### `PUT /banks/BANK_ID/atms/ATM_ID/supported-languages`

- **Endpoint Name**: `updateAtmSupportedLanguages`
- **Summary**: Update ATM Supported Languages
- **Description**: Update ATM Supported Languages. 
- **Route Pattern**: `"banks" :: BankId(bankId) :: "atms" :: AtmId(atmId) :: "supported-languages" :: Nil`
- **Tags**: ATM
- **Path Parameters**: BANK_ID, ATM_ID
- **Error Codes**: UnknownError
- **Source Files**: APIMethods400.scala

#### `PUT /banks/BANK_ID/attribute-definitions/account`

- **Endpoint Name**: `createOrUpdateAccountAttributeDefinition`
- **Summary**: Create or Update Account Attribute Definition
- **Description**: Create or Update Account Attribute Definition The category field must be ${AttributeCategory.Account} The type field must be one of; ${AttributeType.DOUBLE}, ${AttributeType.STRING}, ${AttributeType.INTEGER} and ${AttributeType.DATE_WITH_DAY} ${userAuthenticationMessage(true)} 
- **Route Pattern**: `"banks" :: BankId(bankId) :: "attribute-definitions" :: "account" :: Nil`
- **Tags**: Account
- **Required Roles**: CreateAccountAttributeDefinitionAtOneBank
- **Path Parameters**: BANK_ID
- **Error Codes**: BankNotFound, UnknownError
- **Source Files**: APIMethods400.scala

#### `PUT /banks/BANK_ID/attribute-definitions/bank`

- **Endpoint Name**: `createOrUpdateBankAttributeDefinition`
- **Summary**: Create or Update Bank Attribute Definition
- **Description**: Create or Update Bank Attribute Definition The category field must be ${AttributeCategory.Bank} The type field must be one of; ${AttributeType.DOUBLE}, ${AttributeType.STRING}, ${AttributeType.INTEGER} and ${AttributeType.DATE_WITH_DAY} ${userAuthenticationMessage(true)} 
- **Route Pattern**: `"banks" :: BankId(bankId) :: "attribute-definitions" :: "bank" :: Nil`
- **Tags**: Bank
- **Required Roles**: CreateBankAttributeDefinitionAtOneBank
- **Path Parameters**: BANK_ID
- **Error Codes**: BankNotFound, UnknownError
- **Source Files**: APIMethods400.scala

#### `PUT /banks/BANK_ID/attribute-definitions/card`

- **Endpoint Name**: `createOrUpdateCardAttributeDefinition`
- **Summary**: Create or Update Card Attribute Definition
- **Description**: Create or Update Card Attribute Definition The category field must be ${AttributeCategory.Card} The type field must be one of; ${AttributeType.DOUBLE}, ${AttributeType.STRING}, ${AttributeType.INTEGER} and ${AttributeType.DATE_WITH_DAY} ${userAuthenticationMessage(true)} 
- **Route Pattern**: `"banks" :: BankId(bankId) :: "attribute-definitions" :: "card" :: Nil`
- **Tags**: Card
- **Required Roles**: CreateCardAttributeDefinitionAtOneBank
- **Path Parameters**: BANK_ID
- **Error Codes**: BankNotFound, UnknownError
- **Source Files**: APIMethods400.scala

#### `PUT /banks/BANK_ID/attribute-definitions/customer`

- **Endpoint Name**: `createOrUpdateCustomerAttributeAttributeDefinition`
- **Summary**: Create or Update Customer Attribute Definition
- **Description**: Create or Update Customer Attribute Definition The category field must be one of: ${AttributeCategory.Customer} The type field must be one of; ${AttributeType.DOUBLE}, ${AttributeType.STRING}, ${AttributeType.INTEGER} and ${AttributeType.DATE_WITH_DAY} ${userAuthenticationMessage(true)} 
- **Route Pattern**: `"banks" :: BankId(bankId) :: "attribute-definitions" :: "customer" :: Nil`
- **Tags**: Customer
- **Required Roles**: CreateCustomerAttributeDefinitionAtOneBank
- **Path Parameters**: BANK_ID
- **Error Codes**: BankNotFound, UnknownError
- **Source Files**: APIMethods400.scala

#### `PUT /banks/BANK_ID/attribute-definitions/product`

- **Endpoint Name**: `createOrUpdateProductAttributeDefinition`
- **Summary**: Create or Update Product Attribute Definition
- **Description**: Create or Update Product Attribute Definition The category field must be ${AttributeCategory.Product} The type field must be one of; ${AttributeType.DOUBLE}, ${AttributeType.STRING}, ${AttributeType.INTEGER} and ${AttributeType.DATE_WITH_DAY} ${userAuthenticationMessage(true)} 
- **Route Pattern**: `"banks" :: BankId(bankId) :: "attribute-definitions" :: "product" :: Nil`
- **Tags**: Product
- **Required Roles**: CreateProductAttributeDefinitionAtOneBank
- **Path Parameters**: BANK_ID
- **Error Codes**: BankNotFound, UnknownError
- **Source Files**: APIMethods400.scala

#### `PUT /banks/BANK_ID/attribute-definitions/transaction`

- **Endpoint Name**: `createOrUpdateTransactionAttributeDefinition`
- **Summary**: Create or Update Transaction Attribute Definition
- **Description**: Create or Update Transaction Attribute Definition The category field must be ${AttributeCategory.Transaction} The type field must be one of; ${AttributeType.DOUBLE}, ${AttributeType.STRING}, ${AttributeType.INTEGER} and ${AttributeType.DATE_WITH_DAY} ${userAuthenticationMessage(true)} 
- **Route Pattern**: `"banks" :: BankId(bankId) :: "attribute-definitions" :: "transaction" :: Nil`
- **Tags**: Transaction
- **Required Roles**: CreateTransactionAttributeDefinitionAtOneBank
- **Path Parameters**: BANK_ID
- **Error Codes**: BankNotFound, UnknownError
- **Source Files**: APIMethods400.scala

#### `PUT /banks/BANK_ID/attribute-definitions/transaction-request`

- **Endpoint Name**: `createOrUpdateTransactionRequestAttributeDefinition`
- **Summary**: Create or Update Transaction Request Attribute Definition
- **Description**: Create or Update Transaction Request Attribute Definition The category field must be ${AttributeCategory.TransactionRequest} The type field must be one of: ${AttributeType.DOUBLE}, ${AttributeType.STRING}, ${AttributeType.INTEGER} and ${AttributeType.DATE_WITH_DAY} ${userAuthenticationMessage(true)} 
- **Route Pattern**: `"banks" :: BankId(bankId) :: "attribute-definitions" :: "transaction-request" :: Nil`
- **Tags**: TransactionRequest
- **Required Roles**: CreateTransactionRequestAttributeDefinitionAtOneBank
- **Path Parameters**: BANK_ID
- **Error Codes**: BankNotFound, UnknownError
- **Source Files**: APIMethods400.scala

#### `PUT /banks/BANK_ID/attributes/BANK_ATTRIBUTE_ID`

- **Endpoint Name**: `updateBankAttribute`
- **Summary**: Update Bank Attribute
- **Description**: Update Bank Attribute. Update one Bak Attribute by its id. ${userAuthenticationMessage(true)} 
- **Route Pattern**: `"banks" :: bankId :: "attributes" :: bankAttributeId :: Nil`
- **Tags**: Bank
- **Path Parameters**: BANK_ID, BANK_ATTRIBUTE_ID
- **Error Codes**: UnknownError
- **Source Files**: APIMethods400.scala

#### `PUT /banks/BANK_ID/consents/CONSENT_ID`

- **Endpoint Name**: `updateConsentStatus`
- **Summary**: Update Consent Status
- **Description**:  This endpoint is used to update the Status of Consent. Each Consent has one of the following states: ${ConsentStatus.values.toList.sorted.mkString(", ") }. ${userAuthenticationMessage(true)} 
- **Route Pattern**: `"banks" :: BankId(bankId) :: "consents"  :: consentId :: Nil`
- **Path Parameters**: BANK_ID, CONSENT_ID
- **Source Files**: APIMethods400.scala

#### `PUT /banks/BANK_ID/consents/CONSENT_ID/user-update-request`

- **Endpoint Name**: `addConsentUser`
- **Summary**: Add User to a Consent
- **Description**:  This endpoint is used to add the User of Consent. Each Consent has one of the following states: ${ConsentStatus.values.toList.sorted.mkString(", ") }. ${userAuthenticationMessage(true)} 
- **Route Pattern**: `"banks" :: BankId(bankId) :: "consents"  :: consentId :: "user-update-request" :: Nil`
- **Path Parameters**: BANK_ID, CONSENT_ID
- **Source Files**: APIMethods400.scala

#### `PUT /banks/BANK_ID/customers/CUSTOMER_ID/attributes/CUSTOMER_ATTRIBUTE_ID`

- **Endpoint Name**: `updateCustomerAttribute`
- **Summary**: Update Customer Attribute
- **Description**: Update Customer Attribute ${userAuthenticationMessage(true)} 
- **Route Pattern**: `"banks" :: bankId :: "customers" :: customerId :: "attributes" :: customerAttributeId :: Nil`
- **Tags**: Customer
- **Required Roles**: UpdateCustomerAttributeAtAnyBank, UpdateCustomerAttributeAtOneBank
- **Path Parameters**: BANK_ID, CUSTOMER_ATTRIBUTE_ID, CUSTOMER_ID
- **Error Codes**: BankNotFound, UnknownError
- **Source Files**: APIMethods400.scala

#### `PUT /banks/BANK_ID/products/PRODUCT_CODE`

- **Endpoint Name**: `createProduct`
- **Summary**: Create Product
- **Description**: Create or Update Product for the Bank. Typical Super Family values / Asset classes are: Debt Equity FX Commodity Derivative $productHiearchyAndCollectionNote ${userAuthenticationMessage(true) } 
- **Route Pattern**: `"banks" :: BankId(bankId) :: "products" :: ProductCode(productCode) :: Nil`
- **Tags**: Product
- **Required Roles**: CreateProductAtAnyBank, CreateProduct
- **Path Parameters**: BANK_ID, PRODUCT_CODE
- **Error Codes**: BankNotFound, UnknownError
- **Source Files**: APIMethods400.scala

#### `PUT /banks/BANK_ID/products/PRODUCT_CODE/attributes/PRODUCT_ATTRIBUTE_ID`

- **Endpoint Name**: `updateProductAttribute`
- **Summary**: Update Product Attribute
- **Description**: Update Product Attribute. $productAttributeGeneralInfo Update one Product Attribute by its id. ${userAuthenticationMessage(true)} 
- **Route Pattern**: `"banks" :: bankId :: "products" :: productCode:: "attributes" :: productAttributeId :: Nil`
- **Tags**: Product
- **Required Roles**: UpdateProductAttribute
- **Path Parameters**: BANK_ID, PRODUCT_ATTRIBUTE_ID, PRODUCT_CODE
- **Error Codes**: UnknownError
- **Source Files**: APIMethods400.scala

#### `PUT /banks/BANK_ID/products/PRODUCT_CODE/fees/PRODUCT_FEE_ID`

- **Endpoint Name**: `updateProductFee`
- **Summary**: Update Product Fee
- **Description**: Update Product Fee. Update one Product Fee by its id. ${userAuthenticationMessage(true)} 
- **Route Pattern**: `"banks" :: bankId :: "products" :: productCode:: "fees" :: productFeeId :: Nil`
- **Tags**: Product
- **Required Roles**: UpdateProductFee
- **Path Parameters**: BANK_ID, PRODUCT_FEE_ID, PRODUCT_CODE
- **Error Codes**: BankNotFound, UnknownError
- **Source Files**: APIMethods400.scala

#### `PUT /management/authentication-type-validations/OPERATION_ID`

- **Endpoint Name**: `updateAuthenticationTypeValidation`
- **Summary**: Update an Authentication Type Validation
- **Description**: Update an Authentication Type Validation. Please supply allowed authentication types. 
- **Route Pattern**: `"management" :: "authentication-type-validations" :: operationId :: Nil`
- **Tags**: AuthenticationTypeValidation
- **Required Roles**: UpdateAuthenticationTypeValidation
- **Path Parameters**: OPERATION_ID
- **Error Codes**: UnknownError
- **Source Files**: APIMethods400.scala

#### `PUT /management/banks/BANK_ID/dynamic-endpoints/DYNAMIC_ENDPOINT_ID/host`

- **Endpoint Name**: `updateBankLevelDynamicEndpointHost`
- **Summary**:  Update Bank Level Dynamic Endpoint Host
- **Description**: Update Bank Level dynamic endpoint Host. The value can be obp_mock, dynamic_entity, or some service url. 
- **Route Pattern**: `"management" :: "banks" :: bankId :: "dynamic-endpoints" :: dynamicEndpointId :: "host" :: Nil`
- **Tags**: Api, ManageDynamicEndpoint
- **Required Roles**: UpdateBankLevelDynamicEndpoint, UpdateDynamicEndpoint
- **Path Parameters**: BANK_ID, DYNAMIC_ENDPOINT_ID
- **Error Codes**: BankNotFound, UnknownError, DynamicEntityNotFound
- **Source Files**: APIMethods400.scala

#### `PUT /management/banks/BANK_ID/dynamic-entities/DYNAMIC_ENTITY_ID`

- **Endpoint Name**: `updateBankLevelDynamicEntity`
- **Summary**: Update Bank Level Dynamic Entity
- **Description**: Update a Bank Level DynamicEntity. ${userAuthenticationMessage(true)} Update one DynamicEntity, after update finished, the corresponding CRUD endpoints will be changed. The following field types are as supported: ${DynamicEntityFieldType.values.map(_.toString).mkString("[", ", ", ", reference]")} ${DynamicEntityFieldType.DATE_WITH_DAY} format: ${DynamicEntityFieldType.DATE_WITH_DAY.dateFormat} Reference types are like foreign keys and composite foreign keys are supported. The value you need to s
- **Route Pattern**: `"management" :: "banks" :: bankId :: "dynamic-entities" :: dynamicEntityId :: Nil`
- **Tags**: ManageDynamicEntity, Api
- **Required Roles**: UpdateBankLevelDynamicEntity
- **Path Parameters**: BANK_ID, DYNAMIC_ENTITY_ID
- **Error Codes**: BankNotFound, UnknownError
- **Source File**: APIMethods400.scala

#### `PUT /management/banks/BANK_ID/dynamic-message-docs/DYNAMIC_MESSAGE_DOC_ID`

- **Endpoint Name**: `updateBankLevelDynamicMessageDoc`
- **Summary**: Update Bank Level Dynamic Message Doc
- **Description**: Update a Bank Level Dynamic Message Doc. 
- **Route Pattern**: `"management" :: "banks" :: bankId::"dynamic-message-docs" :: dynamicMessageDocId :: Nil`
- **Tags**: DynamicMessageDoc
- **Required Roles**: UpdateDynamicMessageDoc
- **Path Parameters**: BANK_ID, DYNAMIC_MESSAGE_DOC_ID
- **Error Codes**: BankNotFound, UnknownError
- **Source Files**: APIMethods400.scala

#### `PUT /management/banks/BANK_ID/dynamic-resource-docs/DYNAMIC-RESOURCE-DOC-ID`

- **Endpoint Name**: `updateBankLevelDynamicResourceDoc`
- **Summary**: Update Bank Level Dynamic Resource Doc
- **Description**: Update a Bank Level Dynamic Resource Doc. The connector_method_body is URL-encoded format String 
- **Route Pattern**: `"management" :: "banks" :: bankId :: "dynamic-resource-docs" :: dynamicResourceDocId :: Nil`
- **Tags**: DynamicResourceDoc
- **Required Roles**: UpdateBankLevelDynamicResourceDoc
- **Path Parameters**: BANK_ID, DYNAMIC, RESOURCE, ID, DOC
- **Error Codes**: BankNotFound, UnknownError
- **Source Files**: APIMethods400.scala

#### `PUT /management/banks/BANK_ID/endpoint-mappings/ENDPOINT_MAPPING_ID`

- **Endpoint Name**: `updateBankLevelEndpointMapping`
- **Summary**: Update Bank Level Endpoint Mapping
- **Description**: Update an Bank Level Endpoint Mapping. 
- **Route Pattern**: `"management" :: "banks" :: bankId :: "endpoint-mappings" :: endpointMappingId :: Nil`
- **Tags**: EndpointMapping
- **Required Roles**: UpdateEndpointMapping, UpdateBankLevelEndpointMapping
- **Path Parameters**: BANK_ID, ENDPOINT_MAPPING_ID
- **Error Codes**: BankNotFound, UnknownError
- **Source Files**: APIMethods400.scala

#### `PUT /management/banks/BANK_ID/endpoints/OPERATION_ID/tags/ENDPOINT_TAG_ID`

- **Endpoint Name**: `updateBankLevelEndpointTag`
- **Summary**: Update Bank Level Endpoint Tag
- **Description**: Update Endpoint Tag, you can only update the tag_name here, operation_id can not be updated. Note: Resource Docs are cached, TTL is ${CREATE_LOCALISED_RESOURCE_DOC_JSON_TTL} seconds 
- **Tags**: Api
- **Required Roles**: UpdateBankLevelEndpointTag
- **Path Parameters**: BANK_ID, OPERATION_ID, ENDPOINT_TAG_ID
- **Request Body**: CREATE_LOCALISED_RESOURCE_DOC_JSON_TTL
- **Error Codes**: BankNotFound, EndpointTagNotFound, UnknownError
- **Source Files**: APIMethods400.scala

#### `PUT /management/connector-methods/CONNECTOR_METHOD_ID`

- **Endpoint Name**: `updateConnectorMethod`
- **Summary**: Update Connector Method
- **Description**: Update an internal connector. The method_body is URL-encoded format String 
- **Route Pattern**: `"management" :: "connector-methods" :: connectorMethodId :: Nil`
- **Tags**: ConnectorMethod
- **Required Roles**: UpdateConnectorMethod
- **Path Parameters**: CONNECTOR_METHOD_ID
- **Error Codes**: UnknownError
- **Source Files**: APIMethods400.scala

#### `PUT /management/consumers/CONSUMER_ID/consumer/call-limits`

- **Endpoint Name**: `callsLimit`
- **Summary**: Set Rate Limits / Call Limits per Consumer
- **Description**:  Set the API rate limits / call limits for a Consumer: Rate limiting can be set: Per Second Per Minute Per Hour Per Week Per Month ${userAuthenticationMessage(true)} 
- **Route Pattern**: `"management" :: "consumers" :: consumerId :: "consumer" :: "call-limits" :: Nil`
- **Tags**: RateLimits, Consumer
- **Required Roles**: SetCallLimits
- **Path Parameters**: CONSUMER_ID
- **Error Codes**: UpdateConsumerError, ConsumerNotFound, UnknownError
- **Source Files**: APIMethods400.scala

#### `PUT /management/dynamic-endpoints/DYNAMIC_ENDPOINT_ID/host`

- **Endpoint Name**: `updateDynamicEndpointHost`
- **Summary**:  Update Dynamic Endpoint Host
- **Description**: Update dynamic endpoint Host. The value can be obp_mock, dynamic_entity, or some service url. 
- **Route Pattern**: `"management" :: "dynamic-endpoints" :: dynamicEndpointId :: "host" :: Nil`
- **Tags**: Api, ManageDynamicEndpoint
- **Required Roles**: UpdateDynamicEndpoint
- **Path Parameters**: DYNAMIC_ENDPOINT_ID
- **Error Codes**: UnknownError, DynamicEntityNotFound
- **Source Files**: APIMethods400.scala

#### `PUT /management/dynamic-message-docs/DYNAMIC_MESSAGE_DOC_ID`

- **Endpoint Name**: `updateDynamicMessageDoc`
- **Summary**: Update Dynamic Message Doc
- **Description**: Update a Dynamic Message Doc. 
- **Route Pattern**: `"management" :: "dynamic-message-docs" :: dynamicMessageDocId :: Nil`
- **Tags**: DynamicMessageDoc
- **Required Roles**: UpdateDynamicMessageDoc
- **Path Parameters**: DYNAMIC_MESSAGE_DOC_ID
- **Error Codes**: UnknownError
- **Source Files**: APIMethods400.scala

#### `PUT /management/dynamic-resource-docs/DYNAMIC-RESOURCE-DOC-ID`

- **Endpoint Name**: `updateDynamicResourceDoc`
- **Summary**: Update Dynamic Resource Doc
- **Description**: Update a Dynamic Resource Doc. The connector_method_body is URL-encoded format String 
- **Route Pattern**: `"management" :: "dynamic-resource-docs" :: dynamicResourceDocId :: Nil`
- **Tags**: DynamicResourceDoc
- **Required Roles**: UpdateDynamicResourceDoc
- **Path Parameters**: DOC, RESOURCE, ID, DYNAMIC
- **Error Codes**: UnknownError
- **Source Files**: APIMethods400.scala

#### `PUT /management/endpoint-mappings/ENDPOINT_MAPPING_ID`

- **Endpoint Name**: `updateEndpointMapping`
- **Summary**: Update Endpoint Mapping
- **Description**: Update an Endpoint Mapping. 
- **Route Pattern**: `"management" :: "endpoint-mappings" :: endpointMappingId :: Nil`
- **Tags**: EndpointMapping
- **Required Roles**: UpdateEndpointMapping
- **Path Parameters**: ENDPOINT_MAPPING_ID
- **Error Codes**: UnknownError
- **Source Files**: APIMethods400.scala

#### `PUT /management/endpoints/OPERATION_ID/tags/ENDPOINT_TAG_ID`

- **Endpoint Name**: `updateSystemLevelEndpointTag`
- **Summary**: Update System Level Endpoint Tag
- **Description**: Update System Level Endpoint Tag, you can only update the tag_name here, operation_id can not be updated. Note: Resource Docs are cached, TTL is ${CREATE_LOCALISED_RESOURCE_DOC_JSON_TTL} seconds 
- **Route Pattern**: `"management" :: "endpoints" :: operationId :: "tags" :: endpointTagId :: Nil`
- **Tags**: Api
- **Required Roles**: UpdateSystemLevelEndpointTag
- **Path Parameters**: OPERATION_ID, ENDPOINT_TAG_ID
- **Request Body**: CREATE_LOCALISED_RESOURCE_DOC_JSON_TTL
- **Error Codes**: EndpointTagNotFound, UnknownError
- **Source Files**: APIMethods400.scala

#### `PUT /management/json-schema-validations/OPERATION_ID`

- **Endpoint Name**: `updateJsonSchemaValidation`
- **Summary**: Update a JSON Schema Validation
- **Description**: Update a JSON Schema Validation. Introduction: ${Glossary.getGlossaryItemSimple("JSON Schema Validation")} To use this endpoint, please supply a valid json-schema in the request body. 
- **Route Pattern**: `"management" :: "json-schema-validations" :: operationId :: Nil`
- **Tags**: JsonSchemaValidation
- **Required Roles**: UpdateJsonSchemaValidation
- **Path Parameters**: OPERATION_ID
- **Error Codes**: UnknownError
- **Source Files**: APIMethods400.scala

#### `PUT /management/system-dynamic-entities/DYNAMIC_ENTITY_ID`

- **Endpoint Name**: `updateSystemDynamicEntity`
- **Summary**: Update System Level Dynamic Entity
- **Description**: Update a System Level Dynamic Entity. ${userAuthenticationMessage(true)} Update one DynamicEntity, after update finished, the corresponding CRUD endpoints will be changed. The following field types are as supported: ${DynamicEntityFieldType.values.map(_.toString).mkString("[", ", ", ", reference]")} ${DynamicEntityFieldType.DATE_WITH_DAY} format: ${DynamicEntityFieldType.DATE_WITH_DAY.dateFormat} Reference types are like foreign keys and composite foreign keys are supported. The value you need t
- **Route Pattern**: `"management" :: "system-dynamic-entities" :: dynamicEntityId :: Nil`
- **Tags**: ManageDynamicEntity, Api
- **Required Roles**: UpdateSystemDynamicEntity
- **Path Parameters**: DYNAMIC_ENTITY_ID
- **Error Codes**: DynamicEntityNotFound, UnknownError
- **Source File**: APIMethods400.scala

#### `PUT /my/dynamic-entities/DYNAMIC_ENTITY_ID`

- **Endpoint Name**: `updateMyDynamicEntity`
- **Summary**: Update My Dynamic Entity
- **Description**: Update my DynamicEntity. ${userAuthenticationMessage(true)} Update one of my DynamicEntity, after update finished, the corresponding CRUD endpoints will be changed. Current support filed types as follow: ${DynamicEntityFieldType.values.map(_.toString).mkString("[", ", ", ", reference]")} ${DynamicEntityFieldType.DATE_WITH_DAY} format: ${DynamicEntityFieldType.DATE_WITH_DAY.dateFormat} Reference types are like foreign keys and composite foreign keys are supported. The value you need to supply as 
- **Route Pattern**: `"my" :: "dynamic-entities" :: dynamicEntityId :: Nil`
- **Tags**: ManageDynamicEntity, Api
- **Path Parameters**: DYNAMIC_ENTITY_ID
- **Error Codes**: DynamicEntityNotFound, UnknownError
- **Source File**: APIMethods400.scala

#### `PUT /my/user/attributes/USER_ATTRIBUTE_ID`

- **Endpoint Name**: `updateMyPersonalUserAttribute`
- **Summary**: Update My Personal User Attribute
- **Description**: Update My Personal User Attribute for current user by USER_ATTRIBUTE_ID The type field must be one of "STRING", "INTEGER", "DOUBLE" or DATE_WITH_DAY" ${userAuthenticationMessage(true)} 
- **Route Pattern**: `"my" :: "user" :: "attributes" :: userAttributeId :: Nil`
- **Tags**: User
- **Path Parameters**: USER_ATTRIBUTE_ID
- **Error Codes**: UnknownError
- **Source Files**: APIMethods400.scala

### DELETE Endpoints (39)

#### `DELETE /banks/BANK_ID/CUSTOMER_ID/attributes/CUSTOMER_ATTRIBUTE_ID`

- **Endpoint Name**: `deleteCustomerAttribute`
- **Summary**: Delete Customer Attribute
- **Description**: Delete Customer Attribute $customerAttributeGeneralInfo Delete a Customer Attribute by its id. ${userAuthenticationMessage(true)} 
- **Route Pattern**: `"banks" :: bankId :: "customers" :: "attributes" :: customerAttributeId :: Nil`
- **Tags**: Customer
- **Required Roles**: DeleteCustomerAttributeAtOneBank, DeleteCustomerAttributeAtAnyBank
- **Path Parameters**: BANK_ID, CUSTOMER_ATTRIBUTE_ID, CUSTOMER_ID
- **Error Codes**: BankNotFound, UnknownError
- **Source Files**: APIMethods400.scala

#### `DELETE /banks/BANK_ID/accounts/ACCOUNT_ID/VIEW_ID/metadata/tags/TAG_ID`

- **Endpoint Name**: `deleteTagForViewOnAccount`
- **Summary**: Delete a tag on account
- **Description**: Deletes the tag TAG_ID about the account ACCOUNT_ID made on [view](#1_2_1-getViewsForBankAccount). ${userAuthenticationMessage(true)} Authentication is required as the tag is linked with the user.
- **Route Pattern**: `"banks" :: BankId(bankId) :: "accounts" :: AccountId(accountId) :: ViewId(viewId) :: "metadata" :: "tags" :: tagId :: Nil`
- **Tags**: Account, AccountMetadata
- **Path Parameters**: BANK_ID, ACCOUNT_ID, TAG_ID, VIEW_ID
- **Request Body**: accountTagsJSON
- **Error Codes**: BankNotFound, BankAccountNotFound, ViewNotFound, UnknownError
- **Source Files**: APIMethods400.scala

#### `DELETE /banks/BANK_ID/atms/ATM_ID`

- **Endpoint Name**: `deleteAtm`
- **Summary**: Delete ATM
- **Description**: Delete ATM.
- **Route Pattern**: `"banks" :: BankId(bankId) :: "atms" :: AtmId(atmId) :: Nil`
- **Tags**: ATM
- **Required Roles**: DeleteAtm, DeleteAtmAtAnyBank
- **Path Parameters**: BANK_ID, ATM_ID
- **Error Codes**: UnknownError
- **Source Files**: APIMethods400.scala

#### `DELETE /banks/BANK_ID/attribute-definitions/ATTRIBUTE_DEFINITION_ID/account`

- **Endpoint Name**: `deleteAccountAttributeDefinition`
- **Summary**: Delete Account Attribute Definition
- **Description**: Delete Account Attribute Definition by ATTRIBUTE_DEFINITION_ID ${userAuthenticationMessage(true)} 
- **Route Pattern**: `"banks" :: BankId(bankId) :: "attribute-definitions" :: attributeDefinitionId :: "account" :: Nil`
- **Tags**: Account
- **Required Roles**: DeleteAccountAttributeDefinitionAtOneBank
- **Path Parameters**: BANK_ID, ATTRIBUTE_DEFINITION_ID
- **Error Codes**: BankNotFound, UnknownError
- **Source Files**: APIMethods400.scala

#### `DELETE /banks/BANK_ID/attribute-definitions/ATTRIBUTE_DEFINITION_ID/card`

- **Endpoint Name**: `deleteCardAttributeDefinition`
- **Summary**: Delete Card Attribute Definition
- **Description**: Delete Card Attribute Definition by ATTRIBUTE_DEFINITION_ID ${userAuthenticationMessage(true)} 
- **Route Pattern**: `"banks" :: BankId(bankId) :: "attribute-definitions" :: attributeDefinitionId :: "card" :: Nil`
- **Tags**: Card
- **Required Roles**: DeleteCardAttributeDefinitionAtOneBank
- **Path Parameters**: BANK_ID, ATTRIBUTE_DEFINITION_ID
- **Error Codes**: BankNotFound, UnknownError
- **Source Files**: APIMethods400.scala

#### `DELETE /banks/BANK_ID/attribute-definitions/ATTRIBUTE_DEFINITION_ID/customer`

- **Endpoint Name**: `deleteCustomerAttributeDefinition`
- **Summary**: Delete Customer Attribute Definition
- **Description**: Delete Customer Attribute Definition by ATTRIBUTE_DEFINITION_ID ${userAuthenticationMessage(true)} 
- **Route Pattern**: `"banks" :: BankId(bankId) :: "attribute-definitions" :: attributeDefinitionId :: "customer" :: Nil`
- **Tags**: Customer
- **Required Roles**: DeleteCustomerAttributeDefinitionAtOneBank
- **Path Parameters**: BANK_ID, ATTRIBUTE_DEFINITION_ID
- **Error Codes**: BankNotFound, UnknownError
- **Source Files**: APIMethods400.scala

#### `DELETE /banks/BANK_ID/attribute-definitions/ATTRIBUTE_DEFINITION_ID/product`

- **Endpoint Name**: `deleteProductAttributeDefinition`
- **Summary**: Delete Product Attribute Definition
- **Description**: Delete Product Attribute Definition by ATTRIBUTE_DEFINITION_ID ${userAuthenticationMessage(true)} 
- **Route Pattern**: `"banks" :: BankId(bankId) :: "attribute-definitions" :: attributeDefinitionId :: "product" :: Nil`
- **Tags**: Product
- **Required Roles**: DeleteProductAttributeDefinitionAtOneBank
- **Path Parameters**: BANK_ID, ATTRIBUTE_DEFINITION_ID
- **Error Codes**: BankNotFound, UnknownError
- **Source Files**: APIMethods400.scala

#### `DELETE /banks/BANK_ID/attribute-definitions/ATTRIBUTE_DEFINITION_ID/transaction`

- **Endpoint Name**: `deleteTransactionAttributeDefinition`
- **Summary**: Delete Transaction Attribute Definition
- **Description**: Delete Transaction Attribute Definition by ATTRIBUTE_DEFINITION_ID ${userAuthenticationMessage(true)} 
- **Route Pattern**: `"banks" :: BankId(bankId) :: "attribute-definitions" :: attributeDefinitionId :: "transaction" :: Nil`
- **Tags**: Transaction
- **Required Roles**: DeleteTransactionAttributeDefinitionAtOneBank
- **Path Parameters**: BANK_ID, ATTRIBUTE_DEFINITION_ID
- **Error Codes**: BankNotFound, UnknownError
- **Source Files**: APIMethods400.scala

#### `DELETE /banks/BANK_ID/attribute-definitions/ATTRIBUTE_DEFINITION_ID/transaction-request`

- **Endpoint Name**: `deleteTransactionRequestAttributeDefinition`
- **Summary**: Delete Transaction Request Attribute Definition
- **Description**: Delete Transaction Request Attribute Definition by ATTRIBUTE_DEFINITION_ID ${userAuthenticationMessage(true)} 
- **Route Pattern**: `"banks" :: BankId(bankId) :: "attribute-definitions" :: attributeDefinitionId :: "transaction-request" :: Nil`
- **Tags**: TransactionRequest
- **Required Roles**: DeleteTransactionRequestAttributeDefinitionAtOneBank
- **Path Parameters**: BANK_ID, ATTRIBUTE_DEFINITION_ID
- **Error Codes**: BankNotFound, UnknownError
- **Source Files**: APIMethods400.scala

#### `DELETE /banks/BANK_ID/attributes/BANK_ATTRIBUTE_ID`

- **Endpoint Name**: `deleteBankAttribute`
- **Summary**: Delete Bank Attribute
- **Description**: Delete Bank Attribute Delete a Bank Attribute by its id. ${userAuthenticationMessage(true)} 
- **Route Pattern**: `"banks" :: bankId :: "attributes" :: bankAttributeId :: Nil`
- **Tags**: Bank
- **Path Parameters**: BANK_ID, BANK_ATTRIBUTE_ID
- **Error Codes**: BankNotFound, UnknownError
- **Source Files**: APIMethods400.scala

#### `DELETE /banks/BANK_ID/products/PRODUCT_CODE/fees/PRODUCT_FEE_ID`

- **Endpoint Name**: `deleteProductFee`
- **Summary**: Delete Product Fee
- **Description**: Delete Product Fee Delete one product fee by its id. ${userAuthenticationMessage(true)} 
- **Route Pattern**: `"banks" :: bankId :: "products" :: productCode:: "fees" :: productFeeId :: Nil`
- **Tags**: Product
- **Required Roles**: DeleteProductFee
- **Path Parameters**: BANK_ID, PRODUCT_FEE_ID, PRODUCT_CODE
- **Error Codes**: BankNotFound, UnknownError
- **Source Files**: APIMethods400.scala

#### `DELETE /banks/BANK_ID/user_customer_links/USER_CUSTOMER_LINK_ID`

- **Endpoint Name**: `deleteUserCustomerLink`
- **Summary**: Delete User Customer Link
- **Description**: Delete User Customer Link by USER_CUSTOMER_LINK_ID ${userAuthenticationMessage(true)} 
- **Route Pattern**: `"banks" :: BankId(bankId) :: "user_customer_links" :: userCustomerLinkId :: Nil`
- **Tags**: Customer
- **Required Roles**: DeleteUserCustomerLink
- **Path Parameters**: BANK_ID, _, USER_CUSTOMER_LINK_ID
- **Request Body**: createUserCustomerLinkJSONs
- **Error Codes**: BankNotFound, UnknownError
- **Source Files**: APIMethods400.scala

#### `DELETE /management/authentication-type-validations/OPERATION_ID`

- **Endpoint Name**: `deleteAuthenticationTypeValidation`
- **Summary**: Delete an Authentication Type Validation
- **Description**: Delete an Authentication Type Validation by operation_id. 
- **Route Pattern**: `"management" :: "authentication-type-validations" :: operationId :: Nil`
- **Tags**: AuthenticationTypeValidation
- **Required Roles**: DeleteAuthenticationValidation
- **Path Parameters**: OPERATION_ID
- **Error Codes**: UnknownError
- **Source Files**: APIMethods400.scala

#### `DELETE /management/banks/BANK_ID/accounts/ACCOUNT_ID/VIEW_ID/counterparties/COUNTERPARTY_ID`

- **Endpoint Name**: `deleteCounterpartyForAnyAccount`
- **Summary**: Delete Counterparty for any account (Explicit)
- **Description**: This is a management endpoint that enables the deletion of any specified Counterparty along with any related Metadata of that Counterparty. For a general introduction to Counterparties in OBP, see ${Glossary.getGlossaryItemLink("Counterparties")} ${userAuthenticationMessage(true)} 
- **Route Pattern**: `"management" :: "banks" :: BankId(bankId) :: "accounts" :: AccountId(accountId) :: ViewId(viewId) :: "counterparties" :: CounterpartyId(counterpartyId) :: Nil`
- **Tags**: Account, Counterparty
- **Required Roles**: DeleteCounterparty, DeleteCounterpartyAtAnyBank
- **Path Parameters**: BANK_ID, COUNTERPARTY_ID, ACCOUNT_ID, VIEW_ID
- **Error Codes**: BankNotFound, BankAccountNotFound, UnknownError
- **Source Files**: APIMethods400.scala

#### `DELETE /management/banks/BANK_ID/dynamic-endpoints/DYNAMIC_ENDPOINT_ID`

- **Endpoint Name**: `deleteBankLevelDynamicEndpoint`
- **Summary**:  Delete Bank Level Dynamic Endpoint
- **Description**: Delete a Bank Level DynamicEndpoint specified by DYNAMIC_ENDPOINT_ID.
- **Route Pattern**: `"management" :: "banks" :: bankId :: "dynamic-endpoints" :: dynamicEndpointId :: Nil`
- **Tags**: Api, ManageDynamicEndpoint
- **Required Roles**: DeleteBankLevelDynamicEndpoint, DeleteDynamicEndpoint
- **Path Parameters**: BANK_ID, DYNAMIC_ENDPOINT_ID
- **Error Codes**: BankNotFound, DynamicEndpointNotFound, UnknownError
- **Source Files**: APIMethods400.scala

#### `DELETE /management/banks/BANK_ID/dynamic-entities/DYNAMIC_ENTITY_ID`

- **Endpoint Name**: `deleteBankLevelDynamicEntity`
- **Summary**: Delete Bank Level Dynamic Entity
- **Description**: Delete a Bank Level DynamicEntity specified by DYNAMIC_ENTITY_ID. 
- **Route Pattern**: `"management" :: "banks" :: bankId :: "dynamic-entities" :: dynamicEntityId :: Nil`
- **Tags**: Api, ManageDynamicEntity
- **Required Roles**: DeleteBankLevelDynamicEntity
- **Path Parameters**: BANK_ID, DYNAMIC_ENTITY_ID
- **Error Codes**: BankNotFound, UnknownError
- **Source Files**: APIMethods400.scala

#### `DELETE /management/banks/BANK_ID/dynamic-message-docs/DYNAMIC_MESSAGE_DOC_ID`

- **Endpoint Name**: `deleteBankLevelDynamicMessageDoc`
- **Summary**: Delete Bank Level Dynamic Message Doc
- **Description**: Delete a Bank Level Dynamic Message Doc. 
- **Route Pattern**: `"management" :: "banks" :: bankId :: "dynamic-message-docs" :: dynamicMessageDocId :: Nil`
- **Tags**: DynamicMessageDoc
- **Required Roles**: DeleteBankLevelDynamicMessageDoc
- **Path Parameters**: BANK_ID, DYNAMIC_MESSAGE_DOC_ID
- **Error Codes**: BankNotFound, UnknownError
- **Source Files**: APIMethods400.scala

#### `DELETE /management/banks/BANK_ID/dynamic-resource-docs/DYNAMIC-RESOURCE-DOC-ID`

- **Endpoint Name**: `deleteBankLevelDynamicResourceDoc`
- **Summary**: Delete Bank Level Dynamic Resource Doc
- **Description**: Delete a Bank Level Dynamic Resource Doc. 
- **Route Pattern**: `"management" :: "banks" :: bankId :: "dynamic-resource-docs" :: dynamicResourceDocId :: Nil`
- **Tags**: DynamicResourceDoc
- **Required Roles**: DeleteBankLevelDynamicResourceDoc
- **Path Parameters**: BANK_ID, DYNAMIC, RESOURCE, ID, DOC
- **Error Codes**: BankNotFound, UnknownError
- **Source Files**: APIMethods400.scala

#### `DELETE /management/banks/BANK_ID/endpoint-mappings/ENDPOINT_MAPPING_ID`

- **Endpoint Name**: `deleteBankLevelEndpointMapping`
- **Summary**: Delete Bank Level Endpoint Mapping
- **Description**: Delete a Bank Level Endpoint Mapping. 
- **Route Pattern**: `"management" :: "banks" :: bankId :: "endpoint-mappings" :: endpointMappingId :: Nil`
- **Tags**: EndpointMapping
- **Required Roles**: DeleteEndpointMapping, DeleteBankLevelEndpointMapping
- **Path Parameters**: BANK_ID, ENDPOINT_MAPPING_ID
- **Error Codes**: BankNotFound, UnknownError
- **Source Files**: APIMethods400.scala

#### `DELETE /management/banks/BANK_ID/endpoints/OPERATION_ID/tags/ENDPOINT_TAG_ID`

- **Endpoint Name**: `deleteBankLevelEndpointTag`
- **Summary**: Delete Bank Level Endpoint Tag
- **Description**: Delete Bank Level Endpoint Tag.
- **Route Pattern**: `"my" :: "spaces" :: Nil`
- **Tags**: Api
- **Required Roles**: DeleteBankLevelEndpointTag
- **Path Parameters**: BANK_ID, OPERATION_ID, ENDPOINT_TAG_ID
- **Error Codes**: BankNotFound, UnknownError
- **Source Files**: APIMethods400.scala

#### `DELETE /management/cascading/banks/BANK_ID`

- **Endpoint Name**: `deleteBankCascade`
- **Summary**: Delete Bank Cascade
- **Description**: Delete a Bank Cascade specified by BANK_ID. ${userAuthenticationMessage(true)} 
- **Route Pattern**: `"management" :: "cascading" :: "banks" :: BankId(bankId) :: Nil`
- **Tags**: Bank
- **Required Roles**: DeleteBankCascade
- **Path Parameters**: BANK_ID
- **Error Codes**: BankNotFound, UnknownError
- **Source Files**: APIMethods400.scala

#### `DELETE /management/cascading/banks/BANK_ID/accounts/ACCOUNT_ID`

- **Endpoint Name**: `deleteAccountCascade`
- **Summary**: Delete Account Cascade
- **Description**: Delete an Account Cascade specified by ACCOUNT_ID. ${userAuthenticationMessage(true)} 
- **Route Pattern**: `"management" :: "cascading" :: "banks" :: BankId(bankId) :: "accounts" :: AccountId(accountId) :: Nil`
- **Tags**: Account
- **Required Roles**: DeleteAccountCascade
- **Path Parameters**: BANK_ID, ACCOUNT_ID
- **Error Codes**: BankNotFound, BankAccountNotFound, UnknownError
- **Source Files**: APIMethods400.scala

#### `DELETE /management/cascading/banks/BANK_ID/accounts/ACCOUNT_ID/transactions/TRANSACTION_ID`

- **Endpoint Name**: `deleteTransactionCascade`
- **Summary**: Delete Transaction Cascade
- **Description**: Delete a Transaction Cascade specified by TRANSACTION_ID. ${userAuthenticationMessage(true)} 
- **Tags**: Transaction
- **Required Roles**: DeleteTransactionCascade
- **Path Parameters**: BANK_ID, ACCOUNT_ID, TRANSACTION_ID
- **Error Codes**: BankNotFound, BankAccountNotFound, UnknownError
- **Source Files**: APIMethods400.scala

#### `DELETE /management/cascading/banks/BANK_ID/customers/CUSTOMER_ID`

- **Endpoint Name**: `deleteCustomerCascade`
- **Summary**: Delete Customer Cascade
- **Description**: Delete a Customer Cascade specified by CUSTOMER_ID. ${userAuthenticationMessage(true)} 
- **Route Pattern**: `"management" :: "cascading" :: "banks" :: BankId(bankId) :: "customers" :: CustomerId(customerId) :: Nil`
- **Tags**: Customer
- **Required Roles**: DeleteCustomerCascade
- **Path Parameters**: BANK_ID, CUSTOMER_ID
- **Error Codes**: CustomerNotFound, BankNotFound, UnknownError
- **Source Files**: APIMethods400.scala

#### `DELETE /management/cascading/banks/BANK_ID/products/PRODUCT_CODE`

- **Endpoint Name**: `deleteProductCascade`
- **Summary**: Delete Product Cascade
- **Description**: Delete a Product Cascade specified by PRODUCT_CODE. ${userAuthenticationMessage(true)} 
- **Route Pattern**: `"management" :: "cascading" :: "banks" :: BankId(bankId) :: "products" :: ProductCode(code) :: Nil`
- **Tags**: Product
- **Required Roles**: DeleteProductCascade
- **Path Parameters**: BANK_ID, PRODUCT_CODE
- **Error Codes**: BankNotFound, BankAccountNotFound, UnknownError
- **Source Files**: APIMethods400.scala

#### `DELETE /management/dynamic-endpoints/DYNAMIC_ENDPOINT_ID`

- **Endpoint Name**: `deleteDynamicEndpoint`
- **Summary**:  Delete Dynamic Endpoint
- **Description**: Delete a DynamicEndpoint specified by DYNAMIC_ENDPOINT_ID.
- **Route Pattern**: `"management" :: "dynamic-endpoints" :: dynamicEndpointId :: Nil`
- **Tags**: Api, ManageDynamicEndpoint
- **Required Roles**: DeleteDynamicEndpoint
- **Path Parameters**: DYNAMIC_ENDPOINT_ID
- **Error Codes**: DynamicEndpointNotFound, UnknownError
- **Source Files**: APIMethods400.scala

#### `DELETE /management/dynamic-message-docs/DYNAMIC_MESSAGE_DOC_ID`

- **Endpoint Name**: `deleteDynamicMessageDoc`
- **Summary**: Delete Dynamic Message Doc
- **Description**: Delete a Dynamic Message Doc. 
- **Route Pattern**: `"management" :: "dynamic-message-docs" :: dynamicMessageDocId :: Nil`
- **Tags**: DynamicMessageDoc
- **Required Roles**: DeleteDynamicMessageDoc
- **Path Parameters**: DYNAMIC_MESSAGE_DOC_ID
- **Error Codes**: UnknownError
- **Source Files**: APIMethods400.scala

#### `DELETE /management/dynamic-resource-docs/DYNAMIC-RESOURCE-DOC-ID`

- **Endpoint Name**: `deleteDynamicResourceDoc`
- **Summary**: Delete Dynamic Resource Doc
- **Description**: Delete a Dynamic Resource Doc. 
- **Route Pattern**: `"management" :: "dynamic-resource-docs" :: dynamicResourceDocId :: Nil`
- **Tags**: DynamicResourceDoc
- **Required Roles**: DeleteDynamicResourceDoc
- **Path Parameters**: DOC, RESOURCE, ID, DYNAMIC
- **Error Codes**: UnknownError
- **Source Files**: APIMethods400.scala

#### `DELETE /management/endpoint-mappings/ENDPOINT_MAPPING_ID`

- **Endpoint Name**: `deleteEndpointMapping`
- **Summary**: Delete Endpoint Mapping
- **Description**: Delete a Endpoint Mapping. 
- **Route Pattern**: `"management" :: "endpoint-mappings" :: endpointMappingId :: Nil`
- **Tags**: EndpointMapping
- **Required Roles**: DeleteEndpointMapping
- **Path Parameters**: ENDPOINT_MAPPING_ID
- **Error Codes**: UnknownError
- **Source Files**: APIMethods400.scala

#### `DELETE /management/endpoints/OPERATION_ID/tags/ENDPOINT_TAG_ID`

- **Endpoint Name**: `deleteSystemLevelEndpointTag`
- **Summary**: Delete System Level Endpoint Tag
- **Description**: Delete System Level Endpoint Tag.
- **Route Pattern**: `"management" :: "endpoints" :: operationId :: "tags" :: endpointTagId :: Nil`
- **Tags**: Api
- **Required Roles**: DeleteSystemLevelEndpointTag
- **Path Parameters**: OPERATION_ID, ENDPOINT_TAG_ID
- **Request Body**: CREATE_LOCALISED_RESOURCE_DOC_JSON_TTL
- **Error Codes**: UnknownError
- **Source Files**: APIMethods400.scala

#### `DELETE /management/json-schema-validations/OPERATION_ID`

- **Endpoint Name**: `deleteJsonSchemaValidation`
- **Summary**: Delete a JSON Schema Validation
- **Description**: Delete a JSON Schema Validation by operation_id. 
- **Route Pattern**: `"management" :: "json-schema-validations" :: operationId :: Nil`
- **Tags**: JsonSchemaValidation
- **Required Roles**: DeleteJsonSchemaValidation
- **Path Parameters**: OPERATION_ID
- **Error Codes**: UnknownError
- **Source Files**: APIMethods400.scala

#### `DELETE /management/system-dynamic-entities/DYNAMIC_ENTITY_ID`

- **Endpoint Name**: `deleteSystemDynamicEntity`
- **Summary**: Delete System Level Dynamic Entity
- **Description**: Delete a DynamicEntity specified by DYNAMIC_ENTITY_ID. 
- **Route Pattern**: `"management" :: "system-dynamic-entities" :: dynamicEntityId :: Nil`
- **Tags**: Api, ManageDynamicEntity
- **Required Roles**: DeleteSystemLevelDynamicEntity
- **Path Parameters**: DYNAMIC_ENTITY_ID
- **Error Codes**: UnknownError
- **Source Files**: APIMethods400.scala

#### `DELETE /my/api-collection-ids/API_COLLECTION_ID/api-collection-endpoint-ids/API_COLLECTION_ENDPOINT_ID`

- **Endpoint Name**: `deleteMyApiCollectionEndpointById`
- **Summary**: Delete My Api Collection Endpoint By Id
- **Description**: ${Glossary.getGlossaryItem("API Collections")} Delete Api Collection Endpoint Delete Api Collection Endpoint By Id ${userAuthenticationMessage(true)} 
- **Route Pattern**: `"my" :: "api-collection-ids" :: apiCollectionId :: "api-collection-endpoint-ids" :: apiCollectionEndpointId :: Nil`
- **Tags**: ApiCollection
- **Path Parameters**: API_COLLECTION_ID, API_COLLECTION_ENDPOINT_ID
- **Error Codes**: UserNotFound, UnknownError
- **Source Files**: APIMethods400.scala

#### `DELETE /my/api-collection-ids/API_COLLECTION_ID/api-collection-endpoints/OPERATION_ID`

- **Endpoint Name**: `deleteMyApiCollectionEndpointByOperationId`
- **Summary**: Delete My Api Collection Endpoint By Id
- **Description**: ${Glossary.getGlossaryItem("API Collections")} Delete Api Collection Endpoint By OPERATION_ID ${userAuthenticationMessage(true)} 
- **Route Pattern**: `"my" :: "api-collection-ids" :: apiCollectionId :: "api-collection-endpoints" :: operationId :: Nil`
- **Tags**: ApiCollection
- **Path Parameters**: API_COLLECTION_ID, OPERATION_ID
- **Error Codes**: UserNotFound, UnknownError
- **Source Files**: APIMethods400.scala

#### `DELETE /my/api-collections/API_COLLECTION_ID`

- **Endpoint Name**: `deleteMyApiCollection`
- **Summary**: Delete My Api Collection
- **Description**: Delete Api Collection By API_COLLECTION_ID ${Glossary.getGlossaryItem("API Collections")} ${userAuthenticationMessage(true)} 
- **Route Pattern**: `"my" :: "api-collections" :: apiCollectionId :: Nil`
- **Tags**: ApiCollection
- **Path Parameters**: API_COLLECTION_ID
- **Error Codes**: UserNotFound, UnknownError
- **Source Files**: APIMethods400.scala

#### `DELETE /my/api-collections/API_COLLECTION_NAME/api-collection-endpoints/OPERATION_ID`

- **Endpoint Name**: `deleteMyApiCollectionEndpoint`
- **Summary**: Delete My Api Collection Endpoint
- **Description**: ${Glossary.getGlossaryItem("API Collections")} Delete Api Collection Endpoint By OPERATION_ID ${userAuthenticationMessage(true)} 
- **Route Pattern**: `"my" :: "api-collections" :: apiCollectionName :: "api-collection-endpoints" :: operationId :: Nil`
- **Tags**: ApiCollection
- **Path Parameters**: OPERATION_ID, API_COLLECTION_NAME
- **Error Codes**: UserNotFound, UnknownError
- **Source Files**: APIMethods400.scala

#### `DELETE /my/dynamic-endpoints/DYNAMIC_ENDPOINT_ID`

- **Endpoint Name**: `deleteMyDynamicEndpoint`
- **Summary**: Delete My Dynamic Endpoint
- **Description**: Delete a DynamicEndpoint specified by DYNAMIC_ENDPOINT_ID.
- **Route Pattern**: `"my" :: "dynamic-endpoints" :: dynamicEndpointId :: Nil`
- **Path Parameters**: DYNAMIC_ENDPOINT_ID
- **Error Codes**: DynamicEndpointNotFound, UnknownError
- **Source Files**: APIMethods400.scala

#### `DELETE /my/dynamic-entities/DYNAMIC_ENTITY_ID`

- **Endpoint Name**: `deleteMyDynamicEntity`
- **Summary**: Delete My Dynamic Entity
- **Description**: Delete my DynamicEntity specified by DYNAMIC_ENTITY_ID. 
- **Route Pattern**: `"my" :: "dynamic-entities" :: dynamicEntityId :: Nil`
- **Tags**: Api, ManageDynamicEntity
- **Path Parameters**: DYNAMIC_ENTITY_ID
- **Error Codes**: UnknownError
- **Source Files**: APIMethods400.scala

#### `DELETE /users/USER_ID`

- **Endpoint Name**: `deleteUser`
- **Summary**: Delete a User
- **Description**: Delete a User. ${userAuthenticationMessage(true)} 
- **Route Pattern**: `"users" :: userId :: Nil`
- **Tags**: User
- **Required Roles**: DeleteUser
- **Path Parameters**: USER_ID
- **Error Codes**: UnknownError
- **Source Files**: APIMethods400.scala


---

## API Context: v5_0_0

**Total Endpoints**: 35

### GET Endpoints (15)

#### `GET /adapter`

- **Endpoint Name**: `getAdapterInfo`
- **Summary**: Get Adapter Info
- **Description**: Get basic information about the Adapter. ${userAuthenticationMessage(true)} 
- **Route Pattern**: `case "adapter" :: Nil JsonGet`
- **Tags**: Api
- **Required Roles**: GetAdapterInfo
- **Error Codes**: UnknownError
- **Source Files**: APIMethods500.scala

#### `GET /banks/BANK_ID`

- **Endpoint Name**: `getBank`
- **Summary**: Get Bank
- **Description**: Get the bank specified by BANK_ID Returns information about a single bank specified by BANK_ID including: * Bank code and full name of bank * Logo URL * Website
- **Route Pattern**: `"banks" :: BankId(bankId) :: Nil`
- **Path Parameters**: BANK_ID
- **Request Body**: createBankJSON500
- **Error Codes**: BankNotFound, UnknownError
- **Source Files**: APIMethods500.scala

#### `GET /banks/BANK_ID/accounts/ACCOUNT_ID/customer-account-links`

- **Endpoint Name**: `getCustomerAccountLinksByBankIdAccountId`
- **Summary**: Get Customer Account Links by ACCOUNT_ID
- **Description**: Get Customer Account Links by ACCOUNT_ID ${userAuthenticationMessage(true)} 
- **Route Pattern**: `"banks" :: bankId :: "accounts" :: accountId :: "customer-account-links" :: Nil`
- **Tags**: Customer
- **Required Roles**: GetCustomerAccountLinks
- **Path Parameters**: BANK_ID, ACCOUNT_ID
- **Error Codes**: BankNotFound, BankAccountNotFound, UnknownError
- **Source Files**: APIMethods500.scala

#### `GET /banks/BANK_ID/accounts/ACCOUNT_ID/views`

- **Endpoint Name**: `getViewsForBankAccount`
- **Summary**: Get Views for Account
- **Description**: #Views Views in Open Bank Project provide a mechanism for fine grained access control and delegation to Accounts and Transactions. Account holders use the 'owner' view by default. Delegated access is made through other views for example 'accountants', 'share-holders' or 'tagging-application'. Views can be created via the API and each view has a list of entitlements. Views on accounts and transactions filter the underlying data to redact certain fields for certain users. For instance the balance 
- **Route Pattern**: `"banks" :: BankId(bankId) :: "accounts" :: AccountId(accountId) :: "views" :: Nil`
- **Path Parameters**: BANK_ID, ACCOUNT_ID
- **Source Files**: APIMethods500.scala

#### `GET /banks/BANK_ID/customer-account-links/CUSTOMER_ACCOUNT_LINK_ID`

- **Endpoint Name**: `getCustomerAccountLinkById`
- **Summary**: Get Customer Account Link by Id
- **Description**: Get Customer Account Link by CUSTOMER_ACCOUNT_LINK_ID ${userAuthenticationMessage(true)} 
- **Route Pattern**: `"banks" :: BankId(bankId) :: "customer-account-links" :: customerAccountLinkId :: Nil`
- **Tags**: Customer
- **Required Roles**: GetCustomerAccountLink
- **Path Parameters**: BANK_ID, CUSTOMER_ACCOUNT_LINK_ID
- **Error Codes**: BankNotFound, UnknownError
- **Source Files**: APIMethods500.scala

#### `GET /banks/BANK_ID/customers`

- **Endpoint Name**: `getCustomersAtOneBank`
- **Summary**: Get Customers at Bank
- **Description**: Get Customers at Bank. ${userAuthenticationMessage(true)} 
- **Route Pattern**: `"banks" :: BankId(bankId) :: "customers" :: Nil`
- **Tags**: Customer, User
- **Required Roles**: GetCustomers
- **Path Parameters**: BANK_ID
- **Error Codes**: UserCustomerLinksNotFound, UnknownError
- **Source Files**: APIMethods500.scala

#### `GET /banks/BANK_ID/customers-minimal`

- **Endpoint Name**: `getCustomersMinimalAtOneBank`
- **Summary**: Get Customers Minimal at Bank
- **Description**: Get Customers Minimal at Bank. 
- **Route Pattern**: `"banks" :: BankId(bankId) :: "customers-minimal" :: Nil`
- **Tags**: Customer, User
- **Required Roles**: GetCustomersMinimal
- **Path Parameters**: BANK_ID
- **Error Codes**: UserCustomerLinksNotFound, UnknownError
- **Source Files**: APIMethods500.scala

#### `GET /banks/BANK_ID/customers/CUSTOMER_ID/customer-account-links`

- **Endpoint Name**: `getCustomerAccountLinksByCustomerId`
- **Summary**: Get Customer Account Links by CUSTOMER_ID
- **Description**: Get Customer Account Links by CUSTOMER_ID ${userAuthenticationMessage(true)} 
- **Route Pattern**: `"banks" :: BankId(bankId) :: "customers" :: customerId :: "customer-account-links" :: Nil`
- **Tags**: Customer
- **Required Roles**: GetCustomerAccountLinks
- **Path Parameters**: BANK_ID, CUSTOMER_ID
- **Error Codes**: CustomerNotFound, BankNotFound, UnknownError
- **Source Files**: APIMethods500.scala

#### `GET /banks/BANK_ID/my/customers`

- **Endpoint Name**: `getMyCustomersAtBank`
- **Summary**: Get My Customers at Bank
- **Description**: Returns a list of Customers at the Bank that are linked to the currently authenticated User. ${userAuthenticationMessage(true)}
- **Route Pattern**: `"banks" :: BankId(bankId) :: "my" :: "customers" :: Nil`
- **Tags**: Customer
- **Path Parameters**: BANK_ID
- **Request Body**: customerJSONs
- **Response Body**: customerJSONs
- **Error Codes**: BankNotFound, UserCustomerLinksNotFound, UnknownError
- **Source Files**: APIMethods500.scala

#### `GET /consumer/consent-requests/CONSENT_REQUEST_ID`

- **Endpoint Name**: `getConsentRequest`
- **Summary**: Get Consent Request
- **Route Pattern**: `"consumer" :: "consent-requests" :: consentRequestId :: Nil`
- **Path Parameters**: CONSENT_REQUEST_ID
- **Error Codes**: X509GeneralError, ConsentRequestNotFound, UnknownError
- **Source Files**: APIMethods500.scala

#### `GET /consumer/consent-requests/CONSENT_REQUEST_ID/consents`

- **Endpoint Name**: `getConsentByConsentRequestId`
- **Summary**: Get Consent By Consent Request Id via Consumer
- **Description**:  This endpoint gets the Consent By consent request id. ${userAuthenticationMessage(true)} 
- **Route Pattern**: `"consumer" :: "consent-requests" :: consentRequestId :: "consents" :: Nil`
- **Tags**: Psd2, Consent, PSD2AIS
- **Path Parameters**: CONSENT_REQUEST_ID
- **Error Codes**: UnknownError
- **Source Files**: APIMethods500.scala

#### `GET /management/metrics/banks/BANK_ID`

- **Endpoint Name**: `getMetricsAtBank`
- **Summary**: Get Metrics at Bank
- **Description**: Get the all metrics at the Bank specified by BANK_ID require CanReadMetrics role Filters Part 1.*filtering* (no wilde cards etc.) parameters to GET /management/metrics Should be able to filter on the following metrics fields eg: /management/metrics?from_date=$DateWithMsExampleString&to_date=$DateWithMsExampleString&limit=50&offset=2 1 from_date (defaults to one week before current date): eg:from_date=$DateWithMsExampleString 2 to_date (defaults to current date) eg:to_date=$DateWithMsExampleStrin
- **Route Pattern**: `"management" :: "metrics" :: "banks" :: bankId :: Nil`
- **Path Parameters**: BANK_ID
- **Source Files**: APIMethods500.scala

#### `GET /my/customers`

- **Endpoint Name**: `getMyCustomersAtAnyBank`
- **Summary**: Get My Customers
- **Description**: Gets all Customers that are linked to me. Authentication via OAuth is required.
- **Route Pattern**: `"my" :: "customers" :: Nil`
- **Tags**: Customer, User
- **Request Body**: customerJSONs
- **Response Body**: customerJSONs
- **Error Codes**: UserCustomerLinksNotFound, UnknownError
- **Source Files**: APIMethods500.scala

#### `GET /system-views-ids`

- **Endpoint Name**: `getSystemViewsIds`
- **Summary**: Get Ids of System Views
- **Description**: Get Ids of System Views ${userAuthenticationMessage(true)} 
- **Route Pattern**: `case "system-views-ids" :: Nil JsonGet`
- **Tags**: SystemView
- **Required Roles**: GetSystemView
- **Error Codes**: BankNotFound, UnknownError
- **Source Files**: APIMethods500.scala

#### `GET /users/USER_ID/auth-context`

- **Endpoint Name**: `getUserAuthContexts`
- **Summary**: Get User Auth Contexts
- **Description**: Get User Auth Contexts for a User. ${userAuthenticationMessage(true)} 
- **Route Pattern**: `"users" :: userId :: "auth-context" :: Nil`
- **Tags**: User
- **Path Parameters**: USER_ID
- **Error Codes**: UnknownError
- **Source Files**: APIMethods500.scala

### POST Endpoints (14)

#### `POST /banks`

- **Endpoint Name**: `createBank`
- **Summary**: Create Bank
- **Description**: Create a new bank (Authenticated access). The user creating this will be automatically assigned the Role CanCreateEntitlementAtOneBank. Thus the User can manage the bank they create and assign Roles to other Users. Only SANDBOX mode The settlement accounts are created specified by the bank in the POST body. Name and account id are created in accordance to the next rules: - Incoming account (name: Default incoming settlement account, Account ID: OBP_DEFAULT_INCOMING_ACCOUNT_ID, currency: EUR) - O
- **Route Pattern**: `case "banks" :: Nil JsonPost`
- **Tags**: Bank
- **Required Roles**: CreateBank
- **Error Codes**: UnknownError
- **Source Files**: APIMethods500.scala

#### `POST /banks/BANK_ID/customer-account-links`

- **Endpoint Name**: `createCustomerAccountLink`
- **Summary**: Create Customer Account Link
- **Description**: Link a Customer to a Account ${userAuthenticationMessage(true)} 
- **Route Pattern**: `"banks" :: BankId(bankId):: "customer-account-links" :: Nil`
- **Tags**: Account, Customer
- **Required Roles**: CreateCustomerAccountLink
- **Path Parameters**: BANK_ID
- **Error Codes**: BankNotFound, BankAccountNotFound, CustomerNotFound, UnknownError, CreateCustomerAccountLinkError
- **Source Files**: APIMethods500.scala

#### `POST /banks/BANK_ID/customers`

- **Endpoint Name**: `createCustomer`
- **Summary**: Create Customer
- **Description**:  The Customer resource stores the customer number (which is set by the backend), legal name, email, phone number, their date of birth, relationship status, education attained, a url for a profile image, KYC status etc. Dates need to be in the format 2013-01-21T23:08:00Z Note: If you need to set a specific customer number, use the Update Customer Number endpoint after this call. ${userAuthenticationMessage(true)} 
- **Route Pattern**: `"banks" :: BankId(bankId) :: "customers" :: Nil`
- **Tags**: Person, Customer
- **Required Roles**: CreateCustomer, CreateCustomerAtAnyBank
- **Path Parameters**: BANK_ID
- **Error Codes**: UserNotFound, BankNotFound, CreateConsumerError, UnknownError
- **Source Files**: APIMethods500.scala

#### `POST /banks/BANK_ID/customers/customer-number-query/overview`

- **Endpoint Name**: `getCustomerOverview`
- **Summary**: Get Customer Overview
- **Description**: Gets the Customer Overview specified by customer_number and bank_code. ${userAuthenticationMessage(true)} 
- **Route Pattern**: `"banks" :: BankId(bankId) :: "customers" :: "customer-number-query" :: "overview" :: Nil`
- **Tags**: Customer, Kyc
- **Required Roles**: GetCustomerOverview
- **Path Parameters**: BANK_ID
- **Error Codes**: UserCustomerLinksNotFound, UnknownError
- **Source Files**: APIMethods500.scala

#### `POST /banks/BANK_ID/customers/customer-number-query/overview-flat`

- **Endpoint Name**: `getCustomerOverviewFlat`
- **Summary**: Get Customer Overview Flat
- **Description**: Gets the Customer Overview Flat specified by customer_number and bank_code. ${userAuthenticationMessage(true)} 
- **Route Pattern**: `"banks" :: BankId(bankId) :: "customers" :: "customer-number-query" :: "overview-flat" :: Nil`
- **Tags**: Customer, Kyc
- **Required Roles**: GetCustomerOverviewFlat
- **Path Parameters**: BANK_ID
- **Error Codes**: UserCustomerLinksNotFound, UnknownError
- **Source Files**: APIMethods500.scala

#### `POST /banks/BANK_ID/users/current/auth-context-updates/AUTH_CONTEXT_UPDATE_ID/challenge`

- **Endpoint Name**: `answerUserAuthContextUpdateChallenge`
- **Summary**: Answer User Auth Context Update Challenge
- **Description**:  Answer User Auth Context Update Challenge. 
- **Route Pattern**: `"banks" :: BankId(bankId) :: "users" :: "current" ::"auth-context-updates"  :: authContextUpdateId :: "challenge" :: Nil`
- **Path Parameters**: BANK_ID, AUTH_CONTEXT_UPDATE_ID
- **Source Files**: APIMethods500.scala

#### `POST /banks/BANK_ID/users/current/auth-context-updates/SCA_METHOD`

- **Endpoint Name**: `createUserAuthContextUpdateRequest`
- **Summary**: Create User Auth Context Update Request
- **Description**: Create User Auth Context Update Request. ${userAuthenticationMessage(true)} A One Time Password (OTP) (AKA security challenge) is sent Out of Band (OOB) to the User via the transport defined in SCA_METHOD SCA_METHOD is typically "SMS" or "EMAIL". "EMAIL" is used for testing purposes. 
- **Route Pattern**: `"banks" :: BankId(bankId) :: "users" :: "current" ::"auth-context-updates" :: scaMethod :: Nil`
- **Tags**: User
- **Path Parameters**: BANK_ID, SCA_METHOD
- **Error Codes**: BankNotFound, UnknownError, CreateUserAuthContextError
- **Source Files**: APIMethods500.scala

#### `POST /consumer/consent-requests`

- **Endpoint Name**: `createConsentRequest`
- **Summary**: Create Consent Request
- **Description**:  Client Authentication (mandatory) It is used when applications request an access token to access their own resources, not on behalf of a user. The client needs to authenticate themselves for this request. In case of public client we use client_id and private key to obtain access token, otherwise we use client_id and client_secret. The obtained access token is used in the HTTP Bearer auth header of our request. Example: Authorization: Bearer eXtneO-THbQtn3zvK_kQtXXfvOZyZFdBCItlPDbR2Bk.dOWqtXCtFX
- **Route Pattern**: `"consumer" :: "consent-requests" :: Nil`
- **Source Files**: APIMethods500.scala

#### `POST /consumer/consent-requests/CONSENT_REQUEST_ID/EMAIL/consents`

- **Endpoint Name**: `createConsentByConsentRequestIdEmail`
- **Summary**: Create Consent By CONSENT_REQUEST_ID (EMAIL)
- **Description**:  This endpoint continues the process of creating a Consent. It starts the SCA flow which changes the status of the consent from INITIATED to ACCEPTED or REJECTED. Please note that the Consent cannot elevate the privileges of the logged in user. 
- **Path Parameters**: CONSENT_REQUEST_ID, EMAIL
- **Source Files**: APIMethods500.scala

#### `POST /consumer/consent-requests/CONSENT_REQUEST_ID/IMPLICIT/consents`

- **Endpoint Name**: `createConsentByConsentRequestIdImplicit`
- **Summary**: Create Consent By CONSENT_REQUEST_ID (IMPLICIT)
- **Description**:  This endpoint continues the process of creating a Consent. It starts the SCA flow which changes the status of the consent from INITIATED to ACCEPTED or REJECTED. Please note that the Consent cannot elevate the privileges logged in user already have. 
- **Path Parameters**: CONSENT_REQUEST_ID, IMPLICIT
- **Source Files**: APIMethods500.scala

#### `POST /consumer/consent-requests/CONSENT_REQUEST_ID/SMS/consents`

- **Endpoint Name**: `createConsentByConsentRequestIdSms`
- **Summary**: Create Consent By CONSENT_REQUEST_ID (SMS)
- **Description**:  This endpoint continues the process of creating a Consent. It starts the SCA flow which changes the status of the consent from INITIATED to ACCEPTED or REJECTED. Please note that the Consent you are creating cannot exceed the entitlements that the User creating this consents already has. 
- **Path Parameters**: CONSENT_REQUEST_ID, SMS
- **Source Files**: APIMethods500.scala

#### `POST /management/banks/BANK_ID/cards`

- **Endpoint Name**: `addCardForBank`
- **Summary**: Create Card
- **Description**: Create Card at bank specified by BANK_ID . ${userAuthenticationMessage(true)} 
- **Route Pattern**: `"management" :: "banks" :: BankId(bankId) :: "cards" :: Nil`
- **Tags**: Card
- **Required Roles**: CreateCardsForBank
- **Path Parameters**: BANK_ID
- **Error Codes**: BankNotFound, UnknownError
- **Source Files**: APIMethods500.scala

#### `POST /system-views`

- **Endpoint Name**: `createSystemView`
- **Summary**: Create System View
- **Description**: Create a system view ${userAuthenticationMessage(true)} and the user needs to have access to the $canCreateSystemView entitlement. The 'allowed_actions' field is a list containing the names of the actions allowed through this view. All the actions contained in the list will be set to `true` on the view creation, the rest will be set to `false`. The 'alias' field in the JSON can take one of three values: * _public_: to use the public alias if there is one specified for the other account. * _priva
- **Route Pattern**: `case "system-views" :: Nil JsonPost`
- **Tags**: SystemView
- **Required Roles**: CreateSystemView
- **Error Codes**: UnknownError
- **Source Files**: APIMethods500.scala

#### `POST /users/USER_ID/auth-context`

- **Endpoint Name**: `createUserAuthContext`
- **Summary**: Create User Auth Context
- **Description**: Create User Auth Context. These key value pairs will be propagated over connector to adapter. Normally used for mapping OBP user and Bank User/Customer. ${userAuthenticationMessage(true)} 
- **Route Pattern**: `"users" :: userId ::"auth-context" :: Nil`
- **Tags**: User
- **Required Roles**: CreateUserAuthContext
- **Path Parameters**: USER_ID
- **Error Codes**: CreateUserAuthContextError, UnknownError
- **Source Files**: APIMethods500.scala

### PUT Endpoints (5)

#### `PUT /banks`

- **Endpoint Name**: `updateBank`
- **Summary**: Update Bank
- **Description**: Update an existing bank (Authenticated access). 
- **Route Pattern**: `case "banks" :: Nil JsonPut`
- **Tags**: Bank
- **Required Roles**: CreateBank
- **Error Codes**: BankNotFound, updateBankError, UnknownError
- **Source Files**: APIMethods500.scala

#### `PUT /banks/BANK_ID/accounts/ACCOUNT_ID`

- **Endpoint Name**: `createAccount`
- **Summary**: Create Account (PUT)
- **Description**: Create Account at bank specified by BANK_ID with Id specified by ACCOUNT_ID. The User can create an Account for themself - or - the User that has the USER_ID specified in the POST body. If the PUT body USER_ID *is* specified, the logged in user must have the Role canCreateAccount. Once created, the Account will be owned by the User specified by USER_ID. If the PUT body USER_ID is *not* specified, the account will be owned by the logged in User. The 'product_code' field SHOULD be a product_code f
- **Route Pattern**: `"banks" :: BankId(bankId) :: "accounts" :: AccountId(accountId) :: Nil`
- **Tags**: Account, Onboarding
- **Required Roles**: CreateAccount
- **Path Parameters**: BANK_ID, ACCOUNT_ID
- **Error Codes**: UserNotFound, BankNotFound, UnknownError
- **Source Files**: APIMethods500.scala

#### `PUT /banks/BANK_ID/customer-account-links/CUSTOMER_ACCOUNT_LINK_ID`

- **Endpoint Name**: `updateCustomerAccountLinkById`
- **Summary**: Update Customer Account Link by Id
- **Description**: Update Customer Account Link by CUSTOMER_ACCOUNT_LINK_ID ${userAuthenticationMessage(true)} 
- **Route Pattern**: `"banks" :: BankId(bankId) :: "customer-account-links" :: customerAccountLinkId :: Nil`
- **Tags**: Customer
- **Required Roles**: UpdateCustomerAccountLink
- **Path Parameters**: BANK_ID, CUSTOMER_ACCOUNT_LINK_ID
- **Error Codes**: BankNotFound, UnknownError
- **Source Files**: APIMethods500.scala

#### `PUT /banks/BANK_ID/products/PRODUCT_CODE`

- **Endpoint Name**: `createProduct`
- **Summary**: Create Product
- **Description**: Create or Update Product for the Bank. Typical Super Family values / Asset classes are: Debt Equity FX Commodity Derivative $productHiearchyAndCollectionNote ${userAuthenticationMessage(true) } 
- **Route Pattern**: `"banks" :: BankId(bankId) :: "products" :: ProductCode(productCode) :: Nil`
- **Tags**: Product
- **Required Roles**: CreateProductAtAnyBank, CreateProduct
- **Path Parameters**: BANK_ID, PRODUCT_CODE
- **Error Codes**: BankNotFound, UnknownError
- **Source Files**: APIMethods500.scala

#### `PUT /system-views/VIEW_ID`

- **Endpoint Name**: `updateSystemView`
- **Summary**: Update System View
- **Description**: Update an existing view on a bank account ${userAuthenticationMessage(true)} and the user needs to have access to the owner view. The json sent is the same as during view creation (above), with one difference: the 'name' field of a view is not editable (it is only set when a view is created)
- **Route Pattern**: `"system-views" :: viewId :: Nil`
- **Tags**: SystemView
- **Required Roles**: UpdateSystemView
- **Path Parameters**: VIEW_ID
- **Request Body**: UpdateViewJSON
- **Error Codes**: BankAccountNotFound, UnknownError
- **Source Files**: APIMethods500.scala

### DELETE Endpoints (1)

#### `DELETE /banks/BANK_ID/customer-account-links/CUSTOMER_ACCOUNT_LINK_ID`

- **Endpoint Name**: `deleteCustomerAccountLinkById`
- **Summary**: Delete Customer Account Link
- **Description**: Delete Customer Account Link by CUSTOMER_ACCOUNT_LINK_ID ${userAuthenticationMessage(true)} 
- **Route Pattern**: `"banks" :: BankId(bankId) :: "customer-account-links" :: customerAccountLinkId :: Nil`
- **Tags**: Customer
- **Required Roles**: DeleteCustomerAccountLink
- **Path Parameters**: BANK_ID, CUSTOMER_ACCOUNT_LINK_ID
- **Error Codes**: BankNotFound, UnknownError
- **Source Files**: APIMethods500.scala


---

## API Context: v5_1_0

**Total Endpoints**: 102

### GET Endpoints (52)

#### `GET /banks/BANK_ID/accounts/ACCOUNT_ID/VIEW_ID/transaction-requests`

- **Endpoint Name**: `getTransactionRequests`
- **Summary**: Get Transaction Requests.
- **Description**: Returns transaction requests for account specified by ACCOUNT_ID at bank specified by BANK_ID. The VIEW_ID specified must be 'owner' and the user must have access to this view. Version 2.0.0 now returns charge information. Transaction Requests serve to initiate transactions that may or may not proceed. They contain information including: * Transaction Request Id * Type * Status (INITIATED, COMPLETED) * Challenge (in order to confirm the request) * From Bank / Account * Details including Currency
- **Route Pattern**: `"banks" :: BankId(bankId) :: "accounts" :: AccountId(accountId) :: ViewId(viewId) :: "transaction-requests" :: Nil`
- **Path Parameters**: BANK_ID, ACCOUNT_ID, VIEW_ID
- **Request Body**: transactionRequestWithChargeJSONs210
- **Response Body**: transactionRequestWithChargeJSONs210
- **Error Codes**: BankNotFound, BankAccountNotFound, UnknownError
- **Source Files**: APIMethods510.scala

#### `GET /banks/BANK_ID/accounts/ACCOUNT_ID/balances`

- **Endpoint Name**: `getAllBankAccountBalances`
- **Summary**: Get All Bank Account Balances
- **Description**: Get all Balances for a Bank Account. ${userAuthenticationMessage(true)} 
- **Route Pattern**: `"banks" :: BankId(bankId):: "accounts" :: AccountId(accountId) :: "balances" :: Nil`
- **Tags**: Account, Balance
- **Path Parameters**: BANK_ID, ACCOUNT_ID
- **Error Codes**: UnknownError
- **Source Files**: APIMethods510.scala

#### `GET /banks/BANK_ID/accounts/ACCOUNT_ID/balances/BALANCE_ID`

- **Endpoint Name**: `getBankAccountBalanceById`
- **Summary**: Get Bank Account Balance By ID
- **Description**: Get a specific Bank Account Balance by its BALANCE_ID. ${userAuthenticationMessage(true)} 
- **Route Pattern**: `"banks" :: BankId(bankId):: "accounts" :: AccountId(accountId) :: "balances" :: BalanceId(balanceId) :: Nil`
- **Tags**: Account, Balance
- **Path Parameters**: BANK_ID, BALANCE_ID, ACCOUNT_ID
- **Error Codes**: UnknownError
- **Source Files**: APIMethods510.scala

#### `GET /banks/BANK_ID/accounts/ACCOUNT_ID/views/VIEW_ID`

- **Endpoint Name**: `getCoreAccountByIdThroughView`
- **Summary**: Get Account by Id (Core) through the VIEW_ID
- **Description**: Information returned about the account through VIEW_ID : 
- **Route Pattern**: `"banks" :: BankId(bankId) :: "accounts" :: AccountId(accountId) :: "views" :: ViewId(viewId) :: Nil`
- **Path Parameters**: BANK_ID, ACCOUNT_ID, VIEW_ID
- **Source Files**: APIMethods510.scala

#### `GET /banks/BANK_ID/accounts/ACCOUNT_ID/views/VIEW_ID/balances`

- **Endpoint Name**: `getBankAccountBalances`
- **Summary**: Get Account Balances by BANK_ID and ACCOUNT_ID through the VIEW_ID
- **Description**: Get the Balances for the Account specified by BANK_ID and ACCOUNT_ID through the VIEW_ID.
- **Route Pattern**: `"banks" :: BankId(bankId) :: "accounts" :: AccountId(accountId)  :: "views" :: ViewId(viewId) :: "balances" :: Nil`
- **Path Parameters**: BANK_ID, ACCOUNT_ID, VIEW_ID
- **Source Files**: APIMethods510.scala

#### `GET /banks/BANK_ID/accounts/ACCOUNT_ID/views/VIEW_ID/counterparties/COUNTERPARTY_ID/limit-status`

- **Endpoint Name**: `getCounterpartyLimitStatus`
- **Summary**: Get Counterparty Limit Status
- **Description**: Get Counterparty Limit Status.
- **Route Pattern**: `"banks" :: BankId(bankId) :: "accounts" :: AccountId(accountId) :: "views" :: ViewId(viewId) ::"counterparties" :: CounterpartyId(counterpartyId) ::"limit-status" :: Nil`
- **Path Parameters**: BANK_ID, COUNTERPARTY_ID, ACCOUNT_ID, VIEW_ID
- **Error Codes**: BankNotFound, BankAccountNotFound, CounterpartyNotFound, UnknownError
- **Source Files**: APIMethods510.scala

#### `GET /banks/BANK_ID/accounts/ACCOUNT_ID/views/VIEW_ID/counterparties/COUNTERPARTY_ID/limits`

- **Endpoint Name**: `getCounterpartyLimit`
- **Summary**: Get Counterparty Limit
- **Description**: Get Counterparty Limit.
- **Route Pattern**: `"banks" :: BankId(bankId) :: "accounts" :: AccountId(accountId) :: "views" :: ViewId(viewId) ::"counterparties" :: CounterpartyId(counterpartyId) ::"limits" :: Nil`
- **Path Parameters**: BANK_ID, COUNTERPARTY_ID, ACCOUNT_ID, VIEW_ID
- **Error Codes**: BankNotFound, BankAccountNotFound, CounterpartyNotFound, UnknownError
- **Source Files**: APIMethods510.scala

#### `GET /banks/BANK_ID/accounts/ACCOUNT_ID/views/VIEW_ID/target-views/TARGET_VIEW_ID`

- **Endpoint Name**: `getCustomView`
- **Summary**: Get Custom View
- **Description**: #Views Views in Open Bank Project provide a mechanism for fine grained access control and delegation to Accounts and Transactions. Account holders use the 'owner' view by default. Delegated access is made through other views for example 'accountants', 'share-holders' or 'tagging-application'. Views can be created via the API and each view has a list of entitlements. Views on accounts and transactions filter the underlying data to redact certain fields for certain users. For instance the balance 
- **Path Parameters**: BANK_ID, ACCOUNT_ID, VIEW_ID, TARGET_VIEW_ID
- **Source Files**: APIMethods510.scala

#### `GET /banks/BANK_ID/agents`

- **Endpoint Name**: `getAgents`
- **Summary**: Get Agents at Bank
- **Description**: Get Agents at Bank. ${userAuthenticationMessage(false)} ${urlParametersDocument(true, true)} 
- **Route Pattern**: `"banks" :: BankId(bankId) :: "agents" :: Nil`
- **Tags**: Account
- **Required Roles**: GetAtmAttribute, GetAtmAttributeAtAnyBank
- **Path Parameters**: BANK_ID
- **Error Codes**: BankNotFound, AgentsNotFound, UnknownError
- **Source Files**: APIMethods510.scala

#### `GET /banks/BANK_ID/agents/AGENT_ID`

- **Endpoint Name**: `getAgent`
- **Summary**: Get Agent
- **Description**: Get Agent. ${userAuthenticationMessage(true)} 
- **Route Pattern**: `"banks" :: BankId(bankId) :: "agents" :: agentId :: Nil`
- **Tags**: Account
- **Path Parameters**: BANK_ID, AGENT_ID
- **Error Codes**: AgentNotFound, BankNotFound, AgentAccountLinkNotFound, UnknownError
- **Source Files**: APIMethods510.scala

#### `GET /banks/BANK_ID/atms`

- **Endpoint Name**: `getAtms`
- **Summary**: Get Bank ATMS
- **Description**: Returns information about ATMs for a single bank specified by BANK_ID including: * Address * Geo Location * License the data under this endpoint is released under Pagination: By default, 100 records are returned. You can use the url query parameters *limit* and *offset* for pagination ${userAuthenticationMessage(!getAtmsIsPublic)}
- **Route Pattern**: `"banks" :: BankId(bankId) :: "atms" :: Nil`
- **Tags**: ATM
- **Path Parameters**: BANK_ID
- **Error Codes**: BankNotFound, UnknownError
- **Source Files**: APIMethods510.scala

#### `GET /banks/BANK_ID/atms/ATM_ID`

- **Endpoint Name**: `getAtm`
- **Summary**: Get Bank ATM
- **Description**: Returns information about ATM for a single bank specified by BANK_ID and ATM_ID including: * Address * Geo Location * License the data under this endpoint is released under * ATM Attributes ${userAuthenticationMessage(!getAtmsIsPublic)}
- **Route Pattern**: `"banks" :: BankId(bankId) :: "atms" :: AtmId(atmId) :: Nil`
- **Tags**: ATM
- **Path Parameters**: BANK_ID, ATM_ID
- **Error Codes**: BankNotFound, AtmNotFound, UnknownError
- **Source Files**: APIMethods510.scala

#### `GET /banks/BANK_ID/atms/ATM_ID/attributes`

- **Endpoint Name**: `getAtmAttributes`
- **Summary**: Get ATM Attributes
- **Description**: Get ATM Attributes ${userAuthenticationMessage(true)} 
- **Route Pattern**: `"banks" :: BankId(bankId) :: "atms" :: AtmId(atmId) :: "attributes" :: Nil`
- **Tags**: ATM
- **Required Roles**: GetAtmAttribute, GetAtmAttributeAtAnyBank
- **Path Parameters**: BANK_ID, ATM_ID
- **Error Codes**: BankNotFound, UnknownError
- **Source Files**: APIMethods510.scala

#### `GET /banks/BANK_ID/atms/ATM_ID/attributes/ATM_ATTRIBUTE_ID`

- **Endpoint Name**: `getAtmAttribute`
- **Summary**: Get ATM Attribute By ATM_ATTRIBUTE_ID
- **Description**: Get ATM Attribute By ATM_ATTRIBUTE_ID ${userAuthenticationMessage(true)} 
- **Route Pattern**: `"banks" :: BankId(bankId) :: "atms" :: AtmId(atmId) :: "attributes" :: atmAttributeId :: Nil`
- **Tags**: ATM
- **Required Roles**: GetAtmAttribute, GetAtmAttributeAtAnyBank
- **Path Parameters**: BANK_ID, ATM_ID, ATM_ATTRIBUTE_ID
- **Error Codes**: BankNotFound, UnknownError
- **Source Files**: APIMethods510.scala

#### `GET /banks/BANK_ID/balances`

- **Endpoint Name**: `getBankAccountsBalances`
- **Summary**: Get Account Balances by BANK_ID
- **Description**: Get the Balances for the Account specified by BANK_ID.
- **Route Pattern**: `"banks" :: BankId(bankId) :: "balances" :: Nil`
- **Path Parameters**: BANK_ID
- **Source Files**: APIMethods510.scala

#### `GET /banks/BANK_ID/currencies`

- **Endpoint Name**: `getCurrenciesAtBank`
- **Summary**: Get Currencies at a Bank
- **Description**: Get Currencies specified by BANK_ID 
- **Route Pattern**: `"banks" :: BankId(bankId) :: "currencies" :: Nil`
- **Tags**: Fx
- **Path Parameters**: BANK_ID
- **Error Codes**: UnknownError
- **Source Files**: APIMethods510.scala

#### `GET /banks/BANK_ID/my/consents`

- **Endpoint Name**: `getMyConsentsByBank`
- **Summary**: Get My Consents at Bank
- **Description**:  This endpoint gets the Consents created by a current User. ${userAuthenticationMessage(true)} 
- **Route Pattern**: `"banks" :: BankId(bankId) :: "my" :: "consents" :: Nil`
- **Tags**: Psd2, Consent, PSD2AIS
- **Path Parameters**: BANK_ID
- **Error Codes**: BankNotFound, UnknownError
- **Source Files**: APIMethods510.scala

#### `GET /banks/BANK_ID/views/VIEW_ID/balances`

- **Endpoint Name**: `getBankAccountsBalancesThroughView`
- **Summary**: Get Account Balances by BANK_ID through the VIEW_ID
- **Description**: Get the Balances for the Account specified by BANK_ID.
- **Route Pattern**: `"banks" :: BankId(bankId) :: "views" :: ViewId(viewId) :: "balances" :: Nil`
- **Path Parameters**: BANK_ID, VIEW_ID
- **Source Files**: APIMethods510.scala

#### `GET /consumer/current/consents/CONSENT_ID`

- **Endpoint Name**: `getConsentByConsentIdViaConsumer`
- **Summary**: Get Consent By Consent Id via Consumer
- **Description**:  This endpoint gets the Consent By consent id. ${userAuthenticationMessage(true)} 
- **Route Pattern**: `"consumer" :: "current"  :: "consents" :: consentId :: Nil`
- **Tags**: Psd2, Consent, PSD2AIS
- **Path Parameters**: CONSENT_ID
- **Error Codes**: UnknownError
- **Source Files**: APIMethods510.scala

#### `GET /management/aggregate-metrics`

- **Endpoint Name**: `getAggregateMetrics`
- **Summary**: Get Aggregate Metrics
- **Description**: Returns aggregate metrics on api usage eg. total count, response time (in ms), etc. Should be able to filter on the following fields eg: /management/aggregate-metrics?from_date=$DateWithMsExampleString&to_date=$DateWithMsExampleString&consumer_id=5 &user_id=66214b8e-259e-44ad-8868-3eb47be70646&implemented_by_partial_function=getTransactionsForBankAccount &implemented_in_version=v3.0.0&url=/obp/v3.0.0/banks/gh.29.uk/accounts/8ca8a7e4-6d02-48e3-a029-0b2bf89de9f0/owner/transactions &verb=GET&anon=f
- **Route Pattern**: `"management" :: "aggregate-metrics" :: Nil`
- **Source Files**: APIMethods510.scala

#### `GET /management/api-collections`

- **Endpoint Name**: `getAllApiCollections`
- **Summary**: Get All API Collections
- **Description**: Get All API Collections. ${userAuthenticationMessage(true)} 
- **Route Pattern**: `"management" :: "api-collections" :: Nil`
- **Tags**: ApiCollection
- **Error Codes**: UnknownError
- **Source Files**: APIMethods510.scala

#### `GET /management/consents`

- **Endpoint Name**: `getConsents`
- **Summary**: Get Consents
- **Description**:  This endpoint gets the Consents. ${userAuthenticationMessage(true)} 1 limit (for pagination: defaults to 50) eg:limit=200 2 offset (for pagination: zero index, defaults to 0) eg: offset=10 3 consumer_id (ignore if omitted) 4 consent_id (ignore if omitted) 5 user_id (ignore if omitted) 6 status (ignore if omitted) 7 bank_id (ignore if omitted) eg:/management/consents?consumer_id=78&limit=10&offset=10 
- **Route Pattern**: `"management" :: "consents" :: Nil`
- **Tags**: Psd2, Consent, PSD2AIS
- **Required Roles**: GetConsentsAtAnyBank
- **Error Codes**: BankNotFound, UnknownError
- **Source Files**: APIMethods510.scala

#### `GET /management/consents/banks/BANK_ID`

- **Endpoint Name**: `getConsentsAtBank`
- **Summary**: Get Consents at Bank
- **Description**:  This endpoint gets the Consents at Bank by BANK_ID. ${userAuthenticationMessage(true)} 1 limit (for pagination: defaults to 50) eg:limit=200 2 offset (for pagination: zero index, defaults to 0) eg: offset=10 3 consumer_id (ignore if omitted) 4 user_id (ignore if omitted) 5 status (ignore if omitted) eg: /management/consents/banks/BANK_ID?&consumer_id=78&limit=10&offset=10 
- **Route Pattern**: `"management" :: "consents" :: "banks" :: BankId(bankId) :: Nil`
- **Tags**: Psd2, Consent, PSD2AIS
- **Required Roles**: GetConsentsAtAnyBank, GetConsentsAtOneBank
- **Path Parameters**: BANK_ID
- **Error Codes**: BankNotFound, UnknownError
- **Source Files**: APIMethods510.scala

#### `GET /management/consumers`

- **Endpoint Name**: `getConsumers`
- **Summary**: Get Consumers
- **Description**: Get the all Consumers. ${userAuthenticationMessage(true)} ${urlParametersDocument(true, true)} 
- **Route Pattern**: `"management" :: "consumers" :: Nil`
- **Tags**: Consumer
- **Required Roles**: GetConsumers
- **Error Codes**: UnknownError
- **Source Files**: APIMethods510.scala

#### `GET /management/consumers/CONSUMER_ID`

- **Endpoint Name**: `getConsumer`
- **Summary**: Get Consumer
- **Description**: Get the Consumer specified by CONSUMER_ID. 
- **Route Pattern**: `"management" :: "consumers" :: consumerId :: Nil`
- **Tags**: Consumer
- **Required Roles**: GetConsumers
- **Path Parameters**: CONSUMER_ID
- **Request Body**: consumerJSON
- **Error Codes**: ConsumerNotFound, UnknownError
- **Source Files**: APIMethods510.scala

#### `GET /management/metrics`

- **Endpoint Name**: `getMetrics`
- **Summary**: Get Metrics
- **Description**: Get API metrics rows. These are records of each REST API call. require CanReadMetrics role Filters Part 1.*filtering* (no wilde cards etc.) parameters to GET /management/metrics You can filter by the following fields by applying url parameters eg: /management/metrics?from_date=$DateWithMsExampleString&to_date=$DateWithMsExampleString&limit=50&offset=2 1 from_date e.g.:from_date=$DateWithMsExampleString Defaults to the Unix Epoch i.e. ${theEpochTime} 2 to_date e.g.:to_date=$DateWithMsExampleStrin
- **Route Pattern**: `"management" :: "metrics" :: Nil`
- **Source Files**: APIMethods510.scala

#### `GET /management/system/integrity/account-access-unique-index-1-check`

- **Endpoint Name**: `accountAccessUniqueIndexCheck`
- **Summary**: Check Unique Index at Account Access
- **Description**: Check unique index at account access table. ${userAuthenticationMessage(true)} 
- **Route Pattern**: `"management" :: "system" :: "integrity" :: "account-access-unique-index-1-check" :: Nil`
- **Tags**: SystemIntegrity
- **Error Codes**: UnknownError
- **Source Files**: APIMethods510.scala

#### `GET /management/system/integrity/banks/BANK_ID/account-currency-check`

- **Endpoint Name**: `accountCurrencyCheck`
- **Summary**: Check for Sensible Currencies
- **Description**: Check for sensible currencies at Bank Account model ${userAuthenticationMessage(true)} 
- **Route Pattern**: `"management" :: "system" :: "integrity"  :: "banks" :: BankId(bankId) :: "account-currency-check" :: Nil`
- **Tags**: SystemIntegrity
- **Path Parameters**: BANK_ID
- **Error Codes**: UnknownError
- **Source Files**: APIMethods510.scala

#### `GET /management/system/integrity/banks/BANK_ID/orphaned-account-check`

- **Endpoint Name**: `orphanedAccountCheck`
- **Summary**: Check for Orphaned Accounts
- **Description**: Check for orphaned accounts at Bank Account model ${userAuthenticationMessage(true)} 
- **Route Pattern**: `"management" :: "system" :: "integrity"  :: "banks" :: BankId(bankId) :: "orphaned-account-check" :: Nil`
- **Tags**: SystemIntegrity
- **Path Parameters**: BANK_ID
- **Error Codes**: UnknownError
- **Source Files**: APIMethods510.scala

#### `GET /management/system/integrity/custom-view-names-check`

- **Endpoint Name**: `customViewNamesCheck`
- **Summary**: Check Custom View Names
- **Description**: Check custom view names. ${userAuthenticationMessage(true)} 
- **Route Pattern**: `"management" :: "system" :: "integrity" :: "custom-view-names-check" :: Nil`
- **Tags**: SystemIntegrity
- **Error Codes**: UnknownError
- **Source Files**: APIMethods510.scala

#### `GET /management/system/integrity/system-view-names-check`

- **Endpoint Name**: `systemViewNamesCheck`
- **Summary**: Check System View Names
- **Description**: Check system view names. ${userAuthenticationMessage(true)} 
- **Route Pattern**: `"management" :: "system" :: "integrity" :: "system-view-names-check" :: Nil`
- **Tags**: SystemIntegrity
- **Error Codes**: UnknownError
- **Source Files**: APIMethods510.scala

#### `GET /management/transaction-requests/TRANSACTION_REQUEST_ID`

- **Endpoint Name**: `getTransactionRequestById`
- **Summary**: Get Transaction Request by ID.
- **Description**: Returns transaction request for transaction specified by TRANSACTION_REQUEST_ID. 
- **Route Pattern**: `"management" :: "transaction-requests" :: TransactionRequestId(requestId) :: Nil`
- **Tags**: Psd2, PSD2PIS, TransactionRequest
- **Required Roles**: GetTransactionRequestAtAnyBank
- **Path Parameters**: TRANSACTION_REQUEST_ID
- **Request Body**: transactionRequestWithChargeJSON210
- **Response Body**: transactionRequestWithChargeJSON210
- **Error Codes**: UnknownError
- **Source Files**: APIMethods510.scala

#### `GET /my/consents`

- **Endpoint Name**: `getMyConsents`
- **Summary**: Get My Consents
- **Description**:  This endpoint gets the Consents created by a current User. ${userAuthenticationMessage(true)} 
- **Route Pattern**: `"my" :: "consents" :: Nil`
- **Tags**: Psd2, Consent, PSD2AIS
- **Error Codes**: BankNotFound, UnknownError
- **Source Files**: APIMethods510.scala

#### `GET /my/mtls/certificate/current`

- **Endpoint Name**: `mtlsClientCertificateInfo`
- **Summary**: Provide client's certificate info of a current call
- **Description**:  Provide client's certificate info of a current call specified by PSD2-CERT value at Request Header ${userAuthenticationMessage(true)} 
- **Route Pattern**: `"my" :: "mtls" :: "certificate" :: "current" :: Nil`
- **Tags**: Psd2, Consent, PSD2AIS
- **Error Codes**: BankNotFound, UnknownError
- **Source Files**: APIMethods510.scala

#### `GET /regulated-entities`

- **Endpoint Name**: `regulatedEntities`
- **Summary**: Get Regulated Entities
- **Description**: Returns information about: * Regulated Entities
- **Route Pattern**: `"regulated-entities" :: regulatedEntityId :: Nil`
- **Error Codes**: UnknownError
- **Source Files**: APIMethods510.scala

#### `GET /regulated-entities/REGULATED_ENTITY_ID`

- **Endpoint Name**: `getRegulatedEntityById`
- **Summary**: Get Regulated Entity
- **Description**: Get Regulated Entity By REGULATED_ENTITY_ID
- **Route Pattern**: `"regulated-entities" :: regulatedEntityId :: Nil`
- **Tags**: Api, Directory
- **Required Roles**: CreateRegulatedEntity
- **Path Parameters**: REGULATED_ENTITY_ID
- **Error Codes**: UnknownError
- **Source Files**: APIMethods510.scala

#### `GET /regulated-entities/REGULATED_ENTITY_ID/attributes`

- **Endpoint Name**: `getAllRegulatedEntityAttributes`
- **Summary**: Get All Regulated Entity Attributes
- **Description**:  Get all attributes for the specified Regulated Entity. ${userAuthenticationMessage(true)} 
- **Route Pattern**: `"regulated-entities" :: RegulatedEntityId(entityId) :: "attributes" :: Nil`
- **Tags**: Api, Directory
- **Required Roles**: GetRegulatedEntityAttributes
- **Path Parameters**: REGULATED_ENTITY_ID
- **Error Codes**: UnknownError
- **Source Files**: APIMethods510.scala

#### `GET /regulated-entities/REGULATED_ENTITY_ID/attributes/REGULATED_ENTITY_ATTRIBUTE_ID`

- **Endpoint Name**: `getRegulatedEntityAttributeById`
- **Summary**: Get Regulated Entity Attribute By ID
- **Description**:  Get a specific Regulated Entity Attribute by its REGULATED_ENTITY_ATTRIBUTE_ID. ${userAuthenticationMessage(true)} 
- **Route Pattern**: `"regulated-entities" :: entityId :: "attributes" :: attributeId :: Nil`
- **Tags**: Api, Directory
- **Required Roles**: GetRegulatedEntityAttribute
- **Path Parameters**: REGULATED_ENTITY_ID, REGULATED_ENTITY_ATTRIBUTE_ID
- **Error Codes**: UnknownError
- **Source Files**: APIMethods510.scala

#### `GET /root`

- **Endpoint Name**: `root`
- **Summary**: Get API Info (root)
- **Description**: Returns information about: * API version * Hosted by information * Hosted at information * Energy source information * Git Commit
- **Route Pattern**: `"ui" :: "suggested-session-timeout" :: Nil`
- **Request Body**: getApiInfoJSON
- **Error Codes**: UnknownError
- **Source Files**: APIMethods510.scala

#### `GET /tags`

- **Endpoint Name**: `getApiTags`
- **Summary**: Get API Tags
- **Description**: Get API TagsGet API Tags ${userAuthenticationMessage(false)} 
- **Route Pattern**: `case "tags" ::  Nil JsonGet`
- **Tags**: Api
- **Error Codes**: UnknownError
- **Source Files**: APIMethods510.scala

#### `GET /ui/suggested-session-timeout`

- **Endpoint Name**: `suggestedSessionTimeout`
- **Summary**: Get Suggested Session Timeout
- **Description**: Returns information about: * Suggested session timeout in case of a user inactivity
- **Route Pattern**: `"ui" :: "suggested-session-timeout" :: Nil`
- **Tags**: Api
- **Error Codes**: UnknownError
- **Source Files**: APIMethods510.scala

#### `GET /user/current/consents/CONSENT_ID`

- **Endpoint Name**: `getConsentByConsentId`
- **Summary**: Get Consent By Consent Id via User
- **Description**:  This endpoint gets the Consent By consent id. ${userAuthenticationMessage(true)} 
- **Route Pattern**: `"user" :: "current" :: "consents" :: consentId :: Nil`
- **Tags**: Psd2, Consent, PSD2AIS
- **Path Parameters**: CONSENT_ID
- **Error Codes**: UnknownError
- **Source Files**: APIMethods510.scala

#### `GET /users/PROVIDER/USERNAME/lock-status`

- **Endpoint Name**: `getUserLockStatus`
- **Summary**: Get User Lock Status
- **Description**:  Get User Login Status. ${userAuthenticationMessage(true)} 
- **Tags**: User
- **Required Roles**: ReadUserLockedStatus
- **Path Parameters**: PROVIDER, USERNAME
- **Error Codes**: UserNotFound, UnknownError
- **Source Files**: APIMethods510.scala

#### `GET /users/USER_ID/account-access`

- **Endpoint Name**: `getAccountAccessByUserId`
- **Summary**: Get Account Access by USER_ID
- **Description**: Get Account Access by USER_ID ${userAuthenticationMessage(true)} 
- **Route Pattern**: `"users" :: userId :: "account-access" :: Nil`
- **Tags**: Account
- **Required Roles**: SeeAccountAccessForAnyUser
- **Path Parameters**: USER_ID
- **Error Codes**: UserNotFound, UnknownError
- **Source Files**: APIMethods510.scala

#### `GET /users/USER_ID/accounts-held`

- **Endpoint Name**: `getAccountsHeldByUser`
- **Summary**: Get Accounts Held By User
- **Description**: Get Accounts held by the User if even the User has not been assigned the owner View yet. Can be used to onboard the account to the API - since all other account and transaction endpoints require views to be assigned. ${accountTypeFilterText("/users/USER_ID/accounts-held")} 
- **Route Pattern**: `"users" :: userId :: "accounts-held" :: Nil`
- **Tags**: Account
- **Required Roles**: GetAccountsHeldAtAnyBank
- **Path Parameters**: USER_ID
- **Request Body**: createCoreAccountsByCoreAccountsJSON
- **Error Codes**: UserNotFound, BankNotFound, UnknownError
- **Source Files**: APIMethods510.scala

#### `GET /users/USER_ID/banks/BANK_ID/accounts-held`

- **Endpoint Name**: `getAccountsHeldByUserAtBank`
- **Summary**: Get Accounts Held By User
- **Description**: Get Accounts held by the User if even the User has not been assigned the owner View yet. Can be used to onboard the account to the API - since all other account and transaction endpoints require views to be assigned. ${accountTypeFilterText("/users/USER_ID/banks/BANK_ID/accounts-held")} 
- **Route Pattern**: `"users" :: userId :: "banks" :: BankId(bankId) :: "accounts-held" :: Nil`
- **Tags**: Account
- **Required Roles**: GetAccountsHeldAtOneBank, GetAccountsHeldAtAnyBank
- **Path Parameters**: BANK_ID, USER_ID
- **Request Body**: createCoreAccountsByCoreAccountsJSON
- **Error Codes**: UserNotFound, BankNotFound, UnknownError
- **Source Files**: APIMethods510.scala

#### `GET /users/USER_ID/entitlements-and-permissions`

- **Endpoint Name**: `getEntitlementsAndPermissions`
- **Summary**: Get Entitlements and Permissions for a User
- **Description**:  
- **Route Pattern**: `"users" :: userId :: "entitlements-and-permissions" :: Nil`
- **Tags**: Entitlement, Role, User
- **Required Roles**: GetEntitlementsForAnyUserAtAnyBank
- **Path Parameters**: USER_ID
- **Request Body**: createUserInfoJSON
- **Error Codes**: UserNotFound, UnknownError
- **Source Files**: APIMethods510.scala

#### `GET /users/USER_ID/non-personal/attributes`

- **Endpoint Name**: `getNonPersonalUserAttributes`
- **Summary**: Get Non Personal User Attributes
- **Description**: Get Non Personal User Attribute for a user specified by USER_ID ${userAuthenticationMessage(true)} 
- **Route Pattern**: `"users" :: userId :: "non-personal" ::"attributes" :: Nil`
- **Tags**: User
- **Required Roles**: GetNonPersonalUserAttributes
- **Path Parameters**: USER_ID
- **Error Codes**: UnknownError
- **Source Files**: APIMethods510.scala

#### `GET /users/current/customers/customer_ids`

- **Endpoint Name**: `getCustomersForUserIdsOnly`
- **Summary**: Get Customers for Current User (IDs only)
- **Description**: Gets all Customers Ids that are linked to a User. ${userAuthenticationMessage(true)} 
- **Route Pattern**: `"users" :: "current" :: "customers" :: "customer_ids" :: Nil`
- **Tags**: Customer, User
- **Path Parameters**: _
- **Error Codes**: UserCustomerLinksNotFound, UnknownError
- **Source Files**: APIMethods510.scala

#### `GET /users/provider/PROVIDER/username/USERNAME`

- **Endpoint Name**: `getUserByProviderAndUsername`
- **Summary**: Get User by USERNAME
- **Description**: Get user by PROVIDER and USERNAME ${userAuthenticationMessage(true)} CanGetAnyUser entitlement is required, 
- **Route Pattern**: `"users" :: "provider" :: provider :: "username" :: username :: Nil`
- **Tags**: User
- **Required Roles**: GetAnyUser
- **Path Parameters**: PROVIDER, USERNAME
- **Request Body**: createUserInfoJSON
- **Error Codes**: UserNotFound, UnknownError
- **Source Files**: APIMethods510.scala

#### `GET /waiting-for-godot`

- **Endpoint Name**: `waitingForGodot`
- **Summary**: Waiting For Godot
- **Description**: Waiting For Godot Uses query parameter "sleep" in milliseconds. For instance: .../waiting-for-godot?sleep=50 means postpone response in 50 milliseconds. 
- **Route Pattern**: `case "waiting-for-godot" :: Nil JsonGet`
- **Error Codes**: UnknownError
- **Source Files**: APIMethods510.scala

#### `GET /webui-props`

- **Endpoint Name**: `getWebUiProps`
- **Summary**: Get WebUiProps
- **Description**:  Get the all WebUiProps key values, those props key with "webui_" can be stored in DB, this endpoint get all from DB. url query parameter: active: It must be a boolean string. and If active = true, it will show combination of explicit (inserted) + implicit (default) method_routings. eg: ${getObpApiRoot}/v5.1.0/webui-props ${getObpApiRoot}/v5.1.0/webui-props?active=true 
- **Source Files**: APIMethods510.scala

### POST Endpoints (21)

#### `POST /banks/BANK_ID/accounts/ACCOUNT_ID/balances`

- **Endpoint Name**: `createBankAccountBalance`
- **Summary**: Create Bank Account Balance
- **Description**: Create a new Balance for a Bank Account. ${userAuthenticationMessage(true)} 
- **Route Pattern**: `"banks" :: BankId(bankId):: "accounts" :: AccountId(accountId) :: "balances" :: Nil`
- **Tags**: Account, Balance
- **Required Roles**: CreateBankAccountBalance
- **Path Parameters**: BANK_ID, ACCOUNT_ID
- **Error Codes**: UnknownError
- **Source Files**: APIMethods510.scala

#### `POST /banks/BANK_ID/accounts/ACCOUNT_ID/views/VIEW_ID/account-access/grant`

- **Endpoint Name**: `grantUserAccessToViewById`
- **Summary**: Grant User access to View
- **Description**: Grants the User identified by USER_ID access to the view identified. ${userAuthenticationMessage(true)} and the user needs to be account holder. 
- **Route Pattern**: `"banks" :: BankId(bankId) :: "accounts" :: AccountId(accountId) ::"views":: ViewId(viewId):: "account-access" :: "grant" :: Nil`
- **Tags**: Account, User, AccountAccess, View, OwnerRequired
- **Path Parameters**: BANK_ID, ACCOUNT_ID, VIEW_ID
- **Error Codes**: BankNotFound, BankAccountNotFound, UserNotFound, ViewNotFound, UnknownError, SystemViewNotFound
- **Source Files**: APIMethods510.scala

#### `POST /banks/BANK_ID/accounts/ACCOUNT_ID/views/VIEW_ID/account-access/revoke`

- **Endpoint Name**: `revokeUserAccessToViewById`
- **Summary**: Revoke User access to View
- **Description**: Revoke the User identified by USER_ID access to the view identified. ${userAuthenticationMessage(true)}. 
- **Route Pattern**: `"banks" :: BankId(bankId) :: "accounts" :: AccountId(accountId) :: "views" ::ViewId(viewId) :: "account-access" :: "revoke" :: Nil`
- **Tags**: Account, User, AccountAccess, View, OwnerRequired
- **Path Parameters**: BANK_ID, ACCOUNT_ID, VIEW_ID
- **Error Codes**: BankNotFound, BankAccountNotFound, UserNotFound, ViewNotFound, UnknownError, SystemViewNotFound
- **Source Files**: APIMethods510.scala

#### `POST /banks/BANK_ID/accounts/ACCOUNT_ID/views/VIEW_ID/counterparties/COUNTERPARTY_ID/limits`

- **Endpoint Name**: `createCounterpartyLimit`
- **Summary**: Create Counterparty Limit
- **Description**: Create limits (for single or recurring payments) for a counterparty specified by the COUNTERPARTY_ID. Using this endpoint, we can attach a limit record to a Counterparty referenced by its counterparty_id (a UUID). For more information on Counterparty Limits, see ${Glossary.getGlossaryItemLink("Counterparty-Limits")} For an introduction to Counterparties in OBP, see ${Glossary.getGlossaryItemLink("Counterparties")} You can automate the process of creating counterparty limits and consents for VRP 
- **Route Pattern**: `"banks" :: BankId(bankId) :: "accounts" :: AccountId(accountId) :: "views" :: ViewId(viewId) ::"counterparties" :: CounterpartyId(counterpartyId) ::"limits" :: Nil`
- **Path Parameters**: BANK_ID, COUNTERPARTY_ID, ACCOUNT_ID, VIEW_ID
- **Error Codes**: BankNotFound, BankAccountNotFound, CounterpartyNotFound, UnknownError
- **Source Files**: APIMethods510.scala

#### `POST /banks/BANK_ID/accounts/ACCOUNT_ID/views/VIEW_ID/target-views`

- **Endpoint Name**: `createCustomView`
- **Summary**: Create Custom View
- **Description**: Create a custom view on bank account ${userAuthenticationMessage(true)} and the user needs to have access to the owner view. The 'alias' field in the JSON can take one of three values: * _public_: to use the public alias if there is one specified for the other account. * _private_: to use the private alias if there is one specified for the other account. * _''(empty string)_: to use no alias; the view shows the real name of the other account. The 'hide_metadata_if_alias_used' field in the JSON c
- **Route Pattern**: `"banks" :: BankId(bankId) :: "accounts" :: AccountId(accountId) :: "views" :: ViewId(viewId) ::"target-views" :: Nil`
- **Path Parameters**: BANK_ID, ACCOUNT_ID, VIEW_ID
- **Error Codes**: BankNotFound, BankAccountNotFound, UnknownError
- **Source Files**: APIMethods510.scala

#### `POST /banks/BANK_ID/accounts/ACCOUNT_ID/views/VIEW_ID/user-account-access`

- **Endpoint Name**: `createUserWithAccountAccessById`
- **Summary**: Create (DAuth) User with Account Access
- **Description**: This endpoint is used as part of the DAuth solution to grant access to account and transaction data to a smart contract on the blockchain. Put the smart contract address in username For provider use "dauth" This endpoint will create the (DAuth) User with username and provider if the User does not already exist. ${userAuthenticationMessage(true)} and the logged in user needs to be account holder. For information about DAuth see below: ${Glossary.getGlossaryItem("DAuth")} 
- **Route Pattern**: `"banks" :: BankId(bankId) :: "accounts" :: AccountId(accountId) :: "views" ::ViewId(viewId) :: "user-account-access" :: Nil`
- **Tags**: Account, User, AccountAccess, DAuth, View, OwnerRequired
- **Path Parameters**: BANK_ID, ACCOUNT_ID, VIEW_ID
- **Source Files**: APIMethods510.scala

#### `POST /banks/BANK_ID/agents`

- **Endpoint Name**: `createAgent`
- **Summary**: Create Agent
- **Description**:  ${userAuthenticationMessage(true)} 
- **Route Pattern**: `"banks" :: BankId(bankId) :: "agents" :: Nil`
- **Tags**: Person, Customer
- **Path Parameters**: BANK_ID
- **Error Codes**: BankNotFound, CreateAgentError, UnknownError
- **Source Files**: APIMethods510.scala

#### `POST /banks/BANK_ID/atms`

- **Endpoint Name**: `createAtm`
- **Summary**: Create ATM
- **Description**: Create ATM.
- **Route Pattern**: `"banks" :: BankId(bankId) :: "atms" :: Nil`
- **Tags**: ATM
- **Required Roles**: CreateAtm, CreateAtmAtAnyBank
- **Path Parameters**: BANK_ID
- **Error Codes**: UnknownError
- **Source Files**: APIMethods510.scala

#### `POST /banks/BANK_ID/atms/ATM_ID/attributes`

- **Endpoint Name**: `createAtmAttribute`
- **Summary**: Create ATM Attribute
- **Description**: Create ATM Attribute The type field must be one of "STRING", "INTEGER", "DOUBLE" or DATE_WITH_DAY" ${userAuthenticationMessage(true)} 
- **Route Pattern**: `"banks" :: BankId(bankId) :: "atms" :: AtmId(atmId) :: "attributes" :: Nil`
- **Tags**: ATM
- **Required Roles**: CreateAtmAttribute, CreateAtmAttributeAtAnyBank
- **Path Parameters**: BANK_ID, ATM_ID
- **Error Codes**: BankNotFound, UnknownError
- **Source Files**: APIMethods510.scala

#### `POST /banks/BANK_ID/customers/legal-name`

- **Endpoint Name**: `getCustomersByLegalName`
- **Summary**: Get Customers by Legal Name
- **Description**: Gets the Customers specified by Legal Name. ${userAuthenticationMessage(true)} 
- **Route Pattern**: `"banks" :: BankId(bankId) :: "customers" :: "legal-name" :: Nil`
- **Tags**: Customer, Kyc
- **Required Roles**: GetCustomer
- **Path Parameters**: BANK_ID
- **Error Codes**: UserCustomerLinksNotFound, UnknownError
- **Source Files**: APIMethods510.scala

#### `POST /consumer/vrp-consent-requests`

- **Endpoint Name**: `createVRPConsentRequest`
- **Summary**: Create Consent Request VRP
- **Description**:  This endpoint is used to begin the process of creating a consent that may be used for Variable Recurring Payments (VRPs). VRPs are useful in situations when a beneficiary needs to be paid different amounts on a regular basis. Once granted, the consent allows its holder to initiate multiple Transaction Requests to the Counterparty defined in this endpoint as long as the Counterparty Limits linked to this particular consent are respected. Client, Consumer or Application Authentication is mandator
- **Route Pattern**: `"consumer" :: "vrp-consent-requests" :: Nil`
- **Source Files**: APIMethods510.scala

#### `POST /dynamic-registration/consumers`

- **Endpoint Name**: `createConsumerDynamicRegistration`
- **Summary**: Create a Consumer(Dynamic Registration)
- **Description**: Create a Consumer (mTLS access). JWT payload: - minimal { "description":"Description" } - full { "description": "Description", "app_name": "Tesobe GmbH", "app_type": "Sofit", "developer_email": "marko@tesobe.com", "redirect_url": "http://localhost:8082" } Please note that JWT must be signed with the counterpart private key of the public key used to establish mTLS 
- **Route Pattern**: `"dynamic-registration" :: "consumers" :: Nil`
- **Tags**: Consumer, Directory
- **Error Codes**: UnknownError
- **Source Files**: APIMethods510.scala

#### `POST /management/consumers`

- **Endpoint Name**: `createConsumer`
- **Summary**: Create a Consumer
- **Description**: Create a Consumer (Authenticated access). 
- **Route Pattern**: `"management" :: "consumers" :: Nil`
- **Tags**: Consumer
- **Required Roles**: CreateConsumer
- **Error Codes**: UnknownError
- **Source Files**: APIMethods510.scala

#### `POST /my/consents/IMPLICIT`

- **Endpoint Name**: `createConsentImplicit`
- **Summary**: Create Consent (IMPLICIT)
- **Description**:  This endpoint starts the process of creating a Consent. The Consent is created in an ${ConsentStatus.INITIATED} state. A One Time Password (OTP) (AKA security challenge) is sent Out of Band (OOB) to the User via the transport defined in SCA_METHOD SCA_METHOD is typically "SMS","EMAIL" or "IMPLICIT". "EMAIL" is used for testing purposes. OBP mapped mode "IMPLICIT" is "EMAIL". Other mode, bank can decide it in the connector method 'getConsentImplicitSCA'. When the Consent is created, OBP (or a ba
- **Path Parameters**: IMPLICIT
- **Source Files**: APIMethods510.scala

#### `POST /my/consumers`

- **Endpoint Name**: `createMyConsumer`
- **Summary**: Create a Consumer
- **Description**: Create a Consumer (Authenticated access). 
- **Route Pattern**: `"my" :: "consumers" :: Nil`
- **Tags**: Consumer
- **Error Codes**: UnknownError
- **Source Files**: APIMethods510.scala

#### `POST /regulated-entities`

- **Endpoint Name**: `createRegulatedEntity`
- **Summary**: Create Regulated Entity
- **Description**: Create Regulated Entity ${userAuthenticationMessage(true)} 
- **Route Pattern**: `case "regulated-entities" :: Nil JsonPost`
- **Tags**: Api, Directory
- **Required Roles**: CreateRegulatedEntity
- **Error Codes**: UnknownError
- **Source Files**: APIMethods510.scala

#### `POST /regulated-entities/REGULATED_ENTITY_ID/attributes`

- **Endpoint Name**: `createRegulatedEntityAttribute`
- **Summary**: Create Regulated Entity Attribute
- **Description**:  Create a new Regulated Entity Attribute for a given REGULATED_ENTITY_ID. The type field must be one of "STRING", "INTEGER", "DOUBLE" or "DATE_WITH_DAY". ${userAuthenticationMessage(true)} 
- **Route Pattern**: `"regulated-entities" :: entityId :: "attributes" :: Nil`
- **Tags**: Api, Directory
- **Required Roles**: CreateRegulatedEntityAttribute
- **Path Parameters**: REGULATED_ENTITY_ID
- **Error Codes**: UnknownError
- **Source Files**: APIMethods510.scala

#### `POST /system-views/VIEW_ID/permissions`

- **Endpoint Name**: `addSystemViewPermission`
- **Summary**: Add Permission to a System View
- **Description**: Add Permission to a System View.
- **Route Pattern**: `"system-views" :: ViewId(viewId) :: "permissions" :: Nil`
- **Tags**: SystemView
- **Required Roles**: CreateSystemViewPermission
- **Path Parameters**: VIEW_ID
- **Request Body**: entitlementJSON
- **Error Codes**: UnknownError
- **Source Files**: APIMethods510.scala

#### `POST /users/PROVIDER/PROVIDER_ID/sync`

- **Endpoint Name**: `syncExternalUser`
- **Summary**: Sync User
- **Description**: The endpoint is used to create or sync an OBP User with User from an external identity provider. PROVIDER is the host of the provider e.g. a Keycloak Host. PROVIDER_ID is the unique identifier for the User at the PROVIDER. At the end of the process, a User will exist in OBP with the Account Access records defined by the CBS. ${userAuthenticationMessage(true)} 
- **Route Pattern**: `"users" :: provider :: providerId :: "sync" :: Nil`
- **Tags**: User
- **Required Roles**: SyncUser
- **Path Parameters**: PROVIDER, PROVIDER_ID
- **Error Codes**: UnknownError
- **Source Files**: APIMethods510.scala

#### `POST /users/PROVIDER/USERNAME/locks`

- **Endpoint Name**: `lockUserByProviderAndUsername`
- **Summary**: Lock the user
- **Description**:  Lock a User. ${userAuthenticationMessage(true)} 
- **Route Pattern**: `"users" :: provider :: username :: "locks" :: Nil`
- **Tags**: User
- **Required Roles**: LockUser
- **Path Parameters**: PROVIDER, USERNAME
- **Error Codes**: UserNotFound, UnknownError
- **Source Files**: APIMethods510.scala

#### `POST /users/USER_ID/non-personal/attributes`

- **Endpoint Name**: `createNonPersonalUserAttribute`
- **Summary**: Create Non Personal User Attribute
- **Description**: Create Non Personal User Attribute The type field must be one of "STRING", "INTEGER", "DOUBLE" or DATE_WITH_DAY" ${userAuthenticationMessage(true)} 
- **Route Pattern**: `"users" :: userId ::"non-personal":: "attributes" :: Nil`
- **Tags**: User
- **Required Roles**: CreateNonPersonalUserAttribute
- **Path Parameters**: USER_ID
- **Error Codes**: UnknownError
- **Source Files**: APIMethods510.scala

### PUT Endpoints (18)

#### `PUT /banks/BANK_ID/accounts/ACCOUNT_ID/balances/BALANCE_ID`

- **Endpoint Name**: `updateBankAccountBalance`
- **Summary**: Update Bank Account Balance
- **Description**: Update an existing Bank Account Balance specified by BALANCE_ID. ${userAuthenticationMessage(true)} 
- **Route Pattern**: `"banks" :: BankId(bankId):: "accounts" :: AccountId(accountId) :: "balances" :: BalanceId(balanceId) :: Nil`
- **Tags**: Account, Balance
- **Required Roles**: UpdateBankAccountBalance
- **Path Parameters**: BANK_ID, BALANCE_ID, ACCOUNT_ID
- **Error Codes**: UnknownError
- **Source Files**: APIMethods510.scala

#### `PUT /banks/BANK_ID/accounts/ACCOUNT_ID/views/VIEW_ID/counterparties/COUNTERPARTY_ID/limits`

- **Endpoint Name**: `updateCounterpartyLimit`
- **Summary**: Update Counterparty Limit
- **Description**: Update Counterparty Limit.
- **Route Pattern**: `"banks" :: BankId(bankId) :: "accounts" :: AccountId(accountId) :: "views" :: ViewId(viewId) ::"counterparties" :: CounterpartyId(counterpartyId) ::"limits" :: Nil`
- **Path Parameters**: BANK_ID, COUNTERPARTY_ID, ACCOUNT_ID, VIEW_ID
- **Error Codes**: BankNotFound, BankAccountNotFound, CounterpartyNotFound, UnknownError
- **Source Files**: APIMethods510.scala

#### `PUT /banks/BANK_ID/accounts/ACCOUNT_ID/views/VIEW_ID/target-views/TARGET_VIEW_ID`

- **Endpoint Name**: `updateCustomView`
- **Summary**: Update Custom View
- **Description**: Update an existing custom view on a bank account ${userAuthenticationMessage(true)} and the user needs to have access to the owner view. The json sent is the same as during view creation (above), with one difference: the 'name' field of a view is not editable (it is only set when a view is created)
- **Route Pattern**: `"banks" :: BankId(bankId) :: "accounts" :: AccountId(accountId) :: "views" :: ViewId(viewId) :: "target-views" :: ViewId(targetViewId) :: Nil`
- **Tags**: Account, View
- **Path Parameters**: BANK_ID, ACCOUNT_ID, VIEW_ID, TARGET_VIEW_ID
- **Error Codes**: BankNotFound, BankAccountNotFound, UnknownError
- **Source Files**: APIMethods510.scala

#### `PUT /banks/BANK_ID/agents/AGENT_ID`

- **Endpoint Name**: `updateAgentStatus`
- **Summary**: Update Agent status
- **Description**:  ${userAuthenticationMessage(true)} 
- **Route Pattern**: `"banks" :: BankId(bankId) :: "agents"  :: agentId :: Nil`
- **Tags**: Person, Customer
- **Path Parameters**: BANK_ID, AGENT_ID
- **Error Codes**: AgentNotFound, BankNotFound, AgentAccountLinkNotFound, UnknownError
- **Source Files**: APIMethods510.scala

#### `PUT /banks/BANK_ID/atms/ATM_ID`

- **Endpoint Name**: `updateAtm`
- **Summary**: UPDATE ATM
- **Description**: Update ATM.
- **Route Pattern**: `"banks" :: BankId(bankId) :: "atms" :: AtmId(atmId) :: Nil`
- **Tags**: ATM
- **Required Roles**: UpdateAtm, UpdateAtmAtAnyBank
- **Path Parameters**: BANK_ID, ATM_ID
- **Error Codes**: UnknownError
- **Source Files**: APIMethods510.scala

#### `PUT /banks/BANK_ID/atms/ATM_ID/attributes/ATM_ATTRIBUTE_ID`

- **Endpoint Name**: `updateAtmAttribute`
- **Summary**: Update ATM Attribute
- **Description**: Update ATM Attribute. Update an ATM Attribute by its id. ${userAuthenticationMessage(true)} 
- **Route Pattern**: `"banks" :: BankId(bankId) :: "atms" :: AtmId(atmId) :: "attributes" :: atmAttributeId :: Nil`
- **Tags**: ATM
- **Required Roles**: UpdateAtmAttributeAtAnyBank, UpdateAtmAttribute
- **Path Parameters**: BANK_ID, ATM_ID, ATM_ATTRIBUTE_ID
- **Error Codes**: BankNotFound, UnknownError
- **Source Files**: APIMethods510.scala

#### `PUT /management/banks/BANK_ID/consents/CONSENT_ID`

- **Endpoint Name**: `updateConsentStatusByConsent`
- **Summary**: Update Consent Status by CONSENT_ID
- **Description**:  This endpoint is used to update the Status of Consent. Each Consent has one of the following states: ${ConsentStatus.values.toList.sorted.mkString(", ")}. ${userAuthenticationMessage(true)} 
- **Route Pattern**: `"management" :: "banks" :: BankId(bankId) :: "consents" :: consentId :: Nil`
- **Path Parameters**: BANK_ID, CONSENT_ID
- **Source Files**: APIMethods510.scala

#### `PUT /management/banks/BANK_ID/consents/CONSENT_ID/account-access`

- **Endpoint Name**: `updateConsentAccountAccessByConsentId`
- **Summary**: Update Consent Account Access by CONSENT_ID
- **Description**:  This endpoint is used to update the Account Access of Consent. ${userAuthenticationMessage(true)} 
- **Route Pattern**: `"management" :: "banks" :: BankId(bankId) :: "consents" :: consentId :: "account-access" :: Nil`
- **Path Parameters**: BANK_ID, CONSENT_ID
- **Source Files**: APIMethods510.scala

#### `PUT /management/banks/BANK_ID/consents/CONSENT_ID/created-by-user`

- **Endpoint Name**: `updateConsentUserIdByConsentId`
- **Summary**: Update Created by User of Consent by CONSENT_ID
- **Description**:  This endpoint is used to Update the User bound to a consent. In general we would not expect for a management user to set the User bound to a consent, but there may be some use cases where this workflow is useful. If successful, the "Created by User ID" field in the OBP Consent table will be updated. ${userAuthenticationMessage(true)} 
- **Route Pattern**: `"management" :: "banks" :: BankId(bankId) :: "consents" :: consentId :: "created-by-user" :: Nil`
- **Path Parameters**: BANK_ID, CONSENT_ID
- **Source Files**: APIMethods510.scala

#### `PUT /management/consumers/CONSUMER_ID/consumer/certificate`

- **Endpoint Name**: `updateConsumerCertificate`
- **Summary**: Update Consumer Certificate
- **Description**: Update a Certificate for a Consumer specified by CONSUMER_ID. ${consumerDisabledText()} CONSUMER_ID can be obtained after you register the application. Or use the endpoint 'Get Consumers' to get it 
- **Route Pattern**: `"management" :: "consumers" :: consumerId :: "consumer" :: "certificate" :: Nil`
- **Tags**: Consumer
- **Required Roles**: UpdateConsumerCertificate
- **Path Parameters**: CONSUMER_ID
- **Request Body**: createConsumerJSON
- **Error Codes**: UnknownError
- **Source Files**: APIMethods510.scala

#### `PUT /management/consumers/CONSUMER_ID/consumer/logo_url`

- **Endpoint Name**: `updateConsumerLogoURL`
- **Summary**: Update Consumer LogoURL
- **Description**: Update an existing logoURL for a Consumer specified by CONSUMER_ID. ${consumerDisabledText()} CONSUMER_ID can be obtained after you register the application. Or use the endpoint 'Get Consumers' to get it 
- **Route Pattern**: `"management" :: "consumers" :: consumerId :: "consumer" :: "logo_url" :: Nil`
- **Tags**: Consumer
- **Required Roles**: UpdateConsumerLogoUrl
- **Path Parameters**: CONSUMER_ID, _
- **Request Body**: createConsumerJSON
- **Error Codes**: UnknownError
- **Source Files**: APIMethods510.scala

#### `PUT /management/consumers/CONSUMER_ID/consumer/name`

- **Endpoint Name**: `updateConsumerName`
- **Summary**: Update Consumer Name
- **Description**: Update an existing name for a Consumer specified by CONSUMER_ID. ${consumerDisabledText()} CONSUMER_ID can be obtained after you register the application. Or use the endpoint 'Get Consumers' to get it 
- **Route Pattern**: `"management" :: "consumers" :: consumerId :: "consumer" :: "name" :: Nil`
- **Tags**: Consumer
- **Required Roles**: UpdateConsumerName
- **Path Parameters**: CONSUMER_ID
- **Request Body**: createConsumerJSON
- **Error Codes**: UnknownError
- **Source Files**: APIMethods510.scala

#### `PUT /management/consumers/CONSUMER_ID/consumer/redirect_url`

- **Endpoint Name**: `updateConsumerRedirectURL`
- **Summary**: Update Consumer RedirectURL
- **Description**: Update an existing redirectUrl for a Consumer specified by CONSUMER_ID. ${consumerDisabledText()} CONSUMER_ID can be obtained after you register the application. Or use the endpoint 'Get Consumers' to get it 
- **Route Pattern**: `"management" :: "consumers" :: consumerId :: "consumer" :: "redirect_url" :: Nil`
- **Tags**: Consumer
- **Required Roles**: UpdateConsumerRedirectUrl
- **Path Parameters**: CONSUMER_ID, _
- **Request Body**: consumerRedirectUrlJSON
- **Error Codes**: UnknownError
- **Source Files**: APIMethods510.scala

#### `PUT /management/transaction-requests/TRANSACTION_REQUEST_ID`

- **Endpoint Name**: `updateTransactionRequestStatus`
- **Summary**: Update Transaction Request Status
- **Description**: Update Transaction Request Status ${userAuthenticationMessage(true)} 
- **Route Pattern**: `"management" :: "transaction-requests" :: TransactionRequestId(transactionRequestId) :: Nil`
- **Tags**: TransactionRequest
- **Required Roles**: UpdateTransactionRequestStatusAtAnyBank
- **Path Parameters**: TRANSACTION_REQUEST_ID
- **Error Codes**: BankNotFound, BankAccountNotFound, UnknownError
- **Source Files**: APIMethods510.scala

#### `PUT /management/users/USER_ID`

- **Endpoint Name**: `validateUserByUserId`
- **Summary**: Validate a user
- **Description**:  Validate the User by USER_ID. ${userAuthenticationMessage(true)} 
- **Route Pattern**: `"management" :: "users" :: userId :: Nil`
- **Tags**: User
- **Required Roles**: ValidateUser
- **Path Parameters**: USER_ID
- **Error Codes**: UserNotFound, UnknownError
- **Source Files**: APIMethods510.scala

#### `PUT /my/api-collections/API_COLLECTION_ID`

- **Endpoint Name**: `updateMyApiCollection`
- **Summary**: Update My Api Collection By API_COLLECTION_ID
- **Description**: Update Api Collection for logged in user. ${userAuthenticationMessage(true)} 
- **Route Pattern**: `"my" :: "api-collections" :: apiCollectionId :: Nil`
- **Tags**: ApiCollection
- **Path Parameters**: API_COLLECTION_ID
- **Error Codes**: UserNotFound, UnknownError
- **Source Files**: APIMethods510.scala

#### `PUT /regulated-entities/REGULATED_ENTITY_ID/attributes/REGULATED_ENTITY_ATTRIBUTE_ID`

- **Endpoint Name**: `updateRegulatedEntityAttribute`
- **Summary**: Update Regulated Entity Attribute
- **Description**:  Update an existing Regulated Entity Attribute specified by ATTRIBUTE_ID. ${userAuthenticationMessage(true)} 
- **Route Pattern**: `"regulated-entities" :: entityId :: "attributes" :: attributeId :: Nil`
- **Tags**: Api, Directory
- **Required Roles**: UpdateRegulatedEntityAttribute
- **Path Parameters**: REGULATED_ENTITY_ID, REGULATED_ENTITY_ATTRIBUTE_ID
- **Error Codes**: UnknownError
- **Source Files**: APIMethods510.scala

#### `PUT /users/PROVIDER/USERNAME/lock-status`

- **Endpoint Name**: `unlockUserByProviderAndUsername`
- **Summary**: Unlock the user
- **Description**:  Unlock a User. (Perhaps the user was locked due to multiple failed login attempts) ${userAuthenticationMessage(true)} 
- **Route Pattern**: `"users" :: provider :: username :: "lock-status" :: Nil`
- **Tags**: User
- **Required Roles**: UnlockUser
- **Path Parameters**: PROVIDER, USERNAME
- **Error Codes**: UserNotFound, UnknownError
- **Source Files**: APIMethods510.scala

### DELETE Endpoints (11)

#### `DELETE /banks/BANK_ID/accounts/ACCOUNT_ID/balances/BALANCE_ID`

- **Endpoint Name**: `deleteBankAccountBalance`
- **Summary**: Delete Bank Account Balance
- **Description**: Delete a Bank Account Balance specified by BALANCE_ID. ${userAuthenticationMessage(true)} 
- **Route Pattern**: `"banks" :: BankId(bankId):: "accounts" :: AccountId(accountId) :: "balances" :: BalanceId(balanceId) :: Nil`
- **Tags**: Account, Balance
- **Required Roles**: DeleteBankAccountBalance
- **Path Parameters**: BANK_ID, BALANCE_ID, ACCOUNT_ID
- **Error Codes**: UnknownError
- **Source Files**: APIMethods510.scala

#### `DELETE /banks/BANK_ID/accounts/ACCOUNT_ID/views/VIEW_ID/counterparties/COUNTERPARTY_ID/limits`

- **Endpoint Name**: `deleteCounterpartyLimit`
- **Summary**: Delete Counterparty Limit
- **Description**: Delete Counterparty Limit.
- **Route Pattern**: `"banks" :: BankId(bankId) :: "accounts" :: AccountId(accountId) :: "views" :: ViewId(viewId) ::"counterparties" :: CounterpartyId(counterpartyId) ::"limits" :: Nil`
- **Path Parameters**: BANK_ID, COUNTERPARTY_ID, ACCOUNT_ID, VIEW_ID
- **Error Codes**: BankNotFound, BankAccountNotFound, CounterpartyNotFound, UnknownError
- **Source Files**: APIMethods510.scala

#### `DELETE /banks/BANK_ID/accounts/ACCOUNT_ID/views/VIEW_ID/target-views/TARGET_VIEW_ID`

- **Endpoint Name**: `deleteCustomView`
- **Summary**: Delete Custom View
- **Description**: Deletes the custom view specified by VIEW_ID on the bank account specified by ACCOUNT_ID at bank BANK_ID
- **Route Pattern**: `"banks" :: BankId(bankId) :: "accounts" :: AccountId(accountId ) :: "views" :: ViewId(viewId) :: "target-views" :: ViewId(targetViewId) :: Nil`
- **Tags**: Account, View
- **Path Parameters**: BANK_ID, ACCOUNT_ID, VIEW_ID, TARGET_VIEW_ID
- **Error Codes**: BankNotFound, BankAccountNotFound, UnknownError
- **Source Files**: APIMethods510.scala

#### `DELETE /banks/BANK_ID/atms/ATM_ID`

- **Endpoint Name**: `deleteAtm`
- **Summary**: Delete ATM
- **Description**: Delete ATM. This will also delete all its attributes. 
- **Route Pattern**: `"banks" :: BankId(bankId) :: "atms" :: AtmId(atmId) :: Nil`
- **Tags**: ATM
- **Required Roles**: DeleteAtm, DeleteAtmAtAnyBank
- **Path Parameters**: BANK_ID, ATM_ID
- **Error Codes**: UnknownError
- **Source Files**: APIMethods510.scala

#### `DELETE /banks/BANK_ID/atms/ATM_ID/attributes/ATM_ATTRIBUTE_ID`

- **Endpoint Name**: `deleteAtmAttribute`
- **Summary**: Delete ATM Attribute
- **Description**: Delete ATM Attribute Delete a Atm Attribute by its id. ${userAuthenticationMessage(true)} 
- **Route Pattern**: `"banks" :: BankId(bankId) :: "atms" :: AtmId(atmId) :: "attributes" :: atmAttributeId :: Nil`
- **Tags**: ATM
- **Required Roles**: DeleteAtmAttributeAtAnyBank, DeleteAtmAttribute
- **Path Parameters**: BANK_ID, ATM_ID, ATM_ATTRIBUTE_ID
- **Error Codes**: BankNotFound, UnknownError
- **Source Files**: APIMethods510.scala

#### `DELETE /banks/BANK_ID/consents/CONSENT_ID`

- **Endpoint Name**: `revokeConsentAtBank`
- **Summary**: Revoke Consent at Bank
- **Description**:  Revoke Consent specified by CONSENT_ID There are a few reasons you might need to revoke an application’s access to a user’s account: - The user explicitly wishes to revoke the application’s access - You as the service provider have determined an application is compromised or malicious, and want to disable it - etc. OBP as a resource server stores access tokens in a database, then it is relatively easy to revoke some token that belongs to a particular user. The status of the token is changed to 
- **Route Pattern**: `"banks" :: BankId(bankId) :: "consents" :: consentId :: Nil`
- **Tags**: Psd2, Consent, PSD2AIS
- **Required Roles**: RevokeConsentAtBank
- **Path Parameters**: BANK_ID, CONSENT_ID
- **Error Codes**: BankNotFound, UnknownError
- **Source Files**: APIMethods510.scala

#### `DELETE /my/consent/current`

- **Endpoint Name**: `selfRevokeConsent`
- **Summary**: Revoke Consent used in the Current Call
- **Description**:  Revoke Consent specified by Consent-Id at Request Header There are a few reasons you might need to revoke an application’s access to a user’s account: - The user explicitly wishes to revoke the application’s access - You as the service provider have determined an application is compromised or malicious, and want to disable it - etc. OBP as a resource server stores access tokens in a database, then it is relatively easy to revoke some token that belongs to a particular user. The status of the to
- **Route Pattern**: `"my" :: "consent" :: "current" :: Nil`
- **Tags**: Psd2, Consent, PSD2AIS
- **Error Codes**: BankNotFound, UnknownError
- **Source Files**: APIMethods510.scala

#### `DELETE /regulated-entities/REGULATED_ENTITY_ID`

- **Endpoint Name**: `deleteRegulatedEntity`
- **Summary**: Delete Regulated Entity
- **Description**: Delete Regulated Entity specified by REGULATED_ENTITY_ID ${userAuthenticationMessage(true)} 
- **Route Pattern**: `"regulated-entities" :: regulatedEntityId :: Nil`
- **Tags**: Api, Directory
- **Required Roles**: DeleteRegulatedEntity
- **Path Parameters**: REGULATED_ENTITY_ID
- **Error Codes**: UnknownError
- **Source Files**: APIMethods510.scala

#### `DELETE /regulated-entities/REGULATED_ENTITY_ID/attributes/REGULATED_ENTITY_ATTRIBUTE_ID`

- **Endpoint Name**: `deleteRegulatedEntityAttribute`
- **Summary**: Delete Regulated Entity Attribute
- **Description**:  Delete a Regulated Entity Attribute specified by REGULATED_ENTITY_ATTRIBUTE_ID. ${userAuthenticationMessage(true)} 
- **Route Pattern**: `"regulated-entities" :: entityId :: "attributes" :: attributeId :: Nil`
- **Tags**: Api, Directory
- **Required Roles**: DeleteRegulatedEntityAttribute
- **Path Parameters**: REGULATED_ENTITY_ID, REGULATED_ENTITY_ATTRIBUTE_ID
- **Error Codes**: UnknownError
- **Source Files**: APIMethods510.scala

#### `DELETE /system-views/VIEW_ID/permissions/PERMISSION_NAME`

- **Endpoint Name**: `deleteSystemViewPermission`
- **Summary**: Delete Permission to a System View
- **Description**: Delete Permission to a System View
- **Route Pattern**: `"system-views" :: ViewId(viewId) :: "permissions" :: permissionName :: Nil`
- **Tags**: SystemView
- **Required Roles**: DeleteSystemViewPermission
- **Path Parameters**: PERMISSION_NAME, VIEW_ID
- **Error Codes**: UnknownError
- **Source Files**: APIMethods510.scala

#### `DELETE /users/USER_ID/non-personal/attributes/USER_ATTRIBUTE_ID`

- **Endpoint Name**: `deleteNonPersonalUserAttribute`
- **Summary**: Delete Non Personal User Attribute
- **Description**: Delete the Non Personal User Attribute specified by ENTITLEMENT_REQUEST_ID for a user specified by USER_ID ${userAuthenticationMessage(true)} 
- **Route Pattern**: `"users" :: userId :: "non-personal" :: "attributes" :: userAttributeId :: Nil`
- **Tags**: User
- **Required Roles**: DeleteNonPersonalUserAttribute
- **Path Parameters**: USER_ID, USER_ATTRIBUTE_ID
- **Error Codes**: UnknownError
- **Source Files**: APIMethods510.scala


---

