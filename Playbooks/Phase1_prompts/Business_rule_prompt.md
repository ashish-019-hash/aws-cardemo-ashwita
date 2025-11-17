# Business Rules Extraction Playbook for Scala to Go Migration

## Purpose
Extract **true business rules** from Scala code - the calculations, decisions, and logic that represent real business knowledge, not technical implementation. This extraction is critical for migrating your Scala application to Go while preserving all business logic.

**Important Note on Multiple Runs:** When using this prompt multiple times, each run is for analyzing a **DIFFERENT application or codebase** (e.g., BankingAPI, PaymentService, AccountManagement, etc.), not an attempt to get a better response for the same application. Even if you are re-analyzing the same application or codebase, treat each run as an independent analysis. Always explicitly specify which application or codebase you are analyzing at the beginning of your response. Use the consistent extraction format as defined in this prompt, regardless of previous runs. Each analysis is independent.

**Migration Context:** You are migrating a Scala application to Go. Post-migration, the new Go-based application will be validated using existing test cases. Therefore, you MUST consider all functions with exact endpoints present in the Scala application and extract ALL business rules completely and accurately.

## Quick Decision Guide
```
Found calculation or logic in Scala code?
↓
Business amounts/rates/decisions? → YES → Complex logic? → YES → Business user cares? → YES → INCLUDE
                                 ↓ NO                    ↓ NO                      ↓ NO
                                EXCLUDE               EXCLUDE                   EXCLUDE
```

## What to INCLUDE ✅
**Business Logic** - Would exist in any system:
- **Calculations**: Interest rates, fees, balances, percentages, pricing formulas
- **Decisions**: Approval criteria, eligibility rules, thresholds, routing logic
- **Formulas**: Business-specific mathematical operations and algorithms
- **Conditions**: Business scenarios and branching logic with business meaning
- **Workflows**: Business process orchestration and state transitions
- **Aggregations**: Business totals, averages, summaries, KPI calculations

**Examples**:
```scala
// Business calculation
val interest = balance * rate / 1200

// Business decision
if (creditLimit < newBalance) reject() else approve()

// Business workflow
status match {
  case "PENDING" if amount > threshold => requireApproval()
  case "PENDING" => autoApprove()
  case "APPROVED" => processPayment()
}

// Business aggregation
transactions.filter(_.date.isAfter(startDate)).map(_.amount).sum
```

## What to EXCLUDE ❌
**Technical Logic** - System implementation details:
- **Data Conversion**: JSON/XML parsing, type conversions, serialization
- **Validation**: Field checks, required field validation, format validation
- **System Operations**: Database queries, HTTP calls, logging, error formatting
- **Housekeeping**: ID generation, timestamps, correlation IDs
- **Framework Logic**: Dependency injection, lifecycle hooks, configuration loading
- **Infrastructure**: Connection pooling, caching, retry logic, circuit breakers

**Examples**:
```scala
// Data conversion - EXCLUDE
val numericAmount = stringAmount.toDouble

// Input validation - EXCLUDE
if (customerName.isEmpty) throw ValidationError("Name required")

// System housekeeping - EXCLUDE
val nextId = currentId + 1

// Framework logic - EXCLUDE
@Inject val repository: CustomerRepository

// Infrastructure - EXCLUDE
implicit val timeout: Timeout = 30.seconds
```

## 3-Phase Method

### Phase 1: Discovery
1. **Understand the application** - Review API endpoints, service layer, domain models
2. **Find business programs** - Look for service classes with calculation, processing, decision logic
3. **Map endpoints to business functions** - Identify what business operations each endpoint performs
4. **Source code is truth** - Ignore documentation conflicts, trust the actual Scala implementation

### Phase 2: Analysis
**Search for business logic in**:
- Service layer methods (`*Service.scala`, `*Manager.scala`, `*Processor.scala`)
- Domain model companion objects with business logic
- Pattern matching with business conditions
- For-comprehensions orchestrating business workflows
- Methods with business-meaningful names (calculate*, process*, determine*, evaluate*)

**Look for**:
- Mathematical operations on business values (amounts, rates, balances)
- Business variables (rate, balance, limit, amount, fee, price, score)
- Conditional logic with business meaning (if/match with business states)
- Threshold comparisons and business rule enforcement
- Business state transitions and workflow logic

### Phase 3: Validation
**Ask for each rule**:
1. Would a business user care about this logic?
2. Would this exist in any system implementation (not just Scala)?
3. Does it affect business outcomes or decisions?
4. Is it more than simple data movement or format conversion?
5. Is this logic tested in your existing test cases?

## Documentation Template
```markdown
### BR-[###]: [Business Rule Name]

**Category**: [CALC/DECISION/THRESHOLD/AGGREGATION/WORKFLOW/TRANSFORMATION]

**Description**: [What this rule does in business terms - clear, concise statement]

**Source**: 
- File: [path/to/ScalaFile.scala]
- Class/Object: [ClassName or ObjectName]
- Method: [methodName]
- Lines: [###-###]

**Business Logic**:
[Step-by-step breakdown of the business rule in business terms]
1. [First step with business context]
2. [Second step with conditions or calculations]
3. [Third step with outcomes]

**Scala Implementation**:
```scala
// Relevant code snippet showing the business logic
def calculateInterest(balance: BigDecimal, rate: Double, days: Int): BigDecimal = {
  balance * rate * days / 365
}
```

**Variables**:
- **Input**: [Business data used - entity.field names and business meaning]
- **Output**: [Business result produced and its meaning]
- **Constants**: [Any business constants or thresholds used]

**Business Conditions** (if applicable):
| Condition | Business Meaning | Values/Thresholds |
|-----------|------------------|-------------------|
| balance > 10000 | High-value account | $10,000 threshold |
| status == "ACTIVE" | Account is operational | Active status only |

**Business Impact**: 
[How this affects business operations, what business problem it solves, why it exists]

**API Endpoints Using This Rule**:
- [GET/POST/PUT/DELETE /api/path] - [Business operation]

**Related Test Cases**:
[Reference to test cases that validate this rule - helps with Go migration validation]

**Migration Notes for Go**:
- [Specific considerations for implementing this in Go]
- [How Scala patterns map to Go idioms]
- [Any edge cases or special handling needed]

**Example Scenarios**:
```
Scenario 1: [Business scenario description]
Input: balance = 5000, rate = 0.05, days = 30
Processing: 5000 * 0.05 * 30 / 365
Output: interest = 20.55

Scenario 2: [Alternative scenario]
Input: balance = 0, rate = 0.05, days = 30
Processing: 0 * 0.05 * 30 / 365
Output: interest = 0.00
```
```

## Rule Categories (Types)
- **CALC**: Mathematical calculations (rates, fees, balances, pricing)
- **DECISION**: Approval/rejection logic, routing decisions, eligibility
- **THRESHOLD**: Limit-based decisions, boundary checks with business meaning
- **AGGREGATION**: Totals, averages, accumulations, summaries, KPIs
- **WORKFLOW**: Business process flows, state transitions, orchestration
- **TRANSFORMATION**: Business data mapping, enrichment, derivation

## Quality Checks

**Red Flags** (likely NOT business rules):
- JSON/XML serialization and deserialization
- Database query construction (SQL, Slick queries)
- HTTP request/response handling
- Null checks and Option unwrapping without business logic
- Sequential ID generation or UUID creation
- Empty field checks without business context
- Logging statements and error formatting
- Type conversions (String to Int, etc.)
- Framework annotations and configuration

**Green Flags** (likely business rules):
- Rate applications and interest calculations
- Balance calculations and account operations
- Approval criteria and authorization logic
- Business thresholds and limit enforcement
- Fee computations and pricing logic
- Business state transitions (order status, account status)
- Eligibility determination and qualification logic
- Business aggregations and reporting calculations

## Scala-Specific Patterns to Extract

### Pattern Matching with Business Logic
```scala
// INCLUDE - Business decision logic
status match {
  case "ACTIVE" => processTransaction()
  case "SUSPENDED" => requireApproval()
  case "CLOSED" => rejectTransaction()
}
```

### For-Comprehensions with Business Workflows
```scala
// INCLUDE - Business workflow orchestration
for {
  account <- validateAccount(accountId)
  balance <- checkSufficientFunds(account, amount)
  transaction <- createTransaction(account, amount)
  result <- processPayment(transaction)
} yield result
```

### Business Validation with Either/Try
```scala
// INCLUDE if business constraint, EXCLUDE if just input validation
Either.cond(
  amount > 0 && amount <= account.creditLimit,  // Business constraint
  transaction,
  InsufficientCreditError
)
```

### Business Calculations in Methods
```scala
// INCLUDE - Business calculation
def calculateMonthlyPayment(principal: BigDecimal, rate: Double, months: Int): BigDecimal = {
  val monthlyRate = rate / 12
  principal * monthlyRate / (1 - Math.pow(1 + monthlyRate, -months))
}
```

## Success Metrics
- **Endpoint Coverage**: 100% of API endpoints analyzed for business rules
- **Clarity**: Business users can understand each rule without Scala knowledge
- **Relevance**: >90% of rules are business-meaningful (not technical)
- **Completeness**: All significant business logic from all endpoints captured
- **Traceability**: Every rule maps to source code and test cases
- **Migration Readiness**: Sufficient detail for accurate Go implementation

## Common Mistakes to Avoid

1. **Including validation as business rules** → Focus on what happens AFTER validation passes
2. **Technical calculations** → Ask "Would business care about this specific calculation?"
3. **Too granular** → Group related logic into cohesive rules (not one rule per line)
4. **Missing complex logic** → Look for multi-step business processes across methods
5. **Ignoring endpoints** → Every API endpoint should be analyzed for business rules
6. **Framework confusion** → Don't extract Play Framework, Akka, or other framework logic
7. **Missing pattern matching** → Scala pattern matching often contains critical business decisions
8. **Incomplete workflows** → For-comprehensions often represent complete business workflows

## Key Principle
**If a business user wouldn't care about the logic, it's probably technical implementation, not a business rule.**

**Migration Focus**: Every business rule you extract must be implementable in Go and testable with existing test cases. If you can't explain how to validate it, you may be extracting technical logic instead of business logic.

---

## Deliverables

### Single Output File: business-rules.md

**File Structure**:
```markdown
# Business Rules Extraction

**Extracted From**: [Scala Application Name]
**Analysis Date**: [Date]
**Analyst**: Expert Scala Business Logic Analyst
**Migration Target**: Go Application

## Executive Summary
- Total Business Rules Extracted: [count]
- API Endpoints Analyzed: [count]
- Rule Categories:
  - Calculations: [count]
  - Decisions: [count]
  - Thresholds: [count]
  - Aggregations: [count]
  - Workflows: [count]
  - Transformations: [count]

## Business Rules Catalog

[Use the documentation template above for each rule: BR-001, BR-002, etc.]

## API Endpoint Coverage

| Endpoint | HTTP Method | Business Rules | Rule IDs |
|----------|-------------|----------------|----------|
| /api/accounts/{id}/balance | GET | Balance calculation | BR-001, BR-003 |
| /api/transactions | POST | Transaction validation, fee calculation | BR-005, BR-007 |

## Migration Validation Matrix

| Rule ID | Test Case Reference | Go Implementation Status | Validation Status |
|---------|---------------------|--------------------------|-------------------|
| BR-001 | TestBalanceCalculation | Pending | Pending |
| BR-002 | TestInterestAccrual | Pending | Pending |

## Notes and Assumptions
[Any assumptions made, gaps identified, or clarifications needed]
```

---

## Analysis Checklist

Before finalizing, ensure:
- ☐ All API endpoints have been analyzed
- ☐ All service layer classes have been reviewed
- ☐ Pattern matching expressions with business logic are extracted
- ☐ For-comprehensions representing workflows are documented
- ☐ Business calculations have formulas and examples
- ☐ Each rule references specific Scala source code
- ☐ Each rule includes migration notes for Go
- ☐ Each rule maps to test cases where applicable
- ☐ No technical/framework logic is included
- ☐ All rules are described in business terms
- ☐ Endpoint coverage table is complete
- ☐ Migration validation matrix is included

---

**Remember**: 
1. **Code is the source of truth** - Extract from actual Scala implementation, not documentation
2. **Business knowledge only** - Focus on logic that will be valuable in the Go implementation
3. **Complete endpoint coverage** - Analyze every API endpoint for business rules
4. **Test case alignment** - Every rule should align with existing test cases for validation
5. **Migration focus** - Provide sufficient detail for accurate Go implementation

**Output**: Generate ONLY the business-rules.md file. Do not generate business entities, validation rules, or screen flow files.
