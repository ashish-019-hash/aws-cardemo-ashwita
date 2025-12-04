package models

// BankRouting represents bank routing information
// Source: code/api/v4_0_0/JSONFactory4.0.0.scala
type BankRouting struct {
	Scheme  string `json:"scheme" validate:"required"`
	Address string `json:"address" validate:"required"`
}

// Branding represents bank branding information
// Maps to user story "branding" field
type Branding struct {
	Logo   string `json:"logo" validate:"required"`
	Colors string `json:"colors" validate:"required"`
}

// OperationalParams represents bank operational parameters
// Maps to user story "operationalParams" field
type OperationalParams struct {
	BusinessHours string                 `json:"businessHours" validate:"required"`
	Limits        map[string]interface{} `json:"limits" validate:"required"`
	Currencies    []string               `json:"currencies" validate:"required,min=1,max=50,dive,len=3"`
}

// CreateBankRequest represents the request body for POST /api/banks
// Maps to user story: Create Bank Entity
// Source: code/api/v4_0_0/JSONFactory4.0.0.scala:106 (PostBankJson400)
type CreateBankRequest struct {
	// Bank ID - maps to user story "bankId"
	BankID string `json:"bankId" validate:"required,min=4"`

	// Bank code - maps to user story "bankCode"
	BankCode string `json:"bankCode" validate:"required"`

	// Bank name - maps to user story "bankName"
	BankName string `json:"bankName" validate:"required,min=1,max=255"`

	// Branding information - maps to user story "branding"
	Branding Branding `json:"branding" validate:"required"`

	// Operational parameters - maps to user story "operationalParams"
	OperationalParams OperationalParams `json:"operationalParams" validate:"required"`
}

// UpdateBankRequest represents the request body for PUT /api/banks/{bankId}
// Maps to user story: Manage Bank Entity
// Source: code/api/v5_0_0/APIMethods500.scala:262 (PostBankJson500)
type UpdateBankRequest struct {
	// Bank name - maps to user story "bankName" (optional for update)
	BankName *string `json:"bankName,omitempty"`

	// Branding information - maps to user story "branding" (optional for update)
	Branding *Branding `json:"branding,omitempty"`

	// Operational parameters - maps to user story "operationalParams" (optional for update)
	OperationalParams *OperationalParams `json:"operationalParams,omitempty"`
}

// CreateBankResponse represents the response for POST /api/banks
// Maps to user story response format
type CreateBankResponse struct {
	BankID string `json:"bankId"`
	Status string `json:"status"`
}

// UpdateBankResponse represents the response for PUT /api/banks/{bankId}
// Maps to user story response format
type UpdateBankResponse struct {
	BankID string `json:"bankId"`
	Status string `json:"status"`
}

// BankResponse represents the full bank response
// Source: code/api/v4_0_0/JSONFactory4.0.0.scala:97 (BankJson400)
type BankResponse struct {
	ID           string        `json:"id"`
	ShortName    string        `json:"short_name"`
	FullName     string        `json:"full_name"`
	Logo         string        `json:"logo"`
	Website      string        `json:"website"`
	BankRoutings []BankRouting `json:"bank_routings"`
}

// ErrorResponse represents an API error response
type ErrorResponse struct {
	Code    string `json:"code"`
	Message string `json:"message"`
}

// ValidationError represents a validation error with field details
type ValidationError struct {
	Field   string `json:"field"`
	Code    string `json:"code"`
	Message string `json:"message"`
}

// ValidationErrorResponse represents a response with multiple validation errors
type ValidationErrorResponse struct {
	Errors []ValidationError `json:"errors"`
}
