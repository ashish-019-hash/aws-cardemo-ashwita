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

// ============================================================================
// Bank Information Retrieval DTOs
// User Story: Bank Information Retrieval
// ============================================================================

// BankAttributeResponse represents a bank attribute in API responses
// Source: code/bankattribute/MappedBankAttributeProvider.scala
// Used in: GET /banks/BANK_ID response, GET /banks/BANK_ID/attributes response
// User Story: Bank Attribute Management
type BankAttributeResponse struct {
	BankID          string `json:"bank_id"`
	BankAttributeID string `json:"bank_attribute_id"`
	Name            string `json:"name"`
	Type            string `json:"type"`
	Value           string `json:"value"`
	IsActive        bool   `json:"is_active"`
}

// BankListItem represents a single bank in the bank list response
// Used in: GET /banks response
// Note: Does NOT include attributes (per BR-003 for performance optimization)
type BankListItem struct {
	ID           string        `json:"id"`
	ShortName    string        `json:"short_name"`
	FullName     string        `json:"full_name"`
	Logo         string        `json:"logo"`
	Website      string        `json:"website"`
	BankRoutings []BankRouting `json:"bank_routings"`
}

// BankListResponse represents the response for GET /banks
// User Story: Bank Information Retrieval - Retrieve All Banks
// BR-003: Basic bank info composition (excludes attributes)
// BR-004: Empty result returns 200 with empty array, not 404
type BankListResponse struct {
	Banks []BankListItem `json:"banks"`
}

// BankDetailResponse represents the response for GET /banks/BANK_ID
// User Story: Bank Information Retrieval - Retrieve Single Bank Details
// BR-002: Complete bank info composition (includes attributes)
// VR-004: Must include all fields (name, logo, website, routing, attributes)
// VR-007: Empty attributes returns empty array, not null
type BankDetailResponse struct {
	ID           string                  `json:"id"`
	ShortName    string                  `json:"short_name"`
	FullName     string                  `json:"full_name"`
	Logo         string                  `json:"logo"`
	Website      string                  `json:"website"`
	BankRoutings []BankRouting           `json:"bank_routings"`
	Attributes   []BankAttributeResponse `json:"attributes"`
}

// ToBankListItem converts a MappedBank to BankListItem
func (b *MappedBank) ToBankListItem() BankListItem {
	routings := []BankRouting{}
	if b.BankRoutingScheme != "" || b.BankRoutingAddress != "" {
		routings = append(routings, BankRouting{
			Scheme:  b.BankRoutingScheme,
			Address: b.BankRoutingAddress,
		})
	}
	return BankListItem{
		ID:           b.Permalink,
		ShortName:    b.ShortBankName,
		FullName:     b.FullBankName,
		Logo:         b.LogoURL,
		Website:      b.WebsiteURL,
		BankRoutings: routings,
	}
}

// ToBankDetailResponse converts a MappedBank to BankDetailResponse
// Attributes must be provided separately (from BankAttributeRepository)
func (b *MappedBank) ToBankDetailResponse(attributes []BankAttributeResponse) BankDetailResponse {
	routings := []BankRouting{}
	if b.BankRoutingScheme != "" || b.BankRoutingAddress != "" {
		routings = append(routings, BankRouting{
			Scheme:  b.BankRoutingScheme,
			Address: b.BankRoutingAddress,
		})
	}
	// VR-007: Ensure attributes is never nil, always empty array if no attributes
	if attributes == nil {
		attributes = []BankAttributeResponse{}
	}
	return BankDetailResponse{
		ID:           b.Permalink,
		ShortName:    b.ShortBankName,
		FullName:     b.FullBankName,
		Logo:         b.LogoURL,
		Website:      b.WebsiteURL,
		BankRoutings: routings,
		Attributes:   attributes,
	}
}

// ToBankAttributeResponse converts a BankAttribute to BankAttributeResponse
func (a *BankAttribute) ToBankAttributeResponse() BankAttributeResponse {
	return BankAttributeResponse{
		BankID:          a.BankID,
		BankAttributeID: a.BankAttributeID,
		Name:            a.Name,
		Type:            a.Type,
		Value:           a.Value,
		IsActive:        a.IsActive,
	}
}

// ============================================================================
// Bank Attribute Management DTOs
// User Story: Bank Attribute Management
// ============================================================================

// CreateBankAttributeRequest represents the request body for POST /banks/BANK_ID/attribute
// User Story: Bank Attribute Management - Define Bank Attribute
type CreateBankAttributeRequest struct {
	// Name of the attribute (required)
	// VR-003: Attribute name must be provided and non-empty
	Name string `json:"name" validate:"required,min=1,max=50"`

	// Type of the attribute (required)
	// VR-004: Attribute type must be one of: STRING, INTEGER, DOUBLE, DATE_WITH_DAY
	// BR-002: Attribute type validation and enforcement
	Type string `json:"type" validate:"required,oneof=STRING INTEGER DOUBLE DATE_WITH_DAY"`

	// Value of the attribute (required)
	// BR-003: Type-value consistency enforcement
	Value string `json:"value" validate:"required"`

	// Active status of the attribute (optional, defaults to true)
	// BR-006: Attribute active/inactive status management
	IsActive *bool `json:"is_active,omitempty"`
}

// UpdateBankAttributeRequest represents the request body for PUT /banks/BANK_ID/attributes/BANK_ATTRIBUTE_ID
// User Story: Bank Attribute Management - Manage Bank Attribute
type UpdateBankAttributeRequest struct {
	// Name of the attribute (required for update)
	// VR-003: Attribute name must be provided and non-empty
	Name string `json:"name" validate:"required,min=1,max=50"`

	// Type of the attribute (required for update)
	// VR-004: Attribute type must be one of: STRING, INTEGER, DOUBLE, DATE_WITH_DAY
	// BR-002: Attribute type validation and enforcement
	Type string `json:"type" validate:"required,oneof=STRING INTEGER DOUBLE DATE_WITH_DAY"`

	// Value of the attribute (required for update)
	// BR-003: Type-value consistency enforcement
	Value string `json:"value" validate:"required"`

	// Active status of the attribute (required for update)
	// BR-006: Attribute active/inactive status management
	IsActive bool `json:"is_active"`
}

// BankAttributesListResponse represents the response for GET /banks/BANK_ID/attributes
// User Story: Bank Attribute Management - Retrieve All Bank Attributes
// BR-005: Empty result handling - returns 200 with empty array, not 404
type BankAttributesListResponse struct {
	BankAttributes []BankAttributeResponse `json:"bank_attributes"`
}
