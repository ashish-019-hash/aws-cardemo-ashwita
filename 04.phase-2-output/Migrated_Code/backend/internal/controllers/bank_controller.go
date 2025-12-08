package controllers

import (
	"net/http"

	"github.com/gin-gonic/gin"
	"github.com/obp-api/bank-registration/internal/models"
	"github.com/obp-api/bank-registration/internal/services"
)

// BankController handles HTTP requests for Bank entities
// Maps to: User Story API Endpoints
type BankController struct {
	service *services.BankService
}

// NewBankController creates a new BankController instance
func NewBankController(service *services.BankService) *BankController {
	return &BankController{service: service}
}

// CreateBank handles POST /api/banks
// Maps to: User Story "Bank Creation Endpoint"
// Request: CreateBankRequest
// Response: CreateBankResponse (201 Created) or ErrorResponse (400/409)
func (c *BankController) CreateBank(ctx *gin.Context) {
	var req models.CreateBankRequest

	// Parse request body
	if err := ctx.ShouldBindJSON(&req); err != nil {
		ctx.JSON(http.StatusBadRequest, models.ErrorResponse{
			Code:    "BANK-ERR-PARSE",
			Message: "Invalid request body: " + err.Error(),
		})
		return
	}

	// Call service to create bank
	response, err := c.service.CreateBank(ctx.Request.Context(), &req)
	if err != nil {
		// Handle service errors
		if serviceErr, ok := err.(*services.ServiceError); ok {
			ctx.JSON(serviceErr.HTTPStatus, models.ErrorResponse{
				Code:    serviceErr.Code,
				Message: serviceErr.Message,
			})
			return
		}
		ctx.JSON(http.StatusInternalServerError, models.ErrorResponse{
			Code:    "BANK-ERR-INTERNAL",
			Message: "Internal server error",
		})
		return
	}

	// Return success response
	ctx.JSON(http.StatusCreated, response)
}

// UpdateBank handles PUT /api/banks/:bankId
// Maps to: User Story "Bank Management (Update) Endpoint"
// Request: UpdateBankRequest
// Response: UpdateBankResponse (200 OK) or ErrorResponse (400/404)
func (c *BankController) UpdateBank(ctx *gin.Context) {
	// Get bank ID from path parameter
	bankID := ctx.Param("bankId")

	var req models.UpdateBankRequest

	// Parse request body
	if err := ctx.ShouldBindJSON(&req); err != nil {
		ctx.JSON(http.StatusBadRequest, models.ErrorResponse{
			Code:    "BANK-ERR-PARSE",
			Message: "Invalid request body: " + err.Error(),
		})
		return
	}

	// Call service to update bank
	response, err := c.service.UpdateBank(ctx.Request.Context(), bankID, &req)
	if err != nil {
		// Handle service errors
		if serviceErr, ok := err.(*services.ServiceError); ok {
			ctx.JSON(serviceErr.HTTPStatus, models.ErrorResponse{
				Code:    serviceErr.Code,
				Message: serviceErr.Message,
			})
			return
		}
		ctx.JSON(http.StatusInternalServerError, models.ErrorResponse{
			Code:    "BANK-ERR-INTERNAL",
			Message: "Internal server error",
		})
		return
	}

	// Return success response
	ctx.JSON(http.StatusOK, response)
}

// GetBank handles GET /api/banks/:bankId (helper endpoint, not in user story)
func (c *BankController) GetBank(ctx *gin.Context) {
	bankID := ctx.Param("bankId")

	bank, err := c.service.GetBank(ctx.Request.Context(), bankID)
	if err != nil {
		if serviceErr, ok := err.(*services.ServiceError); ok {
			ctx.JSON(serviceErr.HTTPStatus, models.ErrorResponse{
				Code:    serviceErr.Code,
				Message: serviceErr.Message,
			})
			return
		}
		ctx.JSON(http.StatusInternalServerError, models.ErrorResponse{
			Code:    "BANK-ERR-INTERNAL",
			Message: "Internal server error",
		})
		return
	}

	// Convert to response format
	response := models.BankResponse{
		ID:        bank.Permalink,
		ShortName: bank.ShortBankName,
		FullName:  bank.FullBankName,
		Logo:      bank.LogoURL,
		Website:   bank.WebsiteURL,
		BankRoutings: []models.BankRouting{
			{Scheme: bank.BankRoutingScheme, Address: bank.BankRoutingAddress},
		},
	}

	ctx.JSON(http.StatusOK, response)
}

// ============================================================================
// Bank Information Retrieval Handlers
// User Story: Bank Information Retrieval
// ============================================================================

// GetAllBanks handles GET /banks
// Maps to: User Story "Bank Information Retrieval - Retrieve All Banks"
// Response: BankListResponse (200 OK) with array of banks (empty array if none)
// Implements: BR-003 (Basic Bank Information Composition - excludes attributes)
// Implements: BR-004 (Empty Result Handling - returns 200 with empty array, not 404)
// Implements: VR-005 (Basic Information for Bank List)
// Implements: VR-006 (Empty Bank List Handling)
// Implements: VR-008 (HTTP Status Code Validation - always 200)
func (c *BankController) GetAllBanks(ctx *gin.Context) {
	response, err := c.service.GetAllBanks(ctx.Request.Context())
	if err != nil {
		if serviceErr, ok := err.(*services.ServiceError); ok {
			ctx.JSON(serviceErr.HTTPStatus, models.ErrorResponse{
				Code:    serviceErr.Code,
				Message: serviceErr.Message,
			})
			return
		}
		ctx.JSON(http.StatusInternalServerError, models.ErrorResponse{
			Code:    "BANK-ERR-INTERNAL",
			Message: "Internal server error",
		})
		return
	}

	// VR-008: Always return 200 OK for bank list (even if empty)
	ctx.JSON(http.StatusOK, response)
}

// GetBankByIdWithAttributes handles GET /banks/:bankId
// Maps to: User Story "Bank Information Retrieval - Retrieve Single Bank Details"
// Response: BankDetailResponse (200 OK) or ErrorResponse (400/404)
// Implements: BR-001 (Bank Existence Validation - returns 404 if not found)
// Implements: BR-002 (Complete Bank Information Composition - includes attributes)
// Implements: VR-001 (Bank Identifier Required Validation)
// Implements: VR-002 (Bank Identifier Existence Validation)
// Implements: VR-003 (Valid Bank Identifier Business Rule)
// Implements: VR-004 (Complete Information for Single Bank)
// Implements: VR-007 (Empty Attributes Array Handling)
// Implements: VR-008 (HTTP Status Code Validation - 200 or 404)
func (c *BankController) GetBankByIdWithAttributes(ctx *gin.Context) {
	bankID := ctx.Param("bankId")

	response, err := c.service.GetBankByIdWithAttributes(ctx.Request.Context(), bankID)
	if err != nil {
		if serviceErr, ok := err.(*services.ServiceError); ok {
			ctx.JSON(serviceErr.HTTPStatus, models.ErrorResponse{
				Code:    serviceErr.Code,
				Message: serviceErr.Message,
			})
			return
		}
		ctx.JSON(http.StatusInternalServerError, models.ErrorResponse{
			Code:    "BANK-ERR-INTERNAL",
			Message: "Internal server error",
		})
		return
	}

	// VR-008: Return 200 OK for successful retrieval
	ctx.JSON(http.StatusOK, response)
}

// ============================================================================
// Bank Attribute Management Handlers
// User Story: Bank Attribute Management
// ============================================================================

// BankAttributeController handles HTTP requests for BankAttribute entities
type BankAttributeController struct {
	service *services.BankAttributeService
}

// NewBankAttributeController creates a new BankAttributeController instance
func NewBankAttributeController(service *services.BankAttributeService) *BankAttributeController {
	return &BankAttributeController{service: service}
}

// CreateBankAttribute handles POST /banks/:bankId/attribute
// Maps to: User Story "Bank Attribute Management - Define Bank Attribute"
// Request: CreateBankAttributeRequest
// Response: BankAttributeResponse (201 Created) or ErrorResponse (400/404/409)
// Implements: BR-001 (Bank Existence Validation)
// Implements: BR-002 (Attribute Type Validation)
// Implements: BR-003 (Type-Value Consistency)
// Implements: VR-003, VR-004, VR-011
func (c *BankAttributeController) CreateBankAttribute(ctx *gin.Context) {
	bankID := ctx.Param("bankId")

	var req models.CreateBankAttributeRequest
	if err := ctx.ShouldBindJSON(&req); err != nil {
		ctx.JSON(http.StatusBadRequest, models.ErrorResponse{
			Code:    "ATTR-ERR-PARSE",
			Message: "Invalid request body: " + err.Error(),
		})
		return
	}

	response, err := c.service.CreateBankAttribute(ctx.Request.Context(), bankID, &req)
	if err != nil {
		if serviceErr, ok := err.(*services.ServiceError); ok {
			ctx.JSON(serviceErr.HTTPStatus, models.ErrorResponse{
				Code:    serviceErr.Code,
				Message: serviceErr.Message,
			})
			return
		}
		ctx.JSON(http.StatusInternalServerError, models.ErrorResponse{
			Code:    "ATTR-ERR-INTERNAL",
			Message: "Internal server error",
		})
		return
	}

	ctx.JSON(http.StatusCreated, response)
}

// UpdateBankAttribute handles PUT /banks/:bankId/attributes/:attributeId
// Maps to: User Story "Bank Attribute Management - Manage Bank Attribute"
// Request: UpdateBankAttributeRequest
// Response: BankAttributeResponse (200 OK) or ErrorResponse (400/404/409)
// Implements: BR-001, BR-004, BR-002, BR-003, VR-012
func (c *BankAttributeController) UpdateBankAttribute(ctx *gin.Context) {
	bankID := ctx.Param("bankId")
	attributeID := ctx.Param("attributeId")

	var req models.UpdateBankAttributeRequest
	if err := ctx.ShouldBindJSON(&req); err != nil {
		ctx.JSON(http.StatusBadRequest, models.ErrorResponse{
			Code:    "ATTR-ERR-PARSE",
			Message: "Invalid request body: " + err.Error(),
		})
		return
	}

	response, err := c.service.UpdateBankAttribute(ctx.Request.Context(), bankID, attributeID, &req)
	if err != nil {
		if serviceErr, ok := err.(*services.ServiceError); ok {
			ctx.JSON(serviceErr.HTTPStatus, models.ErrorResponse{
				Code:    serviceErr.Code,
				Message: serviceErr.Message,
			})
			return
		}
		ctx.JSON(http.StatusInternalServerError, models.ErrorResponse{
			Code:    "ATTR-ERR-INTERNAL",
			Message: "Internal server error",
		})
		return
	}

	ctx.JSON(http.StatusOK, response)
}

// GetBankAttributes handles GET /banks/:bankId/attributes
// Maps to: User Story "Bank Attribute Management - Retrieve All Bank Attributes"
// Response: BankAttributesListResponse (200 OK) or ErrorResponse (400/404)
// Implements: BR-001, BR-005, VR-015
func (c *BankAttributeController) GetBankAttributes(ctx *gin.Context) {
	bankID := ctx.Param("bankId")

	response, err := c.service.GetBankAttributes(ctx.Request.Context(), bankID)
	if err != nil {
		if serviceErr, ok := err.(*services.ServiceError); ok {
			ctx.JSON(serviceErr.HTTPStatus, models.ErrorResponse{
				Code:    serviceErr.Code,
				Message: serviceErr.Message,
			})
			return
		}
		ctx.JSON(http.StatusInternalServerError, models.ErrorResponse{
			Code:    "ATTR-ERR-INTERNAL",
			Message: "Internal server error",
		})
		return
	}

	// BR-005: Return 200 OK with empty array if no attributes
	ctx.JSON(http.StatusOK, response)
}

// GetBankAttributeByID handles GET /banks/:bankId/attributes/:attributeId
// Maps to: User Story "Bank Attribute Management - Retrieve Single Bank Attribute"
// Response: BankAttributeResponse (200 OK) or ErrorResponse (400/404)
// Implements: BR-001, BR-004, BR-007, VR-006
func (c *BankAttributeController) GetBankAttributeByID(ctx *gin.Context) {
	bankID := ctx.Param("bankId")
	attributeID := ctx.Param("attributeId")

	response, err := c.service.GetBankAttributeByID(ctx.Request.Context(), bankID, attributeID)
	if err != nil {
		if serviceErr, ok := err.(*services.ServiceError); ok {
			ctx.JSON(serviceErr.HTTPStatus, models.ErrorResponse{
				Code:    serviceErr.Code,
				Message: serviceErr.Message,
			})
			return
		}
		ctx.JSON(http.StatusInternalServerError, models.ErrorResponse{
			Code:    "ATTR-ERR-INTERNAL",
			Message: "Internal server error",
		})
		return
	}

	ctx.JSON(http.StatusOK, response)
}

// DeleteBankAttribute handles DELETE /banks/:bankId/attributes/:attributeId
// Maps to: User Story "Bank Attribute Management - Delete Bank Attribute"
// Response: 204 No Content or ErrorResponse (400/404)
// Implements: BR-001, BR-004, VR-013
func (c *BankAttributeController) DeleteBankAttribute(ctx *gin.Context) {
	bankID := ctx.Param("bankId")
	attributeID := ctx.Param("attributeId")

	err := c.service.DeleteBankAttribute(ctx.Request.Context(), bankID, attributeID)
	if err != nil {
		if serviceErr, ok := err.(*services.ServiceError); ok {
			ctx.JSON(serviceErr.HTTPStatus, models.ErrorResponse{
				Code:    serviceErr.Code,
				Message: serviceErr.Message,
			})
			return
		}
		ctx.JSON(http.StatusInternalServerError, models.ErrorResponse{
			Code:    "ATTR-ERR-INTERNAL",
			Message: "Internal server error",
		})
		return
	}

	ctx.Status(http.StatusNoContent)
}

// ============================================================================
// Multi-Bank Support Handlers
// User Story: Multi-Bank Support
// ============================================================================

// MultiBankController handles HTTP requests for multi-bank support operations
// Implements: BR-001 through BR-008 for multi-bank support
type MultiBankController struct {
	service *services.MultiBankService
}

// NewMultiBankController creates a new MultiBankController instance
func NewMultiBankController(service *services.MultiBankService) *MultiBankController {
	return &MultiBankController{service: service}
}

// GetBankAccounts handles GET /banks/:bankId/accounts
// Maps to: User Story "Multi-Bank Support - Retrieve Bank Accounts"
// Response: BankAccountsListResponse (200 OK) or ErrorResponse (400/404)
// Implements: BR-001 (Mandatory Bank Identifier for Resource Access)
// Implements: BR-002 (Bank Existence Validation)
// Implements: BR-003 (Data Isolation Enforcement)
// Implements: BR-004 (Empty Result Handling - returns 200 with empty array)
// Implements: VR-001 (Bank Identifier Required Validation)
// Implements: VR-002 (Bank Identifier Existence Validation)
// Implements: VR-003 (Data Isolation Enforcement Validation)
// Implements: VR-010 (Cross-Bank Access Error Response Validation)
// Source: LocalMappedConnector.getBankAccounts
func (c *MultiBankController) GetBankAccounts(ctx *gin.Context) {
	bankID := ctx.Param("bankId")

	response, err := c.service.GetBankAccounts(ctx.Request.Context(), bankID)
	if err != nil {
		if serviceErr, ok := err.(*services.ServiceError); ok {
			ctx.JSON(serviceErr.HTTPStatus, models.ErrorResponse{
				Code:    serviceErr.Code,
				Message: serviceErr.Message,
			})
			return
		}
		ctx.JSON(http.StatusInternalServerError, models.ErrorResponse{
			Code:    "BANK-ERR-INTERNAL",
			Message: "Internal server error",
		})
		return
	}

	// BR-004: Return 200 OK with accounts (empty array if none)
	ctx.JSON(http.StatusOK, response)
}

// GetBankEntitlements handles GET /banks/:bankId/entitlements
// Maps to: User Story "Multi-Bank Support - Retrieve Bank Entitlements"
// Response: EntitlementsListResponse (200 OK) or ErrorResponse (400/404)
// Implements: BR-001 (Mandatory Bank Identifier for Resource Access)
// Implements: BR-002 (Bank Existence Validation)
// Implements: BR-005 (Bank-Scoped Entitlements)
// Implements: BR-004 (Empty Result Handling - returns 200 with empty array)
// Implements: VR-001 (Bank Identifier Required Validation)
// Implements: VR-002 (Bank Identifier Existence Validation)
// Implements: VR-008 (Bank-Specific Entitlement Validation)
// Source: MappedEntitlementsProvider.getEntitlementsByBankId
func (c *MultiBankController) GetBankEntitlements(ctx *gin.Context) {
	bankID := ctx.Param("bankId")

	response, err := c.service.GetBankEntitlements(ctx.Request.Context(), bankID)
	if err != nil {
		if serviceErr, ok := err.(*services.ServiceError); ok {
			ctx.JSON(serviceErr.HTTPStatus, models.ErrorResponse{
				Code:    serviceErr.Code,
				Message: serviceErr.Message,
			})
			return
		}
		ctx.JSON(http.StatusInternalServerError, models.ErrorResponse{
			Code:    "BANK-ERR-INTERNAL",
			Message: "Internal server error",
		})
		return
	}

	// BR-004: Return 200 OK with entitlements (empty array if none)
	ctx.JSON(http.StatusOK, response)
}
