package routes

import (
	"github.com/gin-gonic/gin"
	"github.com/obp-api/bank-registration/internal/controllers"
)

// SetupRoutes configures all API routes
// Maps to: User Story API Endpoints
func SetupRoutes(router *gin.Engine, bankController *controllers.BankController, bankAttributeController *controllers.BankAttributeController) {
	// API v1 group - Bank Registration User Story endpoints
	api := router.Group("/api")
	{
		// Bank routes - Maps to User Story "Bank Registration and Configuration"
		banks := api.Group("/banks")
		{
			// POST /api/banks - Create Bank Entity
			// Maps to: User Story "Bank Creation Endpoint"
			banks.POST("", bankController.CreateBank)

			// PUT /api/banks/:bankId - Update Bank Entity
			// Maps to: User Story "Bank Management (Update) Endpoint"
			banks.PUT("/:bankId", bankController.UpdateBank)

			// Note: GET endpoint removed - use /banks/:bankId from Bank Information Retrieval
			// The original Bank Registration user story only specified POST and PUT endpoints
		}
	}

	// ============================================================================
	// Bank Information Retrieval Routes
	// User Story: Bank Information Retrieval
	// Note: These routes are at /banks (not /api/banks) to match the user story
	// ============================================================================

	// Bank Information Retrieval routes
	banksRetrieval := router.Group("/banks")
	{
		// GET /banks - Retrieve All Banks
		// Maps to: User Story "Bank Information Retrieval - Retrieve All Banks"
		// BR-003: Returns basic bank info (excludes attributes)
		// BR-004: Returns 200 with empty array if no banks exist
		banksRetrieval.GET("", bankController.GetAllBanks)

		// GET /banks/:bankId - Retrieve Single Bank with Attributes
		// Maps to: User Story "Bank Information Retrieval - Retrieve Single Bank Details"
		// BR-001: Returns 404 if bank not found
		// BR-002: Returns complete bank info (includes attributes)
		banksRetrieval.GET("/:bankId", bankController.GetBankByIdWithAttributes)

		// ============================================================================
		// Bank Attribute Management Routes
		// User Story: Bank Attribute Management
		// Note: These routes are nested under /banks/:bankId
		// ============================================================================

		// POST /banks/:bankId/attribute - Define Bank Attribute
		// Maps to: User Story "Bank Attribute Management - Define Bank Attribute"
		// BR-001: Bank must exist
		// BR-002, BR-003: Type validation and type-value consistency
		banksRetrieval.POST("/:bankId/attribute", bankAttributeController.CreateBankAttribute)

		// GET /banks/:bankId/attributes - Retrieve All Bank Attributes
		// Maps to: User Story "Bank Attribute Management - Retrieve All Bank Attributes"
		// BR-001: Bank must exist
		// BR-005: Returns 200 with empty array if no attributes
		banksRetrieval.GET("/:bankId/attributes", bankAttributeController.GetBankAttributes)

		// GET /banks/:bankId/attributes/:attributeId - Retrieve Single Bank Attribute
		// Maps to: User Story "Bank Attribute Management - Retrieve Single Bank Attribute"
		// BR-001: Bank must exist
		// BR-004: Attribute must exist
		// BR-007: Returns complete attribute information
		banksRetrieval.GET("/:bankId/attributes/:attributeId", bankAttributeController.GetBankAttributeByID)

		// PUT /banks/:bankId/attributes/:attributeId - Update Bank Attribute
		// Maps to: User Story "Bank Attribute Management - Manage Bank Attribute"
		// BR-001: Bank must exist
		// BR-004: Attribute must exist
		// BR-002, BR-003: Type validation and type-value consistency
		banksRetrieval.PUT("/:bankId/attributes/:attributeId", bankAttributeController.UpdateBankAttribute)

		// DELETE /banks/:bankId/attributes/:attributeId - Delete Bank Attribute
		// Maps to: User Story "Bank Attribute Management - Delete Bank Attribute"
		// BR-001: Bank must exist
		// BR-004: Attribute must exist
		banksRetrieval.DELETE("/:bankId/attributes/:attributeId", bankAttributeController.DeleteBankAttribute)
	}

	// Health check endpoint
	router.GET("/health", func(c *gin.Context) {
		c.JSON(200, gin.H{
			"status": "healthy",
		})
	})
}
