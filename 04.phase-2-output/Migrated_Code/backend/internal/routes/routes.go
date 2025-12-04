package routes

import (
	"github.com/gin-gonic/gin"
	"github.com/obp-api/bank-registration/internal/controllers"
)

// SetupRoutes configures all API routes
// Maps to: User Story API Endpoints
func SetupRoutes(router *gin.Engine, bankController *controllers.BankController) {
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
	}

	// Health check endpoint
	router.GET("/health", func(c *gin.Context) {
		c.JSON(200, gin.H{
			"status": "healthy",
		})
	})
}
