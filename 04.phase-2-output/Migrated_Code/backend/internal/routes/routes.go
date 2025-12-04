package routes

import (
	"github.com/gin-gonic/gin"
	"github.com/obp-api/bank-registration/internal/controllers"
)

// SetupRoutes configures all API routes
// Maps to: User Story API Endpoints
func SetupRoutes(router *gin.Engine, bankController *controllers.BankController) {
	// API v1 group
	api := router.Group("/api")
	{
		// Bank routes - Maps to User Story endpoints
		banks := api.Group("/banks")
		{
			// POST /api/banks - Create Bank Entity
			// Maps to: User Story "Bank Creation Endpoint"
			banks.POST("", bankController.CreateBank)

			// PUT /api/banks/:bankId - Update Bank Entity
			// Maps to: User Story "Bank Management (Update) Endpoint"
			banks.PUT("/:bankId", bankController.UpdateBank)

			// GET /api/banks/:bankId - Get Bank (helper, not in user story)
			banks.GET("/:bankId", bankController.GetBank)
		}
	}

	// Health check endpoint
	router.GET("/health", func(c *gin.Context) {
		c.JSON(200, gin.H{
			"status": "healthy",
		})
	})
}
