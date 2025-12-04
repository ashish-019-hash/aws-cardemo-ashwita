package main

import (
	"log"

	"github.com/gin-gonic/gin"
	"github.com/obp-api/bank-registration/internal/config"
	"github.com/obp-api/bank-registration/internal/controllers"
	"github.com/obp-api/bank-registration/internal/repositories"
	"github.com/obp-api/bank-registration/internal/routes"
	"github.com/obp-api/bank-registration/internal/services"
	"github.com/obp-api/bank-registration/pkg/db"
)

func main() {
	// Load configuration
	cfg := config.Load()

	// Initialize database
	database, err := db.GetDB()
	if err != nil {
		log.Fatalf("Failed to initialize database: %v", err)
	}
	defer db.CloseDB()

	// Initialize repository
	bankRepo := repositories.NewBankRepository(database)

	// Initialize service
	bankService := services.NewBankService(bankRepo)

	// Initialize controller
	bankController := controllers.NewBankController(bankService)

	// Setup Gin router
	if cfg.IsProduction() {
		gin.SetMode(gin.ReleaseMode)
	}
	router := gin.Default()

	// Setup routes
	routes.SetupRoutes(router, bankController)

	// Start server
	log.Printf("Starting server on %s", cfg.GetServerAddress())
	log.Printf("Health check: http://%s/health", cfg.GetServerAddress())
	log.Printf("API endpoints:")
	log.Printf("  POST http://%s/api/banks - Create Bank", cfg.GetServerAddress())
	log.Printf("  PUT  http://%s/api/banks/{bankId} - Update Bank", cfg.GetServerAddress())

	if err := router.Run(cfg.GetServerAddress()); err != nil {
		log.Fatalf("Failed to start server: %v", err)
	}
}
