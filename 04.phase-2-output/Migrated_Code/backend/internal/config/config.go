package config

import (
	"os"
)

// Config holds application configuration
type Config struct {
	// Server configuration
	ServerPort string
	ServerHost string

	// Database configuration
	DatabasePath string

	// Environment
	Environment string
}

// Load loads configuration from environment variables
func Load() *Config {
	return &Config{
		ServerPort:   getEnv("SERVER_PORT", "8080"),
		ServerHost:   getEnv("SERVER_HOST", "0.0.0.0"),
		DatabasePath: getEnv("DATABASE_PATH", ":memory:"),
		Environment:  getEnv("ENVIRONMENT", "development"),
	}
}

// getEnv gets an environment variable with a default value
func getEnv(key, defaultValue string) string {
	value := os.Getenv(key)
	if value == "" {
		return defaultValue
	}
	return value
}

// IsDevelopment returns true if running in development mode
func (c *Config) IsDevelopment() bool {
	return c.Environment == "development"
}

// IsProduction returns true if running in production mode
func (c *Config) IsProduction() bool {
	return c.Environment == "production"
}

// GetServerAddress returns the full server address
func (c *Config) GetServerAddress() string {
	return c.ServerHost + ":" + c.ServerPort
}
