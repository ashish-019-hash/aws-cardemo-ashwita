package middleware

import (
	"net/http"
	"strings"

	"github.com/gin-gonic/gin"
	"github.com/obp-api/bank-registration/internal/models"
)

// AuthMiddleware provides authentication middleware
// Maps to: User Story "Authentication: Required"
type AuthMiddleware struct {
	// In a real implementation, this would include JWT validation, etc.
}

// NewAuthMiddleware creates a new AuthMiddleware instance
func NewAuthMiddleware() *AuthMiddleware {
	return &AuthMiddleware{}
}

// RequireAuth middleware checks for valid authentication
// Maps to: User Story requirement "Authentication: Required"
func (m *AuthMiddleware) RequireAuth() gin.HandlerFunc {
	return func(c *gin.Context) {
		// Get Authorization header
		authHeader := c.GetHeader("Authorization")
		if authHeader == "" {
			c.AbortWithStatusJSON(http.StatusUnauthorized, models.ErrorResponse{
				Code:    "AUTH-001",
				Message: "Authorization header is required",
			})
			return
		}

		// Check for Bearer token format
		if !strings.HasPrefix(authHeader, "Bearer ") {
			c.AbortWithStatusJSON(http.StatusUnauthorized, models.ErrorResponse{
				Code:    "AUTH-002",
				Message: "Invalid authorization format. Expected: Bearer <token>",
			})
			return
		}

		// Extract token
		token := strings.TrimPrefix(authHeader, "Bearer ")
		if token == "" {
			c.AbortWithStatusJSON(http.StatusUnauthorized, models.ErrorResponse{
				Code:    "AUTH-003",
				Message: "Token is required",
			})
			return
		}

		// In a real implementation, validate the token here
		// For now, we accept any non-empty token for testing purposes

		// Set user context (would be extracted from token in real implementation)
		c.Set("user_id", "test-user")
		c.Set("roles", []string{"CanCreateBank"})

		c.Next()
	}
}

// RequireRole middleware checks for required role
// Maps to: User Story "Role Required: CanCreateBank"
func (m *AuthMiddleware) RequireRole(requiredRole string) gin.HandlerFunc {
	return func(c *gin.Context) {
		roles, exists := c.Get("roles")
		if !exists {
			c.AbortWithStatusJSON(http.StatusForbidden, models.ErrorResponse{
				Code:    "AUTH-004",
				Message: "No roles found for user",
			})
			return
		}

		roleList, ok := roles.([]string)
		if !ok {
			c.AbortWithStatusJSON(http.StatusInternalServerError, models.ErrorResponse{
				Code:    "AUTH-005",
				Message: "Invalid roles format",
			})
			return
		}

		// Check if user has required role
		hasRole := false
		for _, role := range roleList {
			if role == requiredRole {
				hasRole = true
				break
			}
		}

		if !hasRole {
			c.AbortWithStatusJSON(http.StatusForbidden, models.ErrorResponse{
				Code:    "AUTH-006",
				Message: "User does not have required role: " + requiredRole,
			})
			return
		}

		c.Next()
	}
}
