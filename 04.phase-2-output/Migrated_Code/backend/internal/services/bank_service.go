package services

import (
	"context"
	"database/sql"
	"errors"

	"github.com/obp-api/bank-registration/internal/models"
	"github.com/obp-api/bank-registration/internal/repositories"
	"github.com/obp-api/bank-registration/internal/validators"
)

// BankService handles creation and management of Bank entities
// Maps to: User Story "BankService: Handles creation and management of Bank entities"
type BankService struct {
	repo      repositories.BankRepository
	validator *validators.BankValidator
}

// NewBankService creates a new BankService instance
func NewBankService(repo repositories.BankRepository) *BankService {
	return &BankService{
		repo:      repo,
		validator: validators.NewBankValidator(),
	}
}

// ServiceError represents a service-level error with code and HTTP status
type ServiceError struct {
	Code       string
	Message    string
	HTTPStatus int
	Field      string
}

func (e *ServiceError) Error() string {
	return e.Message
}

// CreateBank creates a new Bank entity
// Maps to: POST /api/banks
// Implements: BR-001 (Unique Identification), BR-002 (Required Fields)
// Source: NewStyle.function.createOrUpdateBank (code/api/util/NewStyle.scala:310)
func (s *BankService) CreateBank(ctx context.Context, req *models.CreateBankRequest) (*models.CreateBankResponse, error) {
	// BR-002: Validate required fields
	validationResults := s.validator.ValidateCreateBankRequest(req)
	if len(validationResults) > 0 {
		result := validationResults[0]
		return nil, &ServiceError{
			Code:       result.Code,
			Message:    result.Message,
			HTTPStatus: 400,
			Field:      result.Field,
		}
	}

	// BR-001: Check unique identification - Bank ID
	exists, err := s.repo.BankExistsByPermalink(ctx, req.BankID)
	if err != nil {
		return nil, &ServiceError{
			Code:       "BANK-ERR-001",
			Message:    "Failed to check bank existence",
			HTTPStatus: 500,
		}
	}
	if exists {
		result := validators.NewBankIDUniqueError(req.BankID)
		return nil, &ServiceError{
			Code:       result.Code,
			Message:    result.Message,
			HTTPStatus: 409,
			Field:      result.Field,
		}
	}

	// BR-001: Check unique identification - Bank Code
	exists, err = s.repo.BankExistsByCode(ctx, req.BankCode)
	if err != nil {
		return nil, &ServiceError{
			Code:       "BANK-ERR-001",
			Message:    "Failed to check bank code existence",
			HTTPStatus: 500,
		}
	}
	if exists {
		result := validators.NewBankCodeUniqueError(req.BankCode)
		return nil, &ServiceError{
			Code:       result.Code,
			Message:    result.Message,
			HTTPStatus: 409,
			Field:      result.Field,
		}
	}

	// Create bank entity from request
	bank := &models.MappedBank{
		Permalink:     req.BankID,
		FullBankName:  req.BankName,
		ShortBankName: req.BankCode,
		LogoURL:       req.Branding.Logo,
		WebsiteURL:    req.Branding.Colors, // Using colors field for website as per user story mapping
	}

	// Persist via repository
	if err := s.repo.CreateBank(ctx, bank); err != nil {
		return nil, &ServiceError{
			Code:       "BANK-ERR-002",
			Message:    "Failed to create bank: " + err.Error(),
			HTTPStatus: 500,
		}
	}

	return &models.CreateBankResponse{
		BankID: bank.Permalink,
		Status: "created",
	}, nil
}

// UpdateBank updates an existing Bank entity
// Maps to: PUT /api/banks/{bankId}
// Implements: BR-003 (Valid Updates - only existing banks can be updated)
// Source: APIMethods500.updateBank (code/api/v5_0_0/APIMethods500.scala:262)
func (s *BankService) UpdateBank(ctx context.Context, bankID string, req *models.UpdateBankRequest) (*models.UpdateBankResponse, error) {
	// VR-006: Validate bank ID path parameter
	if result := s.validator.ValidateBankIDPathRequired(bankID); !result.Valid {
		return nil, &ServiceError{
			Code:       result.Code,
			Message:    result.Message,
			HTTPStatus: 400,
			Field:      result.Field,
		}
	}

	// VR-010: Validate bank ID format
	if result := s.validator.ValidateBankIDFormat(bankID); !result.Valid {
		return nil, &ServiceError{
			Code:       result.Code,
			Message:    result.Message,
			HTTPStatus: 400,
			Field:      result.Field,
		}
	}

	// Validate update request
	validationResults := s.validator.ValidateUpdateBankRequest(req)
	if len(validationResults) > 0 {
		result := validationResults[0]
		return nil, &ServiceError{
			Code:       result.Code,
			Message:    result.Message,
			HTTPStatus: 400,
			Field:      result.Field,
		}
	}

	// BR-003: Verify bank exists before update
	existing, err := s.repo.GetBankByPermalink(ctx, bankID)
	if err != nil {
		if errors.Is(err, sql.ErrNoRows) {
			result := validators.NewBankNotFoundError(bankID)
			return nil, &ServiceError{
				Code:       result.Code,
				Message:    result.Message,
				HTTPStatus: 404,
				Field:      result.Field,
			}
		}
		return nil, &ServiceError{
			Code:       "BANK-ERR-001",
			Message:    "Failed to retrieve bank",
			HTTPStatus: 500,
		}
	}

	// Update fields if provided
	if req.BankName != nil {
		existing.FullBankName = *req.BankName
	}
	if req.Branding != nil {
		if req.Branding.Logo != "" {
			existing.LogoURL = req.Branding.Logo
		}
		if req.Branding.Colors != "" {
			existing.WebsiteURL = req.Branding.Colors
		}
	}

	// Persist via repository
	if err := s.repo.UpdateBank(ctx, existing); err != nil {
		return nil, &ServiceError{
			Code:       "BANK-ERR-003",
			Message:    "Failed to update bank: " + err.Error(),
			HTTPStatus: 500,
		}
	}

	return &models.UpdateBankResponse{
		BankID: existing.Permalink,
		Status: "updated",
	}, nil
}

// GetBank retrieves a bank by ID (internal helper)
func (s *BankService) GetBank(ctx context.Context, bankID string) (*models.MappedBank, error) {
	bank, err := s.repo.GetBankByPermalink(ctx, bankID)
	if err != nil {
		if errors.Is(err, sql.ErrNoRows) {
			result := validators.NewBankNotFoundError(bankID)
			return nil, &ServiceError{
				Code:       result.Code,
				Message:    result.Message,
				HTTPStatus: 404,
				Field:      result.Field,
			}
		}
		return nil, &ServiceError{
			Code:       "BANK-ERR-001",
			Message:    "Failed to retrieve bank",
			HTTPStatus: 500,
		}
	}
	return bank, nil
}

// ============================================================================
// Bank Information Retrieval Methods
// User Story: Bank Information Retrieval
// ============================================================================

// GetAllBanks retrieves all banks (basic info without attributes)
// Maps to: GET /banks
// Implements: BR-003 (Basic Bank Information Composition)
// Implements: BR-004 (Empty Result Handling - returns 200 with empty array, not 404)
// Source: LocalMappedConnector.getBanks (code/bankconnectors/LocalMappedConnector.scala)
func (s *BankService) GetAllBanks(ctx context.Context) (*models.BankListResponse, error) {
	// Retrieve all banks from repository
	banks, err := s.repo.GetAllBanks(ctx)
	if err != nil {
		return nil, &ServiceError{
			Code:       "BANK-ERR-001",
			Message:    "Failed to retrieve banks",
			HTTPStatus: 500,
		}
	}

	// BR-003: Convert to basic bank info (excludes attributes for performance)
	// BR-004: Return empty array if no banks exist (not 404)
	bankItems := make([]models.BankListItem, 0, len(banks))
	for _, bank := range banks {
		bankItems = append(bankItems, bank.ToBankListItem())
	}

	return &models.BankListResponse{
		Banks: bankItems,
	}, nil
}

// GetBankByIdWithAttributes retrieves a single bank with its attributes
// Maps to: GET /banks/BANK_ID
// Implements: BR-001 (Bank Existence Validation - returns 404 if not found)
// Implements: BR-002 (Complete Bank Information Composition - includes attributes)
// Implements: VR-001 (Bank Identifier Required Validation)
// Implements: VR-002 (Bank Identifier Existence Validation)
// Implements: VR-003 (Valid Bank Identifier Business Rule)
// Implements: VR-004 (Complete Information for Single Bank)
// Implements: VR-007 (Empty Attributes Array Handling)
// Implements: VR-008 (HTTP Status Code Validation)
// Source: LocalMappedConnector.getBank (code/bankconnectors/LocalMappedConnector.scala)
func (s *BankService) GetBankByIdWithAttributes(ctx context.Context, bankID string) (*models.BankDetailResponse, error) {
	// VR-001: Validate bank ID is provided (required)
	if bankID == "" {
		return nil, &ServiceError{
			Code:       "BANK-VAL-001",
			Message:    "Bank identifier is required",
			HTTPStatus: 400,
			Field:      "bankId",
		}
	}

	// VR-002, VR-003, BR-001: Check if bank exists
	bank, err := s.repo.GetBankByPermalink(ctx, bankID)
	if err != nil {
		if errors.Is(err, sql.ErrNoRows) {
			// VR-008: Return 404 for non-existent bank
			return nil, &ServiceError{
				Code:       "BANK-VAL-009",
				Message:    "Bank not found: " + bankID,
				HTTPStatus: 404,
				Field:      "bankId",
			}
		}
		return nil, &ServiceError{
			Code:       "BANK-ERR-001",
			Message:    "Failed to retrieve bank",
			HTTPStatus: 500,
		}
	}

	// BR-002: Retrieve bank attributes for complete information
	attributes, err := s.repo.GetBankAttributes(ctx, bankID)
	if err != nil {
		return nil, &ServiceError{
			Code:       "BANK-ERR-001",
			Message:    "Failed to retrieve bank attributes",
			HTTPStatus: 500,
		}
	}

	// VR-007: Convert attributes to response format (empty array if none)
	attrResponses := make([]models.BankAttributeResponse, 0, len(attributes))
	for _, attr := range attributes {
		attrResponses = append(attrResponses, attr.ToBankAttributeResponse())
	}

	// VR-004: Return complete bank information with all fields
	response := bank.ToBankDetailResponse(attrResponses)
	return &response, nil
}

// ============================================================================
// Bank Attribute Management Methods
// User Story: Bank Attribute Management
// ============================================================================

// BankAttributeService handles CRUD operations for bank attributes
type BankAttributeService struct {
	repo      repositories.BankRepository
	validator *validators.BankAttributeValidator
}

// NewBankAttributeService creates a new BankAttributeService instance
func NewBankAttributeService(repo repositories.BankRepository) *BankAttributeService {
	return &BankAttributeService{
		repo:      repo,
		validator: validators.NewBankAttributeValidator(),
	}
}

// CreateBankAttribute creates a new bank attribute
// Maps to: POST /banks/BANK_ID/attribute
// Implements: BR-001 (Bank Existence Validation)
// Implements: BR-002 (Attribute Type Validation)
// Implements: BR-003 (Type-Value Consistency)
// Implements: VR-003, VR-004, VR-011
// Source: MappedBankAttributeProvider.createOrUpdateBankAttribute
func (s *BankAttributeService) CreateBankAttribute(ctx context.Context, bankID string, req *models.CreateBankAttributeRequest) (*models.BankAttributeResponse, error) {
	// VR-001: Validate bank ID is provided
	if result := s.validator.ValidateBankIDRequired(bankID); !result.Valid {
		return nil, &ServiceError{
			Code:       result.Code,
			Message:    result.Message,
			HTTPStatus: 400,
			Field:      result.Field,
		}
	}

	// BR-001: Validate bank exists
	_, err := s.repo.GetBankByPermalink(ctx, bankID)
	if err != nil {
		if errors.Is(err, sql.ErrNoRows) {
			result := validators.NewBankNotFoundForAttributeError(bankID)
			return nil, &ServiceError{
				Code:       result.Code,
				Message:    result.Message,
				HTTPStatus: 404,
				Field:      result.Field,
			}
		}
		return nil, &ServiceError{
			Code:       "ATTR-ERR-001",
			Message:    "Failed to validate bank existence",
			HTTPStatus: 500,
		}
	}

	// VR-003, VR-004, BR-002, BR-003: Validate request
	validationResults := s.validator.ValidateCreateBankAttributeRequest(req)
	if len(validationResults) > 0 {
		result := validationResults[0]
		return nil, &ServiceError{
			Code:       result.Code,
			Message:    result.Message,
			HTTPStatus: 400,
			Field:      result.Field,
		}
	}

	// VR-011: Check for duplicate attribute name within bank
	exists, err := s.repo.AttributeExistsByName(ctx, bankID, req.Name)
	if err != nil {
		return nil, &ServiceError{
			Code:       "ATTR-ERR-001",
			Message:    "Failed to check attribute name uniqueness",
			HTTPStatus: 500,
		}
	}
	if exists {
		result := validators.NewAttributeNameDuplicateError(req.Name, bankID)
		return nil, &ServiceError{
			Code:       result.Code,
			Message:    result.Message,
			HTTPStatus: 409,
			Field:      result.Field,
		}
	}

	// BR-006: Set default is_active to true if not provided
	isActive := true
	if req.IsActive != nil {
		isActive = *req.IsActive
	}

	// Create attribute entity
	attr := models.NewBankAttribute(bankID, req.Name, req.Type, req.Value)
	attr.IsActive = isActive

	// Persist via repository
	if err := s.repo.CreateBankAttribute(ctx, attr); err != nil {
		return nil, &ServiceError{
			Code:       "ATTR-ERR-002",
			Message:    "Failed to create bank attribute: " + err.Error(),
			HTTPStatus: 500,
		}
	}

	// BR-007: Return complete attribute information
	response := attr.ToBankAttributeResponse()
	return &response, nil
}

// UpdateBankAttribute updates an existing bank attribute
// Maps to: PUT /banks/BANK_ID/attributes/BANK_ATTRIBUTE_ID
// Implements: BR-001 (Bank Existence Validation)
// Implements: BR-004 (Attribute Existence Validation)
// Implements: BR-002, BR-003, VR-012
// Source: MappedBankAttributeProvider.createOrUpdateBankAttribute
func (s *BankAttributeService) UpdateBankAttribute(ctx context.Context, bankID, attributeID string, req *models.UpdateBankAttributeRequest) (*models.BankAttributeResponse, error) {
	// VR-001: Validate bank ID is provided
	if result := s.validator.ValidateBankIDRequired(bankID); !result.Valid {
		return nil, &ServiceError{
			Code:       result.Code,
			Message:    result.Message,
			HTTPStatus: 400,
			Field:      result.Field,
		}
	}

	// VR-005: Validate attribute ID is provided
	if result := s.validator.ValidateAttributeIDRequired(attributeID); !result.Valid {
		return nil, &ServiceError{
			Code:       result.Code,
			Message:    result.Message,
			HTTPStatus: 400,
			Field:      result.Field,
		}
	}

	// BR-001: Validate bank exists
	_, err := s.repo.GetBankByPermalink(ctx, bankID)
	if err != nil {
		if errors.Is(err, sql.ErrNoRows) {
			result := validators.NewBankNotFoundForAttributeError(bankID)
			return nil, &ServiceError{
				Code:       result.Code,
				Message:    result.Message,
				HTTPStatus: 404,
				Field:      result.Field,
			}
		}
		return nil, &ServiceError{
			Code:       "ATTR-ERR-001",
			Message:    "Failed to validate bank existence",
			HTTPStatus: 500,
		}
	}

	// BR-004, VR-012: Validate attribute exists
	existing, err := s.repo.GetBankAttributeByID(ctx, bankID, attributeID)
	if err != nil {
		if errors.Is(err, sql.ErrNoRows) {
			result := validators.NewAttributeNotFoundError(attributeID, bankID)
			return nil, &ServiceError{
				Code:       result.Code,
				Message:    result.Message,
				HTTPStatus: 404,
				Field:      result.Field,
			}
		}
		return nil, &ServiceError{
			Code:       "ATTR-ERR-001",
			Message:    "Failed to retrieve attribute",
			HTTPStatus: 500,
		}
	}

	// VR-003, VR-004, BR-002, BR-003: Validate request
	validationResults := s.validator.ValidateUpdateBankAttributeRequest(req)
	if len(validationResults) > 0 {
		result := validationResults[0]
		return nil, &ServiceError{
			Code:       result.Code,
			Message:    result.Message,
			HTTPStatus: 400,
			Field:      result.Field,
		}
	}

	// VR-011: Check for duplicate attribute name within bank (excluding current attribute)
	exists, err := s.repo.AttributeExistsByNameExcluding(ctx, bankID, req.Name, attributeID)
	if err != nil {
		return nil, &ServiceError{
			Code:       "ATTR-ERR-001",
			Message:    "Failed to check attribute name uniqueness",
			HTTPStatus: 500,
		}
	}
	if exists {
		result := validators.NewAttributeNameDuplicateError(req.Name, bankID)
		return nil, &ServiceError{
			Code:       result.Code,
			Message:    result.Message,
			HTTPStatus: 409,
			Field:      result.Field,
		}
	}

	// Update attribute fields
	existing.Name = req.Name
	existing.Type = req.Type
	existing.Value = req.Value
	existing.IsActive = req.IsActive

	// Persist via repository
	if err := s.repo.UpdateBankAttribute(ctx, existing); err != nil {
		return nil, &ServiceError{
			Code:       "ATTR-ERR-003",
			Message:    "Failed to update bank attribute: " + err.Error(),
			HTTPStatus: 500,
		}
	}

	// BR-007: Return complete attribute information
	response := existing.ToBankAttributeResponse()
	return &response, nil
}

// GetBankAttributes retrieves all attributes for a bank
// Maps to: GET /banks/BANK_ID/attributes
// Implements: BR-001 (Bank Existence Validation)
// Implements: BR-005 (Empty Result Handling - returns 200 with empty array)
// Implements: VR-015 (Empty Attribute List Handling)
// Source: MappedBankAttributeProvider.getBankAttributesByBank
func (s *BankAttributeService) GetBankAttributes(ctx context.Context, bankID string) (*models.BankAttributesListResponse, error) {
	// VR-001: Validate bank ID is provided
	if result := s.validator.ValidateBankIDRequired(bankID); !result.Valid {
		return nil, &ServiceError{
			Code:       result.Code,
			Message:    result.Message,
			HTTPStatus: 400,
			Field:      result.Field,
		}
	}

	// BR-001: Validate bank exists
	_, err := s.repo.GetBankByPermalink(ctx, bankID)
	if err != nil {
		if errors.Is(err, sql.ErrNoRows) {
			result := validators.NewBankNotFoundForAttributeError(bankID)
			return nil, &ServiceError{
				Code:       result.Code,
				Message:    result.Message,
				HTTPStatus: 404,
				Field:      result.Field,
			}
		}
		return nil, &ServiceError{
			Code:       "ATTR-ERR-001",
			Message:    "Failed to validate bank existence",
			HTTPStatus: 500,
		}
	}

	// Retrieve attributes
	attributes, err := s.repo.GetBankAttributes(ctx, bankID)
	if err != nil {
		return nil, &ServiceError{
			Code:       "ATTR-ERR-001",
			Message:    "Failed to retrieve bank attributes",
			HTTPStatus: 500,
		}
	}

	// BR-005, VR-015: Convert to response format (empty array if none)
	attrResponses := make([]models.BankAttributeResponse, 0, len(attributes))
	for _, attr := range attributes {
		attrResponses = append(attrResponses, attr.ToBankAttributeResponse())
	}

	return &models.BankAttributesListResponse{
		BankAttributes: attrResponses,
	}, nil
}

// GetBankAttributeByID retrieves a single bank attribute by ID
// Maps to: GET /banks/BANK_ID/attributes/BANK_ATTRIBUTE_ID
// Implements: BR-001 (Bank Existence Validation)
// Implements: BR-004 (Attribute Existence Validation)
// Implements: BR-007 (Complete Attribute Information)
// Implements: VR-006 (Attribute ID Existence Validation)
// Source: MappedBankAttributeProvider.getBankAttributeById
func (s *BankAttributeService) GetBankAttributeByID(ctx context.Context, bankID, attributeID string) (*models.BankAttributeResponse, error) {
	// VR-001: Validate bank ID is provided
	if result := s.validator.ValidateBankIDRequired(bankID); !result.Valid {
		return nil, &ServiceError{
			Code:       result.Code,
			Message:    result.Message,
			HTTPStatus: 400,
			Field:      result.Field,
		}
	}

	// VR-005: Validate attribute ID is provided
	if result := s.validator.ValidateAttributeIDRequired(attributeID); !result.Valid {
		return nil, &ServiceError{
			Code:       result.Code,
			Message:    result.Message,
			HTTPStatus: 400,
			Field:      result.Field,
		}
	}

	// BR-001: Validate bank exists
	_, err := s.repo.GetBankByPermalink(ctx, bankID)
	if err != nil {
		if errors.Is(err, sql.ErrNoRows) {
			result := validators.NewBankNotFoundForAttributeError(bankID)
			return nil, &ServiceError{
				Code:       result.Code,
				Message:    result.Message,
				HTTPStatus: 404,
				Field:      result.Field,
			}
		}
		return nil, &ServiceError{
			Code:       "ATTR-ERR-001",
			Message:    "Failed to validate bank existence",
			HTTPStatus: 500,
		}
	}

	// BR-004, VR-006: Retrieve attribute
	attr, err := s.repo.GetBankAttributeByID(ctx, bankID, attributeID)
	if err != nil {
		if errors.Is(err, sql.ErrNoRows) {
			result := validators.NewAttributeNotFoundError(attributeID, bankID)
			return nil, &ServiceError{
				Code:       result.Code,
				Message:    result.Message,
				HTTPStatus: 404,
				Field:      result.Field,
			}
		}
		return nil, &ServiceError{
			Code:       "ATTR-ERR-001",
			Message:    "Failed to retrieve bank attribute",
			HTTPStatus: 500,
		}
	}

	// BR-007: Return complete attribute information
	response := attr.ToBankAttributeResponse()
	return &response, nil
}

// DeleteBankAttribute deletes a bank attribute
// Maps to: DELETE /banks/BANK_ID/attributes/BANK_ATTRIBUTE_ID
// Implements: BR-001 (Bank Existence Validation)
// Implements: BR-004 (Attribute Existence Validation)
// Implements: VR-013 (Existing Attribute for Deletion)
// Source: MappedBankAttributeProvider.deleteBankAttribute
func (s *BankAttributeService) DeleteBankAttribute(ctx context.Context, bankID, attributeID string) error {
	// VR-001: Validate bank ID is provided
	if result := s.validator.ValidateBankIDRequired(bankID); !result.Valid {
		return &ServiceError{
			Code:       result.Code,
			Message:    result.Message,
			HTTPStatus: 400,
			Field:      result.Field,
		}
	}

	// VR-005: Validate attribute ID is provided
	if result := s.validator.ValidateAttributeIDRequired(attributeID); !result.Valid {
		return &ServiceError{
			Code:       result.Code,
			Message:    result.Message,
			HTTPStatus: 400,
			Field:      result.Field,
		}
	}

	// BR-001: Validate bank exists
	_, err := s.repo.GetBankByPermalink(ctx, bankID)
	if err != nil {
		if errors.Is(err, sql.ErrNoRows) {
			result := validators.NewBankNotFoundForAttributeError(bankID)
			return &ServiceError{
				Code:       result.Code,
				Message:    result.Message,
				HTTPStatus: 404,
				Field:      result.Field,
			}
		}
		return &ServiceError{
			Code:       "ATTR-ERR-001",
			Message:    "Failed to validate bank existence",
			HTTPStatus: 500,
		}
	}

	// BR-004, VR-013: Delete attribute (returns error if not found)
	if err := s.repo.DeleteBankAttribute(ctx, bankID, attributeID); err != nil {
		if errors.Is(err, sql.ErrNoRows) {
			result := validators.NewAttributeNotFoundError(attributeID, bankID)
			return &ServiceError{
				Code:       result.Code,
				Message:    result.Message,
				HTTPStatus: 404,
				Field:      result.Field,
			}
		}
		return &ServiceError{
			Code:       "ATTR-ERR-004",
			Message:    "Failed to delete bank attribute: " + err.Error(),
			HTTPStatus: 500,
		}
	}

	return nil
}
