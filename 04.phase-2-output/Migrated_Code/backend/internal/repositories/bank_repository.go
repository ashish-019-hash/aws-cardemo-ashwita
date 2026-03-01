package repositories

import (
	"context"
	"database/sql"
	"time"

	"github.com/obp-api/bank-registration/internal/models"
)

// BankRepository handles data persistence for Bank entities
// Maps to: User Story "BankRepository: Data persistence for Bank entities"
type BankRepository interface {
	// CreateBank creates a new bank in the database
	// Returns error if bank with same permalink or shortbankname already exists
	CreateBank(ctx context.Context, bank *models.MappedBank) error

	// UpdateBank updates an existing bank in the database
	UpdateBank(ctx context.Context, bank *models.MappedBank) error

	// GetBankByPermalink retrieves a bank by its permalink (bank ID)
	// Source: MappedBank.findByBankId (code/model/dataAccess/MappedBank.scala:37)
	GetBankByPermalink(ctx context.Context, permalink string) (*models.MappedBank, error)

	// GetBankByCode retrieves a bank by its short name/code
	GetBankByCode(ctx context.Context, code string) (*models.MappedBank, error)

	// BankExistsByPermalink checks if a bank exists by permalink
	BankExistsByPermalink(ctx context.Context, permalink string) (bool, error)

	// BankExistsByCode checks if a bank exists by code
	BankExistsByCode(ctx context.Context, code string) (bool, error)

	// ============================================================================
	// Bank Information Retrieval Methods
	// User Story: Bank Information Retrieval
	// ============================================================================

	// GetAllBanks retrieves all banks from the database
	// Source: LocalMappedConnector.getBanks (code/bankconnectors/LocalMappedConnector.scala)
	// BR-003: Returns basic bank info (excludes attributes for performance)
	// BR-004: Returns empty slice if no banks exist (not error)
	GetAllBanks(ctx context.Context) ([]*models.MappedBank, error)

	// GetBankAttributes retrieves all attributes for a specific bank
	// Source: MappedBankAttributeProvider.getBankAttributesByBank
	// VR-007: Returns empty slice if no attributes exist (not nil)
	GetBankAttributes(ctx context.Context, bankID string) ([]*models.BankAttribute, error)

	// CreateBankAttribute creates a new bank attribute
	// Used for testing and seeding data
	CreateBankAttribute(ctx context.Context, attr *models.BankAttribute) error

	// ============================================================================
	// Bank Attribute Management Methods
	// User Story: Bank Attribute Management
	// ============================================================================

	// GetBankAttributeByID retrieves a single bank attribute by its ID
	// BR-004: Returns error if attribute does not exist
	// VR-006: Validates attribute exists for the specified bank
	GetBankAttributeByID(ctx context.Context, bankID, attributeID string) (*models.BankAttribute, error)

	// UpdateBankAttribute updates an existing bank attribute
	// BR-004: Attribute must exist before update
	// VR-012: Validates attribute exists for updates
	UpdateBankAttribute(ctx context.Context, attr *models.BankAttribute) error

	// DeleteBankAttribute deletes a bank attribute
	// BR-004: Attribute must exist before deletion
	// VR-013: Validates attribute exists for deletion
	DeleteBankAttribute(ctx context.Context, bankID, attributeID string) error

	// AttributeExistsByName checks if an attribute with the given name exists for a bank
	// VR-011: Unique attribute name within bank validation
	AttributeExistsByName(ctx context.Context, bankID, name string) (bool, error)

	// AttributeExistsByNameExcluding checks if an attribute with the given name exists for a bank
	// excluding a specific attribute ID (for updates)
	AttributeExistsByNameExcluding(ctx context.Context, bankID, name, excludeAttributeID string) (bool, error)

	// ============================================================================
	// Multi-Bank Support Methods
	// User Story: Multi-Bank Support
	// ============================================================================

	// GetBankAccounts retrieves all accounts for a specific bank
	// BR-003: Data Isolation Enforcement - returns only accounts for specified bank
	// BR-004: Returns empty slice if no accounts exist (not error)
	GetBankAccounts(ctx context.Context, bankID string) ([]*models.MappedBankAccount, error)

	// CreateBankAccount creates a new bank account
	// BR-006: Bank-Scoped Resource Ownership - account is permanently associated with bank
	CreateBankAccount(ctx context.Context, account *models.MappedBankAccount) error

	// GetBankEntitlements retrieves all entitlements for a specific bank
	// BR-005: Bank-Scoped Entitlements - returns only entitlements for specified bank
	// BR-004: Returns empty slice if no entitlements exist (not error)
	GetBankEntitlements(ctx context.Context, bankID string) ([]*models.MappedEntitlement, error)

	// CreateBankEntitlement creates a new bank entitlement
	// BR-005: Bank-Scoped Entitlements - entitlement is associated with specific bank
	CreateBankEntitlement(ctx context.Context, entitlement *models.MappedEntitlement) error

	// GetUserEntitlementForBank checks if a user has a specific entitlement for a bank
	// BR-005: Bank-Scoped Entitlements - permissions are scoped to specific banks
	// VR-008: Bank-Specific Entitlement Validation
	GetUserEntitlementForBank(ctx context.Context, userID, bankID, roleName string) (*models.MappedEntitlement, error)
}

// bankRepository implements BankRepository
// Source: code/bankconnectors/LocalMappedConnector.scala:3169
type bankRepository struct {
	db *sql.DB
}

// NewBankRepository creates a new BankRepository instance
func NewBankRepository(db *sql.DB) BankRepository {
	return &bankRepository{db: db}
}

// CreateBank creates a new bank in the database
// Source: LocalMappedConnector.createOrUpdateBank (code/bankconnectors/LocalMappedConnector.scala:3169)
func (r *bankRepository) CreateBank(ctx context.Context, bank *models.MappedBank) error {
	now := time.Now()
	bank.CreatedAt = now
	bank.UpdatedAt = now

	query := `
		INSERT INTO mappedbank (
			permalink, fullbankname, shortbankname, logourl, websiteurl,
			swiftbic, national_identifier, mbankroutingscheme, mbankroutingaddress,
			createdat, updatedat
		) VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)
	`
	result, err := r.db.ExecContext(ctx, query,
		bank.Permalink,
		bank.FullBankName,
		bank.ShortBankName,
		bank.LogoURL,
		bank.WebsiteURL,
		bank.SwiftBIC,
		bank.NationalIdentifier,
		bank.BankRoutingScheme,
		bank.BankRoutingAddress,
		bank.CreatedAt,
		bank.UpdatedAt,
	)
	if err != nil {
		return err
	}

	id, err := result.LastInsertId()
	if err != nil {
		return err
	}
	bank.ID = id
	return nil
}

// UpdateBank updates an existing bank in the database
func (r *bankRepository) UpdateBank(ctx context.Context, bank *models.MappedBank) error {
	bank.UpdatedAt = time.Now()

	query := `
		UPDATE mappedbank SET
			fullbankname = ?,
			shortbankname = ?,
			logourl = ?,
			websiteurl = ?,
			swiftbic = ?,
			national_identifier = ?,
			mbankroutingscheme = ?,
			mbankroutingaddress = ?,
			updatedat = ?
		WHERE permalink = ?
	`
	_, err := r.db.ExecContext(ctx, query,
		bank.FullBankName,
		bank.ShortBankName,
		bank.LogoURL,
		bank.WebsiteURL,
		bank.SwiftBIC,
		bank.NationalIdentifier,
		bank.BankRoutingScheme,
		bank.BankRoutingAddress,
		bank.UpdatedAt,
		bank.Permalink,
	)
	return err
}

// GetBankByPermalink retrieves a bank by its permalink (bank ID)
// Source: MappedBank.findByBankId (code/model/dataAccess/MappedBank.scala:37)
func (r *bankRepository) GetBankByPermalink(ctx context.Context, permalink string) (*models.MappedBank, error) {
	query := `
		SELECT id, permalink, fullbankname, shortbankname, logourl, websiteurl,
			   swiftbic, national_identifier, mbankroutingscheme, mbankroutingaddress,
			   createdat, updatedat
		FROM mappedbank
		WHERE permalink = ?
	`

	bank := &models.MappedBank{}
	err := r.db.QueryRowContext(ctx, query, permalink).Scan(
		&bank.ID,
		&bank.Permalink,
		&bank.FullBankName,
		&bank.ShortBankName,
		&bank.LogoURL,
		&bank.WebsiteURL,
		&bank.SwiftBIC,
		&bank.NationalIdentifier,
		&bank.BankRoutingScheme,
		&bank.BankRoutingAddress,
		&bank.CreatedAt,
		&bank.UpdatedAt,
	)
	if err != nil {
		return nil, err
	}
	return bank, nil
}

// GetBankByCode retrieves a bank by its short name/code
func (r *bankRepository) GetBankByCode(ctx context.Context, code string) (*models.MappedBank, error) {
	query := `
		SELECT id, permalink, fullbankname, shortbankname, logourl, websiteurl,
			   swiftbic, national_identifier, mbankroutingscheme, mbankroutingaddress,
			   createdat, updatedat
		FROM mappedbank
		WHERE shortbankname = ?
	`

	bank := &models.MappedBank{}
	err := r.db.QueryRowContext(ctx, query, code).Scan(
		&bank.ID,
		&bank.Permalink,
		&bank.FullBankName,
		&bank.ShortBankName,
		&bank.LogoURL,
		&bank.WebsiteURL,
		&bank.SwiftBIC,
		&bank.NationalIdentifier,
		&bank.BankRoutingScheme,
		&bank.BankRoutingAddress,
		&bank.CreatedAt,
		&bank.UpdatedAt,
	)
	if err != nil {
		return nil, err
	}
	return bank, nil
}

// BankExistsByPermalink checks if a bank exists by permalink
func (r *bankRepository) BankExistsByPermalink(ctx context.Context, permalink string) (bool, error) {
	query := `SELECT COUNT(*) FROM mappedbank WHERE permalink = ?`
	var count int
	err := r.db.QueryRowContext(ctx, query, permalink).Scan(&count)
	if err != nil {
		return false, err
	}
	return count > 0, nil
}

// BankExistsByCode checks if a bank exists by code
func (r *bankRepository) BankExistsByCode(ctx context.Context, code string) (bool, error) {
	query := `SELECT COUNT(*) FROM mappedbank WHERE shortbankname = ?`
	var count int
	err := r.db.QueryRowContext(ctx, query, code).Scan(&count)
	if err != nil {
		return false, err
	}
	return count > 0, nil
}

// ============================================================================
// Bank Information Retrieval Methods Implementation
// User Story: Bank Information Retrieval
// ============================================================================

// GetAllBanks retrieves all banks from the database
// Source: LocalMappedConnector.getBanks (code/bankconnectors/LocalMappedConnector.scala)
// BR-003: Returns basic bank info (excludes attributes for performance)
// BR-004: Returns empty slice if no banks exist (not error)
func (r *bankRepository) GetAllBanks(ctx context.Context) ([]*models.MappedBank, error) {
	query := `
		SELECT id, permalink, fullbankname, shortbankname, logourl, websiteurl,
			   swiftbic, national_identifier, mbankroutingscheme, mbankroutingaddress,
			   createdat, updatedat
		FROM mappedbank
		ORDER BY fullbankname ASC
	`

	rows, err := r.db.QueryContext(ctx, query)
	if err != nil {
		return nil, err
	}
	defer rows.Close()

	// BR-004: Initialize as empty slice, not nil
	banks := make([]*models.MappedBank, 0)

	for rows.Next() {
		bank := &models.MappedBank{}
		err := rows.Scan(
			&bank.ID,
			&bank.Permalink,
			&bank.FullBankName,
			&bank.ShortBankName,
			&bank.LogoURL,
			&bank.WebsiteURL,
			&bank.SwiftBIC,
			&bank.NationalIdentifier,
			&bank.BankRoutingScheme,
			&bank.BankRoutingAddress,
			&bank.CreatedAt,
			&bank.UpdatedAt,
		)
		if err != nil {
			return nil, err
		}
		banks = append(banks, bank)
	}

	if err := rows.Err(); err != nil {
		return nil, err
	}

	return banks, nil
}

// GetBankAttributes retrieves all attributes for a specific bank
// Source: MappedBankAttributeProvider.getBankAttributesByBank
// VR-007: Returns empty slice if no attributes exist (not nil)
func (r *bankRepository) GetBankAttributes(ctx context.Context, bankID string) ([]*models.BankAttribute, error) {
	query := `
		SELECT id, bankid, bankattributeid, name, type, value, isactive
		FROM bankattribute
		WHERE bankid = ?
		ORDER BY name ASC
	`

	rows, err := r.db.QueryContext(ctx, query, bankID)
	if err != nil {
		return nil, err
	}
	defer rows.Close()

	// VR-007: Initialize as empty slice, not nil
	attributes := make([]*models.BankAttribute, 0)

	for rows.Next() {
		attr := &models.BankAttribute{}
		err := rows.Scan(
			&attr.ID,
			&attr.BankID,
			&attr.BankAttributeID,
			&attr.Name,
			&attr.Type,
			&attr.Value,
			&attr.IsActive,
		)
		if err != nil {
			return nil, err
		}
		attributes = append(attributes, attr)
	}

	if err := rows.Err(); err != nil {
		return nil, err
	}

	return attributes, nil
}

// CreateBankAttribute creates a new bank attribute
// Used for testing and seeding data
func (r *bankRepository) CreateBankAttribute(ctx context.Context, attr *models.BankAttribute) error {
	query := `
		INSERT INTO bankattribute (bankid, bankattributeid, name, type, value, isactive)
		VALUES (?, ?, ?, ?, ?, ?)
	`
	result, err := r.db.ExecContext(ctx, query,
		attr.BankID,
		attr.BankAttributeID,
		attr.Name,
		attr.Type,
		attr.Value,
		attr.IsActive,
	)
	if err != nil {
		return err
	}

	id, err := result.LastInsertId()
	if err != nil {
		return err
	}
	attr.ID = id
	return nil
}

// ============================================================================
// Bank Attribute Management Methods Implementation
// User Story: Bank Attribute Management
// ============================================================================

// GetBankAttributeByID retrieves a single bank attribute by its ID
// BR-004: Returns error if attribute does not exist
// VR-006: Validates attribute exists for the specified bank
func (r *bankRepository) GetBankAttributeByID(ctx context.Context, bankID, attributeID string) (*models.BankAttribute, error) {
	query := `
		SELECT id, bankid, bankattributeid, name, type, value, isactive
		FROM bankattribute
		WHERE bankid = ? AND bankattributeid = ?
	`

	attr := &models.BankAttribute{}
	err := r.db.QueryRowContext(ctx, query, bankID, attributeID).Scan(
		&attr.ID,
		&attr.BankID,
		&attr.BankAttributeID,
		&attr.Name,
		&attr.Type,
		&attr.Value,
		&attr.IsActive,
	)
	if err != nil {
		return nil, err
	}
	return attr, nil
}

// UpdateBankAttribute updates an existing bank attribute
// BR-004: Attribute must exist before update
// VR-012: Validates attribute exists for updates
func (r *bankRepository) UpdateBankAttribute(ctx context.Context, attr *models.BankAttribute) error {
	query := `
		UPDATE bankattribute SET
			name = ?,
			type = ?,
			value = ?,
			isactive = ?
		WHERE bankid = ? AND bankattributeid = ?
	`
	result, err := r.db.ExecContext(ctx, query,
		attr.Name,
		attr.Type,
		attr.Value,
		attr.IsActive,
		attr.BankID,
		attr.BankAttributeID,
	)
	if err != nil {
		return err
	}

	rowsAffected, err := result.RowsAffected()
	if err != nil {
		return err
	}
	if rowsAffected == 0 {
		return sql.ErrNoRows
	}
	return nil
}

// DeleteBankAttribute deletes a bank attribute
// BR-004: Attribute must exist before deletion
// VR-013: Validates attribute exists for deletion
func (r *bankRepository) DeleteBankAttribute(ctx context.Context, bankID, attributeID string) error {
	query := `DELETE FROM bankattribute WHERE bankid = ? AND bankattributeid = ?`
	result, err := r.db.ExecContext(ctx, query, bankID, attributeID)
	if err != nil {
		return err
	}

	rowsAffected, err := result.RowsAffected()
	if err != nil {
		return err
	}
	if rowsAffected == 0 {
		return sql.ErrNoRows
	}
	return nil
}

// AttributeExistsByName checks if an attribute with the given name exists for a bank
// VR-011: Unique attribute name within bank validation
func (r *bankRepository) AttributeExistsByName(ctx context.Context, bankID, name string) (bool, error) {
	query := `SELECT COUNT(*) FROM bankattribute WHERE bankid = ? AND name = ?`
	var count int
	err := r.db.QueryRowContext(ctx, query, bankID, name).Scan(&count)
	if err != nil {
		return false, err
	}
	return count > 0, nil
}

// AttributeExistsByNameExcluding checks if an attribute with the given name exists for a bank
// excluding a specific attribute ID (for updates)
func (r *bankRepository) AttributeExistsByNameExcluding(ctx context.Context, bankID, name, excludeAttributeID string) (bool, error) {
	query := `SELECT COUNT(*) FROM bankattribute WHERE bankid = ? AND name = ? AND bankattributeid != ?`
	var count int
	err := r.db.QueryRowContext(ctx, query, bankID, name, excludeAttributeID).Scan(&count)
	if err != nil {
		return false, err
	}
	return count > 0, nil
}

// ============================================================================
// Multi-Bank Support Methods Implementation
// User Story: Multi-Bank Support
// ============================================================================

// GetBankAccounts retrieves all accounts for a specific bank
// BR-003: Data Isolation Enforcement - returns only accounts for specified bank
// BR-004: Returns empty slice if no accounts exist (not error)
func (r *bankRepository) GetBankAccounts(ctx context.Context, bankID string) ([]*models.MappedBankAccount, error) {
	query := `
		SELECT id, bank, theaccountid, accountcurrency, accountbalance, accountlabel, kind, createdat, updatedat
		FROM mappedbankaccount
		WHERE bank = ?
		ORDER BY theaccountid ASC
	`

	rows, err := r.db.QueryContext(ctx, query, bankID)
	if err != nil {
		return nil, err
	}
	defer rows.Close()

	// BR-004: Initialize as empty slice, not nil
	accounts := make([]*models.MappedBankAccount, 0)

	for rows.Next() {
		account := &models.MappedBankAccount{}
		err := rows.Scan(
			&account.ID,
			&account.BankID,
			&account.AccountID,
			&account.Currency,
			&account.Balance,
			&account.Label,
			&account.Kind,
			&account.CreatedAt,
			&account.UpdatedAt,
		)
		if err != nil {
			return nil, err
		}
		accounts = append(accounts, account)
	}

	if err := rows.Err(); err != nil {
		return nil, err
	}

	return accounts, nil
}

// CreateBankAccount creates a new bank account
// BR-006: Bank-Scoped Resource Ownership - account is permanently associated with bank
func (r *bankRepository) CreateBankAccount(ctx context.Context, account *models.MappedBankAccount) error {
	now := time.Now()
	account.CreatedAt = now
	account.UpdatedAt = now

	query := `
		INSERT INTO mappedbankaccount (bank, theaccountid, accountcurrency, accountbalance, accountlabel, kind, createdat, updatedat)
		VALUES (?, ?, ?, ?, ?, ?, ?, ?)
	`
	result, err := r.db.ExecContext(ctx, query,
		account.BankID,
		account.AccountID,
		account.Currency,
		account.Balance,
		account.Label,
		account.Kind,
		account.CreatedAt,
		account.UpdatedAt,
	)
	if err != nil {
		return err
	}

	id, err := result.LastInsertId()
	if err != nil {
		return err
	}
	account.ID = id
	return nil
}

// GetBankEntitlements retrieves all entitlements for a specific bank
// BR-005: Bank-Scoped Entitlements - returns only entitlements for specified bank
// BR-004: Returns empty slice if no entitlements exist (not error)
func (r *bankRepository) GetBankEntitlements(ctx context.Context, bankID string) ([]*models.MappedEntitlement, error) {
	query := `
		SELECT id, entitlementid, mbankid, muserid, mrolename, createdat, updatedat
		FROM mappedentitlement
		WHERE mbankid = ?
		ORDER BY mrolename ASC
	`

	rows, err := r.db.QueryContext(ctx, query, bankID)
	if err != nil {
		return nil, err
	}
	defer rows.Close()

	// BR-004: Initialize as empty slice, not nil
	entitlements := make([]*models.MappedEntitlement, 0)

	for rows.Next() {
		entitlement := &models.MappedEntitlement{}
		err := rows.Scan(
			&entitlement.ID,
			&entitlement.EntitlementID,
			&entitlement.BankID,
			&entitlement.UserID,
			&entitlement.RoleName,
			&entitlement.CreatedAt,
			&entitlement.UpdatedAt,
		)
		if err != nil {
			return nil, err
		}
		entitlements = append(entitlements, entitlement)
	}

	if err := rows.Err(); err != nil {
		return nil, err
	}

	return entitlements, nil
}

// CreateBankEntitlement creates a new bank entitlement
// BR-005: Bank-Scoped Entitlements - entitlement is associated with specific bank
func (r *bankRepository) CreateBankEntitlement(ctx context.Context, entitlement *models.MappedEntitlement) error {
	now := time.Now()
	entitlement.CreatedAt = now
	entitlement.UpdatedAt = now

	query := `
		INSERT INTO mappedentitlement (entitlementid, mbankid, muserid, mrolename, createdat, updatedat)
		VALUES (?, ?, ?, ?, ?, ?)
	`
	result, err := r.db.ExecContext(ctx, query,
		entitlement.EntitlementID,
		entitlement.BankID,
		entitlement.UserID,
		entitlement.RoleName,
		entitlement.CreatedAt,
		entitlement.UpdatedAt,
	)
	if err != nil {
		return err
	}

	id, err := result.LastInsertId()
	if err != nil {
		return err
	}
	entitlement.ID = id
	return nil
}

// GetUserEntitlementForBank checks if a user has a specific entitlement for a bank
// BR-005: Bank-Scoped Entitlements - permissions are scoped to specific banks
// VR-008: Bank-Specific Entitlement Validation
func (r *bankRepository) GetUserEntitlementForBank(ctx context.Context, userID, bankID, roleName string) (*models.MappedEntitlement, error) {
	query := `
		SELECT id, entitlementid, mbankid, muserid, mrolename, createdat, updatedat
		FROM mappedentitlement
		WHERE muserid = ? AND mbankid = ? AND mrolename = ?
	`

	entitlement := &models.MappedEntitlement{}
	err := r.db.QueryRowContext(ctx, query, userID, bankID, roleName).Scan(
		&entitlement.ID,
		&entitlement.EntitlementID,
		&entitlement.BankID,
		&entitlement.UserID,
		&entitlement.RoleName,
		&entitlement.CreatedAt,
		&entitlement.UpdatedAt,
	)
	if err != nil {
		return nil, err
	}
	return entitlement, nil
}
