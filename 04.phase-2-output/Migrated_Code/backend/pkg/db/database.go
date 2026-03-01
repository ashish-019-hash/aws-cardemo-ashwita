package db

import (
	"database/sql"
	"sync"

	_ "github.com/mattn/go-sqlite3"
)

var (
	instance *sql.DB
	once     sync.Once
)

// GetDB returns the singleton database connection
// Uses SQLite in-memory mode as specified in the playbook
func GetDB() (*sql.DB, error) {
	var err error
	once.Do(func() {
		instance, err = sql.Open("sqlite3", ":memory:")
		if err != nil {
			return
		}
		err = initSchema(instance)
	})
	return instance, err
}

// initSchema creates the database schema
func initSchema(db *sql.DB) error {
	schema := `
	CREATE TABLE IF NOT EXISTS mappedbank (
		id INTEGER PRIMARY KEY AUTOINCREMENT,
		permalink TEXT NOT NULL UNIQUE,
		fullbankname TEXT NOT NULL,
		shortbankname TEXT NOT NULL UNIQUE,
		logourl TEXT,
		websiteurl TEXT,
		swiftbic TEXT,
		national_identifier TEXT,
		mbankroutingscheme TEXT,
		mbankroutingaddress TEXT,
		createdat DATETIME NOT NULL,
		updatedat DATETIME NOT NULL
	);

	CREATE INDEX IF NOT EXISTS idx_mappedbank_permalink ON mappedbank(permalink);
	CREATE INDEX IF NOT EXISTS idx_mappedbank_shortbankname ON mappedbank(shortbankname);

	-- BankAttribute table for Bank Information Retrieval user story
	-- Source: code/bankattribute/MappedBankAttributeProvider.scala
	-- Relationship: Many-to-One with MappedBank (multiple BankAttribute records belong to one MappedBank)
	CREATE TABLE IF NOT EXISTS bankattribute (
		id INTEGER PRIMARY KEY AUTOINCREMENT,
		bankid TEXT NOT NULL,
		bankattributeid TEXT NOT NULL UNIQUE,
		name TEXT NOT NULL,
		type TEXT NOT NULL,
		value TEXT NOT NULL,
		isactive INTEGER NOT NULL DEFAULT 1,
		FOREIGN KEY (bankid) REFERENCES mappedbank(permalink)
	);

	CREATE INDEX IF NOT EXISTS idx_bankattribute_bankid ON bankattribute(bankid);

	-- MappedBankAccount table for Multi-Bank Support user story
	-- Source: code/model/dataAccess/MappedBankAccount.scala
	-- Relationship: Many-to-One with MappedBank (multiple accounts belong to one bank)
	-- BR-003: Data Isolation Enforcement - accounts are scoped to specific bank
	-- BR-006: Bank-Scoped Resource Ownership - account is permanently associated with bank
	CREATE TABLE IF NOT EXISTS mappedbankaccount (
		id INTEGER PRIMARY KEY AUTOINCREMENT,
		bank TEXT NOT NULL,
		theaccountid TEXT NOT NULL,
		accountcurrency TEXT NOT NULL,
		accountbalance REAL NOT NULL DEFAULT 0,
		accountlabel TEXT,
		kind TEXT NOT NULL,
		createdat DATETIME NOT NULL,
		updatedat DATETIME NOT NULL,
		FOREIGN KEY (bank) REFERENCES mappedbank(permalink),
		UNIQUE(bank, theaccountid)
	);

	CREATE INDEX IF NOT EXISTS idx_mappedbankaccount_bank ON mappedbankaccount(bank);

	-- MappedEntitlement table for Multi-Bank Support user story
	-- Source: code/entitlement/MappedEntitlementsProvider.scala
	-- Relationship: Many-to-One with MappedBank (multiple entitlements belong to one bank)
	-- BR-005: Bank-Scoped Entitlements - permissions are scoped to specific banks
	CREATE TABLE IF NOT EXISTS mappedentitlement (
		id INTEGER PRIMARY KEY AUTOINCREMENT,
		entitlementid TEXT NOT NULL UNIQUE,
		mbankid TEXT NOT NULL,
		muserid TEXT NOT NULL,
		mrolename TEXT NOT NULL,
		createdat DATETIME NOT NULL,
		updatedat DATETIME NOT NULL,
		FOREIGN KEY (mbankid) REFERENCES mappedbank(permalink),
		UNIQUE(mbankid, muserid, mrolename)
	);

	CREATE INDEX IF NOT EXISTS idx_mappedentitlement_mbankid ON mappedentitlement(mbankid);
	CREATE INDEX IF NOT EXISTS idx_mappedentitlement_muserid ON mappedentitlement(muserid);
	`
	_, err := db.Exec(schema)
	return err
}

// NewTestDB creates a new in-memory database for testing
// Each test gets its own isolated database instance
func NewTestDB() (*sql.DB, error) {
	db, err := sql.Open("sqlite3", ":memory:")
	if err != nil {
		return nil, err
	}
	err = initSchema(db)
	if err != nil {
		db.Close()
		return nil, err
	}
	return db, nil
}

// CloseDB closes the database connection
func CloseDB() error {
	if instance != nil {
		return instance.Close()
	}
	return nil
}
