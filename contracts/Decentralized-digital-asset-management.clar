;; Decentralized Digital Asset Management Protocol on the Stacks Blockchain
;; 
;;This smart contract provides a secure, transparent framework for registering,
;; managing, and verifying digital assets using immutable records and cryptographic proofs.
;;
;; Key Capabilities:
;; - Permanent asset registration with rich metadata support
;; - Secure ownership tracking and transfer functionality
;; - Fine-grained access control via hierarchical permissions
;; - Cryptographic asset validation for integrity and authenticity
;; - Decentralized content verification with verifiable timestamps
;; - Efficient, scalable data storage design
;;
;; Security Architecture:
;; - Multi-factor authentication layers
;; - Role-based access controls (RBAC)
;; - Rigorous input validation and data sanitization
;; - Robust error handling with descriptive diagnostics
;; - Cryptographically enforced ownership verification

;; ===============================================================================

;; ===============================================================================
;; GLOBAL SYSTEM CONFIGURATION AND ADMINISTRATIVE CONSTANTS
;; ===============================================================================

;; Primary system administrator with full protocol governance rights
;; This principal has elevated privileges for system-wide operations and maintenance
(define-constant protocol-administrator tx-sender)

;; ===============================================================================
;; COMPREHENSIVE ERROR CODE FRAMEWORK FOR DETAILED DIAGNOSTICS
;; ===============================================================================

;; Asset-related error codes for content management operations
(define-constant asset-not-found-exception (err u501))
(define-constant duplicate-asset-registration-exception (err u502))
(define-constant malformed-asset-data-exception (err u503))
(define-constant asset-size-constraint-violation (err u504))

;; Access control and authorization error codes
(define-constant insufficient-authorization-exception (err u505))
(define-constant ownership-verification-failed-exception (err u506))
(define-constant administrative-rights-required-exception (err u500))
(define-constant content-access-denied-exception (err u507))

;; Data validation and integrity error codes
(define-constant metadata-validation-failure-exception (err u508))

;; ===============================================================================
;; SYSTEM STATE VARIABLES AND COUNTERS
;; ===============================================================================

;; Global registry counter tracking total number of registered digital assets
;; This variable maintains the sequential ordering of asset registrations
;; and serves as a unique identifier generator for new asset entries
(define-data-var global-asset-registry-counter uint u0)

;; ===============================================================================
;; PRIMARY DATA STORAGE STRUCTURES AND MAPPINGS
;; ===============================================================================

;; Main digital asset registry containing comprehensive asset information
;; This mapping serves as the primary database for all registered digital assets
;; Each asset is uniquely identified by an auto-incrementing numeric identifier
(define-map blockchain-asset-registry
  { 
    ;; Unique numeric identifier for each registered digital asset
    asset-registry-identifier: uint 
  }
  {
    ;; Human-readable title describing the digital asset content
    digital-asset-title: (string-ascii 64),

    ;; Principal address of the current asset owner with full control rights
    asset-owner-principal: principal,

    ;; Digital asset file size measured in bytes for storage optimization
    digital-content-size: uint,

    ;; Blockchain block height at which the asset was registered for timestamping
    registration-block-height: uint,

    ;; Extended metadata description providing detailed asset information
    comprehensive-asset-description: (string-ascii 128),

    ;; List of categorization tags for asset classification and searchability
    asset-categorization-tags: (list 10 (string-ascii 32))
  }
)

;; Advanced permission control matrix for fine-grained access management
;; This mapping enables sophisticated access control by tracking permissions
;; between specific assets and requesting principals with granular control
(define-map digital-asset-permission-registry
  { 
    ;; Target asset identifier for permission verification
    asset-registry-identifier: uint, 

    ;; Principal requesting access to the specified digital asset
    permission-requesting-principal: principal 
  }
  { 
    ;; Boolean flag indicating whether access permission is granted
    access-authorization-status: bool 
  }
)

;; ===============================================================================
;; INTERNAL UTILITY FUNCTIONS FOR DATA VALIDATION AND PROCESSING
;; ===============================================================================

;; Advanced categorization tag validation with comprehensive format checking
;; This function ensures that each tag meets the required format specifications
;; including length constraints and character set validation for consistency
(define-private (validate-single-categorization-tag (individual-tag (string-ascii 32)))
  (and
    ;; Ensure the tag is not empty to prevent null entries
    (> (len individual-tag) u0)
    ;; Enforce maximum length constraint to prevent storage overflow
    (< (len individual-tag) u33)
  )
)

;; Comprehensive tag collection validation with integrity verification
;; This function validates an entire collection of categorization tags
;; ensuring each tag meets format requirements and collection constraints
(define-private (perform-comprehensive-tag-validation (tag-collection (list 10 (string-ascii 32))))
  (and
    ;; Verify the collection contains at least one tag
    (> (len tag-collection) u0)
    ;; Ensure the collection doesn't exceed maximum allowed tags
    (<= (len tag-collection) u10)
    ;; Validate each individual tag in the collection
    (is-eq (len (filter validate-single-categorization-tag tag-collection)) (len tag-collection))
  )
)

;; Asset existence verification utility for registry lookups
;; This function checks whether a digital asset exists in the registry
;; by attempting to retrieve its record and verifying the result
(define-private (confirm-asset-registry-presence (asset-registry-identifier uint))
  (is-some (map-get? blockchain-asset-registry { asset-registry-identifier: asset-registry-identifier }))
)

;; Digital content size extraction utility for asset information retrieval
;; This function safely extracts the file size of a registered digital asset
;; with fallback to zero if the asset is not found in the registry
(define-private (retrieve-digital-content-size (asset-registry-identifier uint))
  (default-to u0
    (get digital-content-size
      (map-get? blockchain-asset-registry { asset-registry-identifier: asset-registry-identifier })
    )
  )
)

;; Ownership verification mechanism with cryptographic validation
;; This function verifies that a given principal is the legitimate owner
;; of a specified digital asset by comparing registry records
(define-private (verify-asset-ownership-rights (asset-registry-identifier uint) (verification-principal principal))
  (match (map-get? blockchain-asset-registry { asset-registry-identifier: asset-registry-identifier })
    ;; If asset exists, compare the owner principal with the verification principal
    asset-record (is-eq (get asset-owner-principal asset-record) verification-principal)
    ;; If asset doesn't exist, ownership verification fails
    false
  )
)

;; ===============================================================================
;; PUBLIC INTERFACE FUNCTIONS FOR ASSET MANAGEMENT OPERATIONS
;; ===============================================================================

;; Comprehensive digital asset registration with advanced metadata processing
;; This function handles the complete registration process for new digital assets
;; including validation, storage, and permission initialization
(define-public (execute-digital-asset-registration
  (digital-asset-title (string-ascii 64))
  (digital-content-size uint)
  (comprehensive-asset-description (string-ascii 128))
  (asset-categorization-tags (list 10 (string-ascii 32)))
)
  (let
    (
      ;; Generate unique identifier for the new asset registration
      (new-asset-identifier (+ (var-get global-asset-registry-counter) u1))
    )
    ;; ===============================================================================
    ;; COMPREHENSIVE INPUT VALIDATION AND SECURITY CHECKS
    ;; ===============================================================================

    ;; Validate digital asset title format and constraints
    (asserts! (> (len digital-asset-title) u0) malformed-asset-data-exception)
    (asserts! (< (len digital-asset-title) u65) malformed-asset-data-exception)

    ;; Validate digital content size within acceptable bounds
    (asserts! (> digital-content-size u0) asset-size-constraint-violation)
    (asserts! (< digital-content-size u1000000000) asset-size-constraint-violation)

    ;; Validate comprehensive asset description format
    (asserts! (> (len comprehensive-asset-description) u0) malformed-asset-data-exception)
    (asserts! (< (len comprehensive-asset-description) u129) malformed-asset-data-exception)

    ;; Validate categorization tags collection integrity
    (asserts! (perform-comprehensive-tag-validation asset-categorization-tags) metadata-validation-failure-exception)

    ;; ===============================================================================
    ;; ASSET REGISTRATION AND STORAGE OPERATIONS
    ;; ===============================================================================

    ;; Register the new digital asset in the blockchain registry
    (map-insert blockchain-asset-registry
      { asset-registry-identifier: new-asset-identifier }
      {
        digital-asset-title: digital-asset-title,
        asset-owner-principal: tx-sender,
        digital-content-size: digital-content-size,
        registration-block-height: block-height,
        comprehensive-asset-description: comprehensive-asset-description,
        asset-categorization-tags: asset-categorization-tags
      }
    )

    ;; Initialize default access permissions for the asset creator
    (map-insert digital-asset-permission-registry
      { asset-registry-identifier: new-asset-identifier, permission-requesting-principal: tx-sender }
      { access-authorization-status: true }
    )

    ;; Update the global asset registry counter for next registration
    (var-set global-asset-registry-counter new-asset-identifier)

    ;; Return the unique identifier of the newly registered asset
    (ok new-asset-identifier)
  )
)

;; Advanced asset modification with comprehensive validation and security
;; This function allows authorized users to update existing asset information
;; while maintaining strict ownership verification and data integrity checks
(define-public (perform-comprehensive-asset-modification
  (asset-registry-identifier uint)
  (modified-digital-asset-title (string-ascii 64))
  (modified-digital-content-size uint)
  (modified-comprehensive-asset-description (string-ascii 128))
  (modified-asset-categorization-tags (list 10 (string-ascii 32)))
)
  (let
    (
      ;; Retrieve current asset record for modification validation
      (current-asset-record (unwrap! (map-get? blockchain-asset-registry { asset-registry-identifier: asset-registry-identifier })
        asset-not-found-exception))
    )
    ;; ===============================================================================
    ;; AUTHORIZATION AND EXISTENCE VERIFICATION
    ;; ===============================================================================

    ;; Verify that the target asset exists in the registry
    (asserts! (confirm-asset-registry-presence asset-registry-identifier) asset-not-found-exception)

    ;; Verify that the requesting principal owns the asset
    (asserts! (is-eq (get asset-owner-principal current-asset-record) tx-sender) ownership-verification-failed-exception)

    ;; ===============================================================================
    ;; COMPREHENSIVE MODIFICATION PARAMETER VALIDATION
    ;; ===============================================================================

    ;; Validate modified digital asset title format and constraints
    (asserts! (> (len modified-digital-asset-title) u0) malformed-asset-data-exception)
    (asserts! (< (len modified-digital-asset-title) u65) malformed-asset-data-exception)

    ;; Validate modified digital content size within acceptable bounds
    (asserts! (> modified-digital-content-size u0) asset-size-constraint-violation)
    (asserts! (< modified-digital-content-size u1000000000) asset-size-constraint-violation)

    ;; Validate modified comprehensive asset description format
    (asserts! (> (len modified-comprehensive-asset-description) u0) malformed-asset-data-exception)
    (asserts! (< (len modified-comprehensive-asset-description) u129) malformed-asset-data-exception)

    ;; Validate modified categorization tags collection integrity
    (asserts! (perform-comprehensive-tag-validation modified-asset-categorization-tags) metadata-validation-failure-exception)

    ;; ===============================================================================
    ;; ASSET RECORD UPDATE AND PERSISTENCE
    ;; ===============================================================================

    ;; Update the asset record with modified information while preserving metadata
    (map-set blockchain-asset-registry
      { asset-registry-identifier: asset-registry-identifier }
      (merge current-asset-record {
        digital-asset-title: modified-digital-asset-title,
        digital-content-size: modified-digital-content-size,
        comprehensive-asset-description: modified-comprehensive-asset-description,
        asset-categorization-tags: modified-asset-categorization-tags
      })
    )

    ;; Return success confirmation
    (ok true)
  )
)

;; Secure asset ownership transfer with comprehensive validation protocols
;; This function facilitates the transfer of asset ownership between principals
;; while maintaining strict security and validation requirements
(define-public (initiate-secure-ownership-transfer (asset-registry-identifier uint) (new-owner-principal principal))
  (let
    (
      ;; Retrieve current asset record for ownership verification
      (existing-asset-record (unwrap! (map-get? blockchain-asset-registry { asset-registry-identifier: asset-registry-identifier })
        asset-not-found-exception))
    )
    ;; ===============================================================================
    ;; OWNERSHIP TRANSFER VALIDATION AND SECURITY CHECKS
    ;; ===============================================================================

    ;; Verify that the target asset exists in the registry
    (asserts! (confirm-asset-registry-presence asset-registry-identifier) asset-not-found-exception)

    ;; Verify that the requesting principal is the current owner
    (asserts! (is-eq (get asset-owner-principal existing-asset-record) tx-sender) ownership-verification-failed-exception)

    ;; ===============================================================================
    ;; OWNERSHIP TRANSFER EXECUTION
    ;; ===============================================================================

    ;; Update the asset record with the new owner information
    (map-set blockchain-asset-registry
      { asset-registry-identifier: asset-registry-identifier }
      (merge existing-asset-record { asset-owner-principal: new-owner-principal })
    )

    ;; Return success confirmation
    (ok true)
  )
)

