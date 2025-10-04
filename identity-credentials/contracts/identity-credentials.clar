;; Identity Credentials - Decentralized Professional Identity Network
;; A smart contract for managing professional identities, skills, and reputation

;; Constants
(define-constant contract-owner tx-sender)
(define-constant err-owner-only (err u100))
(define-constant err-not-found (err u101))
(define-constant err-already-exists (err u102))
(define-constant err-unauthorized (err u103))
(define-constant err-invalid-input (err u104))
(define-constant err-expired (err u105))

;; Data Variables
(define-data-var reputation-decay-rate uint u1)
(define-data-var min-endorsement-stake uint u1000000) ;; 1 STX minimum

;; Data Maps

;; Professional Identity Storage
(define-map professional-identities
    principal
    {
        reputation-score: uint,
        total-endorsements: uint,
        verified-skills: uint,
        registration-height: uint,
        is-active: bool
    }
)

;; Skill Attestations
(define-map skill-attestations
    {professional: principal, skill-id: uint}
    {
        skill-name: (string-ascii 50),
        proof-hash: (buff 32),
        attestation-height: uint,
        verifier: principal,
        confidence-score: uint,
        is-verified: bool
    }
)

;; Endorsements
(define-map endorsements
    {endorser: principal, endorsed: principal, endorsement-id: uint}
    {
        skill-id: uint,
        endorsement-text: (string-utf8 256),
        stake-amount: uint,
        timestamp: uint,
        is-active: bool
    }
)

;; Reputation Challenges
(define-map reputation-challenges
    {challenger: principal, challenged: principal, challenge-id: uint}
    {
        skill-id: uint,
        challenge-proof: (buff 32),
        resolution-height: uint,
        is-resolved: bool,
        outcome: (optional bool)
    }
)

;; Skill Definitions
(define-map skill-registry
    uint
    {
        skill-name: (string-ascii 50),
        category: (string-ascii 30),
        difficulty-level: uint,
        creator: principal
    }
)

;; Counters
(define-data-var next-skill-id uint u1)
(define-data-var next-endorsement-id uint u1)
(define-data-var next-challenge-id uint u1)

;; Read-Only Functions

(define-read-only (get-professional-identity (professional principal))
    (ok (map-get? professional-identities professional))
)

(define-read-only (get-skill-attestation (professional principal) (skill-id uint))
    (ok (map-get? skill-attestations {professional: professional, skill-id: skill-id}))
)

(define-read-only (get-endorsement (endorser principal) (endorsed principal) (endorsement-id uint))
    (ok (map-get? endorsements {endorser: endorser, endorsed: endorsed, endorsement-id: endorsement-id}))
)

(define-read-only (get-skill-info (skill-id uint))
    (ok (map-get? skill-registry skill-id))
)

(define-read-only (calculate-reputation-score (professional principal))
    (let
        (
            (identity (unwrap! (map-get? professional-identities professional) (err err-not-found)))
        )
        (ok {
            base-score: (get reputation-score identity),
            total-endorsements: (get total-endorsements identity),
            verified-skills: (get verified-skills identity),
            computed-score: (+ 
                (get reputation-score identity)
                (* (get total-endorsements identity) u10)
                (* (get verified-skills identity) u50)
            )
        })
    )
)

;; Public Functions

;; Register a new professional identity
(define-public (register-professional)
    (let
        (
            (caller tx-sender)
            (existing-identity (map-get? professional-identities caller))
        )
        (asserts! (is-none existing-identity) err-already-exists)
        (ok (map-set professional-identities caller {
            reputation-score: u100,
            total-endorsements: u0,
            verified-skills: u0,
            registration-height: block-height,
            is-active: true
        }))
    )
)

;; Register a new skill in the registry
(define-public (register-skill (skill-name (string-ascii 50)) (category (string-ascii 30)) (difficulty uint))
    (let
        (
            (skill-id (var-get next-skill-id))
        )
        (asserts! (> difficulty u0) err-invalid-input)
        (asserts! (<= difficulty u10) err-invalid-input)
        (map-set skill-registry skill-id {
            skill-name: skill-name,
            category: category,
            difficulty-level: difficulty,
            creator: tx-sender
        })
        (var-set next-skill-id (+ skill-id u1))
        (ok skill-id)
    )
)

;; Submit a skill attestation
(define-public (attest-skill (skill-id uint) (proof-hash (buff 32)))
    (let
        (
            (caller tx-sender)
            (identity (unwrap! (map-get? professional-identities caller) err-not-found))
            (skill-info (unwrap! (map-get? skill-registry skill-id) err-not-found))
        )
        (asserts! (get is-active identity) err-unauthorized)
        (map-set skill-attestations 
            {professional: caller, skill-id: skill-id}
            {
                skill-name: (get skill-name skill-info),
                proof-hash: proof-hash,
                attestation-height: block-height,
                verifier: caller,
                confidence-score: u50,
                is-verified: false
            }
        )
        (ok true)
    )
)

;; Verify a skill attestation (simplified verification)
(define-public (verify-skill-attestation (professional principal) (skill-id uint))
    (let
        (
            (attestation (unwrap! (map-get? skill-attestations {professional: professional, skill-id: skill-id}) err-not-found))
            (identity (unwrap! (map-get? professional-identities professional) err-not-found))
        )
        (asserts! (not (get is-verified attestation)) err-already-exists)
        (map-set skill-attestations 
            {professional: professional, skill-id: skill-id}
            (merge attestation {
                is-verified: true,
                confidence-score: u100
            })
        )
        (map-set professional-identities professional
            (merge identity {
                verified-skills: (+ (get verified-skills identity) u1),
                reputation-score: (+ (get reputation-score identity) u25)
            })
        )
        (ok true)
    )
)

;; Create an endorsement with stake
(define-public (endorse-professional (endorsed principal) (skill-id uint) (endorsement-text (string-utf8 256)) (stake-amount uint))
    (let
        (
            (caller tx-sender)
            (endorsement-id (var-get next-endorsement-id))
            (endorsed-identity (unwrap! (map-get? professional-identities endorsed) err-not-found))
        )
        (asserts! (>= stake-amount (var-get min-endorsement-stake)) err-invalid-input)
        (asserts! (not (is-eq caller endorsed)) err-unauthorized)
        (asserts! (get is-active endorsed-identity) err-unauthorized)
        
        ;; Transfer stake (simplified - in production would use STX transfer)
        (try! (stx-transfer? stake-amount caller (as-contract tx-sender)))
        
        (map-set endorsements 
            {endorser: caller, endorsed: endorsed, endorsement-id: endorsement-id}
            {
                skill-id: skill-id,
                endorsement-text: endorsement-text,
                stake-amount: stake-amount,
                timestamp: block-height,
                is-active: true
            }
        )
        
        (map-set professional-identities endorsed
            (merge endorsed-identity {
                total-endorsements: (+ (get total-endorsements endorsed-identity) u1),
                reputation-score: (+ (get reputation-score endorsed-identity) u10)
            })
        )
        
        (var-set next-endorsement-id (+ endorsement-id u1))
        (ok endorsement-id)
    )
)

;; Challenge a reputation claim
(define-public (challenge-reputation (challenged principal) (skill-id uint) (challenge-proof (buff 32)))
    (let
        (
            (caller tx-sender)
            (challenge-id (var-get next-challenge-id))
            (challenged-identity (unwrap! (map-get? professional-identities challenged) err-not-found))
        )
        (asserts! (not (is-eq caller challenged)) err-unauthorized)
        (asserts! (get is-active challenged-identity) err-unauthorized)
        
        (map-set reputation-challenges
            {challenger: caller, challenged: challenged, challenge-id: challenge-id}
            {
                skill-id: skill-id,
                challenge-proof: challenge-proof,
                resolution-height: (+ block-height u144), ;; ~24 hours
                is-resolved: false,
                outcome: none
            }
        )
        
        (var-set next-challenge-id (+ challenge-id u1))
        (ok challenge-id)
    )
)

;; Resolve a reputation challenge
(define-public (resolve-challenge (challenger principal) (challenged principal) (challenge-id uint) (outcome bool))
    (let
        (
            (challenge (unwrap! (map-get? reputation-challenges {challenger: challenger, challenged: challenged, challenge-id: challenge-id}) err-not-found))
            (challenged-identity (unwrap! (map-get? professional-identities challenged) err-not-found))
        )
        (asserts! (is-eq tx-sender contract-owner) err-owner-only)
        (asserts! (not (get is-resolved challenge)) err-already-exists)
        (asserts! (>= block-height (get resolution-height challenge)) err-unauthorized)
        
        (map-set reputation-challenges
            {challenger: challenger, challenged: challenged, challenge-id: challenge-id}
            (merge challenge {
                is-resolved: true,
                outcome: (some outcome)
            })
        )
        
        ;; Adjust reputation based on outcome
        (if outcome
            ;; Challenge successful - reduce reputation
            (map-set professional-identities challenged
                (merge challenged-identity {
                    reputation-score: (if (> (get reputation-score challenged-identity) u50)
                        (- (get reputation-score challenged-identity) u50)
                        u0
                    )
                })
            )
            ;; Challenge failed - increase reputation
            (map-set professional-identities challenged
                (merge challenged-identity {
                    reputation-score: (+ (get reputation-score challenged-identity) u20)
                })
            )
        )
        
        (ok true)
    )
)

;; Update professional status
(define-public (update-active-status (is-active bool))
    (let
        (
            (caller tx-sender)
            (identity (unwrap! (map-get? professional-identities caller) err-not-found))
        )
        (ok (map-set professional-identities caller
            (merge identity {is-active: is-active})
        ))
    )
)

;; Administrative Functions

(define-public (set-reputation-decay-rate (new-rate uint))
    (begin
        (asserts! (is-eq tx-sender contract-owner) err-owner-only)
        (ok (var-set reputation-decay-rate new-rate))
    )
)

(define-public (set-min-endorsement-stake (new-min uint))
    (begin
        (asserts! (is-eq tx-sender contract-owner) err-owner-only)
        (ok (var-set min-endorsement-stake new-min))
    )
)