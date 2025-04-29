(define-non-fungible-token pet-policy uint)

(define-constant contract-owner tx-sender)
(define-constant err-owner-only (err u100))
(define-constant err-not-found (err u101))
(define-constant err-unauthorized (err u102))
(define-constant err-policy-exists (err u103))
(define-constant err-policy-expired (err u104))
(define-constant err-insufficient-funds (err u105))
(define-constant err-invalid-amount (err u106))
(define-constant err-not-vet (err u107))
(define-constant err-claim-limit-reached (err u108))

(define-data-var next-policy-id uint u1)
(define-data-var next-vet-id uint u1)
(define-data-var claim-fee uint u10000000) ;; 10 STX

(define-map policies
  { policy-id: uint }
  {
    owner: principal,
    pet-name: (string-ascii 64),
    pet-species: (string-ascii 32),
    pet-age: uint,
    coverage-amount: uint,
    premium-paid: uint,
    expiration-block: uint,
    claims-made: uint,
    max-claims: uint,
    active: bool
  }
)

(define-map vet-registry
  { vet-id: uint }
  {
    vet-principal: principal,
    vet-name: (string-ascii 64),
    verified: bool
  }
)

(define-map vet-principals
  { principal: principal }
  { vet-id: uint }
)

(define-map claims
  { policy-id: uint, claim-id: uint }
  {
    amount: uint,
    description: (string-ascii 256),
    approved: bool,
    processed-by: principal,
    stacks-block-height: uint
  }
)

(define-map policy-claims
  { policy-id: uint }
  { claim-count: uint }
)

(define-read-only (get-policy (policy-id uint))
  (map-get? policies { policy-id: policy-id })
)

(define-read-only (get-policy-claims-count (policy-id uint))
  (default-to { claim-count: u0 } (map-get? policy-claims { policy-id: policy-id }))
)

(define-read-only (get-claim (policy-id uint) (claim-id uint))
  (map-get? claims { policy-id: policy-id, claim-id: claim-id })
)

(define-read-only (get-vet (vet-id uint))
  (map-get? vet-registry { vet-id: vet-id })
)

(define-read-only (is-vet (principal-to-check principal))
  (is-some (map-get? vet-principals { principal: principal-to-check }))
)

(define-read-only (get-claim-fee)
  (var-get claim-fee)
)

(define-public (register-vet (vet-name (string-ascii 64)))
  (let
    (
      (vet-id (var-get next-vet-id))
    )
    (map-set vet-registry
      { vet-id: vet-id }
      {
        vet-principal: tx-sender,
        vet-name: vet-name,
        verified: false
      }
    )
    (map-set vet-principals
      { principal: tx-sender }
      { vet-id: vet-id }
    )
    (var-set next-vet-id (+ vet-id u1))
    (ok vet-id)
  )
)

(define-public (verify-vet (vet-id uint))
  (begin
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (match (map-get? vet-registry { vet-id: vet-id })
      vet-data (ok (map-set vet-registry
          { vet-id: vet-id }
          (merge vet-data { verified: true })))
      err-not-found)
    )
  )

(define-public (create-policy 
    (pet-name (string-ascii 64))
    (pet-species (string-ascii 32))
    (pet-age uint)
    (coverage-amount uint)
    (premium-amount uint)
    (duration-blocks uint)
    (max-claims uint))
  (let
    (
      (policy-id (var-get next-policy-id))
      (expiration (+ stacks-block-height duration-blocks))
    )
    (asserts! (> premium-amount u0) err-invalid-amount)
    (asserts! (> coverage-amount u0) err-invalid-amount)
    (asserts! (> max-claims u0) err-invalid-amount)
    
    (try! (stx-transfer? premium-amount tx-sender contract-owner))
    
    (try! (nft-mint? pet-policy policy-id tx-sender))
    
    (map-set policies
      { policy-id: policy-id }
      {
        owner: tx-sender,
        pet-name: pet-name,
        pet-species: pet-species,
        pet-age: pet-age,
        coverage-amount: coverage-amount,
        premium-paid: premium-amount,
        expiration-block: expiration,
        claims-made: u0,
        max-claims: max-claims,
        active: true
      }
    )
    
    (map-set policy-claims
      { policy-id: policy-id }
      { claim-count: u0 }
    )
    
    (var-set next-policy-id (+ policy-id u1))
    (ok policy-id)
  )
)

(define-public (file-claim (policy-id uint) (amount uint) (description (string-ascii 256)))
  (let
    (
      (policy (unwrap! (map-get? policies { policy-id: policy-id }) err-not-found))
      (claim-counts (get-policy-claims-count policy-id))
      (claim-id (get claim-count claim-counts))
    )
    (asserts! (is-vet tx-sender) err-not-vet)
    (asserts! (< (get claims-made policy) (get max-claims policy)) err-claim-limit-reached)
    (asserts! (<= stacks-block-height (get expiration-block policy)) err-policy-expired)
    (asserts! (get active policy) err-unauthorized)
    (asserts! (<= amount (get coverage-amount policy)) err-invalid-amount)
    
    (try! (stx-transfer? (var-get claim-fee) tx-sender contract-owner))
    
    (map-set claims
      { policy-id: policy-id, claim-id: claim-id }
      {
        amount: amount,
        description: description,
        approved: false,
        processed-by: tx-sender,
        stacks-block-height: stacks-block-height
      }
    )
    
    (map-set policy-claims
      { policy-id: policy-id }
      { claim-count: (+ claim-id u1) }
    )
    
    (map-set policies
      { policy-id: policy-id }
      (merge policy { claims-made: (+ (get claims-made policy) u1) })
    )
    
    (ok claim-id)
  )
)

(define-public (approve-claim (policy-id uint) (claim-id uint))
  (let
    (
      (policy (unwrap! (map-get? policies { policy-id: policy-id }) err-not-found))
      (claim (unwrap! (map-get? claims { policy-id: policy-id, claim-id: claim-id }) err-not-found))
    )
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (asserts! (not (get approved claim)) err-unauthorized)
    
    (map-set claims
      { policy-id: policy-id, claim-id: claim-id }
      (merge claim { approved: true })
    )
    
    (try! (as-contract (stx-transfer? (get amount claim) contract-owner (get owner policy))))
    
    (ok true)
  )
)

(define-public (cancel-policy (policy-id uint))
  (let
    (
      (policy (unwrap! (map-get? policies { policy-id: policy-id }) err-not-found))
    )
    (asserts! (or (is-eq tx-sender (get owner policy)) (is-eq tx-sender contract-owner)) err-unauthorized)
    (asserts! (get active policy) err-unauthorized)
    
    (try! (nft-burn? pet-policy policy-id (get owner policy)))
    
    (map-set policies
      { policy-id: policy-id }
      (merge policy { active: false })
    )
    
    (ok true)
  )
)

(define-public (update-claim-fee (new-fee uint))
  (begin
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (var-set claim-fee new-fee)
    (ok true)
  )
)