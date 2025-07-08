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

(define-constant max-discount-percentage u30)
(define-constant records-for-max-discount u12)

(define-map policy-discounts
  { policy-id: uint }
  {
    current-discount: uint,
    total-records: uint,
    last-calculated: uint
  }
)

(define-map discount-tiers
  { tier: uint }
  {
    min-records: uint,
    discount-percentage: uint
  }
)

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



(define-map health-records 
  { policy-id: uint, record-id: uint }
  {
    vet-id: uint,
    record-type: (string-ascii 32),
    description: (string-ascii 256),
    timestamp: uint
  }
)

(define-map policy-records
  { policy-id: uint }
  { record-count: uint }
)

(define-read-only (get-health-record (policy-id uint) (record-id uint))
  (map-get? health-records { policy-id: policy-id, record-id: record-id })
)

(define-public (add-health-record 
    (policy-id uint)
    (record-type (string-ascii 32))
    (description (string-ascii 256)))
  (let
    (
      (policy (unwrap! (map-get? policies { policy-id: policy-id }) err-not-found))
      (vet-data (unwrap! (map-get? vet-principals { principal: tx-sender }) err-not-vet))
      (records (default-to { record-count: u0 } (map-get? policy-records { policy-id: policy-id })))
      (record-id (get record-count records))
    )
    (asserts! (get active policy) err-unauthorized)
    
    (map-set health-records
      { policy-id: policy-id, record-id: record-id }
      {
        vet-id: (get vet-id vet-data),
        record-type: record-type,
        description: description,
        timestamp: stacks-block-height
      }
    )
    
    (map-set policy-records
      { policy-id: policy-id }
      { record-count: (+ record-id u1) }
    )
    
    (ok record-id)
  )
)


(define-public (extend-policy (policy-id uint) (additional-blocks uint))
  (let
    (
      (policy (unwrap! (map-get? policies { policy-id: policy-id }) err-not-found))
    )
    (asserts! (is-eq tx-sender (get owner policy)) err-unauthorized)
    (asserts! (get active policy) err-unauthorized)
    
    (map-set policies
      { policy-id: policy-id }
      (merge policy { expiration-block: (+ (get expiration-block policy) additional-blocks) })
    )
    
    (ok true)
  )
)

(define-read-only (get-policy-records-count (policy-id uint))
  (default-to { record-count: u0 } (map-get? policy-records { policy-id: policy-id }))
)


(define-constant err-transfer-not-allowed (err u109))

(define-public (transfer-policy (policy-id uint) (new-owner principal))
  (let
    (
      (policy (unwrap! (map-get? policies { policy-id: policy-id }) err-not-found))
    )
    (asserts! (is-eq tx-sender (get owner policy)) err-unauthorized)
    (asserts! (get active policy) err-unauthorized)
    
    (try! (nft-transfer? pet-policy policy-id tx-sender new-owner))
    
    (map-set policies
      { policy-id: policy-id }
      (merge policy { owner: new-owner })
    )
    
    (ok true)
  )
)



(define-data-var discount-calculation-fee uint u1000000)

(define-private (init-discount-tiers)
  (begin
    (map-set discount-tiers { tier: u1 } { min-records: u0, discount-percentage: u0 })
    (map-set discount-tiers { tier: u2 } { min-records: u3, discount-percentage: u5 })
    (map-set discount-tiers { tier: u3 } { min-records: u6, discount-percentage: u10 })
    (map-set discount-tiers { tier: u4 } { min-records: u9, discount-percentage: u20 })
    (map-set discount-tiers { tier: u5 } { min-records: u12, discount-percentage: u30 })
  )
)

(define-read-only (get-policy-discount (policy-id uint))
  (map-get? policy-discounts { policy-id: policy-id })
)

(define-read-only (get-discount-tier (tier uint))
  (map-get? discount-tiers { tier: tier })
)

(define-read-only (calculate-discount-percentage (record-count uint))
  (if (<= record-count u2)
    u0
    (if (<= record-count u5)
      u5
      (if (<= record-count u8)
        u10
        (if (<= record-count u11)
          u20
          u30))))
)

(define-read-only (calculate-discounted-premium (original-premium uint) (discount-percentage uint))
  (let
    (
      (discount-amount (/ (* original-premium discount-percentage) u100))
    )
    (- original-premium discount-amount)
  )
)

(define-read-only (get-policy-record-count (policy-id uint))
  (let
    (
      (records (get-policy-records-count policy-id))
    )
    (get record-count records)
  )
)

(define-public (update-policy-discount (policy-id uint))
  (let
    (
      (policy (unwrap! (get-policy policy-id) err-not-found))
      (current-records (get-policy-record-count policy-id))
      (new-discount (calculate-discount-percentage current-records))
    )
    (asserts! (get active policy) err-unauthorized)
    
    (map-set policy-discounts
      { policy-id: policy-id }
      {
        current-discount: new-discount,
        total-records: current-records,
        last-calculated: stacks-block-height
      }
    )
    
    (ok new-discount)
  )
)

(define-public (renew-policy-with-discount 
    (policy-id uint)
    (new-premium uint)
    (duration-blocks uint))
  (let
    (
      (policy (unwrap! (get-policy policy-id) err-not-found))
      (discount-data (get-policy-discount policy-id))
      (discount-percentage (match discount-data
        data (get current-discount data)
        u0))
      (discounted-premium (calculate-discounted-premium new-premium discount-percentage))
    )
    (asserts! (is-eq tx-sender (get owner policy)) err-unauthorized)
    (asserts! (get active policy) err-unauthorized)
    (asserts! (> new-premium u0) err-invalid-amount)
    
    (try! (stx-transfer? discounted-premium tx-sender contract-owner))
    
    (try! (extend-policy policy-id duration-blocks))
    
    (ok {
      original-premium: new-premium,
      discount-applied: discount-percentage,
      final-premium: discounted-premium,
      savings: (- new-premium discounted-premium)
    })
  )
)

(define-public (create-policy-with-discount
    (pet-name (string-ascii 64))
    (pet-species (string-ascii 32))
    (pet-age uint)
    (coverage-amount uint)
    (premium-amount uint)
    (duration-blocks uint)
    (max-claims uint))
  (let
    (
      (policy-result (try! (create-policy 
        pet-name pet-species pet-age coverage-amount premium-amount duration-blocks max-claims)))
    )
    (map-set policy-discounts
      { policy-id: policy-result }
      {
        current-discount: u0,
        total-records: u0,
        last-calculated: stacks-block-height
      }
    )
    
    (ok policy-result)
  )
)

(define-public (bulk-update-discounts (policy-ids (list 50 uint)))
  (begin
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (ok (map update-policy-discount policy-ids))
  )
)

(define-public (set-discount-calculation-fee (new-fee uint))
  (begin
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (var-set discount-calculation-fee new-fee)
    (ok true)
  )
)

(define-public (update-discount-tier (tier uint) (min-records uint) (discount-percentage uint))
  (begin
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (asserts! (<= discount-percentage max-discount-percentage) err-invalid-amount)
    
    (map-set discount-tiers
      { tier: tier }
      {
        min-records: min-records,
        discount-percentage: discount-percentage
      }
    )
    
    (ok true)
  )
)

(define-read-only (preview-renewal-cost (policy-id uint) (base-premium uint))
  (let
    (
      (discount-data (get-policy-discount policy-id))
      (discount-percentage (match discount-data
        data (get current-discount data)
        u0))
      (discounted-premium (calculate-discounted-premium base-premium discount-percentage))
    )
    {
      base-premium: base-premium,
      discount-percentage: discount-percentage,
      discounted-premium: discounted-premium,
      savings: (- base-premium discounted-premium)
    }
  )
)

(define-read-only (get-discount-calculation-fee)
  (var-get discount-calculation-fee)
)

(define-constant err-insufficient-emergency-funds (err u110))
(define-constant err-emergency-request-exists (err u111))
(define-constant err-emergency-request-not-found (err u112))
(define-constant err-voting-ended (err u113))
(define-constant err-already-voted (err u114))
(define-constant err-self-vote (err u115))
(define-constant err-insufficient-contribution (err u116))
(define-constant err-emergency-not-approved (err u117))
(define-constant err-emergency-expired (err u118))
(define-constant err-emergency-already-funded (err u119))

(define-data-var emergency-fund-balance uint u0)
(define-data-var next-emergency-id uint u1)
(define-data-var min-emergency-contribution uint u1000000)
(define-data-var emergency-voting-period uint u144)
(define-data-var min-votes-required uint u3)

(define-map emergency-requests
  { emergency-id: uint }
  {
    policy-id: uint,
    requester: principal,
    amount-requested: uint,
    description: (string-ascii 256),
    vet-verification: principal,
    created-at: uint,
    voting-ends-at: uint,
    yes-votes: uint,
    no-votes: uint,
    approved: bool,
    funded: bool,
    total-funded: uint
  }
)

(define-map emergency-votes
  { emergency-id: uint, voter: principal }
  { vote: bool, timestamp: uint }
)

(define-map emergency-contributors
  { contributor: principal }
  {
    total-contributed: uint,
    contribution-count: uint,
    last-contribution: uint
  }
)

(define-map emergency-fund-contributions
  { emergency-id: uint, contributor: principal }
  { amount: uint, timestamp: uint }
)

(define-map policy-emergency-requests
  { policy-id: uint }
  { request-count: uint }
)

(define-read-only (get-emergency-fund-balance)
  (var-get emergency-fund-balance)
)

(define-read-only (get-emergency-request (emergency-id uint))
  (map-get? emergency-requests { emergency-id: emergency-id })
)

(define-read-only (get-emergency-vote (emergency-id uint) (voter principal))
  (map-get? emergency-votes { emergency-id: emergency-id, voter: voter })
)

(define-read-only (get-contributor-stats (contributor principal))
  (map-get? emergency-contributors { contributor: contributor })
)

(define-read-only (get-emergency-settings)
  {
    min-contribution: (var-get min-emergency-contribution),
    voting-period: (var-get emergency-voting-period),
    min-votes-required: (var-get min-votes-required)
  }
)

(define-public (contribute-to-emergency-fund (amount uint))
  (let
    (
      (current-contributor (default-to
        { total-contributed: u0, contribution-count: u0, last-contribution: u0 }
        (map-get? emergency-contributors { contributor: tx-sender })))
    )
    (asserts! (>= amount (var-get min-emergency-contribution)) err-insufficient-contribution)
    
    (try! (stx-transfer? amount tx-sender (as-contract tx-sender)))
    
    (var-set emergency-fund-balance (+ (var-get emergency-fund-balance) amount))
    
    (map-set emergency-contributors
      { contributor: tx-sender }
      {
        total-contributed: (+ (get total-contributed current-contributor) amount),
        contribution-count: (+ (get contribution-count current-contributor) u1),
        last-contribution: stacks-block-height
      }
    )
    
    (ok amount)
  )
)

(define-public (request-emergency-funding
    (policy-id uint)
    (amount uint)
    (description (string-ascii 256)))
  (let
    (
      (policy (unwrap! (map-get? policies { policy-id: policy-id }) err-not-found))
      (emergency-id (var-get next-emergency-id))
      (voting-ends (+ stacks-block-height (var-get emergency-voting-period)))
      (request-count (default-to { request-count: u0 } 
        (map-get? policy-emergency-requests { policy-id: policy-id })))
    )
    (asserts! (is-eq (get owner policy) tx-sender) err-unauthorized)
    (asserts! (get active policy) err-unauthorized)
    (asserts! (> amount u0) err-invalid-amount)
    
    (map-set emergency-requests
      { emergency-id: emergency-id }
      {
        policy-id: policy-id,
        requester: tx-sender,
        amount-requested: amount,
        description: description,
        vet-verification: tx-sender,
        created-at: stacks-block-height,
        voting-ends-at: voting-ends,
        yes-votes: u0,
        no-votes: u0,
        approved: false,
        funded: false,
        total-funded: u0
      }
    )
    
    (map-set policy-emergency-requests
      { policy-id: policy-id }
      { request-count: (+ (get request-count request-count) u1) }
    )
    
    (var-set next-emergency-id (+ emergency-id u1))
    (ok emergency-id)
  )
)

(define-public (vote-on-emergency-request (emergency-id uint) (vote bool))
  (let
    (
      (emergency-request (unwrap! (map-get? emergency-requests { emergency-id: emergency-id }) err-emergency-request-not-found))
      (contributor-data (unwrap! (map-get? emergency-contributors { contributor: tx-sender }) err-unauthorized))
    )
    (asserts! (<= stacks-block-height (get voting-ends-at emergency-request)) err-voting-ended)
    (asserts! (not (is-eq tx-sender (get requester emergency-request))) err-self-vote)
    (asserts! (is-none (map-get? emergency-votes { emergency-id: emergency-id, voter: tx-sender })) err-already-voted)
    
    (map-set emergency-votes
      { emergency-id: emergency-id, voter: tx-sender }
      { vote: vote, timestamp: stacks-block-height }
    )
    
    (if vote
      (map-set emergency-requests
        { emergency-id: emergency-id }
        (merge emergency-request { yes-votes: (+ (get yes-votes emergency-request) u1) }))
      (map-set emergency-requests
        { emergency-id: emergency-id }
        (merge emergency-request { no-votes: (+ (get no-votes emergency-request) u1) }))
    )
    
    (ok vote)
  )
)

(define-public (finalize-emergency-request (emergency-id uint))
  (let
    (
      (emergency-request (unwrap! (map-get? emergency-requests { emergency-id: emergency-id }) err-emergency-request-not-found))
      (total-votes (+ (get yes-votes emergency-request) (get no-votes emergency-request)))
      (approved (and 
        (>= total-votes (var-get min-votes-required))
        (> (get yes-votes emergency-request) (get no-votes emergency-request))))
    )
    (asserts! (> stacks-block-height (get voting-ends-at emergency-request)) err-voting-ended)
    (asserts! (not (get approved emergency-request)) err-emergency-request-exists)
    
    (map-set emergency-requests
      { emergency-id: emergency-id }
      (merge emergency-request { approved: approved })
    )
    
    (ok approved)
  )
)

(define-public (fund-emergency-request (emergency-id uint))
  (let
    (
      (emergency-request (unwrap! (map-get? emergency-requests { emergency-id: emergency-id }) err-emergency-request-not-found))
      (policy (unwrap! (map-get? policies { policy-id: (get policy-id emergency-request) }) err-not-found))
      (amount (get amount-requested emergency-request))
    )
    (asserts! (get approved emergency-request) err-emergency-not-approved)
    (asserts! (not (get funded emergency-request)) err-emergency-already-funded)
    (asserts! (>= (var-get emergency-fund-balance) amount) err-insufficient-emergency-funds)
    
    (try! (as-contract (stx-transfer? amount tx-sender (get owner policy))))
    
    (var-set emergency-fund-balance (- (var-get emergency-fund-balance) amount))
    
    (map-set emergency-requests
      { emergency-id: emergency-id }
      (merge emergency-request { funded: true, total-funded: amount })
    )
    
    (ok amount)
  )
)

(define-public (set-emergency-settings
    (min-contribution uint)
    (voting-period uint)
    (min-votes uint))
  (begin
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (asserts! (> min-contribution u0) err-invalid-amount)
    (asserts! (> voting-period u0) err-invalid-amount)
    (asserts! (> min-votes u0) err-invalid-amount)
    
    (var-set min-emergency-contribution min-contribution)
    (var-set emergency-voting-period voting-period)
    (var-set min-votes-required min-votes)
    
    (ok true)
  )
)

(define-read-only (get-emergency-request-stats (emergency-id uint))
  (match (map-get? emergency-requests { emergency-id: emergency-id })
    emergency-request
    (let
      (
        (total-votes (+ (get yes-votes emergency-request) (get no-votes emergency-request)))
        (voting-still-open (<= stacks-block-height (get voting-ends-at emergency-request)))
      )
      (some {
        total-votes: total-votes,
        yes-percentage: (if (> total-votes u0) (/ (* (get yes-votes emergency-request) u100) total-votes) u0),
        voting-still-open: voting-still-open,
        blocks-remaining: (if voting-still-open 
          (- (get voting-ends-at emergency-request) stacks-block-height) 
          u0)
      })
    )
    none
  )
)

(define-read-only (get-emergency-funding-summary)
  {
    total-fund-balance: (var-get emergency-fund-balance),
    total-requests: (- (var-get next-emergency-id) u1),
    settings: (get-emergency-settings)
  }
)

(init-discount-tiers)