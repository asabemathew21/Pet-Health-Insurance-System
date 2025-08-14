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

(define-constant err-wellness-calculation-failed (err u120))
(define-constant err-invalid-wellness-tier (err u121))
(define-constant err-wellness-not-updated (err u122))

(define-data-var base-wellness-score uint u50)
(define-data-var max-wellness-score uint u100)
(define-data-var wellness-decay-rate uint u5)
(define-data-var checkup-score-bonus uint u15)
(define-data-var vaccination-score-bonus uint u10)
(define-data-var preventive-care-bonus uint u8)

(define-map pet-wellness-scores
  { policy-id: uint }
  {
    current-score: uint,
    last-updated: uint,
    total-checkups: uint,
    recent-checkups: uint,
    vaccination-status: uint,
    preventive-care-count: uint,
    wellness-tier: uint,
    premium-discount: uint
  }
)

(define-map wellness-tier-thresholds
  { tier: uint }
  {
    min-score: uint,
    max-score: uint,
    premium-discount: uint,
    tier-name: (string-ascii 20)
  }
)

(define-map wellness-activities
  { policy-id: uint, activity-id: uint }
  {
    activity-type: (string-ascii 32),
    score-impact: uint,
    recorded-by: principal,
    timestamp: uint,
    description: (string-ascii 128)
  }
)

(define-map policy-wellness-activities
  { policy-id: uint }
  { activity-count: uint }
)

(define-private (init-wellness-tiers)
  (begin
    (map-set wellness-tier-thresholds { tier: u1 } 
      { min-score: u0, max-score: u30, premium-discount: u0, tier-name: "At Risk" })
    (map-set wellness-tier-thresholds { tier: u2 } 
      { min-score: u31, max-score: u50, premium-discount: u5, tier-name: "Basic" })
    (map-set wellness-tier-thresholds { tier: u3 } 
      { min-score: u51, max-score: u70, premium-discount: u10, tier-name: "Good" })
    (map-set wellness-tier-thresholds { tier: u4 } 
      { min-score: u71, max-score: u85, premium-discount: u15, tier-name: "Excellent" })
    (map-set wellness-tier-thresholds { tier: u5 } 
      { min-score: u86, max-score: u100, premium-discount: u20, tier-name: "Optimal" })
  )
)

(define-read-only (get-pet-wellness-score (policy-id uint))
  (map-get? pet-wellness-scores { policy-id: policy-id })
)

(define-read-only (get-wellness-tier-info (tier uint))
  (map-get? wellness-tier-thresholds { tier: tier })
)

(define-read-only (get-wellness-activity (policy-id uint) (activity-id uint))
  (map-get? wellness-activities { policy-id: policy-id, activity-id: activity-id })
)

(define-read-only (get-wellness-settings)
  {
    base-score: (var-get base-wellness-score),
    max-score: (var-get max-wellness-score),
    decay-rate: (var-get wellness-decay-rate),
    checkup-bonus: (var-get checkup-score-bonus),
    vaccination-bonus: (var-get vaccination-score-bonus),
    preventive-bonus: (var-get preventive-care-bonus)
  }
)

(define-private (calculate-wellness-tier (score uint))
  (if (<= score u30) u1
    (if (<= score u50) u2
      (if (<= score u70) u3
        (if (<= score u85) u4
          u5))))
)

(define-private (calculate-wellness-decay (last-updated uint) (current-score uint))
  (let
    (
      (blocks-passed (- stacks-block-height last-updated))
      (months-passed (/ blocks-passed u4320))
      (decay-amount (* months-passed (var-get wellness-decay-rate)))
    )
    (if (> decay-amount current-score)
      u0
      (- current-score decay-amount))
  )
)

(define-public (record-wellness-activity
    (policy-id uint)
    (activity-type (string-ascii 32))
    (description (string-ascii 128)))
  (let
    (
      (policy (unwrap! (map-get? policies { policy-id: policy-id }) err-not-found))
      (vet-data (map-get? vet-principals { principal: tx-sender }))
      (activity-counts (default-to { activity-count: u0 } 
        (map-get? policy-wellness-activities { policy-id: policy-id })))
      (activity-id (get activity-count activity-counts))
      (score-bonus (if (is-eq activity-type "checkup") (var-get checkup-score-bonus)
        (if (is-eq activity-type "vaccination") (var-get vaccination-score-bonus)
          (if (is-eq activity-type "preventive") (var-get preventive-care-bonus)
            u0))))
    )
    (asserts! (get active policy) err-unauthorized)
    (asserts! (is-some vet-data) err-not-vet)
    
    (map-set wellness-activities
      { policy-id: policy-id, activity-id: activity-id }
      {
        activity-type: activity-type,
        score-impact: score-bonus,
        recorded-by: tx-sender,
        timestamp: stacks-block-height,
        description: description
      }
    )
    
    (map-set policy-wellness-activities
      { policy-id: policy-id }
      { activity-count: (+ activity-id u1) }
    )
    
    (try! (update-wellness-score policy-id))
    
    (ok activity-id)
  )
)

(define-public (update-wellness-score (policy-id uint))
  (let
    (
      (policy (unwrap! (map-get? policies { policy-id: policy-id }) err-not-found))
      (current-wellness (default-to
        {
          current-score: (var-get base-wellness-score),
          last-updated: stacks-block-height,
          total-checkups: u0,
          recent-checkups: u0,
          vaccination-status: u0,
          preventive-care-count: u0,
          wellness-tier: u2,
          premium-discount: u5
        }
        (map-get? pet-wellness-scores { policy-id: policy-id })))
    )
    (asserts! (get active policy) err-unauthorized)
    
    (let
      (
        (decayed-score (calculate-wellness-decay 
          (get last-updated current-wellness) 
          (get current-score current-wellness)))
        (recent-checkup-count (count-recent-activities policy-id "checkup"))
        (vaccination-count (count-recent-activities policy-id "vaccination"))
        (preventive-count (count-recent-activities policy-id "preventive"))
        (checkup-bonus (* recent-checkup-count (var-get checkup-score-bonus)))
        (vaccination-bonus (* vaccination-count (var-get vaccination-score-bonus)))
        (preventive-bonus (* preventive-count (var-get preventive-care-bonus)))
        (calculated-score (+ decayed-score checkup-bonus vaccination-bonus preventive-bonus))
        (new-score (if (> calculated-score (var-get max-wellness-score)) 
          (var-get max-wellness-score) 
          calculated-score))
        (new-tier (calculate-wellness-tier new-score))
        (tier-info (unwrap! (map-get? wellness-tier-thresholds { tier: new-tier }) err-invalid-wellness-tier))
        (new-discount (get premium-discount tier-info))
      )
      
      (map-set pet-wellness-scores
        { policy-id: policy-id }
        {
          current-score: new-score,
          last-updated: stacks-block-height,
          total-checkups: (+ (get total-checkups current-wellness) recent-checkup-count),
          recent-checkups: recent-checkup-count,
          vaccination-status: (if (> (* vaccination-count u25) u100) u100 (* vaccination-count u25)),
          preventive-care-count: (+ (get preventive-care-count current-wellness) preventive-count),
          wellness-tier: new-tier,
          premium-discount: new-discount
        }
      )
      
      (ok new-score)
    )
  )
)

(define-private (count-recent-activities (policy-id uint) (activity-type (string-ascii 32)))
  (let
    (
      (activity-counts (default-to { activity-count: u0 } 
        (map-get? policy-wellness-activities { policy-id: policy-id })))
      (total-activities (get activity-count activity-counts))
      (six-months-ago (- stacks-block-height u25920))
    )
    (get count (fold count-recent-activity-helper 
      (list u0 u1 u2 u3 u4 u5 u6 u7 u8 u9)
      { count: u0, policy-id: policy-id, activity-type: activity-type, cutoff: six-months-ago, max-id: total-activities }))
  )
)

(define-private (count-recent-activity-helper 
    (activity-index uint) 
    (accumulator { count: uint, policy-id: uint, activity-type: (string-ascii 32), cutoff: uint, max-id: uint }))
  (if (>= activity-index (get max-id accumulator))
    accumulator
    (match (map-get? wellness-activities { policy-id: (get policy-id accumulator), activity-id: activity-index })
      activity-data
      (if (and 
            (is-eq (get activity-type activity-data) (get activity-type accumulator))
            (>= (get timestamp activity-data) (get cutoff accumulator)))
        (merge accumulator { count: (+ (get count accumulator) u1) })
        accumulator)
      accumulator)
  )
)

(define-read-only (calculate-wellness-premium (base-premium uint) (policy-id uint))
  (match (map-get? pet-wellness-scores { policy-id: policy-id })
    wellness-data
    (let
      (
        (discount-percentage (get premium-discount wellness-data))
        (discount-amount (/ (* base-premium discount-percentage) u100))
      )
      {
        base-premium: base-premium,
        wellness-discount: discount-percentage,
        discount-amount: discount-amount,
        final-premium: (- base-premium discount-amount),
        wellness-tier: (get wellness-tier wellness-data),
        wellness-score: (get current-score wellness-data)
      }
    )
    {
      base-premium: base-premium,
      wellness-discount: u0,
      discount-amount: u0,
      final-premium: base-premium,
      wellness-tier: u2,
      wellness-score: (var-get base-wellness-score)
    }
  )
)

(define-public (initialize-wellness-score (policy-id uint))
  (let
    (
      (policy (unwrap! (map-get? policies { policy-id: policy-id }) err-not-found))
    )
    (asserts! (is-eq tx-sender (get owner policy)) err-unauthorized)
    (asserts! (get active policy) err-unauthorized)
    (asserts! (is-none (map-get? pet-wellness-scores { policy-id: policy-id })) err-policy-exists)
    
    (map-set pet-wellness-scores
      { policy-id: policy-id }
      {
        current-score: (var-get base-wellness-score),
        last-updated: stacks-block-height,
        total-checkups: u0,
        recent-checkups: u0,
        vaccination-status: u0,
        preventive-care-count: u0,
        wellness-tier: u2,
        premium-discount: u5
      }
    )
    
    (map-set policy-wellness-activities
      { policy-id: policy-id }
      { activity-count: u0 }
    )
    
    (ok true)
  )
)

(define-public (update-wellness-settings
    (base-score uint)
    (max-score uint)
    (decay-rate uint)
    (checkup-bonus uint)
    (vaccination-bonus uint)
    (preventive-bonus uint))
  (begin
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (asserts! (and (> max-score base-score) (<= max-score u100)) err-invalid-amount)
    (asserts! (<= decay-rate u20) err-invalid-amount)
    
    (var-set base-wellness-score base-score)
    (var-set max-wellness-score max-score)
    (var-set wellness-decay-rate decay-rate)
    (var-set checkup-score-bonus checkup-bonus)
    (var-set vaccination-score-bonus vaccination-bonus)
    (var-set preventive-care-bonus preventive-bonus)
    
    (ok true)
  )
)

(define-public (bulk-update-wellness-scores (policy-ids (list 10 uint)))
  (begin
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (ok (map update-wellness-score policy-ids))
  )
)

(init-discount-tiers)
(init-wellness-tiers)



