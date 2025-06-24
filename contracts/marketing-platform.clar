;; =============================================================================
;; CONSTANTS & ERROR CODES
;; =============================================================================

(define-constant CONTRACT-OWNER tx-sender)
(define-constant PLATFORM-FEE u250) ;; 2.5% in basis points (10000 = 100%)

;; Error codes
(define-constant ERR-UNAUTHORIZED (err u100))
(define-constant ERR-CAMPAIGN-NOT-FOUND (err u101))
(define-constant ERR-CAMPAIGN-EXPIRED (err u102))
(define-constant ERR-CAMPAIGN-COMPLETED (err u103))
(define-constant ERR-INSUFFICIENT-FUNDS (err u104))
(define-constant ERR-INVALID-METRICS (err u105))
(define-constant ERR-ALREADY-PARTICIPATED (err u106))
(define-constant ERR-MINIMUM-NOT-MET (err u107))
(define-constant ERR-FRAUD-DETECTED (err u108))
(define-constant ERR-INVALID-PARAMETERS (err u109))
(define-constant ERR-WITHDRAWAL-FAILED (err u110))

;; =============================================================================
;; DATA STRUCTURES
;; =============================================================================

;; Campaign structure
(define-map campaigns
  { campaign-id: uint }
  {
    brand: principal,
    title: (string-ascii 100),
    description: (string-ascii 500),
    budget: uint,
    remaining-budget: uint,
    reward-per-engagement: uint,
    min-followers: uint,
    max-participants: uint,
    current-participants: uint,
    start-block: uint,
    end-block: uint,
    category: (string-ascii 50),
    status: (string-ascii 20), ;; "active", "paused", "completed", "cancelled"
    fraud-score-threshold: uint,
    created-at: uint
  }
)

;; Influencer profiles
(define-map influencers
  { influencer: principal }
  {
    username: (string-ascii 50),
    bio: (string-ascii 200),
    follower-count: uint,
    engagement-rate: uint, ;; in basis points
    reputation-score: uint, ;; 0-10000
    total-campaigns: uint,
    successful-campaigns: uint,
    verified: bool,
    created-at: uint
  }
)

;; Campaign participation tracking
(define-map campaign-participants
  { campaign-id: uint, influencer: principal }
  {
    joined-at: uint,
    content-submitted: bool,
    content-hash: (string-ascii 64), ;; IPFS hash or similar
    performance-metrics: {
      views: uint,
      likes: uint,
      shares: uint,
      comments: uint,
      clicks: uint
    },
    reward-earned: uint,
    reward-claimed: bool,
    fraud-score: uint,
    verified-by-oracle: bool
  }
)

;; Platform analytics
(define-map platform-stats
  { key: (string-ascii 20) }
  { value: uint }
)

;; Fraud detection patterns
(define-map fraud-patterns
  { pattern-id: uint }
  {
    description: (string-ascii 100),
    weight: uint,
    threshold: uint,
    active: bool
  }
)

;; =============================================================================
;; DATA VARIABLES
;; =============================================================================

(define-data-var campaign-counter uint u0)
(define-data-var platform-treasury uint u0)
(define-data-var fraud-pattern-counter uint u0)
(define-data-var oracle-address (optional principal) none)
(define-data-var minimum-reputation uint u5000) ;; 50% reputation score required

;; =============================================================================
;; PRIVATE HELPER FUNCTIONS
;; =============================================================================

(define-private (is-campaign-active (campaign-id uint))
  (match (map-get? campaigns { campaign-id: campaign-id })
    campaign-data (and
      (is-eq (get status campaign-data) "active")
      (>= stacks-block-height (get start-block campaign-data))
      (<= stacks-block-height (get end-block campaign-data))
      (> (get remaining-budget campaign-data) u0)
    )
    false
  )
)

(define-private (calculate-engagement-score (metrics { views: uint, likes: uint, shares: uint, comments: uint, clicks: uint }))
  (let (
    (total-interactions (+ (+ (get likes metrics) (get shares metrics)) (get comments metrics)))
    (view-count (get views metrics))
  )
    (if (> view-count u0)
      (* (/ total-interactions view-count) u10000) ;; Convert to basis points
      u0
    )
  )
)

(define-private (detect-fraud (campaign-id uint) (influencer principal) (metrics { views: uint, likes: uint, shares: uint, comments: uint, clicks: uint }))
  (let (
    (influencer-data (unwrap! (map-get? influencers { influencer: influencer }) u0))
    (engagement-score (calculate-engagement-score metrics))
    (expected-engagement (get engagement-rate influencer-data))
    (follower-count (get follower-count influencer-data))
    (engagement-deviation (if (> engagement-score expected-engagement)
                            (- engagement-score expected-engagement)
                            (- expected-engagement engagement-score)))
  )
    ;; Fraud indicators:
    ;; 1. Engagement rate significantly higher than historical average
    ;; 2. Suspicious ratio of likes to views
    ;; 3. Very low click-through rate despite high engagement
    (if (or
          (> engagement-deviation (* expected-engagement u2)) ;; 200% deviation
          (and (> (get likes metrics) u0) (< (/ (get views metrics) (get likes metrics)) u5)) ;; Unrealistic like ratio
          (and (> (get likes metrics) u100) (is-eq (get clicks metrics) u0)) ;; High engagement, no clicks
        )
      u8000 ;; High fraud score
      (if (> engagement-deviation expected-engagement)
        u3000 ;; Medium fraud score
        u1000 ;; Low fraud score
      )
    )
  )
)

;; =============================================================================
;; CAMPAIGN MANAGEMENT FUNCTIONS
;; =============================================================================

(define-public (create-campaign
  (title (string-ascii 100))
  (description (string-ascii 500))
  (budget uint)
  (reward-per-engagement uint)
  (min-followers uint)
  (max-participants uint)
  (duration-blocks uint)
  (category (string-ascii 50))
)
  (let (
    (campaign-id (+ (var-get campaign-counter) u1))
    (end-block (+ stacks-block-height duration-blocks))
  )
    (asserts! (> budget u0) ERR-INVALID-PARAMETERS)
    (asserts! (> reward-per-engagement u0) ERR-INVALID-PARAMETERS)
    (asserts! (> duration-blocks u0) ERR-INVALID-PARAMETERS)
    (asserts! (> max-participants u0) ERR-INVALID-PARAMETERS)

    ;; Transfer budget to contract
    (try! (stx-transfer? budget tx-sender (as-contract tx-sender)))

    ;; Create campaign
    (map-set campaigns
      { campaign-id: campaign-id }
      {
        brand: tx-sender,
        title: title,
        description: description,
        budget: budget,
        remaining-budget: budget,
        reward-per-engagement: reward-per-engagement,
        min-followers: min-followers,
        max-participants: max-participants,
        current-participants: u0,
        start-block: stacks-block-height,
        end-block: end-block,
        category: category,
        status: "active",
        fraud-score-threshold: u5000,
        created-at: stacks-block-height
      }
    )

    ;; Update counter
    (var-set campaign-counter campaign-id)

    ;; Update platform stats
    (map-set platform-stats { key: "total-campaigns" }
      { value: (+ (default-to u0 (get value (map-get? platform-stats { key: "total-campaigns" }))) u1) })

    (ok campaign-id)
  )
)

(define-public (join-campaign (campaign-id uint))
  (let (
    (campaign-data (unwrap! (map-get? campaigns { campaign-id: campaign-id }) ERR-CAMPAIGN-NOT-FOUND))
    (influencer-data (unwrap! (map-get? influencers { influencer: tx-sender }) ERR-UNAUTHORIZED))
  )
    (asserts! (is-campaign-active campaign-id) ERR-CAMPAIGN-EXPIRED)
    (asserts! (>= (get follower-count influencer-data) (get min-followers campaign-data)) ERR-MINIMUM-NOT-MET)
    (asserts! (< (get current-participants campaign-data) (get max-participants campaign-data)) ERR-CAMPAIGN-COMPLETED)
    (asserts! (>= (get reputation-score influencer-data) (var-get minimum-reputation)) ERR-MINIMUM-NOT-MET)
    (asserts! (is-none (map-get? campaign-participants { campaign-id: campaign-id, influencer: tx-sender })) ERR-ALREADY-PARTICIPATED)

    ;; Add participant
    (map-set campaign-participants
      { campaign-id: campaign-id, influencer: tx-sender }
      {
        joined-at: stacks-block-height,
        content-submitted: false,
        content-hash: "",
        performance-metrics: { views: u0, likes: u0, shares: u0, comments: u0, clicks: u0 },
        reward-earned: u0,
        reward-claimed: false,
        fraud-score: u0,
        verified-by-oracle: false
      }
    )

    ;; Update campaign participant count
    (map-set campaigns
      { campaign-id: campaign-id }
      (merge campaign-data { current-participants: (+ (get current-participants campaign-data) u1) })
    )

    (ok true)
  )
)

;; =============================================================================
;; PERFORMANCE TRACKING FUNCTIONS
;; =============================================================================

(define-public (submit-content (campaign-id uint) (content-hash (string-ascii 64)))
  (let (
    (participant-data (unwrap! (map-get? campaign-participants { campaign-id: campaign-id, influencer: tx-sender }) ERR-UNAUTHORIZED))
  )
    (asserts! (is-campaign-active campaign-id) ERR-CAMPAIGN-EXPIRED)
    (asserts! (> (len content-hash) u0) ERR-INVALID-PARAMETERS)

    ;; Update participation record
    (map-set campaign-participants
      { campaign-id: campaign-id, influencer: tx-sender }
      (merge participant-data {
        content-submitted: true,
        content-hash: content-hash
      })
    )

    (ok true)
  )
)

(define-public (update-performance-metrics
  (campaign-id uint)
  (influencer principal)
  (views uint)
  (likes uint)
  (shares uint)
  (comments uint)
  (clicks uint)
)
  (let (
    (participant-data (unwrap! (map-get? campaign-participants { campaign-id: campaign-id, influencer: influencer }) ERR-UNAUTHORIZED))
    (campaign-data (unwrap! (map-get? campaigns { campaign-id: campaign-id }) ERR-CAMPAIGN-NOT-FOUND))
    (metrics { views: views, likes: likes, shares: shares, comments: comments, clicks: clicks })
    (fraud-score (detect-fraud campaign-id influencer metrics))
    (total-engagements (+ (+ likes shares) comments))
    (reward-amount (* total-engagements (get reward-per-engagement campaign-data)))
  )
    ;; Only oracle or contract owner can update metrics
    (asserts! (or (is-eq tx-sender CONTRACT-OWNER)
                  (is-eq (some tx-sender) (var-get oracle-address))) ERR-UNAUTHORIZED)
    (asserts! (get content-submitted participant-data) ERR-INVALID-PARAMETERS)

    ;; Check for fraud
    (asserts! (< fraud-score (get fraud-score-threshold campaign-data)) ERR-FRAUD-DETECTED)

    ;; Ensure sufficient budget
    (asserts! (>= (get remaining-budget campaign-data) reward-amount) ERR-INSUFFICIENT-FUNDS)

    ;; Update metrics and calculate reward
    (map-set campaign-participants
      { campaign-id: campaign-id, influencer: influencer }
      (merge participant-data {
        performance-metrics: metrics,
        reward-earned: reward-amount,
        fraud-score: fraud-score,
        verified-by-oracle: true
      })
    )

    ;; Update campaign budget
    (map-set campaigns
      { campaign-id: campaign-id }
      (merge campaign-data { remaining-budget: (- (get remaining-budget campaign-data) reward-amount) })
    )

    (ok reward-amount)
  )
)

;; =============================================================================
;; REWARD DISTRIBUTION FUNCTIONS
;; =============================================================================

(define-public (claim-reward (campaign-id uint))
  (let (
    (participant-data (unwrap! (map-get? campaign-participants { campaign-id: campaign-id, influencer: tx-sender }) ERR-UNAUTHORIZED))
    (reward-amount (get reward-earned participant-data))
    (platform-fee-amount (/ (* reward-amount PLATFORM-FEE) u10000))
    (net-reward (- reward-amount platform-fee-amount))
  )
    (asserts! (get verified-by-oracle participant-data) ERR-UNAUTHORIZED)
    (asserts! (not (get reward-claimed participant-data)) ERR-UNAUTHORIZED)
    (asserts! (> reward-amount u0) ERR-INVALID-PARAMETERS)
    (asserts! (< (get fraud-score participant-data) u5000) ERR-FRAUD-DETECTED)

    ;; Transfer reward to influencer
    (try! (as-contract (stx-transfer? net-reward tx-sender tx-sender)))

    ;; Transfer platform fee to treasury
    (var-set platform-treasury (+ (var-get platform-treasury) platform-fee-amount))

    ;; Mark as claimed
    (map-set campaign-participants
      { campaign-id: campaign-id, influencer: tx-sender }
      (merge participant-data { reward-claimed: true })
    )

    ;; Update influencer stats
    (match (map-get? influencers { influencer: tx-sender })
      influencer-data (let (
        (new-reputation (+ (get reputation-score influencer-data) u100))
      )
        (map-set influencers
          { influencer: tx-sender }
          (merge influencer-data {
            successful-campaigns: (+ (get successful-campaigns influencer-data) u1),
            reputation-score: (if (> new-reputation u10000) u10000 new-reputation)
          })
        )
      )
      false ;; Should not happen if properly validated
    )

    (ok net-reward)
  )
)

;; =============================================================================
;; INFLUENCER MANAGEMENT FUNCTIONS
;; =============================================================================

(define-public (register-influencer
  (username (string-ascii 50))
  (bio (string-ascii 200))
  (follower-count uint)
  (engagement-rate uint)
)
  (begin
    (asserts! (is-none (map-get? influencers { influencer: tx-sender })) ERR-ALREADY-PARTICIPATED)
    (asserts! (> (len username) u0) ERR-INVALID-PARAMETERS)
    (asserts! (> follower-count u0) ERR-INVALID-PARAMETERS)

    (map-set influencers
      { influencer: tx-sender }
      {
        username: username,
        bio: bio,
        follower-count: follower-count,
        engagement-rate: engagement-rate,
        reputation-score: u7500, ;; Start with 75% reputation
        total-campaigns: u0,
        successful-campaigns: u0,
        verified: false,
        created-at: stacks-block-height
      }
    )

    (ok true)
  )
)

(define-public (update-influencer-profile
  (username (string-ascii 50))
  (bio (string-ascii 200))
  (follower-count uint)
  (engagement-rate uint)
)
  (let (
    (influencer-data (unwrap! (map-get? influencers { influencer: tx-sender }) ERR-UNAUTHORIZED))
  )
    (map-set influencers
      { influencer: tx-sender }
      (merge influencer-data {
        username: username,
        bio: bio,
        follower-count: follower-count,
        engagement-rate: engagement-rate
      })
    )

    (ok true)
  )
)

;; =============================================================================
;; ADMIN FUNCTIONS
;; =============================================================================

(define-public (set-oracle-address (oracle principal))
  (begin
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-UNAUTHORIZED)
    (var-set oracle-address (some oracle))
    (ok true)
  )
)

(define-public (verify-influencer (influencer principal))
  (let (
    (influencer-data (unwrap! (map-get? influencers { influencer: influencer }) ERR-UNAUTHORIZED))
  )
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-UNAUTHORIZED)

    (map-set influencers
      { influencer: influencer }
      (merge influencer-data { verified: true })
    )

    (ok true)
  )
)

(define-public (pause-campaign (campaign-id uint))
  (let (
    (campaign-data (unwrap! (map-get? campaigns { campaign-id: campaign-id }) ERR-CAMPAIGN-NOT-FOUND))
  )
    (asserts! (or (is-eq tx-sender (get brand campaign-data)) (is-eq tx-sender CONTRACT-OWNER)) ERR-UNAUTHORIZED)

    (map-set campaigns
      { campaign-id: campaign-id }
      (merge campaign-data { status: "paused" })
    )

    (ok true)
  )
)

(define-public (withdraw-platform-fees)
  (let (
    (treasury-amount (var-get platform-treasury))
  )
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-UNAUTHORIZED)
    (asserts! (> treasury-amount u0) ERR-INSUFFICIENT-FUNDS)

    (try! (as-contract (stx-transfer? treasury-amount tx-sender CONTRACT-OWNER)))
    (var-set platform-treasury u0)

    (ok treasury-amount)
  )
)

;; =============================================================================
;; READ-ONLY FUNCTIONS
;; =============================================================================

(define-read-only (get-campaign (campaign-id uint))
  (map-get? campaigns { campaign-id: campaign-id })
)

(define-read-only (get-influencer (influencer principal))
  (map-get? influencers { influencer: influencer })
)

(define-read-only (get-campaign-participation (campaign-id uint) (influencer principal))
  (map-get? campaign-participants { campaign-id: campaign-id, influencer: influencer })
)

(define-read-only (get-platform-stats (key (string-ascii 20)))
  (get value (map-get? platform-stats { key: key }))
)

(define-read-only (get-campaign-count)
  (var-get campaign-counter)
)

(define-read-only (get-platform-treasury)
  (var-get platform-treasury)
)

(define-read-only (is-campaign-participant (campaign-id uint) (influencer principal))
  (is-some (map-get? campaign-participants { campaign-id: campaign-id, influencer: influencer }))
)

(define-read-only (calculate-campaign-roi (campaign-id uint))
  (match (map-get? campaigns { campaign-id: campaign-id })
    campaign-data (let (
      (total-spent (- (get budget campaign-data) (get remaining-budget campaign-data)))
      (total-engagement u0) ;; This would need to be calculated by iterating through participants
    )
      (if (> total-spent u0)
        (some (/ (* total-engagement u10000) total-spent))
        none
      )
    )
    none
  )
)

;; =============================================================================
;; INITIALIZATION
;; =============================================================================

;; Initialize platform stats
(map-set platform-stats { key: "total-campaigns" } { value: u0 })
(map-set platform-stats { key: "total-influencers" } { value: u0 })
(map-set platform-stats { key: "total-rewards-paid" } { value: u0 })

;; Initialize fraud detection patterns
(map-set fraud-patterns { pattern-id: u1 } {
  description: "Suspicious engagement spike",
  weight: u3000,
  threshold: u5000,
  active: true
})

(map-set fraud-patterns { pattern-id: u2 } {
  description: "Unrealistic like-to-view ratio",
  weight: u4000,
  threshold: u3000,
  active: true
})

(var-set fraud-pattern-counter u2)
