;; Consumer Reviews Contract
;; Enables end consumers to review and rate fair trade products

;; Error constants
(define-constant ERR-NOT-AUTHORIZED (err u401))
(define-constant ERR-ALREADY-REVIEWED (err u403))
(define-constant ERR-INVALID-RATING (err u404))
(define-constant ERR-REVIEW-NOT-FOUND (err u405))
(define-constant ERR-OWNER-ONLY (err u406))
(define-constant ERR-INVALID-PURCHASE (err u407))

(define-constant contract-owner tx-sender)

;; Data variables
(define-data-var next-review-id uint u1)
(define-data-var review-period-blocks uint u14400)
(define-data-var min-rating uint u1)
(define-data-var max-rating uint u10)

;; Consumer purchase verification
(define-map consumer-purchases
  { consumer: principal, batch-id: uint }
  {
    purchase-date: uint,
    purchase-verified: bool,
    purchase-amount: uint,
    retailer: principal
  }
)

;; Product reviews data
(define-map product-reviews
  { review-id: uint }
  {
    batch-id: uint,
    consumer: principal,
    quality-rating: uint,
    ethics-rating: uint,
    sustainability-rating: uint,
    overall-rating: uint,
    review-text: (string-ascii 200),
    verified-purchase: bool,
    helpful-votes: uint,
    review-date: uint
  }
)

;; Review aggregations per batch
(define-map batch-review-summary
  uint
  {
    total-reviews: uint,
    avg-quality: uint,
    avg-ethics: uint,
    avg-sustainability: uint,
    avg-overall: uint,
    last-review-date: uint
  }
)

;; Review helpfulness votes
(define-map review-votes
  { review-id: uint, voter: principal }
  {
    helpful: bool,
    vote-date: uint
  }
)

;; Track consumer reviews per batch
(define-map consumer-batch-reviews
  { consumer: principal, batch-id: uint }
  {
    review-id: uint,
    submitted: bool
  }
)

;; Read-only functions
(define-read-only (get-review (review-id uint))
  (map-get? product-reviews { review-id: review-id })
)

(define-read-only (get-batch-reviews-summary (batch-id uint))
  (map-get? batch-review-summary batch-id)
)

(define-read-only (has-consumer-purchased (consumer principal) (batch-id uint))
  (is-some (map-get? consumer-purchases { consumer: consumer, batch-id: batch-id }))
)

(define-read-only (has-consumer-reviewed (consumer principal) (batch-id uint))
  (is-some (map-get? consumer-batch-reviews { consumer: consumer, batch-id: batch-id }))
)

;; Public functions
(define-public (verify-purchase 
  (consumer principal) 
  (batch-id uint) 
  (retailer principal) 
  (amount uint)
)
  (begin
    (asserts! (is-eq tx-sender contract-owner) ERR-NOT-AUTHORIZED)
    (asserts! (> amount u0) ERR-INVALID-PURCHASE)
    
    (map-set consumer-purchases
      { consumer: consumer, batch-id: batch-id }
      {
        purchase-date: stacks-block-height,
        purchase-verified: true,
        purchase-amount: amount,
        retailer: retailer
      }
    )
    (ok true)
  )
)

(define-public (submit-review 
  (batch-id uint)
  (quality-rating uint)
  (ethics-rating uint)
  (sustainability-rating uint)
  (review-text (string-ascii 200))
)
  (let (
    (review-id (var-get next-review-id))
    (purchase-info (unwrap! (map-get? consumer-purchases { consumer: tx-sender, batch-id: batch-id }) ERR-INVALID-PURCHASE))
    (overall-rating (/ (+ (+ quality-rating ethics-rating) sustainability-rating) u3))
  )
    ;; Validate ratings are within range
    (asserts! (and (>= quality-rating (var-get min-rating)) (<= quality-rating (var-get max-rating))) ERR-INVALID-RATING)
    (asserts! (and (>= ethics-rating (var-get min-rating)) (<= ethics-rating (var-get max-rating))) ERR-INVALID-RATING)
    (asserts! (and (>= sustainability-rating (var-get min-rating)) (<= sustainability-rating (var-get max-rating))) ERR-INVALID-RATING)
    
    ;; Check if consumer already reviewed this batch
    (asserts! (not (has-consumer-reviewed tx-sender batch-id)) ERR-ALREADY-REVIEWED)
    
    ;; Create the review
    (map-set product-reviews
      { review-id: review-id }
      {
        batch-id: batch-id,
        consumer: tx-sender,
        quality-rating: quality-rating,
        ethics-rating: ethics-rating,
        sustainability-rating: sustainability-rating,
        overall-rating: overall-rating,
        review-text: review-text,
        verified-purchase: true,
        helpful-votes: u0,
        review-date: stacks-block-height
      }
    )
    
    ;; Mark that consumer has reviewed this batch
    (map-set consumer-batch-reviews
      { consumer: tx-sender, batch-id: batch-id }
      {
        review-id: review-id,
        submitted: true
      }
    )
    
    ;; Update batch review summary
    (unwrap-panic (update-batch-summary batch-id quality-rating ethics-rating sustainability-rating overall-rating))
    
    (var-set next-review-id (+ review-id u1))
    (ok review-id)
  )
)

(define-public (vote-review-helpful (review-id uint) (helpful bool))
  (let (
    (review-info (unwrap! (map-get? product-reviews { review-id: review-id }) ERR-REVIEW-NOT-FOUND))
    (existing-vote (map-get? review-votes { review-id: review-id, voter: tx-sender }))
  )
    (asserts! (is-none existing-vote) ERR-ALREADY-REVIEWED)
    
    (map-set review-votes
      { review-id: review-id, voter: tx-sender }
      {
        helpful: helpful,
        vote-date: stacks-block-height
      }
    )
    
    (if helpful
      (map-set product-reviews
        { review-id: review-id }
        (merge review-info { helpful-votes: (+ (get helpful-votes review-info) u1) })
      )
      true
    )
    (ok true)
  )
)

;; Private helper functions
(define-private (update-batch-summary 
  (batch-id uint) 
  (quality uint) 
  (ethics uint) 
  (sustainability uint) 
  (overall uint)
)
  (let (
    (current-summary (default-to 
      { total-reviews: u0, avg-quality: u0, avg-ethics: u0, avg-sustainability: u0, avg-overall: u0, last-review-date: u0 }
      (map-get? batch-review-summary batch-id)
    ))
    (new-total (+ (get total-reviews current-summary) u1))
    (prev-total (get total-reviews current-summary))
  )
    (map-set batch-review-summary batch-id
      {
        total-reviews: new-total,
        avg-quality: (if (is-eq prev-total u0) quality (/ (+ (* (get avg-quality current-summary) prev-total) quality) new-total)),
        avg-ethics: (if (is-eq prev-total u0) ethics (/ (+ (* (get avg-ethics current-summary) prev-total) ethics) new-total)),
        avg-sustainability: (if (is-eq prev-total u0) sustainability (/ (+ (* (get avg-sustainability current-summary) prev-total) sustainability) new-total)),
        avg-overall: (if (is-eq prev-total u0) overall (/ (+ (* (get avg-overall current-summary) prev-total) overall) new-total)),
        last-review-date: stacks-block-height
      }
    )
    (ok true)
  )
)