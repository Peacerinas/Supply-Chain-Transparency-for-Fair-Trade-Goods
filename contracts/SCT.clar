;; (impl-trait .nft-trait.nft-trait)

(define-non-fungible-token sct-batch uint)

(define-constant contract-owner tx-sender)
(define-constant err-owner-only (err u100))
(define-constant err-not-found (err u101))
(define-constant err-already-exists (err u102))
(define-constant err-invalid-stage (err u103))
(define-map authorized-verifiers
    principal
    {
        verifier-type: (string-ascii 32),
        active: bool,
        verification-count: uint
    }
)

(define-map batch-verifications
    { batch-id: uint, verifier: principal }
    {
        verification-type: (string-ascii 32),
        verified: bool,
        verification-date: uint,
        notes: (string-ascii 128)
    }
)

(define-map batch-verification-summary
    uint
    {
        total-verifications: uint,
        passed-verifications: uint,
        verification-score: uint,
        fully-verified: bool
    }
)

(define-data-var min-verifications-required uint u2)
(define-map batch-details
    uint 
    {
        product-name: (string-ascii 64),
        origin-location: (string-ascii 64),
        producer-id: principal,
        timestamp: uint,
        current-stage: uint,
        fair-trade-certified: bool
    }
)

(define-map stage-details
    {batch-id: uint, stage-id: uint}
    {
        handler: principal,
        location: (string-ascii 64),
        timestamp: uint,
        quality-check: bool
    }
)

(define-data-var last-batch-id uint u0)

(define-public (create-batch (product-name (string-ascii 64)) (origin-location (string-ascii 64)) (producer-id principal))
    (let
        (
            (new-id (+ (var-get last-batch-id) u1))
        )
        (try! (nft-mint? sct-batch new-id tx-sender))
        (map-set batch-details new-id {
            product-name: product-name,
            origin-location: origin-location,
            producer-id: producer-id,
            timestamp: stacks-block-height,
            current-stage: u0,
            fair-trade-certified: false
        })
        (var-set last-batch-id new-id)
        (ok new-id)
    )
)

(define-public (update-stage (batch-id uint) (location (string-ascii 64)) (quality-passed bool))
    (let
        (
            (batch (unwrap! (map-get? batch-details batch-id) (err err-not-found)))
            (new-stage (+ (get current-stage batch) u1))
        )
        (asserts! (<= new-stage u5) (err err-invalid-stage))
        (map-set stage-details {batch-id: batch-id, stage-id: new-stage}
            {
                handler: tx-sender,
                location: location,
                timestamp: stacks-block-height,
                quality-check: quality-passed
            }
        )
        (map-set batch-details batch-id (merge batch {current-stage: new-stage}))
        (ok true)
    )
)



(define-read-only (get-batch-details (batch-id uint))
    (ok (map-get? batch-details batch-id))
)

(define-read-only (get-stage-details (batch-id uint) (stage-id uint))
    (ok (map-get? stage-details {batch-id: batch-id, stage-id: stage-id}))
)

(define-read-only (get-all-stages (batch-id uint))
    (let
        (
            (batch (unwrap! (map-get? batch-details batch-id) (err err-not-found)))
            (current-stage (get current-stage batch))
        )
        (ok {
            total-stages: current-stage,
            is-certified: (get fair-trade-certified batch)
        })
    )
)

(define-read-only (is-fair-trade-certified (batch-id uint))
    (match (map-get? batch-details batch-id)
        batch (ok (get fair-trade-certified batch))
        (err err-not-found)
    )
)

(define-public (transfer (batch-id uint) (sender principal) (recipient principal))
    (begin
        (asserts! (is-eq tx-sender sender) (err u1))
        (nft-transfer? sct-batch batch-id sender recipient)
    )
)

(define-read-only (get-owner (batch-id uint))
    (ok (nft-get-owner? sct-batch batch-id))
)

(define-read-only (get-last-token-id)
    (ok (var-get last-batch-id))
)

(define-map quality-scores
    uint
    {
        base-score: uint,
        quality-metrics: (list 5 uint),
        last-updated: uint,
        verified: bool
    }
)



(define-public (add-verifier (verifier principal) (verifier-type (string-ascii 32)))
    (begin
        (asserts! (is-eq tx-sender contract-owner) (err err-owner-only))
        (map-set authorized-verifiers verifier
            {
                verifier-type: verifier-type,
                active: true,
                verification-count: u0
            })
        (ok true)
    )
)

(define-public (remove-verifier (verifier principal))
    (begin
        (asserts! (is-eq tx-sender contract-owner) (err err-owner-only))
        (match (map-get? authorized-verifiers verifier)
            verifier-data (ok (map-set authorized-verifiers verifier (merge verifier-data { active: false })))
            (err err-not-found)
        )
    )
)

(define-public (verify-batch (batch-id uint) (verification-type (string-ascii 32)) (passed bool) (notes (string-ascii 128)))
    (let
        (
            (verifier-data (unwrap! (map-get? authorized-verifiers tx-sender) (err u104)))
        )
        (asserts! (get active verifier-data) (err u105))
        
        (map-set batch-verifications { batch-id: batch-id, verifier: tx-sender }
            {
                verification-type: verification-type,
                verified: passed,
                verification-date: stacks-block-height,
                notes: notes
            })
        
        (map-set authorized-verifiers tx-sender 
            (merge verifier-data { verification-count: (+ (get verification-count verifier-data) u1) }))
        
        ;; (try! (update-verification-summary batch-id))
        (ok true)
    )
)

(define-private (update-verification-summary (batch-id uint))
    (let
        (
            (current-summary (default-to { total-verifications: u0, passed-verifications: u0, verification-score: u0, fully-verified: false } 
                                        (map-get? batch-verification-summary batch-id)))
            (new-total (+ (get total-verifications current-summary) u1))
            (verification-passed (match (map-get? batch-verifications { batch-id: batch-id, verifier: tx-sender })
                                    verification-data (get verified verification-data)
                                    false))
            (new-passed (if verification-passed (+ (get passed-verifications current-summary) u1) (get passed-verifications current-summary)))
            (new-score (if (> new-total u0) (/ (* new-passed u100) new-total) u0))
            (is-fully-verified (>= new-passed (var-get min-verifications-required)))
        )
        (map-set batch-verification-summary batch-id
            {
                total-verifications: new-total,
                passed-verifications: new-passed,
                verification-score: new-score,
                fully-verified: is-fully-verified
            })
        (ok true)
    )
)

(define-read-only (get-batch-verification-status (batch-id uint))
    (ok (map-get? batch-verification-summary batch-id))
)

(define-read-only (get-verifier-info (verifier principal))
    (ok (map-get? authorized-verifiers verifier))
)

(define-read-only (get-batch-verification (batch-id uint) (verifier principal))
    (ok (map-get? batch-verifications { batch-id: batch-id, verifier: verifier }))
)

(define-public (set-min-verifications (min-count uint))
    (begin
        (asserts! (is-eq tx-sender contract-owner) (err err-owner-only))
        (var-set min-verifications-required min-count)
        (ok true)
    )
)

(define-read-only (is-batch-fully-verified (batch-id uint))
    (match (map-get? batch-verification-summary batch-id)
        summary (ok (get fully-verified summary))
        (err err-not-found)
    )
)

(define-public (set-batch-quality (batch-id uint) (metrics (list 5 uint)))
    (let
        ((batch (unwrap! (map-get? batch-details batch-id) (err err-not-found)))
         (avg-score (/ (fold + metrics u0) u5)))
        
        (asserts! (is-eq tx-sender contract-owner) (err err-owner-only))
        
        (map-set quality-scores batch-id
            {
                base-score: avg-score,
                quality-metrics: metrics,
                last-updated: stacks-block-height,
                verified: true
            })
        (ok avg-score)))

(define-read-only (get-batch-quality (batch-id uint))
    (ok (map-get? quality-scores batch-id)))


(define-map batch-timeline
    { batch-id: uint, event-id: uint }
    {
        event-type: (string-ascii 24),
        actor: principal,
        details: (string-ascii 64),
        timestamp: uint
    }
)

(define-map batch-event-counter
    uint
    uint
)

(define-read-only (get-batch-timeline (batch-id uint))
    (let
        ((event-count (default-to u0 (map-get? batch-event-counter batch-id))))
        {
            batch-id: batch-id,
            total-events: event-count,
            timeline: (map-get? batch-timeline (tuple (batch-id batch-id) (event-id event-count)))
        })
)

(define-private (record-timeline-event (batch-id uint) (event-type (string-ascii 24)) (details (string-ascii 64)))
    (let
        ((current-count (default-to u0 (map-get? batch-event-counter batch-id)))
         (new-count (+ current-count u1)))
        
        (map-set batch-timeline 
            { batch-id: batch-id, event-id: new-count }
            {
                event-type: event-type,
                actor: tx-sender,
                details: details,
                timestamp: stacks-block-height
            })
        (map-set batch-event-counter batch-id new-count)
        (ok new-count)))

(define-public (add-batch-note (batch-id uint) (note (string-ascii 64)))
    (let
        ((batch (unwrap! (map-get? batch-details batch-id) (err err-not-found))))
        (ok true)))

