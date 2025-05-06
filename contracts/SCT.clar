;; (impl-trait .nft-trait.nft-trait)

(define-non-fungible-token sct-batch uint)

(define-constant contract-owner tx-sender)
(define-constant err-owner-only (err u100))
(define-constant err-not-found (err u101))
(define-constant err-already-exists (err u102))
(define-constant err-invalid-stage (err u103))

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

(define-public (certify-fair-trade (batch-id uint))
    (begin
        (asserts! (is-eq tx-sender contract-owner) (err err-owner-only))
        (match (map-get? batch-details batch-id)
            batch (ok (map-set batch-details batch-id (merge batch {fair-trade-certified: true})))
            (err err-not-found)
        )
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
        (ok {
            batch-id: batch-id,
            total-events: event-count,
            timeline: (map-get? batch-timeline (tuple (batch-id batch-id) (event-id event-count)))
        })))

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