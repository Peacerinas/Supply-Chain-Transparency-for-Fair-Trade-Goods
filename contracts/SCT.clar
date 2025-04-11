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
