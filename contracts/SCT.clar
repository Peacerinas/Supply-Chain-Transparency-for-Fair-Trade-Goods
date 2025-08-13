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

;; =================================================================================
;; CARBON FOOTPRINT TRACKING SYSTEM
;; =================================================================================

;; Error constants for carbon tracking
(define-constant err-carbon-limit-exceeded (err u300))
(define-constant err-invalid-transport-method (err u301))
(define-constant err-carbon-already-recorded (err u302))
(define-constant err-invalid-emission-value (err u303))

;; Carbon footprint data for each stage
(define-map stage-carbon-footprint
    { batch-id: uint, stage-id: uint }
    {
        co2-emissions: uint,        ;; CO2 in grams
        transport-method: (string-ascii 32),
        distance-km: uint,
        energy-consumption: uint,    ;; Energy in kWh
        offset-applied: uint,        ;; Carbon offset in grams CO2
        recorded-by: principal,
        timestamp: uint
    }
)

;; Total carbon footprint summary per batch
(define-map batch-carbon-summary
    uint
    {
        total-co2-emissions: uint,
        total-distance: uint,
        total-energy-used: uint,
        total-offsets: uint,
        net-carbon-footprint: uint,
        sustainability-score: uint,  ;; Score out of 100
        carbon-neutral: bool,
        last-updated: uint
    }
)

;; Carbon emission limits and thresholds
(define-data-var max-carbon-per-stage uint u5000)     ;; Max 5kg CO2 per stage
(define-data-var carbon-neutral-threshold uint u100)  ;; Max 100g net emissions for carbon neutral
(define-data-var sustainability-multiplier uint u20)  ;; Multiplier for sustainability score calculation

;; Valid transport methods with emission factors (grams CO2 per km)
(define-map transport-emission-factors
    (string-ascii 32)
    uint
)

;; Carbon offset registry
(define-map carbon-offsets
    { batch-id: uint, offset-id: uint }
    {
        offset-type: (string-ascii 32),
        co2-offset: uint,
        verification-authority: principal,
        purchase-date: uint,
        cost-per-tonne: uint
    }
)

(define-map batch-offset-counter
    uint
    uint
)

;; Initialize transport emission factors
(define-public (initialize-transport-factors)
    (begin
        (asserts! (is-eq tx-sender contract-owner) (err err-owner-only))
        (map-set transport-emission-factors "truck" u300)      ;; 300g CO2/km
        (map-set transport-emission-factors "ship" u50)       ;; 50g CO2/km
        (map-set transport-emission-factors "plane" u1200)    ;; 1200g CO2/km
        (map-set transport-emission-factors "train" u80)      ;; 80g CO2/km
        (map-set transport-emission-factors "electric-truck" u100) ;; 100g CO2/km
        (map-set transport-emission-factors "bicycle" u0)     ;; 0g CO2/km
        (ok true)
    )
)

;; Record carbon footprint for a specific stage
(define-public (record-stage-carbon-footprint 
    (batch-id uint) 
    (stage-id uint) 
    (transport-method (string-ascii 32)) 
    (distance-km uint) 
    (energy-consumption uint))
    (let
        (
            (batch (unwrap! (map-get? batch-details batch-id) (err err-not-found)))
            (emission-factor (unwrap! (map-get? transport-emission-factors transport-method) (err err-invalid-transport-method)))
            (transport-emissions (* distance-km emission-factor))
            (energy-emissions (* energy-consumption u400))  ;; Assume 400g CO2 per kWh
            (total-emissions (+ transport-emissions energy-emissions))
            (existing-record (map-get? stage-carbon-footprint { batch-id: batch-id, stage-id: stage-id }))
        )
        ;; Validate inputs
        (asserts! (<= stage-id (get current-stage batch)) (err err-invalid-stage))
        (asserts! (is-none existing-record) (err err-carbon-already-recorded))
        (asserts! (<= total-emissions (var-get max-carbon-per-stage)) (err err-carbon-limit-exceeded))
        
        ;; Record stage carbon footprint
        (map-set stage-carbon-footprint { batch-id: batch-id, stage-id: stage-id }
            {
                co2-emissions: total-emissions,
                transport-method: transport-method,
                distance-km: distance-km,
                energy-consumption: energy-consumption,
                offset-applied: u0,
                recorded-by: tx-sender,
                timestamp: stacks-block-height
            })
        
        ;; Update batch carbon summary
        (try! (update-batch-carbon-summary batch-id))
        (ok total-emissions)
    )
)

;; Purchase and apply carbon offsets
(define-public (purchase-carbon-offset 
    (batch-id uint) 
    (offset-type (string-ascii 32)) 
    (co2-offset uint) 
    (cost-per-tonne uint))
    (let
        (
            (batch (unwrap! (map-get? batch-details batch-id) (err err-not-found)))
            (current-offset-count (default-to u0 (map-get? batch-offset-counter batch-id)))
            (new-offset-id (+ current-offset-count u1))
        )
        (asserts! (> co2-offset u0) (err err-invalid-emission-value))
        
        ;; Record carbon offset purchase
        (map-set carbon-offsets { batch-id: batch-id, offset-id: new-offset-id }
            {
                offset-type: offset-type,
                co2-offset: co2-offset,
                verification-authority: tx-sender,
                purchase-date: stacks-block-height,
                cost-per-tonne: cost-per-tonne
            })
        
        (map-set batch-offset-counter batch-id new-offset-id)
        
        ;; Update batch carbon summary
        (try! (update-batch-carbon-summary batch-id))
        (ok new-offset-id)
    )
)

;; Calculate and update batch carbon summary
(define-private (update-batch-carbon-summary (batch-id uint))
    (let
        (
            (batch (unwrap! (map-get? batch-details batch-id) (err err-not-found)))
            (current-stage (get current-stage batch))
            (stage-totals (calculate-stage-totals batch-id current-stage))
            (total-offsets (calculate-total-offsets batch-id))
            (total-emissions (get total-emissions stage-totals))
            (total-distance (get total-distance stage-totals))
            (total-energy (get total-energy stage-totals))
            (net-footprint (if (> total-emissions total-offsets) (- total-emissions total-offsets) u0))
            (sustainability-score (calculate-sustainability-score total-emissions total-offsets total-distance))
            (is-carbon-neutral (<= net-footprint (var-get carbon-neutral-threshold)))
        )
        (map-set batch-carbon-summary batch-id
            {
                total-co2-emissions: total-emissions,
                total-distance: total-distance,
                total-energy-used: total-energy,
                total-offsets: total-offsets,
                net-carbon-footprint: net-footprint,
                sustainability-score: sustainability-score,
                carbon-neutral: is-carbon-neutral,
                last-updated: stacks-block-height
            })
        (ok true)
    )
)

;; Helper function to calculate stage totals
(define-private (calculate-stage-totals (batch-id uint) (max-stage uint))
    (let
        (
            (stage-1 (default-to { co2-emissions: u0, distance-km: u0, energy-consumption: u0 } 
                                 (map-get? stage-carbon-footprint { batch-id: batch-id, stage-id: u1 })))
            (stage-2 (default-to { co2-emissions: u0, distance-km: u0, energy-consumption: u0 } 
                                 (map-get? stage-carbon-footprint { batch-id: batch-id, stage-id: u2 })))
            (stage-3 (default-to { co2-emissions: u0, distance-km: u0, energy-consumption: u0 } 
                                 (map-get? stage-carbon-footprint { batch-id: batch-id, stage-id: u3 })))
            (stage-4 (default-to { co2-emissions: u0, distance-km: u0, energy-consumption: u0 } 
                                 (map-get? stage-carbon-footprint { batch-id: batch-id, stage-id: u4 })))
            (stage-5 (default-to { co2-emissions: u0, distance-km: u0, energy-consumption: u0 } 
                                 (map-get? stage-carbon-footprint { batch-id: batch-id, stage-id: u5 })))
        )
        {
            total-emissions: (+ (+ (+ (+ (get co2-emissions stage-1) (get co2-emissions stage-2)) 
                                      (get co2-emissions stage-3)) (get co2-emissions stage-4)) (get co2-emissions stage-5)),
            total-distance: (+ (+ (+ (+ (get distance-km stage-1) (get distance-km stage-2)) 
                                     (get distance-km stage-3)) (get distance-km stage-4)) (get distance-km stage-5)),
            total-energy: (+ (+ (+ (+ (get energy-consumption stage-1) (get energy-consumption stage-2)) 
                                   (get energy-consumption stage-3)) (get energy-consumption stage-4)) (get energy-consumption stage-5))
        }
    )
)

;; Helper function to calculate total offsets
(define-private (calculate-total-offsets (batch-id uint))
    (let
        (
            (offset-count (default-to u0 (map-get? batch-offset-counter batch-id)))
            (offset-1 (if (>= offset-count u1) (default-to { co2-offset: u0 } (map-get? carbon-offsets { batch-id: batch-id, offset-id: u1 })) { co2-offset: u0 }))
            (offset-2 (if (>= offset-count u2) (default-to { co2-offset: u0 } (map-get? carbon-offsets { batch-id: batch-id, offset-id: u2 })) { co2-offset: u0 }))
            (offset-3 (if (>= offset-count u3) (default-to { co2-offset: u0 } (map-get? carbon-offsets { batch-id: batch-id, offset-id: u3 })) { co2-offset: u0 }))
        )
        (+ (+ (get co2-offset offset-1) (get co2-offset offset-2)) (get co2-offset offset-3))
    )
)

;; Calculate sustainability score (0-100)
(define-private (calculate-sustainability-score (total-emissions uint) (total-offsets uint) (total-distance uint))
    (let
        (
            (efficiency-score (if (> total-distance u0) (/ u100000 (/ total-emissions total-distance)) u100))
            (offset-score (if (> total-emissions u0) (/ (* total-offsets u100) total-emissions) u100))
            (combined-score (/ (+ efficiency-score offset-score) u2))
        )
        (if (> combined-score u100) u100 combined-score)
    )
)

;; Read-only functions for carbon tracking
(define-read-only (get-stage-carbon-footprint (batch-id uint) (stage-id uint))
    (ok (map-get? stage-carbon-footprint { batch-id: batch-id, stage-id: stage-id }))
)

(define-read-only (get-batch-carbon-summary (batch-id uint))
    (ok (map-get? batch-carbon-summary batch-id))
)

(define-read-only (get-carbon-offset (batch-id uint) (offset-id uint))
    (ok (map-get? carbon-offsets { batch-id: batch-id, offset-id: offset-id }))
)

(define-read-only (get-transport-emission-factor (transport-method (string-ascii 32)))
    (ok (map-get? transport-emission-factors transport-method))
)

(define-read-only (is-batch-carbon-neutral (batch-id uint))
    (match (map-get? batch-carbon-summary batch-id)
        summary (ok (get carbon-neutral summary))
        (err err-not-found)
    )
)

;; Administrative functions
(define-public (set-carbon-limits (max-per-stage uint) (neutral-threshold uint) (score-multiplier uint))
    (begin
        (asserts! (is-eq tx-sender contract-owner) (err err-owner-only))
        (var-set max-carbon-per-stage max-per-stage)
        (var-set carbon-neutral-threshold neutral-threshold)
        (var-set sustainability-multiplier score-multiplier)
        (ok true)
    )
)

(define-public (add-transport-method (method (string-ascii 32)) (emission-factor uint))
    (begin
        (asserts! (is-eq tx-sender contract-owner) (err err-owner-only))
        (map-set transport-emission-factors method emission-factor)
        (ok true)
    )
)

