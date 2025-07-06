(define-fungible-token sct-rewards)

(define-constant contract-owner tx-sender)
(define-constant err-owner-only (err u200))
(define-constant err-insufficient-balance (err u201))
(define-constant err-not-found (err u202))
(define-constant err-already-claimed (err u203))
(define-constant err-not-eligible (err u204))

(define-data-var reward-pool uint u1000000)
(define-data-var verification-reward uint u100)
(define-data-var quality-bonus uint u50)
(define-data-var certification-reward uint u200)

(define-map batch-rewards
    uint
    {
        total-earned: uint,
        claimed: bool,
        eligible-participants: (list 10 principal),
        reward-per-participant: uint
    }
)

(define-map participant-rewards
    principal
    {
        total-earned: uint,
        batches-completed: uint,
        verification-bonus: uint,
        last-claim-height: uint
    }
)

(define-map reward-eligibility
    { batch-id: uint, participant: principal }
    {
        contributions: uint,
        quality-score: uint,
        verification-status: bool,
        reward-amount: uint
    }
)



(define-public (calculate-batch-rewards (batch-id uint) (participants (list 10 principal)) (quality-scores (list 10 uint)))
    (let
        (
            (total-participants (len participants))
            (total-quality (fold + quality-scores u0))
            (avg-quality (/ total-quality total-participants))
            (base-reward-per-participant (var-get verification-reward))
            (quality-multiplier (if (> avg-quality u80) u2 u1))
            (total-batch-reward (* base-reward-per-participant total-participants quality-multiplier))
            (reward-per-participant (/ total-batch-reward total-participants))
        )
        (begin
            (map-set batch-rewards batch-id
                {
                    total-earned: total-batch-reward,
                    claimed: false,
                    eligible-participants: participants,
                    reward-per-participant: reward-per-participant
                })
            (ok total-batch-reward)
        )
    )
)

(define-public (claim-batch-rewards (batch-id uint))
    (let
        (
            (batch-reward (unwrap! (map-get? batch-rewards batch-id) (err err-not-found)))
            (participants (get eligible-participants batch-reward))
            (reward-amount (get reward-per-participant batch-reward))
            (existing-rewards (default-to { total-earned: u0, batches-completed: u0, verification-bonus: u0, last-claim-height: u0 }
                                          (map-get? participant-rewards tx-sender)))
        )
        (asserts! (not (get claimed batch-reward)) (err err-already-claimed))
        (asserts! (is-participant-in-list tx-sender participants) (err err-not-eligible))
        
        ;; (try! (ft-transfer? sct-rewards reward-amount contract-owner tx-sender))
        
        (map-set participant-rewards tx-sender
            (merge existing-rewards
                { 
                    total-earned: (+ (get total-earned existing-rewards) reward-amount),
                    batches-completed: (+ (get batches-completed existing-rewards) u1),
                    last-claim-height: stacks-block-height
                }))
        (ok reward-amount)
    )
)

(define-private (is-participant-in-list (participant principal) (participants (list 10 principal)))
    (or 
        (is-eq participant (unwrap! (element-at participants u0) false))
        (is-eq participant (unwrap! (element-at participants u1) false))
        (is-eq participant (unwrap! (element-at participants u2) false))
        (is-eq participant (unwrap! (element-at participants u3) false))
        (is-eq participant (unwrap! (element-at participants u4) false))
        (is-eq participant (unwrap! (element-at participants u5) false))
        (is-eq participant (unwrap! (element-at participants u6) false))
        (is-eq participant (unwrap! (element-at participants u7) false))
        (is-eq participant (unwrap! (element-at participants u8) false))
        (is-eq participant (unwrap! (element-at participants u9) false))
    )
)

(define-public (award-verification-bonus (participant principal) (bonus-amount uint))
    (let 
        ((existing-rewards (default-to { total-earned: u0, batches-completed: u0, verification-bonus: u0, last-claim-height: u0 }
                                      (map-get? participant-rewards participant))))
        (asserts! (is-eq tx-sender contract-owner) (err err-owner-only))
        ;; (try! (ft-transfer? sct-rewards bonus-amount contract-owner participant))
        
        (map-set participant-rewards participant
            (merge existing-rewards
                { verification-bonus: (+ (get verification-bonus existing-rewards) bonus-amount) }))
        (ok true)
    )
)

(define-public (set-reward-rates (verification-amt uint) (quality-bonus-amt uint) (certification-amt uint))
    (begin
        (asserts! (is-eq tx-sender contract-owner) (err err-owner-only))
        (var-set verification-reward verification-amt)
        (var-set quality-bonus quality-bonus-amt)
        (var-set certification-reward certification-amt)
        (ok true)
    )
)

(define-public (distribute-certification-rewards (batch-id uint) (participant-1 principal) (participant-2 principal) (participant-3 principal))
    (let
        (
            (reward-amount (var-get certification-reward))
            (reward-per-participant (/ reward-amount u3))
            (existing-rewards-1 (default-to { total-earned: u0, batches-completed: u0, verification-bonus: u0, last-claim-height: u0 }
                                           (map-get? participant-rewards participant-1)))
            (existing-rewards-2 (default-to { total-earned: u0, batches-completed: u0, verification-bonus: u0, last-claim-height: u0 }
                                           (map-get? participant-rewards participant-2)))
            (existing-rewards-3 (default-to { total-earned: u0, batches-completed: u0, verification-bonus: u0, last-claim-height: u0 }
                                           (map-get? participant-rewards participant-3)))
        )
        (asserts! (is-eq tx-sender contract-owner) (err err-owner-only))
        ;; (try! (ft-transfer? sct-rewards reward-per-participant contract-owner participant-1))
        ;; (try! (ft-transfer? sct-rewards reward-per-participant contract-owner participant-2))
        ;; (try! (ft-transfer? sct-rewards reward-per-participant contract-owner participant-3))
        
        (map-set participant-rewards participant-1
            (merge existing-rewards-1
                { total-earned: (+ (get total-earned existing-rewards-1) reward-per-participant) }))
        
        (map-set participant-rewards participant-2
            (merge existing-rewards-2
                { total-earned: (+ (get total-earned existing-rewards-2) reward-per-participant) }))
        
        (map-set participant-rewards participant-3
            (merge existing-rewards-3
                { total-earned: (+ (get total-earned existing-rewards-3) reward-per-participant) }))
        
        (ok true)
    )
)

(define-read-only (get-participant-rewards (participant principal))
    (ok (map-get? participant-rewards participant))
)

(define-read-only (get-batch-reward-info (batch-id uint))
    (ok (map-get? batch-rewards batch-id))
)

(define-read-only (get-token-balance (account principal))
    (ok (ft-get-balance sct-rewards account))
)

(define-read-only (get-reward-rates)
    (ok {
        verification-reward: (var-get verification-reward),
        quality-bonus: (var-get quality-bonus),
        certification-reward: (var-get certification-reward)
    })
)

(define-read-only (calculate-potential-rewards (batch-id uint) (participant principal) (quality-score uint))
    (let
        (
            (base-reward (var-get verification-reward))
            (quality-multiplier (if (> quality-score u80) u2 u1))
            (total-potential (+ (* base-reward quality-multiplier) (if (> quality-score u90) (var-get quality-bonus) u0)))
        )
        (ok total-potential)
    )
)

(define-public (emergency-withdraw (amount uint))
    (begin
        (asserts! (is-eq tx-sender contract-owner) (err err-owner-only))
        ;; (try! (ft-transfer? sct-rewards amount contract-owner tx-sender))
        (ok true)
    )
)
