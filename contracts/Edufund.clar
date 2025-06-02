(define-constant ERR-NOT-AUTHORIZED (err u100))
(define-constant ERR-INVALID-AMOUNT (err u101))
(define-constant ERR-NO-FUNDS (err u102))
(define-constant ERR-ALREADY-VOTED (err u103))
(define-constant ERR-PROPOSAL-EXPIRED (err u104))
(define-constant ERR-PROPOSAL-NOT-FOUND (err u105))
(define-constant PROPOSAL-DURATION u1440)
(define-constant MIN-PROPOSAL-AMOUNT u100000)
(define-constant VOTE_THRESHOLD u3)

(define-data-var total-funds uint u0)
(define-data-var proposal-counter uint u0)

(define-map proposals 
    uint 
    {
        student: principal,
        amount: uint,
        description: (string-ascii 256),
        votes: uint,
        expires-at: uint,
        status: (string-ascii 20),
        claimed: bool
    }
)

(define-map votes 
    { proposal-id: uint, voter: principal } 
    bool
)

(define-public (donate) 
    (let ((amount (stx-get-balance tx-sender)))
        (try! (stx-transfer? amount tx-sender (as-contract tx-sender)))
        (var-set total-funds (+ (var-get total-funds) amount))
        (ok amount)))

(define-public (create-proposal (student principal) (amount uint) (description (string-ascii 256)))
    (let ((proposal-id (+ (var-get proposal-counter) u1)))
        (asserts! (>= amount MIN-PROPOSAL-AMOUNT) ERR-INVALID-AMOUNT)
        (map-set proposals 
            proposal-id
            {
                student: student,
                amount: amount,
                description: description,
                votes: u0,
                expires-at: (+ stacks-block-height PROPOSAL-DURATION),
                status: "active",
                claimed: false
            }
        )
        (var-set proposal-counter proposal-id)
        (ok proposal-id)))

(define-public (vote (proposal-id uint))
    (let ((proposal (unwrap! (map-get? proposals proposal-id) ERR-PROPOSAL-NOT-FOUND)))
        (asserts! (< stacks-block-height (get expires-at proposal)) ERR-PROPOSAL-EXPIRED)
        (asserts! (is-none (map-get? votes {proposal-id: proposal-id, voter: tx-sender})) ERR-ALREADY-VOTED)
        (map-set votes {proposal-id: proposal-id, voter: tx-sender} true)
        (map-set proposals 
            proposal-id
            (merge proposal {votes: (+ (get votes proposal) u1)})
        )
        (ok true)))

(define-public (claim-funds (proposal-id uint))
    (let (
        (proposal (unwrap! (map-get? proposals proposal-id) ERR-PROPOSAL-NOT-FOUND))
        (amount (get amount proposal))
        )
        (asserts! (is-eq (get student proposal) tx-sender) ERR-NOT-AUTHORIZED)
        (asserts! (>= (get votes proposal) VOTE_THRESHOLD) ERR-NOT-AUTHORIZED)
        (asserts! (not (get claimed proposal)) ERR-NOT-AUTHORIZED)
        (asserts! (>= (var-get total-funds) amount) ERR-NO-FUNDS)
        (try! (as-contract (stx-transfer? amount tx-sender (get student proposal))))
        (var-set total-funds (- (var-get total-funds) amount))
        (map-set proposals 
            proposal-id
            (merge proposal {claimed: true, status: "completed"})
        )
        (ok amount)))

(define-read-only (get-proposal (proposal-id uint))
    (ok (map-get? proposals proposal-id)))

(define-read-only (get-total-funds)
    (ok (var-get total-funds)))

(define-read-only (has-voted (proposal-id uint) (voter principal))
    (is-some (map-get? votes {proposal-id: proposal-id, voter: voter})))


