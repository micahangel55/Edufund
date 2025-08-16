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

(define-constant ERR-SCHOLARSHIP-NOT-FOUND (err u200))
(define-constant ERR-APPLICATION-PERIOD-CLOSED (err u201))
(define-constant ERR-ALREADY-APPLIED (err u202))
(define-constant ERR-SCHOLARSHIP-EXPIRED (err u203))
(define-constant ERR-INSUFFICIENT-SCHOLARSHIP-FUNDS (err u204))
(define-constant ERR-EVALUATION-PERIOD-ACTIVE (err u205))
(define-constant ERR-WINNERS-ALREADY-SELECTED (err u206))
(define-constant ERR-NOT-WINNER (err u207))
(define-constant ERR-ALREADY-CLAIMED (err u208))
(define-constant ERR-NOT-EVALUATOR (err u209))
(define-constant ERR-INVALID-SCORE (err u210))
(define-constant ERR-ALREADY-EVALUATED (err u211))

(define-data-var scholarship-counter uint u0)

(define-map scholarships
    uint
    {
        sponsor: principal,
        title: (string-ascii 100),
        description: (string-ascii 500),
        total-amount: uint,
        individual-amount: uint,
        winners-count: uint,
        application-start: uint,
        application-end: uint,
        evaluation-end: uint,
        status: (string-ascii 20),
        applicants-count: uint,
        evaluators-count: uint,
        evaluation-complete: bool
    }
)

(define-map scholarship-applications
    { scholarship-id: uint, applicant: principal }
    {
        essay: (string-ascii 1000),
        gpa: uint,
        financial-need: uint,
        achievements: (string-ascii 500),
        submitted-at: uint,
        total-score: uint,
        evaluations-count: uint
    }
)

(define-map scholarship-evaluators
    { scholarship-id: uint, evaluator: principal }
    bool
)

(define-map application-evaluations
    { scholarship-id: uint, applicant: principal, evaluator: principal }
    {
        essay-score: uint,
        gpa-score: uint,
        need-score: uint,
        achievements-score: uint,
        total-score: uint
    }
)

(define-map scholarship-winners
    { scholarship-id: uint, winner: principal }
    {
        rank: uint,
        final-score: uint,
        claimed: bool
    }
)

(define-public (create-scholarship 
    (title (string-ascii 100)) 
    (description (string-ascii 500)) 
    (individual-amount uint) 
    (winners-count uint) 
    (application-duration uint) 
    (evaluation-duration uint))
    (let (
        (scholarship-id (+ (var-get scholarship-counter) u1))
        (total-amount (* individual-amount winners-count))
        (application-start (+ stacks-block-height u1))
        (application-end (+ application-start application-duration))
        (evaluation-end (+ application-end evaluation-duration))
        )
        (asserts! (>= individual-amount u50000) ERR-INVALID-AMOUNT)
        (asserts! (>= winners-count u1) ERR-INVALID-AMOUNT)
        (asserts! (>= application-duration u144) ERR-INVALID-AMOUNT)
        (asserts! (>= evaluation-duration u144) ERR-INVALID-AMOUNT)
        (try! (stx-transfer? total-amount tx-sender (as-contract tx-sender)))
        (map-set scholarships 
            scholarship-id
            {
                sponsor: tx-sender,
                title: title,
                description: description,
                total-amount: total-amount,
                individual-amount: individual-amount,
                winners-count: winners-count,
                application-start: application-start,
                application-end: application-end,
                evaluation-end: evaluation-end,
                status: "active",
                applicants-count: u0,
                evaluators-count: u0,
                evaluation-complete: false
            }
        )
        (var-set scholarship-counter scholarship-id)
        (ok scholarship-id)))

(define-public (apply-for-scholarship 
    (scholarship-id uint) 
    (essay (string-ascii 1000)) 
    (gpa uint) 
    (financial-need uint) 
    (achievements (string-ascii 500)))
    (let (
        (scholarship (unwrap! (map-get? scholarships scholarship-id) ERR-SCHOLARSHIP-NOT-FOUND))
        (current-height stacks-block-height)
        )
        (asserts! (>= current-height (get application-start scholarship)) ERR-APPLICATION-PERIOD-CLOSED)
        (asserts! (< current-height (get application-end scholarship)) ERR-APPLICATION-PERIOD-CLOSED)
        (asserts! (is-none (map-get? scholarship-applications {scholarship-id: scholarship-id, applicant: tx-sender})) ERR-ALREADY-APPLIED)
        (asserts! (<= gpa u400) ERR-INVALID-AMOUNT)
        (asserts! (<= financial-need u10) ERR-INVALID-AMOUNT)
        (map-set scholarship-applications 
            {scholarship-id: scholarship-id, applicant: tx-sender}
            {
                essay: essay,
                gpa: gpa,
                financial-need: financial-need,
                achievements: achievements,
                submitted-at: current-height,
                total-score: u0,
                evaluations-count: u0
            }
        )
        (map-set scholarships 
            scholarship-id
            (merge scholarship {applicants-count: (+ (get applicants-count scholarship) u1)})
        )
        (ok true)))

(define-public (register-as-evaluator (scholarship-id uint))
    (let (
        (scholarship (unwrap! (map-get? scholarships scholarship-id) ERR-SCHOLARSHIP-NOT-FOUND))
        (current-height stacks-block-height)
        )
        (asserts! (>= current-height (get application-end scholarship)) ERR-APPLICATION-PERIOD-CLOSED)
        (asserts! (< current-height (get evaluation-end scholarship)) ERR-EVALUATION-PERIOD-ACTIVE)
        (asserts! (is-none (map-get? scholarship-evaluators {scholarship-id: scholarship-id, evaluator: tx-sender})) ERR-ALREADY-APPLIED)
        (map-set scholarship-evaluators 
            {scholarship-id: scholarship-id, evaluator: tx-sender}
            true
        )
        (map-set scholarships 
            scholarship-id
            (merge scholarship {evaluators-count: (+ (get evaluators-count scholarship) u1)})
        )
        (ok true)))

(define-public (evaluate-application 
    (scholarship-id uint) 
    (applicant principal) 
    (essay-score uint) 
    (gpa-score uint) 
    (need-score uint) 
    (achievements-score uint))
    (let (
        (scholarship (unwrap! (map-get? scholarships scholarship-id) ERR-SCHOLARSHIP-NOT-FOUND))
        (application (unwrap! (map-get? scholarship-applications {scholarship-id: scholarship-id, applicant: applicant}) ERR-PROPOSAL-NOT-FOUND))
        (current-height stacks-block-height)
        (total-score (+ essay-score gpa-score need-score achievements-score))
        )
        (asserts! (>= current-height (get application-end scholarship)) ERR-APPLICATION-PERIOD-CLOSED)
        (asserts! (< current-height (get evaluation-end scholarship)) ERR-EVALUATION-PERIOD-ACTIVE)
        (asserts! (is-some (map-get? scholarship-evaluators {scholarship-id: scholarship-id, evaluator: tx-sender})) ERR-NOT-EVALUATOR)
        (asserts! (is-none (map-get? application-evaluations {scholarship-id: scholarship-id, applicant: applicant, evaluator: tx-sender})) ERR-ALREADY-EVALUATED)
        (asserts! (<= essay-score u25) ERR-INVALID-SCORE)
        (asserts! (<= gpa-score u25) ERR-INVALID-SCORE)
        (asserts! (<= need-score u25) ERR-INVALID-SCORE)
        (asserts! (<= achievements-score u25) ERR-INVALID-SCORE)
        (map-set application-evaluations 
            {scholarship-id: scholarship-id, applicant: applicant, evaluator: tx-sender}
            {
                essay-score: essay-score,
                gpa-score: gpa-score,
                need-score: need-score,
                achievements-score: achievements-score,
                total-score: total-score
            }
        )
        (map-set scholarship-applications 
            {scholarship-id: scholarship-id, applicant: applicant}
            (merge application {
                total-score: (+ (get total-score application) total-score),
                evaluations-count: (+ (get evaluations-count application) u1)
            })
        )
        (ok true)))

(define-public (select-winners (scholarship-id uint) (winners (list 10 principal)))
    (let (
        (scholarship (unwrap! (map-get? scholarships scholarship-id) ERR-SCHOLARSHIP-NOT-FOUND))
        (current-height stacks-block-height)
        )
        (asserts! (is-eq (get sponsor scholarship) tx-sender) ERR-NOT-AUTHORIZED)
        (asserts! (>= current-height (get evaluation-end scholarship)) ERR-EVALUATION-PERIOD-ACTIVE)
        (asserts! (not (get evaluation-complete scholarship)) ERR-WINNERS-ALREADY-SELECTED)
        (asserts! (is-eq (len winners) (get winners-count scholarship)) ERR-INVALID-AMOUNT)
        (process-winners scholarship-id winners)
        (map-set scholarships 
            scholarship-id
            (merge scholarship {
                status: "completed",
                evaluation-complete: true
            })
        )
        (ok true)))

(define-private (process-winners (scholarship-id uint) (winners (list 10 principal)))
    (fold process-winner-entry winners {scholarship-id: scholarship-id, rank: u1, success: true}))

(define-private (process-winner-entry (winner principal) (context {scholarship-id: uint, rank: uint, success: bool}))
    (let (
        (scholarship-id (get scholarship-id context))
        (rank (get rank context))
        (application (map-get? scholarship-applications {scholarship-id: scholarship-id, applicant: winner}))
        )
        (if (get success context)
            (match application
                some-app (begin
                    (map-set scholarship-winners 
                        {scholarship-id: scholarship-id, winner: winner}
                        {
                            rank: rank,
                            final-score: (get total-score some-app),
                            claimed: false
                        }
                    )
                    {scholarship-id: scholarship-id, rank: (+ rank u1), success: true}
                )
                {scholarship-id: scholarship-id, rank: rank, success: false}
            )
            context
        )
    ))

(define-public (claim-scholarship (scholarship-id uint))
    (let (
        (scholarship (unwrap! (map-get? scholarships scholarship-id) ERR-SCHOLARSHIP-NOT-FOUND))
        (winner-data (unwrap! (map-get? scholarship-winners {scholarship-id: scholarship-id, winner: tx-sender}) ERR-NOT-WINNER))
        )
        (asserts! (get evaluation-complete scholarship) ERR-EVALUATION-PERIOD-ACTIVE)
        (asserts! (not (get claimed winner-data)) ERR-ALREADY-CLAIMED)
        (try! (as-contract (stx-transfer? (get individual-amount scholarship) tx-sender tx-sender)))
        (map-set scholarship-winners 
            {scholarship-id: scholarship-id, winner: tx-sender}
            (merge winner-data {claimed: true})
        )
        (ok (get individual-amount scholarship))))

(define-read-only (get-scholarship (scholarship-id uint))
    (ok (map-get? scholarships scholarship-id)))

(define-read-only (get-scholarship-application (scholarship-id uint) (applicant principal))
    (ok (map-get? scholarship-applications {scholarship-id: scholarship-id, applicant: applicant})))

(define-read-only (get-scholarship-winner (scholarship-id uint) (winner principal))
    (ok (map-get? scholarship-winners {scholarship-id: scholarship-id, winner: winner})))

(define-read-only (is-scholarship-evaluator (scholarship-id uint) (evaluator principal))
    (is-some (map-get? scholarship-evaluators {scholarship-id: scholarship-id, evaluator: evaluator})))

(define-read-only (get-application-evaluation (scholarship-id uint) (applicant principal) (evaluator principal))
    (ok (map-get? application-evaluations {scholarship-id: scholarship-id, applicant: applicant, evaluator: evaluator})))

(define-constant ERR-RESOURCE-NOT-FOUND (err u300))
(define-constant ERR-SUPPLIER-NOT-FOUND (err u301))
(define-constant ERR-ALREADY-FULFILLED (err u302))
(define-constant ERR-REQUEST-EXPIRED (err u303))
(define-constant ERR-INVALID-PRICE (err u304))
(define-constant ERR-NOT-SUPPLIER (err u305))
(define-constant ERR-ALREADY-CONFIRMED (err u306))
(define-constant ERR-NOT-DONOR (err u307))
(define-constant ERR-RESOURCE-UNAVAILABLE (err u308))
(define-constant ERR-ALREADY-BORROWED (err u309))
(define-constant ERR-NOT-OWNER (err u310))
(define-constant ERR-NOT-BORROWER (err u311))
(define-constant RESOURCE-REQUEST-DURATION u2880)
(define-constant MIN-RESOURCE-PRICE u10000)

(define-data-var resource-counter uint u0)
(define-data-var supplier-counter uint u0)

(define-map resource-suppliers
    uint
    {
        owner: principal,
        name: (string-ascii 100),
        category: (string-ascii 50),
        contact-info: (string-ascii 200),
        verified: bool,
        total-sales: uint,
        reputation-score: uint
    }
)

(define-map resource-requests
    uint
    {
        student: principal,
        title: (string-ascii 100),
        description: (string-ascii 500),
        category: (string-ascii 50),
        target-price: uint,
        urgency: uint,
        supplier-id: uint,
        created-at: uint,
        expires-at: uint,
        status: (string-ascii 20),
        donor: (optional principal),
        fulfilled-at: (optional uint),
        delivery-confirmed: bool
    }
)

(define-map resource-inventory
    { supplier-id: uint, resource-title: (string-ascii 100) }
    {
        price: uint,
        stock-quantity: uint,
        condition: (string-ascii 20),
        description: (string-ascii 300),
        availability: bool
    }
)

(define-map resource-fulfillments
    { request-id: uint, donor: principal }
    {
        amount-paid: uint,
        purchase-date: uint,
        tracking-info: (string-ascii 100),
        delivery-status: (string-ascii 30)
    }
)

(define-map borrowed-resources
    { resource-id: uint, borrower: principal }
    {
        owner: principal,
        borrowed-at: uint,
        return-due: uint,
        deposit-amount: uint,
        returned: bool
    }
)

(define-public (register-supplier 
    (name (string-ascii 100)) 
    (category (string-ascii 50)) 
    (contact-info (string-ascii 200)))
    (let ((supplier-id (+ (var-get supplier-counter) u1)))
        (map-set resource-suppliers 
            supplier-id
            {
                owner: tx-sender,
                name: name,
                category: category,
                contact-info: contact-info,
                verified: false,
                total-sales: u0,
                reputation-score: u100
            }
        )
        (var-set supplier-counter supplier-id)
        (ok supplier-id)))

(define-public (add-resource-to-inventory 
    (supplier-id uint) 
    (resource-title (string-ascii 100)) 
    (price uint) 
    (stock-quantity uint) 
    (condition (string-ascii 20)) 
    (description (string-ascii 300)))
    (let ((supplier (unwrap! (map-get? resource-suppliers supplier-id) ERR-SUPPLIER-NOT-FOUND)))
        (asserts! (is-eq (get owner supplier) tx-sender) ERR-NOT-SUPPLIER)
        (asserts! (>= price MIN-RESOURCE-PRICE) ERR-INVALID-PRICE)
        (asserts! (> stock-quantity u0) ERR-INVALID-AMOUNT)
        (map-set resource-inventory 
            {supplier-id: supplier-id, resource-title: resource-title}
            {
                price: price,
                stock-quantity: stock-quantity,
                condition: condition,
                description: description,
                availability: true
            }
        )
        (ok true)))

(define-public (create-resource-request 
    (title (string-ascii 100)) 
    (description (string-ascii 500)) 
    (category (string-ascii 50)) 
    (target-price uint) 
    (urgency uint) 
    (supplier-id uint))
    (let (
        (request-id (+ (var-get resource-counter) u1))
        (current-height stacks-block-height)
        (expires-at (+ current-height RESOURCE-REQUEST-DURATION))
        )
        (asserts! (>= target-price MIN-RESOURCE-PRICE) ERR-INVALID-PRICE)
        (asserts! (<= urgency u5) ERR-INVALID-AMOUNT)
        (asserts! (is-some (map-get? resource-suppliers supplier-id)) ERR-SUPPLIER-NOT-FOUND)
        (map-set resource-requests 
            request-id
            {
                student: tx-sender,
                title: title,
                description: description,
                category: category,
                target-price: target-price,
                urgency: urgency,
                supplier-id: supplier-id,
                created-at: current-height,
                expires-at: expires-at,
                status: "open",
                donor: none,
                fulfilled-at: none,
                delivery-confirmed: false
            }
        )
        (var-set resource-counter request-id)
        (ok request-id)))

(define-public (fulfill-resource-request (request-id uint) (tracking-info (string-ascii 100)))
    (let (
        (request (unwrap! (map-get? resource-requests request-id) ERR-RESOURCE-NOT-FOUND))
        (supplier-id (get supplier-id request))
        (resource-title (get title request))
        (target-price (get target-price request))
        (current-height stacks-block-height)
        (inventory-item (map-get? resource-inventory {supplier-id: supplier-id, resource-title: resource-title}))
        )
        (asserts! (< current-height (get expires-at request)) ERR-REQUEST-EXPIRED)
        (asserts! (is-eq (get status request) "open") ERR-ALREADY-FULFILLED)
        (match inventory-item
            some-item (asserts! (and (get availability some-item) (> (get stock-quantity some-item) u0)) ERR-RESOURCE-UNAVAILABLE)
            (asserts! false ERR-RESOURCE-NOT-FOUND)
        )
        (try! (stx-transfer? target-price tx-sender (as-contract tx-sender)))
        (map-set resource-requests 
            request-id
            (merge request {
                status: "fulfilled",
                donor: (some tx-sender),
                fulfilled-at: (some current-height)
            })
        )
        (map-set resource-fulfillments 
            {request-id: request-id, donor: tx-sender}
            {
                amount-paid: target-price,
                purchase-date: current-height,
                tracking-info: tracking-info,
                delivery-status: "shipped"
            }
        )
        (match inventory-item
            some-item (map-set resource-inventory 
                {supplier-id: supplier-id, resource-title: resource-title}
                (merge some-item {stock-quantity: (- (get stock-quantity some-item) u1)})
            )
            false
        )
        (ok true)))

(define-public (confirm-resource-delivery (request-id uint))
    (let (
        (request (unwrap! (map-get? resource-requests request-id) ERR-RESOURCE-NOT-FOUND))
        (supplier-id (get supplier-id request))
        (supplier (unwrap! (map-get? resource-suppliers supplier-id) ERR-SUPPLIER-NOT-FOUND))
        )
        (asserts! (is-eq (get student request) tx-sender) ERR-NOT-AUTHORIZED)
        (asserts! (is-eq (get status request) "fulfilled") ERR-NOT-AUTHORIZED)
        (asserts! (not (get delivery-confirmed request)) ERR-ALREADY-CONFIRMED)
        (match (get donor request)
            some-donor (match (get fulfilled-at request)
                some-fulfillment-date (begin
                    (try! (as-contract (stx-transfer? (get target-price request) tx-sender (get owner supplier))))
                    (map-set resource-requests 
                        request-id
                        (merge request {
                            status: "completed",
                            delivery-confirmed: true
                        })
                    )
                    (map-set resource-fulfillments 
                        {request-id: request-id, donor: some-donor}
                        (merge (unwrap-panic (map-get? resource-fulfillments {request-id: request-id, donor: some-donor})) 
                            {delivery-status: "delivered"})
                    )
                    (map-set resource-suppliers 
                        supplier-id
                        (merge supplier {
                            total-sales: (+ (get total-sales supplier) u1),
                            reputation-score: (if (> (+ (get reputation-score supplier) u5) u1000) u1000 (+ (get reputation-score supplier) u5))
                        })
                    )
                    (ok true)
                )
                ERR-NOT-AUTHORIZED
            )
            ERR-NOT-DONOR
        )))

(define-public (list-resource-for-sharing 
    (resource-title (string-ascii 100)) 
    (description (string-ascii 300)) 
    (deposit-amount uint) 
    (max-borrow-duration uint))
    (let ((resource-id (+ (var-get resource-counter) u1)))
        (asserts! (>= deposit-amount u10000) ERR-INVALID-AMOUNT)
        (asserts! (>= max-borrow-duration u144) ERR-INVALID-AMOUNT)
        (map-set resource-inventory 
            {supplier-id: u0, resource-title: resource-title}
            {
                price: deposit-amount,
                stock-quantity: u1,
                condition: "shared",
                description: description,
                availability: true
            }
        )
        (var-set resource-counter resource-id)
        (ok resource-id)))

(define-public (borrow-resource (resource-id uint) (borrow-duration uint))
    (let (
        (resource-key {supplier-id: u0, resource-title: (unwrap-panic (element-at? (list "shared-resource") u0))})
        (resource-item (unwrap! (map-get? resource-inventory resource-key) ERR-RESOURCE-NOT-FOUND))
        (deposit-amount (get price resource-item))
        (current-height stacks-block-height)
        (return-due (+ current-height borrow-duration))
        )
        (asserts! (get availability resource-item) ERR-RESOURCE-UNAVAILABLE)
        (asserts! (is-none (map-get? borrowed-resources {resource-id: resource-id, borrower: tx-sender})) ERR-ALREADY-BORROWED)
        (asserts! (>= borrow-duration u144) ERR-INVALID-AMOUNT)
        (try! (stx-transfer? deposit-amount tx-sender (as-contract tx-sender)))
        (map-set borrowed-resources 
            {resource-id: resource-id, borrower: tx-sender}
            {
                owner: tx-sender,
                borrowed-at: current-height,
                return-due: return-due,
                deposit-amount: deposit-amount,
                returned: false
            }
        )
        (map-set resource-inventory 
            resource-key
            (merge resource-item {availability: false})
        )
        (ok true)))

(define-public (return-borrowed-resource (resource-id uint))
    (let (
        (borrow-record (unwrap! (map-get? borrowed-resources {resource-id: resource-id, borrower: tx-sender}) ERR-NOT-BORROWER))
        (resource-key {supplier-id: u0, resource-title: (unwrap-panic (element-at? (list "shared-resource") u0))})
        (resource-item (unwrap! (map-get? resource-inventory resource-key) ERR-RESOURCE-NOT-FOUND))
        (deposit-amount (get deposit-amount borrow-record))
        )
        (asserts! (not (get returned borrow-record)) ERR-ALREADY-CONFIRMED)
        (try! (as-contract (stx-transfer? deposit-amount tx-sender tx-sender)))
        (map-set borrowed-resources 
            {resource-id: resource-id, borrower: tx-sender}
            (merge borrow-record {returned: true})
        )
        (map-set resource-inventory 
            resource-key
            (merge resource-item {availability: true})
        )
        (ok deposit-amount)))

(define-read-only (get-resource-request (request-id uint))
    (ok (map-get? resource-requests request-id)))

(define-read-only (get-supplier (supplier-id uint))
    (ok (map-get? resource-suppliers supplier-id)))

(define-read-only (get-inventory-item (supplier-id uint) (resource-title (string-ascii 100)))
    (ok (map-get? resource-inventory {supplier-id: supplier-id, resource-title: resource-title})))

(define-read-only (get-fulfillment (request-id uint) (donor principal))
    (ok (map-get? resource-fulfillments {request-id: request-id, donor: donor})))

(define-read-only (get-borrow-record (resource-id uint) (borrower principal))
    (ok (map-get? borrowed-resources {resource-id: resource-id, borrower: borrower})))


