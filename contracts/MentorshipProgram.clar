;; MentorshipProgram - Student-Mentor Matching and Session Management System
;; Connects students with qualified mentors for guidance, career advice, and academic support

;; Constants
(define-constant contract-owner tx-sender)
(define-constant err-not-authorized (err u400))
(define-constant err-mentor-not-found (err u401))
(define-constant err-student-not-found (err u402))
(define-constant err-session-not-found (err u403))
(define-constant err-invalid-amount (err u404))
(define-constant err-mentor-not-available (err u405))
(define-constant err-already-registered (err u406))
(define-constant err-session-already-completed (err u407))
(define-constant err-invalid-rating (err u408))
(define-constant err-insufficient-balance (err u409))
(define-constant err-session-not-confirmed (err u410))
(define-constant err-already-rated (err u411))
(define-constant err-session-expired (err u412))

;; Data Variables
(define-data-var mentor-counter uint u0)
(define-data-var session-counter uint u0)
(define-data-var platform-fee-rate uint u10) ;; 10% platform fee

;; Data Maps

;; Mentor profiles and qualifications
(define-map mentors
    principal ;; mentor address
    {
        name: (string-ascii 100),
        expertise: (string-ascii 200),
        bio: (string-ascii 500),
        experience-years: uint,
        hourly-rate: uint,
        total-sessions: uint,
        average-rating: uint,
        total-ratings: uint,
        available: bool,
        verified: bool,
        registration-date: uint,
        total-earnings: uint
    }
)

;; Student profiles for mentorship
(define-map students
    principal ;; student address
    {
        name: (string-ascii 100),
        field-of-study: (string-ascii 100),
        academic-level: (string-ascii 50),
        goals: (string-ascii 300),
        total-sessions: uint,
        balance: uint,
        registration-date: uint
    }
)

;; Mentorship sessions
(define-map mentorship-sessions
    uint ;; session-id
    {
        mentor: principal,
        student: principal,
        session-type: (string-ascii 50), ;; "career", "academic", "general"
        scheduled-time: uint,
        duration-hours: uint,
        session-fee: uint,
        platform-fee: uint,
        status: (string-ascii 20), ;; "scheduled", "completed", "cancelled"
        session-notes: (string-ascii 500),
        completion-date: uint,
        student-rating: uint,
        mentor-rating: uint,
        paid: bool
    }
)

;; Session ratings and feedback
(define-map session-feedback
    {session-id: uint, rater: principal}
    {
        rating: uint,
        feedback: (string-ascii 300),
        submitted-at: uint
    }
)

;; Mentor availability calendar (simplified)
(define-map mentor-availability
    {mentor: principal, time-slot: uint}
    bool
)

;; Student mentor preferences
(define-map mentor-preferences
    principal ;; student
    {
        preferred-expertise: (string-ascii 200),
        max-hourly-rate: uint,
        preferred-session-type: (string-ascii 50)
    }
)

;; Private Functions
(define-private (is-owner)
    (is-eq tx-sender contract-owner)
)

(define-private (calculate-platform-fee (amount uint))
    (/ (* amount (var-get platform-fee-rate)) u100)
)

;; Public Functions

;; Register as a mentor
(define-public (register-mentor 
    (name (string-ascii 100))
    (expertise (string-ascii 200))
    (bio (string-ascii 500))
    (experience-years uint)
    (hourly-rate uint))
    (begin
        (asserts! (is-none (map-get? mentors tx-sender)) err-already-registered)
        (asserts! (>= hourly-rate u1000) err-invalid-amount) ;; Minimum $10 equivalent
        (asserts! (>= experience-years u1) err-invalid-amount)
        
        (map-set mentors tx-sender
            {
                name: name,
                expertise: expertise,
                bio: bio,
                experience-years: experience-years,
                hourly-rate: hourly-rate,
                total-sessions: u0,
                average-rating: u0,
                total-ratings: u0,
                available: true,
                verified: false,
                registration-date: stacks-block-height,
                total-earnings: u0
            }
        )
        (var-set mentor-counter (+ (var-get mentor-counter) u1))
        (ok true)
    )
)

;; Register as a student
(define-public (register-student
    (name (string-ascii 100))
    (field-of-study (string-ascii 100))
    (academic-level (string-ascii 50))
    (goals (string-ascii 300)))
    (begin
        (asserts! (is-none (map-get? students tx-sender)) err-already-registered)
        
        (map-set students tx-sender
            {
                name: name,
                field-of-study: field-of-study,
                academic-level: academic-level,
                goals: goals,
                total-sessions: u0,
                balance: u0,
                registration-date: stacks-block-height
            }
        )
        (ok true)
    )
)

;; Student adds funds to their balance
(define-public (add-student-balance (amount uint))
    (let
        ((student (unwrap! (map-get? students tx-sender) err-student-not-found)))
        
        (asserts! (> amount u0) err-invalid-amount)
        
        ;; Transfer STX to contract
        (try! (stx-transfer? amount tx-sender (as-contract tx-sender)))
        
        ;; Update student balance
        (map-set students tx-sender
            (merge student
                {
                    balance: (+ (get balance student) amount)
                }
            )
        )
        (ok amount)
    )
)

;; Book a mentorship session
(define-public (book-session
    (mentor principal)
    (session-type (string-ascii 50))
    (scheduled-time uint)
    (duration-hours uint))
    (let
        ((session-id (+ (var-get session-counter) u1))
         (mentor-data (unwrap! (map-get? mentors mentor) err-mentor-not-found))
         (student-data (unwrap! (map-get? students tx-sender) err-student-not-found))
         (session-fee (* (get hourly-rate mentor-data) duration-hours))
         (platform-fee (calculate-platform-fee session-fee)))
        
        (asserts! (get available mentor-data) err-mentor-not-available)
        (asserts! (> duration-hours u0) err-invalid-amount)
        (asserts! (>= (get balance student-data) session-fee) err-insufficient-balance)
        (asserts! (> scheduled-time stacks-block-height) err-invalid-amount)
        
        ;; Create session
        (map-set mentorship-sessions session-id
            {
                mentor: mentor,
                student: tx-sender,
                session-type: session-type,
                scheduled-time: scheduled-time,
                duration-hours: duration-hours,
                session-fee: session-fee,
                platform-fee: platform-fee,
                status: "scheduled",
                session-notes: "",
                completion-date: u0,
                student-rating: u0,
                mentor-rating: u0,
                paid: false
            }
        )
        
        ;; Deduct from student balance
        (map-set students tx-sender
            (merge student-data
                {
                    balance: (- (get balance student-data) session-fee)
                }
            )
        )
        
        (var-set session-counter session-id)
        (ok session-id)
    )
)

;; Mark session as completed (by mentor)
(define-public (complete-session (session-id uint) (session-notes (string-ascii 500)))
    (let
        ((session (unwrap! (map-get? mentorship-sessions session-id) err-session-not-found)))
        
        (asserts! (is-eq tx-sender (get mentor session)) err-not-authorized)
        (asserts! (is-eq (get status session) "scheduled") err-session-already-completed)
        
        (map-set mentorship-sessions session-id
            (merge session
                {
                    status: "completed",
                    session-notes: session-notes,
                    completion-date: stacks-block-height
                }
            )
        )
        (ok true)
    )
)

;; Student confirms session completion and releases payment
(define-public (confirm-and-pay-session (session-id uint))
    (let
        ((session (unwrap! (map-get? mentorship-sessions session-id) err-session-not-found))
         (mentor-data (unwrap! (map-get? mentors (get mentor session)) err-mentor-not-found))
         (student-data (unwrap! (map-get? students tx-sender) err-student-not-found))
         (mentor-payment (- (get session-fee session) (get platform-fee session))))
        
        (asserts! (is-eq tx-sender (get student session)) err-not-authorized)
        (asserts! (is-eq (get status session) "completed") err-session-not-confirmed)
        (asserts! (not (get paid session)) err-already-rated)
        
        ;; Transfer payment to mentor
        (try! (as-contract (stx-transfer? mentor-payment tx-sender (get mentor session))))
        
        ;; Mark session as paid
        (map-set mentorship-sessions session-id
            (merge session
                {
                    paid: true
                }
            )
        )
        
        ;; Update mentor stats
        (map-set mentors (get mentor session)
            (merge mentor-data
                {
                    total-sessions: (+ (get total-sessions mentor-data) u1),
                    total-earnings: (+ (get total-earnings mentor-data) mentor-payment)
                }
            )
        )
        
        ;; Update student stats
        (map-set students tx-sender
            (merge student-data
                {
                    total-sessions: (+ (get total-sessions student-data) u1)
                }
            )
        )
        
        (ok mentor-payment)
    )
)

;; Rate a mentor after session completion
(define-public (rate-mentor (session-id uint) (rating uint) (feedback (string-ascii 300)))
    (let
        ((session (unwrap! (map-get? mentorship-sessions session-id) err-session-not-found))
         (mentor-data (unwrap! (map-get? mentors (get mentor session)) err-mentor-not-found)))
        
        (asserts! (is-eq tx-sender (get student session)) err-not-authorized)
        (asserts! (is-eq (get status session) "completed") err-session-not-confirmed)
        (asserts! (and (>= rating u1) (<= rating u5)) err-invalid-rating)
        (asserts! (is-eq (get student-rating session) u0) err-already-rated)
        
        ;; Record feedback
        (map-set session-feedback {session-id: session-id, rater: tx-sender}
            {
                rating: rating,
                feedback: feedback,
                submitted-at: stacks-block-height
            }
        )
        
        ;; Update session with rating
        (map-set mentorship-sessions session-id
            (merge session
                {
                    student-rating: rating
                }
            )
        )
        
        ;; Update mentor's average rating
        (let
            ((total-ratings (get total-ratings mentor-data))
             (current-avg (get average-rating mentor-data))
             (new-total-ratings (+ total-ratings u1))
             (new-average (/ (+ (* current-avg total-ratings) rating) new-total-ratings)))
            
            (map-set mentors (get mentor session)
                (merge mentor-data
                    {
                        average-rating: new-average,
                        total-ratings: new-total-ratings
                    }
                )
            )
        )
        
        (ok true)
    )
)

;; Mentor can rate student after session
(define-public (rate-student (session-id uint) (rating uint))
    (let
        ((session (unwrap! (map-get? mentorship-sessions session-id) err-session-not-found)))
        
        (asserts! (is-eq tx-sender (get mentor session)) err-not-authorized)
        (asserts! (is-eq (get status session) "completed") err-session-not-confirmed)
        (asserts! (and (>= rating u1) (<= rating u5)) err-invalid-rating)
        (asserts! (is-eq (get mentor-rating session) u0) err-already-rated)
        
        (map-set mentorship-sessions session-id
            (merge session
                {
                    mentor-rating: rating
                }
            )
        )
        (ok true)
    )
)

;; Update mentor availability
(define-public (update-availability (available bool))
    (let
        ((mentor-data (unwrap! (map-get? mentors tx-sender) err-mentor-not-found)))
        
        (map-set mentors tx-sender
            (merge mentor-data
                {
                    available: available
                }
            )
        )
        (ok available)
    )
)

;; Owner can verify mentors
(define-public (verify-mentor (mentor principal))
    (let
        ((mentor-data (unwrap! (map-get? mentors mentor) err-mentor-not-found)))
        
        (asserts! (is-owner) err-not-authorized)
        
        (map-set mentors mentor
            (merge mentor-data
                {
                    verified: true
                }
            )
        )
        (ok true)
    )
)

;; Read-only Functions
(define-read-only (get-mentor-profile (mentor principal))
    (ok (map-get? mentors mentor))
)

(define-read-only (get-student-profile (student principal))
    (ok (map-get? students student))
)

(define-read-only (get-session (session-id uint))
    (ok (map-get? mentorship-sessions session-id))
)

(define-read-only (get-session-feedback (session-id uint) (rater principal))
    (ok (map-get? session-feedback {session-id: session-id, rater: rater}))
)

(define-read-only (get-platform-fee-rate)
    (ok (var-get platform-fee-rate))
)

(define-read-only (calculate-session-cost (mentor principal) (duration-hours uint))
    (match (map-get? mentors mentor)
        mentor-data (let
            ((base-cost (* (get hourly-rate mentor-data) duration-hours))
             (platform-fee (calculate-platform-fee base-cost)))
            (ok {total-cost: base-cost, platform-fee: platform-fee, mentor-payment: (- base-cost platform-fee)}))
        (err err-mentor-not-found)
    )
)

(define-read-only (get-contract-balance)
    (ok (stx-get-balance (as-contract tx-sender)))
)
