
;; description: A DAO for funding language preservation efforts including speakers, dictionaries, and recordings


;; constants
(define-constant CONTRACT_OWNER tx-sender)
(define-constant ERR_UNAUTHORIZED (err u100))
(define-constant ERR_INVALID_AMOUNT (err u101))
(define-constant ERR_PROPOSAL_NOT_FOUND (err u102))
(define-constant ERR_ALREADY_VOTED (err u103))
(define-constant ERR_VOTING_ENDED (err u104))
(define-constant ERR_INSUFFICIENT_FUNDS (err u105))
(define-constant ERR_PROPOSAL_NOT_PASSED (err u106))
(define-constant ERR_ALREADY_EXECUTED (err u107))
(define-constant VOTING_PERIOD u144)
(define-constant MIN_PROPOSAL_AMOUNT u1000000)
(define-constant QUORUM_THRESHOLD u51)

;; data vars
(define-data-var proposal-counter uint u0)
(define-data-var total-members uint u0)
(define-data-var treasury-balance uint u0)

;; data maps
(define-map members principal bool)
(define-map member-contributions principal uint)
(define-map proposals uint {
    id: uint,
    proposer: principal,
    title: (string-ascii 100),
    description: (string-ascii 500),
    amount: uint,
    recipient: principal,
    votes-for: uint,
    votes-against: uint,
    start-block: uint,
    executed: bool,
    category: (string-ascii 20)
})
(define-map votes {proposal-id: uint, voter: principal} bool)
(define-map language-projects uint {
    id: uint,
    language-name: (string-ascii 50),
    region: (string-ascii 50),
    speakers-count: uint,
    project-type: (string-ascii 20),
    funding-received: uint,
    status: (string-ascii 20)
})
(define-map project-counter uint uint)

;; public functions
(define-public (join-dao (contribution uint))
    (let ((sender tx-sender))
        (asserts! (> contribution u0) ERR_INVALID_AMOUNT)
        (try! (stx-transfer? contribution sender (as-contract tx-sender)))
        (map-set members sender true)
        (map-set member-contributions sender 
            (+ (default-to u0 (map-get? member-contributions sender)) contribution))
        (var-set treasury-balance (+ (var-get treasury-balance) contribution))
        (var-set total-members (+ (var-get total-members) u1))
        (ok true)
    )
)

(define-public (create-proposal (title (string-ascii 100)) 
                               (description (string-ascii 500))
                               (amount uint)
                               (recipient principal)
                               (category (string-ascii 20)))
    (let ((proposal-id (+ (var-get proposal-counter) u1))
          (sender tx-sender))
        (asserts! (is-member sender) ERR_UNAUTHORIZED)
        (asserts! (>= amount MIN_PROPOSAL_AMOUNT) ERR_INVALID_AMOUNT)
        (asserts! (<= amount (var-get treasury-balance)) ERR_INSUFFICIENT_FUNDS)
        (map-set proposals proposal-id {
            id: proposal-id,
            proposer: sender,
            title: title,
            description: description,
            amount: amount,
            recipient: recipient,
            votes-for: u0,
            votes-against: u0,
            start-block: stacks-block-height,
            executed: false,
            category: category
        })
        (var-set proposal-counter proposal-id)
        (ok proposal-id)
    )
)

(define-public (vote-on-proposal (proposal-id uint) (vote-for bool))
    (let ((sender tx-sender)
          (proposal (unwrap! (map-get? proposals proposal-id) ERR_PROPOSAL_NOT_FOUND)))
        (asserts! (is-member sender) ERR_UNAUTHORIZED)
        (asserts! (is-none (map-get? votes {proposal-id: proposal-id, voter: sender})) ERR_ALREADY_VOTED)
        (asserts! (< stacks-block-height (+ (get start-block proposal) VOTING_PERIOD)) ERR_VOTING_ENDED)
        (map-set votes {proposal-id: proposal-id, voter: sender} vote-for)
        (if vote-for
            (map-set proposals proposal-id 
                (merge proposal {votes-for: (+ (get votes-for proposal) u1)}))
            (map-set proposals proposal-id 
                (merge proposal {votes-against: (+ (get votes-against proposal) u1)}))
        )
        (ok true)
    )
)

(define-public (execute-proposal (proposal-id uint))
    (let ((proposal (unwrap! (map-get? proposals proposal-id) ERR_PROPOSAL_NOT_FOUND)))
        (asserts! (>= stacks-block-height (+ (get start-block proposal) VOTING_PERIOD)) ERR_VOTING_ENDED)
        (asserts! (not (get executed proposal)) ERR_ALREADY_EXECUTED)
        (asserts! (proposal-passed? proposal-id) ERR_PROPOSAL_NOT_PASSED)
        (try! (as-contract (stx-transfer? (get amount proposal) tx-sender (get recipient proposal))))
        (var-set treasury-balance (- (var-get treasury-balance) (get amount proposal)))
        (map-set proposals proposal-id (merge proposal {executed: true}))
        (ok true)
    )
)

(define-public (register-language-project (language-name (string-ascii 50))
                                         (region (string-ascii 50))
                                         (speakers-count uint)
                                         (project-type (string-ascii 20)))
    (let ((project-id (+ (default-to u0 (map-get? project-counter u0)) u1)))
        (asserts! (is-member tx-sender) ERR_UNAUTHORIZED)
        (map-set language-projects project-id {
            id: project-id,
            language-name: language-name,
            region: region,
            speakers-count: speakers-count,
            project-type: project-type,
            funding-received: u0,
            status: "pending"
        })
        (map-set project-counter u0 project-id)
        (ok project-id)
    )
)

(define-public (update-project-funding (project-id uint) (amount uint))
    (let ((project (unwrap! (map-get? language-projects project-id) ERR_PROPOSAL_NOT_FOUND)))
        (asserts! (is-member tx-sender) ERR_UNAUTHORIZED)
        (map-set language-projects project-id 
            (merge project {
                funding-received: (+ (get funding-received project) amount),
                status: "funded"
            }))
        (ok true)
    )
)

(define-public (contribute-to-treasury (amount uint))
    (let ((sender tx-sender))
        (asserts! (> amount u0) ERR_INVALID_AMOUNT)
        (try! (stx-transfer? amount sender (as-contract tx-sender)))
        (var-set treasury-balance (+ (var-get treasury-balance) amount))
        (map-set member-contributions sender 
            (+ (default-to u0 (map-get? member-contributions sender)) amount))
        (ok true)
    )
)

;; read only functions
(define-read-only (get-proposal (proposal-id uint))
    (map-get? proposals proposal-id)
)

(define-read-only (get-treasury-balance)
    (var-get treasury-balance)
)

(define-read-only (get-total-members)
    (var-get total-members)
)

(define-read-only (is-member (user principal))
    (default-to false (map-get? members user))
)

(define-read-only (get-member-contribution (user principal))
    (default-to u0 (map-get? member-contributions user))
)

(define-read-only (has-voted (proposal-id uint) (voter principal))
    (is-some (map-get? votes {proposal-id: proposal-id, voter: voter}))
)

(define-read-only (get-vote (proposal-id uint) (voter principal))
    (map-get? votes {proposal-id: proposal-id, voter: voter})
)

(define-read-only (proposal-passed? (proposal-id uint))
    (match (map-get? proposals proposal-id)
        proposal (let ((total-votes (+ (get votes-for proposal) (get votes-against proposal)))
                      (approval-rate (if (> total-votes u0) 
                                       (* (/ (get votes-for proposal) total-votes) u100) 
                                       u0)))
                   (and (>= approval-rate QUORUM_THRESHOLD)
                        (>= total-votes (/ (var-get total-members) u3))))
        false
    )
)

(define-read-only (get-language-project (project-id uint))
    (map-get? language-projects project-id)
)

(define-read-only (get-proposal-counter)
    (var-get proposal-counter)
)

(define-read-only (get-project-counter)
    (default-to u0 (map-get? project-counter u0))
)

(define-read-only (is-voting-active (proposal-id uint))
    (match (map-get? proposals proposal-id)
        proposal (< stacks-block-height (+ (get start-block proposal) VOTING_PERIOD))
        false
    )
)

;; private functions