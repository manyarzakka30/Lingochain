
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
(define-constant REPUTATION_MULTIPLIER u10)
(define-constant HERITAGE_GUARDIAN_THRESHOLD u5000)
(define-constant LANGUAGE_CHAMPION_THRESHOLD u10000)
(define-constant PRESERVATION_PIONEER_THRESHOLD u25000)
(define-constant ERR_RESOURCE_NOT_FOUND (err u108))
(define-constant ERR_INSUFFICIENT_PRICE (err u109))
(define-constant ERR_ALREADY_PURCHASED (err u110))
(define-constant ERR_CANNOT_PURCHASE_OWN (err u111))
(define-constant ERR_RESOURCE_NOT_ACTIVE (err u112))
(define-constant MIN_RESOURCE_PRICE u10000)
(define-constant MAX_RATING u5)
(define-constant MARKETPLACE_FEE_PERCENTAGE u5)

;; data vars
(define-data-var proposal-counter uint u0)
(define-data-var total-members uint u0)
(define-data-var treasury-balance uint u0)
(define-data-var resource-counter uint u0)
(define-data-var marketplace-revenue uint u0)

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
(define-map member-reputation principal {
    score: uint,
    proposals-created: uint,
    votes-cast: uint,
    projects-registered: uint,
    total-contributions: uint,
    badges: (list 10 (string-ascii 30)),
    voting-power-multiplier: uint,
    last-activity-block: uint
})
(define-map reputation-leaderboard uint {
    rank: uint,
    member: principal,
    score: uint
})
(define-map activity-streaks principal {
    current-streak: uint,
    longest-streak: uint,
    last-activity-block: uint
})
(define-map marketplace-resources uint {
    id: uint,
    seller: principal,
    title: (string-ascii 100),
    description: (string-ascii 500),
    resource-type: (string-ascii 30),
    language: (string-ascii 50),
    price: uint,
    total-sales: uint,
    average-rating: uint,
    rating-count: uint,
    status: (string-ascii 20),
    created-block: uint,
    metadata-uri: (string-ascii 200)
})
(define-map resource-purchases {resource-id: uint, buyer: principal} {
    purchase-block: uint,
    price-paid: uint,
    access-granted: bool
})
(define-map resource-ratings {resource-id: uint, rater: principal} {
    rating: uint,
    review: (string-ascii 200),
    created-block: uint
})
(define-map seller-earnings principal {
    total-earned: uint,
    total-sales: uint,
    average-rating: uint,
    resources-listed: uint
})

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
        (let ((result (initialize-member-reputation sender contribution)))
            (ok true))
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
        (try! (update-reputation-for-proposal sender))
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
        (try! (update-reputation-for-vote sender))
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
        (try! (update-reputation-for-project tx-sender))
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
        (try! (update-reputation-for-contribution sender amount))
        (ok true)
    )
)

(define-public (list-resource (title (string-ascii 100))
                             (description (string-ascii 500))
                             (resource-type (string-ascii 30))
                             (language (string-ascii 50))
                             (price uint)
                             (metadata-uri (string-ascii 200)))
    (let ((resource-id (+ (var-get resource-counter) u1))
          (seller tx-sender))
        (asserts! (is-member seller) ERR_UNAUTHORIZED)
        (asserts! (>= price MIN_RESOURCE_PRICE) ERR_INVALID_AMOUNT)
        (map-set marketplace-resources resource-id {
            id: resource-id,
            seller: seller,
            title: title,
            description: description,
            resource-type: resource-type,
            language: language,
            price: price,
            total-sales: u0,
            average-rating: u0,
            rating-count: u0,
            status: "active",
            created-block: stacks-block-height,
            metadata-uri: metadata-uri
        })
        (var-set resource-counter resource-id)
        (let ((result (update-seller-listings seller)))
            (ok resource-id))
    )
)

(define-public (purchase-resource (resource-id uint))
    (let ((buyer tx-sender)
          (resource (unwrap! (map-get? marketplace-resources resource-id) ERR_RESOURCE_NOT_FOUND)))
        (asserts! (is-member buyer) ERR_UNAUTHORIZED)
        (asserts! (not (is-eq buyer (get seller resource))) ERR_CANNOT_PURCHASE_OWN)
        (asserts! (is-eq (get status resource) "active") ERR_RESOURCE_NOT_ACTIVE)
        (asserts! (is-none (map-get? resource-purchases {resource-id: resource-id, buyer: buyer})) ERR_ALREADY_PURCHASED)
        (let ((price (get price resource))
              (marketplace-fee (/ (* price MARKETPLACE_FEE_PERCENTAGE) u100))
              (seller-amount (- price marketplace-fee)))
            (try! (stx-transfer? price buyer (get seller resource)))
            (try! (stx-transfer? marketplace-fee buyer (as-contract tx-sender)))
            (map-set resource-purchases {resource-id: resource-id, buyer: buyer} {
                purchase-block: stacks-block-height,
                price-paid: price,
                access-granted: true
            })
            (map-set marketplace-resources resource-id 
                (merge resource {total-sales: (+ (get total-sales resource) u1)}))
            (var-set marketplace-revenue (+ (var-get marketplace-revenue) marketplace-fee))
            (let ((result (update-seller-earnings (get seller resource) seller-amount)))
                (ok true))
        )
    )
)

(define-public (rate-resource (resource-id uint) (rating uint) (review (string-ascii 200)))
    (let ((rater tx-sender)
          (resource (unwrap! (map-get? marketplace-resources resource-id) ERR_RESOURCE_NOT_FOUND)))
        (asserts! (is-member rater) ERR_UNAUTHORIZED)
        (asserts! (and (>= rating u1) (<= rating MAX_RATING)) ERR_INVALID_AMOUNT)
        (asserts! (is-some (map-get? resource-purchases {resource-id: resource-id, buyer: rater})) ERR_UNAUTHORIZED)
        (asserts! (is-none (map-get? resource-ratings {resource-id: resource-id, rater: rater})) ERR_ALREADY_VOTED)
        (map-set resource-ratings {resource-id: resource-id, rater: rater} {
            rating: rating,
            review: review,
            created-block: stacks-block-height
        })
        (let ((result (update-resource-rating resource-id rating)))
            (ok true))
    )
)

(define-public (update-resource-status (resource-id uint) (new-status (string-ascii 20)))
    (let ((resource (unwrap! (map-get? marketplace-resources resource-id) ERR_RESOURCE_NOT_FOUND)))
        (asserts! (is-eq tx-sender (get seller resource)) ERR_UNAUTHORIZED)
        (map-set marketplace-resources resource-id 
            (merge resource {status: new-status}))
        (ok true)
    )
)

(define-public (withdraw-marketplace-earnings)
    (let ((seller tx-sender)
          (earnings (default-to {total-earned: u0, total-sales: u0, average-rating: u0, resources-listed: u0} 
                                (map-get? seller-earnings seller))))
        (asserts! (is-member seller) ERR_UNAUTHORIZED)
        (asserts! (> (get total-earned earnings) u0) ERR_INSUFFICIENT_FUNDS)
        (try! (as-contract (stx-transfer? (get total-earned earnings) tx-sender seller)))
        (map-set seller-earnings seller 
            (merge earnings {total-earned: u0}))
        (ok (get total-earned earnings))
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

(define-public (award-badge (member principal) (badge-name (string-ascii 30)))
    (let ((current-rep (get-member-reputation member)))
        (asserts! (is-member tx-sender) ERR_UNAUTHORIZED)
        (match current-rep
            rep (let ((updated-badges (unwrap! (as-max-len? (append (get badges rep) badge-name) u10) ERR_INVALID_AMOUNT)))
                    (map-set member-reputation member 
                        (merge rep {badges: updated-badges}))
                    (ok true))
            ERR_UNAUTHORIZED
        )
    )
)

(define-public (calculate-weighted-vote (proposal-id uint) (vote-for bool))
    (let ((sender tx-sender)
          (proposal (unwrap! (map-get? proposals proposal-id) ERR_PROPOSAL_NOT_FOUND))
          (member-rep (get-member-reputation sender)))
        (asserts! (is-member sender) ERR_UNAUTHORIZED)
        (asserts! (is-none (map-get? votes {proposal-id: proposal-id, voter: sender})) ERR_ALREADY_VOTED)
        (asserts! (< stacks-block-height (+ (get start-block proposal) VOTING_PERIOD)) ERR_VOTING_ENDED)
        (map-set votes {proposal-id: proposal-id, voter: sender} vote-for)
        (match member-rep
            rep (let ((vote-weight (get voting-power-multiplier rep)))
                    (if vote-for
                        (map-set proposals proposal-id 
                            (merge proposal {votes-for: (+ (get votes-for proposal) vote-weight)}))
                        (map-set proposals proposal-id 
                            (merge proposal {votes-against: (+ (get votes-against proposal) vote-weight)}))
                    )
                    (try! (update-reputation-for-vote sender))
                    (ok vote-weight))
            (begin
                (try! (update-reputation-for-vote sender))
                (ok u1))
        )
    )
)

(define-public (update-leaderboard)
    (let ((members-list (get-top-members u10)))
        (fold update-leaderboard-entry members-list (ok u0))
    )
)

(define-read-only (get-member-reputation (member principal))
    (map-get? member-reputation member)
)

(define-read-only (get-member-badges (member principal))
    (match (map-get? member-reputation member)
        rep (get badges rep)
        (list)
    )
)

(define-read-only (get-voting-power (member principal))
    (match (map-get? member-reputation member)
        rep (get voting-power-multiplier rep)
        u1
    )
)

(define-read-only (get-leaderboard-position (rank uint))
    (map-get? reputation-leaderboard rank)
)

(define-read-only (get-activity-streak (member principal))
    (map-get? activity-streaks member)
)

(define-read-only (calculate-reputation-score (member principal))
    (match (map-get? member-reputation member)
        rep (+ 
            (* (get proposals-created rep) u100)
            (* (get votes-cast rep) u10)
            (* (get projects-registered rep) u200)
            (/ (get total-contributions rep) u100000)
        )
        u0
    )
)

(define-read-only (get-top-members (limit uint))
    (list {member: 'SP000000000000000000002Q6VF78, score: u0})
)

(define-read-only (has-badge (member principal) (badge-name (string-ascii 30)))
    (match (map-get? member-reputation member)
        rep (is-some (index-of (get badges rep) badge-name))
        false
    )
)

(define-read-only (get-marketplace-resource (resource-id uint))
    (map-get? marketplace-resources resource-id)
)

(define-read-only (get-resource-purchase (resource-id uint) (buyer principal))
    (map-get? resource-purchases {resource-id: resource-id, buyer: buyer})
)

(define-read-only (get-resource-rating (resource-id uint) (rater principal))
    (map-get? resource-ratings {resource-id: resource-id, rater: rater})
)

(define-read-only (get-seller-earnings (seller principal))
    (map-get? seller-earnings seller)
)

(define-read-only (get-marketplace-stats)
    {
        total-resources: (var-get resource-counter),
        marketplace-revenue: (var-get marketplace-revenue)
    }
)

(define-read-only (has-purchased-resource (resource-id uint) (buyer principal))
    (is-some (map-get? resource-purchases {resource-id: resource-id, buyer: buyer}))
)

;; private functions
(define-private (initialize-member-reputation (member principal) (initial-contribution uint))
    (let ((initial-score (/ initial-contribution u100000))
          (voting-power (if (>= initial-contribution u1000000) u2 u1))
          (initial-badges (if (>= initial-contribution u5000000) 
                            (list "founding-member") 
                            (list))))
        (map-set member-reputation member {
            score: initial-score,
            proposals-created: u0,
            votes-cast: u0,
            projects-registered: u0,
            total-contributions: initial-contribution,
            badges: initial-badges,
            voting-power-multiplier: voting-power,
            last-activity-block: stacks-block-height
        })
        (map-set activity-streaks member {
            current-streak: u1,
            longest-streak: u1,
            last-activity-block: stacks-block-height
        })
        (ok true)
    )
)

(define-private (update-reputation-for-proposal (member principal))
    (match (map-get? member-reputation member)
        rep (let ((new-score (+ (get score rep) u100))
                  (new-proposals (+ (get proposals-created rep) u1))
                  (updated-badges (check-and-award-badges member new-score (get badges rep))))
                (map-set member-reputation member 
                    (merge rep {
                        score: new-score,
                        proposals-created: new-proposals,
                        badges: updated-badges,
                        last-activity-block: stacks-block-height
                    }))
                (let ((result (update-activity-streak member))) true)
                (ok true))
        ERR_UNAUTHORIZED
    )
)

(define-private (update-reputation-for-vote (member principal))
    (match (map-get? member-reputation member)
        rep (let ((new-score (+ (get score rep) u10))
                  (new-votes (+ (get votes-cast rep) u1))
                  (updated-badges (check-and-award-badges member new-score (get badges rep))))
                (map-set member-reputation member 
                    (merge rep {
                        score: new-score,
                        votes-cast: new-votes,
                        badges: updated-badges,
                        last-activity-block: stacks-block-height
                    }))
                (let ((result (update-activity-streak member))) true)
                (ok true))
        ERR_UNAUTHORIZED
    )
)

(define-private (update-reputation-for-project (member principal))
    (match (map-get? member-reputation member)
        rep (let ((new-score (+ (get score rep) u200))
                  (new-projects (+ (get projects-registered rep) u1))
                  (updated-badges (check-and-award-badges member new-score (get badges rep))))
                (map-set member-reputation member 
                    (merge rep {
                        score: new-score,
                        projects-registered: new-projects,
                        badges: updated-badges,
                        last-activity-block: stacks-block-height
                    }))
                (let ((result (update-activity-streak member))) true)
                (ok true))
        ERR_UNAUTHORIZED
    )
)

(define-private (update-reputation-for-contribution (member principal) (amount uint))
    (match (map-get? member-reputation member)
        rep (let ((contribution-score (/ amount u100000))
                  (new-score (+ (get score rep) contribution-score))
                  (new-total (+ (get total-contributions rep) amount))
                  (updated-badges (check-and-award-badges member new-score (get badges rep)))
                  (new-voting-power (calculate-voting-power new-total)))
                (map-set member-reputation member 
                    (merge rep {
                        score: new-score,
                        total-contributions: new-total,
                        badges: updated-badges,
                        voting-power-multiplier: new-voting-power,
                        last-activity-block: stacks-block-height
                    }))
                (let ((result (update-activity-streak member))) true)
                (ok true))
        ERR_UNAUTHORIZED
    )
)

(define-private (check-and-award-badges (member principal) (score uint) (current-badges (list 10 (string-ascii 30))))
    (let ((heritage-badge "heritage-guardian")
          (champion-badge "language-champion")
          (pioneer-badge "preservation-pioneer"))
        (if (and (>= score PRESERVATION_PIONEER_THRESHOLD) 
                 (is-none (index-of current-badges pioneer-badge)))
            (unwrap-panic (as-max-len? (append current-badges pioneer-badge) u10))
            (if (and (>= score LANGUAGE_CHAMPION_THRESHOLD) 
                     (is-none (index-of current-badges champion-badge)))
                (unwrap-panic (as-max-len? (append current-badges champion-badge) u10))
                (if (and (>= score HERITAGE_GUARDIAN_THRESHOLD) 
                         (is-none (index-of current-badges heritage-badge)))
                    (unwrap-panic (as-max-len? (append current-badges heritage-badge) u10))
                    current-badges
                )
            )
        )
    )
)

(define-private (calculate-voting-power (total-contributions uint))
    (if (>= total-contributions u10000000) u4
        (if (>= total-contributions u5000000) u3
            (if (>= total-contributions u1000000) u2
                u1
            )
        )
    )
)

(define-private (update-activity-streak (member principal))
    (match (map-get? activity-streaks member)
        streak (let ((blocks-since-last (- stacks-block-height (get last-activity-block streak)))
                     (is-consecutive (< blocks-since-last u144)))
                    (if is-consecutive
                        (let ((new-current (+ (get current-streak streak) u1))
                              (new-longest (if (> new-current (get longest-streak streak)) new-current (get longest-streak streak))))
                            (map-set activity-streaks member {
                                current-streak: new-current,
                                longest-streak: new-longest,
                                last-activity-block: stacks-block-height
                            })
                            (ok true))
                        (begin
                            (map-set activity-streaks member {
                                current-streak: u1,
                                longest-streak: (get longest-streak streak),
                                last-activity-block: stacks-block-height
                            })
                            (ok true))
                    ))
        (begin
            (map-set activity-streaks member {
                current-streak: u1,
                longest-streak: u1,
                last-activity-block: stacks-block-height
            })
            (ok true))
    )
)

(define-private (update-leaderboard-entry (member-entry {member: principal, score: uint}) (result (response uint uint)))
    result
)

(define-private (update-seller-listings (seller principal))
    (let ((current-earnings (default-to {total-earned: u0, total-sales: u0, average-rating: u0, resources-listed: u0} 
                                       (map-get? seller-earnings seller))))
        (map-set seller-earnings seller 
            (merge current-earnings {resources-listed: (+ (get resources-listed current-earnings) u1)}))
        (ok true)
    )
)

(define-private (update-seller-earnings (seller principal) (amount uint))
    (let ((current-earnings (default-to {total-earned: u0, total-sales: u0, average-rating: u0, resources-listed: u0} 
                                       (map-get? seller-earnings seller))))
        (map-set seller-earnings seller 
            (merge current-earnings {
                total-earned: (+ (get total-earned current-earnings) amount),
                total-sales: (+ (get total-sales current-earnings) u1)
            }))
        (ok true)
    )
)

(define-private (update-resource-rating (resource-id uint) (new-rating uint))
    (let ((resource (unwrap! (map-get? marketplace-resources resource-id) ERR_RESOURCE_NOT_FOUND)))
        (let ((current-total (+ (* (get average-rating resource) (get rating-count resource)) new-rating))
              (new-count (+ (get rating-count resource) u1)))
            (map-set marketplace-resources resource-id 
                (merge resource {
                    average-rating: (/ current-total new-count),
                    rating-count: new-count
                }))
            (ok true)
        )
    )
)