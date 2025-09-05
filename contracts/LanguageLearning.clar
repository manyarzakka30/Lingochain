;; description: Language Learning Progress Tracker for Lingochain DAO - track goals, streaks, and achievements

;; constants
(define-constant CONTRACT_OWNER tx-sender)
(define-constant ERR_UNAUTHORIZED (err u200))
(define-constant ERR_INVALID_INPUT (err u201))
(define-constant ERR_GOAL_NOT_FOUND (err u202))
(define-constant ERR_ALREADY_COMPLETED (err u203))
(define-constant ERR_PROFILE_UPDATE_FAILED (err u204))
(define-constant MIN_GOAL_DAYS u1)
(define-constant MAX_GOAL_DAYS u365)
(define-constant DAILY_PRACTICE_REWARD u5)
(define-constant GOAL_COMPLETION_REWARD u50)

;; data vars
(define-data-var goal-counter uint u0)
(define-data-var total-learners uint u0)

;; data maps
(define-map learning-goals uint {
    id: uint,
    learner: principal,
    language: (string-ascii 50),
    target-days: uint,
    days-completed: uint,
    description: (string-ascii 200),
    status: (string-ascii 20),
    created-block: uint
})

(define-map learner-profiles principal {
    total-goals: uint,
    completed-goals: uint,
    total-reward-points: uint,
    current-streak: uint,
    longest-streak: uint,
    last-practice-block: uint
})

(define-map daily-practice {learner: principal, practice-day: uint} {
    language: (string-ascii 50),
    minutes-practiced: uint,
    practice-block: uint
})

;; public functions
(define-public (create-learning-goal (language (string-ascii 50))
                                   (target-days uint)
                                   (description (string-ascii 200)))
    (let ((goal-id (+ (var-get goal-counter) u1))
          (learner tx-sender))
        (asserts! (and (>= target-days MIN_GOAL_DAYS) (<= target-days MAX_GOAL_DAYS)) ERR_INVALID_INPUT)
        (asserts! (> (len language) u0) ERR_INVALID_INPUT)
        (map-set learning-goals goal-id {
            id: goal-id,
            learner: learner,
            language: language,
            target-days: target-days,
            days-completed: u0,
            description: description,
            status: "active",
            created-block: stacks-block-height
        })
        (var-set goal-counter goal-id)
        (update-learner-profile learner "goal-created")
        (ok goal-id)
    )
)

(define-public (record-daily-practice (language (string-ascii 50))
                                    (minutes-practiced uint))
    (let ((learner tx-sender)
          (practice-day (/ stacks-block-height u144)))
        (asserts! (> (len language) u0) ERR_INVALID_INPUT)
        (asserts! (> minutes-practiced u0) ERR_INVALID_INPUT)
        (map-set daily-practice {learner: learner, practice-day: practice-day} {
            language: language,
            minutes-practiced: minutes-practiced,
            practice-block: stacks-block-height
        })
        (update-learning-streak learner)
        (award-practice-reward learner minutes-practiced)
        (ok true)
    )
)

(define-public (update-goal-progress (goal-id uint))
    (let ((goal (unwrap! (map-get? learning-goals goal-id) ERR_GOAL_NOT_FOUND))
          (learner tx-sender))
        (asserts! (is-eq learner (get learner goal)) ERR_UNAUTHORIZED)
        (asserts! (is-eq (get status goal) "active") ERR_ALREADY_COMPLETED)
        (map-set learning-goals goal-id 
            (merge goal {days-completed: (+ (get days-completed goal) u1)}))
        (if (>= (+ (get days-completed goal) u1) (get target-days goal))
            (begin
                (map-set learning-goals goal-id (merge goal {status: "completed"}))
                (update-learner-profile learner "goal-completed")
                (award-completion-reward learner (get target-days goal))
                (ok "goal-completed"))
            (ok "progress-updated"))
    )
)

;; read-only functions
(define-read-only (get-learning-goal (goal-id uint))
    (map-get? learning-goals goal-id)
)

(define-read-only (get-learner-profile (learner principal))
    (map-get? learner-profiles learner)
)

(define-read-only (get-daily-practice (learner principal) (practice-day uint))
    (map-get? daily-practice {learner: learner, practice-day: practice-day})
)

(define-read-only (get-learning-stats)
    {
        total-goals: (var-get goal-counter),
        total-learners: (var-get total-learners)
    }
)

;; private functions
(define-private (update-learner-profile (learner principal) (action (string-ascii 20)))
    (let ((current-profile (default-to {total-goals: u0, completed-goals: u0, 
                                      total-reward-points: u0, current-streak: u0,
                                      longest-streak: u0, last-practice-block: u0} 
                                      (map-get? learner-profiles learner))))
        (if (is-eq action "goal-created")
            (map-set learner-profiles learner (merge current-profile {total-goals: (+ (get total-goals current-profile) u1)}))
            (if (is-eq action "goal-completed")
                (map-set learner-profiles learner (merge current-profile {completed-goals: (+ (get completed-goals current-profile) u1)}))
                true))
    )
)

(define-private (update-learning-streak (learner principal))
    (let ((current-profile (default-to {total-goals: u0, completed-goals: u0, 
                                      total-reward-points: u0, current-streak: u0,
                                      longest-streak: u0, last-practice-block: u0} 
                                      (map-get? learner-profiles learner)))
          (blocks-since-last (- stacks-block-height (get last-practice-block current-profile))))
        (if (< blocks-since-last u288)  ;; Less than 2 days
            (let ((new-current (+ (get current-streak current-profile) u1))
                  (new-longest (if (> new-current (get longest-streak current-profile)) 
                                 new-current (get longest-streak current-profile))))
                (map-set learner-profiles learner (merge current-profile {
                    current-streak: new-current,
                    longest-streak: new-longest,
                    last-practice-block: stacks-block-height
                })))
            (map-set learner-profiles learner (merge current-profile {
                current-streak: u1,
                last-practice-block: stacks-block-height
            })))
    )
)

(define-private (award-practice-reward (learner principal) (minutes uint))
    (let ((reward-points (/ minutes u10))
          (current-profile (default-to {total-goals: u0, completed-goals: u0, 
                                      total-reward-points: u0, current-streak: u0,
                                      longest-streak: u0, last-practice-block: u0} 
                                      (map-get? learner-profiles learner))))
        (map-set learner-profiles learner 
            (merge current-profile {total-reward-points: (+ (get total-reward-points current-profile) reward-points)}))
    )
)

(define-private (award-completion-reward (learner principal) (goal-days uint))
    (let ((completion-bonus (* goal-days GOAL_COMPLETION_REWARD))
          (current-profile (default-to {total-goals: u0, completed-goals: u0, 
                                      total-reward-points: u0, current-streak: u0,
                                      longest-streak: u0, last-practice-block: u0} 
                                      (map-get? learner-profiles learner))))
        (map-set learner-profiles learner 
            (merge current-profile {total-reward-points: (+ (get total-reward-points current-profile) completion-bonus)}))
    )
)
