;; TalentBridge: Decentralized freelance marketplace platform
;; Enables clients to post projects, freelancers to submit bids, and mediators to resolve disputes

(define-data-var platform-mediator principal tx-sender)

(define-map project-board
  { project-id: uint }
  {
    client: principal,
    budget: uint,
    project-title: (string-ascii 50),
    requirements: (string-ascii 500),
    deadline-days: uint,
    completed: bool
  }
)

(define-map engagement-log
  { project-id: uint, log-id: uint }
  {
    freelancer: principal,
    bid-date: uint,
    proposal: (string-ascii 20)
  }
)

(define-data-var next-project-id uint u1)

(define-map log-tracker
  { project-id: uint }
  { total-logs: uint }
)

;; Post new freelance project
(define-public (post-project (title-input (string-ascii 50)) (requirements-input (string-ascii 500)) (deadline-input uint) (budget-input uint))
  (let
    (
      (project-id (var-get next-project-id))
      (log-id u0)
      (title title-input)
      (requirements requirements-input)
      (deadline deadline-input)
      (budget budget-input)
    )
    ;; Input validation
    (asserts! (> budget u0) (err u1))
    (asserts! (> (len title) u0) (err u5))
    (asserts! (> (len requirements) u0) (err u6))
    (asserts! (> deadline u0) (err u7))
    
    (map-set project-board
      { project-id: project-id }
      {
        client: tx-sender,
        budget: budget,
        project-title: title,
        requirements: requirements,
        deadline-days: deadline,
        completed: false
      }
    )
    (map-set engagement-log
      { project-id: project-id, log-id: log-id }
      {
        freelancer: tx-sender,
        bid-date: project-id,
        proposal: "posted"
      }
    )
    (map-set log-tracker
      { project-id: project-id }
      { total-logs: u1 }
    )
    (var-set next-project-id (+ project-id u1))
    (ok project-id)
  )
)

;; Submit bid for project
(define-public (submit-bid (project-id-input uint))
  (let
    (
      (project-id project-id-input)
      (project-info (unwrap! (map-get? project-board { project-id: project-id }) (err u2)))
      (budget (get budget project-info))
      (client (get client project-info))
      (log-data (default-to { total-logs: u0 } (map-get? log-tracker { project-id: project-id })))
      (log-id (get total-logs log-data))
      (new-log-id (+ log-id u1))
    )
    ;; Input validation
    (asserts! (> project-id u0) (err u8))
    (asserts! (not (is-eq tx-sender client)) (err u3))
    
    (try! (stx-transfer? budget tx-sender client))
    (map-set engagement-log
      { project-id: project-id, log-id: log-id }
      {
        freelancer: tx-sender,
        bid-date: (var-get next-project-id),
        proposal: "submitted"
      }
    )
    (map-set log-tracker
      { project-id: project-id }
      { total-logs: new-log-id }
    )
    (ok true)
  )
)

;; Complete project (mediator only)
(define-public (complete-project (project-id-input uint))
  (let
    (
      (project-id project-id-input)
      (project-info (unwrap! (map-get? project-board { project-id: project-id }) (err u2)))
      (log-data (default-to { total-logs: u0 } (map-get? log-tracker { project-id: project-id })))
      (log-id (get total-logs log-data))
      (new-log-id (+ log-id u1))
    )
    ;; Input validation
    (asserts! (> project-id u0) (err u8))
    (asserts! (is-eq tx-sender (var-get platform-mediator)) (err u4))
    
    (map-set project-board
      { project-id: project-id }
      (merge project-info { completed: true })
    )
    (map-set engagement-log
      { project-id: project-id, log-id: log-id }
      {
        freelancer: (get client project-info),
        bid-date: (var-get next-project-id),
        proposal: "completed"
      }
    )
    (map-set log-tracker
      { project-id: project-id }
      { total-logs: new-log-id }
    )
    (ok true)
  )
)

;; Get project details
(define-read-only (get-project (project-id uint))
  (map-get? project-board { project-id: project-id })
)

;; Get engagement log entry
(define-read-only (get-engagement-log (project-id uint) (log-id uint))
  (map-get? engagement-log { project-id: project-id, log-id: log-id })
)

;; Get total engagement logs
(define-read-only (get-engagement-count (project-id uint))
  (let
    (
      (log-data (default-to { total-logs: u0 } (map-get? log-tracker { project-id: project-id })))
    )
    (get total-logs log-data)
  )
)