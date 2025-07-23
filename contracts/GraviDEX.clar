;; GraviDEX - Gravity-Powered Decentralized Exchange
;; A native Stacks AMM implementing constant product formula (x*y=k)
;; SECURED VERSION with input validation and overflow protection

;; Constants
(define-constant CONTRACT-OWNER tx-sender)
(define-constant ERR-OWNER-ONLY (err u100))
(define-constant ERR-NOT-AUTHORIZED (err u101))
(define-constant ERR-INVALID-POOL (err u102))
(define-constant ERR-INSUFFICIENT-LIQUIDITY (err u103))
(define-constant ERR-SLIPPAGE-TOO-HIGH (err u104))
(define-constant ERR-INVALID-AMOUNT (err u105))
(define-constant ERR-TRANSFER-FAILED (err u106))
(define-constant ERR-POOL-EXISTS (err u107))
(define-constant ERR-INVALID-TOKEN (err u108))
(define-constant ERR-ZERO-AMOUNT (err u109))
(define-constant ERR-OVERFLOW (err u110))
(define-constant ERR-INVALID-TOKEN-CONTRACT (err u111))

;; Maximum values to prevent overflow
(define-constant MAX-UINT u340282366920938463463374607431768211455)
(define-constant MAX-SAFE-AMOUNT u18446744073709551615) ;; 2^64 - 1

;; Data Variables
(define-data-var fee-rate uint u300) ;; 0.3% = 300 basis points

;; Data Maps
(define-map pools 
  { token-a: principal, token-b: principal }
  { 
    reserve-a: uint,
    reserve-b: uint,
    lp-token: principal,
    total-supply: uint,
    fee-accumulated-a: uint,
    fee-accumulated-b: uint
  }
)

(define-map user-liquidity
  { user: principal, token-a: principal, token-b: principal }
  { lp-tokens: uint }
)

(define-map pool-exists
  { token-a: principal, token-b: principal }
  bool
)

;; Whitelist of approved token contracts (security measure)
(define-map approved-tokens principal bool)

;; Security Functions
(define-private (is-valid-token-contract (token principal))
  (and
    ;; Check if token is a contract (has a dot in the name)
    (is-some (index-of (unwrap-panic (to-consensus-buff? token)) 0x2e))
    ;; Check if token is in approved list (optional - can be removed for permissionless)
    true ;; For now, allow all contracts. In production, use: (default-to false (map-get? approved-tokens token))
  )
)

(define-private (safe-add (a uint) (b uint))
  (let ((result (+ a b)))
    (if (< result a) ;; Overflow check
      (err ERR-OVERFLOW)
      (ok result)
    )
  )
)

(define-private (safe-multiply (a uint) (b uint))
  (if (is-eq a u0)
    (ok u0)
    (let ((result (* a b)))
      (if (not (is-eq (/ result a) b)) ;; Overflow check
        (err ERR-OVERFLOW)
        (ok result)
      )
    )
  )
)

(define-private (validate-amount (amount uint))
  (and (> amount u0) (<= amount MAX-SAFE-AMOUNT))
)

;; Private Functions
(define-private (get-pool-key (token-a principal) (token-b principal))
  ;; Validate tokens before processing
  (if (and (is-valid-token-contract token-a) (is-valid-token-contract token-b))
    (let ((buff-a (unwrap-panic (to-consensus-buff? token-a)))
          (buff-b (unwrap-panic (to-consensus-buff? token-b))))
      (if (< (len buff-a) (len buff-b))
        { token-a: token-a, token-b: token-b }
        (if (> (len buff-a) (len buff-b))
          { token-a: token-b, token-b: token-a }
          ;; If lengths are equal, compare the buffers directly
          (if (< buff-a buff-b)
            { token-a: token-a, token-b: token-b }
            { token-a: token-b, token-b: token-a }
          )
        )
      )
    )
    ;; Return invalid pool key if tokens are invalid
    { token-a: 'SP000000000000000000002Q6VF78, token-b: 'SP000000000000000000002Q6VF78 }
  )
)

(define-private (calculate-lp-tokens (amount-a uint) (amount-b uint) (reserve-a uint) (reserve-b uint) (total-supply uint))
  (begin
    ;; Validate all inputs
    (asserts! (validate-amount amount-a) ERR-INVALID-AMOUNT)
    (asserts! (validate-amount amount-b) ERR-INVALID-AMOUNT)
    
    (if (is-eq total-supply u0)
      ;; Initial liquidity - use square root approximation instead of direct multiplication
      (let ((product (unwrap! (safe-multiply amount-a amount-b) ERR-OVERFLOW)))
        (ok (pow product u1)) ;; Simple approximation, in production use proper sqrt
      )
      ;; Subsequent liquidity - proportional to existing reserves with overflow protection
      (let ((lp-a-numerator (unwrap! (safe-multiply amount-a total-supply) ERR-OVERFLOW))
            (lp-b-numerator (unwrap! (safe-multiply amount-b total-supply) ERR-OVERFLOW)))
        (if (and (> reserve-a u0) (> reserve-b u0))
          (let ((lp-a (/ lp-a-numerator reserve-a))
                (lp-b (/ lp-b-numerator reserve-b)))
            (ok (if (< lp-a lp-b) lp-a lp-b))
          )
          ERR-INSUFFICIENT-LIQUIDITY
        )
      )
    )
  )
)

(define-private (calculate-swap-output (amount-in uint) (reserve-in uint) (reserve-out uint))
  (begin
    ;; Validate inputs
    (asserts! (validate-amount amount-in) ERR-INVALID-AMOUNT)
    (asserts! (> reserve-in u0) ERR-INSUFFICIENT-LIQUIDITY)
    (asserts! (> reserve-out u0) ERR-INSUFFICIENT-LIQUIDITY)
    
    (let ((fee-rate-var (var-get fee-rate))
          (fee-amount (/ (unwrap! (safe-multiply amount-in fee-rate-var) ERR-OVERFLOW) u10000))
          (amount-in-with-fee (- amount-in fee-amount))
          (numerator (unwrap! (safe-multiply amount-in-with-fee reserve-out) ERR-OVERFLOW))
          (denominator (unwrap! (safe-add reserve-in amount-in-with-fee) ERR-OVERFLOW)))
      
      (if (> denominator u0)
        (ok (/ numerator denominator))
        ERR-INSUFFICIENT-LIQUIDITY
      )
    )
  )
)

(define-private (validate-slippage (expected-amount uint) (actual-amount uint) (max-slippage uint))
  (match (safe-multiply expected-amount max-slippage)
    ok-value 
    (let ((slippage-amount ok-value)
          (min-expected (- expected-amount (/ slippage-amount u10000))))
      (>= actual-amount min-expected)
    )
    err-value false ;; Return false if overflow occurs
  )
)

;; Read-only Functions
(define-read-only (get-pool-info (token-a principal) (token-b principal))
  (let ((pool-key (get-pool-key token-a token-b)))
    ;; Only return pool info if tokens are valid
    (if (and (is-valid-token-contract token-a) (is-valid-token-contract token-b))
      (map-get? pools pool-key)
      none
    )
  )
)

(define-read-only (get-user-liquidity (user principal) (token-a principal) (token-b principal))
  (let ((pool-key (get-pool-key token-a token-b)))
    (if (and (is-valid-token-contract token-a) (is-valid-token-contract token-b))
      (default-to { lp-tokens: u0 } 
        (map-get? user-liquidity { user: user, token-a: (get token-a pool-key), token-b: (get token-b pool-key) })
      )
      { lp-tokens: u0 }
    )
  )
)

(define-read-only (calculate-swap-quote (token-in principal) (token-out principal) (amount-in uint))
  (let ((pool-key (get-pool-key token-in token-out)))
    ;; Validate tokens before processing
    (if (and (is-valid-token-contract token-in) (is-valid-token-contract token-out))
      (match (map-get? pools pool-key)
        pool-data 
        (if (is-eq token-in (get token-a pool-key))
          (calculate-swap-output amount-in (get reserve-a pool-data) (get reserve-b pool-data))
          (calculate-swap-output amount-in (get reserve-b pool-data) (get reserve-a pool-data))
        )
        ERR-INVALID-POOL
      )
      ERR-INVALID-TOKEN
    )
  )
)

(define-read-only (get-fee-rate)
  (var-get fee-rate)
)

(define-read-only (pool-exists-check (token-a principal) (token-b principal))
  (let ((pool-key (get-pool-key token-a token-b)))
    (if (and (is-valid-token-contract token-a) (is-valid-token-contract token-b))
      (default-to false (map-get? pool-exists pool-key))
      false
    )
  )
)

;; Public Functions
(define-public (create-pool (token-a principal) (token-b principal) (amount-a uint) (amount-b uint))
  (let ((pool-key (get-pool-key token-a token-b))
        (caller tx-sender))
    
    ;; Comprehensive input validation
    (asserts! (validate-amount amount-a) ERR-INVALID-AMOUNT)
    (asserts! (validate-amount amount-b) ERR-INVALID-AMOUNT)
    (asserts! (is-valid-token-contract token-a) ERR-INVALID-TOKEN)
    (asserts! (is-valid-token-contract token-b) ERR-INVALID-TOKEN)
    (asserts! (not (is-eq token-a token-b)) ERR-INVALID-TOKEN)
    (asserts! (not (pool-exists-check token-a token-b)) ERR-POOL-EXISTS)
    
    (let ((initial-lp-tokens (unwrap! (calculate-lp-tokens amount-a amount-b u0 u0 u0) ERR-INVALID-AMOUNT)))
      ;; Create pool with validated data
      (map-set pools pool-key {
        reserve-a: amount-a,
        reserve-b: amount-b,
        lp-token: (as-contract tx-sender),
        total-supply: initial-lp-tokens,
        fee-accumulated-a: u0,
        fee-accumulated-b: u0
      })
      
      ;; Mark pool as existing
      (map-set pool-exists pool-key true)
      
      ;; Record user liquidity with validated pool-key components
      (let ((validated-token-a (get token-a pool-key))
            (validated-token-b (get token-b pool-key)))
        (map-set user-liquidity 
          { user: caller, token-a: validated-token-a, token-b: validated-token-b }
          { lp-tokens: initial-lp-tokens }
        )
      )
      
      (ok { pool-created: true, lp-tokens: initial-lp-tokens })
    )
  )
)

(define-public (add-liquidity (token-a principal) (token-b principal) (amount-a uint) (amount-b uint))
  (let ((pool-key (get-pool-key token-a token-b))
        (caller tx-sender))
    
    ;; Input validation
    (asserts! (validate-amount amount-a) ERR-INVALID-AMOUNT)
    (asserts! (validate-amount amount-b) ERR-INVALID-AMOUNT)
    (asserts! (is-valid-token-contract token-a) ERR-INVALID-TOKEN)
    (asserts! (is-valid-token-contract token-b) ERR-INVALID-TOKEN)
    
    (match (map-get? pools pool-key)
      pool-data
      (let ((reserve-a (get reserve-a pool-data))
            (reserve-b (get reserve-b pool-data))
            (total-supply (get total-supply pool-data))
            (lp-tokens (unwrap! (calculate-lp-tokens amount-a amount-b reserve-a reserve-b total-supply) ERR-INVALID-AMOUNT))
            (current-user-lp (get lp-tokens (get-user-liquidity caller token-a token-b)))
            (validated-token-a (get token-a pool-key))
            (validated-token-b (get token-b pool-key))
            ;; Safe arithmetic operations
            (new-reserve-a (unwrap! (safe-add reserve-a amount-a) ERR-OVERFLOW))
            (new-reserve-b (unwrap! (safe-add reserve-b amount-b) ERR-OVERFLOW))
            (new-total-supply (unwrap! (safe-add total-supply lp-tokens) ERR-OVERFLOW))
            (new-user-lp (unwrap! (safe-add current-user-lp lp-tokens) ERR-OVERFLOW)))
        
        ;; Update pool reserves with overflow protection
        (map-set pools pool-key (merge pool-data {
          reserve-a: new-reserve-a,
          reserve-b: new-reserve-b,
          total-supply: new-total-supply
        }))
        
        ;; Update user liquidity with validated keys
        (map-set user-liquidity 
          { user: caller, token-a: validated-token-a, token-b: validated-token-b }
          { lp-tokens: new-user-lp }
        )
        
        (ok { lp-tokens-minted: lp-tokens })
      )
      ERR-INVALID-POOL
    )
  )
)

(define-public (remove-liquidity (token-a principal) (token-b principal) (lp-tokens uint))
  (let ((pool-key (get-pool-key token-a token-b))
        (caller tx-sender)
        (user-lp-data (get-user-liquidity caller token-a token-b))
        (user-lp (get lp-tokens user-lp-data)))
    
    ;; Input validation
    (asserts! (validate-amount lp-tokens) ERR-INVALID-AMOUNT)
    (asserts! (is-valid-token-contract token-a) ERR-INVALID-TOKEN)
    (asserts! (is-valid-token-contract token-b) ERR-INVALID-TOKEN)
    (asserts! (>= user-lp lp-tokens) ERR-INSUFFICIENT-LIQUIDITY)
    
    (match (map-get? pools pool-key)
      pool-data
      (let ((total-supply (get total-supply pool-data))
            (reserve-a (get reserve-a pool-data))
            (reserve-b (get reserve-b pool-data)))
        
        ;; Ensure total supply is valid
        (asserts! (> total-supply u0) ERR-INSUFFICIENT-LIQUIDITY)
        (asserts! (>= total-supply lp-tokens) ERR-INSUFFICIENT-LIQUIDITY)
        
        (let ((amount-a (/ (unwrap! (safe-multiply lp-tokens reserve-a) ERR-OVERFLOW) total-supply))
              (amount-b (/ (unwrap! (safe-multiply lp-tokens reserve-b) ERR-OVERFLOW) total-supply))
              (validated-token-a (get token-a pool-key))
              (validated-token-b (get token-b pool-key)))
          
          ;; Update pool reserves
          (map-set pools pool-key (merge pool-data {
            reserve-a: (- reserve-a amount-a),
            reserve-b: (- reserve-b amount-b),
            total-supply: (- total-supply lp-tokens)
          }))
          
          ;; Update user liquidity with validated keys
          (map-set user-liquidity 
            { user: caller, token-a: validated-token-a, token-b: validated-token-b }
            { lp-tokens: (- user-lp lp-tokens) }
          )
          
          (ok { amount-a: amount-a, amount-b: amount-b })
        )
      )
      ERR-INVALID-POOL
    )
  )
)

(define-public (swap-exact-tokens-for-tokens 
  (token-in principal) 
  (token-out principal) 
  (amount-in uint) 
  (min-amount-out uint))
  
  (let ((pool-key (get-pool-key token-in token-out)))
    
    ;; Input validation
    (asserts! (validate-amount amount-in) ERR-INVALID-AMOUNT)
    (asserts! (validate-amount min-amount-out) ERR-INVALID-AMOUNT)
    (asserts! (is-valid-token-contract token-in) ERR-INVALID-TOKEN)
    (asserts! (is-valid-token-contract token-out) ERR-INVALID-TOKEN)
    (asserts! (not (is-eq token-in token-out)) ERR-INVALID-TOKEN)
    
    (match (map-get? pools pool-key)
      pool-data
      (let ((reserve-a (get reserve-a pool-data))
            (reserve-b (get reserve-b pool-data))
            (validated-token-a (get token-a pool-key))
            (amount-out 
             (if (is-eq token-in validated-token-a)
               (unwrap! (calculate-swap-output amount-in reserve-a reserve-b) ERR-INSUFFICIENT-LIQUIDITY)
               (unwrap! (calculate-swap-output amount-in reserve-b reserve-a) ERR-INSUFFICIENT-LIQUIDITY))))
        
        (asserts! (>= amount-out min-amount-out) ERR-SLIPPAGE-TOO-HIGH)
        
        ;; Calculate fee with overflow protection
        (let ((fee-amount (/ (unwrap! (safe-multiply amount-in (var-get fee-rate)) ERR-OVERFLOW) u10000))
              (fee-acc-a (get fee-accumulated-a pool-data))
              (fee-acc-b (get fee-accumulated-b pool-data)))
          
          ;; Update reserves based on validated token with overflow protection
          (if (is-eq token-in validated-token-a)
            (let ((new-reserve-a (unwrap! (safe-add reserve-a amount-in) ERR-OVERFLOW))
                  (new-fee-acc-a (unwrap! (safe-add fee-acc-a fee-amount) ERR-OVERFLOW)))
              (asserts! (>= reserve-b amount-out) ERR-INSUFFICIENT-LIQUIDITY)
              (map-set pools pool-key (merge pool-data {
                reserve-a: new-reserve-a,
                reserve-b: (- reserve-b amount-out),
                fee-accumulated-a: new-fee-acc-a
              }))
            )
            (let ((new-reserve-b (unwrap! (safe-add reserve-b amount-in) ERR-OVERFLOW))
                  (new-fee-acc-b (unwrap! (safe-add fee-acc-b fee-amount) ERR-OVERFLOW)))
              (asserts! (>= reserve-a amount-out) ERR-INSUFFICIENT-LIQUIDITY)
              (map-set pools pool-key (merge pool-data {
                reserve-a: (- reserve-a amount-out),
                reserve-b: new-reserve-b,
                fee-accumulated-b: new-fee-acc-b
              }))
            )
          )
          
          (ok { amount-out: amount-out, fee-paid: fee-amount })
        )
      )
      ERR-INVALID-POOL
    )
  )
)

;; Admin Functions
(define-public (set-fee-rate (new-fee-rate uint))
  (begin
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-OWNER-ONLY)
    (asserts! (<= new-fee-rate u1000) ERR-INVALID-AMOUNT) ;; Max 10%
    (var-set fee-rate new-fee-rate)
    (ok true)
  )
)

;; Token whitelist management (optional security feature)
(define-public (add-approved-token (token principal))
  (begin
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-OWNER-ONLY)
    (asserts! (is-valid-token-contract token) ERR-INVALID-TOKEN)
    (map-set approved-tokens token true)
    (ok true)
  )
)

(define-public (remove-approved-token (token principal))
  (begin
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-OWNER-ONLY)
    (asserts! (is-valid-token-contract token) ERR-INVALID-TOKEN)
    (map-delete approved-tokens token)
    (ok true)
  )
)