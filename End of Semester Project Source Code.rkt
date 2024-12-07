#lang racket
(require plot)

;-------------------------------------------------------- helper functions -----------------------------------------------------------

; procedure to update an associative list
(define (update-assoc alist key value)
  (cond ((null? alist) (list (cons key value)))  ;; If the list is empty, create a new pair
        ((equal? key (car (car alist))) (cons (cons key value) (cdr alist)))  ;; Update if key matches
        (else (cons (car alist) (update-assoc (cdr alist) key value)))))  ;; Recurse otherwise


(define (assoc-get key alist default)
  (let ((pair (assoc key alist)))
    (if pair
        (cdr pair)  ; Return the count if the key is found
        default)))  ; Return the default value (0) if the key is not found



;------------------------------------------ Begin Definitions for procedure and data abstraction -----------------------------------

(define (fetch-tweets keyword country start-date end-date)
  ; Mock tweet data for Uganda example
  ; Output: Returns a list of tweets with timestamps, content, and sentiment.
  (list '("2023-01-01" "Great progress in Uganda! #Happy" "positive")
        '("2023-01-02" "Challenging times ahead in Kampala. #Concerned" "negative")
        '("2023-01-03" "Unity and peace! #Optimistic" "positive")))


; pipeline to clean all tweets
; Cleans a single Tweet
(define (clean-tweet tweet-text)
  (let* ((no-urls (regexp-replace* #rx"https?://[a-zA-Z0-9./?=&%_-]+" tweet-text "")) ; Remove URLs
         (no-hashtags (regexp-replace* #rx"#[a-zA-Z0-9_]+" no-urls ""))               ; Remove hashtags
         (no-emojis (regexp-replace* #rx"[^\x20-\x7E]+" no-hashtags ""))              ; Remove emojis/non-standard chars
         (no-special-chars (regexp-replace* #rx"[|.]" no-emojis ""))                  ; Remove '|' and '.'
         (no-punctuation (regexp-replace* #rx"[[:punct:]]+" no-special-chars ""))     ; Remove other punctuation
         (trimmed (string-trim no-punctuation)))                                      ; Trim whitespace
    trimmed))

; Clean a list of Tweets
(define (clean-tweets tweet-list)
  (map (lambda (tweet)
         (list (car tweet)  ; Date
               (clean-tweet (cadr tweet))  ;Cleaned tweet content. Removes urls and hastag
               (caddr tweet)))  ; Sentiment
       tweet-list))

; analyze-sentiment
; Abstract: the expected out is a list of tweets with computed sentiment scores.
(define (analyze-sentiment tweets)
  (map (lambda (tweet)
         (list (car tweet)
               (cadr tweet)
               ;; Simple sentiment mapping
               (cond ((string-contains? (cadr tweet) "progress") "positive")
                     ((string-contains? (cadr tweet) "sad") "negative")
                     (else "neutral"))))
       tweets))


; aggregate-sentiment
; the procedure extracts date from tweet
(define (aggregate-sentiment tweets period)
  (foldl (lambda (tweet agg)
           (let ((month (substring (car tweet) 0 7))) ; extracts YYYY-MM
             (update-assoc agg month (+ 1 (assoc-get month agg 0)))))
         '() tweets))


; visualize-trends
; The output is ascii-based bar graph of the trend.
(define (visualize-trends aggregated-data)
  ;; ASCII visualization as an example
  (for-each (lambda (data)
              (display (car data)) ;; Month
              (display ": ")
              (display (make-string (cdr data) #\*)) ;; Bar graph
              (newline))
            aggregated-data))


;------------------------------------------ End Definitions for procedure and data abstraction ---------------------------------

; Function to count sentiments - counts the occurances of words.
(define (count-sentiments tweets)
  (foldl (lambda (tweet counts)
           (let ((sentiment (caddr tweet)))  ; extract sentiments (positive/negative/neutral)
             ; Update the sentiment count, ensuring numeric values are stored as counts
             (update-assoc counts sentiment (+ 1 (assoc-get sentiment counts 0)))))
         '() tweets))  ; Starst with an empty list for aggregation


; procedure to plot sentiment values
(define (plot-sentiment-bar tweets)
 (let* ((sentiment-counts (count-sentiments tweets))  ; Get the sentiment counts
         (sentiment-pairs
          (map (lambda (entry) 
                 (vector (car entry) 
                         (exact->inexact (cdr entry))))  ; Convert count to a number. 
               sentiment-counts)))  ; Prepare data for plotting

    ; check if there are any sentiment values to plot. 
    (if (null? sentiment-counts)
        (error "No data to plot: sentiment-counts is empty")
        (begin
          ; Debugging: Print the data being plotted
          (displayln "These are the Sentiment Pairs in the Tweets:")
          (displayln sentiment-pairs)

          ; creates the discrete histogram
          (plot 
           (discrete-histogram
            sentiment-pairs  ; Sentiment counts as pairs (x, y)
            )
           )
          )
        )
   )
  )

; Mock tweet data (for testing)
(define tweets
  '((2024-11-22 "Just saw the 😢 most amazing sunset in a progress way! 😍 #nature https://sunsetphotos.com" "positive")
    (2024-11-23 "Feeling sad about the state of the world today. 😢 #depressed https://newsupdate.com" "negative")
    (2024-11-24 "Python is an awesome 😢 programming language! Check it out at https://python.org #coding #developer" "neutral")
    (2024-12-23 "Feeling good about the state of the world today. 😢 #depressed https://newsupdate.com" "positive")
    (2024-12-24 "Python is an awesome 😢 programming language! Check it out at https://python.org #coding #developer" "positive")
    (2024-11-22 "TonyFergusonXT,On #SnapDownCity | Choking Necks & Cashing Checks | https://python.org #coding #developer" "positive")
    (2024-12-05 "So many people are requesting for this juicy video, I have no choice but to drop 💧" "positive")))


;(count-sentiments tweets)
;(plot-sentiment-bar tweets)
; Clean and analyze tweets, then visualize
(plot-sentiment-bar (analyze-sentiment (clean-tweets tweets)))
