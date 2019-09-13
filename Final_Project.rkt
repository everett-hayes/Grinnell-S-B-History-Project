#lang racket
(require loudhum)
(require plot)

;Used Files
;;; Purpose: grab a list of 10,000 most used words
(define english-words (file->words "/home/lualuis/CSC151/Project/english-words.txt"))

;;; Purpose: grab the words within the S&B articles as one list of lists of words
(define file-set (car (file->list "/home/lualuis/CSC151/Project/mrclean.txt")))

;GRAPHING

;;; Procedure:
;;;   directory->file-names
;;; Parameters:
;;;   base, a string for the directory of S&B article files  
;;; Purpose:
;;;   converts a directory into a list of strings; those strings are the document names for each file.
;;; Produces:
;;;   file-names, a list of words
;;; Preconditions:
;;;   [No Additional]
;;; Postconditions:
;;;   (length file-names) = number of files is base
;;;   if directory has no files, then file-names is an empty list
(define directory->file-names
  (lambda (base)
    (let ([path-list (directory-list base)])
      (map path->string
           (map
            (section build-path base <>)
            path-list)))))

;;; Procedure:
;;;   date-grabber
;;; Parameters:
;;;   date-str, a file name from the SandB folder in string format
;;; Purpose:
;;;   Takes the date information from a file name in string format and converts
;;;   it into a string with the following concise format: dd/mm/yyyy
;;; Produces:
;;;   result, a string
(define date-grabber
  (lambda (date-str)
    (let* ([dates (string-split date-str "_")]
           [year (list-ref dates 2)]
           [month (list-ref dates 3)]
           [day (list-ref dates 4)])
      (string-append year "/" month "/" day))))

;;; Procedure:
;;;   hash-maker
;;; Parameters:
;;;   file-lst, a list
;;;   file-contents-lst, a list
;;; Purpose:
;;;   Creates a hash table which stores lists in file-contents-lst so that each list is referenced by the corresponding string in file-lst
;;; Produces:
;;;   hash, a hash table
;;; Preconditions:
;;;   * (length file-lst) = (length file-contents-lst))
;;;   * file-lst must be a list of file names in string form
;;; Postconditions:
;;;   * For all i in (length file-lst),
;;;      (hash-ref hash (list-ref file-lst i)) = (list-ref file-contents-lst i)
;;;   * If file-lst and file-contents-lst are empty, then hash will be empty
(define hash-maker
  (lambda (file-lst file-contents-lst)
    (let* ([haash (make-hash)]
           [sorted-file-lst (sort file-lst string-ci<?)]
           [dates-lst (map date-grabber sorted-file-lst)])
      (map (section hash-set! haash <> <>) dates-lst file-contents-lst)
      haash)))

;;; this cleaner was used to create the data used in all graphing code
;;; Procedure:
;;;   cleaner
;;; Parameters:
;;;   str, a string
;;; Purpose:
;;;   Cleans up the string by removing all symbols and numbers,
;;;   and all words that are greater than 10 and less than 3 letters in length
;;; Produces:
;;;   result, a lst of words
(define cleaner
  (lambda (str)
    (let ([length-specifier?
           (lambda (word)
             (> 10 (string-length word) 2))]
          [list-cleaner (o string-split
                           string-downcase
                           (section regexp-replace* #px"- *" <> "")
                           (section regexp-replace* #px"[—’‘”“~<>(){},.;:/?!\\\'\"\\[\\]]" <> " ")
                           (section regexp-replace* #px"\\d|\\s" <> " "))])
      (filter length-specifier? (list-cleaner str)))))

;;; Procedure:
;;;   value-counter
;;; Parameters:
;;;   val, a string
;;;   lst, a list
;;; Purpose:
;;;   Counts the number of occurences of val in the lst
;;; Produces:
;;;   result, a positive integer
(define value-counter
  (lambda (val lst)
    (let helper ([count 0]
                 [remaining lst])
    (cond
      [(null? remaining)
       count]
      [(equal? val (car remaining))
       (helper (+ 1 count) (cdr remaining))]
      [else
       (helper count (cdr remaining))]))))
;;; Purpose
;;;   Creates a hash table with with the number of entries as there are files in the folder "/home/rebelsky/Desktop/SandB/" so that the
;;;   cleaned contents of each file can be accessed by the corresonding date of the file
(define the-hash
  (let* ([file-lst (directory->file-names "/home/rebelsky/Desktop/SandB/")]
         [file-contents-lst (map (o cleaner file->string) file-lst)])
    (hash-maker file-lst file-contents-lst)))

;;; Procedure:
;;;   word-referencer
;;; Parameters:
;;;   val, a string
;;; Purpose:
;;;   Counts the number of occurences of val in each entry of the-hash
;;; Produces:
;;;   result-lst, a list of paired lists
;;; Preconditions:
;;;   [No Additional]
;;; Postconditions:
;;;   * For all i in (length result-lst),
;;;      (value-counter (hash-ref the-hash (car (list-ref result-lst i)))) = (caar (list-ref result-lst i))
;;;   * (length (hash-keys the-hash)) = (length result-lst)
(define word-referencer
  (lambda (val)
    (let ([keys-vec (list->vector (hash-keys the-hash))]
          [haaash (make-hash)])
      (let helper ([index 0])
        (cond
          [(equal? index (vector-length keys-vec))
           haaash]
          [else
           (hash-set!
            haaash
            (vector-ref keys-vec index)
            (value-counter val (hash-ref the-hash (vector-ref keys-vec index))))
           (helper (+ index 1))])))))

;;; Procedure:
;;;   grapher
;;; Parameters:
;;;   val, a string
;;; Purpose:
;;;   To produce a 2D graph which displays the occurences of val (on the y-axis) over time (on the x-axis).
;;;   The time element for each datapoint is obtained from the keys of the hash, which are dates from 1960 to 1970
;;; Produces:
;;;   graph, a 2D graph
(define grapher
  (lambda (val)
    (let* ([haaaash (word-referencer val)]
           [dates-lst (hash-keys haaaash)]
           [sorted-dates (sort dates-lst string-ci<?)]
           [sorted-freq (map (section hash-ref haaaash <>) sorted-dates)])
      (plot
       (discrete-histogram
        (map (o list->vector
                (section list "time" <>)) sorted-freq))))))


;;; Procedure:
;;;   grapher3d
;;; Parameters:
;;;   val, a string
;;; Purpose:
;;;   To produce a 3D graph which displays the frequency of a word in an article suggested by the thickness of a slab.
;;;   The time element for each datapoint is obtained from the keys of the hash, which are dates from 1960 to 1970
;;; Produces:
;;;   graph, a 3D graph
(define grapher3d
  (lambda (val)
    (let* ([haaaash (word-referencer val)]
           [dates-lst (hash-keys haaaash)]
           [sorted-dates (sort dates-lst string-ci<?)]
           [sorted-freq (map (section hash-ref haaaash <>) sorted-dates)])
      (plot3d
       (stacked-histogram3d
        (list (vector "" "" sorted-freq)))))))


;PRETTY DISPLAY

;;; this cleanup was used to create file-set, which is used for all pretty-display and article generation code
;;; Procedure:
;;;   cleanup
;;; Parameters:
;;;   str, a string
;;; Purpose:
;;;   first downcases all words in the string
;;;   then splits the string into words
;;;   then removes all words with length less than 4
;;; Produces:
;;;   cleaned, a list of strings that are considered important
;;; Preconditions:
;;;   str is a non-empty string
;;; Postconditions:
;;;   (< 3 (reduce min (map string-length (string-split (string-downcase str)))) = #t
(define cleanup
  (lambda (str)
    (letrec ([less-than-three?
              (lambda (word)
                (> (string-length word) 3))])
      (filter less-than-three? (string-split (string-downcase str) " ")))))

;;; Procedure:
;;;   list-maker
;;; Parameters:
;;;   lst, a list of pairs
;;; Purpose:
;;;   creates a list of words using the word and the frequency of the word
;;; Produces:
;;;   a list of words
;;; Preconditions:
;;;   the first part of each pair must be a string,
;;;   the second part of each pair must be a non-negative integer
;;; Postconditions:
;;;   (list-maker (list (cons "string" num)) = a list of "string" n number of times
(define list-maker
  (lambda (lst)
    (let ([proc
           (lambda (lst)
             (make-list (cdr lst) (car lst)))])
      (apply append (map proc lst)))))
#| Example/Tests for list-maker
> (list-maker (list (cons "chris" 1) (cons "john" 3)))
'("chris" "john" "john" "john")
|#

;;; Procedure:
;;;   assign-probs-exact
;;; Parameters:
;;;   lst, a list
;;; Purpose:
;;;   count the number of appearances of each element in lst as a fraction
;;; Produces:
;;;   counted, a list of lists of pairs
;;; Preconditions:
;;;   although this function can work on any data type, the input will be a list of strings
;;; Postconditions:
;;;   the first pair in each list of counted is a word
;;;   the second pair in each list of counted is the fraction frequency of the word in lst
;;;   adding all the probabilities in counted will equal 1
;;;   every word in counted is unique (meaning that each word has no duplicates)
;;;   if lst is empty, then counted will be an empty list
(define assign-probs-exact
  (lambda (lst)
    (let ([len (length lst)])
      (let kernel ([probs-so-far null]
                   [remaining lst])
        (if (null? remaining)
            probs-so-far
            (let* ([word (car remaining)]
                   [tally (tally-value remaining word)]
                   [without-word (filter (lambda (val) (not (equal? val word))) remaining)])
              (kernel (cons (cons word (/ tally len)) probs-so-far)
                      without-word)))))))

#| Examples/Test for assign-probs-exact
> (assign-probs-exact (list "hi" "hi" "there" "a" "a" "a"))
'(("a" . 1/2) ("there" . 1/6) ("hi" . 1/3))
> (assign-probs-exact (list 1 1 2 3 3 3))
'((3 . 1/2) (2 . 1/6) (1 . 1/3))
|#

;;; Procedure:
;;;   assign-probs-inexact
;;; Parameters:
;;;   lst, a list
;;; Purpose:
;;;   count the number of appearances of each element in lst as a decimal
;;; Produces:
;;;   counted, a list of lists of pairs
;;; Preconditions:
;;;   although this function can work on any data type, lst will be a list of strings
;;; Postconditions:
;;;   the first pair in each list of counted is a word
;;;   the second pair in each list of counted is the decimal frequency of the word in lst
;;;   adding all the probabilities in counted will equal 100
;;;   every word in counted is unique (meaning that each word has no duplicates)
;;;   if lst is empty, then counted will be an empty list
(define assign-probs-inexact
  (lambda (lst)
    (let ([len (length lst)])
      (let kernel ([probs-so-far null]
                   [remaining lst])
        (if (null? remaining)
            probs-so-far
            (let* ([word (car remaining)]
                   [tally (tally-value remaining word)]
                   [without-word (filter (lambda (val) (not (equal? val word))) remaining)])
              (kernel (cons (cons word (* 100 (exact->inexact (/ tally len)))) probs-so-far)
                      without-word)))))))

#| Examples/Test for assign-probs-inexact
> (assign-probs-inexact (list 1 1 2 3 3 3))
'((3 . 50.0) (2 . 16.666666666666664) (1 . 33.33333333333333))
> (assign-probs-inexact (list "hi" "hi" "there" "a" "a" "a"))
'(("a" . 50.0) ("there" . 16.666666666666664) ("hi" . 33.33333333333333))
|#

;;; Procedure:
;;;   remove-non-english
;;; Parameters:
;;;   related-words, a list of strings
;;; Purpose:
;;;   Removes all non-english words from a list of strings and creates a new list
;;;   including all english-words. Accomplishes this by comparing the list to an
;;;   english dictionary. (the file english-words)
;;; Produces:
;;;   a list of strings that are english words found in the file english words
;;; Preconditions:
;;;   related-words is a non empty list of strings
;;; Postconditions:
;;;  let x be an arbitrary word within related-words
;;;  (if (and (contains? x related-words) (contains? x english-words))
;;;      (contains? x new-words) = #t
;;;      else (contains? x new-words) = #f)
(define remove-non-english
  (lambda (related-words)
    (let ([count (length related-words)])
      (let helper ([index 0]
                   [new-words null])
        (cond [(equal? index count)
               new-words]
              [(list? (member (list-ref related-words index) english-words))
               (helper (+ index 1) (cons (list-ref related-words index) new-words))]
              [else
               (helper (+ index 1) new-words)])))))

;;; Thanks to our professor Samuel Rebelsky for help writing this code
;;; Procedure:
;;;   assign-counts
;;; Parameters:
;;;   lst, a list
;;; Purpose:
;;;   creates a list of pairs with the car having the word and the cdr having the tally
;;; Produces:
;;;   a list of pairs with the car having the word and the cdr having the tally
(define assign-counts
  (lambda (lst)
    (let kernel ([count-so-far null]
                 [remaining lst])
      (if (null? remaining)
          count-so-far
          (let* ([word (car remaining)]
                 [tally (tally-value remaining word)]
                 [without-word (filter (lambda (val) (not (equal? val word))) remaining)])
            (kernel (cons (cons word tally) count-so-far)
                    without-word))))))

;;; Procedure:
;;;   indices-of
;;; Parameters:
;;;   val, an element
;;;   lst, a list
;;; Purpose:
;;;   computes every position of val within lst
;;; Produces:
;;;   a list of integers that are locations of val
;;; Preconditions:
;;;   lst is a non-empty list
;;; Postconditions:
;;;   (map (section equal? val <>) (map (section list-ref lst <>) (indices-of val lst)))
;;;   must always be true
(define indices-of
  (lambda (val lst)
    (letrec ([kernel
              (lambda (pos indices-so-far val lst)
                (cond
                  [(null? lst)
                   indices-so-far]
                  [(equal? (car lst) val)
                   (kernel (+ pos 1) (append indices-so-far (list pos)) val (cdr lst))]
                  [else
                   (kernel (+ pos 1) indices-so-far val (cdr lst))]))])
      (cond [(not (list? lst))
             (error "contains: Procedure expects a list, given:" lst)]
            [else
             (kernel 0 null val lst)]))))

;; Procedure:
;;;   find
;;; Parameters:
;;;   num, a non-negative integer
;;;   lst, a list of strings
;;;   span, the amount of words to the left and right of each word occurence that will be extracted, a non-negative integer
;;; Purpose:
;;;   find the (span) words surrounding the word corresponding to the location of num
;;; Produces:
;;;   a list of strings
;;; Preconditions:
;;;   lst is a non-empty list
;;; Postconditions:
;;;   (find 1 '("hello" "jenny" .....) 20) = a list of the twenty words to the right of jenny
;;;   (find (index-of lst "jenny") '(.... "jenny" "brother") 20) = a list of the twenty words to the left of jenny
;;;   (find (index-of lst "jenny") '(.... "jenny" ....) 20) = a list of twenty words, ten to the left of jenny and ten to the right of jenny
(define find
  (lambda (num lst span)
    (cond [(<= (length lst) (* 2 (+ span 1)))
           (error "The scope of analysis is too broad. Please enter a smaller number as the last parameter")]
          [(< num span)
           (take (drop lst num) (* 2 span))]
          [(> num (- (length lst) span))
           (drop (take lst num) (* 2 span))]
          [else
           (take (drop lst (- num span)) (* 2 span))])))

;;; Procedure:
;;;   join
;;; Parameters:
;;;   lst, a list of lists
;;; Purpose:
;;;   append a list of lists together
;;; Produces:
;;;   a list
;;; Preconditions:
;;;   lst must be a list of at least one list
;;; Postconditions:
;;;   (reduce append (list (list "a") (list "b")))) = (join (list (list "a") (list "b")))
(define join
  (lambda (lst)
    (if (null? lst)
        null
        (append (car lst) (join (cdr lst))))))

;;; Procedure:
;;;   related-words-counts
;;; Parameters:
;;;   lst, a list of words
;;;   word, a one-word string
;;; Purpose:
;;;   counts the closest words to word in lst
;;; Produces:
;;;   counts, a list of paired lists with each list having the word and its count.
;;; Preconditions:
;;;   to produce the best results, lst must be a list of more than 20 words and word must be in the list
;;; Postconditions:
;;;   if word is not in lists, then ___ is null list
;;;   counts has no duplicate words.
(define related-words-counts
  (lambda (lst word parameter)
    (let ([related-words (join (map (section find <> lst parameter) (indices-of word lst)))])
      (remove-duplicates (assign-counts (remove-non-english related-words))))))

#| Example/Tests for related-words-counts
>(define one-file (car file-set))  ; first S&B file
>(related-words-counts one-file "song")
'(("hope" . 1)
  ("true" . 1)
  ("else" . 1)
  ("also" . 1)
  ("being" . 1)
  ("besides" . 1)
  ("last" . 1)
  ("show" . 1)
  ("variety" . 1)
  ("song" . 1)
  ("apology" . 1)
  ("like" . 1)
  ("very" . 1)
  ("help" . 1)
  ("ever" . 1)
  ("newspaper" . 1))
|#

;;; Procedure:
;;;   top-related-words
;;; Parameters:
;;;   file, a list of lists of words
;;;   span, the amount of words near the given string that will be considered
;;;   word, a one-word string
;;;   files,the database of S&B article we are pulling from
;;; Purpose:
;;;   provide a list of the top 10 most frequent words with their counts
;;; Produces:
;;;   a list of pairs with the word and it's count relative to the input word
(define top-related-words
  (lambda (file word span)
    (let* ([related-words-in-files (list-maker (apply append (map (section related-words-counts <> word span) file)))]
           [fraction-probabilities (assign-probs-exact related-words-in-files)]
           [decimal-probabilities (assign-probs-inexact related-words-in-files)])
      (cond [(null? related-words-in-files)
             (error (string-append "The word: \"" word "\" does not appear in the file set. Please enter a different word as the first parameter"))])
      (letrec ([rank-related-words (lambda (lsts)
                                     (sort lsts (lambda (elt1 elt2) (> (cdr elt1) (cdr elt2)))))])
        (cond
          [(>= (length (rank-related-words decimal-probabilities)) 10)
           (take (rank-related-words decimal-probabilities) 10)]
          [else (error "The scope of analysis is too specific. Please enter a larger number as the last parameter")])))))

;;; Procedure:
;;;   display-most-related-words
;;; Parameters:
;;;   val, a one-word string
;;;   span, a positive integer  
;;; Purpose:
;;;   Presents the top 10 commonly appearing words that appear close to val in the SandB files, 
;;;   specifically in the range specified by span of words on each side of all occurrences of val 
;;; Produces:
;;;   display, a displayed string
;;; Preconditions:
;;;   [No Additional]
;;; Postconditions:
;;;   * If the val does not appear in file-set, then the following error message will be returned:
;;;     “The word: "ent" does not appear in the file set. Please enter a different word as the first
;;;     parameter”
;;;   * If the scope of analysis is too specific (if val is a somewhat obscure word), so that the
;;;     number of commonly appearing words is less than 10, then the following error message will ;;;     be returned: “The scope of analysis is too specific. Please enter a larger number as the last ;;;     parameter”
;;;   * If the scope of analysis it too large (if val appears in very short article, so that the searching ;;;     range is larger than the size of the article), then the following error message will be
;;;     returned: "The scope of analysis is too broad. Please enter a smaller number as the last parameter"
(define display-most-related-words
  (lambda (word span)
    (let* ([lsts (top-related-words file-set word span)]
           [relative-multiplier (/ 100 (cdr (car lsts)))]
           [intro (string-append "\n You searched the word: "
                                 (string-upcase word)
                                 "\n\n The most related words are:\n")]
           [ending (string-append "\n[Notice that "
                                 (string-upcase (car (car lsts)))
                                 ", the word most related to "
                                 (string-upcase word)
                                 ", is given as the reference case with a relative frequency of 100]")])
      (let helper ([index 0]
                   [result (string)])
        (if (equal? index 9)
            (display (string-append intro result ending))
            (helper (+ index 1)
                    (string-append result
                                   " "
                                   (string-upcase (car (list-ref lsts index)))
                                   " with a relative frequency of "
                                   (number->string (round (* relative-multiplier (cdr (list-ref lsts index)))))
                                   "\n")))))))

#|Example/Tests of most-related-words-and-sample-sentence
> (most-related-words-and-sample-sentence file-set "nixon" 5)

   You searched the word: NIXON

   The most related words are:
 (with NIXON given as the reference case
 with a relative frequency of 100):

PRESIDENT with a relative frequency of 100.0
POLICY with a relative frequency of 25.0
PEOPLE with a relative frequency of 19.0
COUNTRY with a relative frequency of 17.0
ADMINISTRATION with a relative frequency of 14.0
MAJOR with a relative frequency of 14.0
THINK with a relative frequency of 14.0
LIKE with a relative frequency of 14.0
SPEAK with a relative frequency of 14.0
|#

;ARTICLE GENERATION

;;; Procedure:
;;;   article-generator
;;; Parameters:
;;;   word, a one word string
;;;   span, the amount of words near the given string that will be considered
;;;   file, the file from which we are searching for the word
;;; Purpose:
;;;   creates a bank of words that can be chosen by the procedure sentence
;;; Produces:
;;;   a string of 60 words that are all related to the given word
(define article-generator
  (lambda (file word span)
    (cond [(< (string-length word) 4)
         (error "article-generator: expected a word of at least length 4, given:" word)]
          [(null? (reduce append (map (section words <> word span) file)))
         (error (string-append "article-generator-> the given word: " word " is not found in the file set"))])
    (let ([final (remove-duplicates (assign-probs
                                     (fraction->count (reduce append
                                                  (map (section words <> word span) file)))))])
      (let kernel ([i 0]
                   [so-far ""])
        (if (= i 60)
            so-far
            (kernel (+ i 1) (string-append (biased-select final) " " so-far)))))))
;;;;;;;; This code was taken from Assignment 8
;;; Procedure:
;;;   biased-select
;;; Parameters:
;;;   lst, a non-empty list of value/probability lists
;;; Purpose:
;;;   Select one of the elements in the list, choosing
;;;   the element according to probability.  (This is
;;;   called "biased selection" in the literature.)
;;; Produces:
;;;   value, a value
;;; Preconditions:
;;;   * Each element of lst has the form (val prob).
;;;   * Each probability is a real number.
;;;     That is (all (o real? cadr) lst)
;;;   * Each probability is between 0 and 1, inclusive.
;;;   * The sum of all the probabilities is 1.
;;;     That is, (reduce + (map cadr lst)) = 1.
;;; Postconditions:
;;;   * value is one of the values in the list.  That is
;;;     (member? value (map car lst)).
;;;   * It is difficult to predict which value we get.
;;;   * Suppose the list is of the form ((val1 prob1)
;;;     (val2 prob2) ... (valn probn)).  Over a long
;;;     series of calls, we'll see val1 about prob1
;;;     of the time, val2 about prob2 of the time, and so
;;;     on and so forth.
(define biased-select
  (lambda (lst)
    (let kernel ([r (random)]
                 [remaining lst])
      (let* ([entry (car remaining)]
             [value (car entry)]
             [prob (cdr entry)])
        (cond
          [(null? (cdr remaining))
           value]
          [(< r prob)
           value]
          [else
           (kernel (- r prob)
                   (cdr remaining))])))))

;;; Procedure:
;;;   words
;;; Parameters:
;;;   lst, a list
;;;   word, a string
;;;   span, a positive integer
;;; Purpose:
;;;   Replaces the indice of the word with the words near by it, removes words that does not make sense,
;;;   calls assign-probs and then finally remove any duplicates created.
;;; Produces:
;;;   result, a list of pairs

(define words
  (lambda (lst word span)
    (let ([related-words (join (map (section find <> lst span) (indices-of word lst)))])
      (remove-duplicates (assign-probs (remove-non-english related-words))))))

;;; Procedure:
;;;   assign-probs
;;; Parameters:
;;;   lst, a list of lists
;;; Purpose:
;;;   creates a pair with the word in the car and the fraction of frequency for which the word for
;;;   appears for every single list in the list
;;; Produces:
;;;    a pair with the word in the car and the fraction of frequency for which the word appears for
;;;    every single list in the list
(define assign-probs
  (lambda (lst)
      (map (section assign-probs-singleton <> lst) lst)))

;;; Procedure:
;;;   assign-probs-singleton
;;; Parameters:
;;;   lst, a list
;;;   word, a string
;;; Purpose:
;;;   creates a pair with the word in the car and the fraction of frequency for which the word
;;; appears
;;; Produces:
;;;    a pair with the word in the car and the fraction of frequency for which the word appears
(define assign-probs-singleton
  (lambda (word lst)
    (cons word (/ (length (indexes-of lst word)) (length lst)))))

;;; Procedure:
;;;   fraction->count
;;; Parameters:
;;;   lst, a list of pairs
;;; Purpose:
;;;   converts a pair with a car of a word and a cdr of a fraction into a list
;;; Produces:
;;;    a list of each occurence of each word
(define fraction->count
  (lambda (lst)
    (let ([proc
           (lambda (lst)
             (make-list (numerator (cdr lst)) (car lst)))])
      (reduce append (map proc lst)))))

;;; Procedure:
;;;   verb
;;; Parameters:
;;;   N/A
;;; Purpose:
;;;   selects a random verb from a file of verbs
;;; Produces:
;;;   a string that is an verb
(define verb
  (lambda ()
    (file->words "/home/bainwala/Desktop/Verbs")))

;;; this documentation and code was found in a reading on randomness
;;; Procedure:
;;;   random-elt
;;; Parameters:
;;;   lst, a non-empty list
;;; Purpose:
;;;   Unpredictably pick an element of lst.
;;; Produces:
;;;   val, a value
;;; Preconditions:
;;;   [No additional]
;;; Postconditions:
;;;   * val is an element of lst.
;;;   * If lst contains more than one element, it is difficult to predict
;;;     which element val is.
(define random-elt
  (lambda (lst)
    (list-ref lst (random (length lst)))))

;;; Procedure:
;;;   articles
;;; Parameters:
;;;   N/A
;;; Purpose:
;;;   selects a random article from a list of articles
;;; Produces:
;;;   a string that is an article
(define articles
  (random-elt (list "a" "an" "the")))

;;; Procedure:
;;;   cause
;;; Parameters:
;;;   N/A
;;; Purpose:
;;;   selects a random causation word from a list of causation words
;;; Produces:
;;;   a string that is a causation word
(define cause
  (lambda()
  (random-elt (list "because" "since" "so that"))))

;;; Procedure:
;;;   pronoun
;;; Parameters:
;;; N/A
;;; Purpose:
;;;   selects a random pronoun from a file of pronouns
;;; Produces:
;;;   a string that is a pronoun
(define pronoun
  (lambda()
    (random-elt (file->words "/home/bainwala/Desktop/Pronouns"))))

;;; Procedure:
;;;   helper-word
;;; Parameters:
;;; N/A
;;; Purpose:
;;;   selects a random helper-word from a list of helper-words
;;; Produces:
;;;   a string that is a helper-word
(define helper-word
  (lambda()
    (random-elt (list "have" "has" "had" "may" "might" "must" "could" "am" "is" "are" "did" "shall" "should" "will" "would"))))

;;; Procedure:
;;;   adjective
;;; Parameters:
;;; N/A
;;; Purpose:
;;;   selects a random adjective from a file of adjectives
;;; Produces:
;;;   a string that is an adjective
(define adjective
  (lambda ()
    (random-elt (file->words "/home/bainwala/Desktop/Adjectives"))))

;;; Procedure:
;;;   template
;;; Parameters:
;;;   noun1, a string
;;;   noun2, a string
;;;   verb1, a string
;;;   verb2, a string
;;; Purpose:
;;;   contains all the different sentences structures
;;; Produces:
;;;   randomly selects one of the sentence structures for use in the procedure sentences.
(define template
  (lambda (noun1 noun2 verb1 verb2)
    (let ([x (random 7)])
      (cond
        [(equal? x 0)
         (string-append "The " (adjective) " " noun1 " always " verb1 " " (cause) " " (pronoun) " " (helper-word) " " (adjective) " " noun2 ".")]
        [(equal? x 1)
         (string-append "A " noun1 ", who " verb1 " " noun2 ", " (helper-word) " " verb2 " more often.")]
        [(equal? x 2)
         (string-append (string-titlecase noun1) " " verb1 " " (cause) " " noun2 " " (helper-word) " " verb2 ".")]
        [(equal? x 3)
         (string-append "I went to " noun1 " and " verb1 " " noun2 " looking " (adjective) ".")]
        [(equal? x 4)
         (string-append (string-titlecase noun1) " " verb1 " with " noun2 " " (cause) " they were " (adjective) ".")]
        [(equal? x 5)
         (string-append (string-titlecase noun1) " was " (adjective) " because " noun2 " was " (adjective) ".")]
        [(equal? x 6)
         (string-append (string-titlecase noun1) " took the " noun2 " and " verb1 ".")]
        ))))  
        

;;; Procedure:
;;;   paragraph
;;; Parameters:
;;;   file, a list of lists of words
;;;   word, a one word string
;;;   span, the amount of words near the given string that will be considered
;;; Purpose:
;;;   creates a short article about the given word
;;; Produces:
;;;   a string consisting of 7 sentences generated using words that most often appear around the input word
;;;  Postconditions:
;;;    since sentence is our reach, the end result is not as effective as possible. So, further work is needed for perfection.
;;;    (time (sentence file-set “word” 3)) = about 5 seconds
(define paragraph
  (lambda (word span)
    (let kernel ([index 0]
                 [so-far ""])
      (let* ([noun1 (random-elt (string-split (article-generator file-set word span)))]
             [noun2 (random-elt (string-split (article-generator file-set word span)))]
             [verb1 (random-elt (verb))]
             [verb2 (random-elt (verb))])
      (if (= index 7)
          (display so-far)
          (kernel (+ 1 index) (string-append (template noun1 noun2 verb1 verb2) " " so-far)))))))







