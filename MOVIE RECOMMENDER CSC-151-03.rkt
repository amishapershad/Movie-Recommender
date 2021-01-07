#lang racket
(require csc151)
(require csc151/counters)
(provide (all-defined-out))

;Data
(define movies  (read-csv-file "/Users/amishapershad/Desktop/IMDb movies.csv"))
; From Kaggle
; https://www.kaggle.com/stefanoleone992/imdb-extensive-dataset

;https://www.cs.grinnell.edu/~hamidfah/csc151S20/labs/insertion-sort.html

;Filtering Genre

(define invalid-chars (string->list "0123456789,.?*%$#@!~;:"))

;;; Procedure:
;;;   char-valid?
;;; Parameters:
;;;  x, a character 
;;; Purpose:
;;;   checks if it is an invald character 
;;; Produces:
;;;;  the character if it is false and #\space if it is true 
;;; Preconditions:
;;;   [No additional]
;;; Postconditions:
;;;   [No additional]

(define char-valid?
  (lambda (x)
    (if (null? (filter (section equal? <> x) invalid-chars)) x #\space) ))

;;; Procedure:
;;;   string->words
;;; Parameters:
;;;  str, a string
;;; Purpose:
;;;   turns a string into a list of strings that are words
;;; Produces:
;;;;  a list
;;; Preconditions:
;;;   [No additional]
;;; Postconditions:
;;;   [No additional]

(define string->words
  (lambda (str)
    (string-split(list->string  (map char-valid?
                                     (string->list (string-replace (string-downcase str) "\'" "")))))))

;From Amisha's assignment 5 ^^^^^

;;; Procedure:
;;;   list-contains
;;; Parameters:
;;;   val, a Scheme value
;;;   lst, a list of Scheme values
;;; Purpose:
;;;   to determine if val is present in lst
;;; Produces:
;;;   result, a boolean value
;;; Preconditions:
;;;   [No additional]
;;; Postconditions:
;;;   result is #t if val appears somewhere in lst
;;;   result is #f if val does not appear in lst
;;;   if null? lst , then result is #f 

(define list-contains
  (lambda (val lst)
    (cond
      [(null? lst)
       #f]
      [(equal? val (car lst))
       #t]
      [else (list-contains val (cdr lst))])))

;;; Procedure:
;;;   clean-genre
;;; Parameters:
;;;   lst, a list of genres from a single entry in "IMBd movies.csv"
;;; Purpose:
;;;   to clean the list of genres
;;; Produces:
;;;   clean-lst, a cleaned list of genres
;;; Preconditions:
;;;   lst must contain a single string containing the genres for one movie
;;; Postconditions:
;;;   resulting lst has commas removed and each genres split into an indvidual string
;;;   clean-genre differs from helper-genre-filter by taking a lst of genres instead
;;;      of a string containing a movie name 

(define clean-genre
  (lambda (lst)
    (string->words (cadr lst))))

;;; Procedure:
;;;   helper-genre-filter 
;;; Parameters:
;;;   str, a name of a movie from "IMBd movies.csv" as a string
;;; Purpose:
;;;   to find and clean the genres for str entered
;;; Produces:
;;;   result, a cleaned list of genres for movie str
;;; Preconditions:
;;;   str must be a valid name of a movie in "IMBd movies.csv"
;;; Postconditions:
;;;   result is the genres for string without commas and separated into
;;;      individual strings

(define helper-genre-filter
  (lambda (str)
    (string->words (cadr (assoc str movies)))))

;;; Procedure:
;;;   helper-genre-filter-2 
;;; Parameters:
;;;   genres, a cleaned list of genres 
;;;   lst, the data from "IMBd movies.csv" 
;;; Purpose:
;;;   filter out all the genres that don't match the genres entered
;;; Produces:
;;;   result, a lst of movies that match the genres entered 
;;; Preconditions:
;;;   genres must be the cleaned genres for the string desired
;;; Postconditions:
;;;   result is all the movies in "IMBd movies.csv" that have at least one genre
;;;      that matches a any genre from the genres entered

(define helper-genre-filter-2
  (lambda (genres lst)
    (cond
      [(equal? (length genres) 1)
       (let kernel-1 ([genres genres] [lst lst] [so-far null])
         (let ([genre1 (car genres)])
           (cond
             [(null? (cdr lst))
              so-far]
             [(or (list-contains genre1 (clean-genre (car lst))))
              (kernel-1 genres (cdr lst) (cons (car lst) so-far))]
             [else (kernel-1 genres (cdr lst) so-far)])))]
                      
      [(equal? (length genres) 2)
       (let kernel-2 ([genres genres] [lst lst] [so-far null])
         (let ([genre1 (car genres)] [genre2 (cadr genres)])
           (cond
             [(null? (cdr lst))
              so-far]
             [(or (list-contains genre1 (clean-genre (car lst)))
                  (list-contains genre2 (clean-genre (car lst))))
              (kernel-2 genres (cdr lst) (cons (car lst) so-far))]
             [else (kernel-2 genres (cdr lst) so-far)])))]
      
      [(equal? (length genres) 3)
       (let kernel-3 ([genres genres] [lst lst] [so-far null])
         (let ([genre1 (car genres)] [genre2 (cadr genres)] [genre3 (caddr genres)])
           (cond
             [(null? (cdr lst))
              so-far]
             [(or (list-contains genre1 (clean-genre (car lst)))
                  (list-contains genre2 (clean-genre (car lst)))
                  (list-contains genre3 (clean-genre (car lst))))
              (kernel-3 genres (cdr lst) (cons (car lst) so-far))]
             [else (kernel-3 genres (cdr lst) so-far)])))]))) 

;;; Procedure:
;;;   genre-filter 
;;; Parameters:
;;;   str, a name of a movie from "IMBd movies.csv" as a string 
;;; Purpose:
;;;   to filter out all the movies from "IMBd movies.csv" that have
;;;     the same genre as str 
;;; Produces:
;;;   result, a list of movies entries 
;;; Preconditions:
;;;    str must be a valid name of a movie in "IMBd movies.csv"
;;; Postconditions:
;;;   result contains movies that match any genre listed for str

(define genre-filter
  (lambda (str)
    (helper-genre-filter-2 (helper-genre-filter str) movies)))

;Comparing Descriptions


(define invalid-words (string->words "a the and he his she hers they them their but and to for under get let if can could would or should 



  "))

;;; Procedure:
;;;   words-valid?
;;; Parameters:
;;;  x, a string of a single word 
;;; Purpose:
;;;   checks if it is an invald word
;;; Produces:
;;;;  the word if it is false and #\space if it is true 
;;; Preconditions:
;;;   [No additional]
;;; Postconditions:
;;;   [No additional]  

(define word-valid?
  (lambda (x)
    (if (null? (filter (section equal? <> x) invalid-words)) x #\space)))

;;; Procedure:
;;;   remove-extras
;;; Parameters:
;;;   lst, a list
;;; Purpose:
;;;   removes all the #\space characters
;;; Produces:
;;;   a cleaned list
;;; Preconditions:
;;;   [no additional]
;;; Postconditions:
;;;   result is a list with no #\space characters

(define remove-extras
  (lambda (lst)
    (cond
      [(empty? lst)
       null]
      [(equal? #\space (car lst))
       (remove-extras (cdr lst))]
      [else (cons (car lst)(remove-extras (cdr lst)))])))

;;; Procedure:
;;;   remove-invalid-words
;;; Parameters:
;;;   lst, a list
;;; Purpose:
;;;   combines words-valid? and remove-extras 
;;; Produces:
;;;   a cleaned list
;;; Preconditions:
;;;   the list can only have strings in it 
;;; Postconditions:
;;;   result is a list with no words from invalid-words

(define remove-invalid-words
  (lambda (lst)
    (remove-extras (map word-valid? lst))))
;;; Procedure:
;;;   clean-description
;;; Parameters:
;;;   lst, a list 
;;; Purpose:
;;;   cleans the descriptions in the data set
;;; Produces:
;;;   a cleaned list 
;;; Preconditions:
;;;   the list has to come from the descriptions of the movies in the data set
;;; Postconditions:
;;;   result is a list with no words from invalid-words

(define clean-description
  (lambda (lst)
    (remove-invalid-words (string->words (cadddr lst)))))

;;; Procedure:
;;;   compare-movies
;;; Parameters:
;;;   movie1, movie entered
;;;   movie2, the movies in the data set
;;; Purpose:
;;;   compares the descriptions of movie1 and movie2 and tallys the common words
;;; Produces:
;;;   movies 2 with the number of common words in the front 
;;; Preconditions:
;;;   both the movies have to be in the data set
;;; Postconditions:
;;;   result is a list
;Compares finds similar words in movie1's description in movie2
;  add the number of similar words to the beginning of movie2's list 
(define compare-movies
  (lambda (movie1 movie2)
    (let kernel ([lst1 (clean-description movie1)] [lst2 (clean-description movie2)] [so-far '(0)])
      (cond
        [(empty? lst1)
         (cons so-far movie2)]
        [(list-contains (car lst1) lst2)
         (kernel (cdr lst1) lst2 (map increment so-far))]
        [else (kernel (cdr lst1) lst2 so-far)]))))
        
;Movie Recommender

;From insertion sort lab:  https://www.cs.grinnell.edu/~hamidfah/csc151S20/labs/insertion-sort.html

;;; Procedure:
;;;   list-insertion-sort
;;; Parameters:
;;;   lst, a list to be sorted
;;;   may-precede?, a binary predicate
;;; Purpose:
;;;   Sort lst.
;;; Produces:
;;;   sorted, a list.
;;; Preconditions:
;;;   * may-precede? can be used with the elements of lst. That is for
;;;     all values a and b in lst, (may-precede? a b) successfully
;;;     returns a truth value.
;;;   * may-precede? is transitive.  That is, for all values a, b, and 
;;;     c in lst, if (may-precede? a b) and (may-precede? b c), then
;;;     (may-precede? a c).
;;;   * may-precede? is sensible.  That is, for all values a and b,
;;;     either (may-precede? a b), (may-precede? b a), or both.
;;; Postconditions:
;;;   * sorted is sorted.  That is, for all reasonable i,
;;;     (may-precede? (list-ref lst i) (list-ref lst (increment i)))
;;;   * sorted is a permutation of lst.

(define list-insertion-sort
  (lambda (lst may-precede?)
    (letrec ([insert
              (lambda (lst val)
                (cond
                  [(null? lst)
                   (list val)]
                  [(may-precede? val (car lst))
                   (cons val lst)]
                  [else
                   (cons (car lst) (insert (cdr lst) val))]))]
             [kernel
              (lambda (unsorted sorted)
                (if (null? unsorted) 
                    sorted
                    (kernel (cdr unsorted) (insert sorted (car unsorted)))))])
      (kernel lst null))))

;From insertion sort lab : https://www.cs.grinnell.edu/~hamidfah/csc151S20/labs/insertion-sort.html

;;; Procedure:
;;;   list-keyed-insertion-sort
;;; Parameters:
;;;   lst, a list
;;;   get-key, a procedure
;;;   may-precede?, a binary predicate
;;; Purpose:
;;;   Sort lst.
;;; Produces:
;;;   sorted, a list
;;; Preconditions:
;;;   * get-key? can be applied to each element of lst.
;;;   * may-precede? can be used with the values returned by get-key. That is 
;;;     for all values a and b in lst, (may-precede? (get-key a) (get-key b)) 
;;;     successfully returns a truth value.
;;;   * may-precede? is transitive.  That is, for all keys a, b, and 
;;;     c, if (may-precede? a b) and (may-precede? b c), then
;;;     (may-precede? a c).
;;;   * may-precede? is sensible.  That is, for all keys a and b,
;;;     (may-precede? a b) holds, (may-precede? b a) holds, or both
;;;     hold.
;;; Postconditions:
;;;   * sorted is sorted by key using may-precede?.  That is, for all i 
;;;     such that 0 <= i < (- (length lst) 1),
;;;     (may-precede? (get-key (list-ref sorted i))
;;;                   (get-key (list-ref sorted (+ i 1))))
;;;   * sorted is a permutation of lst.

(define list-keyed-insertion-sort
  (lambda (lst get-key may-precede?)
    (list-insertion-sort lst
                         (lambda (v1 v2)
                           (may-precede? (get-key v1) (get-key v2))))))



;;; Procedure:
;;;   movie-recommender
;;; Parameters:
;;;   str, name of movie
;;; Purpose:
;;;   Recommend 5 movies that share the most similarities with the movie entered
;;; Produces:
;;;   a list of 5 movies
;;; Preconditions:
;;;  The movies must be a valid name of a movie in IMbd movies 
;;; Postconditions:
;;;  The five movies are ordered from most similar to least similar
;;;  Determines similarities by genre and description

(define movie-recommender
  (lambda (str)
    (take
     (map cdr 
          (list-keyed-insertion-sort
           (map (section compare-movies (assoc str movies) <>) (genre-filter str))
           (lambda (entry) (car (car entry))) >=))5)))


;;; Attempt at Filter Actors

;(define actors
;  (lambda (str)
;    (caddr (assoc str movies))))
;
;(define string-maker
;  (lambda (str) ; actors
;    (let kernel ([ my-str str ][ char-maker (string->list (actors str))][pos 0] [so-far null ])
;    (cond
;      [(equal? pos (string-length char-maker))
;       so-far]
;      [(equal? ( list-ref char-maker pos) #\,)
;       
;       ( kernel (string-drop my-str pos)(drop char-maker pos) (- pos pos) (cons (substring my-str 0 pos)
;       so-far))]
;[else (kernel my-str char-maker (+ 1 pos) so-far)]))))



 


