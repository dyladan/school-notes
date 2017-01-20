#lang racket
;;; Merge two lists of numbers which are already in increasing order
  (define merge-lists
    (lambda (l1 l2)
      (if (null? l1)
          l2
          (if (null? l2)
              l1
              (if (< (car l1) (car l2))
                  (cons (car l1) (merge-lists (cdr l1) l2))
                  (cons (car l2) (merge-lists (cdr l2) l1)))))))
;;; -------------------------------------------------------------------
;;; Given list l, output those tokens of l which are in even positions
  (define even-positions
    (lambda (l)
      (if (null? l)
          '()
          (if (null? (cdr l))
              '()
              (cons (car (cdr l)) (even-positions (cdr (cdr l))))))))
;;; -------------------------------------------------------------------
;;; Given list l, output those tokens of l which are in odd positions
  (define odd-positions
    (lambda (l)
      (if (null? l)
          '()
          (if (null? (cdr l))
              (list (car l))
              (cons (car l) (odd-positions (cdr (cdr l))))))))
;;; ---------------------------------------------------------------------
;;; Use the procedures above to create a simple and efficient merge-sort
  (define merge-sort
    (lambda (l)
      (if (null? l)
          l
          (if (null? (cdr l))
              l
              (merge-lists
                (merge-sort (odd-positions l))
                (merge-sort (even-positions l)))))))

(merge-sort '(1 5 2 2))
