(defpackage #:components-test
  (:use #:cl
        #:prove
        #:components))
(in-package #:components-test)

(defun set-equal (set-1 set-2 &key (test #'eql) key)
  (and (subsetp set-1 set-2 :key key :test test)
       (subsetp set-2 set-1 :key key :test test)))

(defvar *start-hook* nil)
(defvar *stop-hook* nil)

(defclass test-component (component)
  ((started :initform nil :accessor startedp)
   (stopped :initform nil :accessor stoppedp)))

(defun make-test-component (&rest dependencies)
  (make-instance 'test-component :depends-on dependencies))

(defmethod start ((component test-component))
  (setf (startedp component) t)
  (when *start-hook*
    (funcall *start-hook* component))
  component)

(defmethod stop ((component test-component))
  (setf (stoppedp component) t)
  (when *stop-hook*
    (funcall *stop-hook* component))
  component)

(plan nil)

(subtest "Check for dependencies"
  (let ((component (make-test-component :foo)))
    (ok (has-dependency-p component :foo)
        "Has dependency on :FOO")
    (ok (not (has-dependency-p component :bar))
        "Doesn't have a dependency on :BAR")))

(subtest "START and STOP return a (possibly modified) component."
  (let ((component (make-test-component)))
    (is-type (start component) 'component)
    (is-type (stop component) 'component)))


(subtest "Starting system wires and starts components."
  (let ((system `((:a . ,(make-test-component :b-prime))
                  (:b-prime . ,(make-test-component :b))
                  (:b . ,(make-test-component :c :d))
                  (:c . ,(make-test-component))
                  (:d . ,(make-test-component))
                  (:e . ,(make-test-component :b))))
        (*start-hook* (lambda (c)
                        (ok (every (lambda (dep)
                                     (subcomponent c dep))
                                   (dependencies c))
                            "All dependencies wired in at startup.")
                        (ok (every (lambda (dep)
                                     (and (startedp (subcomponent c dep))
                                          (not (stoppedp (subcomponent c dep)))))
                                   (dependencies c))
                            "All dependencies started at startup."))))
    (start-system system)
    (ok (every (lambda (component)
                 (and (startedp component)
                      (not (stoppedp component))))
               (mapcar #'cdr system))
        "All component started.")))

(subtest "Stopping system stops component before dependencies."
  (let ((system `((:a . ,(make-test-component :b-prime))
                  (:b-prime . ,(make-test-component :b))
                  (:b . ,(make-test-component :c :d))
                  (:c . ,(make-test-component))
                  (:d . ,(make-test-component))
                  (:e . ,(make-test-component :b))))
        (*stop-hook* (lambda (c)
                        (ok (every (lambda (dep)
                                     (subcomponent c dep))
                                   (dependencies c))
                            "All dependencies still wired in at stop.")
                        (ok (every (lambda (dep)
                                     (and (startedp (subcomponent c dep))
                                          (not (stoppedp (subcomponent c dep)))))
                                   (dependencies c))
                            "All dependencies still started at stop."))))
    (start-system system)
    (stop-system system)
    (ok (every #'stoppedp (mapcar #'cdr system))
        "All components stopped.")))

(subtest "Normalized alist suite"
  (let* ((alist '((:a . 1) (:b . 2) (:a . 3) (:c . 4)))
         (normalized (components::normalize-alist alist)))
    (subtest "No dupes, preserves key order, preserves values."
      (is normalized '((:a . 1) (:b . 2) (:c . 4))
          :test #'equalp)))

  (subtest "Can use :TEST for different key comparisons."
    (let* ((alist '(("foo" . 1) ("bar" . 2) ("FOO" . 3) ("baz" . 4)))
           (normalized (components::normalize-alist alist :test #'equalp)))
      (is normalized '(("foo" . 1) ("bar" . 2) ("baz" . 4))
          :test #'equalp)
      (ok (every #'string= (mapcar #'car normalized) '("foo" "bar" "baz"))
          "Keys have the right case.")))

  (subtest "Can use :KEY for different key structures.")
  (let* ((alist '(((:a . 1) . 1)
                  ((:b . 1) . 2)
                  ((:a . 1) . 3)
                  ((:c . 1) . 4)))
         (normalized (components::normalize-alist alist :key #'car)))
    (is normalized '(((:a . 1) . 1)
                     ((:b . 1) . 2)
                     ((:c . 1) . 4))
        :test #'equalp)))

;; (subtest "Normalized alist suite"
;;   (let* ((alist '((:a . 1) (:b . 2) (:a . 3) (:c . 4)))
;;          (normalized (components::normalize-alist alist)))

;;     (ok (set-equal normalized alist
;;                    :key #'car)
;;         "Normalized and original alist have equal sets of keys.")

;;     (block value-test
;;       (dolist (entry normalized)
;;         (let ((original-val  (cdr (assoc (car entry) alist)))
;;               (normalized-val (cdr entry)))
;;           (unless (= original-val normalized-val)
;;             (fail (format nil "Normalized alist has different value for key ~S: got ~S, expected ~S."
;;                           (car entry)
;;                           normalized-val
;;                           original-val))
;;             (return-from value-test))))
;;       (pass "Normalized alist has matching values for alist keys."))

;;     (let ((normalized-keys (mapcar #'car normalized))
;;           (original-keys (mapcar #'car alist)))
;;       (ok (equalp normalized-keys (remove-duplicates normalized-keys))
;;           "Normalized alist has no duplicate keys.")

;;       ;; REMOVE-DUPLICATES doesn't preserve order, so we need to
;;       ;; re-establish it after removing duplicate keys.
;;       (let ((deduped-keys (sort (remove-duplicates original-keys)
;;                                 (lambda (a b)
;;                                   (< (position a original-keys)
;;                                      (position b original-keys))))))
;;         (is normalized-keys deduped-keys
;;             "Normalized alist has keys in the same order."
;;             :test #'equalp)))))

(finalize)
