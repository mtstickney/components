(in-package #:components)

(defclass component ()
  ((dependencies :initarg :depends-on :accessor dependencies
                 :documentation "A list of named components that this component depends on.")
   (subcomponents :initarg :subcomponents :accessor subcomponent-map
                  :initform nil
                  :documentation "An alist of named subcomponents that satisfy this components dependencies."))
  (:default-initargs :depends-on '()))

(define-condition invalid-dependency-error (error)
  ((name :initarg :name :accessor dependency-name)
   (component :initarg :component :accessor component))
  (:report (lambda (c s)
             (format s "Component ~S has no dependency ~S"
                     (component c)
                     (dependency-name c)))))

(define-condition missing-dependency-error (error)
  ((name :initarg :name :accessor dependency-name)
   (component :initarg :component :accessor component))
  (:report (lambda (c s)
             (format s "Component ~S has no subcomponent for dependency ~S"
                     (component c)
                     (dependency-name c)))))

(defun has-dependency-p (component name)
  (if (member name (dependencies component))
      t
      nil))

;; TODO: argument order? (good for dispatch, bad for currying)
;; TODO: function name
(defun subcomponent (component name)
  (check-type component component)
  (unless (has-dependency-p component name)
    (error 'invalid-dependency-error :component component :name name))
  (let ((subcomponent (assoc name (subcomponent-map component))))
    (unless subcomponent
      (error 'missing-dependency-error :component component :name name))
    (cdr subcomponent)))

(defun add-subcomponent (component name subcomponent)
  (check-type component component)
  (check-type subcomponent component)
  (check-type name symbol)
  ;; Note: this leaks entries if the same component is added more than
  ;; once.
  (setf (subcomponent-map component)
        (acons name subcomponent (subcomponent-map component)))
  component)

(defun remove-subcomponent (component name)
  (check-type component component)
  (check-type name symbol)
  (setf (subcomponent-map component)
        (remove name (subcomponent-map component)
                :key #'car))
  component)

(defgeneric start (component)
  (:documentation "Initialize and possibly start processing for COMPONENT.")
  (:method ((component component))
    component))
(defgeneric stop (component)
  (:documentation "Deinitialize and stop processing for COMPONENT.")
  (:method ((component component))
    component))

(defun load-dependency (component dep system)
  "Load the dependency DEP of COMPONENT from the components defined in SYSTEM."
  (let ((cell (assoc dep system)))
    (unless cell
      ;; TODO: add proper condition type here.
      (error "Dependency ~S of component ~S is not defined in system ~S~%"
             dep
             component
             system))
    (setf component (add-subcomponent component dep (cdr cell)))))

(defun unload-dependency (component dep)
  "Unload the (deinitialized) dependency DEP from COMPONET."
  (check-type component component)
  (check-type dep symbol)
  (unless (has-dependency-p component dep)
    (error 'missing-dependency-error :component component :name dep))
  (remove-subcomponent component dep))

(defun load-dependencies (component system)
  "Load the (initialized) dependencies for COMPONENT from SYSTEM."
  (check-type component component)
  (reduce (lambda (component dep) (load-dependency component dep system))
          (dependencies component)
          :initial-value component))

(defun unload-dependencies (component)
  (check-type component component)
  (reduce #'unload-dependency
          (dependencies component)
          :initial-value component))

(defun normalize-alist (alist &key (key #'identity) (test #'eql testp))
  "Return an alist with the same effective keys and values as ALIST,
but not duplicates."
  (let ((lst '()))
    (dolist (entry alist)
      (unless (apply #'assoc (funcall key (car entry)) lst
                     :key key
                     (if testp
                         (list :test test)
                         nil))
        (push entry lst)))
    (nreverse lst)))

(defun system-componentf (system name)
  (let ((cell (assoc name system)))
    ;; TODO: use a proper condition.
    (unless cell
      (error "There is no component defined for ~S in system ~S.~%"
             name
             system))
    (cdr cell)))

(define-setf-expander system-componentf (system name)
  (let ((name-var (gensym "NAME"))
        (system-var (gensym "SYSTEM"))
        (cell-var (gensym "CELL"))
        (value-var (gensym "VALUE")))
    (values (list name-var
                  system-var
                  cell-var)
            (list name
                  system
                  `(assoc ,name-var ,system-var))
            (list value-var)
            `(if ,cell-var
                 (setf (cdr ,cell-var) ,value-var)
                 (setf ,system (acons ,name-var ,value-var ,system-var)))
            `(system-componentf ,system-var ,name-var))))

(defun start-system (system)
  (dolist (component-name (topological-sort (normalize-alist system)))
    ;; Load dependencies into this component.
    (setf (system-componentf system component-name)
          (load-dependencies (system-componentf system component-name) system))
    ;; Start the component and store it in the system.
    (setf (system-componentf system component-name)
          (start (system-componentf system component-name))))
  system)

(defun stop-system (system)
  (dolist (component-name (nreverse (topological-sort system)))
    ;; Stop the component and store it in the system.
    (setf (system-componentf system component-name)
          (stop (system-componentf system component-name)))
    ;; TODO: this doesnt make much sense except for symmetry. remove it?
    ;; Unload dependencies.
    (setf (system-componentf system component-name)
          (unload-dependencies (system-componentf system component-name))))
  system)

(defmacro with-started-system ((system) &body body)
  "Start SYSTEM and ensure that is is stopped when BODY is exited."
  (let ((system-var (gensym "SYSTEM")))
    `(let ((,system-var ,system))
       (start-system ,system-var)
       (unwind-protect
            ,@body
         (stop-system ,system-var)))))

;; Utility function to make a system from an adjacenty list.
(defun make-system (adj-list)
  (loop for (name . deps) in adj-list
     collect (cons name (make-instance 'component :depends-on deps))))

(defun system-adjacency-list (system)
  (loop for (name . component) in system
     collect (cons name (mapcar (lambda (name) (system-componentf system name))
                                (dependencies component)))))

(defparameter *system*
  (make-system
   `((:a . (:b-prime))
     (:b-prime . (:b))
     (:b . (:c :d))
     (:e . (:b))
     (:c . nil)
     (:d . nil))))

(defparameter *adj-list*
  (system-adjacency-list *system*))

(define-condition missing-node-error (error) ())

(defun dest-nodes (node adjacencies)
  (let ((entry (assoc node adjacencies)))
    (unless entry
      (error 'missing-node-error))
    (dependencies (cdr entry))))

(defun find-roots-1 (adjacencies)
  (let* ((node-set (mapcar #'car adjacencies))
         (roots (copy-list node-set)))
    (dolist (node node-set)
      ;; Perf: use remove-if and MEMBER?
      (dolist (dep (dest-nodes node adjacencies))
        (setf roots (remove dep roots))))
    roots))

;; Fast, conses more than find-roots-1
(defun find-roots-2 (adjacencies)
  (let* ((node-set (mapcar #'car adjacencies))
         (roots (copy-list node-set)))
    (dolist (node node-set)
      (setf roots (remove-if (lambda (n)
                               (member n (dest-nodes node adjacencies)))
                             roots)))
    roots))

;; Fast, conses a bit less than the others.
(defun find-roots-3 (adjacencies)
  (let* ((roots (mapcar #'car adjacencies)))
    (dolist (entry adjacencies)
      (setf roots (set-difference roots (dependencies (cdr entry)))))
    roots))

;; Slowest, conses the most.
(defun find-roots-4 (adjacencies)
  (let ((counts (make-hash-table :size (length adjacencies))))
    (dolist (entry adjacencies)
      (setf (gethash (car entry) counts) 0))
    (dolist (entry adjacencies)
      (let ((outgoing (dependencies (cdr entry))))
        (dolist (node outgoing)
          (incf (gethash node counts)))))
    (loop for k being the hash-keys of counts
       when (zerop (gethash k counts))
       collect k)))

(declaim (inline find-roots))
(defun find-roots (adjacencies)
  (find-roots-3 adjacencies))

;; This is BFS, which is a (broken) degenerate form of Kahn's
;; algorithm (but that requires removing edges from the graph, which
;; would mean having e.g. a incoming-edge-count table, which makes
;; find-roots cons a lot. Worth trying at some point, though.
;; (defun topological-sort (adjacencies)
;;   (let* ((roots (find-roots adjacencies))
;;          (sorted '())
;;          (nodes roots)
;;          (next-nodes '()))
;;     (loop while nodes
;;        do (loop for node in nodes
;;              for deps = (dest-nodes node adjacencies)
;;              for new-deps = (remove-if (lambda (dep)
;;                                                (member dep sorted))
;;                                              deps)
;;              do (setf next-nodes (append new-deps next-nodes))
;;                (push node sorted))
;;          (format *debug-io* "Sorted: ~S~%nodes: ~S~%next: ~S~%"
;;                  sorted
;;                  nodes
;;                  next-nodes)
;;          (setf nodes next-nodes
;;                next-nodes '())
;;        finally (return sorted))))

(define-condition cyclic-graph-error (error)
  ((node :initarg :node :accessor node)
   (path :initarg :path :accessor path))
  (:report (lambda (c s)
             (format s "Graph has a cycle: ~{~S~^ ->~} -> ~S"
                     (path c)
                     (node c)))))

(defun find-tail (elem list &rest args &key &allow-other-keys)
  (let ((pos (apply #'position elem list args)))
    (if pos
        (nthcdr pos list)
        nil)))

(defun find-tail-if (predicate list &rest args &key &allow-other-keys)
  (let ((pos (apply #'position-if predicate list args)))
    (if pos
        (nthcdr pos list)
        nil)))

(defun topological-sort (adjacencies)
  (let* ((roots (find-roots adjacencies))
         (stack roots)
         (sorted '()))
    (loop while stack
       for node = (first stack)
       for new-children = (set-difference (dest-nodes node adjacencies)
                                          sorted)
       for cycle-node = (find-if (lambda (n) (member n stack))
                                 new-children)
       if (endp new-children)
       do (push (pop stack) sorted)
       else
       do (when cycle-node
            (let ((cycle-path (find-tail cycle-node (reverse stack))))
              (error 'cyclic-graph-error
                     :node cycle-node
                     :path cycle-path)))
         (setf stack (append new-children stack))
       finally (return (nreverse sorted)))))
