(defpackage #:org-templater
  (:use #:cl)
  (:export #:render))
(in-package #:org-templater)

(defconstant +whitespace+
  '(#\Space #\Tab #\Return #\Newline))

(defun whitespace-char-p (char)
  (member char +whitespace+ :test #'char=))

(defun trim (string)
  (string-trim +whitespace+ string))

(defun trim-left (string)
  (string-left-trim +whitespace+ string))

(defun string-prefix-p (prefix string)
  (and (<= (length prefix) (length string))
       (string-equal prefix string :end2 (length prefix))))

(defun split-lines (string)
  (let ((lines  '())
        (start  0)
        (length (length string)))
    (loop for end = (position #\Newline string :start start)
          while end
          do (push (subseq string start end) lines)
             (setf start (1+ end)))
    (when (< start length)
      (push (subseq string start) lines))
    (values (nreverse lines)
            (and (plusp length)
                 (char= (char string (1- length)) #\Newline)))))

(defun join-lines (lines trailing-newline-p)
  (if (null lines)
      ""
      (with-output-to-string (out)
        (loop for line in lines
              for firstp = t then nil
              do (unless firstp
                   (write-char #\Newline out))
                 (write-string line out))
        (when trailing-newline-p
          (write-char #\Newline out)))))

(defun parse-block-directive (line prefix)
  (let ((line (trim-left line)))
    (when (string-prefix-p prefix line)
      (let* ((start (length prefix))
             (end (or (position-if #'whitespace-char-p line :start start)
                      (length line))))
        (when (> end start)
          (values (string-downcase (subseq line start end))
                  (trim (subseq line end))
                  t))))))

(defun parse-condition (text)
  (let* ((text       (trim text))
         (split      (position-if #'whitespace-char-p text))
         (key-text   (if split (subseq text 0 split) text))
         (value-text (and split (trim (subseq text split)))))
    (unless (and (> (length key-text) 1)
                 (char= (char key-text 0) #\:))
      (error "Invalid condition ~S." text))
    (values (intern (string-upcase (subseq key-text 1)) "KEYWORD")
            value-text
            (and value-text (plusp (length value-text))))))

(defun condition-true-p (text config)
  (multiple-value-bind (key expected expectedp)
      (parse-condition text)
    (multiple-value-bind (value presentp)
        (gethash key config)
      (and presentp
           (if expectedp
               (string-equal (princ-to-string value) expected)
               value)))))

(defun process-when-blocks (lines config)
  (let ((out          (make-array 16 :adjustable t :fill-pointer 0))
        (active-stack '())
        (opaque-block nil))
    (labels ((activep ()
               (if active-stack (car active-stack) t))
             (emit (line)
               (vector-push-extend line out)))
      (dolist (line lines)
        (if opaque-block
            (progn
              (when (activep)
                (emit line))
              (multiple-value-bind (name args foundp)
                  (parse-block-directive line "#+end_")
                (declare (ignore args))
                (when (and foundp (string-equal name opaque-block))
                  (setf opaque-block nil))))
            (multiple-value-bind (name args beginp)
                (parse-block-directive line "#+begin_")
              (cond
                (beginp
                 (if (string-equal name "when")
                     (push (and (activep)
                                (condition-true-p args config))
                           active-stack)
                     (progn
                       (when (activep)
                         (emit line))
                       (setf opaque-block name))))
                (t
                 (multiple-value-bind (end-name end-args endp)
                     (parse-block-directive line "#+end_")
                   (declare (ignore end-args))
                   (if (and endp (string-equal end-name "when"))
                       (if active-stack
                           (pop active-stack)
                           (error "Unmatched #+end_when."))
                       (when (activep)
                         (emit line)))))))))
      (when active-stack
        (error "Unclosed #+begin_when block."))
      (coerce out 'list))))

(defun heading-level (line)
  (let ((length (length line))
        (level  0))
    (loop while (and (< level length)
                     (char= (char line level) #\*))
          do (incf level))
    (when (and (plusp level)
               (< level length)
               (member (char line level) '(#\Space #\Tab) :test #'char=))
      level)))

(defun heading-levels (lines)
  (let* ((length       (length lines))
         (levels       (make-array length :initial-element nil))
         (opaque-block nil))
    (dotimes (i length levels)
      (let ((line (aref lines i)))
        (if opaque-block
            (multiple-value-bind (name args foundp)
                (parse-block-directive line "#+end_")
              (declare (ignore args))
              (when (and foundp (string-equal name opaque-block))
                (setf opaque-block nil)))
            (multiple-value-bind (name args beginp)
                (parse-block-directive line "#+begin_")
              (declare (ignore args))
              (if beginp
                  (setf opaque-block name)
                  (setf (aref levels i) (heading-level line)))))))))

(defun property-drawer-range (lines heading-index)
  (let ((start  (1+ heading-index))
        (length (length lines)))
    (when (and (< start length)
               (string-equal (trim (aref lines start)) ":PROPERTIES:"))
      (loop for end from (1+ start) below length
            when (string-equal (trim (aref lines end)) ":END:")
              do (return-from property-drawer-range
                   (values start end t)))
      (error "Unclosed property drawer under heading ~S."
             (aref lines heading-index)))))

(defun when-property (line)
  (let ((line (trim line)))
    (when (and (> (length line) 2)
               (char= (char line 0) #\:))
      (let ((end (position #\: line :start 1)))
        (when (and end
                   (string-equal (subseq line 1 end) "WHEN"))
          (values (trim (subseq line (1+ end))) t))))))

(defun drawer-condition (lines start end)
  (let ((condition   nil)
        (foundp      nil)
        (other-lines '()))
    (loop for i from (1+ start) below end
          for line = (aref lines i)
          do (multiple-value-bind (value whenp)
                 (when-property line)
               (if whenp
                   (if foundp
                       (error "Duplicate :WHEN: property.")
                       (setf condition value
                             foundp t))
                   (push line other-lines))))
    (values condition foundp (nreverse other-lines))))

(defun subtree-end (levels start level)
  (loop for i from (1+ start) below (length levels)
        for next-level = (aref levels i)
        when (and next-level (<= next-level level))
          do (return i)
        finally (return (length levels))))

(defun process-heading-whens (lines config)
  (let* ((lines  (coerce lines 'vector))
         (levels (heading-levels lines))
         (out    (make-array 16 :adjustable t :fill-pointer 0))
         (i      0))
    (labels ((emit (line)
               (vector-push-extend line out)))
      (loop while (< i (length lines))
            for level = (aref levels i)
            do (if (null level)
                   (progn
                     (emit (aref lines i))
                     (incf i))
                   (multiple-value-bind (drawer-start drawer-end drawer-p)
                       (property-drawer-range lines i)
                     (if (not drawer-p)
                         (progn
                           (emit (aref lines i))
                           (incf i))
                         (multiple-value-bind (condition conditionp other-lines)
                             (drawer-condition lines drawer-start drawer-end)
                           (cond
                             ((not conditionp)
                              (emit (aref lines i))
                              (incf i))
                             ((not (condition-true-p condition config))
                              (setf i (%subtree-end levels i level)))
                             (t
                              (emit (aref lines i))
                              (when (some (lambda (line)
                                            (plusp (length (trim line))))
                                          other-lines)
                                (emit (aref lines drawer-start))
                                (dolist (line other-lines)
                                  (emit line))
                                (emit (aref lines drawer-end)))
                              (setf i (1+ drawer-end)))))))))
      (coerce out 'list))))

(defun placeholder-key (text)
  (let ((text (trim text)))
    (unless (and (> (length text) 1)
                 (char= (char text 0) #\:)
                 (not (position-if #'%whitespace-char-p text)))
      (error "Invalid placeholder ~S." text))
    (intern (string-upcase (subseq text 1)) "KEYWORD")))

(defun interpolate (string config)
  (with-output-to-string (out)
    (loop with position = 0
          for start = (search "{{{" string :start2 position)
          do (if (null start)
                 (progn
                   (write-string string out :start position)
                   (return))
                 (progn
                   (write-string string out :start position :end start)
                   (let ((end (search "}}}" string :start2 (+ start 3))))
                     (unless end
                       (error "Unclosed placeholder starting at position ~D." start))
                     (let ((value (gethash (placeholder-key
                                            (subseq string (+ start 3) end))
                                           config)))
                       (when value
                         (princ value out)))
                     (setf position (+ end 3))))))))

(defun read-file-string (path)
  (with-open-file (in path :direction :input :element-type 'character)
    (with-output-to-string (out)
      (loop for char = (read-char in nil nil)
            while char
            do (write-char char out)))))

(defun render (&key template template-path
                 (config (make-hash-table :test #'eq)))
  (when (eql (null template) (null template-path))
    (error "Supply exactly one of :TEMPLATE or :TEMPLATE-PATH."))
  (unless (hash-table-p config)
    (error ":CONFIG must be a hash table."))
  (let ((source (if template
                    template
                    (read-file-string template-path))))
    (unless (stringp source)
      (error ":TEMPLATE must be a string."))
    (multiple-value-bind (lines trailing-newline-p)
        (split-lines source)
      (interpolate
       (join-lines
        (process-heading-whens
         (process-when-blocks lines config)
         config)
        trailing-newline-p)
       config))))
