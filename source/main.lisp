(defpackage #:org-templater
  (:use #:cl)
  (:export #:render
           #:template-error
           #:when-condition-error
           #:when-condition-error-text
           #:when-block-end-error
           #:when-block-end-error-line
           #:when-block-unclosed-error
           #:when-block-unclosed-error-depth
           #:property-drawer-unclosed-error
           #:property-drawer-unclosed-error-heading
           #:placeholder-invalid-error
           #:placeholder-invalid-error-text
           #:placeholder-unclosed-error
           #:placeholder-unclosed-error-position
           #:render-source-error
           #:render-source-error-template
           #:render-source-error-template-path
           #:render-argument-type-error
           #:render-argument-type-error-argument))
(in-package #:org-templater)

(defparameter +whitespace+
  '(#\Space #\Tab #\Return #\Newline)
  "The characters that count as whitespace in templates.")

(defun whitespace-char-p (char)
  "Return true when CHAR is a space, tab, carriage return, or newline."
  (member char +whitespace+ :test #'char=))

(defun trim (string)
  "Return STRING without leading and trailing whitespace."
  (string-trim +whitespace+ string))

(defun trim-left (string)
  "Return STRING without leading whitespace."
  (string-left-trim +whitespace+ string))

(defun string-prefix-p (prefix string)
  "Return true when STRING starts with PREFIX, ignoring case."
  (and (<= (length prefix) (length string))
       (string-equal prefix string :end2 (length prefix))))

(defun split-lines (string)
  "Split STRING at its newlines and return the list of lines.
Return as a second value whether STRING ends with a newline."
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
  "Join LINES with newlines into one string.
End it with a newline when TRAILING-NEWLINE-P is true."
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
  "Parse LINE as an Org block line that starts with PREFIX, such as #+begin_.
Return the block name in lower case, the trimmed rest of the line, and true.
Return nil when LINE is not such a line."
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
  "Parse TEXT as a when condition, a keyword with an optional value.
Return the keyword, the value text, and whether TEXT has a value."
  (let* ((text       (trim text))
         (split      (position-if #'whitespace-char-p text))
         (key-text   (if split (subseq text 0 split) text))
         (value-text (and split (trim (subseq text split)))))
    (unless (and (> (length key-text) 1)
                 (char= (char key-text 0) #\:))
      (error 'when-condition-error :text text))
    (values (intern (string-upcase (subseq key-text 1)) "KEYWORD")
            value-text
            (and value-text (plusp (length value-text))))))

(defun condition-true-p (text config)
  "Return true when the condition TEXT holds in CONFIG.
A bare key holds when its value is true. A key with a value holds when the
printed CONFIG value equals that value, ignoring case."
  (multiple-value-bind (key expected expectedp)
      (parse-condition text)
    (multiple-value-bind (value presentp)
        (gethash key config)
      (and presentp
           (if expectedp
               (string-equal (princ-to-string value) expected)
               value)))))

(defun process-when-blocks (lines config)
  "Return LINES without the #+begin_when blocks whose conditions fail in CONFIG.
The begin and end lines of every when block go too. Lines inside other
blocks pass through unchanged."
  (let ((out          (make-array 16 :adjustable t :fill-pointer 0))
        (active-stack '())
        (opaque-block nil))
    (labels ((activep ()
               (if active-stack (first active-stack) t))
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
                           (error 'when-block-end-error :line line))
                       (when (activep)
                         (emit line)))))))))
      (when active-stack
        (error 'when-block-unclosed-error :depth (length active-stack)))
      (coerce out 'list))))

(defun heading-level (line)
  "Return the number of stars that start the Org heading LINE.
Return nil when LINE is not a heading."
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
  "Return a vector of the heading level of each line in the vector LINES.
Lines that are not headings, and lines inside blocks, get nil."
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
  "Find the property drawer right below the heading at HEADING-INDEX in LINES.
Return the indexes of its PROPERTIES and END lines, and true. Return nil
when the heading has no drawer."
  (let ((start  (1+ heading-index))
        (length (length lines)))
    (when (and (< start length)
               (string-equal (trim (aref lines start)) ":PROPERTIES:"))
      (loop for end from (1+ start) below length
            when (string-equal (trim (aref lines end)) ":END:")
              do (return-from property-drawer-range
                   (values start end t)))
      (error 'property-drawer-unclosed-error
             :heading (aref lines heading-index)))))

(defun when-property (line)
  "Parse LINE as a WHEN property line.
Return the property value and true, or nil for any other line."
  (let ((line (trim line)))
    (when (and (> (length line) 2)
               (char= (char line 0) #\:))
      (let ((end (position #\: line :start 1)))
        (when (and end
                   (string-equal (subseq line 1 end) "WHEN"))
          (values (trim (subseq line (1+ end))) t))))))

(defun drawer-condition (lines start end)
  "Read the property drawer between the lines START and END of LINES.
Return the WHEN condition text, whether the drawer has one, and the list of
its other lines."
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
  "Return the index in LEVELS where the subtree of the heading at START ends.
That is the next heading of LEVEL or lower, or the length of LEVELS."
  (loop for i from (1+ start) below (length levels)
        for next-level = (aref levels i)
        when (and next-level (<= next-level level))
          do (return i)
        finally (return (length levels))))

(defun process-heading-whens (lines config)
  "Return LINES without the subtrees whose WHEN property fails in CONFIG.
Kept headings lose the WHEN property, and lose their drawer when nothing
else is in it."
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
                              (setf i (subtree-end levels i level)))
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
  "Return the keyword that the placeholder TEXT names."
  (let ((text (trim text)))
    (unless (and (> (length text) 1)
                 (char= (char text 0) #\:)
                 (not (position-if #'whitespace-char-p text)))
      (error 'placeholder-invalid-error :text text))
    (intern (string-upcase (subseq text 1)) "KEYWORD")))

(defun interpolate (string config)
  "Replace each {{{:key}}} placeholder in STRING with its value in CONFIG.
Keys that are missing or nil become empty text."
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
                       (error 'placeholder-unclosed-error :position start))
                     (let ((value (gethash (placeholder-key
                                            (subseq string (+ start 3) end))
                                           config)))
                       (when value
                         (princ value out)))
                     (setf position (+ end 3))))))))

(defun read-file-string (path)
  "Return the contents of the file at PATH as a string."
  (with-open-file (in path :direction :input :element-type 'character)
    (with-output-to-string (out)
      (loop for char = (read-char in nil nil)
            while char
            do (write-char char out)))))

(defun render (&key template template-path
                 (config (make-hash-table :test #'eq)))
  "Render an Org template and return the result as a string.
Take the template from the string TEMPLATE or from the file at TEMPLATE-PATH,
and supply exactly one of them. CONFIG is a hash table from keywords to
values. Drop the #+begin_when blocks and the subtrees with a WHEN property
whose conditions fail, then fill in the {{{:key}}} placeholders."
  (when (eql (null template) (null template-path))
    (error 'render-source-error :template      template
                                :template-path template-path))
  (unless (hash-table-p config)
    (error 'render-argument-type-error :argument      ':config
                                       :datum         config
                                       :expected-type 'hash-table))
  (let ((source (if template
                    template
                    (read-file-string template-path))))
    (unless (stringp source)
      (error 'render-argument-type-error :argument      ':template
                                         :datum         source
                                         :expected-type 'string))
    (multiple-value-bind (lines trailing-newline-p)
        (split-lines source)
      (interpolate
       (join-lines
        (process-heading-whens
         (process-when-blocks lines config)
         config)
        trailing-newline-p)
       config))))

;;;; -- Conditions --

(define-condition template-error (error)
  ()
  (:documentation "The parent of every condition a malformed template signals."))

(define-condition when-condition-error (template-error)
  ((text :initarg       :text
         :reader        when-condition-error-text
         :documentation "The condition text that does not start with a keyword."))
  (:report (lambda (condition stream)
             (format stream "Invalid condition ~S."
                     (when-condition-error-text condition))))
  (:documentation "Signalled when a when condition does not name a keyword."))

(define-condition when-block-end-error (template-error)
  ((line :initarg       :line
         :reader        when-block-end-error-line
         :documentation "The #+end_when line with no open block."))
  (:report (lambda (condition stream)
             (format stream "Unmatched #+end_when line ~S."
                     (when-block-end-error-line condition))))
  (:documentation "Signalled when an #+end_when line has no open #+begin_when block."))

(define-condition when-block-unclosed-error (template-error)
  ((depth :initarg       :depth
          :reader        when-block-unclosed-error-depth
          :documentation "How many #+begin_when blocks are still open at the end."))
  (:report (lambda (condition stream)
             (format stream "~D unclosed #+begin_when block~:P."
                     (when-block-unclosed-error-depth condition))))
  (:documentation "Signalled when the template ends inside a #+begin_when block."))

(define-condition property-drawer-unclosed-error (template-error)
  ((heading :initarg       :heading
            :reader        property-drawer-unclosed-error-heading
            :documentation "The heading line that owns the drawer."))
  (:report (lambda (condition stream)
             (format stream "Unclosed property drawer under heading ~S."
                     (property-drawer-unclosed-error-heading condition))))
  (:documentation "Signalled when a heading's property drawer has no :END: line."))

(define-condition placeholder-invalid-error (template-error)
  ((text :initarg       :text
         :reader        placeholder-invalid-error-text
         :documentation "The text between the placeholder braces."))
  (:report (lambda (condition stream)
             (format stream "Invalid placeholder ~S."
                     (placeholder-invalid-error-text condition))))
  (:documentation "Signalled when a placeholder does not hold a single keyword."))

(define-condition placeholder-unclosed-error (template-error)
  ((position :initarg       :position
             :reader        placeholder-unclosed-error-position
             :documentation "The index of the opening braces in the template."))
  (:report (lambda (condition stream)
             (format stream "Unclosed placeholder starting at position ~D."
                     (placeholder-unclosed-error-position condition))))
  (:documentation "Signalled when a placeholder has no closing braces."))

(define-condition render-source-error (error)
  ((template      :initarg       :template
                  :reader        render-source-error-template
                  :documentation "The template string passed to render, or nil.")
   (template-path :initarg       :template-path
                  :reader        render-source-error-template-path
                  :documentation "The template path passed to render, or nil."))
  (:report (lambda (condition stream)
             (format stream "Supply exactly one of :TEMPLATE or :TEMPLATE-PATH, got ~S and ~S."
                     (render-source-error-template condition)
                     (render-source-error-template-path condition))))
  (:documentation "Signalled when render gets both a template and a path, or neither."))

(define-condition render-argument-type-error (type-error)
  ((argument :initarg       :argument
             :reader        render-argument-type-error-argument
             :documentation "The keyword that names the render argument."))
  (:report (lambda (condition stream)
             (format stream "~S must be of type ~S, got ~S."
                     (render-argument-type-error-argument condition)
                     (type-error-expected-type condition)
                     (type-error-datum condition))))
  (:documentation "Signalled when a render argument has the wrong type."))
