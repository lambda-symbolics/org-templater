(asdf:defsystem #:org-templater
  :description "Minimalist templated org file library"
  :author      "Lukáš Hozda"
  :license     "COLL"
  :version     "0.1.0"
  :serial      t
  :components  ((:module "source"
                 :serial t
                 :components ((:file "main"))))
  :in-order-to ((asdf:test-op (asdf:test-op #:org-templater/tests))))

(asdf:defsystem #:org-templater/tests
  :depends-on (#:org-templater)
  :pathname "tests"
  :components ((:file "render"))
  :perform (asdf:test-op (op system)
             (declare (ignore op system))
             (uiop:symbol-call :org-templater-test :run)))
