(asdf:defsystem #:org-templater
  :description "Minimalist templated org file library"
  :author      "Lukáš Hozda"
  :license     "COLL"
  :version     "0.1.0"
  :serial      t
  :components  ((:module "source"
                 :serial t
                 :components ((:file "main.lisp")))))
