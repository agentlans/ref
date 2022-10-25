(defsystem "ref"
    :description "Easily access arrays, lists, hash tables, and objects"
    :version "0.0.1"
    :author "Alan Tseng"
    :licence "MIT"
    :depends-on ("alexandria")
    :serial t
    :components ((:file "package")
		 (:file "xref")
		 (:file "with-ref")))
