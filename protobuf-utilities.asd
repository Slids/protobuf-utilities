(defsystem :protobuf-utilities
  :author "Jonathan Godbout"
  :version "0.0.1"
  :licence "MIT-style"
  :description      "Utilities for working with cl-protobufs"
  :long-description "Utilities for working with cl-protobufs"
  :depends-on (:cl-protobufs :flexi-streams :cl-base64)
  :components
  ((:module "src"
    :serial t
    :pathname ""
    :components ((:file "protobuf-utilities")))))
