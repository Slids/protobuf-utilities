(defpackage #:protobuf-utilities
  (:use #:cl)
  (:export #:serialize-proto-to-base64-string
           #:deserialize-proto-from-base64-string
           #:with-deserialized-protos))

(in-package :protobuf-utilities)

(defun deserialize-proto-from-base64-string (proto-type proto-string)
  "Given a string containing a proto PROTO-STRING and
it's desired type PROTO-TYPE deserialize the proto."
  (declare (type string proto-string))
  (let ((bytes (flexi-streams:string-to-octets
                (cl-base64:base64-string-to-string proto-string))))
    (cl-protobufs:deserialize-object-from-bytes
     proto-type bytes)))

(defun serialize-proto-to-base64-string (proto)
  "Serialize a PROTO to octets and call base64 encode"
  (cl-base64:string-to-base64-string
   (flexi-streams:octets-to-string
    (cl-protobufs:serialize-object-to-bytes proto))))

(defmacro with-deserialized-protos (arg-arg-type-list &body body)
  "Take a list (ARGUMENT . PROTO-TYPE) arg-arg-type-list that
will exist in the enclosing scope and create a scope where
the bound paramaters will be deserialized. Expects
body to return a proto that it will then serialize and call
base64 encode on."
  (let ((result-proto (gensym "RESULT-PROTO")))
    `(let ,(loop for (arg . arg-type) in  arg-arg-type-list
                 collect
                 `(,arg (deserialize-proto-from-base64-string
                         ,arg-type
                         (or ,arg ""))))
       (let ((,result-proto ,@body))
         (serialize-proto-to-base64-string ,result-proto)))))
