(defpackage #:protobuf-utilities
  (:use #:cl)
  (:export #:serialize-proto-to-base64-string
           #:deserialize-proto-from-base64-string
           #:with-deserialized-protos))

(in-package :protobuf-utilities)

(defun deserialize-proto-from-base64-string (proto-type proto-string)
  "Deserialize a proto of type PROTO-TYPE from the base-64
encoded string PROTO-STRING."
  (declare (type string proto-string))
  (let ((bytes (flexi-streams:string-to-octets
                (cl-base64:base64-string-to-string proto-string))))
    (cl-protobufs:deserialize-from-bytes
     proto-type bytes)))

(defun serialize-proto-to-base64-string (proto)
  "Serialize a PROTO to octets and call base64 encode"
  (cl-base64:string-to-base64-string
   (flexi-streams:octets-to-string
    (cl-protobufs:serialize-to-bytes proto))))

(defmacro with-deserialized-protos (message-message-type-list &body body)
  "Take a list (MESSAGE . PROTO-TYPE) MESSAGE-MESSAGE-TYPE-LIST that
will exist in the enclosing scope and create a scope where
the bound paramaters will be deserialized. Expects
body to return a proto that it will then serialize and call
base64 encode on."
  (let ((result-proto (gensym "RESULT-PROTO")))
    `(let ,(loop for (message . message-type) in  message-message-type-list
                 collect
                 `(,message (deserialize-proto-from-base64-string
                             ,message-type
                             (or ,message ""))))
       (let ((,result-proto ,@body))
         (serialize-proto-to-base64-string ,result-proto)))))
