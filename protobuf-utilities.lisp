(defpackage #:protobuf-utilities
  (:use #:cl)
  (:export #:serialize-proto-to-base64-string
           #:deserialize-proto-from-base64-string
           #:with-deserialized-protos
           #:serialize-result
           #:with-deserialized-protos-serializing-return))

(in-package :protobuf-utilities)

(defun deserialize-proto-from-base64-string (proto-type proto-string)
  "Deserialize a proto of type PROTO-TYPE from the base64-encoded
 string PROTO-STRING."
  (declare (type string proto-string))
  (let ((bytes (flexi-streams:string-to-octets
                (cl-base64:base64-string-to-string proto-string))))
    (cl-protobufs:deserialize-from-bytes
     proto-type bytes)))

(defun serialize-proto-to-base64-string (proto)
  "Serialize a PROTO to octets and call base64-encode"
  (cl-base64:string-to-base64-string
   (flexi-streams:octets-to-string
    (cl-protobufs:serialize-to-bytes proto))))

(defmacro with-deserialized-protos (message-message-type-list &body body)
  "Take a list (MESSAGE . PROTO-TYPE) MESSAGE-MESSAGE-TYPE-LIST
where the message will be a symbol pointing to a base64-encoded
serialized proto in a string. Deserialize the protos and store
them in the message symbols.

The messages are bound lexically so after this macro finishes the
protos return to be serialized base64-encoded strings."
  `(let ,(loop for (message . message-type) in  message-message-type-list
               collect
               `(,message (deserialize-proto-from-base64-string
                           ',message-type
                           (or ,message ""))))
     ,@body))

(defmacro serialize-result (&body body)
  "Serialize the result of calling BODY into a base64-encoded string."
  (let ((result-proto (gensym "RESULT-PROTO")))
    `(let ((,result-proto ,@body))
       (serialize-proto-to-base64-string ,result-proto))))

(defmacro with-deserialized-protos-serializing-return (message-message-type-list &body body)
  "Wrapper combining WITH-DESERIALIZED-PROTOS and serialize result of BODY.

Useful in an http server context where you know your inputs will be protos which
are contained in base64-encoded strings and you will want to return a base64-encoded
proto."
  `(serialize-result (with-deserialized-protos ,message-message-type-list ,@body)))
