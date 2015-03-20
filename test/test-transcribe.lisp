(in-package :mgl-pax-test)

(defun test-read-prefixed-lines ()
  (assert
   (equal (multiple-value-list
           (with-input-from-string (stream (format nil ">1"))
             (mgl-pax::read-prefixed-lines stream ">")))
          '("1" 1 nil t 2)))
  (assert
   (equal (multiple-value-list
           (with-input-from-string (stream (format nil ">1~%"))
             (mgl-pax::read-prefixed-lines stream ">")))
          '("1" 1 nil t 3)))
  (assert
   (equal (multiple-value-list
           (with-input-from-string (stream (format nil ">1~%>"))
             (mgl-pax::read-prefixed-lines stream ">")))
          `(,(format nil "1~%") 2 nil t 4)))
  (assert
   (equal (multiple-value-list
           (with-input-from-string (stream (format nil ">1~%>2~%> 3"))
             (mgl-pax::read-prefixed-lines stream ">")))
          `(,(format nil "1~%2~%3") 3 nil t 9)))
  (assert
   (equal (multiple-value-list
           (with-input-from-string (stream (format nil ">1~%>2~%> 3~%xy~%"))
             (mgl-pax::read-prefixed-lines stream ">")))
          `(,(format nil "1~%2~%3") 3 "xy" nil 10)))
  (assert
   (equal (multiple-value-list
           (with-input-from-string (stream (format nil ">1~%>2~%> 3~%xy"))
             (mgl-pax::read-prefixed-lines stream ">")))
          `(,(format nil "1~%2~%3") 3 "xy" t 10)))
  (assert
   (equal (multiple-value-list
           (with-input-from-string (stream (format nil ">1"))
             (mgl-pax::read-prefixed-lines stream ">" :first-line-prefix "")))
          '(">1" 1 nil t 2)))
  (assert
   (equal (multiple-value-list
           (with-input-from-string (stream (format nil ">1~%>2~%> 3"))
             (mgl-pax::read-prefixed-lines stream ">" :first-line-prefix "")))
          `(,(format nil ">1~%2~%3") 3 nil t 9)))
  (assert
   (equal (multiple-value-list
           (with-input-from-string (stream (format nil ">1~%>2~%> 3~%xy~%"))
             (mgl-pax::read-prefixed-lines stream ">" :first-line-prefix "")))
          `(,(format nil ">1~%2~%3") 3 "xy" nil 10)))
  (assert
   (equal (multiple-value-list
           (with-input-from-string (stream (format nil ">1~%>2~%> 3~%xy"))
             (mgl-pax::read-prefixed-lines stream ">" :first-line-prefix "")))
          `(,(format nil ">1~%2~%3") 3 "xy" t 10))))

(defclass bbb ()
  ())

(defmethod print-object ((bbb bbb) stream)
  (print-unreadable-object (bbb stream :type t)))

(defclass bbb* ()
  ())

(defmethod print-object ((bbb bbb*) stream)
  (print-unreadable-object (bbb stream :type t)
    (format stream "~%")))

(defparameter *transcribe-test-cases*
  '((:input "1"
     :transcript (((1 "1")))
     :output "1~%"
     :update-only t)
    (:input ";; this~%   (+ 1~%      2)"
     :transcript ((((+ 1 2) ";; this~%   (+ 1~%      2)")))
     :output ";; this~%   (+ 1~%      2)~%"
     :update-only t)
    (:input "(+ 1 2)42"
     :transcript ((((+ 1 2) "(+ 1 2)"))
                  ((42 "42")))
     :output "(+ 1 2)~%42~%"
     :update-only t)
    (:input "(+ 1 2) 42"
     :transcript ((((+ 1 2) "(+ 1 2)"))
                  ((42 "42")))
     :output "(+ 1 2)~%42~%"
     :update-only t)
    (:input "(princ 1)~%.. 1"
     :transcript ((((princ 1) "(princ 1)") (:output "1")))
     :output "(princ 1)~%.. 1~%"
     :update-only t)
    (:input "(progn (princ 1) (print 2))~%.. 1~%.. 2 "
     :transcript ((((progn (princ 1) (print 2)) "(progn (princ 1) (print 2))")
                   (:output "1~%2 ")))
     :output "(progn (princ 1) (print 2))~%.. 1~%.. 2 ~%"
     :update-only t)
    (:input "(princ 1)~%;.. 1"
     :transcript ((((princ 1) "(princ 1)") (:commented-output "1")))
     :output "(princ 1)~%;.. 1~%"
     :update-only t)
    (:input "(progn (princ 1) (print 2))~%;.. 1~%;.. 2 "
     :transcript ((((progn (princ 1) (print 2)) "(progn (princ 1) (print 2))")
                   (:commented-output "1~%2 ")))
     :output "(progn (princ 1) (print 2))~%;.. 1~%;.. 2 ~%"
     :update-only t)
    (:input "1~%=> 1"
     :transcript (((1 "1") (:readable (1 "1"))))
     :output "1~%=> 1~%"
     :update-only t)
    (:input "(list 1 2)~%=> ;; this~%   (1~%    2)"
     :transcript ((((list 1 2) "(list 1 2)")
                   (:readable ((1 2) ";; this~%   (1~%    2)"))))
     :output "(list 1 2)~%=> ;; this~%   (1~%    2)~%"
     :update-only t)
    (:input "(values)~%=> ; No value"
     :transcript ((((values) "(values)") (:no-value nil)))
     :output "(values)~%=> ; No value~%"
     :update-only t)
    (:input "1~%;=> 1"
     :transcript (((1 "1") (:commented-readable (1 "1"))))
     :output "1~%;=> 1~%"
     :update-only t)
    (:input "(list 1 2)~%;=> ;; this~%;-> (1~%;->  2)"
     :transcript ((((list 1 2) "(list 1 2)")
                   (:commented-readable ((1 2) ";; this~%(1~% 2)"))))
     :output "(list 1 2)~%;=> ;; this~%;-> (1~%;->  2)~%"
     :update-only t)
    (:input "(make-instance 'bbb)~%==> #<BBB >"
     :transcript ((((make-instance 'bbb) "(make-instance 'bbb)")
                   (:unreadable "#<BBB >")))
     :output "(make-instance 'bbb)~%==> #<BBB >~%"
     :update-only t)
    (:input "(make-instance 'bbb*)~%==> #<BBB* ~%--> >"
     :transcript ((((make-instance 'bbb*) "(make-instance 'bbb*)")
                   (:unreadable "#<BBB* ~%>")))
     :output "(make-instance 'bbb*)~%==> #<BBB* ~%--> >~%"
     :update-only t)
    (:input "(make-instance 'bbb)~%;==> #<BBB >"
     :transcript ((((make-instance 'bbb) "(make-instance 'bbb)")
                   (:commented-unreadable "#<BBB >")))
     :output "(make-instance 'bbb)~%;==> #<BBB >~%"
     :update-only t)
    (:input "(make-instance 'bbb*)~%;==> #<BBB* ~%;--> >"
     :transcript ((((make-instance 'bbb*) "(make-instance 'bbb*)")
                   (:commented-unreadable "#<BBB* ~%>")))
     :output "(make-instance 'bbb*)~%;==> #<BBB* ~%;--> >~%"
     :update-only t)
    (:input "42"
     :output "42~%=> 42~%")
    (:input "42~%"
     :output "42~%=> 42~%")
    ;; The spaces are discarded.
    (:input "42  ~%"
     :output "42~%=> 42~%")
    ;; No preceeding "==>" error.
    (:input "42~%-->"
     :output ""
     :errors (3))
    ;; No-value marker after values.
    (:input "42~%=> 42~%=> ; No value"
     :output ""
     :errors (9))
    ;; No whitespace after form.
    (:input "42;comment"
     :output "42~%=> 42~%;comment")
    ;; Unexpected eof on non-empty value line.
    (:input "42~%=> ;"
     :output ""
     :errors (3))
    ;; Multiple forms on the same line.
    (:input "(list)(values 1 2)"
     :output "(list)~%=> NIL~%(values 1 2)~%=> 1~%=> 2~%")
    ;; Multiple forms on the same line with space.
    (:input "(list) (values 1 2)"
     :output "(list)~%=> NIL~%(values 1 2)~%=> 1~%=> 2~%")
    ;; No value included.
    (:input "(values)"
     :output "(values)~%=> ; No value~%"
     :include-no-value t)
    ;; No value not included.
    (:input "(values)"
     :output "(values)~%")
    ;; No output included.
    (:input "(values)"
     :output "(values)~%..~%"
     :include-no-output t)
    ;; No output not included.
    (:input "(values)"
     :output "(values)~%")
    ;; Capturing *ERROR-OUTPUT*.
    (:input "(princ 42 *error-output*)"
     :output "(princ 42 *error-output*)~%.. 42~%=> 42~%")
    ;; Capturing *TRACE-OUTPUT*.
    (:input "(princ 42 *trace-output*)"
     :output "(princ 42 *trace-output*)~%.. 42~%=> 42~%")
    ;; Capturing *DEBUG-IO*.
    (:input "(princ 42 *debug-io*)"
     :output "(princ 42 *debug-io*)~%.. 42~%=> 42~%")
    ;; Capturing *QUERY-IO*.
    (:input "(princ 42 *query-io*)"
     :output "(princ 42 *query-io*)~%.. 42~%=> 42~%")
    ;; Capturing *TERMINAL-IO*.
    (:input "(princ 42 *terminal-io*)"
     :output "(princ 42 *terminal-io*)~%.. 42~%=> 42~%")
    ;; output inconsistency
    (:input "(princ 42)~%.. 7~%=> 42~%"
     :output "(princ 42)~%.. 42~%=> 42~%"
     :check-consistency t :output-consistency-errors (nil))
    ;; readable value inconsistency
    (:input "(princ 42)~%.. 42~%=> 7~%"
     :output "(princ 42)~%.. 42~%=> 42~%"
     :check-consistency t
     :values-consistency-errors (nil))
    ;; unreadable value inconsistency
    (:input "(make-instance 'bbb)~%==> #<CCC >~%"
     :output "(make-instance 'bbb)~%==> #<BBB >~%"
     :check-consistency t :values-consistency-errors (nil))
    ;; commenting of new values with update 1
    (:input "(values 1 2)~%=> 1"
     :output "(values 1 2)~%=> 1~%=> 2~%"
     :update-only t)
    ;; commenting of new values with update 2
    (:input "(values 1 2)~%;=> 1"
     :output "(values 1 2)~%;=> 1~%;=> 2~%"
     :update-only t)
    ;; commenting of new output with :KEEP
    (:input "(princ 1)~%;=> 1"
     :output "(princ 1)~%;.. 1~%;=> 1~%"
     :comment :keep)
    ;; eof in form
    (:input "(sdf"
     :errors (0))
    ;; eof in readable value 1
    (:input "(list 1 2)~%=> (1"
     :errors (11))
    ;; eof in commented readable value 1
    (:input "(list 1 2)~%;=> (1~%;-> 2"
     :errors (11))
    ;; missing readable value
    (:input "(list 1 2)~%=>"
     :output "(list 1 2)~%=> (1 2)~%")
    ;; missing commented readable value
    (:input "(list 1 2)~%;=>"
     :output "(list 1 2)~%;=> (1 2)~%")))

(defun call-format-on-strings (tree)
  (mgl-pax::transform-tree (lambda (parent node)
                             (declare (ignore parent))
                             (if (stringp node)
                                 (values (format nil node) nil nil)
                                 (values node t nil)))
                           tree))

(defun test-read-write-transcript ()
  (let ((*package* (find-package :mgl-pax-test)))
    (loop for test-case in *transcribe-test-cases* do
      (format t "test case: ~S~%" test-case)
      (destructuring-bind (&key input transcript output check-consistency
                           update-only (include-no-output update-only)
                           (include-no-value update-only)
                           (comment :keep)
                           errors output-consistency-errors
                           values-consistency-errors)
          test-case
        (let ((output-consistency-errors* ())
              (values-consistency-errors* ())
              (errors* ()))
          (catch 'here
            (handler-bind
                ((transcription-output-consistency-error
                   (lambda (e)
                     (push (mgl-pax::transcription-error-file-position e)
                           output-consistency-errors*)
                     (continue)))
                 (transcription-values-consistency-error
                   (lambda (e)
                     (push (mgl-pax::transcription-error-file-position e)
                           values-consistency-errors*)
                     (continue)))
                 (transcription-error
                   (lambda (e)
                     (push (mgl-pax::transcription-error-file-position e)
                           errors*)
                     (throw 'here nil))))
              (let* ((input (format nil input))
                     (output (when output (format nil output)))
                     (transcript (call-format-on-strings transcript))
                     (transcript* (mgl-pax::read-transcript input))
                     (output*
                       (mgl-pax::write-transcript
                        transcript* nil
                        :check-consistency check-consistency
                        :update-only update-only
                        :include-no-output include-no-output
                        :include-no-value include-no-value
                        :comment comment)))
                (when transcript
                  (assert (equal transcript transcript*)))
                (when output
                  (assert (equal output output*))))))
          (assert (equal (reverse errors*) errors))
          (assert (equal (reverse output-consistency-errors*)
                         output-consistency-errors))
          (assert (equal (reverse values-consistency-errors*)
                         values-consistency-errors)))))))

(defparameter *transcribe-source-file*
  (asdf:system-relative-pathname
   :mgl-pax "test/data/baseline/transcribe-source.lisp"))

(defparameter *transcribe-transcription-file*
  (asdf:system-relative-pathname
   :mgl-pax "test/data/baseline/transcribe-transcription.lisp"))

(defun test-transcribe-from-source ()
  (check-transcription *transcribe-source-file*
                       *transcribe-transcription-file*
                       :check-consistency nil))

;;; Check that repeated transcription produces the same results.
(defun test-transcribe-stability ()
  (check-transcription *transcribe-transcription-file*
                       *transcribe-transcription-file*
                       :check-consistency t))

(defun check-transcription (source-file transcription-file
                            &key check-consistency)
  (unless (string= (alexandria:read-file-into-string transcription-file)
                   (with-output-to-string (transcription)
                     (with-open-file (source source-file)
                       (transcribe source transcription :update-only t
                                   :check-consistency check-consistency))))
    (cerror "Update transcription file." "Transcription differs from ~S."
            transcription-file)
    (with-input-from-string (source (alexandria:read-file-into-string
                                     source-file))
      (with-open-file (transcription transcription-file :direction :output
                                     :if-exists :rename-and-delete)
        (transcribe source transcription :update-only t
                    :check-consistency check-consistency)))))

(defun test-transcribe ()
  (test-read-prefixed-lines)
  (test-read-write-transcript)
  (test-transcribe-from-source)
  (test-transcribe-stability))
