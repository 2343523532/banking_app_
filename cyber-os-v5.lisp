;;;; ================================================================
;;;; CYBER-OS v5.0: ACTIVE TRACE, SPA WEB-MATRIX, AI REST API
;;;; ================================================================

(eval-when (:compile-toplevel :load-toplevel :execute)
  (ql:quickload :hunchentoot :silent t))

;;; ------------------------------------------------------------------
;;; PACKAGE 1: SHARED UTILITIES & OS KERNEL
;;; ------------------------------------------------------------------
(defpackage :cyber-utils
  (:use :cl)
  (:export :print-color :normalize :prompt :typewrite :*syslog* :log-event :sleep-print))

(in-package :cyber-utils)

(defparameter *syslog* nil)

(defun print-color (color text)
  (let ((code (cdr (assoc color '((:red . "31") (:green . "32")
                                  (:yellow . "33") (:blue . "34")
                                  (:magenta . "35") (:cyan . "36"))))))
    (if code
        (format nil "~C[~Am~A~C[0m" #\Esc code text #\Esc)
        text)))

(defun typewrite (text &optional (color :green) (delay 0.005))
  (let ((colored-text (print-color color text)))
    (format t "~A~%" colored-text)
    (sleep delay)))

(defun sleep-print (text delay color)
  (format t "~A" (print-color color text))
  (force-output)
  (sleep delay)
  (format t "~%"))

(defun normalize (str)
  (string-downcase (string-trim " " str)))

(defun prompt (text &optional (color :green))
  (format t "~A " (print-color color text))
  (force-output)
  (read-line nil nil ""))

(defun log-event (source level message)
  (let* ((timestamp (get-universal-time))
         (log-entry (list timestamp source level message)))
    (push log-entry *syslog*)
    (let ((color-level (cond ((string= level "CRITICAL") (print-color :red level))
                             ((string= level "WARNING") (print-color :yellow level))
                             ((string= level "INFO") (print-color :green level))
                             (t level))))
      (format t "[~A] [~A] ~A~%" (print-color :cyan source) color-level message))))


;;; ------------------------------------------------------------------
;;; PACKAGE 2: SYNONYMS BANK (AI INTELLIGENCE LAYER)
;;; ------------------------------------------------------------------
(defpackage :synonyms-bank
  (:use :cl :cyber-utils)
  (:export :is-synonym-p :list-groups :get-random-group :search-fuzzy :search-substring))

(in-package :synonyms-bank)

(defparameter *synonym-groups* (make-hash-table :test #'equal))

;; Knowledge Graph Load
(setf (gethash "sentient_ai" *synonym-groups*)
      '("Sentient artificial intelligence" "Artificial sentience" "Conscious AI" "Digital mind" "Synthetic mind"))
(setf (gethash "omniscience" *synonym-groups*)
      '("Omniscient" "All-knowing" "Universal expert" "Polymath" "Erudite" "Encyclopedic knowledge"))
(setf (gethash "stealth" *synonym-groups*)
      '("Undetectable" "Cloaked" "Covert" "Ghosted" "Invisible" "Sub rosa" "Surreptitious"))
(setf (gethash "cybernetics" *synonym-groups*)
      '("Bionics" "Neural lace" "Wetware" "Cyber-implants" "Augmentation" "Biomechatronics"))

(defun flatten-terms ()
  (let ((result nil))
    (maphash (lambda (group terms)
               (dolist (term terms) (push (list group term) result)))
             *synonym-groups*)
    result))

(defun search-substring (query)
  (let ((q (normalize query)))
    (loop for (group term) in (flatten-terms)
          when (search q (normalize term))
          collect (list group term 1.0))))

(defun similarity-score (a b)
  (let* ((norm-a (normalize a)) (norm-b (normalize b))
         (len (min (length norm-a) (length norm-b)))
         (matches (loop for i below len count (char= (char norm-a i) (char norm-b i)))))
    (/ (float matches) (max 1 (max (length norm-a) (length norm-b))))))

(defun search-fuzzy (query &key (limit 10) (min-score 0.3))
  (let ((results
         (loop for (group term) in (flatten-terms)
               for score = (similarity-score query term)
               when (>= score min-score)
               collect (list group term score))))
    (subseq (sort results #'> :key #'third) 0 (min limit (length results)))))

(defun is-synonym-p (word target-group)
  (let ((terms (gethash target-group *synonym-groups*)))
    (member (normalize word) (mapcar #'normalize terms) :test #'string=)))

(defun get-random-group ()
  (let ((keys (loop for k being the hash-keys of *synonym-groups* collect k)))
    (nth (random (length keys)) keys)))


;;; ------------------------------------------------------------------
;;; PACKAGE 3: CYBER-WEB (HUNCHENTOOT REST API & SPA UI)
;;; ------------------------------------------------------------------
(defpackage :cyber-web
  (:use :cl :hunchentoot :cyber-utils :synonyms-bank)
  (:export :start-server :stop-server))

(in-package :cyber-web)

(defvar *server* nil)

(defun to-json (results)
  (with-output-to-string (out)
    (write-string "[" out)
    (loop for r in results
          for i from 0
          do (format out "{\"group\":\"~A\",\"term\":\"~A\",\"score\":~2f}"
                     (first r) (second r) (third r))
             (when (< i (1- (length results)))
               (write-string "," out)))
    (write-string "]" out)))

(define-easy-handler (api-search :uri "/api/search") (q)
  (setf (content-type*) "application/json")
  (log-event "MATRIX-API" "INFO" (format nil "API Substring Search executed for: '~A'" q))
  (to-json (search-substring q)))

(define-easy-handler (api-fuzzy :uri "/api/fuzzy") (q)
  (setf (content-type*) "application/json")
  (log-event "MATRIX-API" "INFO" (format nil "API Fuzzy Search executed for: '~A'" q))
  (to-json (search-fuzzy q)))

(define-easy-handler (home :uri "/") ()
  (setf (content-type*) "text/html")
  (log-event "MATRIX-WEB" "INFO" "Public HTTP Matrix Interface accessed.")
  "<!DOCTYPE html>
  <html>
  <head>
    <title>SYS@LEXICON // Matrix Node</title>
    <style>
      body { background-color: #050505; color: #00ff00; font-family: 'Courier New', Courier, monospace; margin: 20px;
             background-image: repeating-linear-gradient(0deg, transparent, transparent 2px, rgba(0, 255, 0, 0.05) 2px, rgba(0, 255, 0, 0.05) 4px); }
      .container { display: flex; gap: 20px; max-width: 1200px; margin: 0 auto; }
      .panel { border: 1px solid #00ff00; padding: 20px; box-shadow: 0 0 10px #00ff00; background: #0a0a0a; flex: 1; }
      h2 { color: #ff00ff; text-shadow: 0 0 10px #ff00ff; border-bottom: 1px solid #ff00ff; padding-bottom: 10px; margin-top:0;}
      input[type=text] { background: #000; color: #00ff00; border: 1px solid #00ff00; padding: 10px; width: calc(100% - 110px); font-family: 'Courier New'; outline: none; }
      input[type=text]:focus { box-shadow: 0 0 8px #00ff00; }
      button { background: #00ff00; color: #000; border: none; padding: 10px; width: 90px; cursor: pointer; font-weight: bold; font-family: 'Courier New'; }
      button:hover { background: #ff00ff; color: #fff; box-shadow: 0 0 10px #ff00ff; }
      .form-group { margin-bottom: 25px; }
      .label { color: #00ffff; margin-bottom: 5px; display: block; font-weight: bold; font-size: 0.9em; }
      #output { background: #001100; padding: 15px; border: 1px dashed #00ff00; min-height: 300px; white-space: pre-wrap; font-size: 0.9em; overflow-y: auto;}
    </style>
  </head>
  <body>
    <div class='container'>
      <div class='panel'>
        <h2>// NEURAL KNOWLEDGE GRAPH API</h2>
        <div class='form-group'>
          <span class='label'>// EXACT MATCH (/api/search)</span>
          <input type='text' id='search-input' placeholder='Enter search vector...'/>
          <button onclick=\"fetchData('search')\">SEARCH</button>
        </div>
        <div class='form-group'>
          <span class='label'>// AI FUZZY SEMANTIC MATCH (/api/fuzzy)</span>
          <input type='text' id='fuzzy-input' placeholder='Enter corrupted string...'/>
          <button onclick=\"fetchData('fuzzy')\">ANALYZE</button>
        </div>
      </div>
      <div class='panel' style='flex: 1.5;'>
        <h2>// TERMINAL OUTPUT STREAM</h2>
        <div id='output'>Awaiting query payload...</div>
      </div>
    </div>
    <script>
      async function fetchData(endpoint) {
        const query = document.getElementById(endpoint + '-input').value;
        const out = document.getElementById('output');
        out.innerHTML = '<span style=\"color:#ffff00\">>> DECRYPTING PACKETS...</span>';
        try {
          const res = await fetch(`/api/${endpoint}?q=${encodeURIComponent(query)}`);
          const data = await res.json();
          if (data.length === 0) {
            out.innerHTML = '<span style=\"color:#ff0000\">[ERROR] 0 NODES FOUND.</span>';
          } else {
            out.innerHTML = '<span style=\"color:#00ffff\">[SUCCESS] PAYLOAD DECRYPTED:</span>\\n\\n' + JSON.stringify(data, null, 2);
          }
        } catch (e) {
          out.innerHTML = '<span style=\"color:#ff0000\">[CRITICAL] CONNECTION TO MATRIX LOST.</span>';
        }
      }
    </script>
  </body>
  </html>")

(defun start-server (&optional (port 8080))
  (if *server*
      (log-event "MATRIX" "WARNING" "Server is already running.")
      (progn
        (setf *server* (start (make-instance 'easy-acceptor :port port)))
        (log-event "MATRIX" "INFO" (format nil "Public Matrix Interface online on port ~A" port)))))

(defun stop-server ()
  (if *server*
      (progn
        (stop *server*)
        (setf *server* nil)
        (log-event "MATRIX" "WARNING" "Public Matrix Interface taken offline."))
      (log-event "MATRIX" "WARNING" "Server is not running.")))


;;; ------------------------------------------------------------------
;;; PACKAGE 4: SECURE BANKING SYSTEM (WITH ACTIVE TRACE)
;;; ------------------------------------------------------------------
(defpackage :secure-bank
  (:use :cl :cyber-utils)
  (:export :run-interactive :*trace-level*))

(in-package :secure-bank)

(defparameter *trace-level* 0)
(defparameter *locked* nil)

(define-condition transaction-denied (error)
  ((reason :initarg :reason :reader denial-reason)))

(defclass account ()
  ((id :initarg :id :reader account-id)
   (balance :initarg :balance :accessor account-balance)
   (authorized-users :initarg :authorized-users :reader account-authorized-users)))

(defparameter *accounts*
  (list (make-instance 'account :id "fed_reserve_001" :balance 10000000 :authorized-users '("admin_secure"))))

(defun request-withdrawal (user account-id amount signature pass req-concept)
  (let ((account (find account-id *accounts* :key #'account-id :test #'string=)))
    (unless (and (numberp amount) (> amount 0))
      (error 'transaction-denied :reason "Invalid integer payload. Buffer overflow mitigated."))
    (unless account
      (error 'transaction-denied :reason "Target node offline."))
    (unless (member user (account-authorized-users account) :test #'string=)
      (error 'transaction-denied :reason "Invalid User Credentials."))
    (unless (string= signature "valid_sig")
      (error 'transaction-denied :reason "Invalid Cryptographic Signature."))

    (log-event "BANK-ICE" "INFO" "Initiating Semantic Firewall Validation...")
    (unless (synonyms-bank:is-synonym-p pass req-concept)
      (error 'transaction-denied :reason (format nil "Semantic check failed for: ~A" req-concept)))

    (when (< (account-balance account) amount)
      (error 'transaction-denied :reason "Insufficient network liquidity."))

    (decf (account-balance account) amount)
    (log-event "BANK-ICE" "INFO" (format nil "~A withdrew $~A. Remaining: $~A" user amount (account-balance account)))
    (setf *trace-level* (max 0 (- *trace-level* 20))) ; Lower trace on success
    (typewrite "TRANSACTION APPROVED. FUNDS DISBURSED." :green)))

(defun run-interactive ()
  (when *locked*
    (typewrite "FATAL ERROR: YOUR IP HAS BEEN BLACKLISTED BY FED_RESERVE ICE." :red)
    (return-from run-interactive))

  (typewrite " [ FEDERAL RESERVE SECURE MAINFRAME ] " :magenta)
  (let ((puzzle (synonyms-bank:get-random-group)))
    (format t "~%~A ~A~%"
            (print-color :red ">> ICE ACTIVE:")
            (print-color :cyan (format nil "Semantic lock engaged. Concept required: '~A'" puzzle)))
    (let ((user (prompt "User ID:" :yellow))
          (acc (prompt "Account ID:" :yellow))
          (amt (prompt "Amount:" :yellow))
          (sig (prompt "Signature:" :yellow))
          (pass (prompt "Semantic Passphrase:" :yellow)))
      (handler-case
          (let ((result (request-withdrawal user acc (parse-integer amt :junk-allowed t) sig pass puzzle)))
            (format t "RESULT: ~A~%" result))
        (transaction-denied (c)
          (incf *trace-level* 35)
          (log-event "BANK-ICE" "WARNING" (format nil "Intrusion detected. Trace level at ~A%" *trace-level*))
          (format t "~A~%" (print-color :red (format nil "ACCESS DENIED: ~A" (denial-reason c))))
          (when (>= *trace-level* 100)
            (setf *locked* t)
            (typewrite "CRITICAL: TRACE 100%. BLACK ICE DEPLOYED. TERMINAL LOCKED." :red)))))))


;;; ------------------------------------------------------------------
;;; PACKAGE 5: MASTER CYBER-OS BOOTLOADER
;;; ------------------------------------------------------------------
(defpackage :cyber-os
  (:use :cl :cyber-utils)
  (:export :boot))

(in-package :cyber-os)

(defun print-header ()
  (typewrite "
   _____ __  __ __   __ __   __     __ _____   _____ _____
  / ____|  \\/  |  \\ /  |  \\ /  |   /  |  __ \\ / ____|_   _|
 | (___ | \\  / | \\ V / | \\ V / |  /  /| |  | | (___   | |
  \\___ \\| |\\/| |  > <  |  > <  | /  / | |  | |\\___ \\  | |
  ____) | |  | | / . \\ | / . \\ |/  /  | |__| |____) |_| |_ 
 |_____/|_|  |_|/_/ \\_\\|/_/ \\_\\___/   |_____/|_____/|_____|
  " :cyan)
  (typewrite "             OS KERNEL v9.0 ONLINE             " :magenta))

(defun boot ()
  (print-header)
  (log-event "KERNEL" "INFO" "Cyber-OS booted.")
  (loop
    (format t "~%")
    (let ((cmd (prompt "root@cyber-os:~#" :green)))
      (cond
        ((string= cmd "exit")
         (cyber-web:stop-server)
         (typewrite "Terminating connection..." :red)
         (return))

        ((string= cmd "help")
         (format t "Modules:~%  ~A     - Start Public Matrix Web Server~%  ~A   - Stop Web Server~%  ~A     - Scan Local Subnet~%  ~A    - Access Bank Mainframe~%  ~A   - View Security Audit Logs~%  ~A  - View Network Trace Status~%  ~A    - Shutdown OS~%"
                 (print-color :yellow "net-up") (print-color :yellow "net-down") (print-color :yellow "scan")
                 (print-color :yellow "bank") (print-color :yellow "audit") (print-color :yellow "status")
                 (print-color :yellow "exit")))

        ((string= cmd "net-up") (cyber-web:start-server 8080))
        ((string= cmd "net-down") (cyber-web:stop-server))
        ((string= cmd "bank") (secure-bank:run-interactive))

        ((string= cmd "scan")
         (typewrite "Scanning 192.168.0.x subnet..." :cyan)
         (sleep-print "..." 0.5 :cyan)
         (sleep-print "Node discovered: MATRIX_WEB [Localhost:8080]" :green)
         (sleep-print "Node discovered: FED_RESERVE_MAINFRAME [ID: fed_reserve_001]" :yellow))

        ((string= cmd "status")
         (format t "ACTIVE NETWORK TRACE LEVEL: ~A%~%"
                 (if (> secure-bank:*trace-level* 50)
                     (print-color :red secure-bank:*trace-level*)
                     (print-color :green secure-bank:*trace-level*))))

        ((string= cmd "audit")
         (if (null *syslog*)
             (format t "No events recorded.~%")
             (dolist (entry (reverse *syslog*))
               (destructuring-bind (ts source level msg) entry
                 (declare (ignore ts))
                 (format t "[~A] [~A] ~A~%"
                         (print-color :cyan source)
                         (print-color (if (string= level "CRITICAL") :red :yellow) level) msg)))))
        (t (format t "~A Command not found. Type 'help'.~%" (print-color :red "ERROR:")))))))
