; Balzarotti Niccolò 852003
; Covelli Matteo 861277

; jsonparse esegue il parse su una stringa trasformandola
; in un object o in un array, a seconda della sua
; struttura
(defun jsonparse (string-in)
  (let ((list-code-out (rimuovi-spazi (coerce string-in 'list))))
  (cond ((equal (car list-code-out) '#\{) (json-object (cdr list-code-out)))
        ((equal (car list-code-out) '#\[) (json-array (cdr list-code-out)))
        (T (error "syntax error")))))

; json-array riconosce se il parametro passato in input è un array
(defun json-array (list-code-in)
  (let ((list-code-out (rimuovi-spazi list-code-in)))
    (cond ((and (equal (car list-code-out) '#\])
                (null (rimuovi-spazi (cdr list-code-out))))
           '(jsonarray))
          (T (let ((result (json-elements list-code-out nil)))
               (cond ((null (rimuovi-spazi (car (cdr result))))
                      (append '(jsonarray) (car result)))
                     (T (error "syntax error"))))))))

; json-object riconosce se il parametro passato in input è un object
(defun json-object (list-code-in)
  (let ((list-code-out (rimuovi-spazi list-code-in)))
    (cond ((and (equal (car list-code-out) '#\})
                (null (rimuovi-spazi (cdr list-code-out))))
           '(jsonobj))
          (T (let ((result (json-members list-code-out nil)))
               (cond ((null (rimuovi-spazi (car (cdr result))))
                      (append '(jsonobj) (car result)))
                     (T (error "syntax error"))))))))

; json-elements viene utilizzata nella json-array
(defun json-elements (list-code-in result)
  (let ((list-code-out (json-value list-code-in)))
    (let
        ((result2 (append result (list (car list-code-out))))
        (list-code-out2 (rimuovi-spazi (car (cdr list-code-out)))))
    (cond
     ((char= (car list-code-out2) '#\])
      (append (list result2) (list (cdr list-code-out2))))
     ((char= (car list-code-out2) '#\,)
      (json-elements (cdr list-code-out2) result2))
     (T (error "syntax error"))))))

; json-members viene utilizzata nella json-object
(defun json-members (list-code-in result)
  (let ((list-code-out (json-pair list-code-in)))
    (let
        ((result2 (append result (list (car list-code-out))))
        (list-code-out2 (rimuovi-spazi (car (cdr list-code-out)))))
    (cond
     ((char= (car list-code-out2) '#\})
      (append (list result2) (list (cdr list-code-out2))))
     ((char= (car list-code-out2) '#\,)
      (json-members (cdr list-code-out2) result2))
     (T (error "syntax error"))))))

; json-pair riconosce le coppie attribute,value
(defun json-pair (list-code-in)
  (let ((list-code-out (rimuovi-spazi list-code-in)))
    (cond ((or (equal (car list-code-out) '#\")
               (equal (car list-code-out) '#\'))
           (let ((list-string (json-string list-code-out)))
             (let
                 ((list-string2 (rimuovi-spazi (car (cdr list-string))))
                 (coppia (list (car list-string))))
             (cond ((equal (car list-string2) '#\:)
                    (let ((list-value
                           (json-value (rimuovi-spazi (cdr list-string2)))))
                      (append (list (append coppia (list (car list-value))))
                              (list (car (cdr list-value))))))
                   (T (error "syntax error"))))))
         (T (error "syntax error")))))

; json-value viene utilizzata per ottenere il corrispettivo
; valore, che può essere una stringa, un numero,
; un booleano
(defun json-value (list-string)
  (let ((list-string2 (rimuovi-spazi list-string)))
    (cond ((or (equal (car list-string2) '#\")
               (equal (car list-string2) '#\'))
           (json-string list-string2))
          ((and (char<= '#\0 (car list-string2))
                (char>= '#\9 (car list-string2)))
           (json-number list-string2 nil))
          ((char= '#\- (car list-string2))
           (json-number (cdr list-string2) '(#\-)))
          ((or (char= '#\n (car list-string2))
               (char= '#\t (car list-string2))
               (char= '#\f (car list-string2)))
           (json-boolean list-string2 nil))
          ((or (equal (car list-string2) '#\{)
               (equal (car list-string2) '#\[))
           (json-inside list-string2))
          (T (error "syntax error")))))

; json-number viene utilizzata nella json-value, per
; il caso in cui ho un numero come value
(defun json-number (list-number temp)
  (cond ((null list-number) (error "syntax error"))
        ((and (char<= '#\0 (car list-number))
              (char>= '#\9 (car list-number)))
         (json-number (cdr list-number)
                      (append temp (list (car list-number)))))
        ((char= '#\. (car list-number))
         (json-number-float (cdr list-number)
                            (append temp (list (car list-number)))))
        (T (append (list (parse-integer (coerce temp 'string)))
                   (rimuovi-spazi (list list-number))))))

; json-number-float viene utilizzata nella json-number,
; se il numero è un float
(defun json-number-float (list-number temp)
  (cond ((or (null list-number)
             (char= '#\. (car list-number)))
         (error "syntax error"))
        ((and (char<= '#\0 (car list-number))
              (char>= '#\9 (car list-number)))
         (json-number-float (cdr list-number)
                            (append temp
                                    (list (car list-number)))))
        (T (append (list (parse-float (coerce temp 'string)))
                   (rimuovi-spazi (list list-number))))))

; json-boolean viene utilizzata nella json-value, per
; il caso in cui ho un booleano come value
(defun json-boolean (list-boolean temp)
  (cond ((null list-boolean) (error "syntax error"))
        ((not (or (char= '#\, (car list-boolean))
                  (char= '#\Space (car list-boolean))
                  (char= '#\NewLine (car list-boolean))
                  (char= '#\Tab (car list-boolean))
                  (char= '#\} (car list-boolean))
                  (char= '#\] (car list-boolean))))
         (json-boolean (cdr list-boolean)
                       (append temp (list (car list-boolean)))))
        (T (if (equal (coerce temp 'string) "true")
               (append '(True)
                       (rimuovi-spazi (list list-boolean)))
             (if (equal (coerce temp 'string) "false")
                 (append '(False)
                         (rimuovi-spazi (list list-boolean)))
               (if (equal (coerce temp 'string) "null")
                   (append '(null)
                           (rimuovi-spazi (list list-boolean)))
               (error "syntax error")))))))

; json-string viene utilizzata nella json-value, per
; il caso in cui ho una stringa come value
(defun json-string (list-string)
  (cond ((char= '#\' (car list-string))
         (json-attribute-single (cdr list-string) nil))
        ((char= '#\" (car list-string))
         (json-attribute-double (cdr list-string) nil))))

; json-attribute-single viene utilizzata nella
; json-string per il caso in cui ho apici singoli
(defun json-attribute-single (list-string temp)
  (cond ((null list-string) (error "apici singoli non chiusi"))
        ((equal (car list-string) '#\")
         (error "doppi apici dentro apici singoli"))
        ((not (equal (car list-string) '#\'))
         (json-attribute-single (cdr list-string)
                                (append temp (list (car list-string)))))
        (T (append (list (coerce temp 'string))
                   (rimuovi-spazi (list (cdr list-string)))))))

; json-attribute-double viene utilizzata nella
; json-string nel caso in cui ho doppi apici
(defun json-attribute-double (list-string temp)
  (cond ((null list-string) (error "doppi apici non chiusi"))
        ((equal (car list-string) '#\')
         (error "apici singoli dentro apici doppi"))
        ((not (equal (car list-string) '#\"))
         (json-attribute-double (cdr list-string)
                                (append temp (list (car list-string)))))
        (T (append (list (coerce temp 'string))
                   (rimuovi-spazi (list (cdr list-string)))))))

; json-inside viene utilizzata nella json-value, per
; il caso in cui ho value innestati
(defun json-inside (list-string)
  (cond ((equal (car list-string) '#\{)
         (let ((list-string2
                (let ((list-string3 (rimuovi-spazi (cdr list-string))))
                  (cond ((equal (car list-string3) '#\})
                         (append (list '(jsonobj))
                                 (list (cdr list-string3))))
                        (T (let ((list-string4
                                  (json-members list-string3 nil)))
                             (append (list (append '(jsonobj)
                                                   (car list-string4)))
                                     (list (car (cdr list-string4))))))))))
           list-string2))
        ((equal (car list-string) '#\[)
         (let ((list-string2
                (let ((list-string3 (rimuovi-spazi (cdr list-string))))
                  (cond ((equal (car list-string3) '#\])
                         (append (list '(jsonarray))
                                 (list (cdr list-string3))))
                        (T (let ((list-string4
                                  (json-elements list-string3 nil)))
                             (append (list (append '(jsonarray)
                                                   (car list-string4)))
                                     (list (car (cdr list-string4))))))))))
           list-string2))))

; rimuovi-spazi si occupa di rimuovere tutti i tipi di spazi
; tra cui spazio normale, invio, tab
(defun rimuovi-spazi (list-in)
  (cond ((not (or (equal (car list-in) '#\Space)
             (equal (car list-in) '#\NewLine)
             (equal (car list-in) '#\Tab))) list-in)
   ((or (equal (car list-in) '#\Space)
             (equal (car list-in) '#\NewLine)
             (equal (car list-in) '#\Tab))
         (rimuovi-spazi (cdr list-in)))))

; jsonaccess viene utilizzata per ottenere un value
; presente all'interno del JSON
(defun jsonaccess (JSON &rest fields)
  (if (null fields)
      JSON
    (jsonaccess-app JSON fields)))

; jsonaccess-app funzione di appoggio per jsonaccess
(defun jsonaccess-app (JSON fields)
  (cond
        ((and (equal (list-length fields) 1)
              (listp JSON)
              (stringp (car fields))
              (equal (car JSON) 'jsonobj))
         (jsonaccess-member (cdr JSON) (car fields)))
        ((and (equal (list-length fields) 1)
              (listp JSON)
              (numberp (car fields))
              (>= (car fields) 0)
              (equal (car JSON) 'jsonarray))
         (jsonaccess-elements (cdr JSON) (car fields)))
        ((and (> (list-length fields) 1)
              (listp JSON)
              (stringp (car fields))
              (equal (car JSON) 'jsonobj))
         (jsonaccess-app (jsonaccess-member (cdr JSON) (car fields))
                     (cdr fields)))
        ((and (> (list-length fields) 1)
              (listp JSON)
              (numberp (car fields))
              (>= (car fields) 0)
              (equal (car JSON) 'jsonarray))
         (jsonaccess-app (jsonaccess-elements (cdr JSON) (car fields))
                     (cdr fields)))
        (T (error "syntax error"))))

; jsonaccess-member funzione di appoggio per jsonaccess-app
(defun jsonaccess-member (JSON attribute)
  (cond ((null JSON) (error "attribute non trovato"))
        ((equal (car (car JSON)) attribute)
         (car (cdr (car JSON))))
        (T (jsonaccess-member (cdr JSON) attribute))))

; jsonaccess-elements funzione di appoggio per jsonaccess-app
(defun jsonaccess-elements (JSON index)
  (cond ((null JSON) (error "indice non trovato"))
        ((equal index 0) (car JSON))
        (T (jsonaccess-elements (cdr JSON) (- index 1)))))

; jsonread permette di creare un JSON a partire da
; una stringa in un file.json
(defun jsonread (filename)
  (with-open-file (stream filename :direction :input
                          :if-does-not-exist :error)
    (let ((result (make-string (file-length stream))))
      (jsonparse (subseq result 0 (read-sequence result stream))))))

; jsondump permette di scrivere all'interno di un file.json
; se il file non esiste viene creato
(defun jsondump (JSON filename)
  (with-open-file (stream filename
                          :direction :output
                          :if-exists :supersede
                          :if-does-not-exist :create)
    (format stream (json-write JSON)) filename))

; json-write viene utilizzata all'interno di jsondump
(defun json-write (JSON)
  (cond ((eq (car JSON) 'jsonobj)
         (concatenate 'string "{"
                      (ultimo-carattere (json-write-obj (cdr JSON)))
                      "}"))
        ((equal (car JSON) 'jsonarray)
         (concatenate 'string "["
                      (ultimo-carattere (json-write-array (cdr JSON)))
                      "]"))
        (T (error "syntax error"))))

; json-write-obj viene utilizzata nella json-write per
; la scrittura di un json di tipo object
(defun json-write-obj (JSON)
  (cond ((null JSON) "")
        ((listp (car JSON))
         (concatenate 'string
                      (concatenate 'string "\""
                                   (car (car JSON))
                                   "\"" ":"
                                   (json-write-value (car (cdr (car JSON))))
                                   ",")
                      (json-write-obj (cdr JSON))))))

; json-write-value viene utilizzata per la scrittura di
; un value all'interno della json-write-obj e la
; json-write-array
(defun json-write-value (value)
  (cond ((numberp value)
         (write-to-string value))
        ((or (equal value 'true)
             (equal value 'false)
             (equal value 'null))
         (concatenate 'string "\"" (string-downcase
                                    (write-to-string value)) "\""))
        ((stringp value) (concatenate 'string "\"" value "\""))
        (T (json-write value))))

; json-write-array viene utilizzata nella json-write per
; la scrittura di un json di tipo array
(defun json-write-array (JSON)
  (cond ((null JSON) "")
        (T (concatenate 'string
                        (json-write-value (car JSON))
                        ","
                        (json-write-array (cdr JSON))))))

; ultimo-carattere si occupa di eliminare l'ultimo
; carattere del parametro passato in input
(defun ultimo-carattere (JSON)
  (cond ((string= "" JSON) JSON)
        (T (subseq JSON 0 (- (length JSON) 1)))))