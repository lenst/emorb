;; Corba struct experiment

(defun posq (item vector)
  (let ((i 0) (len (length vector)))
    (while (and (< i len) (not (eq item (aref vector i))))
      (setq i (1+ i)))
    (if (< i len)
        i)))


(defun key-pos (key type)
  (posq key
        (if (symbolp type)
            (get type 'key-vector)
            type)))


(defun corba-get1 (obj key)
  (let ((pos (key-pos key (aref obj 0))))
    (unless pos (error "Key '%s' not in object" key))
    (aref obj pos)))


(defconst corba-x-struct [struct :foo :bar :x])

(defun make-struct (type &rest initargs)
  (let ((n (length type)))
    (let ((obj (make-vector n nil)))
      (aset obj 0 type)
      (dotimes (i n)
        (unless (zerop i)
          (let ((v (plist-get initargs (aref type i))))
            (aset obj i v))))
      obj)))


;; key cache: (type . index)
(defun corba-get2 (obj key cache)
  (let ((type (aref obj 0)))
    (if (eq type (car cache))
        (aref obj (cdr cache))
        (let ((pos (key-pos key type)))
          (unless pos (error "Key '%s' not in object" key))
          (setcar cache type)
          (setcdr cache pos)
          (aref obj pos)))))


(defmacro corba-get (obj key)
  (if (keywordp key)
      `(corba-get2 ,obj ,key ',(cons nil nil))
      `(corba-get1 ,obj ,key)))


(defun foo (s)
  (corba-get s :foo))


(put :tk_objref 'key-vector [:kind :id :name])
(defconst tc_objref [:tk_objref "IDL:omg.org/CORBA/Object:1.0" "Object"])
