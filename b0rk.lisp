(in-package :cl-user)

(defun Memo (Function &key Function-Name
	                   (Test #'equal)
			   (Key #'identity)
			   (Hash-Table-Source :Create))
  "Takes a normal function object and returns an `equivalent' memoized one"
  (let ((Hash-Table (ecase Hash-Table-Source
		      (:Create       (make-hash-table :test Test))
		      (:Disk         (Load-Saved-Memo-Table Function-Name))
		      (:Old-Function (get Function-Name :Memo-Table)))))
    (setf (get Function-Name :Memo-Table) Hash-Table)
    (setf (get Function-Name :Unmemoized-Function) Function)
    (setf (get Function-Name :Memo-Table-Test) Test)
    #'(lambda (&rest Args)
	(declare (optimize (speed 3) (safety 1)))
	(let ((Hash-Key (funcall Key #+:LispM(copy-list Args)
				     #-:LispM Args )))
	  (multiple-value-bind (Value Found?)
	      (gethash Hash-Key Hash-Table)
	    (cond
	      (Found?
	       (incf (the fixnum (get Function-Name :Hash-Table-Lookups)))
	       Value)
	      (t
	       (incf (the fixnum (get Function-Name :Original-Function-Calls)))
	       (setf (gethash Hash-Key Hash-Table)
		     (apply Function Args))))))) ))

  