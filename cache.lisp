(in-package #:org.shirakumo.fraf.modio)

(defstruct (cache
            (:constructor make-cache ())
            (:copier NIL)
            (:predicate NIL))
  (objects (make-hash-table :test 'eql))
  (lists (make-hash-table :test 'equalp)))

(defgeneric valid-p (object))

(defmethod valid-p (object) T)

(defmethod valid-p ((object download))
  (< (get-universal-time) (date-expires object)))

(defmethod valid-p ((object game-stats))
  (< (get-universal-time) (date-expires object)))

(defmethod valid-p ((object mod-stats))
  (< (get-universal-time) (date-expires object)))

(defmethod clear-cache ((cache cache))
  (clrhash (cache-objects cache))
  (clrhash (cache-lists cache)))

(defmethod cache-object ((cache cache) type data)
  (let ((id (gethash "id" data)))
    (if id
        (let ((cached (gethash id (cache-objects cache))))
          (cond (cached
                 (fill-object-from-data cached data))
                (T
                 (setf cached (fill-object-from-data type data))
                 (setf (gethash id (cache-objects cache)) cached))))
        (fill-object-from-data type data))))

(defmethod cache-listing ((cache cache) query list)
  (setf (gethash query (cache-lists cache)) list))

(defmethod get-listing ((cache cache) query)
  (gethash query (cache-lists cache)))

(defmethod games/get ((cache cache) (id integer) &key)
  (gethash id (cache-objects cache)))

(defmethod games/mods/get ((cache cache) game (id integer) &key)
  (gethash id (cache-objects cache)))

(defmethod games/mods/comments/get ((cache cache) game mod (id integer) &key)
  (gethash id (cache-objects cache)))

(defmethod games/mods/files/get ((cache cache) game mod (id integer) &key)
  (gethash id (cache-objects cache)))

;;; Client integration

(defmethod clear-cache ((client client))
  (clear-cache (cache client)))

(defmethod cache-object ((client client) type data)
  (cache-object (cache client) type data))

(defmethod cache-listing ((client client) query list)
  (cache-listing (cache client) query list))

(defmethod get-listing ((client client) query)
  (get-listing (cache client) query))

(defmethod games/get :around ((client client) (id integer) &key ignore-cache)
  (or (unless ignore-cache
        (games/get (cache client) id))
      (call-next-method)))

(defmethod games/mods/get :around ((client client) game (id integer) &key ignore-cache)
  (or (unless ignore-cache
        (games/mods/get (cache client) id))
      (call-next-method)))

(defmethod games/mods/comments/get :around ((client client) game mod (id integer) &key ignore-cache)
  (or (unless ignore-cache
        (games/mods/comments/get (cache client) game mod id))
      (call-next-method)))

(defmethod games/mods/files/get :around ((client client) game mod (id integer) &key ignore-cache)
  (or (unless ignore-cache
        (games/mods/files/get (cache client) game mod id))
      (call-next-method)))
