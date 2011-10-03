(in-package :cl-gdata)

(declaim #.*compile-decl*)

(defclass album (atom-feed-entry)
  ((summary :type (or null string)
            :reader album-summary
            :node "atom:summary/text()"
            :node-default ""
            :documentation "The content of the <atom:summary> node"))
  (:documentation "Class that represents a single album")
  (:metaclass atom-feed-entry-class))

(defun list-all-albums (&key (session *gdata-session*) username)
  (load-atom-feed-url (format nil "https://picasaweb.google.com/data/feed/api/user/~a"
                              (if username (url-rewrite:url-encode username) "default"))
                      'album
                      :session session))

(defclass photo (atom-feed-entry)
  ((published          :type string
                       :reader photo-published
                       :node "atom:published/text()")
   (summary            :type string
                       :reader photo-summary
                       :node "atom:summary/text()"
                       :node-default "")
   (content            :type list
                       :reader photo-content
                       :node ("atom:content" "@type" "@src")
                       :node-collectionp t)
   (photo-id           :type (or null string)
                       :reader photo-id
                       :node "gphoto:id/text()")
   (position           :type (or null string)
                       :reader photo-position
                       :node "gphoto:position/text()")
   (width              :type number
                       :reader photo-width
                       :node "gphoto:width/text()"
                       :node-type :number)
   (height             :type number
                       :reader photo-height
                       :node "gphoto:height/text()"
                       :node-type :number)
   (size               :type number
                       :reader photo-size
                       :node "gphoto:size/text()"
                       :node-type :number)
   (abs-rotation       :type number
                       :reader photo-abs-rotation
                       :node "gphoto:absRotation/text()"
                       :node-type :number)
   (image-version      :type string
                       :reader photo-version
                       :node "gphoto:imageVersion/text()")
   (commenting-enabled :type (or nil t)
                       :reader photo-commenting-enabled
                       :node "gphoto:commentingEnabled/text()"
                       :node-type :true-false)
   (comment-count      :type number
                       :reader photo-comment-count
                       :node "gphoto:commentCount/text()"
                       :node-type :number)
   ;; EXIF data
   (exif-fstop         :type (or null number)
                       :reader photo-exif-fstop
                       :node "exif:tags/exif:fstop/text()"
                       :node-type :number)
   (exif-make          :type (or null string)
                       :reader photo-exif-make
                       :node "exif:tags/exif:make/text()")
   (exif-model         :type (or null string)
                       :reader photo-exif-model
                       :node "exif:tags/exif:model/text()")
   (exif-exposure      :type (or null number)
                       :reader photo-exif-exposure
                       :node "exif:tags/exif:exposure/text()"
                       :node-type :number)
   (exif-flash         :type (or null t)
                       :reader photo-exif-flash
                       :node "exif:tags/exif:flash/text()"
                       :node-type :true-false)
   (exif-focallength   :type (or null number)
                       :reader photo-exif-focallength
                       :node "exif:tags/exif:focallength/text()"
                       :node-type :number)
   (exif-iso           :type (or null number)
                       :reader photo-exif-iso
                       :node "exif:tags/exif:iso/text()"
                       :node-type :number)
   ;; Media tags
   (media-credit       :type (or null string)
                       :reader photo-media-credit
                       :node "media:group/media:credit/text()")
   (media-thumbnail    :type list
                       :reader photo-media-thumbnail
                       :node ("media:group/media:thumbnail" "@url" "@width" "@height")
                       :node-collectionp t))
  (:metaclass atom-feed-entry-class))

(defun list-photos-from-url (url &key (session *gdata-session*))
  (load-atom-feed-url url 'photo :session session))

(defun list-photos (album &key (session *gdata-session*))
  (list-photos-from-url (find-feed-from-atom-feed-entry album +ATOM-TAG-FEED+) :session session))

(defun photo-image-types (photo)
  (check-type photo photo)
  (mapcar #'car (photo-content photo)))

(defun download-photo-to-stream (photo out-stream &key type)
  (let ((url (if type
                 (find type (photo-content photo))
                 (car (photo-content photo)))))
    (unless url
      (error "Can't find photo URL"))
    (multiple-value-bind (stream code received-headers original-url reply-stream should-close reason)
        (drakma:http-request (cadr url)
                             :want-stream t
                             :user-agent +HTTP-GDATA-USER-AGENT+
                             :force-binary t)
      (declare (ignore received-headers original-url reply-stream))
      (cl-fad:copy-stream stream out-stream)
      (when (/= code 200)
        (error "Error downloading photo: ~a" reason))
      (when should-close
        (close stream)))))

(defun download-photo-to-file (photo filespec &key type overwrite)
  (with-open-file (out filespec
                       :direction :output
                       :if-exists (if overwrite :supersede :error)
                       :if-does-not-exist :create
                       :element-type '(unsigned-byte 8))
    (download-photo-to-stream photo out :type type)))

(defun upload-photo (album stream &key (session *gdata-session*))
  (let ((url (find-feed-from-atom-feed-entry album +ATOM-TAG-EDIT+)))
    ))
