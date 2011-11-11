(in-package :cl-gdata-picasa)

(declaim #.cl-gdata::*compile-decl*)

(defvar *allowed-image-mime-types* '("image/bmp" "image/gif" "image/jpeg" "image/png")
  "A list of MIME types that are allowed when uploading photos")

(defclass album (atom-feed-entry)
  ((summary :type (or null string)
            :reader album-summary
            :node "atom:summary/text()"
            :node-default ""
            :documentation "The content of the <atom:summary> node"))
  (:documentation "Class that represents a single album")
  (:metaclass atom-feed-entry-class))

(defun list-all-albums (&key (session *gdata-session*) user)
  "Return a list of all accessible albums for a user. USER indicates the
username whose albmus should be retrieved. Defaults to the current user."
  (load-atom-feed-url (format nil "https://picasaweb.google.com/data/feed/api/user/~a"
                              (if user (url-rewrite:url-encode user) "default"))
                      'album
                      :session session))

(defclass photo (atom-feed-entry)
  ((published          :type string
                       :reader photo-published
                       :node "atom:published/text()"
                       :documentation "The date the photo was uploaded")
   (summary            :type string
                       :reader photo-summary
                       :node "atom:summary/text()"
                       :node-default ""
                       :documentation "Summary for the photo")
   (content            :type list
                       :reader photo-content
                       :node ("atom:content" "@type" "@src")
                       :node-collectionp t
                       :documentation "A list of content locations")
   (photo-id           :type (or null string)
                       :reader photo-id
                       :node "gphoto:id/text()"
                       :documentation "The photo ID")
   (position           :type (or null string)
                       :reader photo-position
                       :node "gphoto:position/text()")
   (width              :type number
                       :reader photo-width
                       :node "gphoto:width/text()"
                       :node-type :number
                       :documentation "The width of the photo in pixels")
   (height             :type number
                       :reader photo-height
                       :node "gphoto:height/text()"
                       :node-type :number
                       :documentation "The height of the photo in pixels")
   (size               :type number
                       :reader photo-size
                       :node "gphoto:size/text()"
                       :node-type :number
                       :documentation "The file size of the photo")
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
                       :node-type :number
                       :documentation "The number of comments that has been posted for this photo")
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
                       :node-collectionp t
                       :documentation "A list of thumbnail descriptions"))
  (:documentation "Class that represents a single photo in an album.")
  (:metaclass atom-feed-entry-class))

(defun list-photos-from-url (url &key (session *gdata-session*))
  "Return a list of photos in a given album specified using the Picasa album url format"
  (load-atom-feed-url url 'photo :session session))

(defun list-photos (album &key (session *gdata-session*))
  "Return a list of photos in a given album"
  (list-photos-from-url (find-feed-from-atom-feed-entry album +ATOM-TAG-FEED+) :session session))

(defun photo-image-types (photo)
  "Return a list of image types available for the given photo"
  (check-type photo photo)
  (mapcar #'car (photo-content photo)))

(defun download-photo-to-stream (photo out-stream &key type)
  "Download the given photo and write the content to OUT-STREAM. If given, TYPE indicates
the image type to download. TYPE must be one of the types returned by PHOTO-IMAGE-TYPES."
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
      (unwind-protect
           (progn
             (when (/= code 200)
               (error "Error downloading photo: ~a" reason))
             (cl-fad:copy-stream stream out-stream))
        (when should-close
          (close stream))))))

(defun download-photo-to-file (photo filespec &key type overwrite)
  "Download the given photo to a file given by FILESPEC. If given, TYPE indicates
the image type to download. TYPE must be one of the types returned by PHOTO-IMAGE-TYPES.
If the OVERWRITE keyword is non-NIL, an existing file will be overwritten, otherwise
an error will be raised."
  (with-open-file (out filespec
                       :direction :output
                       :if-exists (if overwrite :supersede :error)
                       :if-does-not-exist :create
                       :element-type '(unsigned-byte 8))
    (download-photo-to-stream photo out :type type)))

(define-constant +CRLF+ (format nil "~c~c" #\Return #\Newline))

(defun upload-photo (album type stream title &key (session *gdata-session*) summary)
  "Upload a photo to Picasa. ALBUM indicates the album the the photo should be uploaded to.
TYPE is the mime-type of the photo and must be one of the allowed types in *ALLOWED-IMAGE-MIME-TYPES*.
STREAM must be a binary input stream from which to read the image data. TITLE is the title of
the photo. SUMMARY is the summary for the photo."
  (unless (find type *allowed-image-mime-types* :test #'equal)
    (error "Image type ~a must be one of ~s" type *allowed-image-mime-types*))
  (let ((url (find-feed-from-atom-feed-entry album +ATOM-TAG-FEED+))
        (boundary (format nil "MIME_BOUNDARY_END_~d" (random 1000000000))))
    (flet ((send-output (base-outstream)
             (let ((outstream (flexi-streams:make-flexi-stream base-outstream :external-format :utf-8)))
               (format outstream "Media multipart posting~a--~a~a" +CRLF+ boundary +CRLF+)
               (format outstream "Content-Type: ~a; charset=UTF-8~a~a" +ATOM-XML-MIME-TYPE+ +CRLF+ +CRLF+)
               (build-atom-xml-stream `(("atom" "entry")
                                        ,@(when title `((("atom" "title") ,title)))
                                        ,@(when summary `((("atom" "summary") ,summary)))
                                        (("atom" "category"
                                                 "scheme" "http://schemas.google.com/g/2005#kind"
                                                 "term" "http://schemas.google.com/photos/2007#photo")))
                                      outstream)
               (format outstream "~a--~a~a" +CRLF+ boundary +CRLF+)
               (format outstream "Content-Type: ~a~a~a" type +CRLF+ +CRLF+)
               (finish-output outstream))
             (cl-fad:copy-stream (flexi-streams:make-flexi-stream stream) base-outstream)
             (let ((outstream (flexi-streams:make-flexi-stream base-outstream)))
               (format outstream "~a--~a--~a" +CRLF+ boundary +CRLF+)
               (finish-output outstream))))
      (let ((result (load-and-parse url
                                    :session session
                                    :method :post
                                    :additional-headers '(("MIME-Version" . "1.0"))
                                    :content-type (format nil "multipart/related; boundary=\"~a\"" boundary)
                                    :content #'(lambda (os)
                                                 (send-output os))
                                    :force-binary t
                                    :accepted-status '(201))))
        (with-gdata-namespaces
          (make-instance 'photo :node-dom (xpath:first-node (xpath:evaluate "/atom:entry" result))))))))
