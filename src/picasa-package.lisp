(defpackage :cl-gdata-picasa
  (:use :cl :cl-gdata :cl-gdata-misc)
  (:export #:album 
           #:photo
           #:photo-published
           #:photo-summary
           #:photo-content
           #:photo-id
           #:photo-position
           #:photo-width
           #:photo-height
           #:photo-size
           #:photo-abs-rotation
           #:photo-version
           #:photo-commenting-enabled
           #:photo-comment-count
           #:photo-exif-fstop
           #:photo-exif-make
           #:photo-exif-model
           #:photo-exif-exposure
           #:photo-exif-flash
           #:photo-exif-focallength
           #:photo-exif-iso
           #:photo-media-credit
           #:photo-media-thumbnail
           #:list-photos-from-url
           #:list-photos
           #:photo-image-types
           #:download-photo-to-stream
           #:download-photo-to-file
           #:upload-photo
           #:list-albums))
