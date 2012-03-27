(defpackage :cl-gdata-spreadsheets
  (:use :cl :cl-gdata :cl-gdata-misc)
  (:export #:+SPREADSHEETS-WORKSHEETSFEED+
           #:+SPREADSHEETS-TABLESFEED+
           #:+SPREADSHEETS-CELLSFEED+
           #:spreadsheet
           #:spreadsheet-cell
           #:worksheet
           #:worksheet-spreadsheet
           #:worksheet-id-url
           #:worksheet-title
           #:worksheet-cells
           #:worksheet-rows
           #:worksheet-cols
           #:load-worksheets
           #:load-cell-range
           #:cell-input-value
           #:cell-value
           #:cell-update-error
           #:save-updated-cells
           #:map-cell-range
           #:spreadsheet-worksheets))
