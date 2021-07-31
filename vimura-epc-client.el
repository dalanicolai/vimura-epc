(require 'epc)
(require 'svg)

(defvar my-epc (epc:start-epc "python" '("vimura-epc-server.py")))

;; (deferred:$
;;   (epc:call-deferred my-epc 'echo '(10))
;;   (deferred:nextc it
;;     (lambda (x) (message "Return : %S" x))))

(defun vimura-test (page)
  (epc:call-sync my-epc 'test (list page)))

(defun vimura-page-svg-group (page &optional transform-height)
  (let ((width (x-display-pixel-width)))
    (with-temp-buffer
      (save-excursion
        (insert (epc:call-sync my-epc 'echo (list page width))))
      ;; don't jump to beginning if not first page
      (search-forward-regexp "height=\"\\([0-9]*\\)")
      (let ((height (string-to-number (match-string 1))))
        (search-forward "<g ")
        (when transform-height
          (insert (format "transform='translate(0,%s)' " transform-height)))
        (let ((group-start (match-beginning 0)))
          (search-forward "</g>")
          (cons height (buffer-substring group-start (match-end 0))))))))

(defun vimura-page-triplet-image-object ()
  (with-temp-buffer
    (let* ((top (vimura-page-svg-group 26))
           (middle-start-height (car top))
           (middle (vimura-page-svg-group 27 middle-start-height))
           (bottom-start-height (+ (car top) (car middle)))
           (bottom (vimura-page-svg-group 28 bottom-start-height))
           (width (x-display-pixel-width))
           (total-height (+ (car top) (car middle) (car bottom)))
           (svg (svg-image
                 (svg-create width total-height))))
      (with-temp-buffer
        (insert (image-property svg :data))
        (backward-char 6)
        (insert " <rect width='100%' height='100%' fill='white'/>")
        (insert (cdr top))
        (insert (format "<line x1='0' y1='%s' x2='%s' y2='%s' stroke='black'/>"
                        middle-start-height
                        width
                        middle-start-height))
        (insert (cdr middle))
        (insert (format "<line x1='0' y1='%s' x2='%s' y2='%s' stroke='black'/>"
                        bottom-start-height
                        width
                        bottom-start-height))
        (insert (cdr bottom))
        (create-image (buffer-string) 'svg t)))))
