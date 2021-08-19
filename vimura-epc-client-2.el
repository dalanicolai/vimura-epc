(require 'epc)
(require 'svg)

(defvar my-epc (epc:start-epc "python" '("vimura-epc-server.py")))

;; (deferred:$
;;   (epc:call-deferred my-epc 'echo '(10))
;;   (deferred:nextc it
;;     (lambda (x) (message "Return : %S" x))))

(defun vimura-test (page)
  (epc:call-sync my-epc 'test (list page)))

(defun vimura-init ()
  (epc:call-sync my-epc 'vimura_init (list (buffer-file-name))))

(defun vimura-page-svg-group (page &optional transform-height)
  "Retrieve "
  (let ((width (window-width nil t)))
    (with-temp-buffer
      (save-excursion
        (insert (epc:call-sync my-epc 'page_svg (list page width))))
      ;; don't jump to beginning if not first page
      (search-forward-regexp "height=\"\\([0-9]*\\)")
      (let ((height (string-to-number (match-string 1))))
        (search-forward "<g ")
        (when transform-height
          (insert (format "transform='translate(0,%s)' " transform-height)))
        (let ((group-start (match-beginning 0)))
          (goto-char (point-max))
          (search-backward "</g>")
          (cons height (buffer-substring group-start (match-end 0))))))))

(defun vimura-page-doublet-image-object (page)
  (let* ((top (vimura-page-svg-group page))
         (bottom-start-height (car top))
         (bottom (vimura-page-svg-group (1+ page) bottom-start-height))
         (width (window-width nil t))
         (total-height (+ (car top) (car bottom)))
         (svg (svg-image
               (svg-create width total-height))))
    (with-temp-buffer
      (insert (image-property svg :data))
      (backward-char 6)
      (insert " <rect width='100%' height='100%' fill='white'/>")
      (insert (cdr top))
      (insert (format "<line x1='0' y1='%s' x2='%s' y2='%s' stroke='black'/>"
                      bottom-start-height
                      width
                      bottom-start-height))
      (insert (cdr bottom))
      (create-image (buffer-string) 'svg t))))

(defun vimura-goto-page (page)
  (remove-overlays (point-min) (point-max))
  (let* ((page-doublet (if-let (image-object
                                (cdar (cl-member page vimura-cache :key #'car)))
                           image-object
                         (vimura-page-doublet-image-object page)))
         (image-size (image-size page-doublet t))
         (ol (make-overlay (point-min) (point-max))))
    ;; (insert-image page-doublet)
    (overlay-put ol 'display page-doublet)
    (image-set-window-vscroll 0)
    (setq-local image-size image-size)
    (setq-local current-doublet page)
    ;; (setq-local current-page page)
    (add-to-list 'vimura-cache (cons page page-doublet))))


(defun vimura-scroll-up ()
  (interactive)
  (when (= (window-vscroll nil pdf-view-have-image-mode-pixel-vscroll)
           (image-scroll-up 4))
    (vimura-goto-page (1+ current-doublet))))
    ;; (image-set-window-vscroll (- (cdr image-size)
    ;;                              (window-pixel-height)
    ;;                              (/ (cdr image-size) 3)))))

(defun vimura-scroll-down ()
  (interactive)
  (when (= (window-vscroll nil pdf-view-have-image-mode-pixel-vscroll)
           (image-scroll-down 4))
    (vimura-goto-page (1- current-doublet))
    (image-set-window-vscroll (- (cdr image-size)
                                 (window-pixel-height)
                                 (/ (cdr image-size) 2)))))

(defun vimura-next-page ()
  (interactive)
  (let ((vscroll (window-vscroll nil pdf-view-have-image-mode-pixel-vscroll)))
    (vimura-goto-page (1+ current-doublet))
    (image-set-window-vscroll vscroll)))

(defun vimura-previous-page ()
  (interactive)
  (let ((vscroll (window-vscroll nil pdf-view-have-image-mode-pixel-vscroll)))
    (vimura-goto-page (1- current-doublet))
    (image-set-window-vscroll vscroll)))

(define-derived-mode vimura-mode special-mode "vimura-mode"
  "Versatile pdf editor"
  (vimura-init)
  (defvar-local vimura-cache '())
  (vimura-goto-page 0)
  (image-mode-setup-winprops))

(define-key vimura-mode-map (kbd "C-n") 'vimura-scroll-up)
(define-key vimura-mode-map (kbd "C-p") 'vimura-scroll-down)

(evilified-state-evilify-map vimura-mode-map
  :mode vimura-mode
  :bindings
  "j" 'vimura-scroll-up
  "k" 'vimura-scroll-down
  "J" 'vimura-next-page
  "K" 'vimura-previous-page)
