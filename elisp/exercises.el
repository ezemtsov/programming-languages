(defun double (number)
  "Function takes number and doubles it"
  (* number 2))

(double 2)

(defun double-interactive (number)
  "Function takes number and doubles it"
  (interactive "p")
  (message "%d" (* number 2)))

(defun check-fill-column (number)
  "Function takes number and checks if fill-column is greater"
  (if (> fill-column number)
      (message "fill-column is greater")
    (message "fill-column is less")))

(check-fill-column 10)
(check-fill-column 100)

(defun simplified-end-of-buffer ()
  "Jump to end of current buffer"
  (interactive)
  (goto-char (point-max)))

(defun buffer-exists (name)
  "Check if buffer exists"
  (if (get-buffer name)
      (message "Buffer exists")
    (message "Buffer doesn't exist")))

(buffer-exists "*scratch*")
(buffer-exists "*test*")

(defun check-number (&optional arg)
  ""
  (interactive "p")
  (let ((num (if arg arg 56)))
    (if (> fill-column num)
          (message "fill-column is greater")
        (message "fill-column is less"))))

(check-number)
(check-number 100)

(setq bird-list 'owl)
(setq bird-list (cons 'parrot bird-list))
(setq bird-list (cons 'pigeon bird-list))
(setq bird-list (cons 'eagle bird-list))

(setq flowers '(violet butterucup))
(setq more-flowers (cons 'iris (cons 'rose flowers)))
(setq flowers (cons 'pirania flowers))
more-flowers
