(require :cl-ppcre)

(require :stumpwm)
(in-package :stumpwm)


;; please use the correct displays
(defparameter display-intern "LVDS1")
(defparameter display-extern "VGA1")


;; internal variables
(defparameter resolution-change-toggle nil)
(defparameter resolution-extern-change-toggle nil)
(defparameter dual-monitor-toggle nil)


(defstruct xrandr-line
  resolution
  refresh-rate
  star
  plus)

(defconstant OFF 
  (make-xrandr-line :resolution "0x0" :refresh-rate "0.0" :star nil :plus nil))

(defgeneric fuzzy-equal (xrandr-line xrandr-line)
  (:documentation "implements a fuzzy equal, star and plus don't need to be equal but resolution and refresh rate"))

(defmethod fuzzy-equal ((line1 xrandr-line) (line2 xrandr-line))
  (and
   (equal (xrandr-line-resolution line1) (xrandr-line-resolution line2))
   (equal (xrandr-line-refresh-rate line1) (xrandr-line-refresh-rate line2))))

(defun equal-refresh-ignore (intern extern)
   (equal (xrandr-line-resolution intern) (xrandr-line-resolution extern)))
   
(defun get-modes ()
  "parses the xrandr output to get the modes we get
at position 0 we have the extern and at position 1 we have the intern settings"
  (let* ((output (make-string-output-stream)))
    (sb-ext:run-program "/usr/bin/xrandr" '() :wait t :output output)
    (with-input-from-string (input-stream (get-output-stream-string output))
      (let ((array (make-array 2 :initial-element nil))
            (act-value 0))
        (loop for line = (read-line input-stream nil) while line do
             (multiple-value-bind (string value)
                 (cl-ppcre:scan-to-strings "([0-9x]+)\\s+([0-9.]+)\\s*(\\*)*\\s*(\\+)*\\s*" line)
               (cond
                 ((not (null (cl-ppcre:scan (concatenate 'string display-extern ".*") line)))
                  (setf act-value 1))
                 ((not (null (cl-ppcre:scan (concatenate 'string display-intern ".*") line)))
                  (setf act-value 0))
                 ((not (null value))
                  (let ((resolution (elt value 0))
                        (refresh (elt value 1))
                        (plus (elt value 2))
                        (star (elt value 3)))
                    (push (make-xrandr-line
                           :resolution resolution
                         :refresh-rate refresh
                         :star (if (null star)
                                   nil
                                   T)
                         :plus (if (null plus)
                                   nil
                                   T))
                        (elt array act-value)))))))
        array))))
         
(defun reduce-mode-to-compatible (modes)
  "reduces modes which can be used by both devices"
  (let* ((intern (elt modes 0))
         (extern (elt modes 1)))
    (let ((extern-filtered 
           (remove-if-not #'(lambda (x)
                          (find x intern :test #'equal-refresh-ignore))
                      extern))
          (intern-filtered
           (remove-if-not #'(lambda (x)
                          (find x extern :test #'equal-refresh-ignore))
                      intern)))
      (acons :intern intern-filtered
             (acons :extern extern-filtered nil)))))


(defun get-current-setup (modes)
  (xrandr-line-resolution 
   (find-if #'(lambda (x)
                (xrandr-line-star x))
            modes)))

(defun transform-xrandr-line-to-selection-alist (xrandr-list)
  (mapcar #'(lambda (x)
              (cons (xrandr-line-resolution x)
                    x))
          xrandr-list))

(defun ask-resolution (list)
  (stumpwm::select-from-menu (current-screen) list))

(defun set-resolution-fun (assoc-list)
  (let ((intern (xrandr-line-resolution (cdr (assoc :intern assoc-list))))
        (extern (cdr (assoc :extern assoc-list))))
    (let ((off-extern (equal extern off))
          (off-intern (equal intern off)))
    (echo-string (current-screen) (concatenate 'string "using: " intern))
                 (run-shell-command 
                  (if off-extern
                      (concatenate 'string "xrandr --output " display-intern " --mode " intern
                                   " --output " display-extern 
                                   " --off")
                      (if off-intern
                          (concatenate 'string "xrandr --output " display-intern " --off "
                                       " --output " display-extern 
                                       " --mode " (xrandr-line-resolution extern))
                          (concatenate 'string "xrandr --output " display-intern " --mode " intern " --output " display-extern " --mode "
                                       (xrandr-line-resolution extern))))))))

(defun get-optimal (modes)
  (cons "optimal/off" (find-if #'(lambda (x)
                               (xrandr-line-star x))
                           modes)))


(defun set-resolution-compatible ( &optional (beamer-enable-fn nothing-fn) (beamer-disable-fn nothing-fn))
  "gives a menu item which lets you choose resolution which is compatible to both monitors used on second call gives you optimal menu item which sets the intern display to the optimal value and the external to off
calls on-fun on beamer on and of-fun on back to optimal
WARNING: the beamer-enable-fn can be called multiple times"
  (let* ((modes (get-modes))
         (compatible-list (cdr (assoc :intern (reduce-mode-to-compatible modes)))))
    (if (null compatible-list) 
        (echo-string (current-screen) "no beamer found or no resolution compatible")
        (let ((ask-list (transform-xrandr-line-to-selection-alist compatible-list)))
          (when resolution-change-toggle
            (push (get-optimal (elt modes 0)) ask-list))
          (let ((selected (ask-resolution ask-list)))
            (when (not (null selected)) ;a cancel gives us a null value here
              (if (or 
                   (not resolution-change-toggle))
                   ;; (not (xrandr-line-star (cdr selected))))
                                        ;if we select a different resolution we want this compatible one and not an off for the external display
                  (progn
                    (set-resolution-fun (acons :extern (cdr selected)
                                               (acons :intern (cdr selected) nil)))
                    (funcall beamer-enable-fn)
                    (setf resolution-change-toggle t))
                  (progn
                    (set-resolution-fun (acons :extern off
                                               (acons :intern (cdr selected) nil))
                                        )
                    (funcall beamer-disable-fn)
                    (setf resolution-change-toggle nil)))))))))
    
(defun set-resolution-extern ()
  "sets the resolution best for the external display and disables the intern display"
  (let* ((modes (get-modes))
         (extern-modes (elt modes 0))
         (intern-modes (elt modes 1)))
      (if resolution-extern-change-toggle
          (set-resolution-fun (acons :intern
                                     (find-if #'(lambda (x)
                                                  (xrandr-line-plus x))
                                              intern-modes)
                                     (acons :extern off nil)))
          (set-resolution-fun (acons :intern off
                                     (acons :extern (find-if #'(lambda (x)
                                                                 (xrandr-line-plus x))
                                                             extern-modes)
                                            nil))))))

  
(defun dual-monitor (&optional (enable-fn nothing-fn) (disable-fn nothing-fn))
  "enables or disables the dual-monitor setup"
  (if dual-monitor-toggle
      (progn
        (run-shell-command (concatenate 'string "xrandr --output " display-extern " --off"))
        (setf dual-monitor-toggle nil)
        (funcall disable-fn))
      (progn
        (run-shell-command (concatenate 'string "xrandr --output " display-extern " --auto --right-of " display-intern))
        (setf dual-monitor-toggle t)
        (funcall enable-fn))))


(defcommand xrandr () ()
  "prints the xrandr information"
  (echo-string (current-screen)
               (run-shell-command "xrandr" t)))


;; helper functions for various off and on makings
(defun nothing-fn ()
  "helper function to do nothing if we don't specify a function"
  nil)


(defun screensaver-dpms-off ()
  "convinience function for killing screensaver+dpms"
  (run-shell-command "killall xscreensaver")
  (run-shell-command "xset -dpms"))

(defun screensaver-dpms-on ()
  "convinience function for enableing screensaver+dpms"
  (run-shell-command "xscreensaver")
  (run-shell-command "xset +dpms"))
