;;; perfect-margin.el --- Auto center windows, works with line numbers
;; Copyright (C) 2014 Randall Wang

;; Author: Randall Wang <randall.wjz@gmail.com>
;; Created: 19 Nov 2014
;; Version: 0.1
;; URL: https://github.com/mpwang/perfect-margin
;; Keywords: convenience, frames
;; Package-Requires: ((emacs "25.1"))

;; This file is *NOT* part of GNU Emacs.

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 2
;; of the License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANT ABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; # Usage
;;
;; Put perfect-margin under your Emacs load path, and add this to your init.el
;;
;; (require 'perfect-margin)
;;
;; Use `M-x perfect-margin-mode` to turn on/off perfect-margin.
;;
;; To make it permanent add this to your init.el after require.
;;
;; (perfect-margin-mode 1)
;;
;; Note: when using together with minimap or linum/display-line-numbers,
;; make sure you place config for perfect-margin *AFTER* minimap and linum.
;;
;; # Customization
;;
;; Via `M-x customize-group` and enter perfect-margin.
;;
;; Change `perfect-margin-visible-width` and `Apply and Save`. That's it.
;;
;; *Or* you can change the visible window width by setup `perfect-margin-visible-width` on the init.el.
;;
;; (setq perfect-margin-visible-width 128)
;;
;; By default both left and right margins are set, enable this option to only set the left margin of windows.
;;
;; (setq perfect-margin-only-set-left-margin t)
;;
;; # Additional binding on margin area
;;
;; You can place this in your init.el to make mouse wheel scroll on margin area just like it scroll on the visible window.
;;
;; (dolist (margin '("<left-margin> " "<right-margin> "))
;;   (global-set-key (kbd (concat margin "<mouse-1>")) 'ignore)
;;   (global-set-key (kbd (concat margin "<mouse-3>")) 'ignore)
;;   (dolist (multiple '("" "double-" "triple-"))
;;     (global-set-key (kbd (concat margin "<" multiple "wheel-up>")) 'mwheel-scroll)
;;     (global-set-key (kbd (concat margin "<" multiple "wheel-down>")) 'mwheel-scroll)))

;;; Code:
(require 'cl-lib)

;; linum-mode is deprecated since 26.1, use display-line-numbers-mode instead
(when (version< emacs-version "26.1")
  (require 'linum))

;;----------------------------------------------------------------------------
;; external definitions
;;----------------------------------------------------------------------------
(defvar linum-format)
(declare-function linum-update-current "linum")

(defvar minimap-width-fraction)
(defvar minimap-buffer-name)
(declare-function minimap-get-window "minimap")

(declare-function treemacs-get-local-window "treemacs-scope")

(defvar org-side-tree-display-side)
(declare-function org-side-tree-has-tree-p "org-side-tree")

;;----------------------------------------------------------------------------
;; group definitions
;;----------------------------------------------------------------------------
(defgroup perfect-margin nil
  "Auto center windows, work with minimap and/or linum-mode."
  :group 'emacs)

(defcustom perfect-margin-lighter " \u24c2"
  "Mode-line indicator for symbol `perfect-margin-mode'."
  :type '(choice (const :tag "No lighter" "") string)
  :safe 'stringp
  :group 'perfect-margin)

(defcustom perfect-margin-visible-width 128
  "The visible width of main window to be kept at center."
  :group 'perfect-margin
  :type 'number)

(defcustom perfect-margin-hide-fringes nil
  "Whether to set both fringes in all windows to 0."
  :group 'perfect-margin
  :type 'boolean)

(defcustom perfect-margin-only-set-left-margin nil
  "Set the left margin only, leave right margin untouched."
  :group 'perfect-margin
  :type 'boolean)

(defcustom perfect-margin-ignore-regexps
  '("^minibuf" "^[[:space:]]*\\*")
  "List of strings to determine if window is ignored.

Each string is used as regular expression to match the window buffer name."
  :group 'perfect-margin
  :type '(repeat regexp))

(defcustom perfect-margin-ignore-filters
  '(window-minibuffer-p)
  "List of functions to determine if window is ignored.

Each function is called with window as its sole arguemnt,
returning a non-nil value indicate to ignore the window."
  :group 'perfect-margin
  :type '(list function))

(defcustom perfect-margin-ignore-modes
  '(exwm-mode
    doc-view-mode
    nov-mode)
  "List of symbols of ignored major modes."
  :type '(repeat symbol)
  :group 'perfect-margin)

(defcustom perfect-margin-enable-debug-log nil
  "Enable output debug log."
  :group 'perfect-margin
  :type 'boolean)

;;----------------------------------------------------------------------------
;; env predictors
;;----------------------------------------------------------------------------
;; linum-mode is a minor mode
(defun perfect-margin-with-linum-p ()
  "Whether `linum-mode' is found and turn on."
  (bound-and-true-p linum-mode))

;; display-line-numbers-mode is a minor mode
(defun perfect-margin-with-display-line-numbers-p ()
  "Whether `display-line-numbers-mode' is found and turn on."
  (bound-and-true-p display-line-numbers-mode))

;; minimap-mode is a minor mode
(defun perfect-margin-with-minimap-p ()
  "Whether `minimap-mode' is found and turn on."
  (bound-and-true-p minimap-mode))

;; treemacs-mode is a function
(defun perfect-margin-with-treemacs-visible-p ()
  "Whether `treemacs-mode' is found and treemacs window is visible."
  (and
   (fboundp 'treemacs-mode)
   (fboundp 'treemacs-get-local-window)
   (treemacs-get-local-window)))

;; org-side-tree-mode is a major mode
(defun perfect-margin-with-org-side-tree-p ()
  "Whether `org-side-tree' is found."
  (and
   (fboundp 'org-side-tree)
   (fboundp 'org-side-tree-has-tree-p)))

;;----------------------------------------------------------------------------
;; Private functions
;;----------------------------------------------------------------------------
(defun perfect-margin--show-line-numbers-p ()
  "Whether line numbers are displayed."
  (or (perfect-margin-with-linum-p)
      (perfect-margin-with-display-line-numbers-p)))

(defun perfect-margin--default-left-margin ()
  "Default left margin."
  (if (perfect-margin--show-line-numbers-p) (if (perfect-margin-with-linum-p) 3 0) 0))

(defun perfect-margin--init-window-margins ()
  "Calculate target window margins as if there is only one window on frame."
  (let ((init-margin-width (round (max 0 (/ (- (frame-width) perfect-margin-visible-width) 2)))))
    (cons
     init-margin-width
     (if perfect-margin-only-set-left-margin 0 init-margin-width))))

(defun perfect-margin--left-adjacent-covered-p (a-win b-win)
  "If A-WIN is left adjacent to B-WIN."
  (let ((a-edges (window-edges a-win))
        (b-edges (window-edges b-win)))
    (and (= (nth 2 a-edges) (nth 0 b-edges))
         (<= (nth 1 a-edges) (nth 1 b-edges))
         (>= (nth 3 a-edges) (nth 3 b-edges)))))

(defun perfect-margin--get-right-margin (win &optional new-right-margin)
  "Return the value to be use as WIN's right margin.

If `perfect-margin-only-set-left-margin' is nil, return right margin of WIN.
If NEW-RIGHT-MARGIN is non-nil, return it, otherwise use default value."
  (cond
   (perfect-margin-only-set-left-margin (cdr (window-margins win)))
   (new-right-margin new-right-margin)
   (t (cdr (perfect-margin--init-window-margins)))))

(defun perfect-margin--get-min-margins (margin-candidates)
  "Find the maximums in the car and cdr positions of MARGIN-CANDIDATES.

If there are no cdr elements found, return nil for the max-second."
  ;; Example usage:
  ;; (perfect-margin--get-min-margins '((3 . 7) (5 . 6)))
  ;; It returns: (3 6)
  ;; (perfect-margin--get-min-margins '((3 . 7) (5)))
  ;; It returns: (3 nil)
  ;; (perfect-margin--get-min-margins '((3) (5)))
  ;; It returns: (3 nil)
  (let ((max-first nil)
        (max-second nil)
        (cdr-exists nil))
    (dolist (pair margin-candidates)
      (let ((car-val (car pair))
            (cdr-val (cdr pair)))
        (when (or (null max-first) (< car-val max-first))
          (setq max-first car-val))
        (when cdr-val
          (setq cdr-exists t)
          (when (or (null max-second) (< cdr-val max-second))
            (setq max-second cdr-val)))))
    ;; If no cdr exists then we set max-second to nil.
    (unless cdr-exists (setq max-second nil))
    (cons max-first max-second)))

(defun perfect-margin--auto-margin-ignore-p (win)
  "Conditions for filtering window (WIN) to setup margin."
  (let* ((buffer (window-buffer win))
         (name (buffer-name buffer)))
    (or (with-current-buffer buffer
          (apply #'derived-mode-p perfect-margin-ignore-modes))
        (cl-some #'identity
                 (nconc (mapcar (lambda (regexp) (string-match-p regexp name)) perfect-margin-ignore-regexps)
                        (mapcar (lambda (func) (funcall func win)) perfect-margin-ignore-filters)))
        (and (perfect-margin-with-minimap-p)
             (or (string-match minimap-buffer-name (buffer-name (window-buffer win)))
                 (perfect-margin--minimap-window-p win)))
        (and (perfect-margin-with-treemacs-visible-p)
             (eq win (treemacs-get-local-window)))
        (and (perfect-margin-with-org-side-tree-p)
             (with-current-buffer (window-buffer win)
               (eq major-mode 'org-side-tree-mode))))))

;;----------------------------------------------------------------------------
;; Minimap
;;----------------------------------------------------------------------------
(defun perfect-margin--minimap-window-p (win)
  "Judge if the window(WIN) is the minimap window itself, when it's live."
  (when (and (perfect-margin-with-minimap-p)
             (minimap-get-window)
             (window-live-p (minimap-get-window)))
    (let ((minimap-edges (window-edges (minimap-get-window)))
          (current-edges (window-edges win)))
      (and (= (nth 0 minimap-edges) (nth 0 current-edges))
           (= (nth 1 minimap-edges) (nth 1 current-edges))
           (= (nth 2 minimap-edges) (nth 2 current-edges))))))

(defun perfect-margin--minimap-left-adjacent-covered-p (win)
  "Judge if the window(WIN) is left adjacent to minimap window."
  (when (and (perfect-margin-with-minimap-p)
             (minimap-get-window)
             (window-live-p (minimap-get-window)))
    (perfect-margin--left-adjacent-covered-p (minimap-get-window) win)))

(defun perfect-margin-minimap-margin-window (win)
  "Setup window margins with minimap at different stage.

WIN will be any visible window, excluding the ignored windows."
  ;; Hint: do not reply on (window-width (minimap-get-window))
  (when (perfect-margin-with-minimap-p)
    (let ((init-window-margins (perfect-margin--init-window-margins))
          (win-edges (window-edges win)))
      (cond
       ;; minimap left adjacent
       ((perfect-margin--minimap-left-adjacent-covered-p win)
        (if (not (>= (nth 2 win-edges) (frame-width)))
            (cons (perfect-margin--default-left-margin) 0)
          (cons (max (perfect-margin--default-left-margin)
                     (- (car init-window-margins)
                        (round (* perfect-margin-visible-width minimap-width-fraction))))
                (perfect-margin--get-right-margin win))))
       ;; minimap right adjacent
       (t
        (cons (car init-window-margins)
              (perfect-margin--get-right-margin
               win
               (- (cdr init-window-margins)
                  (round (* perfect-margin-visible-width minimap-width-fraction))))))))))

;;----------------------------------------------------------------------------
;; Treemacs
;;----------------------------------------------------------------------------
(defun perfect-margin--treemacs-left-adjacent-covered-p (win)
  "Judge if the window(WIN) is left adjacent to treemacs window."
  (perfect-margin--left-adjacent-covered-p (treemacs-get-local-window) win))

(defun perfect-margin-treemacs-margin-window (win)
  "Setup treemacs window margins.

WIN will be any visible window, excluding the ignored windows."
  (when (perfect-margin-with-treemacs-visible-p)
    (let ((init-window-margins (perfect-margin--init-window-margins))
          (win-edges (window-edges win))
          (treemacs-window (treemacs-get-local-window)))
      (cond
       ((perfect-margin--treemacs-left-adjacent-covered-p win)
        (cons (max (perfect-margin--default-left-margin)
                   (- (car init-window-margins) (window-width treemacs-window) 2))
              (perfect-margin--get-right-margin win)))))))

;;----------------------------------------------------------------------------
;; Org-side-tree
;;----------------------------------------------------------------------------
(defun perfect-margin-org-side-tree-margin-window (win)
  "Setup org-side-tree window margins.

WIN will be any visible window, excluding the ignored windows."
  (when (perfect-margin-with-org-side-tree-p)
    (let* ((init-window-margins (perfect-margin--init-window-margins))
           (tree-buffer (org-side-tree-has-tree-p (window-buffer win)))
           (tree-window (if tree-buffer
                            (get-buffer-window tree-buffer)
                          (get-buffer-window "*Org-Side-Tree*"))))
      (when (and tree-window
                 (window-live-p tree-window))
        (cond
         ((and (eq org-side-tree-display-side 'left)
               (perfect-margin--left-adjacent-covered-p tree-window win))
          (message "1")
          (cons (max (perfect-margin--default-left-margin)
                     (- (car init-window-margins) (window-width tree-window)))
                (perfect-margin--get-right-margin win)))
         ((and (eq org-side-tree-display-side 'right)
               (perfect-margin--left-adjacent-covered-p win tree-window))
          (cons (car init-window-margins)
                (perfect-margin--get-right-margin
                 win
                 (- (cdr init-window-margins) (window-width tree-window))))))))))

;;----------------------------------------------------------------------------
;; Main
;;----------------------------------------------------------------------------
(defvar perfect-margin-margin-window-function-list
  '(perfect-margin-minimap-margin-window
    perfect-margin-treemacs-margin-window
    perfect-margin-org-side-tree-margin-window
    (lambda (win) (perfect-margin--init-window-margins))))

(defun perfect-margin-margin-windows ()
  "Setup margins, keep the visible main window always at center."
  (dolist (win (window-list))
    (unless (perfect-margin--auto-margin-ignore-p win)
      (let ((margin-candidates (thread-last
                                 perfect-margin-margin-window-function-list
                                 (mapcar (lambda (f) (funcall f win)))
                                 (remove nil)
                                 (remove t))))
        (when margin-candidates
          (let ((min-margins (perfect-margin--get-min-margins margin-candidates))
                (win-fringes (window-fringes win)))
            (when perfect-margin-enable-debug-log
              (message "%S candidaets: %S min-margins: %S" win margin-candidates min-margins))
            (set-window-margins win (car min-margins) (cdr min-margins))
            ;; draw the fringes inside the margin space
            ;; for package like git-gutter-fringe to display indicator near the line number
            (set-window-fringes win (nth 0 win-fringes) (nth 1 win-fringes) nil))))
      (when perfect-margin-hide-fringes
        (set-window-fringes win 0 0)))))

(defun perfect-margin-margin-frame (&optional _)
  "Hook to resize window when frame size change."
  (when (and (fboundp 'frame-size-changed-p)
             (frame-size-changed-p))
    (perfect-margin-margin-windows)))

;;----------------------------------------------------------------------------
;; Advice
;;----------------------------------------------------------------------------
(defun perfect-margin--linum-format (line)
  "Function for `linum-format' to set left margin for LINE to be 3 as maximum."
  (propertize
   (format (concat "%" (number-to-string (max 3 (length (number-to-string line)))) "d") line)
   'face
   'linum))

(defvar perfect-margin--linum-update-win-left-margin nil
  "Variable to store original window marings before `linum-update-window'.")

(defadvice linum-update-window (before perfect-margin-linum-update-before (win))
  "Save window's original left margin."
  (setq perfect-margin--linum-update-win-left-margin (or (car (window-margins win)) 0)))

(defadvice linum-update-window (after perfect-margin-linum-update-after (win))
  "Restore windonw's original left margin, as `linum-update-window' always reset left margin."
  (set-window-margins win perfect-margin--linum-update-win-left-margin (cdr (window-margins win))))

(defadvice minimap-update (after minimap-update-no-fringe nil)
  "Prevent fringe overlay of target buffer from drawing on `minimap-window'."
  (when (and  (minimap-get-window)
              (window-live-p (minimap-get-window))
              minimap-hide-fringes)
    (set-window-fringes (minimap-get-window) 0 0)))

(defadvice split-window (before perfect-margin--disable-margins nil)
  "Adjust all existing windows before 'split-window' is called."
  (dolist (win (window-list))
    (set-window-margins win 0 0)
    (when perfect-margin-hide-fringes
      (set-window-fringes win 0 0))))

;;----------------------------------------------------------------------------
;; MINOR mode definition
;;----------------------------------------------------------------------------
;;;###autoload
(define-minor-mode perfect-margin-mode
  "Auto center windows."
  :init-value nil
  :lighter perfect-margin-lighter
  :global t
  (if perfect-margin-mode
      ;; add hook and activate
      (progn
        (when (perfect-margin-with-linum-p)
          (ad-activate 'linum-update-window)
          (when (eq linum-format 'dynamic)
            (setq linum-format 'perfect-margin--linum-format)))
        (when (perfect-margin-with-minimap-p)
          (ad-activate 'minimap-update))
        (ad-activate 'split-window)
        (add-hook 'window-configuration-change-hook 'perfect-margin-margin-windows)
        (add-hook 'window-size-change-functions 'perfect-margin-margin-frame)
        (perfect-margin-margin-windows))
    ;; remove hook and restore margin
    (when (perfect-margin-with-linum-p)
      (ad-deactivate 'linum-update-window)
      (when (eq linum-format 'perfect-margin--linum-format)
        (setq linum-format 'dynamic))
      (linum-update-current))
    (when (perfect-margin-with-minimap-p)
      (ad-deactivate 'minimap-update))
    (ad-deactivate 'split-window)
    (remove-hook 'window-configuration-change-hook 'perfect-margin-margin-windows)
    (remove-hook 'window-size-change-functions 'perfect-margin-margin-frame)
    (dolist (window (window-list))
      (set-window-margins window 0 0))))

(provide 'perfect-margin)

;;; perfect-margin.el ends here
