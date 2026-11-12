;;; iscroll.el --- Smooth scrolling over images      -*- lexical-binding: t; -*-

;; Author: Yuan Fu <casouri@gmail.com>
;; Maintainer: Yuan Fu <casouri@gmail.com>
;; URL: https://github.com/casouri/iscroll
;; Version: 1.0.0
;; Keywords: convenience, image
;; Package-Requires: ((emacs "26.0"))

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; Gone are the days when images jumps in and out of the window when
;; scrolling! This package makes scrolling over images as if the image
;; is made of many lines, instead of a single line. (Indeed, sliced
;; image with default scrolling has the similar behavior as what this
;; package provides.)
;;
;; To use this package:
;;
;;     M-x iscroll-mode RET
;;
;; This mode remaps mouse scrolling functions and `next/previous-line'.
;; If you use other commands, you need to adapt them accordingly. See
;; `iscroll-mode-map' and `iscroll-mode' for some inspiration.
;;
;; You probably don't want to enable this in programming modes because
;; it is slower than normal scrolling commands.
;;
;; If a line is taller than double the default line height, smooth
;; scrolling is triggered and Emacs will reveal one line’s height each
;; time.
;;
;; Line motion (`iscroll-next-line', `iscroll-previous-line') takes a
;; fast path when no image is in or near the window: it calls the
;; built-in `line-move' directly, so it's as fast as plain `next-line'
;; away from images.  See `iscroll--fast-path-p'.
;;
;; Commands provided:
;;
;; - iscroll-up
;; - iscroll-down
;; - iscroll-next-line
;; - iscroll-previous-line
;;

;;; Code:
;;

(require 'cl-lib)

(defvar iscroll-preserve-screen-position
  scroll-preserve-screen-position
  "Whether to preserve screen position when scrolling.
\(I want to control this behavior for iscroll separately.)")

(defun iscroll-up (&optional arg)
  "Scroll up ARG lines.
Normally just calls `scroll-up'. But if the top of the window is
an image, scroll inside the image. Return the number of logical
lines scrolled."
  (interactive "p")
  (let ((arg (or arg 1))
        (display-lines-scrolled 0)
        (original-point (point))
        (scroll-amount nil)
        (need-to-recalculate-img-height t)
        (preserve-screen-pos iscroll-preserve-screen-position)
        img-height
        hit-end-of-buffer)
    ;; 1) We first do a dry-run: not actually scrolling, just moving
    ;; point and modifying SCROLL-AMOUNT.
    (goto-char (window-start))
    (while (> arg 0)
      ;; Initialize SCROLL-AMOUNT when we arrived at a new line or
      ;; first entered the command.
      (when (null scroll-amount)
        (setq scroll-amount (window-vscroll nil t)))
      ;; `line-pixel-height' is expensive so we try to call it as less
      ;; as possible.
      (when need-to-recalculate-img-height
        (setq img-height (line-pixel-height)
              need-to-recalculate-img-height nil))
      ;; Scroll.
      (if (and (>= img-height (* 2 (default-line-height)))
               (< scroll-amount img-height))
          ;; If we are in the middle of scrolling an image, scroll
          ;; that image.
          (setq scroll-amount
                (min (+ scroll-amount (default-line-height))
                     img-height))
        ;; If we are not on an image or the image is scrolled over,
        ;; scroll display line.
        (cl-incf display-lines-scrolled)
        (setq need-to-recalculate-img-height t)
        ;; We hit the end of buffer, stop.
        (when (not (eq (vertical-motion 1) 1))
          (setq hit-end-of-buffer t)
          (setq arg 0))
        (setq scroll-amount nil))
      (cl-decf arg))
    ;; 2) Finally, we’ve finished the dry-run, apply the result.
    ;;
    ;; The third argument `t' tells redisplay that (point) doesn't
    ;; have to be the window start and completely visible. That
    ;; allows our vscroll value to survive.
    (set-window-start nil (point) t)
    (if scroll-amount
        (set-window-vscroll nil scroll-amount t)
      (set-window-vscroll nil 0 t))
    ;; 3) Misc stuff.
    ;;
    ;; If the original point is after window-start, it is in the
    ;; visible portion of the window, and is safe to go back to.
    (if (> original-point (window-start))
        (goto-char original-point)
      ;; If not, we just stay at current position, i.e. window-start.
      (setq preserve-screen-pos nil))
    ;; (Maybe) move point to preserve screen position.
    (when preserve-screen-pos
      (vertical-motion display-lines-scrolled))
    ;; Show “error message”.
    (when hit-end-of-buffer
      (message "%s" (error-message-string '(end-of-buffer))))
    display-lines-scrolled))

(defun iscroll-down (&optional arg)
  "Scroll down ARG lines.
Normally just calls `scroll-down'. But if the top of the window is
an image, scroll inside the image. Return the number of logical
lines scrolled. If PRESERVE-SCREEN-POS non-nil, try to preserve
screen position."
  (interactive "p")
  (let ((arg (or arg 1))
        (display-lines-scrolled 0)
        (original-point (point))
        ;; Nil means this needs to re-measured.
        (scroll-amount nil)
        (preserve-screen-pos iscroll-preserve-screen-position)
        hit-beginning-of-buffer)
    ;; 1) Dry-run.
    (goto-char (window-start))
    (while (> arg 0)
      (when (null scroll-amount)
        (setq scroll-amount (window-vscroll nil t)))
      (let ((img-height (line-pixel-height)))
        (if (and (>= img-height (* 2 (default-line-height)))
                 (> scroll-amount 0))
            ;; Scroll image.
            (setq scroll-amount
                  (- scroll-amount (default-line-height)))
          ;; Scroll display line.
          (when (not (eq (vertical-motion -1) -1))
            ;; If we hit the beginning of buffer, stop.
            (setq hit-beginning-of-buffer t
                  arg 0))
          (cl-incf display-lines-scrolled)
          ;; If the line we stopped at is an image, we don't want to
          ;; show it completely, instead, modify vscroll and only
          ;; show a bottom strip of it. If we are at the beginning
          ;; of the buffer and `vertical-motion' returns 0, we don't
          ;; want to do this.
          (let ((img-height (line-pixel-height)))
            (if (>= img-height (* 2 (default-line-height)))
                (setq scroll-amount (- img-height (default-line-height)))
              (setq scroll-amount nil)))))
      (cl-decf arg))
    ;; 2) Apply result.
    (set-window-start nil (point) t)
    (if scroll-amount
        (set-window-vscroll nil scroll-amount t)
      (set-window-vscroll nil 0 t))
    ;; 3) Misc
    ;;
    ;; HACK: There is no fast and reliable way to get the last visible
    ;; point, hence this hack: move point up until it is visible.
    (goto-char original-point)
    ;; Checking point > window-start is important, otherwise we could
    ;; fall into infinite loop. E.g., when point = window-start and
    ;; under the point is an image that is not completely visible.
    (while (and (> (point) (window-start))
                (not (pos-visible-in-window-p (point))))
      (setq preserve-screen-pos nil)
      (vertical-motion -2))
    (when (and preserve-screen-pos (not hit-beginning-of-buffer))
      (vertical-motion (- display-lines-scrolled)))
    (when hit-beginning-of-buffer
      (message "%s" (error-message-string '(beginning-of-buffer))))
    display-lines-scrolled))

(defvar iscroll--goal-column nil
  "Goal column when scrolling.")

(defun iscroll--current-column ()
  "Return the current column of point in current screen line.
‘current-column’ counts columns from logical line beginning, this
function counts from visual line beginning."
  (save-excursion
    (let ((col (current-column)))
      ;; Go to visual line beginning.
      (vertical-motion 0)
      (- col (current-column)))))

(defvar iscroll--last-move-fast nil
  "Non-nil if the last line motion took the fast path.")

(defvar-local iscroll--saved-scroll-margin nil
  "How to restore `scroll-margin' after `iscroll--suspend-scroll-margin'.
A list holding the previous buffer-local value, or the symbol `global'
if `scroll-margin' had no buffer-local value.")

(defun iscroll--saved-scroll-margin-value ()
  "Return the `scroll-margin' that `iscroll--suspend-scroll-margin' saved."
  (if (consp iscroll--saved-scroll-margin)
      (car iscroll--saved-scroll-margin)
    (default-value 'scroll-margin)))

(defun iscroll--maybe-restore-scroll-margin ()
  "Restore a suspended `scroll-margin' once doing so won't scroll.
That is, once point is at least that many screen lines from both the
top and the bottom of the window.  Restoring it earlier would make
redisplay scroll the text in one jump to re-establish the margin."
  (when iscroll--saved-scroll-margin
    (let* ((margin (iscroll--saved-scroll-margin-value))
           (line (1- (count-screen-lines (window-start) (point) t)))
           (lines-below (- (window-body-height) line 1)))
      (when (and (>= line margin) (>= lines-below margin))
        (iscroll--suspend-scroll-margin nil)))))

(defun iscroll--suspend-scroll-margin (suspend)
  "Set `scroll-margin' to 0 in this buffer if SUSPEND, else restore it.
Near images iscroll scrolls the window one line at a time and may leave
point at the window edge; a non-zero `scroll-margin' makes redisplay
scroll again to restore the margin, which undoes the smooth scroll and
moves images in and out of view in one jump.  The margin comes back
\(see `iscroll--maybe-restore-scroll-margin') once it can do so without
scrolling."
  (if suspend
      (unless iscroll--saved-scroll-margin
        (setq iscroll--saved-scroll-margin
              (if (local-variable-p 'scroll-margin)
                  (list scroll-margin)
                'global))
        (setq-local scroll-margin 0))
    (when iscroll--saved-scroll-margin
      (if (consp iscroll--saved-scroll-margin)
          (setq-local scroll-margin (car iscroll--saved-scroll-margin))
        (kill-local-variable 'scroll-margin))
      (setq iscroll--saved-scroll-margin nil))))

(defun iscroll--image-display-p (display)
  "Return non-nil if the `display' property value DISPLAY shows an image."
  (or (eq (car-safe display) 'image)
      ;; A list or vector of display specs, e.g. ((slice ...) (image ...)).
      (and (or (consp display) (vectorp display))
           (not (keywordp (car-safe display)))
           (seq-some (lambda (spec) (eq (car-safe spec) 'image)) display))))

(defun iscroll--image-between-p (beg end)
  "Return non-nil if an image is displayed anywhere between BEG and END.
Checks overlays and `display' text properties only; does no layout,
so it is cheap compared with measuring line heights."
  (or (seq-some (lambda (ov)
                  (iscroll--image-display-p (overlay-get ov 'display)))
                (overlays-in beg end))
      (let ((pos beg)
            found)
        (while (and (not found) (< pos end))
          (setq found (iscroll--image-display-p
                       (get-text-property pos 'display))
                pos (next-single-property-change pos 'display nil end)))
        found)))

(defun iscroll--fast-path-p (arg)
  "Return non-nil if moving ARG lines can use plain `line-move'.
That's the case unless the window is scrolled part-way into a line
\(non-zero vscroll) or an image is displayed in the window or within
ARG lines past its top or bottom edge, i.e. anywhere this motion could
scroll into view.  The range is measured from the window edges, not
from point: with a `scroll-margin', the window scrolls while point is
still that many lines from the edge."
  (and (zerop (window-vscroll nil t))
       (let* ((ws (window-start))
              (we (or (window-end) (point-max)))
              (beg (if (< arg 0)
                       (save-excursion (goto-char ws) (forward-line (1- arg)) (point))
                     ws))
              (end (if (> arg 0)
                       (save-excursion (goto-char we) (forward-line (1+ arg)) (point))
                     we)))
         (not (iscroll--image-between-p beg end)))))

(defun iscroll--fast-line-move (arg first-command-p)
  "Move ARG lines with the built-in `line-move' (no images nearby).
FIRST-COMMAND-P is non-nil if the previous command wasn't an iscroll
line motion.  Keeps the goal column in step with the slow path."
  ;; Continuing from a slow-path step: hand iscroll's goal column to
  ;; `line-move', which keeps its own in `temporary-goal-column'.
  (when (and (not first-command-p)
             (not iscroll--last-move-fast)
             iscroll--goal-column)
    (setq temporary-goal-column
          (cons (float iscroll--goal-column) (window-hscroll))))
  ;; `line-move' keeps the goal column only after `next-line' or
  ;; `previous-line' (or the same command), so present iscroll's line
  ;; commands as such.
  (let ((last-command (if first-command-p last-command 'next-line)))
    ;; Like the slow path, report hitting the buffer boundary as a
    ;; message rather than an error.
    (condition-case err
        (line-move arg)
      ((beginning-of-buffer end-of-buffer)
       (message "%s" (error-message-string err)))))
  ;; Hand the goal column back for a following slow-path step.
  (setq iscroll--goal-column (and (consp temporary-goal-column)
                                  (round (car temporary-goal-column)))
        iscroll--last-move-fast t))

(defun iscroll-forward-line (&optional arg)
  "Smooth `forward-line'.
ARG is the number of lines to move.  Uses plain `line-move' when no
image is nearby (see `iscroll--fast-path-p'), and otherwise scrolls
smoothly over images."
  (interactive "^p")
  (let ((arg (or arg 1)))
    (if (iscroll--fast-path-p arg)
        (progn
          (iscroll--fast-line-move
           arg (not (memq last-command
                          '(iscroll-next-line iscroll-previous-line))))
          (iscroll--maybe-restore-scroll-margin))
      (iscroll--suspend-scroll-margin t)
      (setq iscroll--last-move-fast nil)
      (iscroll--slow-forward-line arg))))

(defun iscroll--slow-forward-line (&optional arg)
  "Move ARG lines, scrolling smoothly over images.
This is the original `iscroll-forward-line'."
  (let* ((arg (or arg 1))
         (abs-arg (abs arg))
         (step (if (> arg 0) 1 -1))
         (scroll-fn (if (> arg 0)
                        #'iscroll-up
                      #'iscroll-down))
         (old-point (point))
         (first-command-p (not (memq last-command
                                     '(iscroll-next-line
                                       iscroll-previous-line))))
         ;; Calculate the goal column. The goal column is either
         ;; inherited from previous calls to this command, or
         ;; calculated by visual column.
         (goal-column (if (or first-command-p (not iscroll--goal-column))
                          (setq iscroll--goal-column
                                (iscroll--current-column))
                        (or iscroll--goal-column 0)))
         ;; We don't want to preserve screen position when moving point.
         (iscroll-preserve-screen-position nil)
         hit-boundary)
    ;; Because in most cases we move into visible portions, we move
    ;; first and check after, this should be faster than check first
    ;; and move after.
    (while (> abs-arg 0)
      ;; Move point. `move-to-column' counts columns from logical line
      ;; beginnings and `vertical-motion' counts columns from visual
      ;; beginnings. So `vertical-motion' works with line-wrapping but
      ;; `move-to-column' does not.
      (when (not (eq (vertical-motion (cons iscroll--goal-column step))
                     step))
        ;; If we hit beginning or end of buffer, stop.
        (setq hit-boundary t
              abs-arg 0))
      (when (not (pos-visible-in-window-p (point)))
        ;; The new point is not fully visible! Scroll up/down one line
        ;; to try to accommodate that line.
        (funcall scroll-fn 1))
      ;; We scrolled one line but that line is still not fully
      ;; visible, move the point back so that redisplay doesn’t force
      ;; the whole line into visible region. Partially visible is ok,
      ;; completely invisible is not ok.
      (when (and (not (pos-visible-in-window-p (point)))
                 ;; If the image is taller than the window and is the
                 ;; first row of the window, it is ok to leave point
                 ;; on it.
                 (<= (line-pixel-height) (window-text-height nil t)))
        (goto-char old-point)
        (setq hit-boundary nil))
      (cl-decf abs-arg))
    ;; If we hit buffer boundary and didn’t back off, show “error
    ;; message”.
    (when hit-boundary
      (message "%s" (error-message-string
                     (list (if (> arg 0)
                               'end-of-buffer
                             'beginning-of-buffer)))))))

(defun iscroll-next-line (&optional arg _)
  "Smooth `next-line'.
ARG is the number of lines to move."
  (interactive "^p")
  (iscroll-forward-line arg))

(defun iscroll-previous-line (&optional arg _)
  "Smooth `previous-line'.
ARG is the number of lines to move."
  (interactive "^p")
  (iscroll-forward-line (- (or arg 1))))

(defvar iscroll-mode-map (make-sparse-keymap)
  "Minor mode map for `iscroll-mode'.")

;;;###autoload
(define-minor-mode iscroll-mode
  "Smooth scrolling over images."
  :lighter " IS"
  :keymap iscroll-mode-map
  :group 'scrolling
  (if iscroll-mode
      (progn
        (setq-local mwheel-scroll-up-function #'iscroll-up
                    mwheel-scroll-down-function #'iscroll-down)
        ;; We don’t remap next/previous-line in the minor mode map
        ;; because that shallows ivy’s binding.
        (local-set-key [remap next-line] #'iscroll-next-line)
        (local-set-key [remap previous-line] #'iscroll-previous-line))
    (iscroll--suspend-scroll-margin nil)
    (kill-local-variable 'mwheel-scroll-up-function)
    (kill-local-variable 'mwheel-scroll-down-function)
    (local-set-key [remap next-line] nil)
    (local-set-key [remap previous-line] nil)))

(provide 'iscroll)

;;; iscroll.el ends here
