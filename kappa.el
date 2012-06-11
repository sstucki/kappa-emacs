;;; kappa.el -- major mode for Kappa models

;; Copyright (C) 2012 Sandro Stucki, Sebastian Jaramillo,
;;                    Ricardo Honorato-Zimmer

;; Authors:
;;   Sandro Stucki <sandro.stucki@ed.ac.uk>
;;   Sebastian Jaramillo <sebajarar@gmail.com>
;;   Ricardo Honorato-Zimmer <rikardo.horo@gmail.com>


;; This file is not part of GNU Emacs.

;; This file is free software: you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.

;; This file is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this file.  If not, see <http://www.gnu.org/licenses/>.


;;; Commentary:

;; A major mode for editing Kappa models.  The mode knows enough about
;; Kappa syntax to do some basic fontification but does currently not
;; do indentation or proper slashification.

;; There are numerous font face customization variables.


;;; Known bugs/limitations:

;; * There seems to be a bug when using Kappa mode at the same time as
;;   CEDET causing Emacs to fontify the entire buffer with
;;   font-lock-comment-face.


;;; To Do:

;; * Fix comment font-lock weirdness.
;; * Support for indentation and slashification. Hard.
;; * Documentation.
;; * Remove dependency on sed(1). Yes, this is a hack. Sorry.
;;   To solve this the best way would be to ask Jean to remove
;;   the '#' from KaSim's output


;;; Customization stuff

;; Customization group for Kappa mode.
(defgroup kappa nil
  "Kappa mode customization."
  :group 'languages)

;; Customization of fontification

(defface kappa-keyword-face
  '((t :inherit font-lock-preprocessor-face))
  "Face to use for highlighting Kappa keywords such as \"%var\"
or \"%init\" in Font-Lock mode."
  :group 'faces
  :group 'kappa)

(defface kappa-command-face
  '((t :inherit font-lock-keyword-face))
  "Face to use for highlighting Kappa commands such as \"$ADD\"
or \"$STOP\" in Font-Lock mode."
  :group 'faces
  :group 'kappa)

(defface kappa-rule-operator-face
  '((t :inherit font-lock-builtin-face))
  "Face to use for highlighting the Kappa rule operators \"@\"
and \"->\" in Font-Lock mode."
  :group 'faces
  :group 'kappa)

(defface kappa-math-operator-face
  '((t :inherit font-lock-builtin-face))
  "Face to use for highlighting algebraic and logic operators
such as \"+\" or \"/\" in Kappa expressions in Font-Lock mode."
  :group 'faces
  :group 'kappa)

(defface kappa-interface-symbol-face
  '((t :inherit font-lock-builtin-face))
  "Face to use for highlighting the Kappa agent interface symbols
such as \"!\" or \"~\" in Font-Lock mode."
  :group 'faces
  :group 'kappa)

(defface kappa-builtin-face
  '((t :inherit font-lock-constant-face))
  "Face to use for highlighting built-in functions such as
\"[sin]\" or \"[mod]\" in Kappa expressions in Font-Lock mode."
  :group 'faces
  :group 'kappa)

(defface kappa-constant-face
  '((t :inherit font-lock-constant-face))
  "Face to use for highlighting numerical and logical constants
such as \"[pi]\" or \"[true]\" in Kappa expressions in Font-Lock
mode."
  :group 'faces
  :group 'kappa)

(defface kappa-agent-name-face
  '((t :inherit font-lock-function-name-face))
  "Face to use for highlighting Kappa agent names in Font-Lock
mode."
  :group 'faces
  :group 'kappa)

(defface kappa-site-name-face
  '((t nil))
  "Face to use for highlighting Kappa site names in Font-Lock
mode."
  :group 'faces
  :group 'kappa)

(defface kappa-link-label-face
  '((t :inherit font-lock-type-face))
  "Face to use for highlighting the labels of links between Kappa
sites in Font-Lock mode."
  :group 'faces
  :group 'kappa)

(defface kappa-internal-state-face
  '((t :inherit font-lock-variable-name-face))
  "Face to use for highlighting the internal state names of Kappa
sites in Font-Lock mode."
  :group 'faces
  :group 'kappa)

(defface kappa-string-face
  '((t :inherit font-lock-string-face))
  "Face to use for highlighting string literals such as Kappa
variables in Font-Lock mode."
  :group 'faces
  :group 'kappa)

(defface kappa-cellbreak-face
  '((t (:background (face-background font-lock-string-face)
		    :foreground (face-foreground font-lock-string-face)
		    :overline t
		    :bold t)))
  "*Face to use for cellbreak ## lines."
  :group 'faces
  :group 'kappa
  )

;; Variable definitions from face definitions
(defvar kappa-keyword-face 'kappa-keyword-face
  "Face for highlighting Kappa keywords.")
(defvar kappa-command-face 'kappa-command-face
  "Face for highlighting Kappa commands.")
(defvar kappa-rule-operator-face 'kappa-rule-operator-face
  "Face for highlighting Kappa rule operators.")
(defvar kappa-math-operator-face 'kappa-math-operator-face
  "Face for highlighting math operators in Kappa mode.")
(defvar kappa-interface-symbol-face 'kappa-interface-symbol-face
  "Face for highlighting Kappa agent interface symbols.")
(defvar kappa-builtin-face 'kappa-builtin-face
  "Face for highlighting built-in functions in Kappa mode.")
(defvar kappa-constant-face 'kappa-constant-face
  "Face for highlighting constants in Kappa mode.")
(defvar kappa-agent-name-face 'kappa-agent-name-face
  "Face for highlighting Kappa agent names.")
(defvar kappa-site-name-face 'kappa-site-name-face
  "Face for highlighting Kappa site names.")
(defvar kappa-link-label-face 'kappa-link-label-face
  "Face for highlighting Kappa link labels.")
(defvar kappa-internal-state-face 'kappa-internal-state-face
  "Face for highlighting Kappa internal state names.")
(defvar kappa-string-face 'kappa-string-face
  "Face for highlighting string literals in Kappa mode.")
(defvar kappa-cellbreak-face 'kappa-cellbreak-face
  "Face for making a cellbrake on ## lines.")

;;; Simulation related customization variables

(defcustom kappa-sim-executable-path "/usr/bin/KaSim"
  "File system path to the Kappa simulator executable (default is
'/usr/bin/KaSim')."
  :type 'file
  :group 'kappa)

(defcustom kappa-default-sim-time 200
  "Default simulation duration."
  :type 'number
  :group 'kappa)

(defcustom kappa-default-sim-events 10000
  "Default number of events to produce per simulation."
  :type 'number
  :group 'kappa)

(defcustom kappa-default-sim-points 500
  "Default number of points to produce per simulation."
  :type 'number
  :group 'kappa)

(defcustom kappa-comment-region-s "% $$$ "
  "*String inserted by \\[kappa-comment-region] at start of each line in \
region."
  :group 'kappa
  :type 'string)


;; Variables to remember the values of the arguments of previous
;; invocation of `kappa-run-sim' and `kappa-plot-sim'.
(defvar kappa-prev-sim-output-file ""
  "Value of the `output' or `file-path' argument during the
  previous invocation of `kappa-run-sim' or
  `kappa-plot-sim', respectively.")
(defvar kappa-prev-sim-time kappa-default-sim-time
  "Value of the `time' argument during the previous invocation of
  `kappa-run-sim'.")
(defvar kappa-prev-sim-events kappa-default-sim-events
  "Value of the `events' argument during the previous invocation
  of `kappa-run-sim'.")
(defvar kappa-prev-sim-points kappa-default-sim-points
  "Value of the `points' argument during the previous invocation
  of `kappa-run-sim'.")
(defvar kappa-prev-plot-columns "1"
  "Value of the `columns' argument during the previous invocation
  of `kappa-plot-sim'.")


;;; Local key map

(defvar kappa-mode-keymap (make-sparse-keymap)
  "Kappa major mode keymap.")

;; Add shortcuts for `kappa-run-sim' and `kappa-plot-sim'.
(define-key kappa-mode-keymap "\C-c\C-r" 'kappa-run-sim)
(define-key kappa-mode-keymap "\C-c\C-p" 'kappa-plot-sim)


;;; Main Kappa mode definition (using generic)

(define-generic-mode kappa-mode

  '(?#)      ;; Comments start with '#'
  
  nil ;; Handle keywords using font-lock rules (see below)
  
  ;; Rules for syntax highlighting (font-lock)
  (eval-when-compile
    (let
        ;; Common lexical sub-expressions used in keywords
        ((id "[A-Za-z0-9][A-Za-z0-9_-]*")   ;; IDs/names as defined in
                                            ;; the Kappa spec
         (num "[0-9]+")                     ;; Integer numerals
         (ws "[ \t]*"))                     ;; Whitespace

      (list

       ;; ## comments
       (list "^\\s-*\\(\##[^\n]*\n\\)" '(1 kappa-cellbreak-face append))
       
       ;; Keywords
       (cons
        (regexp-opt
         '("%agent:" "%var:" "%plot:" "%obs:" "%init:" "%mod:"))
        kappa-keyword-face)

       ;; Commands
       (cons
        (regexp-opt
         '("$ADD" "$DEL" "$SNAPSHOT" "$STOP"))
        kappa-command-face)

       ;; Built-in functions
       (cons
        (regexp-opt
         '("[not]" "[log]" "[sin]" "[cos]" "[tan]" "[sqrt]" "[mod]"
           "[exp]" "[int]"))
        kappa-builtin-face)

       ;; Symbolic numerical constants
       (cons
        (regexp-opt
         '("[E]" "[T]" "[inf]" "[pi]" "[emax]" "[tmax]" "[true]"
           "[false]"))
        kappa-constant-face)

       ;; Agent interface symbols
       '("[?!~]" . kappa-interface-symbol-face)

       ;; Internal state names
       (list (concat "~" ws "\\(" id "\\)")
             1 kappa-internal-state-face)

       ;; Link labels
       (list (concat "!" ws "\\(" num          ;; Numeric label
                     "\\|" id ws "\\." ws id   ;; Remote site name
                     "\\|" ws "_\\)")          ;; Wildcard
             1 kappa-link-label-face)

       ;; Variable names
       '("'[^'\n]+'" . kappa-string-face)

       ;; Agent names followed by an interface spec and site names
       (list (concat "\\(" id "\\)" ws "(")      ;; Agents
             '(1 kappa-agent-name-face)
             (list                                ;; Site interface
              (concat "\\=" ws "\\(" id "\\)[^,)\n]*"
                      "\\(," ws "\\([^A-Za-z0-9,)\n][^,)\n]*\\)?\\)*")
              nil nil
              '(1 kappa-site-name-face)))

       ;; Numerals
       (cons (concat "\\<\\(\\(" num "\\)?\\." num "\\([Ee][+-]?" num
                     "\\)?\\|" num "\\)\\>")
             kappa-constant-face)

       ;; Agent names not followed by an interface spec (all
       ;; remaining IDs)
       (cons id kappa-agent-name-face)
       ;; '("\\.\\.\\." . kappa-agent-name-face)
       
       ;; Rule operators
       '("@\\|->" . kappa-rule-operator-face)

       ;; Math operators
       '("&&\\|||\\|[+*/^:=<>-]" . kappa-math-operator-face))))

  ;; File suffixes for which to activate this mode 
  '("\\.ka\\'")

  ;; Activate Kappa mode key map.
  '(kappa-mode-setup)

  "Major mode for editing Kappa models.

\\{kappa-mode-keymap}
Turning on Kappa mode runs the hook `kappa-mode-hook'.
")

(defun kappa-mode-setup ()
  "Set up the Kappa major mode."
  (use-local-map kappa-mode-keymap))   ;; Install local key map.

;;; Font lock tweaking

(defun kappa-font-lock-adjustments ()
  "Some changes to font-lock. Inspired in matlab-mode."
  (progn 
    ;; cellbreak variable faces
    (cond ((facep 'font-comment-face)
	   (copy-face 'font-lock-comment-face 'kappa-cellbreak-face))
	  (t
	   (make-face 'kappa-cellbreak-face)))
    (set-face-bold-p 'kappa-cellbreak-face t)
    (condition-case nil
	(set-face-attribute 'kappa-cellbreak-face nil :overline t)
      (error nil))))

(add-hook 'font-lock-mode-hook 'kappa-font-lock-adjustments)

;;; Simulation related functions.

(defvar *buffer-counter* 1)

(defun get-dirname (path)
  (mapconcat 'identity (butlast (split-string path "/") 1) "/"))

(defun get-filename (path)
  (car (last (split-string path "/"))))

(defun kappa-run-sim (input output time events points)
  "Input:
  INPUT: File path to the Kappa model.
  OUTPUT: Path to the simulation output file.
  TIME: Time (integer). Default is the value of
        `kappa-default-sim-time'.
  EVENTS: Number of events (integer). Default is the value of
          `kappa-default-sim-events'.
  POINTS: Number of points (integer). Default is the value of
          `kappa-default-sim-points'.

Output: none.

Side Effects: Creates *Simulation* buffer, print the arguments
passed to *Messages* and run `kappa-sim-executable-path' in shell
with the appropriate arguments.

Related variables: `kappa-sim-executable-path',
`kappa-default-sim-time', `kappa-default-sim-events',
`kappa-default-sim-points'.
"
  (interactive
    (list (read-file-name "Input: " (file-truename buffer-file-name))
          (read-file-name "Output: "
                          (concat (get-dirname kappa-prev-sim-output-file) "/")
                          nil nil (get-filename kappa-prev-sim-output-file))
          (read-number "Time: " kappa-prev-sim-time)
          (read-number "Events: " kappa-prev-sim-events)
          (read-number "Points: " kappa-prev-sim-points)))

  (setq kappa-prev-sim-output-file output)
  (setq kappa-prev-sim-time time)
  (setq kappa-prev-sim-events events)
  (setq kappa-prev-sim-points points)

  ;; FIXME: Would be nice to eliminate the dependency on sed(1).
  (let ((command (concat kappa-sim-executable-path " -i " input
                         " -o " (get-filename output)
                         " -d " (get-dirname  output)
                         (cond
                           ((> time 0)   (format " -t %s" time))
                           ((> events 0) (format " -e %s" events)))
                         (when points
                           (format " -p %s" points))
                         " && sed -i s/^#// " output "&"))
        (buffer-name (concat "*Simulation (" (get-filename input) ") "
                             (number-to-string *buffer-counter*) "*")))

    (when (file-exists-p output)
          (if (y-or-n-p (concat "File '" output "' exists. Would you like to "
                                "delete it to run the simulation?"))
              (delete-file output)
              (error "%s" (concat "Output file " output " has not been "
                                  "overwritten"))))

    (message "Simulation command executed: %s\n" command) ; save the command to *Message* buffer
    (shell-command command (get-buffer-create buffer-name))

    (setq *buffer-counter* (+ 1 *buffer-counter*))
    (format (concat "Done! See " buffer-name " buffer for details"))))


;;; Gnuplot related functions.

(defun kappa-plot-sim (&optional file-path columns)
  "Simple function for plotting a Kappa simulation file.

  FILE-PATH is the full path to a file that can be read by
            gnuplot.  The first row is expected to contain the
            headers for each column.

  COLUMNS is a string containing the columns to be plotted
          separated by space.

By default the following options would be set in gnuplot:
autoscale, xtic auto, ytic auto, key autotitle columnhead,
ylabel \"Number of Molecules\", xlabel \"Time\"

This function requires the installation of gnuplot-mode. You can
find it at

  https://github.com/bruceravel/gnuplot-mode/
"

  (interactive
   (list (read-file-name "Simulation output file: "
                         (concat (get-dirname kappa-prev-sim-output-file) "/")
                          nil nil (get-filename kappa-prev-sim-output-file))
         (read-string "Columns separated by space: "
                      kappa-prev-plot-columns)))

  ;; This part requires the installation of gnuplot-mode!
  ;; https://github.com/bruceravel/gnuplot-mode/
  (if (not (require 'gnuplot nil t))
      (error "Could not find Gnuplot mode! Gnuplot mode is \
required for plotting.")

    (setq kappa-prev-sim-output-file file-path)
    (setq kappa-prev-plot-columns columns)

    (gnuplot-send-string-to-gnuplot
     (concat "set autoscale\n"
             "set xtic auto\n"
             "set ytic auto\n"
             "set key autotitle columnhead\n"
             "set ylabel \"Number of Molecules\"\n"
             "set xlabel \"Time\"\n"
             "set title \"" (get-filename file-path) "\"\n"
             "plot "
             (let ((cols (mapcar (lambda (x)
                                   (+ 2 (string-to-number x)))
                                 (split-string columns))))
               (mapconcat (lambda (n)
                            (concat "\'" file-path "\' using 1:"
                                    (number-to-string n) " with lines"))
                          cols ", ")) "\n")
     nil)))


(provide 'kappa)

;;; kappa.el ends here
