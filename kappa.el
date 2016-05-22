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

;; `kappa-mode` is a GNU/Emacs major mode for editing files written in
;; the Kappa modeling language.  The mode knows enough about Kappa
;; syntax to do some basic fontification but does currently not do
;; indentation or proper slashification.

;; There are numerous font face customization variables and two
;; convenience functions for running simulations and plotting results.


;;; Known bugs/limitations:

;; * There seems to be a bug when using Kappa mode at the same time as
;;   CEDET causing Emacs to fontify the entire buffer with
;;   font-lock-comment-face.


;;; To Do:

;; * Fix comment font-lock weirdness.
;; * Support for indentation and slashification. Hard.
;; * Documentation.


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
  "Face to use for highlighting Kappa operators such as \"+\",
\"&&\" or \";\" in algebraic, logic and perturbation expressions
in Font-Lock mode."
  :group 'faces
  :group 'kappa)

(defface kappa-interface-symbol-face
  '((t :inherit font-lock-builtin-face))
  "Face to use for highlighting the Kappa agent interface symbols
such as \"!\" or \"~\" in Font-Lock mode."
  :group 'faces
  :group 'kappa)

(defface kappa-builtin-face
  '((t :inherit font-lock-builtin-face))
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

(defface kappa-token-name-face
  '((t :inherit font-lock-variable-name-face))
  "Face to use for highlighting Kappa token names in Font-Lock
mode."
  :group 'faces
  :group 'kappa)

(defface kappa-string-face
  '((t :inherit font-lock-string-face))
  "Face to use for highlighting string literals such as Kappa
variables and file names in Font-Lock mode."
  :group 'faces
  :group 'kappa)

(defface kappa-cellbreak-face
  '((t :inherit font-lock-comment-face
       :overline t
       :bold t))
  "Face to use for cellbreak ## comment lines in Font-Lock mode."
  :group 'faces
  :group 'kappa)

;; Buffer-local variable definitions from face definitions
(defvar kappa-keyword-face 'kappa-keyword-face
  "Face for highlighting Kappa keywords.")
(defvar kappa-command-face 'kappa-command-face
  "Face for highlighting Kappa commands.")
(defvar kappa-rule-operator-face 'kappa-rule-operator-face
  "Face for highlighting Kappa rule operators.")
(defvar kappa-math-operator-face 'kappa-math-operator-face
  "Face for highlighting Kappa math and perturbation operators.")
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
(defvar kappa-token-name-face 'kappa-token-name-face
  "Face for highlighting Kappa token names.")
(defvar kappa-string-face 'kappa-string-face
  "Face for highlighting string literals in Kappa mode.")
(defvar kappa-cellbreak-face 'kappa-cellbreak-face
  "Face for making a cellbreak on ## comment lines in Kappa mode.")


;;; Simulation and plotting-related variables

;; Customization variables

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

(defcustom kappa-gnuplot-executable-path "/usr/bin/gnuplot"
  "File system path to the Gnuplot executable (default is
'/usr/bin/gnuplot')."
  :type 'file
  :group 'kappa)


;; Buffer-local variables to remember the values of the arguments of
;; previous invocation of `kappa-run-sim' and `kappa-plot-sim'.

(defvar kappa-prev-sim-output-file ""
  "Value of the `output' or `file-path' argument during the
previous invocation of `kappa-run-sim' or `kappa-plot-sim',
respectively.")
(defvar kappa-prev-sim-time kappa-default-sim-time
  "Value of the `time' argument during the previous invocation of
`kappa-run-sim'.")
(defvar kappa-prev-sim-events kappa-default-sim-events
  "Value of the `events' argument during the previous invocation
of `kappa-run-sim'.")
(defvar kappa-prev-sim-points kappa-default-sim-points
  "Value of the `points' argument during the previous invocation
of `kappa-run-sim'.")
(defvar kappa-prev-plot-columns "1:2"
  "Value of the `columns' argument during the previous invocation
of `kappa-plot-sim'.")
(defvar kappa-sim-buffer-counter 1
  "Counts the number of simulation buffers in Kappa major mode")


;; Global variable holding Gnuplot processes object for plotting if
;; necessary.

(defvar kappa-gnuplot-process ""
  "Process object of a running Gnuplot process.

The default value is \"\", indicating that no Gnuplot process has
been started by the Kappa major mode yet.")


;;; Local key map

(defvar kappa-mode-keymap (make-sparse-keymap)
  "Kappa major mode keymap.")

;; Add shortcuts for `kappa-run-sim' and `kappa-plot-sim'.
(define-key kappa-mode-keymap "\C-c\C-r" 'kappa-run-sim)
(define-key kappa-mode-keymap "\C-c\C-p" 'kappa-plot-sim)


;;; Main Kappa mode definition (using generic)

(define-generic-mode kappa-mode

  '(?#)      ;; Comments start with '#'

  nil        ;; Handle keywords using font-lock rules (see below)

  ;; Rules for syntax highlighting (font-lock)
  (eval-when-compile
    (let
        ;; Common lexical sub-expressions used in keywords
        ((alnum "[A-Za-z0-9_+-]+")          ;; Alpha-numeric IDs
         (id "[A-Za-z][A-Za-z0-9_+-]*")     ;; IDs/names as defined in
                                            ;; the Kappa spec
         (num "[0-9]+")                     ;; Integer numerals
         (ws "\\(?:\\s-\\|\\\\\n\\)*"))     ;; Whitespace

      (list

       ;; ## comments
       '("^\\s-*\\(\##.*\\)$" 1 kappa-cellbreak-face t)

       ;; String literals and file names
       '("\"\\(?:[^\"\n]\\|\\\\[\"\n]\\)+\"" . kappa-string-face)

       ;; Variable names
       '("'[^'\n]+'" . kappa-string-face)

       ;; Keywords
       (cons
        (regexp-opt
         '("%agent:" "%def:" "%var:" "%plot:" "%obs:" "%init:" "%mod:"
           "%token:" "do" "set" "repeat" "until"))
        kappa-keyword-face)

       ;; Commands
       (cons
        (regexp-opt
         '("$ADD" "$DEL" "$FLUX" "$PLOTENTRY" "$PRINT" "$PRINTF"
           "$SNAPSHOT" "$STOP" "$TRACK" "$UPDATE"))
        kappa-command-face)

       ;; Built-in functions
       (cons
        (regexp-opt
         '("[cos]" "[exp]" "[int]" "[log]" "[max]" "[min]" "[mod]"
           "[not]" "[sin]" "[sqrt]" "[tan]"))
        kappa-builtin-face)

       ;; Symbolic numerical constants
       (cons
        (regexp-opt
         '("[E]" "[E+]" "[E-]" "[Emax]" "[T]" "[Tmax]" "[Tsim]"
           "[false]" "[p]" "[pi]" "[true]" "INF"))
        kappa-constant-face)

       ;; Agent interface symbols
       '("[?!~]" . kappa-interface-symbol-face)

       ;; Internal state names
       ;;
       ;; NOTE: Highlighting slightly diverges from the spec here:
       ;; according to the KaSim manual, an internal state name is
       ;; just an ID, excluding e.g. names starting with digits.
       ;; However, any alphanumeric ID prefixed by a tilde will be
       ;; highlighted as an internal state name by kappa-mode.
       (list (concat "~" ws "\\(" alnum "\\)")
             1 kappa-internal-state-face)

       ;; Link labels
       (list (concat "!" ws "\\(" num          ;; Numeric label
                     "\\|" id ws "\\." ws id   ;; Remote site name
                     "\\|" ws "_\\)")          ;; Wildcard
             1 kappa-link-label-face)

       ;; Agent names followed by an interface spec and site names
       (list (concat "\\(" id "\\)" ws "(")    ;; Agents
             '(1 kappa-agent-name-face)
             (list                             ;; Site interface
              (concat "\\=[^A-Za-z)]*\\(" id "\\)[^,)]*")
              '(kappa-get-end-of-multiline) nil
              '(1 kappa-site-name-face)))

       ;; Token names
       (list (concat ":" ws "\\(" id "\\)") 1 kappa-token-name-face)

       ;; Token concentrations
       (cons (concat "|" ws id ws "|") kappa-token-name-face)

       ;; Numerals
       (cons (concat "[+-]?\\(\\<" num "\\(\\.[0-9]*\\)?"
                     "\\|\\." num "\\)\\([Ee][+-]?" num "\\)?\\>")
             kappa-constant-face)

       ;; Agent names not followed by an interface spec (all
       ;; remaining IDs)
       (cons id kappa-agent-name-face)
       ;; '("\\.\\.\\." . kappa-agent-name-face)

       ;; Two-character logic/perturbation expression operators
       '("&&\\|||\\|:=" . kappa-math-operator-face)

       ;; Rule operators
       '("|\\|->\\|<-\\|<->\\|@" . kappa-rule-operator-face)

       ;; Other logic/math/perturbation expression operators
       '("[+*/^<>=.;-]" . kappa-math-operator-face))))

  ;; File suffixes for which to activate this mode
  '("\\.ka\\'")

  ;; Activate Kappa mode key map.
  '(kappa-mode-setup)

  "Major mode for editing Kappa models.

The syntax highlighting is quite intense by default (almost every
character is highlighted in some way) but also highly customizable.
Users that are unhappy with the default font lock color scheme may
change it through the numerous font face customization variables
(`kappa-keyword-face', `kappa-agent-name-face',
`kappa-rule-operator-face', etc.)

In addition to the syntax highlighting, the mode provides two
convenience functions, `kappa-run-sim' and `kappa-plot-sim', for
simulating the current Kappa file and plotting the result,
respectively.  In order to work properly, they require the
installation of a Kappa simulator such as KaSim and Gnuplot.  The
executable paths of these tools can be adjusted through the
customization variables `kappa-sim-executable-path' and
`kappa-gnuplot-executable-path' respectively.  gnuplot-mode will
be used for plotting if present but is not a requirement.

 * KaSim        -- https://github.com/jkrivine/KaSim/
 * Gnuplot      -- http://www.gnuplot.info/
 * gnuplot-mode -- https://github.com/bruceravel/gnuplot-mode/

\\{kappa-mode-keymap}
Turning on Kappa mode runs the hook `kappa-mode-hook'.
")

(defun kappa-mode-setup ()
  "Set up the Kappa major mode."

  ;; Make variables buffer-local.
  (mapc 'make-local-variable
        '(kappa-keyword-face
          kappa-command-face
          kappa-rule-operator-face
          kappa-math-operator-face
          kappa-interface-symbol-face
          kappa-builtin-face
          kappa-constant-face
          kappa-agent-name-face
          kappa-site-name-face
          kappa-link-label-face
          kappa-internal-state-face
          kappa-string-face
          kappa-cellbreak-face
          kappa-prev-sim-output-file
          kappa-prev-sim-time
          kappa-prev-sim-events
          kappa-prev-sim-points
          kappa-prev-plot-columns
          kappa-mode-keymap
          kappa-sim-buffer-counter))

  ;; Set the default output file.
  (setq kappa-prev-sim-output-file
        (concat (kappa-get-abs-dirname buffer-file-name) "data.out"))

  ;; Make sure multiline expressions are identified and re-highlighted
  ;; correctly.
  (add-to-list 'font-lock-extend-region-functions
               'kappa-extend-font-lock-region-to-multiline t)

  ;; Install the local key map.
  (use-local-map kappa-mode-keymap))


;;; Font-lock-related functions.

(defun kappa-move-to-beginning-of-multiline ()
  "Move to the beginning of the first line before `point' that
doesn't end in a backslash."
  (interactive)
  (beginning-of-line)
  (while (and (not (bobp))
              (= (save-excursion (end-of-line 0) (preceding-char)) ?\\))
    (beginning-of-line 0)))

(defun kappa-move-to-end-of-multiline ()
  "Move to the beginning of the first line after `point' that
doesn't immediately follow a backslash."
  (interactive)
  (if (not (bolp)) (beginning-of-line 2))
  (while (and (not (eobp))
              (= (save-excursion (end-of-line 0) (preceding-char)) ?\\))
    (beginning-of-line 2)))

(defun kappa-get-end-of-multiline ()
  "Return the position of the end of the first line after `point'
that doesn't end in a backslash."
  (save-excursion
    (kappa-move-to-end-of-multiline)
    (point)))

(defun kappa-extend-font-lock-region-to-multiline ()
  "Extend the font-lock fontify region to the beginning of the
first line before `font-lock-beg' that doesn't end in a backslash
and to the beginning of the first line after `font-lock-end' that
doesn't immediately follow a backslash."
  (let ((new-beg                      ; find new start position
         (save-excursion
           (goto-char font-lock-beg)
           (kappa-move-to-beginning-of-multiline)
           (point)))
        (new-end                      ; find new end position
         (save-excursion
           (goto-char font-lock-end)
           (kappa-move-to-end-of-multiline)
           (point))))
    (let ((updated-beg                ; update start position?
           (if (= new-beg font-lock-beg) nil
             (setq font-lock-beg new-beg) t))
          (updated-end                ; update end position?
           (if (= new-end font-lock-end) nil
             (setq font-lock-end new-end) t)))
      (or updated-beg updated-end))))


;;; Simulation-related functions.

(defun kappa-get-abs-dirname (path)
  "Return the absolute directory name of PATH."
  (file-name-directory (file-truename path)))


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

Side Effects: Prints the shell command to be executed to
*Messages*, Creates *Simulation* buffer, and runs
`kappa-sim-executable-path' in shell with the appropriate
arguments.

Related customization variables: `kappa-sim-executable-path',
`kappa-default-sim-time', `kappa-default-sim-events',
`kappa-default-sim-points'.
"
  (interactive
    (list (expand-file-name
           (read-file-name
            "Input: " (file-truename buffer-file-name)
            (file-truename buffer-file-name) 'confirm))
          (expand-file-name
           (read-file-name
            "Output: " (file-truename kappa-prev-sim-output-file)
            (file-truename kappa-prev-sim-output-file)))
          (read-number "Time: " kappa-prev-sim-time)
          (read-number "Events: " kappa-prev-sim-events)
          (read-number "Points: " kappa-prev-sim-points)))

  ;; Save parameters for later
  (setq kappa-prev-sim-output-file output)
  (setq kappa-prev-sim-time time)
  (setq kappa-prev-sim-events events)
  (setq kappa-prev-sim-points points)

  ;; The "--emacs-mode" flag suppresses the generation of an initial
  ;; "#" in front of the column header names on the first line of the
  ;; output file so that Gnuplot can parse them.
  (let ((command (concat kappa-sim-executable-path " -i " input
                         " -o " (file-name-nondirectory output)
                         " -d " (kappa-get-abs-dirname output)
                         (cond
                           ((> time 0)   (format " -t %s" time))
                           ((> events 0) (format " -e %s" events)))
                         (when points
                           (format " -p %s" points))
                         " --emacs-mode &"))
        (buffer-name (concat "*Simulation (" (file-name-nondirectory input)
                             ") " (number-to-string kappa-sim-buffer-counter)
                             "*")))

    (when (file-exists-p output)
          (if (y-or-n-p (concat "Output file '" output "' exists. Would you like to "
                                "delete it to run the simulation?"))
              (delete-file output)
              (error "%s" (concat "Output file " output " has not been "
                                  "overwritten"))))

    ;; Save the command to *Message* buffer and run the simulation
    (message "Running simulation command: %s\n" command)
    (shell-command command (get-buffer-create buffer-name))

    (setq kappa-sim-buffer-counter (+ 1 kappa-sim-buffer-counter))
    (format (concat "Done! See " buffer-name " buffer for details"))))


;;; Gnuplot related functions.

(defun kappa-get-gnuplot-process nil
  "Return the process object of the Gnuplot process associated
with the Kappa major mode.

This function requires the installation of Gnuplot.  You can find
it at

  * http://www.gnuplot.info/

Side Effects: If `kappa-gnuplot-process' does not correspond to a
process object (no running Gnuplot process is currently
associated with the Kappa major mode), starts an asynchronous
process using the command `kappa-gnuplot-executable-path' and
returns the corresponding process object.  If
`kappa-gnuplot-process' refers to an existing Gnuplot process
with a status other than `run', kills it, starts a new
asynchronous Gnuplot process and returns the new process object.
In either case, prints the command to be executed to *Messages*,
creates a *Kappa Gnuplot output* buffer (if none exists) and sets
`kappa-gnuplot-process' to the process object associated with the
newly started process.

Related variables: `kappa-gnuplot-executable-path',
`kappa-gnuplot-process'.
"
  (let ((status (process-status kappa-gnuplot-process)))
    (if (eq status 'run) kappa-gnuplot-process

      ;; Kill a potentially "hanging" process.
      (if (not (null status)) (delete-process kappa-gnuplot-process))

      ;; Start a new asynchronous Gnuplot process.
      (message "Running Gnuplot as '%s -p'" kappa-gnuplot-executable-path)
      (setq kappa-gnuplot-process
            (start-process "gnuplot"
                           (get-buffer-create "*Kappa Gnuplot output*")
                           kappa-gnuplot-executable-path "-p")))))


(defun kappa-plot-sim (&optional file-path columns)
  "Simple function for plotting a Kappa simulation file.

  FILE-PATH is the full path to a file that can be read by
            Gnuplot.  The first row is expected to contain the
            headers for each column.

  COLUMNS is a string containing the columns to be plotted
          separated by space.  Default is \"1:2\" plotting the
          first column (time) against the second one.

By default the following options would be set in Gnuplot:
autoscale, xtic auto, ytic auto, key autotitle columnhead,
ylabel \"Number of Molecules\", xlabel \"Time\"

This function requires the installation of Gnuplot and optionally
gnuplot-mode.  You can find them at

  * http://www.gnuplot.info/
  * https://github.com/bruceravel/gnuplot-mode/

Side Effects: If gnuplot-mode is available, sends commands to
gnuplot-mode.  Otherwise, send commands to a dedicated Gnuplot
process associated with the Kappa major mode.
"
  (interactive
   (list (expand-file-name
          (read-file-name
           "Simulation output file: "
           (file-truename kappa-prev-sim-output-file)
           (file-truename kappa-prev-sim-output-file) 'confirm))
         (read-string "Columns separated by space: "
                      kappa-prev-plot-columns)))

  ;; Save parameters for later
  (setq kappa-prev-sim-output-file file-path)
  (setq kappa-prev-plot-columns columns)

  (let ((gnuplot-commands
         (concat "set autoscale\n"
             "set xtic auto\n"
             "set ytic auto\n"
             "set key autotitle columnhead\n"
             "set ylabel \"Number of Molecules\"\n"
             "set xlabel \"Time\"\n"
             "set title \"" (file-name-nondirectory file-path) "\"\n"
             "plot "
             (mapconcat
              (lambda (n) (concat "\"" file-path "\" using " n " with lines"))
              (split-string columns) ", \\\n") "\n")))

    ;; Check if gnuplot-mode is available, otherwise just run Gnuplot
    ;; in a shell.
    (if (require 'gnuplot nil t)

        ;; Use gnuplot-mode
        (gnuplot-send-string-to-gnuplot gnuplot-commands nil)

      ;; Send the Gnuplot commands to the Gnuplot process associated
      ;; with the Kappa major mode.
      (process-send-string (kappa-get-gnuplot-process) gnuplot-commands))))


(provide 'kappa)

;;; kappa.el ends here
