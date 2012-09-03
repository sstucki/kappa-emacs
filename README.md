kappa-mode
==========

`kappa-mode` is a GNU/Emacs major mode for editing files written in
the Kappa modeling language.  The mode knows enough about Kappa syntax
to do some basic fontification but does currently not do indentation
or proper slashification.


Getting `kappa-mode`
--------------------

`kappa-mode` is free software distributed under the GPL.  You can
download the latest version from the [Github
repository](https://github.com/sstucki/kappa-emacs/).


Installation
------------

In order to use `kappa-mode` just copy the file `kappa.el` to your
`~/.emacs.d` directory and add the following to your `~/.emacs`
configuration file:

    (add-to-list 'load-path "~/.emacs.d")
    (require 'kappa)

Alternatively, you may load `kappa.el` directly using the following
command:

    (load-file "~/.emacs.d/kappa.el")


Usage and customization
-----------------------

The syntax highlighting is quite intense by default (almost every
character is highlighted in some way) but also highly customizable.
Users that are unhappy with the default font lock color scheme may
change it through the numerous *font face customization variables*
(`kappa-keyword-face`, `kappa-agent-name-face`,
`kappa-rule-operator-face`, etc.)

In addition to the syntax highlighting, the mode provides two
convenience functions for simulating the current Kappa file
(`kappa-run-sim`, mapped to `C-c C-r`) and plotting the result
(`kappa-plot-sim`, mapped to `C-c C-p`).  The former requires
[KaSim](https://github.com/jkrivine/KaSim/) and the later
[Gnuplot](http://www.gnuplot.info/) to be installed.  The executable
paths of these tools can be adjusted through the customization
variables `kappa-sim-executable-path` and
`kappa-gnuplot-executable-path` respectively.
[gnuplot-mode](https://github.com/bruceravel/gnuplot-mode/) will be
used for plotting if present but is not a requirement.


Help!
-----

If you have questions, check the built-in documentation, e.g. by
typing `C-h C-f kappa-run-sim`, or try asking for help on the [Kappa
users mailing list](http://groups.google.com/group/kappa-users).  Use
[Github](https://github.com/sstucki/kappa-emacs/issues) to report bugs
and other [issues](https://github.com/sstucki/kappa-emacs/issues).


Known issues
------------

 * There seems to be a bug when using Kappa mode at the same time as
   CEDET causing Emacs to fontify the entire buffer with
   font-lock-comment-face.

 * Indentation and slashification are missing.
