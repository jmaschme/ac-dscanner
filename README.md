Overview
========
ac-dscanner provides a source for the emacs auto-complete extension that uses the Dscanner tool.

ac-dscanner currently depends upon two pull requests that have not been merged into the main Dscanner product.

Requirements
============
* auto-complete
* Dscanner
* yasnippet (optional)

Setup
=====

To use, you will first need to add d-mode to the list of modes supported by auto-complete.

    (add-to-list 'ac-modes 'd-mode)


Then add something like the following to your emacs configuration file:

    (require 'ac-dscanner)
    (defun ac-d-mode-setup ()
      (setq ac-sources (append '(ac-source-dscanner ac-source-yasnippet) ac-sources))
      (global-auto-complete-mode t))
    (add-hook 'd-mode-hook 'ac-d-mode-setup)