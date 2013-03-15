
(setq user-mail-address "coffeemug@gmail.com")
(setq user-full-name "Slava Akhmechet")

(setq gnus-select-method '(nntp "news.verizon.net"))

;; don't use canlock
(setq message-insert-canlock nil)

;; turn off interactivity in some cases
(setq gnus-interactive-exit nil)
(setq gnus-interactive-catchup nil)

;; topic mode
(add-hook 'gnus-group-mode-hook 'gnus-topic-mode)

;; threading
(setq gnus-thread-hide-subtree t)
(setq gnus-fetch-old-headers 50)

;; summary line
(setq gnus-summary-line-format "%U%R%I%-9d%(%*%-23,23f%) %s\n")

;; gmail
(add-to-list 'gnus-secondary-select-methods '(nnml ""))
(setq mail-sources					
      '((pop :server "pop.gmail.com"
	     :port 995
	     :user "coffeemug@gmail.com"
	     :password "dummy"
	     :program "fetchmail -s;cp /var/spool/mail/coffeemug %t"
	     :postscript "cat /dev/null > /var/spool/mail/coffeemug")))

(setq sendmail-program "/usr/bin/msmtp")

;; Splitting mail
(setq nnmail-split-methods 'nnmail-split-fancy)

(setq nnmail-split-fancy
      '(| (from ".*Akhmechet.*" "mail.sent")
	  (to ".*haskell-cafe@haskell.org.*" "mail.haskell")
	  (to ".*glasgow-haskell-users@haskell.org.*" "mail.haskell")
	  (to ".*tbnl-devel@common-lisp.net.*" "mail.hunchentoot")
	  (to ".*sbcl-devel@lists.sourceforge.net.*" "mail.sbcl")
	  (to ".*sbcl-devel@lists.sf.net.*" "mail.sbcl")
	  (to ".*sbcl-help@lists.sourceforge.net.*" "mail.sbcl")
	  (to ".*dhammasukha@yahoogroups.com.*" "mail.dhamma")
	  (to ".*openmcl-devel@clozure.com.*" "mail.openmcl")
	  (to ".*cedet-semantic@lists.sourceforge.net.*" "mail.cedet")
	  (to ".*cmucl-imp@cons.org.*" "mail.cmucl")
	  ("subject" "A New Statement is Ready" "mail.periodical")
	  (from "Apple Developer Connection.*" "mail.periodical")
	  (from ".*Snapfish.*" ("subject" ".*Save.*" "mail.periodical"))
	  ("subject" "Monthly report for Amazon.com Associate.*" "mail.periodical")
	  (from ".*behya@yahoo.com.*" ("subject" "FW:.*" "mail.funny"))
	  "mail.other"))
