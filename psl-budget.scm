;; -*-scheme-*-

;; This is a sample guile report generator for GnuCash.
;; It illustrates the basic techniques used to create
;; new reports for GnuCash.

(define-module (gnucash report psl-budget))
(use-modules (gnucash main)) ;; FIXME: delete after we finish modularizing.
(use-modules (gnucash gnc-module))
(use-modules (gnucash gettext))

;; 'debug is deprecated and unused since guile 2
(cond-expand
  (guile-2 )
  (else
    (debug-enable 'debug)))
(debug-enable 'backtrace)

(gnc:module-load "gnucash/report/report-system" 0)
(gnc:module-load "gnucash/html" 0) ;for gnc-build-url

(define reportname (N_ "YTD Budget Report"))

;; define all option's names so that they are properly defined
;; in *one* place.
;;(define optname-from-date (N_ "From"))
;;(define optname-to-date (N_ "To"))

(define optname-display-depth (N_ "Account Display Depth"))
(define optname-show-subaccounts (N_ "Always show sub-accounts"))
(define optname-accounts (N_ "Account"))

(define optname-period (N_ "Budget Period Number"))
(define optname-price-source (N_ "Price Source"))
(define optname-show-rates (N_ "Show Exchange Rates"))
(define optname-show-full-names (N_ "Show Full Account Names"))
(define optname-select-columns (N_ "Select Columns"))
(define optname-show-budget (N_ "Show Budget"))
(define optname-show-actual (N_ "Show Actual"))
(define optname-show-difference (N_ "Show Difference"))
(define opthelp-show-budget (N_ "Display a column for the budget values"))
(define opthelp-show-actual (N_ "Display a column for the actual values"))
(define opthelp-show-difference (N_ "Display the difference as budget - actual"))
(define optname-show-totalcol (N_ "Show Column with Totals"))
(define opthelp-show-totalcol (N_ "Display a column with the row totals"))
(define optname-bottom-behavior (N_ "Flatten list to depth limit"))
(define opthelp-bottom-behavior
  (N_ "Displays accounts which exceed the depth limit at the depth limit"))

(define optname-budget (N_ "Budget"))

;; options generator
(define (options-generator)
  (let* ((options (gnc:new-options))
    (add-option
     (lambda (new-option)
       (gnc:register-option options new-option))))

    (gnc:register-option
     options
     (gnc:make-budget-option
      gnc:pagename-general optname-budget
      "a" (N_ "Budget")))

    ;; date interval
    ;;(gnc:options-add-date-interval!
    ;; options gnc:pagename-general
    ;; optname-from-date optname-to-date "a")

    (gnc:options-add-price-source!
     options gnc:pagename-general optname-price-source "c" 'average-cost)

    ;;(gnc:register-option
    ;; options
    ;; (gnc:make-simple-boolean-option
    ;;  gnc:pagename-general optname-show-rates
    ;;  "d" (N_ "Show the exchange rates used") #f))

    (gnc:register-option
     options
     (gnc:make-simple-boolean-option
      gnc:pagename-general optname-show-full-names
      "e" (N_ "Show full account names (including parent accounts)") #t))

    (gnc:register-option
     options
     (gnc:make-number-range-option
      gnc:pagename-general optname-period
      "e" (N_ "The budget period to use") 9
      1 12 0 1))

    ;; accounts to work on
    (gnc:options-add-account-selection!
     options gnc:pagename-accounts
     optname-display-depth optname-show-subaccounts
     optname-accounts "a" 2
     (lambda ()
       (gnc:filter-accountlist-type
        (list ACCT-TYPE-ASSET ACCT-TYPE-LIABILITY ACCT-TYPE-INCOME
                          ACCT-TYPE-EXPENSE)
        (gnc-account-get-descendants-sorted (gnc-get-current-root-account))))
     #f)
    (add-option
     (gnc:make-simple-boolean-option
      gnc:pagename-accounts optname-bottom-behavior
      "c" opthelp-bottom-behavior #f))

    ;; columns to display
    (add-option
     (gnc:make-simple-boolean-option
      gnc:pagename-display optname-show-budget
      "s1" opthelp-show-budget #t))
    (add-option
     (gnc:make-simple-boolean-option
      gnc:pagename-display optname-show-actual
      "s2" opthelp-show-actual #t))
    (add-option
     (gnc:make-simple-boolean-option
      gnc:pagename-display optname-show-difference
      "s3" opthelp-show-difference #f))
    (add-option
     (gnc:make-simple-boolean-option
      gnc:pagename-display optname-show-totalcol
      "s4" opthelp-show-totalcol #f))

      ;; Set the general page as default option tab
    (gnc:options-set-default-section options gnc:pagename-general)

    options)
  )

;; Create the html table for the budget report
;;
;; Parameters
;;   html-table - HTML table to fill in
;;   acct-table - Table of accounts to use
;;   budget - budget to use
;;   params - report parameters
(define (gnc:html-table-add-budget-values!
         html-table acct-table budget params)
  (let* ((get-val (lambda (alist key)
                    (let ((lst (assoc-ref alist key)))
                      (if lst (car lst) lst))))
         (show-actual? (get-val params 'show-actual))
         (show-budget? (get-val params 'show-budget))
         (show-diff? (get-val params 'show-difference))
         (show-totalcol? (get-val params 'show-totalcol))
         (budget-period-num (get-val params 'period-number))
        )

  ;; Calculate the sum of all budgets of all children of an account for a specific period
  ;;
  ;; Parameters:
  ;;   budget - budget to use
  ;;   children - list of children
  ;;   period - budget period to use
  ;;
  ;; Return value:
  ;;   budget value to use for account for specified period.
  (define (budget-account-sum budget children period)
	(let* ((sum (cond
                   ((null? children) (gnc-numeric-zero))
                   (else (gnc-numeric-add (gnc:get-account-period-budget-value budget (car children) period)
                             (budget-account-sum budget (cdr children) period)
			     GNC-DENOM-AUTO GNC-RND-ROUND))
                 )
		  ))
	sum)
  )

  ;; Calculate the value to use for the budget of an account for a specific period.
  ;; 1) If the account has a budget value set for the period, use it
  ;; 2) If the account has children, use the sum of budget values for the children
  ;; 3) Otherwise, use 0.
  ;;
  ;; Parameters:
  ;;   budget - budget to use
  ;;   acct - account
  ;;   period - budget period to use
  ;;
  ;; Return value:
  ;;   sum of all budgets for list of children for specified period.
  (define (gnc:get-account-period-budget-value budget acct period)
    (let* ((bgt-set? (gnc-budget-is-account-period-value-set budget acct period))
          (children (gnc-account-get-children acct))
          (amount (cond
                    (bgt-set? (gnc-budget-get-account-period-value budget acct period))
		    ((not (null? children)) (budget-account-sum budget children period))
		    (else (gnc-numeric-zero)))
          ))
    amount)
  )

  ;; Calculate the value to use for the budget of an account for a specific set of periods.
  ;; If there is 1 period, use that period's budget value.  Otherwise, sum the budgets for
  ;; all of the periods.
  ;;
  ;; Parameters:
  ;;   budget - budget to use
  ;;   acct - account
  ;;   periodlist - list of budget periods to use
  ;;
  ;; Return value:
  ;;   Budget sum
  (define (gnc:get-account-periodlist-budget-value budget acct periodlist)
(gnc:debug "gnc:get-account-periodlist-budget-value " periodlist)
    (cond
	  ((= (length periodlist) 1)(gnc:get-account-period-budget-value budget acct (car periodlist)))
	  (else (gnc-numeric-add (gnc:get-account-period-budget-value budget acct (car periodlist))
	                         (gnc:get-account-periodlist-budget-value budget acct (cdr periodlist))
							 GNC-DENOM-AUTO GNC-RND-ROUND))
	)
  )

  ;; Calculate the value to use for the actual of an account for a specific set of periods.
  ;; This is the sum of the actuals for each of the periods.
  ;;
  ;; Parameters:
  ;;   budget - budget to use
  ;;   acct - account
  ;;   periodlist - list of budget periods to use
  ;;
  ;; Return value:
  ;;   Budget sum
  (define (gnc:get-account-periodlist-actual-value budget acct periodlist)
(gnc:debug "gnc:get-account-periodlist-actual-value " periodlist)
    (cond
	  ((= (length periodlist) 1)(gnc-budget-get-account-period-actual-value budget acct (car periodlist)))
	  (else (gnc-numeric-add (gnc-budget-get-account-period-actual-value budget acct (car periodlist))
	                         (gnc:get-account-periodlist-actual-value budget acct (cdr periodlist))
							 GNC-DENOM-AUTO GNC-RND-ROUND))
	)
  )

  ;; Adds a line to tbe budget report.  The line contains a number of columns, controlled by the
  ;; column-list parameter.  Each element of the list can be:
  ;;   1) a period number - this column contains info for the specific period
  ;;   2) a list of period numbers - this column contains the sum of info for the periods
  ;;   3) 'total - this column contains the total of columns to the left
  ;;
  ;; Parameters:
  ;;   html-table - html table being created
  ;;   rownum - row number
  ;;   colnum - starting column number
  ;;   budget - budget to use
  ;;   acct - account being displayed
  ;;   column-list - list of column info
  ;;   exchange-fn - exchange function (not used)
  (define (gnc:html-table-add-budget-line!
           html-table rownum colnum
           budget acct column-list exchange-fn)
    (let* ((num-periods (gnc-budget-get-num-periods budget))
           (period 0)
           (current-col (+ colnum 1))
		   (bgt-total (gnc-numeric-zero))
		   (bgt-total-unset? #t)
		   (act-total (gnc-numeric-zero))
           (comm (xaccAccountGetCommodity acct))
           (reverse-balance? (gnc-reverse-balance acct))
           )

      ;; Determines if this is an income or asset account
      ;;
      ;; Parameters
      ;;   account - Account
      (define (gnc:account-is-inc-asset? account)
        (let ((type (xaccAccountGetType account)))
          (member type (list ACCT-TYPE-INCOME ACCT-TYPE-ASSET))))


	  ;; Displays a set of budget column values.  The execution
      ;; environment must include show-budget? show-actual? and
      ;; show-diff? to indicate which columns in the set should
      ;; be shown.
	  ;;
	  ;; Parameters
      ;;   html-table - html table being created
      ;;   rownum - row number
      ;;   acct - account
	  ;;   bgt-numeric-val - budget value
	  ;;   act-numeric-val - actual value
	  ;;   dif-numeric val - difference value
	  (define (gnc:html-table-display-budget-columns!
	           html-table rownum acct
			   bgt-numeric-val act-numeric-val dif-numeric-val)

           (let* (
              (bgt-val #f)
              (act-val #f)
              (dif-val #f)
              (style-tag "number-cell")
              (inc-asset? (gnc:account-is-inc-asset? acct))
              (style-tag-neg (string-append style-tag "-neg"))

			)
           (if show-budget?
             (begin
               (set! bgt-val (if (gnc-numeric-zero-p bgt-numeric-val)
                                 "."
                                 (gnc:make-gnc-monetary comm bgt-numeric-val)))
               (gnc:html-table-set-cell/tag!
                html-table rownum current-col style-tag bgt-val)
               (set! current-col (+ current-col 1))
             )
           )
           (if show-actual?
             (begin
               (set! act-val (gnc:make-gnc-monetary comm act-numeric-val))
               (gnc:html-table-set-cell/tag!
                 html-table rownum current-col
                 (if (gnc-numeric-negative-p act-numeric-val) style-tag-neg style-tag)
                 act-val)
               (set! current-col (+ current-col 1))
             )
           )

           ;; For the diff column, "good" values are shown in black, even if -ve and
           ;; "bad" values are shown in red, even if +ve.
           (if show-diff?
             (begin
               (set! dif-val
                 (if (and (gnc-numeric-zero-p bgt-numeric-val) (gnc-numeric-zero-p act-numeric-val))
                     "."
                     (gnc:make-gnc-monetary comm dif-numeric-val)))
                 (gnc:html-table-set-cell/tag!
                   html-table rownum current-col
                   (cond
                     (inc-asset?
                       (if (gnc-numeric-negative-p dif-numeric-val) style-tag style-tag-neg))
                     (else
                       (if (gnc-numeric-negative-p dif-numeric-val) style-tag-neg style-tag))
                   )
                   dif-val)
               (set! current-col (+ current-col 1))
             )
           )
		  )
		)

	  ;; Adds a set of column values to the budget report for a specific list
	  ;; of periods.
	  ;;
	  ;; Parameters:
      ;;   html-table - html table being created
      ;;   rownum - row number
      ;;   budget - budget to use
      ;;   acct - account being displayed
	  ;;   period-list - list of periods to use
      (define (gnc:html-table-add-budget-line-columns!
	  			html-table rownum
				budget acct period-list)
             (let* (
                    ;; budgeted amount
                    (bgt-numeric-val (gnc:get-account-periodlist-budget-value
                                  budget acct period-list))

                    ;; actual amount
                    (act-numeric-abs (gnc:get-account-periodlist-actual-value
                                  budget acct period-list))
                    (act-numeric-val (if reverse-balance?
                         (gnc-numeric-neg act-numeric-abs)
                         act-numeric-abs))

                    ;; difference (budget to actual)
                    (dif-numeric-val (gnc-numeric-sub bgt-numeric-val
                                 act-numeric-val GNC-DENOM-AUTO
                                 (+ GNC-DENOM-LCD GNC-RND-NEVER)))
                    )

			   (if (not (gnc-numeric-zero-p bgt-numeric-val))
			     (begin
			   		(set! bgt-total (gnc-numeric-add bgt-total bgt-numeric-val GNC-DENOM-AUTO GNC-RND-ROUND))
					(set! bgt-total-unset? #f))
				 )
			   (set! act-total (gnc-numeric-add act-total act-numeric-val GNC-DENOM-AUTO GNC-RND-ROUND))
	           (gnc:html-table-display-budget-columns!
	               html-table rownum acct
			       bgt-numeric-val act-numeric-val dif-numeric-val)
             )
      )

      (while (not (null? column-list))
	    (let* ((col-info (car column-list)))
		  (cond
		    ((equal? col-info '(quote total))
	           (gnc:html-table-display-budget-columns!
	               html-table rownum acct
		           bgt-total act-total
                   (gnc-numeric-sub bgt-total
                      act-total GNC-DENOM-AUTO
                      (+ GNC-DENOM-LCD GNC-RND-NEVER)
                   )
	            ))
		    ((list? col-info)
                (gnc:html-table-add-budget-line-columns!
	  		      	html-table rownum
				      budget acct col-info))
		    (else
                (gnc:html-table-add-budget-line-columns!
	  		      	html-table rownum
				      budget acct (list col-info)))
		    )
	      (set! column-list (cdr column-list))
		)
     )
    )
  )

  ;; Adds header rows to the budget report.  The columns are specified by the
  ;; column-list parameter.
  ;;
  ;; Parameters:
  ;;   html-table - html table being created
  ;;   colnum - starting column number
  ;;   budget - budget to use
  ;;   column-list - column info list
  (define (gnc:html-table-add-budget-headers!
           html-table colnum budget column-list)
    (let* ((current-col (+ colnum 1)))

      ;; prepend 2 empty rows
      (gnc:html-table-prepend-row! html-table '())
      (gnc:html-table-prepend-row! html-table '())

      ;; make the column headers
      (while (not (= (length column-list) 0))
	    (let* ((col-info (car column-list)))
		  (cond
		    ((equal? col-info '(quote total))
               (gnc:html-table-set-cell!
                html-table 0 (if show-diff? (+ current-col 1) current-col) "Total")
            )
		    ((list? col-info)
               (gnc:html-table-set-cell!
                html-table 0 (if show-diff? (+ current-col 1) current-col) "Multiple periods")
		    )
		    (else
             (let* ((date (gnc-budget-get-period-start-date budget col-info)))
               (gnc:html-table-set-cell!
                html-table 0 (if show-diff? (+ current-col 1) current-col) (gnc-print-date date))
               )
		    )
		  )
          (if show-budget?
            (begin
              (gnc:html-table-set-cell!  html-table 1
               current-col (_ "Bgt")) ;; Translators: Abbreviation for "Budget"
              (set! current-col (+ current-col 1))
            )
          )
          (if show-actual?
            (begin
              (gnc:html-table-set-cell!  html-table 1
               current-col (_ "Act")) ;; Translators: Abbreviation for "Actual"
              (set! current-col (+ current-col 1))
            )
          )
          (if show-diff?
            (begin
              (gnc:html-table-set-cell!  html-table 1
               current-col (_ "Diff")) ;; Translators: Abbrevation for "Difference"
              (set! current-col (+ current-col 1))
            )
          )
	      (set! column-list (cdr column-list))
        )
      )
    )
  )

  ;; Returns a list of the numbers between 0 and n
  ;;
  ;; Parameters:
  ;;   n - Highest number
  (define (0-to-n n)
    (cond
      ((= n 0) '(0))
      (else (append (0-to-n (- n 1)) (list n)))
    )
  )

  (let* ((num-rows (gnc:html-acct-table-num-rows acct-table))
         (rownum 0)
		 (column-info-list
           (list budget-period-num (0-to-n budget-period-num) (0-to-n 11)))
         (numcolumns (gnc:html-table-num-columns html-table))
	 ;;(html-table (or html-table (gnc:make-html-table)))
         ;; WARNING: we implicitly depend here on the details of
         ;; gnc:html-table-add-account-balances.  Specifically, we
         ;; assume that it makes twice as many columns as it uses for
          ;; account labels.  For now, that seems to be a valid
         ;; assumption.
         (colnum (quotient numcolumns 2))

	 )

    ''(display (list "colnum: " colnum  "numcolumns: " numcolumns))
    ;; call gnc:html-table-add-budget-line! for each account
    (while (< rownum num-rows)
           (let* ((env (append
			(gnc:html-acct-table-get-row-env acct-table rownum)
			params))
                  (acct (get-val env 'account))
                  (exchange-fn (get-val env 'exchange-fn))
                  )
             (gnc:html-table-add-budget-line!
              html-table rownum colnum
              budget acct column-info-list exchange-fn)
             (set! rownum (+ rownum 1)) ;; increment rownum
             )
           ) ;; end of while

    ;; column headers
    (gnc:html-table-add-budget-headers! html-table colnum budget column-info-list)

    )
    )
  ) ;; end of define

;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; renderer
;; set up the document and add the table
;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (renderer report-obj)
  (define (get-option pagename optname)
    (gnc:option-value
     (gnc:lookup-option
      (gnc:report-options report-obj) pagename optname)))

  (gnc:report-starting reportname)

  ;; get all option's values
  (let* ((budget (get-option gnc:pagename-general optname-budget))
         (budget-valid? (and budget (not (null? budget))))
         (display-depth (get-option gnc:pagename-accounts
                                    optname-display-depth))
         (show-subaccts? (get-option gnc:pagename-accounts
                                     optname-show-subaccounts))
         (accounts (get-option gnc:pagename-accounts
                               optname-accounts))
	     (bottom-behavior (get-option gnc:pagename-accounts optname-bottom-behavior))
         (budget-period-num (- (inexact->exact (get-option gnc:pagename-general optname-period)) 1))
         (row-num 0) ;; ???
         (work-done 0)
         (work-to-do 0)
         ;;(report-currency (get-option gnc:pagename-general
         ;;                             optname-report-currency))
         (show-full-names? (get-option gnc:pagename-general
                                       optname-show-full-names))
         (doc (gnc:make-html-document))
         ;;(table (gnc:make-html-table))
         ;;(txt (gnc:make-html-text))
         )

    ;; is account in list of accounts?
    (define (same-account? a1 a2)
      (string=? (gncAccountGetGUID a1) (gncAccountGetGUID a2)))

    (define (same-split? s1 s2)
      (string=? (gncSplitGetGUID s1) (gncSplitGetGUID s2)))

    (define account-in-list?
      (lambda (account accounts)
        (cond
          ((null? accounts) #f)
          ((same-account? (car accounts) account) #t)
          (else (account-in-list? account (cdr accounts))))))

    (define split-in-list?
      (lambda (split splits)
	(cond
	 ((null? splits) #f)
	 ((same-split? (car splits) split) #t)
	 (else (split-in-list? split (cdr splits))))))

    (define account-in-alist
      (lambda (account alist)
        (cond
	   ((null? alist) #f)
           ((same-account? (caar alist) account) (car alist))
           (else (account-in-alist account (cdr alist))))))

    ;; helper for sorting of account list
    (define (account-full-name<? a b)
      (string<? (gnc-account-get-full-name a) (gnc-account-get-full-name b)))

    ;; helper for account depth
    (define (accounts-get-children-depth accounts)
      (apply max
	     (map (lambda (acct)
		    (let ((children (gnc-account-get-children acct)))
		      (if (null? children)
			  1
			  (+ 1 (accounts-get-children-depth children)))))
		  accounts)))
    ;; end of defines

    ;; add subaccounts if requested
    (if show-subaccts?
        (let ((sub-accounts (gnc:acccounts-get-all-subaccounts accounts)))
          (for-each
            (lambda (sub-account)
              (if (not (account-in-list? sub-account accounts))
                  (set! accounts (append accounts sub-accounts))))
            sub-accounts)))

    (cond
      ((null? accounts)
        ;; No accounts selected.
        (gnc:html-document-add-object!
         doc
         (gnc:html-make-no-account-warning
	  reportname (gnc:report-id report-obj))))
      ((not budget-valid?)
        ;; No budget selected.
        (gnc:html-document-add-object!
          doc (gnc:html-make-generic-budget-warning reportname)))
      (else (begin
        (let* ((tree-depth (if (equal? display-depth 'all)
                               (accounts-get-children-depth accounts)
                               display-depth))
               ;;(account-disp-list '())

               ;; Things seem to crash if I don't set 'end-date to
               ;; _something_ but the actual value isn't used.
               (env (list (list 'end-date (gnc:get-today))
                          (list 'display-tree-depth tree-depth)
		 				  (list 'depth-limit-behavior
						             (if bottom-behavior 'flatten 'summarize))
                          ))
               (acct-table #f)
               (html-table (gnc:make-html-table))
               (params '())
               (paramsBudget
                (list
                 (list 'show-actual
                       (get-option gnc:pagename-display optname-show-actual))
                 (list 'show-budget
                       (get-option gnc:pagename-display optname-show-budget))
                 (list 'show-difference
                       (get-option gnc:pagename-display optname-show-difference))
                 (list 'show-totalcol
                       (get-option gnc:pagename-display optname-show-totalcol))
                 (list 'period-number budget-period-num)
                )
               )
               (report-name (get-option gnc:pagename-general
                                        gnc:optname-reportname))
               )

          (gnc:html-document-set-title!
           doc (sprintf #f (_ "%s: %s")
                        report-name (gnc-budget-get-name budget)))

          (set! accounts (sort accounts account-full-name<?))

          (set! acct-table
                (gnc:make-html-acct-table/env/accts env accounts))

          ;; We do this in two steps: First the account names...  the
          ;; add-account-balances will actually compute and add a
          ;; bunch of current account balances, too, but we'll
          ;; overwrite them.
          (set! html-table (gnc:html-table-add-account-balances
                            #f acct-table params))

          ;; ... then the budget values
          (gnc:html-table-add-budget-values!
           html-table acct-table budget paramsBudget)

          ;; hmmm... I expected that add-budget-values would have to
          ;; clear out any unused columns to the right, out to the
          ;; table width, since the add-account-balance had put stuff
          ;; there, but it doesn't seem to matter.

          (gnc:html-document-add-object! doc html-table))))
      ) ;; end cond

    (gnc:report-finished)
    doc))

;; Here we define the actual report with gnc:define-report
(gnc:define-report

 ;; The version of this report.
 'version 1

 ;; The name of this report. This will be used, among other things,
 ;; for making its menu item in the main menu. You need to use the
 ;; untranslated value here!
 'name reportname

 ;; The GUID for this report. This string should be unique, set once
 ;; and left alone forever after that. In theory, you could use any
 ;; unique string, even a meaningful one (!) but its probably best to
 ;; use a true uuid. Get them from `uuidgen | sed -e s/-//g` and paste
 ;; the results in here. You must make a new guid for each report!
 'report-guid "e698948392d36ddee6b77174151557c6"

 ;; The name in the menu
 ;; (only necessary if it differs from the name)
 'menu-name (N_ "Month and YTD Budget Report")

 ;; A tip that is used to provide additional information about the
 ;; report to the user.
 'menu-tip (N_ "Used for the monthly board meeting.")

 ;; A path describing where to put the report in the menu system.
 ;; In this case, it's going under the utility menu.
 'menu-path (list gnc:menuname-budget)

 ;; The options generator function defined above.
 'options-generator options-generator

 ;; The rendering function defined above.
 'renderer renderer)
