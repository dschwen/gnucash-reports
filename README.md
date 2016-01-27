# Gnucash Reports

This repository contains a custom gnucash report to generate a monthly budget report with a YTD column.

It is based on work by Phil Longstaff found [here](http://gnucash.1415818.n4.nabble.com/budget-report-td4662274.html).

## Install

Clone this repository into your `.gnucash` directory (you will get a directory `.gnucash/gnucash-reports`). Create `config.user` in the `.gnucash` directory and add the following line to it

```
(load  (gnc-build-dotgnucash-path "gnucash-reports/psl-budget.scm"))
```

Restart Gnucash.
