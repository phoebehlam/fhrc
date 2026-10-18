# fhrc

## Luminex cell-culture QA, before repeats (step 3)

```r
library(fhrc)
luminex_qa_report("C:/path/to/STUDY VISIT master file.xlsx", sheet = "for master",
                  hsp60 = TRUE, r848 = TRUE, il10 = FALSE)
```

Writes one self-contained HTML file next to the master file: a summary of every problem
found, missing wells and data-entry errors, stimulation/inhibition order (counts and
charts), cortisol (and IL-10) dose-response, plates with a run of the same problem, and
each tagged participant's full data.

* `hsp60`, `r848`, `il10`: say whether the study has those wells.
* `luminex_qa()` returns the same results as tables (`qa$summary`, `qa$contrasts`, ...).
* `luminex_example()` makes a fake master file with planted problems, for training.
* The template is also under *File > New File > R Markdown > From Template*.

## Luminex cell-culture pre-scoring QA (after repeats)

```r
luminex_postqa_report("C:/path/to/STUDY VISIT master file.xlsx", sheet = "final master")
```

Looks at the values the way the scoring step will (negatives to 0, US subtracted, ln),
tags participants whose scores would go wrong, and proposes a plan (wells to set to NA,
US to ignore). Review it with the wet-lab team, then run again listing only the
participants you disagree with (`na`, `ignore_us`, `keep`) and `export = TRUE`: the whole
"final master" sheet is saved next to the master with the plan applied (and a `fix`
column), ready for scoring.
