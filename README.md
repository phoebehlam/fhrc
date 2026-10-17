# fhrc

## Luminex cell-culture QA report

```r
library(fhrc)
luminex_qa_report("C:/path/to/STUDY VISIT master file.xlsx", sheet = "for master",
                  hsp60 = TRUE, r848 = TRUE, il10 = FALSE)
```

Writes one self-contained HTML file next to the master file: a summary of every problem
found, missing wells and data-entry errors, stimulation/inhibition order (counts and
charts), cortisol (and IL-10) dose-response, plate effects, and each flagged
participant's full data.

* `hsp60`, `r848`, `il10`: say whether the study has those wells.
* `luminex_qa()` returns the same results as tables (`qa$summary`, `qa$contrasts`, ...).
* `luminex_example()` makes a fake master file with planted problems, for training.
* The template is also under *File > New File > R Markdown > From Template*.
