# Reproducing the tutorial book

Work in the book client checkout on docs/book. The matching book server and client
worktrees are both at 2.10.5. `tools/book/run-pod.sh` installs them into this job's
private pod library and executes every `.Rmd.orig` chapter and solution. Pass
`precompute` to reuse the installed runtime and an optional chapter regular
expression to run only changed lessons. No other worktree is used.

`tools/book/precompute.R` runs fresh R processes and the source-data assertions in
verify.R, verify-book2.R, verify-book3.R, verify-book4.R, verify-longitudinal.R and
verify-standalone-showcase.R. These assertions alone may inspect public synthetic
server memory; reader helpers and chapters use protected analyst calls.

Build with `tools/book/pod.sh Rscript tools/book/build-site.R`, retrieve docs/,
then run `verify-bundle.R` on the pod to execute every chapter in a fresh directory
using only the downloadable helper ZIP. Generated `.Rmd` and HTML contain actual
knitr output. `book_symbols()` displays only the returned symbol mapping; keep
an execution result to inspect its attached codebook explicitly.

The 25 pages comprise a showcase, chapters 0–18 and five case studies (19–23).
Chapter 18 is the final tutorial capstone. The YAML menu, article index and Next
links share that order. `audit-structure.py`, `audit-content.py` and
`audit-outputs.py` verify structure, wording, links and exact HTML output fidelity.
Run `audit-site.js` with Playwright Chromium for mobile and desktop geometry,
copy controls, collapsible solutions and actual bounded output scrolling.

Keep evidence outside the repository in /workspace/dsomop/design/book-evidence/.
The ignored tools/book/evidence/ path may point there while running the tools. The targeted package
wrapper test-book-examples.R invokes the same chapter execution path. Full
package suites belong to integration/release and are not run for this book job.

Practice data are the public synthetic GiBleed and Synthea27Nj resources. The
separate questionnaire fixture models missing answers and partial birth dates;
its optional category catalogue contains explicitly synthetic ATC/ICD teaching
relationships. Neither clinical resource is altered to manufacture an absent
classification, assay or sex assignment. The formula chapter checks the Synthea eGFR prerequisites, then executes the
named helper on explicitly synthetic standardized serum inputs from the optional
questionnaire fixture. An independent direct-power equation verifies the result.

The fixture and practice helpers apply a scoped DSLite quoted-string lexer repair
needed by dsBase calls. Real server methods and disclosure gates remain in use.
Only disclosed tables and summaries appear in the book. Package version and
exact runtime source hashes are retained with the evidence and job report.
