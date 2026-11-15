# Tutorial helpers

These scripts belong to the dsOMOPClient tutorial book. Readers download them, as one ZIP or one by one, from the book's
Getting started page, and keep them under `book-work/tools/book/`.

- `load-practice.R` downloads the two public synthetic datasets (GiBleed and Synthea27Nj) and loads them into local SQLite files. It uses `load-numeric.R`.
- `practice.R` opens the local practice server, DSLite, with both datasets as resources.
- `connection.R` holds the connection settings. Switching to a real Opal server changes only this file.
- `fixture.R` builds the small synthetic questionnaire dataset of the education and cause-of-death chapters.
- `display.R` prints the book's outputs.
- `case-helpers.R` and `capstone.R` support the case studies and the capstone.
- `EunomiaDatasets-LICENSE.txt` is the licence of the public synthetic datasets.
