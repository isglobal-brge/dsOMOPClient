# Learn dsOMOPClient

Build useful study tables while patient records stay on the server.
This book teaches the released **2.10.4 native recipe API**, one small step at a time.
Every worked lesson includes executed code, real synthetic results and an exercise
with its solution.

[Start with chapter 0 →](articles/book-00-start.html)

## Your path through the book

| Chapter | What you will do |
|---|---|
| [0. Getting started](articles/book-00-start.html) | Install and open a practice session |
| [1. DataSHIELD and OMOP](articles/book-01-mental-model.html) | Understand what stays private |
| [2. Connecting](articles/book-02-connect.html) | Attach a resource and use symbols |
| [3. Exploring the CDM](articles/book-03-explore.html) | Read schema and safe counts |
| [4. Vocabulary](articles/book-04-vocabulary.html) | Find concepts and build sets |
| [5. A first recipe](articles/book-05-recipe.html) | Create education and cause columns |
| [6. Events, windows and reducers](articles/book-06-events.html) | Choose records, time and one-value summaries |
| [7. Populations](articles/book-07-populations.html) | Define eligibility, episodes, new users and matching |
| [8. Output formats](articles/book-08-outputs.html) | Choose shapes, censoring and joins |
| [9. Dates](articles/book-09-dates.html) | Use parts, periods, anchored ages and partial dates |
| [10. Concepts as categories](articles/book-08-harmonise.html) | Translate, recode, rename and harmonise two sites |
| [11. Reusable studies and verbs](articles/book-11-parameters.html) | Bind parameters and use mutate/filter |
| [12. Privacy](articles/book-12-privacy.html) | Understand final release and refused columns |
| [13. Visit walkthrough](articles/book-06-visits.html) | Combine offsets, dates and event rows |
| [14. Verbal-autopsy capstone](articles/book-07-verbal-autopsy.html) | Combine answers, age, visit periods and cause |
| [15. Working with lab values](articles/book-15-labs.html) | Reduce, convert and pool synthetic lab values |
| [16. Longitudinal data](articles/book-16-longitudinal.html) | Shape events, build risk sets and model repeated measures |

The main lessons use one DataSHIELD connection at a time. GiBleed teaches the
basics; Synthea27Nj supplies numeric labs. Both have synthetic people. A separately
labelled 24-person test fixture supplies the colleague’s questionnaire examples. Chapter 10 includes
a two-connection vocabulary harmonisation demonstration. Chapter 15 pools numeric
summaries and a Gaussian model across disjoint synthetic sites.
[Reproduction and migration notes](articles/book-09-reproduce.html) explain how
the examples run and how older recipe conventions differ. The visit walkthrough
and capstone retain their original URLs so earlier links continue to work.

## Learn with real execution

The displayed results come from DSLite running the matching released server and
client against the chapter’s stated synthetic dataset. The verbal-autopsy example
uses the released native follow-up recipe, including exact age at death and
calendar precision. The optional local Opal/GiBleed instructions explain the
production-shaped connection route; GiBleed is a different dataset.

Read the book in order. Use the function reference when you need argument details.
The older reference guides are retained for existing links and are not the book's
learning path. This is an OHDSI-inspired theme, not an OHDSI endorsement.
