### 4.6–4.10 Built-in comorbidity indices (five entries)

Each row below is a complete separate template using the common scaffold,
its own one-variable `variables` list, `outputs = list(omop_output("score", rows = "study", format = "wide", grain = "episode", columns = names(variables), options = list(date_handling = "relative")))`
and `publish = "score"`. Parameters for **each**: common `index`, `select`
only. Fixed `reference_date = NULL` means the consumer episode's index date
(A-2), not an unbound parameter. These templates intentionally offer no
`lookback` argument: the built-in score language has none.

| Entry | Exact variable declaration | Expected output | Documentation text |
|---|---|---|---|
| `comorbidity.charlson` | `variables <- list(charlson = omop_score("charlson", reference_date = NULL))` | `score`: `K + charlson` (numeric score) | “Assign the server's reviewed Charlson score using condition history on or before the index date. The existing category, hierarchy and weighting implementation is retained; this is not an age-adjusted variant or a lookback-limited score.” |
| `comorbidity.chads2` | `variables <- list(chads2 = omop_score("chads2", reference_date = NULL))` | `score`: `K + chads2` (numeric score) | “Assign the existing CHADS2 score with condition history through index and age points at the same reference date. The recipe does not itself restrict the cohort to an indication population or turn the score into a treatment recommendation.” |
| `comorbidity.chadsvasc` | `variables <- list(chadsvasc = omop_score("chadsvasc", reference_date = NULL))` | `score`: `K + chadsvasc` (numeric score) | “Assign the existing CHA2DS2-VASc score using its reviewed mappings and index-date age/sex rules. Eligibility for a particular clinical analysis must be specified in the study population.” |
| `comorbidity.dcsi` | `variables <- list(dcsi = omop_score("dcsi", reference_date = NULL))` | `score`: `K + dcsi` (numeric score) | “Assign the existing DCSI implementation from condition history through index. Its reviewed vocabulary mapping is retained; it is not replaced by generic descendants of analyst-selected concepts.” |
| `comorbidity.hfrs` | `variables <- list(hfrs = omop_score("hfrs", reference_date = NULL))` | `score`: `K + hfrs` (numeric score) | “Assign the existing HFRS implementation from condition history through index. The existing reviewed mapping and weights are retained; the recipe does not impose admission-specific or two-year lookback rules.” |

All require the built-in score's existing condition/person/vocabulary inputs.
Document the integrated mapping/weight artifact version or checksum in each
entry's metadata; do not invent a named published score variant that the
current implementation has not established. Empty-history and missing-input
behaviour must match the existing native score contract, verified against
independent category/weight fixtures. A library wrapper does not replace a
missing input with a score of zero. No new score weights or downloaded
vocabulary files are introduced.



The complete executable constructor expansion is in `definition.R` in this directory and is returned by `recipe_library_describe()`.
