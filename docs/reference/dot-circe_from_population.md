# Convert one recipe population's filter list into Circe building blocks

Splits the population's flat criteria into an ObservationWindow (from
prior_observation / followup), demographic criteria, and occurrence/
measurement criteria for InclusionRules. The PrimaryCriteria comes only
from an explicit `omop_index_event`; inclusion filters are never
promoted. OR `omop_filter_group`s become nested Circe CriteriaGroups
(Type ANY). Unsupported filter types fail closed.

## Usage

``` r
.circe_from_population(pop, sets)
```
