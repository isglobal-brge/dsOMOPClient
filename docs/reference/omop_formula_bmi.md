# Versioned clinical formulas expanded into private numeric declarations

Versioned clinical formulas expanded into private numeric declarations

## Usage

``` r
omop_formula_bmi(weight, height, version = "1.0.0")

omop_formula_map(systolic, diastolic, version = "1.0.0")

omop_formula_ratio(numerator, denominator, unit, version = "1.0.0")

omop_formula_egfr_ckdepi2021(creatinine, age, sex, sex_map, version = "1.0.0")
```

## Arguments

  - weight, height:
    
    Typed numeric variables in coordinate:kg and coordinate:metres.

  - version:
    
    Installed exact definition version, currently 1.0.0.

  - systolic, diastolic:
    
    Typed numeric variables in coordinate:mmHg.

  - numerator, denominator:
    
    Aligned numeric declarations with resolved units.

  - unit:
    
    Declared result coordinate for a ratio.

  - creatinine:
    
    Typed selected creatinine in coordinate:idms\_creatinine\_mg\_dl.

  - age:
    
    Completed calendar years from omop\_age\_at at the creatinine
    anchor.

  - sex:
    
    Admitted categorical variable whose source is justified by the
    study.

  - sex\_map:
    
    Named list female and male, each a nonempty vector of disjoint
    stable category identities; no missing, unknown or wildcard
    identities.

## Value

An ordinary numeric variable expanded into closed primitives, with
immutable client formula provenance. No patient computation occurs here.

## Details

Declare named physical coordinates with an explicit omop\_number\_unit
conversion from the source unit; for example micromol/L to IDMS
creatinine mg/dL divides by 88.4. No unit, source concept, sex
interpretation or anchor is guessed. All bindings use stage=presented so
conversions and log/exp transforms survive. Independently selected
measurements do not imply contemporaneity. Invalid inputs become private
typed NA; unavailable sources retain unavailability. Age requires
completed calendar years, not annual\_years or baseline age at an
unrelated visit. Use a first/last/nth measurement selector with
missing=keep and a matching start-date anchor, or a day-zero selector
and age at index. Put source-unit normalization on the shared event used
by both creatinine and its age anchor. An input-local source map must
match that shared map; keep explicit numeric coordinate conversions on
the creatinine input. Sex mapping is an explicit public protocol
assertion, not an interpretation of a field named gender. CKD-EPI 2021
uses no race operand and is an adult estimate.
