# Encode a v2 plan for DataSHIELD transport without losing precision

Like `.ds_encode()` (URL-safe base64 JSON) but using the canonical
encoder's round-tripping numbers while preserving declaration order,
explicit JSON nulls and empty objects, so the server parses exactly the
plan whose semantic digest the client computed (unit conversion factors
such as 0.45359237 must not be rounded).

## Usage

``` r
.ds_encode_lossless(x)
```
