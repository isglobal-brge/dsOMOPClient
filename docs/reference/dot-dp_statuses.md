# DP status of every server, negotiated with the protected-release contract

dsOMOPClient 2.10.3 declares the protected-release payload protocol it
implements when it asks for the status (dsOMOP 2.10.3 servers refuse
status calls from older clients with an upgrade message). A server older
than dsOMOP 2.10.3 does not take that argument; it is asked again
without it, so its legacy status can still be inspected and its legacy
releases reported per site. Every requested server must answer.

## Usage

``` r
.dp_statuses(datasources)
```

## Arguments

  - datasources:

    Named DataSHIELD connections.

## Value

An `omop_dp_status` list; attribute `unpatched` names the servers that
predate dsOMOP 2.10.3.
