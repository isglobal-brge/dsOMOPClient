# dsOMOPClient 2.7.3

- Adapt memory-mode plan and recipe execution to dsOMOP 2.7.0's default
  exclusive DP channel: automatically disable observed factor-level discovery
  across the selected federation with an explanatory message.
- Validate and print the DP status's `exclusive` flag, retaining compatibility
  with dsOMOP 2.6.0, which omits it.
- Standard statistics helpers now surface exclusive-mode refusals as clear
  errors directing analysts to `ds.omop.dp.release()`; no partial standard
  result is returned when a selected server refuses the channel.

# dsOMOPClient 2.7.2

- Document the server 2.6.0 default-enabled DP release channel, custodial opt-out,
  derived resource/snapshot identities, privacy epoch and persistent state
  requirements. Client release calls remain explicit; no API defaults change.
