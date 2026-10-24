# Pre-registration: the testing thread's comparison, thirteenth file

Written and committed 24 October 2026, before any ticker is matched under
PREREG_comparison_12.md. It amends that file's country test.

## What was seen before this file

While checking the NIC attributes files' field formats: the record of
RSSD 1039502 (JPMorgan Chase & Co.) has an empty CNTRY_INC_NM and
CNTRY_NM "UNITED STATES"; the record of RSSD 1074156 carries the legal
name "TRUIST FINANCIAL CORPORATION"; the active file has one record per
RSSD, with the current legal name only. Over all records, CNTRY_INC_NM is
empty for most (56,064 of the active file's records); among holding-company
records it is empty for 3,369 located in the United States and filled for
31 located there, while foreign holding companies have it filled with
their own country (for example Brazil 10, Canada 9, Japan 6). No sample B
parent other than JPMorgan Chase was looked up.

## Amendment

Rule 2 of PREREG_comparison_12.md reads: the parent's country is
CNTRY_INC_NM if that field is filled, and CNTRY_NM (the country of its
location) if not; the bank is covered only if that country is the United
States. Rule 3's "legal names the RSSD has carried" are, in these files,
the name on its active record or, if closed, on its closed records.
Everything else in PREREG_comparison_12.md stands.
