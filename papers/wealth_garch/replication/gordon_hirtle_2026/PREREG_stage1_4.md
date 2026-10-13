# Pre-registration: Gordon and Hirtle replication, stage 1, fourth file

Written and committed 13 October 2026, before the entity types below were
read for any holding company. It amends PREREG_stage1_3.md (commit
51a5a4d3) in how C1 reads the entity type for 2021-2024; the rest stands.

## What was seen before this file

The two NIC attributes files (`CSV_ATTRIBUTES_ACTIVE.zip`,
`CSV_ATTRIBUTES_CLOSED.zip`, uploaded by the author): their headers, and
counts over all institutions of rows, distinct IDs, end-date years, records
starting in 2025 or later, and entity types. Each file holds one row per
institution, its latest attribute record, with no history. 6,099 active
records start in 2025 or later. No holding company of our sample was
looked up.

## Why a change

PREREG_stage1_3.md takes the entity type "as of 31 December of the year".
The files keep only the latest record, so the type at a past date is known
only when that record already applied then; a record that starts in 2025
carries post-2024 information.

## The amended reading, 2021-2024

The entity type of a holding company in year t is, in this order:

1. its attributes record (active or closed file), if that record's
   `DT_START` is on or before 31 December of t and its `DT_END` after it;
2. otherwise its `RSSD9346` in the latest Chicago Fed file (1990-2020) in
   which it appears;
3. otherwise unknown.

C1 drops a bank-year only when the type read this way is known and is
neither BHC nor FHD; unknown types are kept and counted. No record that
starts after 31 December 2024 is used.

## Control, stated before the lookup

For 2020, the type read by step 1 (at 31 December 2020) is compared with
`RSSD9346` in the 2020 Chicago file, over holding companies that pass the
other stage 1 rules and have a step-1 record. Reported: the share on
which the two agree in keep-or-drop terms (BHC or FHD against any other
type).
