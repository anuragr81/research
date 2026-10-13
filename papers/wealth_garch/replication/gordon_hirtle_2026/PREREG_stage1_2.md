# Pre-registration: Gordon and Hirtle replication, stage 1, second file

Written and committed 13 October 2026, before the structure data below were
fetched. It amends PREREG_stage1.md (commit 799cfea6) in one rule only; every
other part of that file stands.

## What was seen before this file

- The output of a development run of stage1.py on 1990-2020 (TODO.md,
  13 October 2026): sample size, R1 to R5 on those years.
- The four FFIEC December files 2021-2024 (`BHCF20211231.ZIP` to
  `BHCF20241231.ZIP`, uploaded by the author): their column headers, row
  counts, report dates (December of the stated year only) and the first
  values of one row. No variable was tabulated.
- That those files carry no holder fields: `RSSD9348` and `RSSD9364`, with
  46 other structure items present in the Chicago Fed files, are absent.

## Why a change

PREREG_stage1.md defines top-tier by `RSSD9348`. It cannot be read for
2021-2024. Treating a missing holder as "no holder" would make every
holding company top-tier in those years.

## The amended rule

For 2021-2024, top-tier is read from the FFIEC National Information Center
structure files, which the Fed's derivation of `RSSD9348` uses (MDRM item
9347: relationships with level 1, equity, control and regulatory
indicators set). A holding company is top-tier in year t if no relationship
in the NIC relationships file has it as offspring, with `RELN_LVL` = 1,
`CTRL_IND` = 1, `REG_IND` = 1, `EQUITY_IND` = 1, and a start date on or
before 31 December of year t and an end date after it. U.S.-owned stays
`RSSD9101` = "Y", which the FFIEC files carry. For 1990-2020 the rule of
PREREG_stage1.md is unchanged.

The NIC files are current snapshots and hold relationships dated in 2025;
only relationships in force on a December date of 2024 or earlier are used.

## Data

The NIC relationships file in CSV, downloaded by the author from the NIC
data download page (scripted access is refused), pinned by sha256 in the
results file.

## Control, stated before the fetch

The amended rule is also applied to 2015-2020, where `RSSD9348` is known,
among holding companies that pass the other sample rules. Reported: the
share of bank-years on which the two rules agree. If they agree on less
than 97%, the 2021-2024 sample is reported both ways (amended rule, and
holding companies top-tier in 2020 carried forward), with the amended rule
primary.

## Code

stage1.py stops if a year's file has neither `RSSD9348` nor the NIC rule
available, rather than defaulting.
