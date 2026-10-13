# Pre-registration: Gordon and Hirtle replication, stage 1, third file

Written and committed 13 October 2026, before the data below were fetched
and before our sample was tabulated by any of the candidates. It adds an
investigation; it changes nothing in PREREG_stage1.md, PREREG_stage1_2.md or
results_stage1.md, which stand.

## What was seen before this file

- results_stage1.md: 7,843 bank-years against their 6,448.
- Our bank-years by year (TODO.md, 13 October 2026): 165 to 220 a year
  through 2011, 202 in 2011 and 249 in 2012, 250 to 317 thereafter.
- The MDRM definitions of `RSSD9331` (entity type code) and `RSSD9346`
  (entity type, values including BHC, FHD, SLHC). No entity type has been
  tabulated for our sample.

## Aim

Find which part of our sample rule differs from theirs, and decide, by a
rule fixed here, whether our stage 1 sample stands or is narrowed.

## Candidates, fixed here

C1. Entity type. The paper describes its sample as bank holding companies
    (pp. 3, 7, 11). Keep only filers whose `RSSD9346` is BHC or FHD (a
    financial holding company that is a bank holding company); drop all
    other entity types, savings and loan holding companies among them. For
    2021-2024, where the FFIEC files lack `RSSD9346`, the entity type comes
    from the NIC attributes files (active and closed institutions, CSV),
    taken as of 31 December of the year.
C2. Price index. The GDP implicit price deflator (FRED `GDPDEF`, annual
    average of the four quarters) in place of the CPI for "2024 dollars".
C3. Continuity. Keep a holding company in year t only if it also has a
    December Y-9C report in year t-1 (its prior year-end equity was
    reported by the same filer).

Each candidate alone, and all combinations (C1+C2, C1+C3, C2+C3,
C1+C2+C3), are run.

## Their counts by year

Their Chart 2 (p. 10) plots the number of sample banks per year. We read
it from the PDF page rendered at 300 dpi, by measuring bar heights against
the axis ticks, and record each reading with a stated precision of the
nearest 5. The readings are approximate and are used only to locate the
years where samples differ, not in any verdict.

## Reported

For our stage 1 sample and each candidate sample: the total, the count by
year, and R1 to R5 of PREREG_stage1.md with the same reproduction rule.
For C1: the counts of dropped bank-years by entity type and year.

## Decision rule, fixed here

- C1 is adopted as the replication's primary sample if it drops any
  bank-years, because its ground is the paper's own description of the
  sample, not the count.
- C2 and C3 are reported and not adopted: the paper states neither, so
  matching their count would be choosing a sample by fit.
- Whatever is adopted, results_stage1.md stays as run, and the adopted
  sample is reported in a new results file. The manuscript's first draft
  states our sample rule, theirs as written, the gap, and which results
  depend on it.

## Data

- NIC attributes files, active and closed institutions, CSV, downloaded by
  the author from the NIC data download page after this commit, pinned by
  sha256.
- FRED `GDPDEF`, 1989-2024, fetched by script after this commit, pinned.
- Nothing from 2025: attributes in force on 31 December 2024 or earlier
  only.
