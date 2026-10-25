# Pre-registration: fetching the 2025 hold-out

Written and committed 25 October 2026, before any 2025 data is requested.
The author agreed on 25 October 2026 to open the hold-out, under
comparison/PREREG_comparison_20.md, which fixes the test it serves.

## Source, fields, storage

- Source: the FDIC BankFind API, endpoint `financials`, exactly as in
  PREREG_common_data.md; the same fields, every one defined in the pinned
  FDIC definitions workbook (the fetch refuses any that is not).
- Report dates: 2025Q1 to 2025Q4, the four quarter ends of 2025. Nothing
  from 2026 is requested; the fetch stops if any returned row has a report
  date after 31 December 2025 or before 1 January 2025.
- Storage: `common_data/holdout/fdic_q_2025.csv.gz`, written as the
  2004-2024 files are (rows sorted by certificate number, gzip without a
  timestamp), kept apart from them so that nothing loading the common
  dataset picks it up by accident. `fetch_common_data.py` and its 2024
  guard are not changed; `fetch_holdout.py` reuses its functions.
- The sha256 and row counts are recorded in `results_holdout_fetch.md` and
  pinned by the hold-out test. Nothing in 2025 is looked at before
  comparison/holdout.py runs, apart from the counts the fetch prints.
