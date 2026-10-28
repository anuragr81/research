# Journal rankings

Lists used for the table of citations that the manuscript skeleton carries
(journal and ABDC rating of each cited paper).

| File | What | sha256 |
|---|---|---|
| `ABDC-JQL-2025-v3-210926.xlsx` | ABDC Journal Quality List, 2025 major review, "Current to September 2026" (version 3 of 21 September 2026, by its file name). Supplied by the author on 28 October 2026. | `4d344d94da1811a274dfe758c5d9d6f055d1243e112d2266e763e477f65659c8` |

The workbook has one sheet per edition: `2025 JQL` (the current list) and the
2022, 2019, 2016, 2013 and 2010 lists. On the `2025 JQL` sheet the header is
row 8 (Journal Title, Publisher, ISSN, ISSNOnline, Year Inception, FoR,
2025 rating) and there are 2,648 rated titles: 219 A*, 630 A, 859 B, 940 C.
Ratings and ISSNs carry trailing whitespace, so strip before matching. Match
on ISSN where possible rather than on title.

Annals of Finance (ISSN 1614-2446, online 1614-2454, Springer) is rated B in
the 2025 list.
