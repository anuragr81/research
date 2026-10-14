# Primary sources for the two institutions in the Research question

These are the sources behind the examples in Section 1 of `MANUSCRIPT.tex`
("Research question"). Every quotation below is from a primary document: a
municipality's own selection rules, or the official Italian legislative
database (Normattiva). The documents were downloaded on 14 October 2026, cached
in `~/.cache/entry_contest/applications/` and pinned by sha256.
`verify_applications.py` checks each quotation verbatim against the cached
document. Ignoring whitespace is the only normalisation.

The same suite checks the quotations in the section "Application search of
14 October 2026" of `LITERATURE.tex`: the primary-source quotations against
these documents, and one sentence from each paper found against
`abstracts_2026.json` (abstracts from Crossref or OpenAlex, 14 October 2026,
pinned by sha256).

The application search of 14 October 2026 (about 45 settings; see `TODO.md`)
chose these two because they are current and their rules are public. The
historical sale of status (Qing *jiansheng*) was dropped at the author's
request in favour of current examples.

## Quotations

| ID | Document | Quotation | What it supports |
|---|---|---|---|
| AP-1 | chiba_senkoukijun_r9.pdf | "選考順位内で入所選考点数が高い方から、希望している保育園等に空きがある場合に、内定します。" | Within a priority tier, places go to the highest selection points |
| AP-2 | chiba_senkoukijun_r9.pdf | "優先項目に該当する児童（Ａ）から選考を行い" | Priority tiers come first |
| AP-3 | chiba_senkoukijun_r9.pdf | "認可外保育施設又はベビーシッター、幼稚園の実施する預かり保育等" | The adjustment item covers unlicensed nurseries and babysitters |
| AP-4 | chiba_senkoukijun_r9.pdf | "一時預かり事業の利用(月６４時間以上)" | The item requires at least 64 hours a month |
| AP-5 | chiba_senkoukijun_r9.pdf | "＋４" | The item adds four points (on the same line of the table as AP-3; checked by position in the suite) |
| AP-6 | chiba_kaisei_r8.pdf | "保育の必要性を点数化し、点数の高い方から、入所の決定をします。" | The city's statement of the allocation rule |
| AP-7 | chiba_senkoukijun_r9.pdf | "令和９年４月利用選考から適用" | The rules are current (applied from April 2027) |
| AP-8 | normattiva_dlgs36_2023_art108.html | "le stazioni appaltanti prevedono, nei bandi di gara, negli avvisi e negli inviti, il maggior punteggio da attribuire alle imprese per l'adozione di politiche tese al raggiungimento della parità di genere comprovata dal possesso della certificazione della parità di genere" | Contracting authorities must award extra points to certified firms |
| AP-9 | normattiva_dlgs198_2006_art46bis.html | "al fine di attestare le politiche e le misure concrete adottate dai datori di lavoro per ridurre il divario di genere" | The certificate attests the firm's policies |
| AP-10 | normattiva_dlgs198_2006_art46bis.html | "i parametri minimi per il conseguimento della certificazione della parità di genere" | The certificate requires meeting parameters set by decree, so it is not bought by a fee alone |

## Reading recorded

| ID | Point | Our reading |
|---|---|---|
| AP-D1 | Chiba, AP-3 to AP-5 | In Chiba the points are earned by using paid childcare at a threshold of hours, not by buying a certificate. The price is set by the provider, so it is not one fixed fee. Of the four properties of the application search, P2 (families without it still compete) and P3 (it buys standing, not skill) hold; P1 (a fixed price) holds only partly. |
| AP-D2 | Italy, AP-8 to AP-10 | The certificate needs compliance as well as a payment, so P3 holds only partly. The example is used for the verification story (the authority weights an attestation it does not make itself), not as a pure purchase. Firms without the certificate still bid and can win. AP-8 gives extra points, not a condition of entry, so P2 holds. |
| AP-D3 | Takarazuka petition (`takarazuka_chinjo21_2023.pdf`, cached, not quoted in the manuscript) | A 2023 citizens' petition to Takarazuka's council says that a paid enrolment certificate, available on a provisional offer, earned five points. It is primary as a petition but not as the city's rule, and the city's own criteria pages could not be retrieved, so the manuscript does not rely on it. |
