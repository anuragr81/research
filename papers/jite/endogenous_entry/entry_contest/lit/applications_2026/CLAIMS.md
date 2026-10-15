# Primary sources for the two institutions in the Research question

These are the sources behind the examples in Section 1 of `MANUSCRIPT.tex`
("Research question"). Every quotation below is from a primary document: a
municipality's own selection rules, or the official Italian legislative
database (Normattiva). The documents were downloaded on 14 October 2026 (RICS and the UK
register: 15 October 2026), cached in `~/.cache/entry_contest/applications/`
and pinned by sha256.
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
| AP-11 | rics/apc_candidate_guide_2026.pdf | "RICS accredited degree – At least 24 months’ structured training and a minimum of 96 hours’ continuing professional development (CPD)." | Chartered status needs structured training before assessment |
| AP-12 | rics/apc_candidate_guide_2026.pdf | "The assessors: chartered surveyors who conduct your final assessment interview." | Chartered status needs a competence assessment by interview |
| AP-13 | rics/apc_candidate_guide_2026.pdf | "Each competency is defined at three levels of attainment. You must reach the required level in a logical progression and in successive stages." | The assessment is of competence, so the credential attests skill |
| AP-14 | rics/join_fees.html | "APC with Structured Training/APC without Structured Training /Academic Assessment Fee £520.00 £173.00" | The 2026 UK enrolment fee and candidate subscription are fixed sums (the row reads enrolment fee, then 2026 subscription) |
| AP-15 | rics/join_fees.html | "Chartered Recognised Professional Qualification Fee £136.00 £578.00" | The 2026 UK chartered subscription is £578 (the row reads entry fee, then 2026 subscription) |
| AP-16 | rics/gov_regulated_profession.html | "Reserved activities None - restrictions apply to use of title only" | On the UK register, the profession reserves no activity; only the title is protected, so non-holders still practise |
| AP-17 | rics/vr_faq_2025.pdf | "RICS members undertaking valuation work must join the Valuer Registration Scheme (VRS)." | Within RICS, valuation work needs registration, a closure inside the profession |
| AP-18 | rics/vr_faq_2025.pdf | "where members undertake only excepted valuation assignments (see Red Book PS1 para 5), registration is not required" | Registration has exceptions |

## Reading recorded

| ID | Point | Our reading |
|---|---|---|
| AP-D1 | Chiba, AP-3 to AP-5 | In Chiba the points are earned by using paid childcare at a threshold of hours, not by buying a certificate. The price is set by the provider, so it is not one fixed fee. Of the four properties of the application search, P2 (families without it still compete) and P3 (it buys standing, not skill) hold; P1 (a fixed price) holds only partly. |
| AP-D2 | Italy, AP-8 to AP-10 | The certificate needs compliance as well as a payment, so P3 holds only partly. The example is used for the verification story (the authority weights an attestation it does not make itself), not as a pure purchase. Firms without the certificate still bid and can win. AP-8 gives extra points, not a condition of entry, so P2 holds. |
| AP-D4 | RICS, AP-11 to AP-18 (author's question, 15 Oct 2026) | Chartered status (MRICS) is a cheap-to-verify token that lenders, courts and clients weight, which is the verification story of the Research question. Against the four properties: P3 (the outlay buys standing, not skill) fails, because the credential needs structured training and a competence assessment by interview (AP-11 to AP-13), so holders are selected on skill and the token is part of the base component, not the bought one. P1 (a fixed outlay) holds only partly: the fees are fixed sums (AP-14, AP-15), but the main cost is training time. P2 (non-holders still compete) holds for the profession, since no activity is reserved and only the title is protected (AP-16), and fails inside RICS for valuation work, which needs registration (AP-17, AP-18), the closure boundary. P4 (wealth only through affordability) is weak, since training time and employer sponsorship carry wealth's effect. RICS is therefore an example of why an institution weights a credential, and of the boundary where the model stops, not of a purely bought component. It is not used in the manuscript. |
| AP-D3 | Takarazuka petition (`takarazuka_chinjo21_2023.pdf`, cached, not quoted in the manuscript) | A 2023 citizens' petition to Takarazuka's council says that a paid enrolment certificate, available on a provisional offer, earned five points. It is primary as a petition but not as the city's rule, and the city's own criteria pages could not be retrieved, so the manuscript does not rely on it. |
