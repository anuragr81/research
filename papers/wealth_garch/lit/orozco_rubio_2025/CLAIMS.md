# Claims: Orozco and Rubio (2025)

Bib key `orozcorubio2025`. Version read: Journal of Money, Credit and Banking 57(6), September 2025, pp. 1421-1464, open access (CC BY), doi 10.1111/jmcb.13230; 44 PDF pages, printed pages 1421 to 1464 (PDF page = printed page - 1420). Read in full, the Internet Appendix not included. Evidence [F].

## Quotations

| ID | Page | Quotation | What we rely on |
|---|---|---|---|
| OR-Q1 | 1421 | a significant discontinuity at the 10% regulatory capital threshold, where banks receive benefits for exceeding it. | Banks bunch just above the well-capitalised 10% total capital ratio: the line banks guard is known empirically. |
| OR-Q2 | 1421 | Banks exceed it to pay lower deposit insurance fees, access brokered deposits, and expanded financial activities. | Why the 10% line matters to banks: the insurer's pricing (ledger A11), the uninsured funding market, and permitted activities. |
| OR-Q3 | 1425 | Our results align with the notion that capital is costly but suggest that the 10% ratio is the relevant minimum, elucidat-ing why previous studies find weak evidence of binding capital requirements at the 8% regulatory minimum | The 10% line, not 8%, is the minimum that binds; they read Gropp and Heider's weak regulation effect this way. |
| OR-Q4 | 1422 | the discontinuity vanished after 2009 | The 10% discontinuity is a 1996-2009 fact. |
| OR-Q5 | 1430 | That is, this new buffer increases the minimum reg-ulatory capital to at least 10.5%. | Under Basel III the conservation buffer moves the effective minimum to 10.5%, our R. |
| OR-Q6 | 1430 | the ratios imposed by Basel III make the 10% nonbind-ing for banks | After 2009 the 10% line no longer binds. |
| OR-Q7 | 1429 | Banks are 3.1 times more likely to report regulatory capital just above than just below the 10% threshold. | Size of the discontinuity, 1996-2009. |
| OR-Q8 | 1432 | we find that the discontinuity is significant for public and private commercial banks | Listing does not drive the bunching (compare results_access.md). |
| OR-Q9 | 1432 | The discontinuity is economically insignificant for the 8% | No bunching at the legal minimum. |
| OR-Q10 | 1427 | Our main sample is restricted to banks that reported regulatory capital between 8% and 12%. | Their evidence is local to the threshold; it says nothing on buffers far above it. |
| OR-Q11 | 1427 | Banks with regulatory capital ratios over 12% might have different business models, characteristics, and incentives. | They leave the far-above-threshold banks, our F2, outside their design. |
| OR-Q12 | 1450 | Equity is singularly employed in 55% of the cases. | Banks reach the line mostly with equity, including parent transfers. |
| OR-Q13 | 1455 | This discontinuity disappears with the new capital requirements introduced by Basel III. | Their conclusion on the post-2009 period. |

## Lean results

| Name | Statement |
|---|---|
| `OrozcoRubio.wellcap_margin` | The well-capitalised line sits 2 points above the 8% minimum. |
| `OrozcoRubio.basel3_floor_eq_R` | Minimum 8% plus the 2.5% conservation buffer is 10.5%, our reference R. |
| `OrozcoRubio.basel3_floor_above_pca` | That floor exceeds the 10% line, which is why it stops binding (OR-Q5, OR-Q6). |
| `OrozcoRubio.control_floor_needs_buffer` | Without the buffer the floor (8%) does not exceed 10%. |
| `OrozcoRubio.control_margin_not_buffer` | The 2-point PCA margin is not the 2.5-point buffer. |

## Readings recorded

| ID | Where | Reading adopted | Reason |
|---|---|---|---|
| OR-D1 | pp. 1427, 1430 | The well-capitalised 10% line is the binding floor for 1996-2009 and the conservation-buffer floor of 10.5% after 2009; our training window (2005-2014) straddles the change and our test window (2015-2024) lies after it. | OR-Q4, OR-Q5, OR-Q6, OR-Q13. |
| OR-D2 | pp. 1427 | Their design is local (8% to 12%); the level of buffers far above the line, which F2 measures, is outside it. | OR-Q10, OR-Q11. |
