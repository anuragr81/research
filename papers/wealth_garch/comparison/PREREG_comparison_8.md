# Pre-registration: the testing thread's comparison, eighth file

Written and committed 20 October 2026, after one fetch failed and before
any other market data is fetched. It amends PREREG_comparison_7.md in one
item.

## What was seen before this file

The SEC's company ticker list (sec.gov/files/company_tickers.json) refused
the request with status 403: the SEC requires a contact e-mail address in
the request, and the author's address is not to be sent to outside
services. Nothing else was fetched for measure (B). For measure (A), the
two stress-test reports were fetched and recorded (lit/dfast_2013,
lit/dfast_2014); 11 of their firms are top holding companies of 16 banks
of sample B, matched on RSSDHCR as fixed.

## Amendment

Tickers for measure (B) are matched by name between the NIC legal name and
the security names of the Nasdaq Trader symbol directory
(nasdaqtrader.com, files nasdaqlisted.txt and otherlisted.txt, which
together list the securities on Nasdaq, NYSE and the other US exchanges),
pinned by sha256, each match listed and checked by hand in the results.
Everything else in PREREG_comparison_7.md stands.
