"""Fetches the citation counts of the papers the theory manuscript cites and writes citations.json.

Source: OpenAlex (api.openalex.org), no API key, for the records and counts;
Crossref (api.crossref.org) for the print date, volume, issue and pages of
journal versions, since OpenAlex dates are sometimes the online or a wrong
date. For each cited bib key the
record of the published version, where there is one, is `main`; `others` are OpenAlex records
of earlier or parallel versions of the same paper (working papers, SSRN,
RePEc), which OpenAlex counts separately; `later` is a record the table names
as a later version that was not read. Each record id was chosen by hand from
an OpenAlex title search on 28 October 2026 and checked against the authors.

The ABDC 2025 rating is looked up by ISSN in
papers/journal_rankings/ABDC-JQL-2025-v3-210926.xlsx.

OpenAlex counts are usually below Google Scholar's. Re-run to refresh; the
table of citations in wealth_garch_theory.tex must then be updated to match
(MS-10 of checks/verify_manuscript.py).

    python3 checks/fetch_citations.py
"""
import datetime
import json
import os
import time
import urllib.parse
import urllib.request

HERE = os.path.dirname(os.path.abspath(__file__))
THEORY = os.path.dirname(HERE)
ABDC = os.path.join(THEORY, "..", "..", "journal_rankings", "ABDC-JQL-2025-v3-210926.xlsx")

RECORDS = {
    "bayraktar2026": dict(main="W7138464927", others=["W7139144187", "W7171674439"]),
    "barberis2001": dict(main="W3121252329", others=["W2109848583", "W3023532572"]),
    "engle2018": dict(main="W3125940130", others=["W2168426853", "W3125943825"]),
    "li2023": dict(main="W3190992390", others=[]),
    "peura2006": dict(main="W3125331046", others=[]),
    "bolton2011": dict(main="W3021945653", others=["W2099323025", "W3121553494"]),
    "lokka2008": dict(main="W2078270141", others=[]),
    "blackperold1992": dict(main="W2068841402", others=[]),
    "daihuangkeppo2019": dict(main="W3123188656", others=[]),
    "milnewhalley2001": dict(main="W1788034067", others=[]),
    "berger2008": dict(main="W2170777567", others=["W3125767733"]),
    "constantinides1978": dict(main="W2147736135", others=[]),
    "decamps2011": dict(main="W2052461731", others=["W1549697278"]),
    "behn2019": dict(main="W2919604362", others=[]),
    "langmenno2026": dict(main="W7128914369", others=["W7131171981"]),
    "glover2023": dict(main="W4322721965", others=[]),
    "lambertini2014": dict(main="W2185057928", others=[]),
    # NBER WP 20155 was published in the AER under another title; the abstracts state the same
    # result (kurtosis over frequency as a sufficient statistic, over the same class of models).
    "alvarez2014": dict(main="W2343185847", others=["W1511179063", "W3121838129", "W3150502032", "W3125896809"]),
    "federico2026": dict(main="W7214928914", others=["W4412630239", "W7214492509"]),
    "bonaparte2011": dict(main="W2123714743", others=[]),
    # Read as FEDS 96-12 (1996); published in the JFI (1999) under a shorter title, not read.
    "calem1999": dict(main="W1974244766", others=["W1541486432", "W2187999494"]),
    "fries1997": dict(main="W2018818305", others=[]),
}

FIELDS = "id,display_name,publication_date,cited_by_count,doi,type,primary_location,authorships,biblio"


def get(ident):
    url = "https://api.openalex.org/works/" + urllib.parse.quote(ident, safe=":") + "?" + urllib.parse.urlencode({"select": FIELDS})
    for attempt in range(4):
        try:
            w = json.load(urllib.request.urlopen(url, timeout=30))
            break
        except Exception:
            if attempt == 3:
                raise
            time.sleep(5 * (attempt + 1))
    src = ((w.get("primary_location") or {}).get("source") or {})
    return dict(id=w["id"].rsplit("/", 1)[-1], title=w["display_name"], date=w["publication_date"],
                cites=w["cited_by_count"], doi=w.get("doi"), type=w.get("type"), venue=src.get("display_name"),
                issn=src.get("issn") or [], authors=[a["author"]["display_name"] for a in w.get("authorships", [])],
                biblio=w.get("biblio"))


def crossref(doi):
    """Print and online dates, volume, issue and pages registered with Crossref by the publisher."""
    if not doi or any(s in doi for s in ("arxiv", "ssrn", "/w2")):
        return None
    m = json.load(urllib.request.urlopen("https://api.crossref.org/works/" + doi.replace("https://doi.org/", ""), timeout=30))["message"]
    date = lambda f: (m.get(f) or {}).get("date-parts", [[None]])[0]
    return dict(journal=(m.get("container-title") or [None])[0], volume=m.get("volume"), issue=m.get("issue"),
                pages=m.get("page"), published_print=date("published-print"), published_online=date("published-online"))


def abdc_index(path):
    import openpyxl
    ws = openpyxl.load_workbook(path, read_only=True)["2025 JQL"]
    idx = {}
    for r in ws.iter_rows(min_row=9, values_only=True):
        if not r[1]:
            continue
        for i in (3, 4):
            if r[i]:
                idx[str(r[i]).strip()] = (str(r[1]).strip(), str(r[7]).strip())
    return idx


def main():
    idx = abdc_index(ABDC)
    out = dict(source="OpenAlex, api.openalex.org", retrieved=datetime.date.today().isoformat(),
               abdc_list=os.path.basename(ABDC), papers={})
    for key, spec in RECORDS.items():
        main_rec = get(spec["main"])
        others = [get(i) for i in spec["others"]]
        later = get(spec["later"]) if spec.get("later") else None
        hit = next((idx[i] for i in main_rec["issn"] if i in idx), None)
        out["papers"][key] = dict(main=main_rec, crossref=crossref(main_rec["doi"]), others=others, later=later,
                                  others_cites=sum(o["cites"] for o in others),
                                  abdc_2025=hit[1] if hit else None, abdc_title=hit[0] if hit else None)
        print(f"{key}: {main_rec['cites']} (+{out['papers'][key]['others_cites']}) {main_rec['venue']} "
              f"{main_rec['date']} ABDC {hit[1] if hit else '-'}", flush=True)
    json.dump(out, open(os.path.join(THEORY, "citations.json"), "w"), indent=1, ensure_ascii=False)
    print("written citations.json")


if __name__ == "__main__":
    main()
