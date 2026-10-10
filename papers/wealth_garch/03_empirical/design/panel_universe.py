"""
The panel's country universe: every sovereign area the source reports for
the indicator, no pre-screen.

REPLACES FINAL_77, the hardcoded 77-area list previously duplicated in
expanding_mean_panel.py and segmented_panel.py. FINAL_77 excluded roughly
half of all areas reporting the capital ratio -- including five of the
seven largest advanced economies, several with MORE usable history than
included countries -- and no rationale for it survives anywhere in this
project or its archive. Two hypotheses for its origin (a joint
FSKRC_PT/FSERE_PT data requirement; an advanced-vs-emerging classification)
were tested directly against the source and the IMF's own classification
and both were rejected. It is not being reconstructed. It is replaced.

THE RULE, stated once, here, and imported everywhere a country universe is
needed: every REF_AREA the source returns for the indicator that is also a
real sovereign or dependent territory (Section "Sovereignty filter" below).
No history-length threshold is applied at this stage. Feasibility --
whether an area has enough history to compute an estimate -- is a property
of the ESTIMATOR, not of the universe, and belongs in each script's own
attrition report, exactly as expanding_mean_panel.py's BURNIN + 2*MIN_OBS
check already does per-country. Folding a feasibility screen into universe
construction is what made FINAL_77 unauditable in the first place: it
collapsed two different questions -- "does this area exist for this
purpose" and "does this area have enough data" -- into one silent list.

Sovereignty filter: ISO 3166-1 alpha-2 codes, which is what REF_AREA codes
from this source are, PLUS one documented, named exception. XK (Kosovo) is
not part of the formal ISO 3166-1 standard but is a widely used exceptional
reservation (used by, among others, the EU, SWIFT, and this project's own
existing panel, where XK is already included). It is kept by explicit
exception, not because it happens to look like a country code. Any other
REF_AREA absent from the ISO list -- for example "5Y", seen in this
source's FSI response -- is a regional or income-group aggregate, not a
sovereign or dependent territory, and is excluded on that basis, reported
by name so the exclusion is auditable rather than silent.

This module fetches nothing and computes nothing. It only defines the
universe rule and applies it to a set of REF_AREA codes handed to it.
"""

# ISO 3166-1 alpha-2, current officially assigned codes (the complete set).
ISO_3166_1_ALPHA_2 = {
'AD','AE','AF','AG','AI','AL','AM','AO','AQ','AR','AS','AT','AU','AW','AX','AZ',
'BA','BB','BD','BE','BF','BG','BH','BI','BJ','BL','BM','BN','BO','BQ','BR','BS',
'BT','BV','BW','BY','BZ','CA','CC','CD','CF','CG','CH','CI','CK','CL','CM','CN',
'CO','CR','CU','CV','CW','CX','CY','CZ','DE','DJ','DK','DM','DO','DZ','EC','EE',
'EG','EH','ER','ES','ET','FI','FJ','FK','FM','FO','FR','GA','GB','GD','GE','GF',
'GG','GH','GI','GL','GM','GN','GP','GQ','GR','GS','GT','GU','GW','GY','HK','HM',
'HN','HR','HT','HU','ID','IE','IL','IM','IN','IO','IQ','IR','IS','IT','JE','JM',
'JO','JP','KE','KG','KH','KI','KM','KN','KP','KR','KW','KY','KZ','LA','LB','LC',
'LI','LK','LR','LS','LT','LU','LV','LY','MA','MC','MD','ME','MF','MG','MH','MK',
'ML','MM','MN','MO','MP','MQ','MR','MS','MT','MU','MV','MW','MX','MY','MZ','NA',
'NC','NE','NF','NG','NI','NL','NO','NP','NR','NU','NZ','OM','PA','PE','PF','PG',
'PH','PK','PL','PM','PN','PR','PS','PT','PW','PY','QA','RE','RO','RS','RU','RW',
'SA','SB','SC','SD','SE','SG','SH','SI','SJ','SK','SL','SM','SN','SO','SR','SS',
'ST','SV','SX','SY','SZ','TC','TD','TF','TG','TH','TJ','TK','TL','TM','TN','TO',
'TR','TT','TV','TW','TZ','UA','UG','UM','US','UY','UZ','VA','VC','VE','VG','VI',
'VN','VU','WF','WS','YE','YT','ZA','ZM','ZW',
}

# Documented, named exceptions to the formal ISO list, kept because this
# project's existing panel already relies on them and dropping them
# silently would be its own undocumented exclusion.
NAMED_EXCEPTIONS = {'XK'}

SOVEREIGN_CODES = ISO_3166_1_ALPHA_2 | NAMED_EXCEPTIONS


def sovereign_universe(available_areas):
    """
    available_areas: iterable of REF_AREA codes the source reports for
    an indicator (e.g. the keys of fetch_full("FSKRC_PT")).

    Returns (universe, excluded) where universe is the sorted list of
    codes to include, and excluded is a sorted list of (code, reason)
    for everything dropped -- so every exclusion is visible.
    """
    available = set(available_areas)
    universe = sorted(available & SOVEREIGN_CODES)
    excluded = sorted(
        (code, "not a sovereign/dependent-territory ISO 3166-1 code "
               "(regional or income-group aggregate)")
        for code in available - SOVEREIGN_CODES
    )
    return universe, excluded


if __name__ == "__main__":
    # Self-test against the known result: applying this rule to the 152
    # areas found for FSKRC_PT on 11 Aug 2026 should exclude exactly one
    # code, "5Y", and nothing else.
    fetched_11aug = {
        'AE','AL','AM','AO','AR','AU','BA','BN','BO','BR','BT','BW','BY','CA','CL','CO',
        'CR','CZ','DJ','EC','EE','FI','FJ','GE','GH','GM','GT','HK','HN','HR','HU','ID',
        'KE','KG','KH','KR','LK','LS','LT','LV','MD','ME','MK','MO','MT','MU','MV','MX',
        'MY','NA','NG','NI','NL','NO','PA','PE','PG','PH','PL','PS','PY','RO','RW','SA',
        'SB','SK','SZ','TH','TJ','TO','TR','TZ','UG','US','XK','ZA','ZM',
        'SV','UA','PK','IL','PT','TT','KZ','GR','SM','CH','CY','AT','SI','BE','RU','IE',
        'IN','ES','UZ','LU','GB','DK','CM','GQ','CG','GA','TD','CF','SE','FR','GN','FM',
        'SC','MG','SG','IS','MZ','VC','KN','AI','DM','LC','MS','GD','5Y','JO','IT','AG',
        'IQ','MW','WS','MN','BG','NP','BD','DO','CN','VU','KW','BZ','JP','SO','CD','BB',
        'KM','CW','BI','VN','LB','UY','ET','MA','DZ','AZ','DE',
    }
    universe, excluded = sovereign_universe(fetched_11aug)
    assert len(fetched_11aug) == 152, len(fetched_11aug)
    assert excluded == [('5Y', "not a sovereign/dependent-territory ISO 3166-1 "
                                "code (regional or income-group aggregate)")], excluded
    assert len(universe) == 151, len(universe)
    assert 'XK' in universe
    assert 'FR' in universe and 'GB' in universe and 'DE' in universe
    print(f"self-test OK: {len(universe)} sovereign areas, "
          f"{len(excluded)} excluded ({[c for c, _ in excluded]})")
