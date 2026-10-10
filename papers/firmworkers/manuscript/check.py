import os
import re
import subprocess
import unicodedata
import sys
import yaml

ROOT = os.path.dirname(os.path.abspath(__file__))
LEAN_DIR = os.path.join(ROOT, 'lean')
LEAN_BIN = os.environ.get('LEAN_BIN', '/opt/lean/lean-4.21.0-linux/bin')
LIT_DIR = os.path.join(ROOT, 'lit')
SOURCE_PDF = os.path.join(ROOT, 'inputs', 'firmworkers_model.pdf')
SOURCE_SHA = 'd79d5651207296d02661381f2f9322391c664de3d9c845cb5a5598bbad8d783e'
BIB = os.path.join(ROOT, 'refs.bib')
ALLOWED_AXIOMS = {'propext', 'Classical.choice', 'Quot.sound'}
LIGATURES = {'\ufb00': 'ff', '\ufb01': 'fi', '\ufb02': 'fl', '\ufb03': 'ffi', '\ufb04': 'ffl'}
QUOTE_ROW = re.compile(r'^\|\s*[A-Z]+-\d+\s*\|\s*(\d+)\s*\|\s*"(.+?)"\s*\|', re.M)
CHECKED = {'LEAN_PROVED', 'LEAN_WRITTEN'}
PLACEMENT = {'repaired_by': ('ILL_POSED',), 'affects': ('UNDERSPECIFIED',), 'waits_on': ('OPEN',), 'rows': ('OPEN',)}


def load():
    with open(os.path.join(ROOT, 'claims.yaml'), encoding='utf8') as fh:
        return yaml.safe_load(fh)


def lean_sources():
    files = {}
    for name in sorted(os.listdir(os.path.join(LEAN_DIR, 'IscLean'))):
        if name.endswith('.lean'):
            with open(os.path.join(LEAN_DIR, 'IscLean', name), encoding='utf8') as fh:
                files['IscLean.' + name[:-5]] = fh.read()
    return files


def declared_theorems(text):
    return re.findall(r"^theorem\s+([A-Za-z0-9_']+)", text, re.M)


def lean_declared(full_name, files):
    namespace, _, short = full_name.rpartition('.')
    return any(re.search(r'\bnamespace\s+' + re.escape(namespace) + r'\b', t) and short in declared_theorems(t)
               for t in files.values())


def lit_norm(s):
    s = unicodedata.normalize('NFKD', s)
    for k, v in LIGATURES.items():
        s = s.replace(k, v)
    s = re.sub(r'-\s*\n\s*', '', s).lower()
    s = re.sub(r'[^a-z0-9]+', ' ', s)
    return re.sub(r'\s+', ' ', s).strip()


def bib_keys():
    if not os.path.exists(BIB):
        return set()
    with open(BIB, encoding='utf8') as fh:
        return set(re.findall(r'@\w+\{([^,\s]+),', fh.read()))


def lit_records():
    records = {}
    if not os.path.isdir(LIT_DIR):
        return records
    for name in sorted(os.listdir(LIT_DIR)):
        keys = os.path.join(LIT_DIR, name, 'KEYS')
        if os.path.exists(keys):
            with open(keys, encoding='utf8') as fh:
                for k in fh.read().split():
                    records[k] = os.path.join(LIT_DIR, name)
    return records


def source_text(errors):
    import hashlib
    with open(SOURCE_PDF, 'rb') as fh:
        if hashlib.sha256(fh.read()).hexdigest() != SOURCE_SHA:
            errors.append('inputs/firmworkers_model.pdf does not match its pinned sha256')
    run = subprocess.run(['pdftotext', '-raw', SOURCE_PDF, '-'], capture_output=True, text=True)
    if run.returncode != 0:
        errors.append('pdftotext failed on inputs/firmworkers_model.pdf: ' + run.stderr[-200:])
        return ''
    return lit_norm(run.stdout)


def record_text(path, name):
    f = os.path.join(path, name)
    if not os.path.exists(f):
        return ''
    with open(f, encoding='utf8') as fh:
        return fh.read()


def lean_names(claim):
    v = claim.get('lean') or []
    return [v] if isinstance(v, str) else list(v)


def lean_module(claim):
    m = re.search(r'lean/(IscLean/\w+)\.lean::', claim.get('evidence', ''))
    return m.group(1).replace('/', '.') if m else None


def module_built(module):
    rel = module.replace('.', '/')
    src = os.path.join(LEAN_DIR, rel + '.lean')
    olean = os.path.join(LEAN_DIR, '.lake', 'build', 'lib', 'lean', rel + '.olean')
    return os.path.exists(olean) and os.path.getmtime(olean) >= os.path.getmtime(src)


def lean_axioms(module, names):
    probe = os.path.join(LEAN_DIR, 'AxiomProbe.lean')
    with open(probe, 'w', encoding='utf8') as fh:
        fh.write(f'import {module}\n' + ''.join(f'#print axioms {n}\n' for n in names))
    env = dict(os.environ, PATH=LEAN_BIN + os.pathsep + os.environ.get('PATH', ''))
    run = subprocess.run(['lake', 'env', 'lean', 'AxiomProbe.lean'], cwd=LEAN_DIR, env=env,
                         capture_output=True, text=True)
    os.remove(probe)
    if run.returncode != 0:
        return None, run.stdout + run.stderr
    axioms = {}
    for n in names:
        m = re.search(rf"'{re.escape(n)}' (does not depend on any axioms|depends on axioms: \[([^\]]*)\])", run.stdout)
        if m:
            axioms[n] = set() if m.group(2) is None else {a.strip() for a in m.group(2).split(',')}
    return axioms, run.stdout


def audit_model_lean(files, written, errors, notes):
    for module, text in files.items():
        declared = declared_theorems(text)
        if module == 'IscLean.IscLean' or not declared:
            continue
        if not module_built(module):
            errors.append(f'{module}: not compiled, cannot audit')
            continue
        names = [f'Isc.{d}' for d in declared]
        axioms, log = lean_axioms(module, names)
        if axioms is None:
            errors.append(f'lean audit failed for {module}:\n' + log[-2000:])
            continue
        if len(axioms) != len(names):
            errors.append(f'{module}: audited {len(axioms)} of {len(names)} declared theorems')
        for n, used in axioms.items():
            bad = used - ALLOWED_AXIOMS - ({'sorryAx'} if n in written else set())
            if bad:
                errors.append(f'{module}: {n} depends on {", ".join(sorted(bad))}')
        notes.append(f'{module}: {len(axioms)} of {len(names)} theorems audited')


def check_controls(files, errors):
    for module, text in files.items():
        declared = declared_theorems(text)
        if declared and not any(d.startswith('control_') for d in declared):
            errors.append(f'{module}: no control_* theorem')


def load_map():
    with open(os.path.join(ROOT, 'measurement_map.yaml'), encoding='utf8') as fh:
        return yaml.safe_load(fh)


def check_map(data, mmap, errors, notes):
    model = {c['id']: c for c in data['model']}
    lit = {c['id'] for c in data['literature']}
    rows = {r['id']: r for r in mmap['rows']}
    sstat = mmap['support_status']
    used = {}
    for c in data['model']:
        for x in c.get('symbols', []) or []:
            if x not in rows:
                errors.append(f'{c["id"]}: symbol {x} has no measurement-map row')
            used.setdefault(x, []).append(c['id'])
        if not c.get('symbols'):
            errors.append(f'{c["id"]}: no symbols declared')
    ids = [r['id'] for r in mmap['rows']]
    for dup in sorted({i for i in ids if ids.count(i) > 1}):
        errors.append(f'duplicate map id {dup}')
    for r in mmap['rows']:
        rid = r['id']
        if not re.fullmatch(r'X\d+', rid):
            errors.append(f'{rid}: map id must match X<n>')
        if r.get('support') not in sstat:
            errors.append(f'{rid}: unknown support {r.get("support")}')
        if r.get('role') not in ('input', 'output'):
            errors.append(f'{rid}: role must be input or output')
        for field in ('symbol', 'meaning'):
            if not str(r.get(field, '')).strip():
                errors.append(f'{rid}: empty {field}')
        if r.get('support') == 'NONE' and str(r.get('referent', '')).strip():
            errors.append(f'{rid}: support NONE but a referent is given')
        if r.get('support') in ('POSIT', 'LITERATURE') and not str(r.get('referent', '')).strip():
            errors.append(f'{rid}: support {r.get("support")} without a referent')
        if r.get('support') == 'LITERATURE':
            refs = r.get('literature', []) or []
            if not refs or any(l not in lit for l in refs):
                errors.append(f'{rid}: LITERATURE support needs existing L-ids')
        if r.get('role') == 'output':
            prod = r.get('produced_by', []) or []
            if not prod:
                errors.append(f'{rid}: output without produced_by')
            for m in prod:
                if m not in model:
                    errors.append(f'{rid}: produced_by {m} missing')
                elif rid not in (model[m].get('symbols') or []):
                    errors.append(f'{rid}: produced_by {m} but {m} does not declare {rid}')
        for m in re.findall(r'\bM\d+\b', str(r.get('direction', ''))):
            if m not in model:
                errors.append(f'{rid}: direction cites missing {m}')
            elif model[m]['status'] not in CHECKED:
                errors.append(f'{rid}: direction cites {m} with status {model[m]["status"]}')
        if rid not in used:
            errors.append(f'{rid}: not used by any model claim')
    return rows


def check_derived_claims(data, rows, errors, notes):
    model = {c['id']: c for c in data['model']}
    lit = {c['id']: c for c in data['literature']}
    for section, prefix in (('headline', 'H'), ('concluding', 'C')):
        for c in data.get(section, []) or []:
            cid = c.get('id', '?')
            if not re.fullmatch(prefix + r'\d+', cid):
                errors.append(f'{cid}: {section} id must match {prefix}<n>')
            if not str(c.get('claim', '')).strip():
                errors.append(f'{cid}: empty claim')
            rests = c.get('rests_on', []) or []
            if not rests:
                errors.append(f'{cid}: rests_on is empty')
            for r in rests:
                if r.startswith('M'):
                    if r not in model:
                        errors.append(f'{cid}: rests on missing {r}')
                    elif model[r]['status'] not in CHECKED:
                        errors.append(f'{cid}: rests on {r} with status {model[r]["status"]}')
                elif r.startswith('L'):
                    if r not in lit:
                        errors.append(f'{cid}: rests on missing {r}')
                elif r.startswith('X'):
                    if r not in rows:
                        errors.append(f'{cid}: rests on missing {r}')
                    elif rows[r]['support'] == 'POSIT':
                        notes.append(f'{cid}: rests on POSIT row {r}')
                else:
                    errors.append(f'{cid}: may rest only on M, L or X ids, not {r}')


def literature_uses(data, rows):
    uses = {}
    for c in data['model']:
        for lid in re.findall(r'\bL\d+\b', str(c.get('evidence', '')) + ' ' + str(c.get('assumptions', ''))):
            uses.setdefault(c['id'], []).append(lid)
    for section in ('headline', 'concluding'):
        for c in data.get(section, []) or []:
            for r in c.get('rests_on', []) or []:
                if r.startswith('L'):
                    uses.setdefault(c['id'], []).append(r)
    for rid, r in rows.items():
        for lid in r.get('literature', []) or []:
            uses.setdefault(rid, []).append(lid)
    return uses


def reference_citations(data, rows):
    lit_ref = {c['id']: c.get('ref') for c in data['literature']}
    cited = {r['key']: [] for r in data.get('references', []) or []}
    def add(lid, who):
        k = lit_ref.get(lid)
        if k in cited and who not in cited[k]:
            cited[k].append(who)
    for c in data['model']:
        for lid in re.findall(r'\bL\d+\b', str(c.get('evidence', '')) + ' ' + str(c.get('assumptions', ''))):
            add(lid, c['id'])
    for section in ('headline', 'concluding'):
        for c in data.get(section, []) or []:
            for r in c.get('rests_on', []) or []:
                if r.startswith('L'):
                    add(r, c['id'])
    for rid, r in rows.items():
        for lid in r.get('literature', []) or []:
            add(lid, rid)
    return cited


def check_references(data, rows, errors, notes):
    refs = {r['key']: r for r in data.get('references', []) or []}
    records = lit_records()
    verbatim_refs = {c.get('ref') for c in data['literature'] if c.get('status') == 'VERBATIM'}
    for c in data['literature']:
        if c.get('ref') not in refs:
            errors.append(f'{c["id"]}: ref {c.get("ref")!r} has no references entry')
    used = {c.get('ref') for c in data['literature']}
    cited = reference_citations(data, rows)
    for k, r in refs.items():
        if k not in used:
            errors.append(f'{k}: reference not linked to any literature claim')
        if r.get('math_content') not in ('yes', 'no', 'to confirm'):
            errors.append(f'{k}: math_content must be yes, no or to confirm')
        if r.get('lean') not in ('NOT_STARTED', 'LEAN_PARTIAL', 'LEAN_PROVED'):
            errors.append(f'{k}: lean status {r.get("lean")} not allowed, every cited paper needs Lean')
        if r.get('lean') in ('LEAN_PARTIAL', 'LEAN_PROVED'):
            rec = records.get(k)
            lean_rel = record_text(rec, 'LEAN').strip() if rec else ''
            if not rec:
                errors.append(f'{k}: lean {r.get("lean")} but no lit record lists the key')
            elif not lean_rel or not os.path.exists(os.path.join(ROOT, lean_rel)):
                errors.append(f'{k}: lit record has no LEAN file naming an existing Lean source')
        if k in verbatim_refs and r.get('lean') not in ('LEAN_PARTIAL', 'LEAN_PROVED'):
            errors.append(f'{k}: read in full but lean is {r.get("lean")}')
        for field in ('reason', 'impact_if_removed'):
            if not str(r.get(field, '')).strip():
                errors.append(f'{k}: empty {field}')
        none_claim = str(r.get('impact_if_removed', '')).startswith('No current claim rests on this paper')
        if not cited[k] and not none_claim:
            errors.append(f'{k}: cited by nothing, impact must begin "No current claim rests on this paper"')
        if cited[k] and none_claim:
            errors.append(f'{k}: cited by {", ".join(cited[k])}, impact cannot say no claim rests on it')
    return cited


def cited_map_rows(data):
    out = []
    for section in ('headline', 'concluding'):
        for c in data.get(section, []) or []:
            for r in c.get('rests_on', []) or []:
                if r.startswith('X') and r not in out:
                    out.append(r)
    return out


def main():
    use_lean = '--lean' in sys.argv
    data = load()
    errors, notes = [], []
    mstat, lstat = data['model_status'], data['literature_status']
    ids = [c['id'] for c in data['model']] + [c['id'] for c in data['literature']]
    for dup in sorted({i for i in ids if ids.count(i) > 1}):
        errors.append(f'duplicate id {dup}')
    by_id = {c['id']: c for c in data['model']}
    lean_text = lean_sources()
    check_controls(lean_text, errors)
    bib, records = bib_keys(), lit_records()
    source = source_text(errors)
    lean_claims = [c for c in data['model'] if c['status'] in ('LEAN_PROVED', 'LEAN_WRITTEN') or lean_names(c)]
    verdict, unbuilt = None, set()
    if use_lean and lean_claims:
        verdict = {}
        modules = sorted({lean_module(c) for c in lean_claims if lean_module(c)})
        for module in modules:
            if not module_built(module):
                unbuilt.add(module)
                notes.append(f'{module}: not compiled, its claims not re-verified')
                continue
            names = [n for c in lean_claims if lean_module(c) == module for n in lean_names(c)]
            result, log = lean_axioms(module, names)
            if result is None:
                errors.append(f'lean axiom probe failed for {module}:\n' + log[-2000:])
            else:
                verdict.update({n: 'sorryAx' not in a for n, a in result.items()})
        written = {n for c in data['model'] if c['status'] == 'LEAN_WRITTEN' for n in lean_names(c)}
        audit_model_lean(lean_text, written, errors, notes)
    for c in data['model']:
        cid, st = c['id'], c['status']
        if not re.fullmatch(r'M\d+', cid):
            errors.append(f'{cid}: model id must match M<n>')
        if st not in mstat:
            errors.append(f'{cid}: unknown status {st}')
        for field in ('anchor', 'claim', 'evidence'):
            if not str(c.get(field, '')).strip():
                errors.append(f'{cid}: empty {field}')
        for q in re.findall(r'"([^"]+)"', str(c.get('anchor', ''))):
            if source and lit_norm(q) not in source:
                errors.append(f'{cid}: anchor quote not found in inputs/firmworkers_model.pdf: {q[:60]}')
        names = lean_names(c)
        if st in ('LEAN_PROVED', 'LEAN_WRITTEN') and not names:
            errors.append(f'{cid}: {st} without a lean theorem')
        if names and verdict is None:
            notes.append(f'{cid}: {st} lean not re-verified (run with --lean)')
        for name in names:
            if not lean_declared(name, lean_text):
                errors.append(f'{cid}: lean theorem {name!r} not declared')
            elif verdict is None:
                continue
            elif lean_module(c) in unbuilt:
                if st != 'LEAN_WRITTEN':
                    errors.append(f'{cid}: {st} but {lean_module(c)} is not compiled')
            elif st == 'LEAN_WRITTEN':
                if verdict.get(name) is None:
                    errors.append(f'{cid}: no axiom report for {name}')
                elif verdict[name]:
                    errors.append(f'{cid}: compiles sorry-free, status must be LEAN_PROVED')
            elif verdict.get(name) is not True:
                errors.append(f'{cid}: {name} absent from axiom report or depends on sorryAx')
        for field, owner in PLACEMENT.items():
            if c.get(field) and st not in owner:
                errors.append(f'{cid}: field {field} is only for status {", ".join(owner)}')
        if st in ('ILL_POSED', 'REFUTED') and not names:
            errors.append(f'{cid}: {st} needs a Lean counterexample')
        if st == 'ILL_POSED':
            rep = c.get('repaired_by') or []
            if not rep:
                errors.append(f'{cid}: ILL_POSED without repaired_by')
            for r in rep:
                if r not in by_id:
                    errors.append(f'{cid}: repaired_by missing {r}')
                elif by_id[r]['status'] not in CHECKED:
                    errors.append(f'{cid}: repair {r} has status {by_id[r]["status"]}')
        if st == 'UNDERSPECIFIED':
            aff = c.get('affects') or []
            if not aff:
                errors.append(f'{cid}: UNDERSPECIFIED without affects')
            for r in aff:
                if r not in by_id:
                    errors.append(f'{cid}: affects missing {r}')
        if st == 'OPEN':
            if not str(c.get('waits_on', '')).strip():
                errors.append(f'{cid}: OPEN without waits_on')
            for r in c.get('rows') or []:
                if r not in by_id:
                    errors.append(f'{cid}: rows cites missing {r}')
        if st == 'REFUTED':
            refs = re.findall(r'\bM\d+\b', c['evidence'])
            if not refs:
                errors.append(f'{cid}: REFUTED without a refuting claim')
            for r in refs:
                if r not in by_id:
                    errors.append(f'{cid}: refers to missing {r}')
                elif by_id[r]['status'] not in CHECKED:
                    errors.append(f'{cid}: refuting claim {r} has status {by_id[r]["status"]}')
                elif by_id[r]['status'] == 'LEAN_WRITTEN':
                    notes.append(f'{cid}: refuted via {r}, which is LEAN_WRITTEN only')
    for c in data['literature']:
        cid, st = c['id'], c['status']
        if not re.fullmatch(r'L\d+', cid):
            errors.append(f'{cid}: literature id must match L<n>')
        if st not in lstat:
            errors.append(f'{cid}: unknown status {st}')
        for field in ('source', 'claim', 'evidence_seen'):
            if not str(c.get(field, '')).strip():
                errors.append(f'{cid}: empty {field}')
        quote, page = str(c.get('quote', '')).strip(), str(c.get('page', '')).strip()
        if st == 'VERBATIM':
            ref = c.get('ref')
            if not quote or not page:
                errors.append(f'{cid}: VERBATIM needs quote and page')
            elif ref not in bib:
                errors.append(f'{cid}: VERBATIM but {ref} is not in refs.bib')
            elif ref not in records:
                errors.append(f'{cid}: VERBATIM but no lit record lists {ref} in KEYS')
            else:
                claims_md = record_text(records[ref], 'CLAIMS.md')
                rows_on_page = [q for p, q in QUOTE_ROW.findall(claims_md) if p == page]
                if not any(lit_norm(quote) in lit_norm(q) for q in rows_on_page):
                    errors.append(f'{cid}: quote not found on p.{page} of {os.path.relpath(records[ref], ROOT)}/CLAIMS.md')
        elif quote:
            errors.append(f'{cid}: quote present but status is {st}')
        if 'version_read' in c and (st != 'VERBATIM' or not str(c['version_read']).strip()):
            errors.append(f'{cid}: version_read must be non-empty and only on a VERBATIM row')
    mmap = load_map()
    rows = check_map(data, mmap, errors, notes)
    lit_status = {c['id']: c['status'] for c in data['literature']}
    for who, lids in literature_uses(data, rows).items():
        for lid in lids:
            if lid in lit_status and lit_status[lid] != 'VERBATIM':
                errors.append(f'{who}: rests on {lid}, which is {lit_status[lid]}; a paper a claim rests on must be read in full')
    check_derived_claims(data, rows, errors, notes)
    check_references(data, rows, errors, notes)
    counts = {}
    for c in data['model'] + data['literature'] + (data.get('headline') or []) + (data.get('concluding') or []):
        if 'status' not in c:
            continue
        counts[c['status']] = counts.get(c['status'], 0) + 1
    print('status counts:', ', '.join(f'{k}={v}' for k, v in sorted(counts.items())))
    supp = {}
    for r in mmap['rows']:
        supp[r['support']] = supp.get(r['support'], 0) + 1
    print('map support counts:', ', '.join(f'{k}={v}' for k, v in sorted(supp.items())))
    for n in notes:
        print('NOTE', n)
    for e in errors:
        print('ERROR', e)
    print('RESULT', 'PASS' if not errors else f'FAIL ({len(errors)})')
    sys.exit(1 if errors else 0)


if __name__ == '__main__':
    main()
