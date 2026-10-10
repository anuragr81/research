import os
import re
import subprocess
import sys
import yaml

ROOT = os.path.dirname(os.path.abspath(__file__))
LEAN_DIR = os.path.join(ROOT, 'lean')
LEAN_BIN = os.environ.get('LEAN_BIN', '/opt/lean/lean-4.21.0-linux/bin')
SYMPY_SCRIPT = os.path.join(ROOT, 'sympy', 'micro_checks.py')
SOURCES_DIR = os.path.join(ROOT, 'sources')
CHECKED = {'LEAN_PROVED', 'LEAN_WRITTEN', 'SYMPY'}


def load():
    with open(os.path.join(ROOT, 'claims.yaml'), encoding='utf8') as fh:
        return yaml.safe_load(fh)


def lean_sources():
    text = ''
    for dirpath, _, files in os.walk(os.path.join(LEAN_DIR, 'IscLean')):
        for name in files:
            if name.endswith('.lean'):
                with open(os.path.join(dirpath, name), encoding='utf8') as fh:
                    text += fh.read() + '\n'
    return text


def lean_declared(full_name, text):
    namespace, _, short = full_name.rpartition('.')
    return bool(re.search(r'\bnamespace\s+' + re.escape(namespace) + r'\b', text)) and bool(
        re.search(r'\btheorem\s+' + re.escape(short) + r'\b', text))


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
    verdict = {}
    for block in re.split(r"(?=^')", run.stdout, flags=re.M):
        m = re.match(r"'([^']+)'", block)
        if m:
            verdict[m.group(1)] = 'sorryAx' not in block
    return verdict, run.stdout


def sympy_results():
    run = subprocess.run([sys.executable, SYMPY_SCRIPT], capture_output=True, text=True)
    return dict(re.findall(r'^(M\d+) (PASS|FAIL)$', run.stdout, flags=re.M))


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
        if r.get('lean') not in ('NOT_STARTED', 'LEAN_PARTIAL', 'LEAN_PROVED', 'NOT_APPLICABLE'):
            errors.append(f'{k}: unknown lean status {r.get("lean")}')
        if r.get('math_content') == 'no' and r.get('lean') != 'NOT_APPLICABLE':
            errors.append(f'{k}: no mathematical content, lean must be NOT_APPLICABLE')
        if r.get('math_content') != 'no' and r.get('lean') == 'NOT_APPLICABLE':
            errors.append(f'{k}: mathematical content possible, lean cannot be NOT_APPLICABLE')
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


def normalise(s):
    return re.sub(r'\s+', ' ', s).strip().lower()


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
    sympy = sympy_results()
    lean_claims = [c for c in data['model'] if c['status'] in ('LEAN_PROVED', 'LEAN_WRITTEN')]
    verdict, unbuilt = None, set()
    if use_lean and lean_claims:
        verdict = {}
        modules = sorted({lean_module(c) for c in lean_claims if lean_module(c)})
        for module in modules:
            if not module_built(module):
                unbuilt.add(module)
                notes.append(f'{module}: not compiled, its claims not re-verified')
                continue
            names = [c['lean'] for c in lean_claims if lean_module(c) == module]
            result, log = lean_axioms(module, names)
            if result is None:
                errors.append(f'lean axiom probe failed for {module}:\n' + log[-2000:])
            else:
                verdict.update(result)
    for c in data['model']:
        cid, st = c['id'], c['status']
        if not re.fullmatch(r'M\d+', cid):
            errors.append(f'{cid}: model id must match M<n>')
        if st not in mstat:
            errors.append(f'{cid}: unknown status {st}')
        for field in ('anchor', 'claim', 'evidence'):
            if not str(c.get(field, '')).strip():
                errors.append(f'{cid}: empty {field}')
        if st in ('LEAN_PROVED', 'LEAN_WRITTEN'):
            name = c.get('lean', '')
            if not name or not lean_declared(name, lean_text):
                errors.append(f'{cid}: lean theorem {name!r} not declared')
            elif verdict is not None and lean_module(c) in unbuilt:
                if st == 'LEAN_PROVED':
                    errors.append(f'{cid}: LEAN_PROVED but {lean_module(c)} is not compiled')
            elif verdict is not None:
                ok = verdict.get(name)
                if ok is None:
                    errors.append(f'{cid}: no axiom report for {name}')
                elif ok and st == 'LEAN_WRITTEN':
                    errors.append(f'{cid}: compiles sorry-free, status must be LEAN_PROVED')
                elif not ok and st == 'LEAN_PROVED':
                    errors.append(f'{cid}: depends on sorryAx, status cannot be LEAN_PROVED')
            elif st == 'LEAN_PROVED':
                notes.append(f'{cid}: LEAN_PROVED not re-verified (run with --lean)')
        if 'micro_checks.py::' in c.get('evidence', ''):
            if sympy.get(cid) != 'PASS':
                errors.append(f'{cid}: sympy check {sympy.get(cid, "missing")}')
        elif st == 'SYMPY':
            errors.append(f'{cid}: SYMPY status without a sympy reference')
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
            path = os.path.join(SOURCES_DIR, f'{cid}.txt')
            if not quote or not page:
                errors.append(f'{cid}: VERBATIM needs quote and page')
            elif not os.path.exists(path):
                errors.append(f'{cid}: VERBATIM but sources/{cid}.txt absent')
            else:
                with open(path, encoding='utf8') as fh:
                    if normalise(quote) not in normalise(fh.read()):
                        errors.append(f'{cid}: quote not found in sources/{cid}.txt')
        elif quote:
            errors.append(f'{cid}: quote present but status is {st}')
    mmap = load_map()
    rows = check_map(data, mmap, errors, notes)
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
