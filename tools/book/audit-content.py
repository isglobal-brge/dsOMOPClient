"""Check generated book outputs and the local website's book links."""
from pathlib import Path
from html.parser import HTMLParser
from urllib.parse import unquote, urlsplit
import json, re
root = Path(__file__).resolve().parents[2]
chapters = sorted((root / 'vignettes').glob('book-*.Rmd'))
assert len(chapters) == 19
counts = {}
for p in chapters:
    text = p.read_text()
    assert '```{r' not in text, f'Uncomputed chunk: {p}'
    assert '#> NULL' not in text, f'Empty result displayed: {p}'
    diagnostics = text
    # dsBase 6.3.5 intentionally uses one local GLM iteration. Keep its
    # explained warnings visible, allowing only this exact message here.
    if p.name in ('book-15-labs.Rmd', 'book-16-longitudinal.Rmd'):
        for block in re.findall(r'<!-- expected-glm-warning:start -->(.*?)<!-- expected-glm-warning:end -->', text, re.S):
            cleaned = block.replace('#> Warning: glm.fit: algorithm did not converge', '')
            assert not re.search(r'#> (Error|Warning)', cleaned), f'Unexpected GLM diagnostic: {p}'
            diagnostics = diagnostics.replace(block, cleaned)
    if p.name == 'book-16-longitudinal.Rmd':
        for block in re.findall(r'<!-- expected-mixed-warning:start -->(.*?)<!-- expected-mixed-warning:end -->', text, re.S):
            cleaned = re.sub(r"#> Warning: the 'nobars' function has moved to the reformulas package\. Please update your\n#> imports, or ask an upstream package maintainer to do so\.\n", '', block)
            assert not re.search(r'#> (Error|Warning)', cleaned), f'Unexpected mixed-model diagnostic: {p}'
            diagnostics = diagnostics.replace(block, cleaned)
    assert not re.search(r'#> (Error|Warning)', diagnostics), f'Unexpected diagnostic: {p}'
    # Refusals return ordinary lists, not necessarily R errors. Only a named,
    # visibly marked refusal lesson may contain a failed aggregate response.
    unmarked = re.sub(r'<!-- expected-refusal:start -->.*?<!-- expected-refusal:end -->',
                      '', text, flags=re.S)
    assert not re.search(r'#>.*(All studies failed|Failed:|\$error.messages|Some studies failed)',
                         unmarked), f'Unintended aggregate refusal: {p}'
    assert 'author: "David Sarrat González"' in text
    assert '**What you will learn**' in text and '## Recap' in text and '<summary>Solution' in text
    # Public table outputs must use concept labels, never the fixture IDs.
    assert not re.search(r'^#>.*\b900\d{3}\b', text, re.M), f'Untranslated output: {p}'
    counts[p.name] = len(re.findall(r'^``` r$', text, re.M))
class Links(HTMLParser):
    def __init__(self): super().__init__(); self.links=[]; self.ids=set()
    def handle_starttag(self,tag,attrs):
        if tag=='a': self.links.extend(v for k,v in attrs if k=='href')
        self.ids.update(v for k,v in attrs if k=='id')
missing=[]
for p in [root/'docs/index.html', root/'docs/authors.html', *sorted((root/'docs/articles').glob('book-*.html'))]:
    parser=Links(); parser.feed(p.read_text())
    for link in parser.links:
        url=urlsplit(link)
        if url.scheme or url.netloc: continue
        target=(p.parent/unquote(url.path)).resolve() if url.path else p.resolve()
        if not target.exists(): missing.append({'page':str(p.relative_to(root)), 'href':link})
        elif url.fragment and target.suffix=='.html':
            target_parser=Links(); target_parser.feed(target.read_text())
            if unquote(url.fragment) not in target_parser.ids:
                missing.append({'page':str(p.relative_to(root)), 'href':link})
assert not missing, missing
report={'chapters':len(chapters),
        'executed_source_chunks':sum(len(re.findall(r'^```\{r ', p.read_text(), re.M))
                                     for p in (root/'vignettes').glob('book-*.Rmd.orig')),
        'rendered_r_code_blocks':sum(counts.values()),'code_blocks_by_chapter':counts,
        'missing_local_links':missing,'uncomputed_chunks':0,'untranslated_fixture_id_outputs':0}
(root/'tools/book/evidence/book3-fix2/content-audit.json').write_text(json.dumps(report,indent=2)+'\n')
print(json.dumps(report,indent=2))
