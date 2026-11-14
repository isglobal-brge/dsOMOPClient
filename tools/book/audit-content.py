"""Check generated book outputs and the local website's book links."""
from pathlib import Path
from html.parser import HTMLParser
from urllib.parse import unquote, urlsplit
import json, re
root = Path(__file__).resolve().parents[2]
chapters = sorted((root / 'vignettes').glob('book-*.Rmd'))
assert len(chapters) == 10
counts = {}
for p in chapters:
    text = p.read_text()
    assert '```{r' not in text, f'Uncomputed chunk: {p}'
    assert '#> NULL' not in text, f'Empty result displayed: {p}'
    assert not re.search(r'#> (Error|Warning)', text), f'Unexpected diagnostic: {p}'
    assert '**What you will learn**' in text and '## Recap' in text and '<summary>Solution' in text
    # Public table outputs must use concept labels, never the fixture IDs.
    assert not re.search(r'^#>.*\b900\d{3}\b', text, re.M), f'Untranslated output: {p}'
    counts[p.name] = len(re.findall(r'^``` r$', text, re.M))
class Links(HTMLParser):
    def __init__(self): super().__init__(); self.links=[]
    def handle_starttag(self,tag,attrs):
        if tag=='a': self.links.extend(v for k,v in attrs if k=='href')
missing=[]
for p in [root/'docs/index.html', *sorted((root/'docs/articles').glob('book-*.html'))]:
    parser=Links(); parser.feed(p.read_text())
    for link in parser.links:
        url=urlsplit(link)
        if url.scheme or url.netloc or not url.path: continue
        target=(p.parent/unquote(url.path)).resolve()
        if not target.exists(): missing.append({'page':str(p.relative_to(root)), 'href':link})
assert not missing, missing
report={'chapters':len(chapters),'executed_visible_chunks':sum(counts.values()),'chunks_by_chapter':counts,
        'missing_local_links':missing,'uncomputed_chunks':0,'untranslated_fixture_id_outputs':0}
(root/'tools/book/evidence/content-audit.json').write_text(json.dumps(report,indent=2)+'\n')
print(json.dumps(report,indent=2))
