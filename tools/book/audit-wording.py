"""Scan the complete rendered site, preserving explicitly exempt function references."""
from pathlib import Path
from html import unescape
from html.parser import HTMLParser
import json, re
root = Path(__file__).resolve().parents[2]
pattern = re.compile(r'APHRC|HDSS|colleague|Nairobi|Kenya|Ethiopia|(?<![A-Za-z0-9_])v[12](?![A-Za-z0-9_])|language 2|plan version|legacy|upgrad(?:e|es|ed|ing)|migration|recorded output|this revision|acceptance criteria|Astra|Claude|Fable|Codex', re.I)
class Text(HTMLParser):
    def __init__(self): super().__init__(); self.text=[]; self.skip=0
    def handle_starttag(self, tag, attrs):
        if tag in ('script','style'): self.skip += 1
    def handle_endtag(self, tag):
        if tag in ('script','style'): self.skip -= 1
    def handle_data(self, data):
        if not self.skip: self.text.append(data)
hits=[]; reference=[]; files=0; raw=[]
for path in sorted((root/'docs').rglob('*')):
    if path.suffix not in ('.html','.md') and path.name != 'llms.txt': continue
    files += 1
    source = path.read_text()
    for i,line in enumerate(source.splitlines(),1):
        if pattern.search(line): raw.append({'file':str(path.relative_to(root)), 'line':i, 'terms':sorted(set(pattern.findall(line)))})
    if path.suffix == '.html':
        parser=Text();parser.feed(source);source=''.join(parser.text)
    source=unescape(source)
    source=re.sub(r'upgrade\s*=\s*"never"', '', source)
    matches=sorted(set(pattern.findall(source)))
    if matches:
        row={'file':str(path.relative_to(root)), 'terms':matches}
        (reference if path.parent.name == 'reference' else hits).append(row)
    if path.parent.name=='articles' and path.name.startswith(('book-','v0')):
        assert not re.search(r'ds[.]omop[.]plan(?:[.(]|\b)|actions/runs|v01_getting_started|v05_multi_server',source),path
report={'files_scanned':files,'reader_site_hits':hits,
        'preserved_exported_function_reference_hits':reference,
        'raw_grep_hits':raw}
out=root/'tools/book/evidence/book4-fix0/wording-audit.json'
out.parent.mkdir(parents=True,exist_ok=True);out.write_text(json.dumps(report,indent=2)+'\n')
print(json.dumps({'files_scanned':files,'reader_site_hits':hits,'preserved_reference_files':len(reference),'raw_matching_lines':len(raw)},indent=2))
assert not hits, hits
