"""Source-only book structure, exercise and navigation checks."""
from pathlib import Path
import json, re
root=Path(__file__).resolve().parents[2]
paths=sorted((root/'vignettes').glob('book-*.Rmd.orig'))
numbered=[p for p in paths if re.match(r'book-\d\d-',p.name)]
assert [int(p.name[5:7]) for p in numbered] == list(range(24))
assert len(paths)==25
menu=(root/'_pkgdown.yml').read_text().split('      menu:\n')[1].split('articles:\n')[0]
nav=re.findall(r'href: articles/(book-.*?)\.html',menu)
order=['book-showcase']+[p.name.removesuffix('.Rmd.orig') for p in numbered]
assert nav==order
index=(root/'pkgdown/index.md').read_text()
assert [m for m in re.findall(r'\]\(articles/(book-.*?)\.html\)',index) if m!='book-showcase'][:24]==order[1:]
rows=[]
for p in paths:
    s=p.read_text();slug=p.name.removesuffix('.Rmd.orig')
    forbidden=r"\b(?:v[12]|language[ -]2|legacy plan|plan version|APHRC|HDSS|Nairobi|Ethiopia|colleague)\b|RECORDED OUTPUT|precomput|[.]Rmd[.]orig|actions/runs|opal-e2e"
    assert not re.search(forbidden,s,re.I),(p,"Forbidden reader wording")
    if slug!='book-showcase':
        title=re.search(r'^title: "(.*?)"',s,re.M)[1]
        assert title.startswith(str(int(p.name[5:7]))+'. '),(p,title)
    assert s.count('<summary>Solution')>=3,p
    assert s.rfind('## Recap')>s.rfind('<summary>Solution'),p
    assert re.search(r'\*\*Next:\*\* .*\n?$',s),p
    rows.append(dict(page=slug,lines=len(s.splitlines()),solutions=s.count('<summary>Solution'),
                     chunks=len(re.findall(r'^```\{r ',s,re.M))))
assert order[19]=='book-18-capstone'
assert all('case-' in s for s in order[20:])
report=dict(pages=len(rows),numbered_chapters=list(range(24)),
            capstone_last_tutorial=True,case_studies=5,
            total_solutions=sum(x['solutions'] for x in rows),pages_detail=rows)
(root/'tools/book/evidence/book4/structure-audit.json').write_text(json.dumps(report,indent=2)+'\n')
print(json.dumps({k:v for k,v in report.items() if k!='pages_detail'},indent=2))
