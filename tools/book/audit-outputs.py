"""Compare every recorded console block and Markdown table with the built HTML."""
from html.parser import HTMLParser
from pathlib import Path
import json
import re


class Rendered(HTMLParser):
    def __init__(self):
        super().__init__()
        self.main = False
        self.pre = None
        self.table = None
        self.row = None
        self.cell = None
        self.outputs = []
        self.tables = []

    def handle_starttag(self, tag, attrs):
        if tag == 'main':
            self.main = True
        if not self.main:
            return
        if tag == 'pre':
            self.pre = ''
        if tag == 'table':
            self.table = []
        if tag == 'tr' and self.table is not None:
            self.row = []
        if tag in ('td', 'th') and self.row is not None:
            self.cell = ''

    def handle_data(self, data):
        if self.pre is not None:
            self.pre += data
        if self.cell is not None:
            self.cell += data

    def handle_endtag(self, tag):
        if tag == 'pre' and self.pre is not None:
            if self.pre.lstrip().startswith('#>'):
                self.outputs.append(normalize(self.pre))
            self.pre = None
        if tag in ('td', 'th') and self.cell is not None:
            self.row.append(table_punctuation(self.cell.strip()))
            self.cell = None
        if tag == 'tr' and self.row is not None:
            self.table.append(self.row)
            self.row = None
        if tag == 'table' and self.table is not None:
            self.tables.append(self.table)
            self.table = None
        if tag == 'main':
            self.main = False


def normalize(text):
    return '\n'.join(line.rstrip() for line in text.strip('\n').splitlines())


def table_punctuation(text):
    # Pandoc smart typography changes a double hyphen in a vocabulary name to
    # an en dash. Normalize that presentation change on both sides only.
    return text.replace('–', '--')


def cell_text(text):
    text = re.sub(r'\[([^\]]+)\]\([^)]*\)', r'\1', text)
    return table_punctuation(text.replace('`', '').replace('**', '').strip())


root = Path(__file__).resolve().parents[2]
report = {}
for chapter in sorted((root / 'vignettes').glob('book-*.Rmd')):
    source = chapter.read_text()
    outputs = [normalize(block) for block in re.findall(
        r'^```[^\n]*\n(.*?)^```\s*$', source, flags=re.M | re.S)
        if block.lstrip().startswith('#>')]
    tables = []
    for block in re.findall(r'(?:^\|[^\n]*\n)+', source, flags=re.M):
        rows = [[cell_text(cell) for cell in line.strip().strip('|').split('|')]
                for line in block.splitlines()]
        if len(rows) >= 2 and all(re.fullmatch(r':?-+:?', cell) for cell in rows[1]):
            tables.append([rows[0], *rows[2:]])
    rendered = Rendered()
    rendered.feed((root / 'docs/articles' / (chapter.stem + '.html')).read_text())
    assert outputs == rendered.outputs, f'Console output mismatch: {chapter.name}'
    assert tables == rendered.tables, f'Table mismatch: {chapter.name}'
    report[chapter.name] = {'outputs': len(outputs), 'tables': len(tables), 'match': True}
assert len(report) == 19
result = {'chapters': report, 'console_blocks': sum(r['outputs'] for r in report.values()),
          'tables': sum(r['tables'] for r in report.values())}
print(json.dumps(result, indent=2))
