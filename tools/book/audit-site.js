// Run with PLAYWRIGHT_BROWSERS_PATH=/home/dev/.cache/ms-playwright
// NODE_PATH=/usr/local/lib/node_modules node tools/book/audit-site.js.
const { chromium } = require('playwright-chromium');
const fs = require('fs');
const path = require('path');
(async () => {
  const browser = await chromium.launch({headless:true,args:['--no-sandbox']});
  const results=[];
  let scrollChecks=0;
  const chapters=fs.readdirSync('docs/articles').filter(x=>/^book-.*\.html$/.test(x));
  for (const width of [1365,390]) {
    const page=await browser.newPage({viewport:{width,height:900},permissions:['clipboard-read','clipboard-write']});
    for (const slug of ['index',...chapters.map(x=>'articles/'+x.replace(/\.html$/,''))]) {
      await page.goto('file://'+path.resolve('docs',slug+'.html'));
      const book=slug.includes('book-');
      if(book) await page.waitForSelector('.chapter-pager');
      const result=await page.evaluate(() => ({title:document.title,
        width:innerWidth,overflow:document.documentElement.scrollWidth>innerWidth,
        learnBoxes:document.querySelectorAll('.learn-box').length,
        chapterLinks:document.querySelectorAll('.chapter-sidebar a').length,
        byline:document.querySelector('.page-header')?.innerText.includes('David Sarrat González'),
        copyButtons:document.querySelectorAll('.btn-copy-ex').length,
        numericNoWrap:[...document.querySelectorAll('td.book-number')].every(td=>getComputedStyle(td).whiteSpace==='nowrap'),
        outputs:document.querySelectorAll('main pre:not(.sourceCode)').length,
        footer:document.querySelector('footer')?.textContent.includes('Book by'),
        headings:[...document.querySelectorAll('h1')].map(x=>x.textContent.trim())}));
      if (slug === 'articles/book-showcase') {
        const cards = await page.locator('.capability-card').evaluateAll(cards => cards.map(card => ({
          border: getComputedStyle(card).borderTopWidth,
          accent: getComputedStyle(card).borderTopColor,
          radius: getComputedStyle(card).borderTopLeftRadius
        })));
        if (cards.length !== 6 || cards.some(c => c.border !== '4px' || c.accent !== 'rgb(241, 145, 25)' || c.radius !== '10px'))
          throw new Error('Capability card styles missing: '+JSON.stringify(cards));
        result.capabilityCards = cards.length;
      }
      if(book) {
        if(!result.numericNoWrap||result.learnBoxes!==1||result.chapterLinks!==25||!result.byline||!result.copyButtons||!result.footer)
          throw new Error('Book presentation incomplete: '+JSON.stringify({slug,...result}));
        const solution=page.locator('details').filter({has:page.locator('summary',{hasText:'Solution'})}).first();
        await solution.locator(':scope > summary').click();
        if(await solution.getAttribute('open')===null) throw new Error('Solution does not open: '+slug);
        if(width===390) {
          const contents=page.locator('.mobile-contents');
          await contents.locator('summary').click();
          if(await contents.locator('a').count()===0) throw new Error('Missing mobile contents: '+slug);
          await contents.locator('summary').click();
        }
      }
      if(slug==='articles/book-18-verbal-autopsy' && !result.headings.some(x=>x.startsWith('18.')))
        throw new Error('Stale capstone number');
      if(['articles/book-showcase','articles/book-11-parameters','articles/book-06-events','articles/book-14-labs','articles/book-16-longitudinal'].includes(slug)) {
        await page.locator('div.sourceCode').first().hover();
        await page.locator('.btn-copy-ex').first().click();
        const copied=await page.evaluate(()=>navigator.clipboard.readText());
        if(!copied.includes('library(dsOMOPClient)')||copied.includes('#>'))
          throw new Error('Code copy included wrong text or recorded output');
        result.copyTest='passed';
      }
      if(['articles/book-05-recipe','articles/book-06-events','articles/book-08-outputs'].includes(slug)) {
        const full=page.locator('details').filter({has:page.locator('summary',{hasText:'Full aggregate response'})}).first();
        if(await full.count()!==1 || await full.getAttribute('open')!==null)
          throw new Error('Full aggregate response must be collapsed initially: '+slug);
      }
      if(['articles/book-14-labs','articles/book-16-longitudinal'].includes(slug)) {
        const warnings=page.locator('details.model-warnings');
        if(await warnings.count()!==1 || await warnings.getAttribute('open')!==null)
          throw new Error('Recorded fitting warnings must start collapsed');
        await warnings.locator('summary').click();
        if(!await warnings.innerText().then(x=>x.includes('glm.fit: algorithm did not converge')))
          throw new Error('Recorded fitting warnings were lost');
        await warnings.locator('summary').click();
      }
      if (book) {
        const scrolling = await page.locator('main pre, main .table').evaluateAll(blocks => {
          let checked = 0;
          for (const block of blocks) {
            if (!block.getClientRects().length) continue;
            const maximum = parseFloat(getComputedStyle(block).maxHeight);
            if (!(maximum <= 400)) throw new Error('Output/code height is not bounded');
            if (block.scrollHeight > block.clientHeight + 20) {
              if (block.clientHeight > 402) throw new Error('Long block flooded the page');
              block.scrollTop = 40;
              if (block.scrollTop <= 0) throw new Error('Long block cannot scroll');
              block.scrollTop = 0; checked++;
            }
          }
          return checked;
        });
        scrollChecks += scrolling;
        result.scrollableBlocks = scrolling;
      }
      if(result.overflow) throw new Error('Page horizontal overflow: '+slug+' '+width);
      results.push({slug,...result});
      if(['articles/book-showcase','articles/book-11-parameters','articles/book-16-longitudinal','articles/book-14-labs','articles/book-00-start','articles/book-05-recipe','articles/book-06-events','articles/book-08-outputs','articles/book-10-harmonise','articles/book-18-verbal-autopsy'].includes(slug)) {
        await page.evaluate(()=>window.scrollTo({top:0,behavior:'instant'}));
        await page.screenshot({path:`tools/book/evidence/book4a/${slug.split('/')[1]}-${width}.png`,fullPage:false});
      }
    }
    for (const [slug, headings] of [
      ['book-showcase', ['Named concepts', 'Choose the output shape', 'Numbers and their groupings']],
      ['book-11-parameters', ['Create a new object', 'Recode with case_when', 'Read the refusal messages', 'Filter using a predicate']]
    ]) {
      await page.goto('file://'+path.resolve('docs/articles/'+slug+'.html'));
      if (slug === 'book-showcase' && await page.locator('.capability-card').count() !== 6)
        throw new Error('Expected six capability cards');
      for (let i=0;i<headings.length;i++) {
        await page.locator('h2').filter({hasText:headings[i]}).scrollIntoViewIfNeeded();
        await page.screenshot({path:`tools/book/evidence/book4a/${slug}-section-${i}-${width}.png`,fullPage:false});
      }
    }
    await page.goto('file://'+path.resolve('docs/articles/book-16-longitudinal.html'));
    for (const [heading, name] of [['The same people, two shapes','shapes'], ['Irregular measurements','windows'], ['Ask the recipe','reshape'], ['Repeated measures','mixed']]) {
      await page.locator('h2').filter({hasText:heading}).scrollIntoViewIfNeeded();
      await page.screenshot({path:`tools/book/evidence/book4a/longitudinal-${name}-${width}.png`,fullPage:false});
    }
    await page.locator('main table').last().scrollIntoViewIfNeeded();
    await page.screenshot({path:`tools/book/evidence/book4a/longitudinal-model-${width}.png`,fullPage:false});
    await page.goto('file://'+path.resolve('docs/articles/book-14-labs.html'));
    await page.locator('h2').filter({hasText:'Pool across two connections'}).scrollIntoViewIfNeeded();
    await page.screenshot({path:`tools/book/evidence/book4a/lab-pooling-${width}.png`,fullPage:false});
    await page.locator('main table').last().scrollIntoViewIfNeeded();
    await page.screenshot({path:`tools/book/evidence/book4a/lab-model-${width}.png`,fullPage:false});
    await page.goto('file://'+path.resolve('docs/articles/book-06-events.html'));
    await page.locator('h2').filter({hasText:'Value bins and explicit high/low flags'}).scrollIntoViewIfNeeded();
    await page.screenshot({path:`tools/book/evidence/book4a/numeric-bins-${width}.png`,fullPage:false});
    await page.goto('file://'+path.resolve('docs/authors.html'));
    if(!await page.locator('main').innerText().then(x=>x.includes('Tutorial book by David Sarrat González')))
      throw new Error('Book credit missing on authors page');
    await page.close();
  }
  if (scrollChecks < 10) throw new Error("Insufficient real long-block scrolling coverage");
  await browser.close();
  fs.writeFileSync('tools/book/evidence/book4a/site-browser-audit.json',JSON.stringify(results,null,2)+'\n');
  console.log('PASS: all 52 desktop/mobile pages; bylines, chapter navigation, copy buttons, contents, solutions and overflow checked.');
})().catch(e=>{console.error(e);process.exit(1)});
