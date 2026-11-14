// Run with NODE_PATH=/usr/local/lib/node_modules node tools/book/audit-site.js.
const { chromium } = require('playwright-chromium');
const fs = require('fs');
const path = require('path');
(async () => {
  const browser = await chromium.launch({headless:true,args:['--no-sandbox']});
  const results=[];
  const chapters=fs.readdirSync('docs/articles').filter(x=>/^book-.*\.html$/.test(x));
  for (const width of [1365,390]) {
    const page=await browser.newPage({viewport:{width,height:900}});
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
        outputs:document.querySelectorAll('main pre:not(.sourceCode)').length,
        footer:document.querySelector('footer')?.textContent.includes('Book by'),
        headings:[...document.querySelectorAll('h1')].map(x=>x.textContent.trim())}));
      if(book) {
        if(result.learnBoxes!==1||result.chapterLinks!==16||!result.byline||!result.copyButtons||!result.footer)
          throw new Error('Book presentation incomplete: '+JSON.stringify({slug,...result}));
        const solution=page.locator('details').filter({has:page.locator('summary',{hasText:'Solution'})}).first();
        await solution.locator('summary').click();
        if(await solution.getAttribute('open')===null) throw new Error('Solution does not open: '+slug);
        if(width===390) {
          const contents=page.locator('.mobile-contents');
          await contents.locator('summary').click();
          if(await contents.locator('a').count()===0) throw new Error('Missing mobile contents: '+slug);
        }
      }
      if(result.overflow) throw new Error('Page horizontal overflow: '+slug+' '+width);
      results.push({slug,...result});
      if(['articles/book-06-events','articles/book-08-outputs','articles/book-08-harmonise','articles/book-07-verbal-autopsy'].includes(slug)) {
        await page.evaluate(()=>window.scrollTo({top:0,behavior:'instant'}));
        await page.screenshot({path:`tools/book/evidence/book2/${slug.split('/')[1]}-${width}.png`,fullPage:false});
      }
    }
    await page.goto('file://'+path.resolve('docs/authors.html'));
    if(!await page.locator('main').innerText().then(x=>x.includes('Tutorial book by David Sarrat González')))
      throw new Error('Book credit missing on authors page');
    await page.close();
  }
  await browser.close();
  fs.writeFileSync('tools/book/evidence/book2/site-browser-audit.json',JSON.stringify(results,null,2)+'\n');
  console.log('PASS: all 34 desktop/mobile pages; bylines, chapter navigation, copy buttons, contents, solutions and overflow checked.');
})().catch(e=>{console.error(e);process.exit(1)});
