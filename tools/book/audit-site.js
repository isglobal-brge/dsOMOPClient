// Run with NODE_PATH=/usr/local/lib/node_modules node tools/book/audit-site.js.
const { chromium } = require('playwright-chromium');
const fs = require('fs');
const path = require('path');
(async () => {
  const browser = await chromium.launch({headless:true,args:['--no-sandbox']});
  const results=[];
  for (const width of [1365,390]) {
    const page=await browser.newPage({viewport:{width,height:1000}});
    for (const slug of ['index', ...fs.readdirSync('docs/articles').filter(x => /^book-.*\.html$/.test(x)).map(x => 'articles/'+x.replace(/\.html$/, ''))]) {
      await page.goto('file://'+path.resolve('docs',slug+'.html'));
      const result=await page.evaluate(() => ({title:document.title,
        width:innerWidth,overflow:document.documentElement.scrollWidth>innerWidth,
        learnBoxes:document.querySelectorAll('.learn-box').length,
        headings:[...document.querySelectorAll('h1')].map(x=>x.textContent.trim()),
        colors:{heading:getComputedStyle(document.querySelector('h2')).color,
                font:getComputedStyle(document.body).fontFamily}}));
      if (slug.includes('book-')) {
        if(result.learnBoxes!==1) throw new Error('Missing learning box: '+slug);
        const solution=page.locator('details').first();
        await solution.locator('summary').click();
        if(!await solution.getAttribute('open') && await solution.getAttribute('open')===null)
          throw new Error('Solution does not open: '+slug);
      }
      if(result.overflow) throw new Error('Page horizontal overflow: '+slug+' '+width);
      results.push({slug,...result});
      if(slug==='articles/book-07-verbal-autopsy') {
        await page.evaluate(() => window.scrollTo({top:0,behavior:"instant"}));
        await page.screenshot({path:`tools/book/evidence/capstone-${width}.png`,fullPage:false});
      }
    }
    await page.close();
  }
  await browser.close();
  fs.writeFileSync('tools/book/evidence/site-browser-audit.json',JSON.stringify(results,null,2)+'\n');
  console.log('PASS: all book desktop/mobile pages; learning boxes, solutions and overflow checked.');
})().catch(e=>{console.error(e);process.exit(1)});
