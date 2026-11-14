// Run from the package root with NODE_PATH and PLAYWRIGHT_BROWSERS_PATH set.
const {chromium}=require('playwright-chromium');
const path=require('path');
(async()=>{
 const browser=await chromium.launch({headless:true,args:['--no-sandbox']});
 for(const width of [1365,390]) {
  const page=await browser.newPage({viewport:{width,height:900}});
  for(const [slug,match,label] of [
   ['book-15-labs','Hemoglobin [Mass/volume] in Blood','lab-discovery'],
   ['book-15-labs','14 to below 15','ordered-counts'],
   ['book-16-longitudinal','Observed','window-values'],
   ['book-16-longitudinal','p-value','hazard-model']]) {
   await page.goto('file://'+path.resolve('docs/articles/'+slug+'.html'));
   const table = label==='window-values'
     ? page.locator('main table').filter({has:page.getByRole('columnheader',{name:'Observed',exact:true})}).first()
     : page.locator('main table').filter({hasText:match}).first();
   await table.scrollIntoViewIfNeeded();
   await page.screenshot({path:`tools/book/evidence/book3-fix1/${label}-${width}.png`});
  }
  await page.close();
 }
 await browser.close();
})().catch(e=>{console.error(e);process.exit(1)});
