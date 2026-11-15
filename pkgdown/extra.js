/* Book navigation derives from pkgdown's declared chapter order. */
document.addEventListener('DOMContentLoaded', () => {
  const main = document.querySelector('main');
  if (!main || !document.querySelector('.learn-box')) return;
  document.body.classList.add('book-page');
  // Keep scientific notation and other numeric results on one line on phones.
  main.querySelectorAll('td').forEach(cell => {
    if (/^(?:[<>]\s*)?[+-]?(?:\d+(?:\.\d*)?|\.\d+)(?:e[+-]?\d+)?$/i.test(cell.textContent.trim()))
      cell.classList.add('book-number');
  });
  const links = [...document.querySelectorAll('.navbar .dropdown-menu a')]
    .filter(a => /book-.*\.html$/.test(a.getAttribute('href') || ''));
  const nav = document.createElement('nav');
  nav.className = 'chapter-sidebar'; nav.setAttribute('aria-label', 'Tutorial chapters');
  const heading = document.createElement('h2'); heading.textContent = 'Tutorial chapters';
  nav.appendChild(heading);
  const list = document.createElement('ol');
  for (const source of links) {
    const li = document.createElement('li'); const a = source.cloneNode(true);
    a.className = ''; a.removeAttribute('tabindex');
    if (new URL(a.href).pathname === location.pathname) a.setAttribute('aria-current','page');
    li.appendChild(a); list.appendChild(li);
  }
  nav.appendChild(list);
  document.querySelector('.template-article > .row')?.prepend(nav);
  const toc = document.querySelector('#toc');
  if (toc) {
    const mobile = document.createElement('details'); mobile.className='mobile-contents';
    const summary = document.createElement('summary'); summary.textContent='In this chapter';
    mobile.appendChild(summary);
    // pkgdown populates its desktop TOC after DOMContentLoaded. Build the
    // mobile list from the already-rendered headings, without a timing race.
    const contents = document.createElement('ul');
    main.querySelectorAll('h2[id]').forEach(h => {
      const li=document.createElement('li'), a=document.createElement('a');
      a.href='#'+h.id; a.textContent=h.textContent.trim();
      li.appendChild(a); contents.appendChild(li);
    });
    mobile.appendChild(contents);
    main.querySelector('.page-header')?.after(mobile);
  }
  const active = links.findIndex(a => new URL(a.href).pathname === location.pathname);
  if (active >= 0) {
    const pager = document.createElement('nav'); pager.className='chapter-pager';
    pager.setAttribute('aria-label','Adjacent chapters');
    for (const [index, label] of [[active-1,'Previous'],[active+1,'Next']]) {
      if (!links[index]) continue;
      const a = links[index].cloneNode(true); a.className='';
      a.textContent = label+': '+links[index].textContent; pager.appendChild(a);
    }
    main.appendChild(pager);
  }
  document.querySelectorAll('main h2').forEach(h => {
    if (h.textContent.trim()==='Try it') h.classList.add('exercise-heading');
  });
});
