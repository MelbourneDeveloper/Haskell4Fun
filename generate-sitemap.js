const fs = require('fs');
const path = require('path');

// Read TOC.json
const toc = JSON.parse(fs.readFileSync('toc.json', 'utf8'));

// Base URL of the site
const baseUrl = 'https://haskell4.fun';

// Static routes
const staticRoutes = [
    '/',
    '/about',
    '/labs',
    '/read'
];

// Generate XML entries for static routes
const staticXml = staticRoutes
    .map(route => `
    <url>
        <loc>${baseUrl}${route === '/' ? '' : `/#${route}`}</loc>
        <changefreq>weekly</changefreq>
        <priority>${route === '/' ? '1.0' : '0.8'}</priority>
    </url>`)
    .join('');

// Generate XML entries for articles
const articlesXml = toc.articles
    .map(article => `
    <url>
        <loc>${baseUrl}/#/read/${article.file.replace('articles/', '').replace('.md', '')}</loc>
        <changefreq>weekly</changefreq>
        <priority>0.7</priority>
    </url>`)
    .join('');

// Generate XML entries for labs
const labsXml = fs.readdirSync('articles/labs')
    .filter(file => file.endsWith('.md'))
    .map(file => `
    <url>
        <loc>${baseUrl}/#/labs/${file.replace('.md', '')}</loc>
        <changefreq>weekly</changefreq>
        <priority>0.7</priority>
    </url>`)
    .join('');

// Combine all entries into final sitemap
const sitemap = `<?xml version="1.0" encoding="UTF-8"?>
<urlset xmlns="http://www.sitemaps.org/schemas/sitemap/0.9">
    ${staticXml}
    ${articlesXml}
    ${labsXml}
</urlset>`;

// Write sitemap to file with proper XML formatting
fs.writeFileSync('./sitemap.xml', sitemap.replace(/^\s+/gm, '    '));
console.log('Sitemap generated successfully!'); 