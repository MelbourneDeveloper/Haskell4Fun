/**
 * Configure marked options first
 */
marked.setOptions({
    highlight: function (code, lang) {
        return '<pre class="prettyprint lang-hs">' + code + '</pre>';
    },
    breaks: true,
    gfm: true,
    renderer: new marked.Renderer()
});

// Override the link renderer to handle our custom anchor format
const renderer = new marked.Renderer();
renderer.link = (href, title, text) => {
    if (href.startsWith('#') && !href.startsWith('#/')) {
        // Only transform pure anchor links that don't already have the path
        const currentRoute = window.location.hash.slice(2).split('#')[0];
        if (currentRoute.startsWith('read/')) {
            href = `#/${currentRoute}${href}`;
        }
    }
    return `<a href="${href}"${title ? ` title="${title}"` : ''}>${text}</a>`;
};

marked.setOptions({ renderer });

/**
 * Make navigateTo available globally
 */
window.navigateTo = (route, event) => {
    if (event) {
        event.preventDefault();
    }
    window.location.hash = `#/${route}`;
};

const updateMetaTags = (title, description, imageUrl, url) => {
    document.title = title || 'Default Title';
    const metaDescription = document.querySelector('meta[name="description"]');
    if (metaDescription) {
        metaDescription.setAttribute('content', description || 'Default description');
    }

    // Open Graph meta tags
    const ogTitle = document.querySelector('meta[property="og:title"]') || document.createElement('meta');
    ogTitle.setAttribute('property', 'og:title');
    ogTitle.setAttribute('content', title || 'Default Title');
    document.head.appendChild(ogTitle);

    const ogDescription = document.querySelector('meta[property="og:description"]') || document.createElement('meta');
    ogDescription.setAttribute('property', 'og:description');
    ogDescription.setAttribute('content', description || 'Default description');
    document.head.appendChild(ogDescription);

    const ogImage = document.querySelector('meta[property="og:image"]') || document.createElement('meta');
    ogImage.setAttribute('property', 'og:image');
    ogImage.setAttribute('content', imageUrl || 'default-image.jpg');
    document.head.appendChild(ogImage);

    const ogUrl = document.querySelector('meta[property="og:url"]') || document.createElement('meta');
    ogUrl.setAttribute('property', 'og:url');
    ogUrl.setAttribute('content', url || window.location.href);
    document.head.appendChild(ogUrl);

    // Twitter Card meta tags
    const twitterCard = document.querySelector('meta[name="twitter:card"]') || document.createElement('meta');
    twitterCard.setAttribute('name', 'twitter:card');
    twitterCard.setAttribute('content', 'summary_large_image');
    document.head.appendChild(twitterCard);

    const twitterTitle = document.querySelector('meta[name="twitter:title"]') || document.createElement('meta');
    twitterTitle.setAttribute('name', 'twitter:title');
    twitterTitle.setAttribute('content', title || 'Default Title');
    document.head.appendChild(twitterTitle);

    const twitterDescription = document.querySelector('meta[name="twitter:description"]') || document.createElement('meta');
    twitterDescription.setAttribute('name', 'twitter:description');
    twitterDescription.setAttribute('content', description || 'Default description');
    document.head.appendChild(twitterDescription);

    const twitterImage = document.querySelector('meta[name="twitter:image"]') || document.createElement('meta');
    twitterImage.setAttribute('name', 'twitter:image');
    twitterImage.setAttribute('content', imageUrl || 'default-image.jpg');
    document.head.appendChild(twitterImage);

    // Additional SEO meta tags
    const robots = document.querySelector('meta[name="robots"]') || document.createElement('meta');
    robots.setAttribute('name', 'robots');
    robots.setAttribute('content', 'index, follow');
    document.head.appendChild(robots);

    const canonical = document.querySelector('link[rel="canonical"]') || document.createElement('link');
    canonical.setAttribute('rel', 'canonical');
    canonical.setAttribute('href', url || window.location.href);
    document.head.appendChild(canonical);
};

/**
 * Handles routing based on the current hash.
 */
const handleRoute = async () => {
    const contentDiv = document.getElementById('content');
    contentDiv.style.opacity = '0';
    contentDiv.innerHTML = '';

    // Use the existing spinner from index.html
    const spinner = document.getElementById('spinner');
    spinner.style.display = 'block';
    spinner.classList.add('visible');

    // Get the current route from the hash
    const hash = window.location.hash;
    const [route, anchor] = hash.slice(2).split('#');

    // Default to 'about' if no route or empty route
    const currentRoute = route || 'about';

    try {
        let content;
        if (currentRoute.startsWith('read/')) {
            const articlePath = currentRoute.split('/').slice(1).join('/');
            const text = await fetchWithCache(`articles/${articlePath}.md`);
            const toc = await loadTOC();
            const article = toc.find(a => a.file === `articles/${articlePath}.md`);

            // Update meta tags with fallback to summary
            updateMetaTags(article.metaTitle, article.metaDescription || article.summary);

            // Transform markdown anchor links to include the article path
            const transformedText = text.replace(
                /\[([^\]]+)\]\(#([^)]+)\)/g,
                (_, title, section) => `[${title}](#/read/${articlePath}#${section})`
            );

            content = marked(transformedText);

            // Handle anchor scrolling after content is loaded
            if (anchor) {
                setTimeout(() => {
                    const element = document.querySelector(`#${anchor}`);
                    if (element) element.scrollIntoView();
                }, 350);
            }
        } else if (currentRoute.startsWith('labs/')) {
            const labName = currentRoute.split('/')[1];
            const text = await fetchWithCache(`articles/labs/${labName}.md`);
            content = marked(text);

        } else if (currentRoute === 'read') {

            // Read index

            content = await renderRead();
            // Default meta tags for read
            updateMetaTags('Read Articles - Haskell4.fun', 'Browse through a collection of articles to learn more about Haskell and functional programming.');
        } else if (currentRoute === 'labs') {
            // Labs index
            const text = await fetchWithCache('labs.md');
            content = marked(text);
            updateMetaTags('Labs - Haskell4.fun', 'Explore various labs and hands-on exercises to deepen your understanding of Haskell.');
        }

        else {
            // Load regular page
            const text = await fetchWithCache(`${currentRoute}.md`);
            content = marked(text);

            // Default meta tags for other pages
            updateMetaTags('Haskell4.fun', 'Learn Haskell and functional programming through articles, labs, and more.');
        }

        // First, prepare the content but don't show it yet
        contentDiv.innerHTML = `<div class="article">${content}</div>`;
        PR.prettyPrint();

        // Remove spinner with a fade out
        spinner.classList.remove('visible');

        // After a short delay to let spinner fade out, show the content
        setTimeout(() => {
            spinner.style.display = 'none';
            contentDiv.style.opacity = '1';

            // Scroll to anchor if present
            if (anchor) {
                const element = document.querySelector(`#${anchor}`);
                if (element) {
                    element.scrollIntoView();
                }
            }
        }, 300);

    } catch (error) {
        console.error('Route error:', error);
        contentDiv.innerHTML = `
            <div class="notification is-danger">
                <strong>${error.message}</strong>
            </div>
        `;
        contentDiv.style.opacity = '1';
        spinner.classList.remove('visible');
        spinner.style.display = 'none';
    }
};

/**
 * Load and display the table of contents.
 * @returns {Promise<string>} The rendered table of contents HTML
 */
const renderRead = async () => {
    try {
        const toc = await loadTOC();
        const content = `
            <h1>Articles</h1>
            <div class="articles-container">
                ${toc.map(({ title, file, summary, image, readingTime, tags }) => {
            if (!file) return ''; // Skip incomplete entries
            const articlePath = file.replace('articles/', '').replace('.md', '');
            return `
                        <div class="article-card">
                            <a href="#/read/${articlePath}" onclick="window.navigateTo('read/${articlePath}', event)">
                                ${image ? `<img src=".${image}" alt="${title}" class="article-image">` : ''}
                            </a>
                            <div class="article-content">
                                <h3 class="article-title">
                                    <a href="#/read/${articlePath}" onclick="window.navigateTo('read/${articlePath}', event)" class="hover-link">${title}</a>
                                </h3>
                                ${summary ? `<p class="article-summary">${summary}</p>` : ''}
                                <div class="article-metadata">
                                    ${readingTime ? `
                                        <span class="reading-time">
                                            <svg width="16" height="16" viewBox="0 0 16 16" fill="none">
                                                <path d="M8 3.5V8L10.5 10.5" stroke="currentColor" stroke-width="1.5" stroke-linecap="round"/>
                                                <circle cx="8" cy="8" r="6" stroke="currentColor" stroke-width="1.5"/>
                                            </svg>
                                            ${readingTime}
                                        </span>
                                    ` : ''}
                                </div>
                                ${tags ? `
                                    <div class="tags-container">
                                        ${tags.map(tag => `<span class="tag">${tag}</span>`).join('')}
                                    </div>
                                ` : ''}
                            </div>
                        </div>
                    `;
        }).join('')}
            </div>
        `;
        return content;
    } catch (error) {
        console.error('TOC error:', error);
        return `
            <div class="notification is-danger">
                <strong>Error loading table of contents: ${error.message}</strong>
            </div>
        `;
    }
};

/**
 * Load the table of contents from toc.json
 * @returns {Promise<Array>} Array of article objects
 */
const loadTOC = async () => {
    const data = await fetchWithCache('toc.json').then(text => JSON.parse(text));
    return data.articles;
};

// Move this block to the very top of the file
const CACHE_DURATION_MS = 5 * 60 * 1000; // 5 minutes

/** 
 * Represents a cached item with content and expiration
 * @typedef {{content: string, expiresAt: number}} CacheEntry
 */

/** @type {Map<string, CacheEntry>} */
const cache = new Map();

/**
 * Fetches content with caching
 * @param {string} url - URL to fetch
 * @returns {Promise<string>} The content
 */
const fetchWithCache = async url => {
    const cached = cache.get(url);
    const now = Date.now();

    if (cached && now < cached.expiresAt) {
        return cached.content;
    }

    const response = await fetch(url);
    if (!response.ok) {
        throw new Error(`Error ${response.status}: ${response.statusText}`);
    }

    const content = await response.text();
    cache.set(url, {
        content,
        expiresAt: now + CACHE_DURATION_MS
    });

    return content;
};

// Event listeners
window.addEventListener('hashchange', handleRoute);
window.addEventListener('load', handleRoute);

// Initialize
document.addEventListener('DOMContentLoaded', () => {
    const burger = document.querySelector('.navbar-burger');
    const menu = document.querySelector('.navbar-menu');

    burger.addEventListener('click', () => {
        burger.classList.toggle('is-active');
        menu.classList.toggle('is-active');
    });

    // Add click handlers to navigation items
    document.querySelectorAll('.navbar-item').forEach(item => {
        item.addEventListener('click', (event) => {
            const href = item.getAttribute('href');
            if (href && href.startsWith('#/')) {
                event.preventDefault();
                const route = href.slice(2); // Remove '#/'
                navigateTo(route, event);
            }
        });
    });

    // Handle initial route
    handleRoute();
});
