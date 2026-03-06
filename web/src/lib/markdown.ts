import DOMPurify from 'dompurify';
import { marked } from 'marked';

const renderer = new marked.Renderer();

// marked's heading signature is broader than the typed package currently exposes.
// @ts-ignore typed package lags the runtime API here
renderer.heading = (text: string, depth: number) => {
    const escapedText = text.toLowerCase().replace(/[^\w]+/g, '-');
    return `<h${depth} id="${escapedText}">${text}</h${depth}>`;
};

marked.use({ renderer });

export async function renderMarkdown(markdown: string): Promise<string> {
    const html = await marked.parse(markdown);
    return sanitizeMarkdownHtml(html);
}

export function sanitizeMarkdownHtml(html: string): string {
    return DOMPurify.sanitize(html, {
        USE_PROFILES: { html: true },
    });
}

export function rewriteInternalDocLinks(html: string, currentPath: string): string {
    const tempDiv = document.createElement('div');
    tempDiv.innerHTML = html;

    const baseDocPath = `${currentPath}.md`;
    const baseUrl = new URL(baseDocPath, window.location.origin);

    const isExternalHref = (href: string) => /^(https?:|mailto:|tel:)/i.test(href);

    tempDiv.querySelectorAll('a').forEach((anchor) => {
        const rawHref = anchor.getAttribute('href');
        if (!rawHref || rawHref.startsWith('#') || isExternalHref(rawHref)) {
            return;
        }

        const resolved = new URL(rawHref, baseUrl);
        let path = resolved.pathname;

        if (path.endsWith('.md')) {
            path = path.slice(0, -3);
        }

        if (path.startsWith('/docs/docs/')) {
            path = path.replace('/docs/docs/', '/docs/');
        }

        const finalHref = `${path}${resolved.hash}`;
        anchor.setAttribute('href', finalHref);
        anchor.setAttribute('data-router-link', 'true');
    });

    return tempDiv.innerHTML;
}
