import { createSignal, createEffect } from '@remyyy/velox';
import { marked } from 'marked';
import './index.css';

// Configure marked to add IDs to headings
// Configure marked to add IDs to headings
const renderer = new marked.Renderer();
// @ts-ignore
renderer.heading = (text: string, depth: number) => {
    const escapedText = text.toLowerCase().replace(/[^\w]+/g, '-');
    return `<h${depth} id="${escapedText}">${text}</h${depth}>`;
};

marked.use({ renderer });

// Navigation structure matches file system
const NAV_ITEMS = [
    { title: 'Overview', path: '/docs/overview.md' },
    {
        title: 'Getting Started', items: [
            { title: 'Installation', path: '/docs/getting_started/installation.md' },
            { title: 'Quick Start', path: '/docs/getting_started/quick_start.md' },
            { title: 'Editor Setup', path: '/docs/getting_started/editor_setup.md' },
        ]
    },
    {
        title: 'Basics', items: [
            { title: 'Syntax', path: '/docs/basics/syntax.md' },
            { title: 'Variables', path: '/docs/basics/variables.md' },
            { title: 'Types', path: '/docs/basics/types.md' },
            { title: 'Control Flow', path: '/docs/basics/control_flow.md' },
        ]
    },
    {
        title: 'Features', items: [
            { title: 'Functions', path: '/docs/features/functions.md' },
            { title: 'Classes', path: '/docs/features/classes.md' },
            { title: 'Interfaces', path: '/docs/features/interfaces.md' },
            { title: 'Enums', path: '/docs/features/enums.md' },
            { title: 'Modules', path: '/docs/features/modules.md' },
        ]
    },
    {
        title: 'Standard Library', items: [
            { title: 'Math', path: '/docs/stdlib/math.md' },
            { title: 'String', path: '/docs/stdlib/string.md' },
            { title: 'Collections', path: '/docs/stdlib/collections.md' },
            { title: 'I/O', path: '/docs/stdlib/io.md' },
            { title: 'System', path: '/docs/stdlib/system.md' },
        ]
    },
    {
        title: 'Advanced', items: [
            { title: 'Ownership', path: '/docs/advanced/ownership.md' },
            { title: 'Generics', path: '/docs/advanced/generics.md' },
            { title: 'Async/Await', path: '/docs/advanced/async.md' },
            { title: 'Error Handling', path: '/docs/advanced/error_handling.md' },
            { title: 'Memory Management', path: '/docs/advanced/memory_management.md' },
        ]
    },
    {
        title: 'Compiler', items: [
            { title: 'CLI', path: '/docs/compiler/cli.md' },
            { title: 'Architecture', path: '/docs/compiler/architecture.md' },
        ]
    }
];

// Helper for conditional rendering
function Show(props: { when: () => boolean, fallback?: any, children: any }) {
    const container = document.createElement('div');
    container.style.display = 'contents';

    createEffect(() => {
        container.innerHTML = '';
        const condition = props.when();

        if (condition) {
            if (typeof props.children === 'function') {
                const child = props.children();
                if (child instanceof Node) container.appendChild(child);
                else if (child && child.exec) container.appendChild(child.exec());
            } else {
                if (props.children instanceof Node) container.appendChild(props.children);
                else if (props.children && props.children.exec) container.appendChild(props.children.exec());
            }
        } else if (props.fallback) {
            if (props.fallback instanceof Node) container.appendChild(props.fallback);
            else if (props.fallback && props.fallback.exec) container.appendChild(props.fallback.exec());
        }
    });

    return { exec: () => container };
}

// InnerHTML helper
function InnerHTML(props: { html: () => string, className?: string }) {
    const el = document.createElement('article');
    if (props.className) el.className = props.className;

    createEffect(() => {
        el.innerHTML = props.html();
    });

    return { exec: () => el };
}

// Dynamic Table Of Contents Component
// Parse H2s from the content HTML
function TableOfContents(props: { html: () => string }) {
    const [headings, setHeadings] = createSignal<{ id: string, text: string }[]>([]);

    createEffect(() => {
        const tempDiv = document.createElement('div');
        tempDiv.innerHTML = props.html();
        const headers = Array.from(tempDiv.querySelectorAll('h2'));

        const extracted = headers.map(h => {
            const text = h.textContent || '';
            // Must match the marked renderer logic
            const id = text.toLowerCase().replace(/[^\w]+/g, '-');
            return { id, text };
        });
        setHeadings(extracted);
    });

    return (
        <div className="fixed w-64 right-0 top-0 h-full p-8 pt-24 border-l border-[#1f1f23] hidden xl:block overflow-y-auto">
            <h5 className="text-xs font-bold text-gray-500 uppercase tracking-widest mb-4">On This Page</h5>
            <ul className="space-y-3">
                {headings().map((h, i) => (
                    <li key={i}>
                        <a href={`#${h.id}`} className="text-[13px] text-gray-400 hover:text-purple-400 transition-colors block leading-snug">
                            {h.text}
                        </a>
                    </li>
                ))}
            </ul>
        </div>
    );
}


export default function App() {
    const [currentPath, setCurrentPath] = createSignal('/docs/overview.md');
    const [content, setContent] = createSignal('');
    const [loading, setLoading] = createSignal(true);

    createEffect(() => {
        const path = currentPath();
        setLoading(true);
        fetch(path)
            .then(res => res.text())
            .then(async text => {
                const html = await marked.parse(text);
                setContent(html);
                setLoading(false);
                // Scroll to top
                window.scrollTo(0, 0);
            })
            .catch(err => {
                console.error(err);
                setContent('<h1>Error loading document</h1>');
                setLoading(false);
            });
    });

    return (
        <div className="flex min-h-screen bg-[#09090b] text-gray-100 font-sans selection:bg-purple-500/30">

            {/* Sidebar: Fixed Left */}
            <nav className="fixed w-72 left-0 top-0 h-full border-r border-[#1f1f23] bg-[#0c0c0e] flex flex-col z-10">
                <div className="p-6 pb-4 border-b border-[#1f1f23]/50">
                    <h1 className="text-xl font-bold bg-gradient-to-br from-white to-gray-400 bg-clip-text text-transparent tracking-tight">
                        Apex Compiler
                    </h1>
                    <div className="mt-2 flex items-center gap-2">
                        <span className="px-2 py-0.5 rounded-full bg-purple-500/10 text-purple-400 text-[11px] font-medium border border-purple-500/20">v1.1.1</span>
                        <span className="text-[11px] text-gray-500">Docs</span>
                    </div>
                </div>

                <div className="flex-1 overflow-y-auto p-6 custom-scrollbar space-y-8">
                    {NAV_ITEMS.map((section, idx) => (
                        <section key={idx}>
                            {section.items ? (
                                <>
                                    <h3 className="text-[11px] font-bold text-gray-500 uppercase tracking-widest mb-3 pl-2">
                                        {section.title}
                                    </h3>
                                    <ul className="space-y-0.5">
                                        {section.items.map((item, itemIdx) => {
                                            // @ts-ignore
                                            return (
                                                <li key={itemIdx}>
                                                    <button
                                                        onClick={() => setCurrentPath(item.path)}
                                                        className={(() => `w-full text-left px-3 py-1.5 rounded-md text-[14px] font-medium transition-all duration-200 ${currentPath() === item.path
                                                            ? 'bg-[#18181b] text-purple-400 shadow-sm border border-[#27272a]'
                                                            : 'text-gray-400 hover:text-gray-200 hover:bg-[#18181b]/50'
                                                            }`) as any}
                                                    >
                                                        {item.title}
                                                    </button>
                                                </li>
                                            );
                                        })}
                                    </ul>
                                </>
                            ) : (
                                <button
                                    onClick={() => setCurrentPath(section.path)}
                                    className={(() => `w-full text-left px-3 py-1.5 rounded-md text-[14px] font-bold uppercase tracking-wider mb-2 transition-colors ${currentPath() === section.path ? 'bg-[#18181b] text-purple-400 shadow-sm border border-[#27272a]' : 'text-gray-500 hover:text-gray-300 hover:bg-[#18181b]/50'
                                        }`) as any}
                                >
                                    {section.title}
                                </button>
                            )}
                        </section>
                    ))}
                </div>
            </nav>

            {/* Center Layout: Fluid with Max Width + Right TOC spacing */}
            <div className="flex-1 ml-72 xl:mr-64 w-full">
                <main className="max-w-4xl mx-auto px-12 py-16 w-full">
                    <Show when={loading} fallback={
                        <>
                            <InnerHTML
                                html={content}
                                className="prose prose-invert prose-zinc max-w-none 
                        prose-headings:scroll-mt-24
                        prose-h1:text-4xl prose-h1:font-bold prose-h1:tracking-tight prose-h1:mb-8 prose-h1:text-white
                        prose-h2:text-2xl prose-h2:font-semibold prose-h2:mt-12 prose-h2:mb-6 prose-h2:text-gray-100 prose-h2:border-b prose-h2:border-[#27272a] prose-h2:pb-2
                        prose-h3:text-xl prose-h3:font-semibold prose-h3:mt-8 prose-h3:mb-4 prose-h3:text-gray-200
                        prose-p:text-[16px] prose-p:leading-7 prose-p:text-gray-300 prose-p:mb-6
                        prose-ul:my-6 prose-ul:list-disc prose-ul:pl-6 prose-li:text-gray-300 prose-li:mb-2
                        prose-strong:text-white prose-strong:font-semibold
                        prose-code:text-[13px] prose-code:bg-[#18181b] prose-code:px-1.5 prose-code:py-0.5 prose-code:rounded-md prose-code:border prose-code:border-[#27272a]/50
                        prose-pre:bg-[#0c0c0e] prose-pre:border prose-pre:border-[#27272a] prose-pre:rounded-lg prose-pre:shadow-sm"
                            />
                            {/* Render TOC ONLY when content is loaded */}
                            <TableOfContents html={content} />
                        </>
                    }>
                        <div className="animate-pulse space-y-8 pt-4">
                            <div className="h-10 bg-[#1f1f23] rounded w-1/2 mb-8"></div>
                            <div className="space-y-4">
                                <div className="h-4 bg-[#1f1f23] rounded w-full"></div>
                                <div className="h-4 bg-[#1f1f23] rounded w-5/6"></div>
                                <div className="h-4 bg-[#1f1f23] rounded w-4/6"></div>
                            </div>
                        </div>
                    </Show>
                </main>
            </div>

        </div>
    );
}
