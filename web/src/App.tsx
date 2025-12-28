import { createSignal, createEffect } from '@remyyy/velox';
import { marked } from 'marked';
import './index.css';

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

// Helper for conditional rendering that reacts to signal changes
function Show(props: { when: () => boolean, fallback?: any, children: any }) {
    const container = document.createElement('div');
    container.style.display = 'contents';

    createEffect(() => {
        container.innerHTML = '';
        const condition = props.when();

        if (condition) {
            if (typeof props.children === 'function') {
                const child = props.children();
                if (child instanceof Node) {
                    container.appendChild(child);
                } else if (child && child.exec) {
                    container.appendChild(child.exec());
                }
            } else {
                if (props.children instanceof Node) {
                    container.appendChild(props.children);
                } else if (props.children && props.children.exec) {
                    container.appendChild(props.children.exec());
                }
            }
        } else if (props.fallback) {
            if (props.fallback instanceof Node) {
                container.appendChild(props.fallback);
            } else if (props.fallback && props.fallback.exec) {
                container.appendChild(props.fallback.exec());
            }
        }
    });

    return { exec: () => container };
}

// Custom directive to set innerHTML reactively
function InnerHTML(props: { html: () => string, className?: string }) {
    const el = document.createElement('article');
    if (props.className) el.className = props.className;

    createEffect(() => {
        el.innerHTML = props.html();
    });

    return { exec: () => el };
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
            })
            .catch(err => {
                console.error(err);
                setContent('<h1>Error loading document</h1>');
                setLoading(false);
            });
    });

    return (
        <div className="flex min-h-screen bg-[#0f0f11] text-gray-100 font-sans selection:bg-purple-500/30">

            {/* Sidebar */}
            <nav className="w-64 border-r border-[#1f1f23] p-6 flex flex-col fixed h-full overflow-y-auto custom-scrollbar">
                <h1 className="text-2xl font-bold text-purple-500 mb-8">
                    Apex Docs
                </h1>

                <div className="space-y-6">
                    {NAV_ITEMS.map((section, idx) => (
                        <div key={idx}>
                            {section.items ? (
                                <>
                                    <h3 className="text-xs font-bold text-gray-500 uppercase tracking-wider mb-2">
                                        {section.title}
                                    </h3>
                                    <ul className="space-y-1">
                                        {section.items.map((item, itemIdx) => {
                                            // @ts-ignore
                                            return (
                                                <li key={itemIdx}>
                                                    <button
                                                        onClick={() => setCurrentPath(item.path)}
                                                        className={(() => `w-full text-left px-3 py-2 rounded-lg text-sm transition-all duration-200 ${currentPath() === item.path
                                                            ? 'bg-purple-500/10 text-purple-400 font-medium'
                                                            : 'text-gray-400 hover:text-gray-100 hover:bg-[#18181b]'
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
                                    className={(() => `w-full text-left text-sm font-bold uppercase tracking-wider mb-2 transition-colors ${currentPath() === section.path ? 'text-purple-400' : 'text-gray-500 hover:text-gray-300'
                                        }`) as any}
                                >
                                    {section.title}
                                </button>
                            )}
                        </div>
                    ))}
                </div>
            </nav>

            {/* Main Content */}
            <main className="flex-1 ml-64 p-12 max-w-5xl mx-auto">
                <Show when={loading} fallback={
                    // Fallback is shown when NOT loading (i.e. content ready)
                    <InnerHTML
                        html={content}
                        className="prose prose-invert prose-purple max-w-none 
                prose-headings:font-bold prose-h1:text-4xl prose-h1:tracking-tight prose-h1:mb-8
                prose-p:text-gray-300 prose-p:leading-7
                prose-code:text-purple-300 prose-code:bg-[#18181b] prose-code:px-1.5 prose-code:py-0.5 prose-code:rounded prose-code:before:content-none prose-code:after:content-none
                prose-pre:bg-[#18181b] prose-pre:border prose-pre:border-[#27272a]
                prose-strong:text-white"
                    />
                }>
                    {/* Children are shown when loading is TRUE */}
                    <div className="animate-pulse flex flex-col space-y-4">
                        <div className="h-8 bg-[#1f1f23] rounded w-3/4"></div>
                        <div className="h-4 bg-[#1f1f23] rounded w-full"></div>
                        <div className="h-4 bg-[#1f1f23] rounded w-5/6"></div>
                    </div>
                </Show>
            </main>
        </div>
    );
}
