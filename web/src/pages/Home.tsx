import { Link } from 'react-router-dom';

export function Home() {
    return (
        <div className="min-h-screen bg-[#050505] text-white pt-20">
            {/* Hero Section */}
            <section className="max-w-7xl mx-auto px-6 py-20 flex flex-col md:flex-row gap-12 items-center">
                <div className="flex-1 text-left space-y-8">
                    <div className="inline-block px-2.5 py-1 rounded-sm border border-neutral-800 bg-neutral-900 text-neutral-400 text-xs font-mono">
                        Available in v1.1.2
                    </div>

                    <h1 className="text-4xl md:text-6xl font-bold tracking-tight text-white leading-[1.1]">
                        Systems programming, <span className="text-neutral-400">simplified.</span>
                    </h1>

                    <p className="text-lg text-neutral-400 max-w-xl leading-relaxed">
                        Apex is a compiled language designed for building reliable software.
                        It guarantees memory safety without garbage collection, and provides
                        modern tooling out of the box.
                    </p>

                    <div className="flex items-center gap-4 pt-4">
                        <Link to="/docs/overview.md" className="h-10 px-6 rounded bg-white text-black font-medium hover:bg-neutral-200 transition-colors flex items-center">
                            Get Started
                        </Link>
                        <a href="https://github.com/TheRemyyy/apex-compiler" target="_blank" className="h-10 px-6 rounded border border-neutral-800 hover:bg-neutral-900 text-neutral-300 font-medium transition-colors flex items-center">
                            GitHub
                        </a>
                    </div>
                </div>

                {/* Code Terminal */}
                <div className="flex-1 w-full max-w-lg">
                    <div className="rounded-lg border border-neutral-800 bg-[#0A0A0A] p-2 shadow-2xl">
                        <div className="flex gap-1.5 px-2 pb-2 opacity-30">
                            <div className="w-2.5 h-2.5 rounded-full bg-white"></div>
                            <div className="w-2.5 h-2.5 rounded-full bg-white"></div>
                            <div className="w-2.5 h-2.5 rounded-full bg-white"></div>
                        </div>
                        <div className="rounded border border-neutral-800/50 bg-[#050505] p-4 font-mono text-sm overflow-x-auto">
                            <pre className="text-neutral-300 font-mono text-sm leading-relaxed">
                                <code>
                                    <div><span className="text-purple-400">function</span> <span className="text-blue-400">main</span>() {'{'}</div>
                                    <div className="pl-4"><span className="text-neutral-500">// Native performance</span></div>
                                    <div className="pl-4"><span className="text-purple-400">let</span> items: <span className="text-yellow-300">List</span>&lt;<span className="text-yellow-300">Integer</span>&gt; = <span className="text-yellow-300">List</span>&lt;<span className="text-yellow-300">Integer</span>&gt;();</div>
                                    <div className="pl-4">items.<span className="text-blue-400">push</span>(1);</div>
                                    <div className="pl-4">items.<span className="text-blue-400">push</span>(2);</div>
                                    <div className="pl-4"></div>
                                    <div className="pl-4"><span className="text-purple-400">for</span> x <span className="text-purple-400">in</span> items {'{'}</div>
                                    <div className="pl-8"><span className="text-blue-400">println</span>(<span className="text-green-400">"Processing: "</span> + x);</div>
                                    <div className="pl-4">{'}'}</div>
                                    <div>{'}'}</div>
                                </code>
                            </pre>
                        </div>
                    </div>
                </div>
            </section>

            {/* Feature Grid - Minimalist */}
            <section className="border-t border-neutral-900 bg-[#050505]">
                <div className="max-w-7xl mx-auto px-6 py-20 grid md:grid-cols-3 gap-12">
                    <div className="space-y-3">
                        <h3 className="text-lg font-bold text-white">Performance First</h3>
                        <p className="text-neutral-500 leading-relaxed">
                            Compiles to optimized native machine code via LLVM.
                            Zero runtime, zero garbage collection, maximum throughput.
                        </p>
                    </div>
                    <div className="space-y-3">
                        <h3 className="text-lg font-bold text-white">Memory Safety</h3>
                        <p className="text-neutral-500 leading-relaxed">
                            Ownership and borrowing rules checked at compile-time.
                            Prevent data races and null pointer exceptions before they happen.
                        </p>
                    </div>
                    <div className="space-y-3">
                        <h3 className="text-lg font-bold text-white">Productivity</h3>
                        <p className="text-neutral-500 leading-relaxed">
                            Modern package manager, build system, and formatter included.
                            Helpful compiler error messages that guide you to the fix.
                        </p>
                    </div>
                </div>
            </section>
        </div>
    );
}
