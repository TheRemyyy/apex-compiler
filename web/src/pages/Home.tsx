import { Link } from 'react-router-dom';
import { Shield, Zap, Tool, ArrowRight, Brain, Cpu, MessageSquare } from 'lucide-react';

export function Home() {
    return (
        <div className="min-h-screen bg-[#050505] text-white pt-20 font-sans">
            {/* Hero Section */}
            <section className="max-w-7xl mx-auto px-6 py-24 flex flex-col lg:flex-row gap-16 items-center">
                <div className="flex-1 text-left space-y-8">
                    <div className="inline-flex items-center gap-2 px-3 py-1 rounded-full border border-neutral-800 bg-neutral-900 text-neutral-400 text-xs font-mono">
                        <span className="w-2 h-2 rounded-full bg-emerald-500 animate-pulse"></span>
                        Apex v1.1.2 — The New Standard
                    </div>

                    <h1 className="text-5xl md:text-7xl font-bold tracking-tight text-white leading-[1.05]">
                        The Language <br />
                        <span className="text-neutral-500">for Modern Systems.</span>
                    </h1>

                    <p className="text-xl text-neutral-400 max-w-2xl leading-relaxed">
                        Apex bridges the gap between the speed of C++ and the safety of Rust. 
                        <span className="text-white block mt-2 font-medium italic">High-level syntax. Zero-cost execution. Infinite possibilities.</span>
                    </p>

                    <div className="flex items-center gap-4 pt-4">
                        <Link to="/docs/overview.md" className="h-12 px-8 rounded-lg bg-white text-black font-semibold hover:bg-neutral-200 transition-all flex items-center gap-2">
                            Get Started <ArrowRight size={18} />
                        </Link>
                        <a href="https://github.com/TheRemyyy/apex-compiler" target="_blank" className="h-12 px-8 rounded-lg border border-neutral-800 hover:border-neutral-700 bg-neutral-900/50 text-neutral-300 font-medium transition-all flex items-center">
                            GitHub
                        </a>
                    </div>
                </div>

                {/* Code Terminal */}
                <div className="flex-1 w-full max-w-xl relative">
                    <div className="absolute -inset-4 bg-white/5 blur-3xl rounded-full"></div>
                    <div className="relative rounded-xl border border-neutral-800 bg-[#0A0A0A] p-2 shadow-[0_0_50px_-12px_rgba(255,255,255,0.1)]">
                        <div className="flex gap-1.5 px-3 pb-3 opacity-30">
                            <div className="w-3 h-3 rounded-full bg-white"></div>
                            <div className="w-3 h-3 rounded-full bg-white"></div>
                            <div className="w-3 h-3 rounded-full bg-white"></div>
                        </div>
                        <div className="rounded-lg border border-neutral-800/50 bg-[#050505] p-6 font-mono text-sm overflow-x-auto">
                            <pre className="text-neutral-300 font-mono text-sm leading-relaxed">
                                <code>
                                    <div><span className="text-purple-400">async function</span> <span className="text-blue-400">fetch_data</span>() {'{'}</div>
                                    <div className="pl-4"><span className="text-neutral-500">// Efficient native concurrency</span></div>
                                    <div className="pl-4"><span className="text-purple-400">let</span> response = <span className="text-purple-400">await</span> io.<span className="text-blue-400">get</span>(<span className="text-green-400">"api.v1/stream"</span>);</div>
                                    <div className="pl-4"></div>
                                    <div className="pl-4"><span className="text-purple-400">match</span> response {'{'}</div>
                                    <div className="pl-8"><span className="text-yellow-300">Ok</span>(data) =&gt; <span className="text-blue-400">process</span>(data),</div>
                                    <div className="pl-8"><span className="text-yellow-300">Err</span>(e) =&gt; <span className="text-red-400">panic</span>(e)</div>
                                    <div className="pl-4">{'}'}</div>
                                    <div>{'}'}</div>
                                </code>
                            </pre>
                        </div>
                    </div>
                </div>
            </section>

            {/* About / Philosophy Section */}
            <section className="py-24 border-y border-neutral-900 bg-neutral-950/30">
                <div className="max-w-7xl mx-auto px-6 grid md:grid-cols-2 gap-20 items-center">
                    <div>
                        <h2 className="text-3xl font-bold mb-8 tracking-tight">Systems Programming Shouldn't Be a Battle.</h2>
                        <div className="space-y-6 text-neutral-400 text-lg leading-relaxed">
                            <p>
                                For decades, developers had to choose: **Safety** or **Productivity**. C++ gave you the keys to the machine but no safety net. Rust gave you safety but made you fight the borrow checker for every line of code.
                            </p>
                            <p>
                                <span className="text-white font-semibold italic underline decoration-neutral-700 decoration-2 underline-offset-4">Apex was built to break this cycle.</span> We've engineered a compiler that understands your intent. By utilizing an intuitive ownership model and LLVM optimizations, Apex allows you to build kernels, engines, and web-servers with the flow of a high-level language.
                            </p>
                        </div>
                    </div>
                    <div className="grid grid-cols-1 sm:grid-cols-2 gap-6">
                        <StatBox label="Execution" value="NATIVE" />
                        <StatBox label="Backend" value="LLVM" />
                        <StatBox label="Safety" value="GUARANTEED" />
                        <StatBox label="Runtime" value="ZERO" />
                    </div>
                </div>
            </section>

            {/* Feature Grid */}
            <section className="max-w-7xl mx-auto px-6 py-32 grid md:grid-cols-3 gap-16">       
                <FeatureItem 
                    icon={Shield} 
                    title="Memory Safety" 
                    desc="Compile-time ownership rules prevent data races, use-after-free, and null pointer exceptions—without a garbage collector."
                />
                <FeatureItem 
                    icon={Zap} 
                    title="Infinite Performance" 
                    desc="By leveraging LLVM, Apex code is transformed into highly optimized machine instructions for your specific CPU architecture."
                />
                <FeatureItem 
                    icon={MessageSquare} 
                    title="Lidský Přístup" 
                    desc="Helpful compiler errors that actually explain what's wrong and how to fix it. We value developer time above all else."
                />
            </section>
        </div>
    );
}

function StatBox({ label, value }: { label: string, value: string }) {
    return (
        <div className="p-8 rounded-xl border border-neutral-800 bg-[#080808] text-center group hover:border-neutral-600 transition-all">
            <div className="text-2xl font-bold text-white mb-1 tracking-tighter group-hover:scale-110 transition-transform">{value}</div>
            <div className="text-xs text-neutral-500 uppercase tracking-widest">{label}</div>
        </div>
    );
}

function FeatureItem({ icon: Icon, title, desc }: { icon: any, title: string, desc: string }) {
    return (
        <div className="space-y-4 group">
            <div className="w-12 h-12 rounded-lg bg-neutral-900 border border-neutral-800 flex items-center justify-center group-hover:border-white/20 transition-all">
                <Icon className="text-neutral-400 group-hover:text-white transition-colors" size={24} />
            </div>
            <h3 className="text-xl font-bold text-white tracking-tight">{title}</h3>     
            <p className="text-neutral-500 leading-relaxed">
                {desc}
            </p>
        </div>
    );
}
