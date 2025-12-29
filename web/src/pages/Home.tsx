import { Link } from 'react-router-dom';
import { Shield, Zap, ArrowRight, Brain, Cpu, MessageSquare, Code, Blocks, Terminal } from 'lucide-react';
import { motion } from 'framer-motion';

export function Home() {
    return (
        <div className="min-h-screen bg-[#050505] text-white font-sans">
            {/* Hero Section */}
            <section className="relative max-w-7xl mx-auto px-6 py-32 flex flex-col lg:flex-row gap-16 items-center overflow-hidden">
                <div className="absolute top-0 right-0 w-[500px] h-[500px] bg-white/5 rounded-full blur-[120px] pointer-events-none" />
                
                <div className="flex-1 text-left space-y-10 relative z-10">
                    <motion.div 
                        initial={{ opacity: 0, y: 10 }}
                        animate={{ opacity: 1, y: 0 }}
                        className="inline-flex items-center gap-2 px-3 py-1.5 rounded-full border border-neutral-800 bg-neutral-900/50 text-neutral-400 text-xs font-mono"
                    >
                        <span className="w-2 h-2 rounded-full bg-emerald-500 animate-pulse"></span>
                        Apex v1.1.3 — The Native Evolution
                    </motion.div>

                    <motion.h1 
                        initial={{ opacity: 0, y: 10 }}
                        animate={{ opacity: 1, y: 0 }}
                        transition={{ delay: 0.1 }}
                        className="text-6xl md:text-8xl font-bold tracking-tighter text-white leading-[0.9]"
                    >
                        Systems. <br />
                        <span className="text-neutral-500 italic">Redefined.</span>
                    </motion.h1>

                    <motion.p 
                        initial={{ opacity: 0, y: 10 }}
                        animate={{ opacity: 1, y: 0 }}
                        transition={{ delay: 0.2 }}
                        className="text-xl text-neutral-400 max-w-2xl leading-relaxed font-medium"
                    >
                        Apex is a modern systems language built on LLVM. It provides the memory safety of Rust with an intuitive, high-level developer experience. No compromises.
                    </motion.p>

                    <motion.div 
                        initial={{ opacity: 0, y: 10 }}
                        animate={{ opacity: 1, y: 0 }}
                        transition={{ delay: 0.3 }}
                        className="flex flex-wrap gap-4 pt-4"
                    >
                        <Link to="/docs/overview.md" className="h-14 px-10 rounded-xl bg-white text-black font-bold hover:bg-neutral-200 transition-all flex items-center gap-3 text-lg">
                            Get Started <ArrowRight size={20} />
                        </Link>
                        <a href="https://github.com/TheRemyyy/apex-compiler" target="_blank" className="h-14 px-10 rounded-xl border border-neutral-800 hover:border-neutral-700 bg-neutral-900/30 text-neutral-300 font-medium transition-all flex items-center">
                            GitHub
                        </a>
                    </motion.div>
                </div>

                {/* Code Terminal */}
                <motion.div 
                    initial={{ opacity: 0, scale: 0.95 }}
                    animate={{ opacity: 1, scale: 1 }}
                    transition={{ delay: 0.2 }}
                    className="flex-1 w-full max-w-xl relative"
                >
                    <div className="relative rounded-2xl border border-neutral-800 bg-[#0A0A0A] p-2 shadow-2xl">
                        <div className="flex gap-1.5 px-4 py-3 border-b border-neutral-900 opacity-50">
                            <div className="w-3 h-3 rounded-full bg-neutral-700"></div>
                            <div className="w-3 h-3 rounded-full bg-neutral-700"></div>
                            <div className="w-3 h-3 rounded-full bg-neutral-700"></div>
                        </div>
                        <div className="p-8 font-mono text-sm leading-relaxed overflow-x-auto text-neutral-400">
                            <pre>
                                <code>
                                    <div><span className="text-purple-400">async function</span> <span className="text-blue-400">initialize_core</span>(): <span className="text-yellow-300">Task</span>&lt;<span className="text-yellow-300">Integer</span>&gt; {'{'}</div>
                                    <div className="pl-4"><span className="text-neutral-500">// Direct collection manipulation</span></div>
                                    <div className="pl-4">cores: <span className="text-yellow-300">List</span>&lt;<span className="text-yellow-300">Integer</span>&gt; = <span className="text-yellow-300">List</span>&lt;<span className="text-yellow-300">Integer</span>&gt;();</div>
                                    <div className="pl-4">cores.<span className="text-blue-400">push</span>(1);</div>
                                    <div className="pl-4"></div>
                                    <div className="pl-4"><span className="text-purple-400">match</span> (cores.<span className="text-blue-400">get</span>(0)) {'{'}</div>
                                    <div className="pl-8">1 =&gt; <span className="text-blue-400">println</span>(<span className="text-green-400">"Core sequence OK"</span>),</div>
                                    <div className="pl-8">_ =&gt; <span className="text-blue-400">exit</span>(1)</div>
                                    <div className="pl-4">{'}'}</div>
                                    <div className="pl-4"></div>
                                    <div className="pl-4"><span className="text-purple-400">return</span> cores.<span className="text-blue-400">length</span>();</div>
                                    <div>{'}'}</div>
                                </code>
                            </pre>
                        </div>
                    </div>
                </motion.div>
            </section>

            {/* About / Philosophy Section */}
            <section className="py-32 border-y border-neutral-900 bg-neutral-950/20">
                <div className="max-w-7xl mx-auto px-6 grid lg:grid-cols-2 gap-24 items-center">
                    <div>
                        <h2 className="text-4xl font-bold mb-10 tracking-tighter uppercase italic">Bridging the Gap.</h2>
                        <div className="space-y-8 text-neutral-400 text-xl leading-relaxed font-medium">
                            <p>
                                C++ gives you power but demands blood. Rust gives you safety but demands your time. <span className="text-white font-semibold italic underline decoration-neutral-700 decoration-2 underline-offset-4">Apex was created to end the war.</span>
                            </p>
                            <p>
                                We've engineered a compiler that feels like an ally, not an obstacle. By leveraging <span className="text-white border-b-2 border-neutral-700">ownership inference</span> and the full power of the LLVM backend, Apex lets you build the next generation of software with the flow of a modern script language.
                            </p>
                        </div>
                    </div>
                    <div className="grid grid-cols-2 gap-6">
                        <StatItem label="Performance" value="NATIVE" />
                        <StatItem label="Backend" value="LLVM" />
                        <StatItem label="Safety" value="CHECKED" />
                        <StatItem label="Tooling" value="MODERN" />
                    </div>
                </div>
            </section>

            {/* Features Grid */}
            <section className="max-w-7xl mx-auto px-6 py-32 space-y-24">
                <div className="text-center space-y-4">
                    <h2 className="text-5xl font-bold tracking-tighter uppercase">Engineered for Speed</h2>
                    <p className="text-neutral-500 text-xl max-w-2xl mx-auto">Modern features integrated into a single, cohesive compiler.</p>
                </div>
                <div className="grid md:grid-cols-3 gap-12">
                    <FeatureBox 
                        icon={Shield} 
                        title="Ownership 2.0" 
                        desc="Advanced lifetime tracking that prevents memory errors without the boilerplate complexity of other languages."
                    />
                    <FeatureBox 
                        icon={Blocks} 
                        title="Zero-Cost Generics" 
                        desc="Build reusable components that compile to specialized, hyper-efficient machine code. No runtime overhead."
                    />
                    <FeatureBox 
                        icon={Terminal} 
                        title="Helpful Errors" 
                        desc="No more cryptic warnings. Apex provides precise error messages with suggestions on how to fix them instantly."
                    />
                </div>
            </section>
        </div>
    );
}

function StatItem({ label, value }: { label: string, value: string }) {
    return (
        <div className="p-10 rounded-2xl border border-neutral-800 bg-[#080808] text-center group hover:border-neutral-600 transition-all">
            <div className="text-3xl font-bold text-white mb-2 tracking-tighter group-hover:scale-110 transition-transform italic">{value}</div>
            <div className="text-xs text-neutral-500 uppercase tracking-widest font-bold font-mono">{label}</div>
        </div>
    );
}

function FeatureBox({ icon: Icon, title, desc }: { icon: any, title: string, desc: string }) {
    return (
        <div className="p-10 rounded-2xl bg-neutral-900/20 border border-neutral-800 hover:border-neutral-700 transition-all group">
            <div className="w-14 h-14 rounded-xl bg-neutral-900 border border-neutral-800 flex items-center justify-center mb-8 group-hover:border-white/20 transition-all">
                <Icon className="text-neutral-500 group-hover:text-white transition-colors" size={28} />
            </div>
            <h3 className="text-2xl font-bold text-white tracking-tight uppercase italic mb-4">{title}</h3>     
            <p className="text-neutral-400 leading-relaxed text-lg">
                {desc}
            </p>
        </div>
    );
}




