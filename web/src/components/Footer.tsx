import { Link } from 'react-router-dom';

export function Footer() {
    return (
        <footer className="border-t border-[#1f1f23] bg-[#050505] text-[#a1a1aa] py-12">
            <div className="max-w-7xl mx-auto px-6 grid md:grid-cols-4 gap-8">
                <div className="col-span-2">
                    <Link to="/" className="text-lg font-bold text-white tracking-tight flex items-center gap-2 mb-4">
                        Apex
                    </Link>
                    <p className="max-w-xs text-sm leading-relaxed">
                        A modern systems programming language designed for reliability, performance, and developer ergonomics.
                    </p>
                </div>

                <div>
                    <h3 className="text-sm font-semibold text-white tracking-wider uppercase mb-4">Resources</h3>
                    <ul className="space-y-3 text-sm">
                        <li><Link to="/docs/overview.md" className="hover:text-white transition-colors">Documentation</Link></li>
                        <li><Link to="/docs/stdlib/overview.md" className="hover:text-white transition-colors">Standard Library</Link></li>`n                        <li><Link to="/changelog" className="hover:text-white transition-colors">Changelog</Link></li>
                        <li><a href="https://github.com/TheRemyyy/apex-compiler" className="hover:text-white transition-colors">GitHub</a></li>
                    </ul>
                </div>

                <div>
                    <h3 className="text-sm font-semibold text-white tracking-wider uppercase mb-4">Community</h3>
                    <ul className="space-y-3 text-sm">
                        <li><a href="#" className="hover:text-white transition-colors">Discord (Coming Soon)</a></li>
                        <li><a href="#" className="hover:text-white transition-colors">Twitter (Coming Soon)</a></li>
                    </ul>
                </div>
            </div>
            <div className="max-w-7xl mx-auto px-6 mt-12 pt-8 border-t border-[#1f1f23] text-xs text-center md:text-left flex flex-col md:flex-row justify-between items-center">
                <p>&copy; {new Date().getFullYear()} Apex Compiler. Open Source (MIT).</p>
                <div className="flex gap-4 mt-4 md:mt-0">
                    <div className="w-2 h-2 rounded-full bg-green-500"></div>
                    <span>Systems Operational</span>
                </div>
            </div>
        </footer>
    );
}

