import { Routes, Route, useLocation } from 'react-router-dom';
import { Analytics } from '@vercel/analytics/react';
import { Header } from './components/Header';
import { Footer } from './components/Footer';
import { Home } from './pages/Home';
import { Docs } from './pages/Docs';

export default function App() {
    const location = useLocation();
    const isDocsPage = location.pathname.startsWith('/docs');

    return (
        <div className="min-h-screen bg-[#09090b] flex flex-col">
            <Header />
            <main className="flex-grow">
                <Routes>
                    <Route path="/" element={<Home />} />
                    <Route path="/docs/*" element={<Docs />} />
                </Routes>
            </main>
            {!isDocsPage && <Footer />}
            <Analytics />
        </div>
    );
}
