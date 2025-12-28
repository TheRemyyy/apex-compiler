import { defineConfig } from 'vite'
import react from '@vitejs/plugin-react'
import compression from 'vite-plugin-compression'

export default defineConfig({
    plugins: [
        react(),
        compression({
            algorithm: 'gzip',
            ext: '.gz',
        })
    ],
    build: {
        minify: 'terser',
        terserOptions: {
            compress: {
                drop_console: true,
                drop_debugger: true,
            },
        },
        rollupOptions: {
            output: {
                manualChunks: {
                    vendor: ['react', 'react-dom', 'react-router-dom'],
                    utils: ['marked'],
                },
                // Force CSS into a single file or split? keeping default is usually best for HTTP/2.
                // However, user specifically complained about the index css file.
                // Minimizing it via cssMinify is good.
            },
        },
        cssCodeSplit: true,
        cssMinify: true, // Use esbuild (default) or lightningcss if available.
    }
})
