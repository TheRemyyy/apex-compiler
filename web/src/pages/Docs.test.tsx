import { afterEach, describe, expect, it, vi } from 'vitest';
import { cleanup, render, screen, waitFor } from '@testing-library/react';
import { MemoryRouter, Route, Routes } from 'react-router-dom';
import { Docs } from './Docs';

describe('Docs page', () => {
    afterEach(() => {
        vi.restoreAllMocks();
        cleanup();
    });

    it('rewrites markdown links to router paths and sanitizes raw html', async () => {
        vi.spyOn(window, 'scrollTo').mockImplementation(() => {});
        vi.stubGlobal(
            'fetch',
            vi.fn().mockResolvedValue({
                ok: true,
                text: async () =>
                    '# Page\n\n[Stdlib](../stdlib/overview.md)\n\n<script>window.__xss = true;</script>',
            }),
        );

        const { container } = render(
            <MemoryRouter initialEntries={['/docs/features/projects']}>
                <Routes>
                    <Route path="/docs/*" element={<Docs />} />
                </Routes>
            </MemoryRouter>,
        );

        const link = await screen.findByRole('link', { name: 'Stdlib' });
        expect(link).toHaveAttribute('href', '/docs/stdlib/overview');

        await waitFor(() => {
            expect(container.querySelector('script')).toBeNull();
        });
    });
});
