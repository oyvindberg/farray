import { defineConfig } from "vite";
import react from "@vitejs/plugin-react";

// SPA. Dev serves public/data/*.json live (re-run `npm run data` to refresh without a rebuild);
// `vite build` copies public/ into dist/ so prod ships the slimmed bench data + extracted snippets baked in.
export default defineConfig({
  plugins: [react()],
  base: "./",
  build: { outDir: "dist", chunkSizeWarningLimit: 1500 },
});
