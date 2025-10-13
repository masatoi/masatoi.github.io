"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
const jsx_runtime_1 = require("@emotion/react/jsx-runtime");
const client_1 = require("react-dom/client");
const App_1 = require("./components/App");
// Add this line to enable Emotion auto label
globalThis.EMOTION_RUNTIME_AUTO_LABEL = true;
const container = document.getElementById("app");
if (!container) {
    throw new Error("`#app` is not found");
}
const root = (0, client_1.createRoot)(container);
root.render((0, jsx_runtime_1.jsx)(App_1.App, {}));
//# sourceMappingURL=index.js.map