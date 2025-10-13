"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
exports.App = void 0;
const jsx_runtime_1 = require("@emotion/react/jsx-runtime");
const react_1 = require("react");
const HotReloadRoot_1 = require("./HotReloadRoot");
const Router_1 = require("./Router");
const App = () => {
    return ((0, jsx_runtime_1.jsx)(react_1.StrictMode, { children: (0, jsx_runtime_1.jsx)(HotReloadRoot_1.HotReloadRoot, { children: (0, jsx_runtime_1.jsx)(Router_1.Router, {}) }) }));
};
exports.App = App;
//# sourceMappingURL=App.js.map