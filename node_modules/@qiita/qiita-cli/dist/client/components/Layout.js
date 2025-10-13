"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
exports.Layout = void 0;
const jsx_runtime_1 = require("@emotion/react/jsx-runtime");
const react_router_dom_1 = require("react-router-dom");
const Layout = () => {
    return ((0, jsx_runtime_1.jsxs)("div", { children: [(0, jsx_runtime_1.jsx)(react_router_dom_1.Outlet, {}), (0, jsx_runtime_1.jsx)(react_router_dom_1.ScrollRestoration, {})] }));
};
exports.Layout = Layout;
//# sourceMappingURL=Layout.js.map