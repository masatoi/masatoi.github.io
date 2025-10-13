"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
exports.Sidebar = void 0;
const jsx_runtime_1 = require("@emotion/react/jsx-runtime");
const react_1 = require("@emotion/react");
const variables_1 = require("../lib/variables");
const Sidebar = ({ children }) => {
    return (0, jsx_runtime_1.jsx)("div", { css: sidebarStyle, children: children });
};
exports.Sidebar = Sidebar;
const sidebarStyle = (0, react_1.css)({
    backgroundColor: variables_1.Colors.gray0,
    gridArea: "sidebar",
    position: "sticky",
    zIndex: 1,
});
//# sourceMappingURL=Sidebar.js.map