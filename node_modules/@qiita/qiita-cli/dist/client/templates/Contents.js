"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
exports.Contents = void 0;
const jsx_runtime_1 = require("@emotion/react/jsx-runtime");
const react_1 = require("@emotion/react");
const Contents = ({ children }) => {
    return (0, jsx_runtime_1.jsx)("div", { css: contentsStyle, children: children });
};
exports.Contents = Contents;
const contentsStyle = (0, react_1.css)({
    gridArea: "contents",
    width: "100%",
});
//# sourceMappingURL=Contents.js.map