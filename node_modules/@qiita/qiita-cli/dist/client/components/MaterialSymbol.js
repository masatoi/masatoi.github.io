"use strict";
var __importDefault = (this && this.__importDefault) || function (mod) {
    return (mod && mod.__esModule) ? mod : { "default": mod };
};
Object.defineProperty(exports, "__esModule", { value: true });
exports.MaterialSymbol = void 0;
const jsx_runtime_1 = require("@emotion/react/jsx-runtime");
const react_1 = require("@emotion/react");
const classnames_1 = __importDefault(require("classnames"));
const MaterialSymbol = ({ children, className, fill = false, size = 16, ariaLabel, title, }) => {
    return ((0, jsx_runtime_1.jsx)("span", { css: symbolStyle(fill, size), className: (0, classnames_1.default)("material-symbols-outlined", className), title: title, "aria-hidden": ariaLabel ? undefined : "true", "aria-label": ariaLabel || undefined, children: children }));
};
exports.MaterialSymbol = MaterialSymbol;
const symbolStyle = (fill, size) => (0, react_1.css)({
    display: "inline-block",
    flexShrink: 0,
    fontSize: size,
    fontVariationSettings: `'FILL' ${fill ? 1 : 0}, 'wght' 500, 'GRAD' 0, 'opsz' 24`,
    height: size,
    overflow: "hidden",
    width: size,
});
//# sourceMappingURL=MaterialSymbol.js.map