"use strict";
var __importDefault = (this && this.__importDefault) || function (mod) {
    return (mod && mod.__esModule) ? mod : { "default": mod };
};
Object.defineProperty(exports, "__esModule", { value: true });
exports.SlideViewerDashboardNavigation = void 0;
const jsx_runtime_1 = require("@emotion/react/jsx-runtime");
const classnames_1 = __importDefault(require("classnames"));
const react_1 = require("react");
const MaterialSymbol_1 = require("../MaterialSymbol");
const SlideViewerDashboardNavigation = ({ currentPage, totalPage, onPrevious, onNext, }) => {
    const disablePrevious = currentPage === 1;
    const disableNext = currentPage === totalPage;
    return ((0, jsx_runtime_1.jsxs)(react_1.Fragment, { children: [(0, jsx_runtime_1.jsx)("button", { "aria-label": "\u524D\u306E\u30DA\u30FC\u30B8", className: (0, classnames_1.default)("slideMode-Dashboard_button", "slideMode-Dashboard_button--prev", {
                    "slideMode-Dashboard_button--clickable": !disablePrevious,
                }), disabled: disablePrevious, onClick: () => onPrevious(), children: (0, jsx_runtime_1.jsx)(MaterialSymbol_1.MaterialSymbol, { fill: true, size: 20, children: "arrow_back" }) }), (0, jsx_runtime_1.jsx)("button", { "aria-label": "\u6B21\u306E\u30DA\u30FC\u30B8", className: (0, classnames_1.default)("slideMode-Dashboard_button", "slideMode-Dashboard_button--next", {
                    "slideMode-Dashboard_button--clickable": !disableNext,
                }), disabled: disableNext, onClick: () => onNext(), children: (0, jsx_runtime_1.jsx)(MaterialSymbol_1.MaterialSymbol, { fill: true, size: 20, children: "arrow_forward" }) })] }));
};
exports.SlideViewerDashboardNavigation = SlideViewerDashboardNavigation;
//# sourceMappingURL=SlideViewerDashboardNavigation.js.map