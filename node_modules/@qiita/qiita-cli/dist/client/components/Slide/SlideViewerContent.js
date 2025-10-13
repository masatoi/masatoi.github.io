"use strict";
var __importDefault = (this && this.__importDefault) || function (mod) {
    return (mod && mod.__esModule) ? mod : { "default": mod };
};
Object.defineProperty(exports, "__esModule", { value: true });
exports.SlideViewerContent = void 0;
const jsx_runtime_1 = require("@emotion/react/jsx-runtime");
const classnames_1 = __importDefault(require("classnames"));
const react_1 = require("react");
const QiitaMarkdownHtmlBody_1 = require("../QiitaMarkdownHtmlBody");
const SlideViewerContent = ({ pages, currentPageIndex, onPrevious, onNext, contentRef, }) => {
    const handleClickScreen = (0, react_1.useCallback)((event) => {
        const clickedElement = event.target;
        // If a viewer clicks <img> or <a> element, we don't navigate.
        if (clickedElement.tagName === "IMG" || clickedElement.tagName === "A") {
            return;
        }
        // We want to use getBoundingClientRect because it always returns
        // the actual rendered element dimensions, even if there are CSS
        // transformations applied to it.
        const rect = event.currentTarget.getBoundingClientRect();
        // Should we transition to the next or the previous slide?
        if (event.clientX - rect.left > rect.width / 2) {
            onNext();
        }
        else {
            onPrevious();
        }
    }, [onPrevious, onNext]);
    return ((0, jsx_runtime_1.jsx)("div", { className: (0, classnames_1.default)("slideMode-Viewer_content", "markdownContent", {
            "slideMode-Viewer_content--firstSlide": currentPageIndex === 0,
        }), onClick: handleClickScreen, children: (0, jsx_runtime_1.jsx)(QiitaMarkdownHtmlBody_1.QiitaMarkdownHtmlBody, { renderedBody: pages[currentPageIndex] || "", bodyRef: contentRef }) }));
};
exports.SlideViewerContent = SlideViewerContent;
//# sourceMappingURL=SlideViewerContent.js.map