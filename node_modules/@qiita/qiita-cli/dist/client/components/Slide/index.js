"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
exports.Slide = void 0;
const jsx_runtime_1 = require("@emotion/react/jsx-runtime");
const SlideViewer_1 = require("./SlideViewer");
const slide_pages_1 = require("./slide-pages");
const Slide = ({ renderedBody, title, }) => {
    const pages = (0, slide_pages_1.slidePages)({
        title,
        author: {
            urlName: "",
        },
        body: renderedBody,
    });
    return (0, jsx_runtime_1.jsx)(SlideViewer_1.SlideViewer, { pages: pages });
};
exports.Slide = Slide;
//# sourceMappingURL=index.js.map