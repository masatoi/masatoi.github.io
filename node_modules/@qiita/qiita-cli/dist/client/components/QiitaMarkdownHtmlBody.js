"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
exports.QiitaMarkdownHtmlBody = void 0;
const jsx_runtime_1 = require("@emotion/react/jsx-runtime");
const react_1 = require("react");
const embed_init_scripts_1 = require("../lib/embed-init-scripts");
const QiitaMarkdownHtmlBody = ({ renderedBody, bodyRef, }) => {
    const [isRendered, setIsRendered] = (0, react_1.useState)(false);
    (0, react_1.useEffect)(() => {
        setIsRendered(true);
    }, []);
    (0, react_1.useEffect)(() => {
        if (isRendered && bodyRef.current) {
            (0, embed_init_scripts_1.executeScriptTagsInElement)(bodyRef.current);
            (0, embed_init_scripts_1.applyMathJax)(bodyRef.current);
        }
    }, [isRendered, bodyRef, renderedBody]);
    return ((0, jsx_runtime_1.jsx)("div", { dangerouslySetInnerHTML: { __html: renderedBody }, ref: bodyRef }));
};
exports.QiitaMarkdownHtmlBody = QiitaMarkdownHtmlBody;
//# sourceMappingURL=QiitaMarkdownHtmlBody.js.map