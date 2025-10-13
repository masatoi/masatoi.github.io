"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
exports.Article = void 0;
const jsx_runtime_1 = require("@emotion/react/jsx-runtime");
const react_1 = require("@emotion/react");
const react_2 = require("react");
const variables_1 = require("../lib/variables");
const QiitaMarkdownHtmlBody_1 = require("./QiitaMarkdownHtmlBody");
const Slide_1 = require("./Slide");
const Article = ({ renderedBody, tags, title, slide }) => {
    const bodyRef = (0, react_2.useRef)(null);
    return ((0, jsx_runtime_1.jsxs)("article", { css: containerStyle, children: [(0, jsx_runtime_1.jsx)("h1", { css: titleStyle, children: title }), (0, jsx_runtime_1.jsx)("div", { css: tagListWrapStyle, children: (0, jsx_runtime_1.jsx)("ul", { css: tagListStyle, children: tags.map((tag, index) => ((0, jsx_runtime_1.jsxs)("li", { css: tagListItemStyle, children: [(0, jsx_runtime_1.jsx)("span", { children: tag }), index !== tags.length - 1 && (0, jsx_runtime_1.jsx)("span", { children: "," })] }, tag))) }) }), (0, jsx_runtime_1.jsxs)("div", { css: bodyStyle, className: "it-MdContent", children: [slide && (0, jsx_runtime_1.jsx)(Slide_1.Slide, { renderedBody: renderedBody, title: title }), (0, jsx_runtime_1.jsx)(QiitaMarkdownHtmlBody_1.QiitaMarkdownHtmlBody, { renderedBody: renderedBody, bodyRef: bodyRef })] })] }));
};
exports.Article = Article;
const containerStyle = (0, react_1.css)({
    wordBreak: "break-word",
});
const titleStyle = (0, react_1.css)({
    fontSize: variables_1.Typography.headline1,
    fontWeight: variables_1.Weight.bold,
    lineHeight: variables_1.LineHeight.headline,
});
const tagListWrapStyle = (0, react_1.css)({
    marginTop: (0, variables_1.getSpace)(1),
    alignItems: "center",
    display: "flex",
    gap: (0, variables_1.getSpace)(1),
});
const tagListStyle = (0, react_1.css)({
    display: "flex",
    gap: (0, variables_1.getSpace)(1),
});
const tagListItemStyle = (0, react_1.css)({
    backgroundColor: variables_1.Colors.surfaceVariant,
    borderRadius: 4,
    color: variables_1.Colors.mediumEmphasis,
    fontSize: variables_1.Typography.body2,
    padding: `0 ${(0, variables_1.getSpace)(3 / 4)}px`,
});
const bodyStyle = (0, react_1.css)({
    marginTop: (0, variables_1.getSpace)(6),
});
//# sourceMappingURL=Article.js.map