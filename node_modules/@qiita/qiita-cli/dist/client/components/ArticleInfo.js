"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
exports.ArticleInfo = void 0;
const jsx_runtime_1 = require("@emotion/react/jsx-runtime");
const react_1 = require("@emotion/react");
const react_2 = require("react");
const variables_1 = require("../lib/variables");
const MaterialSymbol_1 = require("./MaterialSymbol");
const ArticleInfo = ({ secret, modified, organizationUrlName, published, errorMessages, qiitaItemUrl, slide, isOlderThanRemote, }) => {
    const [isOpen, setIsOpen] = (0, react_2.useState)(localStorage.getItem("openInfoState") === "true" ? true : false);
    const toggleAccordion = (event) => {
        event.preventDefault();
        setIsOpen((prev) => !prev);
    };
    (0, react_2.useEffect)(() => {
        localStorage.setItem("openInfoState", JSON.stringify(isOpen));
    }, [isOpen]);
    return ((0, jsx_runtime_1.jsxs)(jsx_runtime_1.Fragment, { children: [(0, jsx_runtime_1.jsxs)("details", { css: infoStyle, open: isOpen, children: [(0, jsx_runtime_1.jsx)("summary", { css: infoSummaryStyle, onClick: toggleAccordion, children: "\u8A18\u4E8B\u60C5\u5831" }), (0, jsx_runtime_1.jsx)(InfoItem, { title: "\u516C\u958B\u7BC4\u56F2", children: secret ? ((0, jsx_runtime_1.jsxs)(jsx_runtime_1.Fragment, { children: [(0, jsx_runtime_1.jsx)(MaterialSymbol_1.MaterialSymbol, { css: { color: variables_1.Colors.disabled }, size: 14, fill: true, children: "lock" }), " ", "\u9650\u5B9A\u5171\u6709"] })) : ((0, jsx_runtime_1.jsxs)(jsx_runtime_1.Fragment, { children: [(0, jsx_runtime_1.jsx)(MaterialSymbol_1.MaterialSymbol, { size: 14, css: { color: variables_1.Colors.disabled }, children: "public" }), " ", "\u5168\u4F53"] })) }), (0, jsx_runtime_1.jsx)(InfoItem, { title: "\u8A18\u4E8B\u306E\u72B6\u614B", children: published ? (qiitaItemUrl ? ((0, jsx_runtime_1.jsx)("a", { href: qiitaItemUrl, target: "_blank", rel: "noopener noreferrer", children: "\u6295\u7A3F\u6E08\u307F" })) : ("投稿済み")) : ("未投稿") }), (0, jsx_runtime_1.jsx)(InfoItem, { title: "\u5DEE\u5206", children: modified ? "あり" : "なし" }), (0, jsx_runtime_1.jsx)(InfoItem, { title: "Organization", children: organizationUrlName || "紐付けなし" }), (0, jsx_runtime_1.jsx)(InfoItem, { title: "\u30B9\u30E9\u30A4\u30C9\u30E2\u30FC\u30C9", children: slide ? "ON" : "OFF" })] }), isOlderThanRemote && ((0, jsx_runtime_1.jsx)("div", { css: errorContentsStyle, children: (0, jsx_runtime_1.jsxs)("p", { css: errorStyle, children: [(0, jsx_runtime_1.jsx)(MaterialSymbol_1.MaterialSymbol, { fill: true, css: exclamationIconStyle, children: "error" }), "この記事ファイルの内容は、Qiita上の記事より古い可能性があります。"] }) })), errorMessages.length > 0 && ((0, jsx_runtime_1.jsx)("div", { css: errorContentsStyle, children: errorMessages.map((errorMessage, index) => ((0, jsx_runtime_1.jsxs)("p", { css: errorStyle, children: [(0, jsx_runtime_1.jsx)(MaterialSymbol_1.MaterialSymbol, { fill: true, css: exclamationIconStyle, children: "error" }), errorMessage] }, `error-message-${index}`))) }))] }));
};
exports.ArticleInfo = ArticleInfo;
const infoStyle = (0, react_1.css)({
    backgroundColor: variables_1.Colors.gray10,
    borderRadius: 8,
    display: "flex",
    flexDirection: "column",
    padding: `${(0, variables_1.getSpace)(3 / 2)}px ${(0, variables_1.getSpace)(2)}px`,
    width: "100%",
    "& > summary::after": {
        fontFamily: "Material Symbols Outlined",
        content: "'expand_less'",
    },
    "&[open] > summary::after": {
        content: "'expand_more'",
    },
});
const infoSummaryStyle = (0, react_1.css)({
    alignItems: "center",
    display: "flex",
    cursor: "pointer",
    "&::-webkit-details-marker": {
        display: "none",
    },
});
const InfoItem = ({ children, title }) => {
    return ((0, jsx_runtime_1.jsxs)("div", { css: infoListStyle, children: [(0, jsx_runtime_1.jsx)("p", { css: titleStyle, children: title }), (0, jsx_runtime_1.jsx)("p", { css: bodyStyle, children: children })] }));
};
const infoListStyle = (0, react_1.css)({
    display: "grid",
    gridTemplateColumns: "100px minmax(0, 1fr)",
    gap: (0, variables_1.getSpace)(3 / 2),
    "& + &": {
        marginTop: (0, variables_1.getSpace)(1 / 2),
    },
});
const titleStyle = (0, react_1.css)({
    color: variables_1.Colors.disabled,
    fontSize: variables_1.Typography.body2,
    fontWeight: variables_1.Weight.bold,
});
const bodyStyle = (0, react_1.css)({
    display: "flex",
    alignItems: "center",
    gap: ` 0 ${(0, variables_1.getSpace)(1 / 2)}px`,
    fontSize: variables_1.Typography.body2,
    lineHeight: variables_1.LineHeight.bodyDense,
    wordBreak: "break-word",
});
const exclamationIconStyle = (0, react_1.css)({
    color: variables_1.Colors.yellow60,
});
const errorContentsStyle = (0, react_1.css)({
    marginTop: (0, variables_1.getSpace)(3),
});
const errorStyle = (0, react_1.css)({
    alignItems: "center",
    display: "flex",
    fontSize: variables_1.Typography.body2,
    lineHeight: variables_1.LineHeight.bodyDense,
    gap: (0, variables_1.getSpace)(1 / 2),
    "& + &": {
        marginTop: (0, variables_1.getSpace)(3 / 2),
    },
});
//# sourceMappingURL=ArticleInfo.js.map