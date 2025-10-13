"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
exports.SidebarArticles = exports.SortType = void 0;
const jsx_runtime_1 = require("@emotion/react/jsx-runtime");
const react_1 = require("@emotion/react");
const react_2 = require("react");
const MaterialSymbol_1 = require("./MaterialSymbol");
const react_router_dom_1 = require("react-router-dom");
const mixins_1 = require("../lib/mixins");
const variables_1 = require("../lib/variables");
exports.SortType = {
    ByUpdatedAt: 1,
    Alphabetically: 2,
};
const SidebarArticles = ({ items, sortType, articleState }) => {
    const ArticleState = {
        Draft: "未投稿",
        Public: "投稿済み",
        Private: "限定共有記事",
    };
    const StorageName = {
        Draft: "openDraftArticlesState",
        Public: "openPublicArticlesState",
        Private: "openPrivateArticlesState",
    };
    const compare = {
        [exports.SortType.ByUpdatedAt]: (a, b) => {
            if (a.updated_at === "")
                return -1;
            if (b.updated_at === "")
                return 1;
            return b.updated_at.localeCompare(a.updated_at);
        },
        [exports.SortType.Alphabetically]: (a, b) => {
            return a.title.localeCompare(b.title);
        },
    };
    const [isDetailsOpen, setIsDetailsOpen] = (0, react_2.useState)(localStorage.getItem(StorageName[articleState]) === "true");
    const toggleAccordion = (event) => {
        event.preventDefault();
        setIsDetailsOpen((prev) => !prev);
    };
    (0, react_2.useEffect)(() => {
        localStorage.setItem(StorageName[articleState], isDetailsOpen.toString());
    }, [isDetailsOpen]);
    return ((0, jsx_runtime_1.jsxs)("details", { css: articleDetailsStyle, open: isDetailsOpen, children: [(0, jsx_runtime_1.jsxs)("summary", { css: articleSummaryStyle, onClick: toggleAccordion, children: [ArticleState[articleState], (0, jsx_runtime_1.jsx)("span", { css: articleSectionTitleCountStyle, children: items.length })] }), (0, jsx_runtime_1.jsx)("ul", { children: items.sort(compare[sortType]).map((item) => ((0, jsx_runtime_1.jsx)("li", { children: (0, jsx_runtime_1.jsxs)(react_router_dom_1.Link, { css: articlesListItemStyle, to: item.items_show_path, children: [(0, jsx_runtime_1.jsx)(MaterialSymbol_1.MaterialSymbol, { fill: item.modified && articleState !== "Draft", children: "note" }), (0, jsx_runtime_1.jsxs)("span", { css: articleListItemInnerStyle, children: [item.modified && articleState !== "Draft" && "(差分あり) ", item.title] })] }) }, item.items_show_path))) })] }));
};
exports.SidebarArticles = SidebarArticles;
const articleDetailsStyle = (0, react_1.css)({
    "& > summary::before": {
        fontFamily: "Material Symbols Outlined",
        content: "'expand_less'",
    },
    "&[open] > summary::before": {
        content: "'expand_more'",
    },
});
const articleSummaryStyle = (0, react_1.css)({
    alignItems: "center",
    backgroundColor: "transparent",
    color: variables_1.Colors.mediumEmphasis,
    cursor: "pointer",
    display: "flex",
    fontWeight: variables_1.Weight.bold,
    fontSize: variables_1.Typography.body2,
    gap: (0, variables_1.getSpace)(1),
    padding: `${(0, variables_1.getSpace)(1 / 2)}px ${(0, variables_1.getSpace)(2)}px`,
    width: "100%",
    boxSizing: "border-box",
    ...(0, mixins_1.pointerFine)({
        "&:hover": {
            backgroundColor: variables_1.Colors.gray5,
            cursor: "pointer",
            textDecoration: "none",
        },
    }),
    "&::-webkit-details-marker": {
        display: "none",
    },
});
const articleSectionTitleCountStyle = (0, react_1.css)({
    backgroundColor: variables_1.Colors.gray20,
    borderRadius: 4,
    fontSize: variables_1.Typography.body3,
    lineHeight: variables_1.LineHeight.bodyDense,
    padding: `0 ${(0, variables_1.getSpace)(1 / 2)}px`,
});
const articlesListItemStyle = (0, react_1.css)({
    alignItems: "center",
    backgroundColor: "transparent",
    color: variables_1.Colors.mediumEmphasis,
    display: "flex",
    fontSize: variables_1.Typography.body2,
    gap: (0, variables_1.getSpace)(1),
    lineHeight: variables_1.LineHeight.bodyDense,
    padding: `${(0, variables_1.getSpace)(3 / 4)}px ${(0, variables_1.getSpace)(5 / 2)}px ${(0, variables_1.getSpace)(3 / 4)}px ${(0, variables_1.getSpace)(3 / 2)}px`,
    whiteSpace: "nowrap",
    textOverflow: "ellipsis",
    ...(0, mixins_1.pointerFine)({
        "&:hover": {
            backgroundColor: variables_1.Colors.gray5,
            textDecoration: "none",
        },
    }),
});
const articleListItemInnerStyle = (0, react_1.css)({
    overflow: "hidden",
    textOverflow: "ellipsis",
    whiteSpace: "nowrap",
});
//# sourceMappingURL=SidebarArticles.js.map