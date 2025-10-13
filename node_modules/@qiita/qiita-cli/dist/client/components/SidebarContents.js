"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
exports.SidebarContents = void 0;
const jsx_runtime_1 = require("@emotion/react/jsx-runtime");
const react_1 = require("@emotion/react");
const react_2 = require("react");
const react_router_dom_1 = require("react-router-dom");
const qiita_cli_url_1 = require("../../lib/qiita-cli-url");
const mixins_1 = require("../lib/mixins");
const variables_1 = require("../lib/variables");
const window_size_1 = require("../lib/window-size");
const HotReloadRoot_1 = require("./HotReloadRoot");
const Logo_1 = require("./Logo");
const MaterialSymbol_1 = require("./MaterialSymbol");
const SidebarArticles_1 = require("./SidebarArticles");
const Tooltip_1 = require("./Tooltip");
const SidebarContents = ({ isStateOpen, handleMobileClose }) => {
    const [items, setItems] = (0, react_2.useState)();
    const [isOpen, setIsOpen] = (0, react_2.useState)(localStorage.getItem("openSidebarState") === "true");
    const [sortType, setSortType] = (0, react_2.useState)(SidebarArticles_1.SortType.ByUpdatedAt);
    const imageUploadUrl = "https://qiita.com/settings/image_upload";
    (0, HotReloadRoot_1.useHotReloadEffect)(() => {
        fetch("/api/items").then((result) => {
            result.json().then((data) => {
                setItems(data);
            });
        });
    }, []);
    const sortTypeName = {
        [SidebarArticles_1.SortType.ByUpdatedAt]: "新着順",
        [SidebarArticles_1.SortType.Alphabetically]: "アルファベット順",
    };
    const handleNew = () => {
        fetch("/api/items", {
            method: "POST",
        }).then((response) => {
            response.json().then((data) => {
                const params = new URLSearchParams(location.search);
                params.set("basename", data.basename);
                const url = new URL("/items/show", location.href);
                url.search = params.toString();
                location.href = url.toString();
            });
        });
    };
    const handleOpen = () => {
        localStorage.setItem("openSidebarState", "true");
        setIsOpen(true);
    };
    const handleClose = () => {
        localStorage.setItem("openSidebarState", "false");
        setIsOpen(false);
    };
    const { currentWidth } = (0, window_size_1.useWindowSize)();
    const isMobile = currentWidth <= mixins_1.breakpoint.S;
    return isMobile ? ((0, jsx_runtime_1.jsxs)("div", { css: mobileSidebarStyle(isStateOpen), children: [(0, jsx_runtime_1.jsxs)("header", { css: headerStyle, children: [(0, jsx_runtime_1.jsx)("h1", { css: headerLogoWrapperStyle, children: (0, jsx_runtime_1.jsx)(react_router_dom_1.Link, { to: (0, qiita_cli_url_1.itemsIndexPath)(), children: (0, jsx_runtime_1.jsx)(Logo_1.QiitaPreviewLogo, { css: headerLogoStyle }) }) }), (0, jsx_runtime_1.jsx)("button", { "aria-label": "\u30B5\u30A4\u30C9\u30D0\u30FC\u3092\u9589\u3058\u308B", onClick: handleMobileClose, css: mobileSidebarButtonStyle, children: (0, jsx_runtime_1.jsx)(MaterialSymbol_1.MaterialSymbol, { size: 24, children: "close" }) })] }), (0, jsx_runtime_1.jsxs)("nav", { css: articlesContainerStyle, children: [(0, jsx_runtime_1.jsxs)("ul", { children: [(0, jsx_runtime_1.jsx)("li", { children: (0, jsx_runtime_1.jsxs)("a", { href: imageUploadUrl, target: "_blank", rel: "noopener noreferrer", css: articlesListItemStyle, children: [(0, jsx_runtime_1.jsx)(MaterialSymbol_1.MaterialSymbol, { css: { color: variables_1.Colors.disabled }, children: "panorama" }), (0, jsx_runtime_1.jsxs)("span", { css: articleListItemImageUploadStyle, children: ["\u753B\u50CF\u3092\u30A2\u30C3\u30D7\u30ED\u30FC\u30C9\u3059\u308B", (0, jsx_runtime_1.jsx)(MaterialSymbol_1.MaterialSymbol, { size: 14, css: { color: variables_1.Colors.disabled }, children: "open_in_new" })] })] }) }), (0, jsx_runtime_1.jsx)("li", { children: (0, jsx_runtime_1.jsxs)("button", { css: articlesListItemStyle, onClick: handleNew, children: [(0, jsx_runtime_1.jsx)(MaterialSymbol_1.MaterialSymbol, { css: { color: variables_1.Colors.disabled }, children: "add" }), "\u65B0\u898F\u8A18\u4E8B\u4F5C\u6210"] }) })] }), (0, jsx_runtime_1.jsxs)("div", { css: articlesStyle, children: [(0, jsx_runtime_1.jsxs)("div", { css: articlesHeaderStyle, children: [(0, jsx_runtime_1.jsx)("h1", { css: articlesHeaderTitleStyle, children: "Articles" }), (0, jsx_runtime_1.jsxs)("div", { css: articlesHeaderSortStyle, children: [(0, jsx_runtime_1.jsx)(MaterialSymbol_1.MaterialSymbol, { css: articlesHeaderSortIconStyle, children: "sort" }), (0, jsx_runtime_1.jsx)("select", { css: articlesHeaderSortButtonStyle, onChange: (e) => setSortType(parseInt(e.currentTarget.value)), children: Object.values(SidebarArticles_1.SortType).map((sortType) => ((0, jsx_runtime_1.jsx)("option", { value: sortType, children: sortTypeName[sortType] }, `sortType-${sortType}`))) })] })] }), (0, jsx_runtime_1.jsx)("div", { children: items && ((0, jsx_runtime_1.jsxs)(jsx_runtime_1.Fragment, { children: [(0, jsx_runtime_1.jsx)(SidebarArticles_1.SidebarArticles, { items: items.draft, sortType: sortType, articleState: "Draft" }), (0, jsx_runtime_1.jsx)(SidebarArticles_1.SidebarArticles, { items: items.public, sortType: sortType, articleState: "Public" }), (0, jsx_runtime_1.jsx)(SidebarArticles_1.SidebarArticles, { items: items.private, sortType: sortType, articleState: "Private" })] })) })] })] }), (0, jsx_runtime_1.jsxs)("footer", { css: articleFooterStyle, children: [(0, jsx_runtime_1.jsx)("a", { css: articleFooterTitleStyle, href: "https://qiita.com", target: "_blank", rel: "noopener noreferrer", children: (0, jsx_runtime_1.jsx)(Logo_1.QiitaLogo, { css: { height: 16 } }) }), (0, jsx_runtime_1.jsxs)("ul", { css: articleFooterListStyle, children: [(0, jsx_runtime_1.jsx)("li", { children: (0, jsx_runtime_1.jsxs)(react_router_dom_1.Link, { css: articlesListItemStyle, to: "/", children: [(0, jsx_runtime_1.jsx)(MaterialSymbol_1.MaterialSymbol, { css: { color: variables_1.Colors.disabled }, children: "book" }), "Readme"] }) }), (0, jsx_runtime_1.jsx)("li", { children: (0, jsx_runtime_1.jsxs)("a", { css: articlesListItemStyle, href: "https://help.qiita.com/ja/articles/qiita-article-guideline", target: "_blank", rel: "noopener noreferrer", children: [(0, jsx_runtime_1.jsx)(MaterialSymbol_1.MaterialSymbol, { css: { color: variables_1.Colors.disabled }, children: "lightbulb" }), "\u826F\u3044\u8A18\u4E8B\u3092\u66F8\u304F\u305F\u3081\u306E\u30AC\u30A4\u30C9\u30E9\u30A4\u30F3"] }) }), (0, jsx_runtime_1.jsx)("li", { children: (0, jsx_runtime_1.jsxs)("a", { css: articlesListItemStyle, href: "https://qiita.com/Qiita/items/c686397e4a0f4f11683d", target: "_blank", rel: "noopener noreferrer", children: [(0, jsx_runtime_1.jsx)(MaterialSymbol_1.MaterialSymbol, { css: { color: variables_1.Colors.disabled }, children: "help" }), "Markdown\u8A18\u6CD5\u30C1\u30FC\u30C8\u30B7\u30FC\u30C8 - Qiita"] }) })] }), (0, jsx_runtime_1.jsxs)("ul", { css: [articleFooterListStyle, { padding: `0 ${(0, variables_1.getSpace)(2)}px` }], children: [(0, jsx_runtime_1.jsx)("li", { children: (0, jsx_runtime_1.jsxs)("a", { css: articleFooterListItemStyle, href: "https://help.qiita.com/ja/articles/qiita-community-guideline", target: "_blank", rel: "noopener noreferrer", children: ["\u30B3\u30DF\u30E5\u30CB\u30C6\u30A3\u30AC\u30A4\u30C9\u30E9\u30A4\u30F3", (0, jsx_runtime_1.jsx)(MaterialSymbol_1.MaterialSymbol, { css: { color: variables_1.Colors.disabled }, children: "open_in_new" })] }) }), (0, jsx_runtime_1.jsx)("li", { children: (0, jsx_runtime_1.jsxs)("a", { css: articleFooterListItemStyle, href: "https://qiita.com/terms", target: "_blank", rel: "noopener noreferrer", children: ["\u5229\u7528\u898F\u7D04", (0, jsx_runtime_1.jsx)(MaterialSymbol_1.MaterialSymbol, { css: { color: variables_1.Colors.disabled }, children: "open_in_new" })] }) }), (0, jsx_runtime_1.jsx)("li", { children: (0, jsx_runtime_1.jsxs)("a", { css: articleFooterListItemStyle, href: "https://qiita.com/privacy", target: "_blank", rel: "noopener noreferrer", children: ["\u30D7\u30E9\u30A4\u30D0\u30B7\u30FC\u30DD\u30EA\u30B7\u30FC", (0, jsx_runtime_1.jsx)(MaterialSymbol_1.MaterialSymbol, { css: { color: variables_1.Colors.disabled }, children: "open_in_new" })] }) })] })] })] })) : isOpen ? ((0, jsx_runtime_1.jsxs)("div", { css: sidebarStyle, children: [(0, jsx_runtime_1.jsxs)("header", { css: headerStyle, children: [(0, jsx_runtime_1.jsx)("h1", { css: headerLogoWrapperStyle, children: (0, jsx_runtime_1.jsx)(react_router_dom_1.Link, { to: (0, qiita_cli_url_1.itemsIndexPath)(), children: (0, jsx_runtime_1.jsx)(Logo_1.QiitaPreviewLogo, { css: headerLogoStyle }) }) }), (0, jsx_runtime_1.jsx)(Tooltip_1.Tooltip, { message: "サイドバーを閉じる", vertical: "bottom", horizontal: "left", offset: 4, children: (0, jsx_runtime_1.jsx)("button", { "aria-label": "\u30B5\u30A4\u30C9\u30D0\u30FC\u3092\u9589\u3058\u308B", css: toggleButtonStyle, onClick: handleClose, children: (0, jsx_runtime_1.jsx)(MaterialSymbol_1.MaterialSymbol, { size: 24, children: "chevron_left" }) }) })] }), (0, jsx_runtime_1.jsxs)("nav", { css: articlesContainerStyle, children: [(0, jsx_runtime_1.jsxs)("ul", { children: [(0, jsx_runtime_1.jsx)("li", { children: (0, jsx_runtime_1.jsxs)("a", { href: imageUploadUrl, target: "_blank", rel: "noopener noreferrer", css: articlesListItemStyle, children: [(0, jsx_runtime_1.jsx)(MaterialSymbol_1.MaterialSymbol, { css: { color: variables_1.Colors.disabled }, children: "panorama" }), (0, jsx_runtime_1.jsxs)("span", { css: articleListItemImageUploadStyle, children: ["\u753B\u50CF\u3092\u30A2\u30C3\u30D7\u30ED\u30FC\u30C9\u3059\u308B", (0, jsx_runtime_1.jsx)(MaterialSymbol_1.MaterialSymbol, { size: 14, css: { color: variables_1.Colors.disabled }, children: "open_in_new" })] })] }) }), (0, jsx_runtime_1.jsx)("li", { children: (0, jsx_runtime_1.jsxs)("button", { css: articlesListItemStyle, onClick: handleNew, children: [(0, jsx_runtime_1.jsx)(MaterialSymbol_1.MaterialSymbol, { css: { color: variables_1.Colors.disabled }, children: "add" }), "\u65B0\u898F\u8A18\u4E8B\u4F5C\u6210"] }) })] }), (0, jsx_runtime_1.jsxs)("div", { css: articlesStyle, children: [(0, jsx_runtime_1.jsxs)("div", { css: articlesHeaderStyle, children: [(0, jsx_runtime_1.jsx)("h1", { css: articlesHeaderTitleStyle, children: "Articles" }), (0, jsx_runtime_1.jsxs)("div", { css: articlesHeaderSortStyle, children: [(0, jsx_runtime_1.jsx)(MaterialSymbol_1.MaterialSymbol, { css: articlesHeaderSortIconStyle, children: "sort" }), (0, jsx_runtime_1.jsx)("select", { css: articlesHeaderSortButtonStyle, onChange: (e) => setSortType(parseInt(e.currentTarget.value)), children: Object.values(SidebarArticles_1.SortType).map((sortType) => ((0, jsx_runtime_1.jsx)("option", { value: sortType, children: sortTypeName[sortType] }, `sortType-${sortType}`))) })] })] }), (0, jsx_runtime_1.jsx)("div", { children: items && ((0, jsx_runtime_1.jsxs)(jsx_runtime_1.Fragment, { children: [(0, jsx_runtime_1.jsx)(SidebarArticles_1.SidebarArticles, { items: items.draft, sortType: sortType, articleState: "Draft" }), (0, jsx_runtime_1.jsx)(SidebarArticles_1.SidebarArticles, { items: items.public, sortType: sortType, articleState: "Public" }), (0, jsx_runtime_1.jsx)(SidebarArticles_1.SidebarArticles, { items: items.private, sortType: sortType, articleState: "Private" })] })) })] })] }), (0, jsx_runtime_1.jsxs)("footer", { css: articleFooterStyle, children: [(0, jsx_runtime_1.jsx)("a", { css: articleFooterTitleStyle, href: "https://qiita.com", target: "_blank", rel: "noopener noreferrer", children: (0, jsx_runtime_1.jsx)(Logo_1.QiitaLogo, { css: { height: 16 } }) }), (0, jsx_runtime_1.jsxs)("ul", { css: articleFooterListStyle, children: [(0, jsx_runtime_1.jsx)("li", { children: (0, jsx_runtime_1.jsxs)(react_router_dom_1.Link, { css: articlesListItemStyle, to: "/", children: [(0, jsx_runtime_1.jsx)(MaterialSymbol_1.MaterialSymbol, { css: { color: variables_1.Colors.disabled }, children: "book" }), "Readme"] }) }), (0, jsx_runtime_1.jsx)("li", { children: (0, jsx_runtime_1.jsxs)("a", { css: articlesListItemStyle, href: "https://help.qiita.com/ja/articles/qiita-article-guideline", target: "_blank", rel: "noopener noreferrer", children: [(0, jsx_runtime_1.jsx)(MaterialSymbol_1.MaterialSymbol, { css: { color: variables_1.Colors.disabled }, children: "lightbulb" }), "\u826F\u3044\u8A18\u4E8B\u3092\u66F8\u304F\u305F\u3081\u306E\u30AC\u30A4\u30C9\u30E9\u30A4\u30F3"] }) }), (0, jsx_runtime_1.jsx)("li", { children: (0, jsx_runtime_1.jsxs)("a", { css: articlesListItemStyle, href: "https://qiita.com/Qiita/items/c686397e4a0f4f11683d", target: "_blank", rel: "noopener noreferrer", children: [(0, jsx_runtime_1.jsx)(MaterialSymbol_1.MaterialSymbol, { css: { color: variables_1.Colors.disabled }, children: "help" }), "Markdown\u8A18\u6CD5\u30C1\u30FC\u30C8\u30B7\u30FC\u30C8 - Qiita"] }) })] }), (0, jsx_runtime_1.jsxs)("ul", { css: [articleFooterListStyle, { padding: `0 ${(0, variables_1.getSpace)(2)}px` }], children: [(0, jsx_runtime_1.jsx)("li", { children: (0, jsx_runtime_1.jsxs)("a", { css: articleFooterListItemStyle, href: "https://help.qiita.com/ja/articles/qiita-community-guideline", target: "_blank", rel: "noopener noreferrer", children: ["\u30B3\u30DF\u30E5\u30CB\u30C6\u30A3\u30AC\u30A4\u30C9\u30E9\u30A4\u30F3", (0, jsx_runtime_1.jsx)(MaterialSymbol_1.MaterialSymbol, { css: { color: variables_1.Colors.disabled }, children: "open_in_new" })] }) }), (0, jsx_runtime_1.jsx)("li", { children: (0, jsx_runtime_1.jsxs)("a", { css: articleFooterListItemStyle, href: "https://qiita.com/terms", target: "_blank", rel: "noopener noreferrer", children: ["\u5229\u7528\u898F\u7D04", (0, jsx_runtime_1.jsx)(MaterialSymbol_1.MaterialSymbol, { css: { color: variables_1.Colors.disabled }, children: "open_in_new" })] }) }), (0, jsx_runtime_1.jsx)("li", { children: (0, jsx_runtime_1.jsxs)("a", { css: articleFooterListItemStyle, href: "https://qiita.com/privacy", target: "_blank", rel: "noopener noreferrer", children: ["\u30D7\u30E9\u30A4\u30D0\u30B7\u30FC\u30DD\u30EA\u30B7\u30FC", (0, jsx_runtime_1.jsx)(MaterialSymbol_1.MaterialSymbol, { css: { color: variables_1.Colors.disabled }, children: "open_in_new" })] }) })] })] })] })) : ((0, jsx_runtime_1.jsxs)("div", { css: closedSidebarStyle, children: [(0, jsx_runtime_1.jsxs)("div", { css: closedSidebarHeaderStyle, children: [(0, jsx_runtime_1.jsx)(Tooltip_1.Tooltip, { message: "サイドバーを開く", vertical: "middle", horizontal: "right", offset: -4, children: (0, jsx_runtime_1.jsx)("button", { "aria-label": "\u30B5\u30A4\u30C9\u30D0\u30FC\u3092\u958B\u304F", css: toggleButtonStyle, onClick: handleOpen, children: (0, jsx_runtime_1.jsx)(MaterialSymbol_1.MaterialSymbol, { size: 24, children: "chevron_right" }) }) }), (0, jsx_runtime_1.jsxs)("ul", { children: [(0, jsx_runtime_1.jsx)("li", { children: (0, jsx_runtime_1.jsx)(Tooltip_1.Tooltip, { message: "画像をアップロードする", vertical: "middle", horizontal: "right", offset: -4, children: (0, jsx_runtime_1.jsx)("a", { "aria-label": "\u753B\u50CF\u3092\u30A2\u30C3\u30D7\u30ED\u30FC\u30C9\u3059\u308B", href: imageUploadUrl, target: "_blank", rel: "noopener noreferrer", css: closeSidebarLinkStyle, children: (0, jsx_runtime_1.jsx)(MaterialSymbol_1.MaterialSymbol, { size: 24, children: "panorama" }) }) }) }), (0, jsx_runtime_1.jsx)("li", { children: (0, jsx_runtime_1.jsx)(Tooltip_1.Tooltip, { message: "新規記事を作成する", vertical: "middle", horizontal: "right", offset: -4, children: (0, jsx_runtime_1.jsx)("button", { "aria-label": "\u65B0\u898F\u8A18\u4E8B\u3092\u4F5C\u6210\u3059\u308B", css: closeSidebarLinkStyle, onClick: handleNew, children: (0, jsx_runtime_1.jsx)(MaterialSymbol_1.MaterialSymbol, { size: 24, children: "note_add" }) }) }) })] })] }), (0, jsx_runtime_1.jsxs)("ul", { children: [(0, jsx_runtime_1.jsx)("li", { children: (0, jsx_runtime_1.jsx)(Tooltip_1.Tooltip, { message: "Readme", vertical: "middle", horizontal: "right", offset: -4, children: (0, jsx_runtime_1.jsx)(react_router_dom_1.Link, { "aria-label": "Readme", css: closeSidebarFooterLinkStyle, to: "/", children: (0, jsx_runtime_1.jsx)(MaterialSymbol_1.MaterialSymbol, { children: "book" }) }) }) }), (0, jsx_runtime_1.jsx)("li", { children: (0, jsx_runtime_1.jsx)(Tooltip_1.Tooltip, { message: "良い記事を書くためのガイドライン", vertical: "middle", horizontal: "right", offset: -4, children: (0, jsx_runtime_1.jsx)("a", { "aria-label": "\u826F\u3044\u8A18\u4E8B\u3092\u66F8\u304F\u305F\u3081\u306E\u30AC\u30A4\u30C9\u30E9\u30A4\u30F3", css: closeSidebarFooterLinkStyle, href: "https://help.qiita.com/ja/articles/qiita-article-guideline", target: "_blank", rel: "noopener noreferrer", children: (0, jsx_runtime_1.jsx)(MaterialSymbol_1.MaterialSymbol, { children: "lightbulb" }) }) }) }), (0, jsx_runtime_1.jsx)("li", { children: (0, jsx_runtime_1.jsx)(Tooltip_1.Tooltip, { message: "Markdown記法チートシート", vertical: "middle", horizontal: "right", offset: -4, children: (0, jsx_runtime_1.jsx)("a", { "aria-label": "Markdown\u8A18\u6CD5\u30C1\u30FC\u30C8\u30B7\u30FC\u30C8 - Qiita", css: closeSidebarFooterLinkStyle, href: "https://qiita.com/Qiita/items/c686397e4a0f4f11683d", target: "_blank", rel: "noopener noreferrer", children: (0, jsx_runtime_1.jsx)(MaterialSymbol_1.MaterialSymbol, { children: "help" }) }) }) })] })] }));
};
exports.SidebarContents = SidebarContents;
const sidebarStyle = (0, react_1.css)({
    height: "100vh",
    width: 360,
    padding: `${(0, variables_1.getSpace)(2)}px 0`,
    position: "sticky",
    top: 0,
    overflow: "auto",
    ...mixins_1.viewport.S({
        display: "none",
    }),
});
const mobileSidebarStyle = (isStateOpen) => (0, react_1.css)({
    backgroundColor: variables_1.Colors.gray0,
    display: isStateOpen ? "block" : "none",
    height: "100vh",
    overflowY: "auto",
    padding: `${(0, variables_1.getSpace)(1)}px 0`,
    position: "fixed",
    top: 0,
    width: "100%",
    zIndex: 10,
});
const headerStyle = (0, react_1.css)({
    alignItems: "center",
    fontSize: variables_1.Typography.subhead1,
    fontWeight: variables_1.Weight.bold,
    padding: `0 ${(0, variables_1.getSpace)(2)}px`,
    display: "flex",
    justifyContent: "space-between",
    gap: (0, variables_1.getSpace)(2),
});
const headerLogoWrapperStyle = (0, react_1.css)({
    display: "flex",
});
const headerLogoStyle = (0, react_1.css)({
    height: 24,
});
const articlesContainerStyle = (0, react_1.css)({
    marginTop: (0, variables_1.getSpace)(2),
});
const articlesStyle = (0, react_1.css)({
    display: "flex",
    flexDirection: "column",
    gap: (0, variables_1.getSpace)(1),
    marginTop: (0, variables_1.getSpace)(2),
});
const articlesHeaderStyle = (0, react_1.css)({
    alignItems: "center",
    display: "flex",
    gap: (0, variables_1.getSpace)(2),
    padding: `0 ${(0, variables_1.getSpace)(2)}px`,
});
const articlesHeaderTitleStyle = (0, react_1.css)({
    fontWeight: variables_1.Weight.bold,
});
const articlesHeaderSortStyle = (0, react_1.css)({
    color: variables_1.Colors.mediumEmphasis,
    position: "relative",
});
const articlesHeaderSortIconStyle = (0, react_1.css)({
    position: "absolute",
    top: "50%",
    transform: "translateY(-50%)",
});
const articlesHeaderSortButtonStyle = (0, react_1.css)({
    background: "transparent",
    color: variables_1.Colors.mediumEmphasis,
    cursor: "pointer",
    fontSize: variables_1.Typography.body2,
    lineHeight: 1.8,
    paddingLeft: (0, variables_1.getSpace)(5 / 2),
    zIndex: 1,
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
    width: "100%",
    ...(0, mixins_1.pointerFine)({
        "&:hover": {
            backgroundColor: variables_1.Colors.gray5,
            textDecoration: "none",
        },
    }),
});
const articleListItemImageUploadStyle = (0, react_1.css)({
    alignItems: "center",
    display: "flex",
});
const toggleButtonStyle = (0, react_1.css)({
    alignItems: "center",
    backgroundColor: variables_1.Colors.onBackground,
    border: `solid 2px ${variables_1.Colors.gray30}`,
    borderRadius: 4,
    display: "flex",
    justifyContent: "center",
    padding: (0, variables_1.getSpace)(1 / 2),
    "&:hover": {
        backgroundColor: variables_1.Colors.gray20,
    },
});
const articleFooterStyle = (0, react_1.css)({
    borderTop: `solid 1px ${variables_1.Colors.divider}`,
    marginTop: (0, variables_1.getSpace)(3),
    paddingTop: (0, variables_1.getSpace)(2),
});
const articleFooterTitleStyle = (0, react_1.css)({
    display: "flex",
    padding: `0 ${(0, variables_1.getSpace)(2)}px`,
});
const articleFooterListStyle = (0, react_1.css)({
    marginTop: (0, variables_1.getSpace)(1),
});
const articleFooterListItemStyle = (0, react_1.css)({
    alignItems: "center",
    display: "flex",
    gap: (0, variables_1.getSpace)(1),
    color: variables_1.Colors.mediumEmphasis,
    fontSize: variables_1.Typography.body3,
});
const closedSidebarStyle = (0, react_1.css)({
    alignItems: "center",
    backgroundColor: variables_1.Colors.gray0,
    display: "flex",
    flexDirection: "column",
    height: "100vh",
    justifyContent: "space-between",
    maxWidth: 48,
    padding: `${(0, variables_1.getSpace)(1)}px ${(0, variables_1.getSpace)(1 / 2)}px ${(0, variables_1.getSpace)(2)}px`,
    position: "sticky",
    top: 0,
    ...mixins_1.viewport.S({
        display: "none",
    }),
});
const closedSidebarHeaderStyle = (0, react_1.css)({
    alignItems: "center",
    display: "flex",
    flexDirection: "column",
    gap: (0, variables_1.getSpace)(2),
});
const closeSidebarLinkStyle = (0, react_1.css)({
    backgroundColor: variables_1.Colors.gray0,
    color: variables_1.Colors.disabled,
    display: "flex",
    padding: `${(0, variables_1.getSpace)(1 / 2)}px ${(0, variables_1.getSpace)(1)}px`,
});
const closeSidebarFooterLinkStyle = (0, react_1.css)({
    color: variables_1.Colors.disabled,
    display: "flex",
    padding: `${(0, variables_1.getSpace)(1)}px ${(0, variables_1.getSpace)(3 / 2)}px`,
});
const mobileSidebarButtonStyle = (0, react_1.css)({
    backgroundColor: "inherit",
    color: variables_1.Colors.mediumEmphasis,
    display: "flex",
    padding: (0, variables_1.getSpace)(5 / 4),
});
//# sourceMappingURL=SidebarContents.js.map