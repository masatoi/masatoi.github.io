"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
exports.ItemsShow = void 0;
const jsx_runtime_1 = require("@emotion/react/jsx-runtime");
const react_1 = require("@emotion/react");
const react_2 = require("react");
const react_router_dom_1 = require("react-router-dom");
const qiita_cli_url_1 = require("../../../lib/qiita-cli-url");
const Article_1 = require("../../components/Article");
const ArticleInfo_1 = require("../../components/ArticleInfo");
const Header_1 = require("../../components/Header");
const HotReloadRoot_1 = require("../../components/HotReloadRoot");
const MaterialSymbol_1 = require("../../components/MaterialSymbol");
const SidebarContents_1 = require("../../components/SidebarContents");
const mixins_1 = require("../../lib/mixins");
const variables_1 = require("../../lib/variables");
const Contents_1 = require("../../templates/Contents");
const Main_1 = require("../../templates/Main");
const Sidebar_1 = require("../../templates/Sidebar");
const ItemsShow = () => {
    const { id } = (0, react_router_dom_1.useParams)();
    const [searchParams] = (0, react_router_dom_1.useSearchParams)();
    const basename = searchParams.get("basename");
    const [item, setItem] = (0, react_2.useState)(null);
    const [error, setError] = (0, react_2.useState)(null);
    const [errorFrontmatterMessages, setErrorFrontmatterMessages] = (0, react_2.useState)(null);
    const [isStateOpen, setIsStateOpen] = (0, react_2.useState)(false);
    const handleMobileOpen = () => {
        setIsStateOpen(true);
    };
    const handleMobileClose = () => {
        setIsStateOpen(false);
    };
    (0, HotReloadRoot_1.useHotReloadEffect)(() => {
        if (!id)
            return;
        const queryParams = basename ? { basename: basename } : undefined;
        const fetchURL = (0, qiita_cli_url_1.apiItemsShowPath)(id, queryParams);
        fetch(fetchURL).then((response) => {
            if (!response.ok) {
                if (response.status === 404) {
                    setError("ファイルが見つかりません");
                    setItem(null);
                }
                else {
                    response.json().then((data) => {
                        setError(null);
                        setErrorFrontmatterMessages(data.errorMessages);
                        setItem(null);
                    });
                }
            }
            else {
                response.json().then((data) => {
                    setItem(data);
                });
            }
        });
    }, [id, basename]);
    return ((0, jsx_runtime_1.jsxs)(Main_1.Main, { children: [(0, jsx_runtime_1.jsx)(Sidebar_1.Sidebar, { children: (0, jsx_runtime_1.jsx)(SidebarContents_1.SidebarContents, { isStateOpen: isStateOpen, handleMobileClose: handleMobileClose }) }), (0, jsx_runtime_1.jsx)(Contents_1.Contents, { children: id && item ? ((0, jsx_runtime_1.jsxs)(jsx_runtime_1.Fragment, { children: [(0, jsx_runtime_1.jsx)(Header_1.Header, { handleMobileOpen: handleMobileOpen, isItemPublishable: item.modified && item.error_messages.length === 0, isOlderThanRemote: item.is_older_than_remote, itemPath: item.item_path, id: id, basename: basename }), (0, jsx_runtime_1.jsx)("div", { css: contentsWrapperStyle, children: (0, jsx_runtime_1.jsxs)("div", { css: contentsContainerStyle, children: [(0, jsx_runtime_1.jsx)(ArticleInfo_1.ArticleInfo, { secret: item.secret, modified: item.modified, organizationUrlName: item.organization_url_name, published: item.published, errorMessages: item.error_messages, qiitaItemUrl: item.qiita_item_url, slide: item.slide, isOlderThanRemote: item.is_older_than_remote }), (0, jsx_runtime_1.jsx)("div", { css: articleWrapStyle, children: (0, jsx_runtime_1.jsx)(Article_1.Article, { renderedBody: item.rendered_body, tags: item.tags, title: item.title, slide: item.slide }) })] }) })] })) : error ? ((0, jsx_runtime_1.jsx)("p", { css: errorMessageStyle, children: error })) : errorFrontmatterMessages && errorFrontmatterMessages.length > 0 ? ((0, jsx_runtime_1.jsxs)("div", { css: errorContentsStyle, children: [(0, jsx_runtime_1.jsx)("p", { css: errorTitleStyle, children: "\u8A18\u4E8B\u306E\u8A2D\u5B9A\u306E\u5165\u529B\u5185\u5BB9\u306B\u8AA4\u308A\u304C\u3042\u308B\u305F\u3081\u3001\u30D7\u30EC\u30D3\u30E5\u30FC\u304C\u8868\u793A\u3067\u304D\u307E\u305B\u3093" }), errorFrontmatterMessages.map((errorMessage, index) => ((0, jsx_runtime_1.jsxs)("p", { css: errorStyle, children: [(0, jsx_runtime_1.jsx)(MaterialSymbol_1.MaterialSymbol, { fill: true, css: exclamationIconStyle, children: "error" }), (0, jsx_runtime_1.jsx)("div", { children: errorMessage })] }, `error-message-${index}`)))] })) : null })] }));
};
exports.ItemsShow = ItemsShow;
const contentsWrapperStyle = (0, react_1.css)({
    margin: `${(0, variables_1.getSpace)(2)}px ${(0, variables_1.getSpace)(2)}px 0`,
});
const contentsContainerStyle = (0, react_1.css)({
    backgroundColor: variables_1.Colors.gray0,
    borderRadius: 8,
    maxWidth: 820,
    margin: "0 auto",
    padding: (0, variables_1.getSpace)(3),
});
const articleWrapStyle = (0, react_1.css)({
    marginTop: (0, variables_1.getSpace)(3),
});
const errorMessageStyle = (0, react_1.css)({
    fontSize: variables_1.Typography.subhead2,
    padding: (0, variables_1.getSpace)(2),
    textAlign: "center",
});
const errorContentsStyle = (0, react_1.css)({
    backgroundColor: variables_1.Colors.red10,
    borderRadius: 8,
    fontSize: variables_1.Typography.body2,
    lineHeight: variables_1.LineHeight.bodyDense,
    margin: `${(0, variables_1.getSpace)(3)}px auto 0`,
    width: "fit-content",
    maxWidth: "calc(100% - 32px)",
    padding: `${(0, variables_1.getSpace)(2)}px ${(0, variables_1.getSpace)(3)}px`,
    ...mixins_1.viewport.S({
        padding: (0, variables_1.getSpace)(2),
    }),
});
const exclamationIconStyle = (0, react_1.css)({
    color: variables_1.Colors.red60,
    marginRight: (0, variables_1.getSpace)(1 / 2),
});
const errorTitleStyle = (0, react_1.css)({
    fontWeight: variables_1.Weight.bold,
});
const errorStyle = (0, react_1.css)({
    alignItems: "center",
    display: "flex",
    marginTop: (0, variables_1.getSpace)(3 / 2),
});
//# sourceMappingURL=show.js.map