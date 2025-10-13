"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
exports.ItemsIndex = void 0;
const jsx_runtime_1 = require("@emotion/react/jsx-runtime");
const react_1 = require("@emotion/react");
const react_2 = require("react");
const qiita_cli_url_1 = require("../../../lib/qiita-cli-url");
const Header_1 = require("../../components/Header");
const HotReloadRoot_1 = require("../../components/HotReloadRoot");
const SidebarContents_1 = require("../../components/SidebarContents");
const mixins_1 = require("../../lib/mixins");
const variables_1 = require("../../lib/variables");
const window_size_1 = require("../../lib/window-size");
const Contents_1 = require("../../templates/Contents");
const Main_1 = require("../../templates/Main");
const Sidebar_1 = require("../../templates/Sidebar");
const ItemsIndex = () => {
    const [isStateOpen, setIsStateOpen] = (0, react_2.useState)(false);
    const handleMobileOpen = () => {
        setIsStateOpen(true);
    };
    const handleMobileClose = () => {
        setIsStateOpen(false);
    };
    const { currentWidth } = (0, window_size_1.useWindowSize)();
    const mobileSize = currentWidth <= mixins_1.breakpoint.S;
    const [readme, setReadme] = (0, react_2.useState)(null);
    const [error, setError] = (0, react_2.useState)(null);
    (0, HotReloadRoot_1.useHotReloadEffect)(() => {
        const fetchURL = (0, qiita_cli_url_1.apiReadmeShowPath)();
        fetch(fetchURL).then((response) => {
            if (!response.ok) {
                if (response.status === 404) {
                    setError("ファイルが見つかりません");
                }
                else {
                    setError("不明なエラーが発生しました");
                }
            }
            else {
                response.json().then((data) => {
                    setReadme(data);
                });
            }
        });
    }, []);
    return ((0, jsx_runtime_1.jsxs)(Main_1.Main, { children: [(0, jsx_runtime_1.jsx)(Sidebar_1.Sidebar, { children: (0, jsx_runtime_1.jsx)(SidebarContents_1.SidebarContents, { isStateOpen: isStateOpen, handleMobileClose: handleMobileClose }) }), (0, jsx_runtime_1.jsxs)(Contents_1.Contents, { children: [mobileSize && (0, jsx_runtime_1.jsx)(Header_1.HeaderIndex, { handleMobileOpen: handleMobileOpen }), readme ? ((0, jsx_runtime_1.jsx)("div", { css: contentsWrapperStyle, children: (0, jsx_runtime_1.jsx)("div", { css: contentsContainerStyle, children: (0, jsx_runtime_1.jsx)("div", { className: "it-MdContent", children: (0, jsx_runtime_1.jsx)("div", { dangerouslySetInnerHTML: { __html: readme.renderedBody } }) }) }) })) : error ? ((0, jsx_runtime_1.jsx)("p", { css: errorMessageStyle, children: error })) : null] })] }));
};
exports.ItemsIndex = ItemsIndex;
const contentsWrapperStyle = (0, react_1.css)({
    margin: `${(0, variables_1.getSpace)(2)}px ${(0, variables_1.getSpace)(2)}px 0`,
    ...mixins_1.viewport.S({
        margin: 0,
    }),
});
const contentsContainerStyle = (0, react_1.css)({
    backgroundColor: variables_1.Colors.gray0,
    borderRadius: 8,
    maxWidth: 820,
    margin: "0 auto",
    padding: (0, variables_1.getSpace)(3),
});
const errorMessageStyle = (0, react_1.css)({
    fontSize: variables_1.Typography.subhead2,
    padding: (0, variables_1.getSpace)(2),
    textAlign: "center",
});
//# sourceMappingURL=index.js.map