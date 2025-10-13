"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
exports.HeaderIndex = exports.Header = void 0;
const jsx_runtime_1 = require("@emotion/react/jsx-runtime");
const react_1 = require("@emotion/react");
const react_2 = require("react");
const react_router_dom_1 = require("react-router-dom");
const qiita_cli_url_1 = require("../../lib/qiita-cli-url");
const mixins_1 = require("../lib/mixins");
const variables_1 = require("../lib/variables");
const window_size_1 = require("../lib/window-size");
const CopyButton_1 = require("./CopyButton");
const Logo_1 = require("./Logo");
const MaterialSymbol_1 = require("./MaterialSymbol");
const Snackbar_1 = require("./Snackbar");
const Header = ({ id, isItemPublishable, isOlderThanRemote, itemPath, basename, handleMobileOpen, }) => {
    const navigate = (0, react_router_dom_1.useNavigate)();
    const [snackbarMessage, setSnackbarMessage] = (0, react_2.useState)(null);
    const { currentWidth } = (0, window_size_1.useWindowSize)();
    const mobileSize = currentWidth <= mixins_1.breakpoint.S;
    const handlePublish = () => {
        if (isOlderThanRemote) {
            if (!window.confirm("この記事はQiita上の記事より古い可能性があります。上書きしますか？")) {
                return;
            }
        }
        const params = {
            method: "POST",
            headers: {
                "Content-Type": "application/json",
            },
            body: JSON.stringify({ basename: basename }),
        };
        if (id !== undefined) {
            fetch((0, qiita_cli_url_1.apiItemsUpdatePath)(id), params)
                .then((response) => response.json())
                .then((data) => {
                if (!data.uuid) {
                    setSnackbarMessage({
                        type: "error",
                        message: "投稿に失敗しました",
                    });
                    return;
                }
                try {
                    setSnackbarMessage({
                        type: "success",
                        message: "記事が投稿されました",
                    });
                    navigate((0, qiita_cli_url_1.itemsShowPath)(data.uuid));
                }
                catch {
                    navigate(0);
                }
            });
        }
    };
    return ((0, jsx_runtime_1.jsxs)("header", { css: headerStyle, children: [mobileSize ? ((0, jsx_runtime_1.jsxs)("div", { css: headerItemStyle, children: [(0, jsx_runtime_1.jsx)("button", { "aria-label": "\u30E1\u30CB\u30E5\u30FC\u3092\u958B\u304F", css: headerMenuButtonStyle, onClick: handleMobileOpen, children: (0, jsx_runtime_1.jsx)(MaterialSymbol_1.MaterialSymbol, { size: 24, children: "menu" }) }), (0, jsx_runtime_1.jsxs)("div", { css: mobileHeaderCopyButtonStyle, children: ["\u30D5\u30A1\u30A4\u30EB\u30D1\u30B9\u3092\u30B3\u30D4\u30FC", (0, jsx_runtime_1.jsx)(CopyButton_1.CopyButton, { text: itemPath })] })] })) : ((0, jsx_runtime_1.jsxs)("div", { css: headerItemStyle, children: [(0, jsx_runtime_1.jsx)("p", { children: itemPath }), (0, jsx_runtime_1.jsx)(CopyButton_1.CopyButton, { text: itemPath })] })), (0, jsx_runtime_1.jsxs)("button", { css: headerButtonStyle, disabled: !isItemPublishable, onClick: handlePublish, children: [mobileSize ? "投稿する" : "記事を投稿する", (0, jsx_runtime_1.jsx)(MaterialSymbol_1.MaterialSymbol, { children: "publish" })] }), (0, jsx_runtime_1.jsx)(Snackbar_1.Snackbar, { message: snackbarMessage, setMessage: setSnackbarMessage })] }));
};
exports.Header = Header;
const HeaderIndex = ({ handleMobileOpen, }) => {
    return ((0, jsx_runtime_1.jsx)("header", { css: headerStyle, children: (0, jsx_runtime_1.jsxs)("div", { css: headerItemStyle, children: [(0, jsx_runtime_1.jsx)("button", { "aria-label": "\u30E1\u30CB\u30E5\u30FC\u3092\u958B\u304F", css: headerMenuButtonStyle, onClick: handleMobileOpen, children: (0, jsx_runtime_1.jsx)(MaterialSymbol_1.MaterialSymbol, { size: 24, children: "menu" }) }), (0, jsx_runtime_1.jsx)("h1", { css: headerLogoWrapperStyle, children: (0, jsx_runtime_1.jsx)(Logo_1.QiitaPreviewLogo, { css: headerLogoStyle }) })] }) }));
};
exports.HeaderIndex = HeaderIndex;
const headerStyle = (0, react_1.css)({
    alignItems: "center",
    backgroundColor: variables_1.Colors.gray0,
    borderBottom: `1px solid ${variables_1.Colors.divider}`,
    display: "flex",
    justifyContent: "space-between",
    padding: `${(0, variables_1.getSpace)(1)}px ${(0, variables_1.getSpace)(2)}px`,
    position: "relative",
});
const headerItemStyle = (0, react_1.css)({
    alignItems: "center",
    display: "flex",
    fontSize: variables_1.Typography.body2,
    color: variables_1.Colors.mediumEmphasis,
    gap: `0 ${(0, variables_1.getSpace)(1 / 2)}px`,
});
const headerMenuButtonStyle = (0, react_1.css)({
    backgroundColor: "inherit",
    padding: (0, variables_1.getSpace)(1),
});
const mobileHeaderCopyButtonStyle = (0, react_1.css)({
    alignItems: "center",
    display: "grid",
    gap: (0, variables_1.getSpace)(1 / 2),
    gridTemplateColumns: "1fr 16px",
    padding: 0,
});
const headerButtonStyle = (0, react_1.css)({
    alignItems: "center",
    backgroundColor: variables_1.Colors.gray0,
    border: `2px solid ${variables_1.Colors.green60}`,
    borderRadius: 8,
    color: variables_1.Colors.green80,
    display: "flex",
    fontWeight: variables_1.Weight.bold,
    gap: `0 ${(0, variables_1.getSpace)(1 / 2)}px`,
    padding: `${(0, variables_1.getSpace)(1 / 2)}px ${(0, variables_1.getSpace)(2)}px`,
    "&:disabled": {
        opacity: 0.32,
    },
    "&:active": {
        backgroundColor: variables_1.Colors.gray20,
    },
    ...(0, mixins_1.pointerFine)({
        "&:hover": {
            backgroundColor: variables_1.Colors.gray20,
        },
    }),
});
const headerLogoWrapperStyle = (0, react_1.css)({
    display: "flex",
});
const headerLogoStyle = (0, react_1.css)({
    height: 24,
});
//# sourceMappingURL=Header.js.map