"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
exports.CopyButton = void 0;
const jsx_runtime_1 = require("@emotion/react/jsx-runtime");
const react_1 = require("@emotion/react");
const react_2 = require("react");
const clipboard_1 = require("../lib/clipboard");
const variables_1 = require("../lib/variables");
const MaterialSymbol_1 = require("./MaterialSymbol");
const Tooltip_1 = require("./Tooltip");
const CopyButton = ({ text }) => {
    const [isCopied, setIsCopied] = (0, react_2.useState)(false);
    const handleCopyItemPath = (0, react_2.useCallback)(() => {
        setIsCopied(true);
        (0, clipboard_1.writeClipboard)(text).then(() => {
            setTimeout(() => {
                setIsCopied(false);
            }, 1000);
        });
    }, [text]);
    return ((0, jsx_runtime_1.jsx)(Tooltip_1.Tooltip, { message: isCopied ? "コピーしました！" : "コピーする", vertical: "bottom", children: (0, jsx_runtime_1.jsx)("button", { css: buttonStyle, onClick: handleCopyItemPath, children: (0, jsx_runtime_1.jsx)(MaterialSymbol_1.MaterialSymbol, { children: "content_copy" }) }) }));
};
exports.CopyButton = CopyButton;
const buttonStyle = (0, react_1.css)({
    backgroundColor: "inherit",
    padding: (0, variables_1.getSpace)(1 / 2),
});
//# sourceMappingURL=CopyButton.js.map