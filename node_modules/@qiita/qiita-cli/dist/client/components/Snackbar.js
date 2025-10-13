"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
exports.Snackbar = void 0;
const jsx_runtime_1 = require("@emotion/react/jsx-runtime");
const react_1 = require("@emotion/react");
const react_2 = require("react");
const variables_1 = require("../lib/variables");
const MaterialSymbol_1 = require("./MaterialSymbol");
const messageTypes = ["success", "error"];
const Snackbar = ({ message, setMessage }) => {
    const REMOVE_DELAY = 5000; // 5s
    const isMessageExists = !!message;
    const isMessageTypeSuccess = isMessageExists && message.type === "success";
    (0, react_2.useEffect)(() => {
        if (!isMessageExists) {
            return;
        }
        setTimeout(() => {
            setMessage(null);
        }, REMOVE_DELAY);
    }, [message]);
    return ((0, jsx_runtime_1.jsx)("p", { css: snackbarStyle(isMessageExists, isMessageTypeSuccess), "aria-live": "polite", children: isMessageExists && ((0, jsx_runtime_1.jsxs)(jsx_runtime_1.Fragment, { children: [(0, jsx_runtime_1.jsx)(MaterialSymbol_1.MaterialSymbol, { css: iconStyle(isMessageTypeSuccess), fill: true, children: isMessageTypeSuccess ? "check_circle" : "cancel" }), message.message] })) }));
};
exports.Snackbar = Snackbar;
const snackbarStyle = (isMessageExists, isMessageTypeSuccess) => (0, react_1.css)({
    alignItems: "center",
    backgroundColor: isMessageTypeSuccess ? variables_1.Colors.green10 : variables_1.Colors.red10,
    borderRadius: 8,
    display: "flex",
    fontSize: variables_1.Typography.body1,
    left: "50%",
    maxWidth: "calc(100% - 32px)",
    padding: isMessageExists ? `${(0, variables_1.getSpace)(1)}px ${(0, variables_1.getSpace)(3)}px` : 0,
    position: "absolute",
    textAlign: "center",
    top: 16,
    transform: "translateX(-50%)",
    width: "max-content",
});
const iconStyle = (isMessageTypeSuccess) => (0, react_1.css)({
    color: isMessageTypeSuccess ? variables_1.Colors.green60 : variables_1.Colors.red60,
    marginRight: (0, variables_1.getSpace)(1),
});
//# sourceMappingURL=Snackbar.js.map