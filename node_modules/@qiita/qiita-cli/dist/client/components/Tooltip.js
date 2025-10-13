"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
exports.Tooltip = void 0;
const jsx_runtime_1 = require("@emotion/react/jsx-runtime");
const react_1 = require("react");
const react_2 = require("@emotion/react");
const variables_1 = require("../lib/variables");
const Tooltip = ({ message, children, horizontal = "center", vertical = "top", offset = 0, tabIndex, id, }) => {
    const [isShow, setIsShow] = (0, react_1.useState)(false);
    const handleShow = () => {
        setIsShow(true);
    };
    const handleHide = () => {
        setIsShow(false);
    };
    const handleKeyDown = (0, react_1.useCallback)((event) => {
        if (event.key === "Escape" && isShow) {
            handleHide();
        }
    }, [isShow, handleHide]);
    (0, react_1.useEffect)(() => {
        if (isShow)
            window.addEventListener("keydown", handleKeyDown);
        return () => {
            window.removeEventListener("keydown", handleKeyDown);
        };
    }, [isShow, handleKeyDown]);
    return ((0, jsx_runtime_1.jsxs)("div", { onMouseEnter: handleShow, onMouseLeave: handleHide, onFocus: handleShow, onBlur: handleHide, css: toolTipWrapStyle, "aria-describedby": id, children: [(0, jsx_runtime_1.jsx)("div", { css: toolTipItemStyle(isShow, horizontal, vertical, offset), role: "tooltip", tabIndex: tabIndex, id: id, children: message }), children] }));
};
exports.Tooltip = Tooltip;
const toolTipWrapStyle = (0, react_2.css)({
    position: "relative",
});
const toolTipItemStyle = (isShow, horizontal, vertical, offset) => (0, react_2.css)({
    visibility: isShow ? "visible" : "hidden",
    color: variables_1.Colors.gray0,
    backgroundColor: variables_1.Colors.gray90,
    borderRadius: 4,
    fontSize: variables_1.Typography.body3,
    position: "absolute",
    padding: `${(0, variables_1.getSpace)(1 / 2)}px ${(0, variables_1.getSpace)(1)}px`,
    zIndex: 10,
    maxWidth: 280,
    width: "max-content",
    ...(vertical === "top" &&
        horizontal === "center" && {
        bottom: `calc(100% + ${offset}px + 4px)`,
        left: "50%",
        transform: "translateX(-50%)",
        "&::after": {
            content: "''",
            width: 8,
            height: 4,
            position: "absolute",
            backgroundColor: variables_1.Colors.gray90,
            top: "100%",
            left: "50%",
            transform: "translateX(-50%)",
            clipPath: "polygon(0% 0%, 100% 0%, 50% 100%, 50% 100%)",
        },
    }),
    ...(vertical === "top" &&
        horizontal === "left" && {
        bottom: `calc(100% + ${offset}px + 4px)`,
        right: "calc(50% - 12px)",
        "&::after": {
            content: "''",
            width: 8,
            height: 4,
            position: "absolute",
            backgroundColor: variables_1.Colors.gray90,
            top: "100%",
            right: 8,
            clipPath: "polygon(0% 0%, 100% 0%, 50% 100%, 50% 100%)",
        },
    }),
    ...(vertical === "top" &&
        horizontal === "right" && {
        bottom: `calc(100% + ${offset}px + 4px)`,
        left: "calc(50% - 12px)",
        "&::after": {
            content: "''",
            width: 8,
            height: 4,
            position: "absolute",
            backgroundColor: variables_1.Colors.gray90,
            top: "100%",
            left: 8,
            clipPath: "polygon(0% 0%, 100% 0%, 50% 100%, 50% 100%)",
        },
    }),
    ...(vertical === "middle" &&
        horizontal === "left" && {
        top: "50%",
        transform: "translateY(-50%)",
        right: `calc(100% + ${offset}px + 4px)`,
        "&::after": {
            content: "''",
            width: 4,
            height: 8,
            position: "absolute",
            backgroundColor: variables_1.Colors.gray90,
            top: "50%",
            transform: "translateY(-50%)",
            left: "100%",
            clipPath: "polygon(0% 0%, 100% 50%, 100% 50%, 0% 100%)",
        },
    }),
    ...(vertical === "middle" &&
        horizontal === "right" && {
        top: "50%",
        transform: "translateY(-50%)",
        left: `calc(100% + ${offset}px + 4px)`,
        "&::after": {
            content: "''",
            width: 4,
            height: 8,
            position: "absolute",
            backgroundColor: variables_1.Colors.gray90,
            top: "50%",
            transform: "translateY(-50%)",
            right: "100%",
            clipPath: "polygon(0% 50%, 100% 0%, 100% 100%, 0% 50%)",
        },
    }),
    ...(vertical === "bottom" &&
        horizontal === "left" && {
        top: `calc(100% + ${offset}px + 4px)`,
        right: "calc(50% - 12px)",
        "&::after": {
            content: "''",
            width: 8,
            height: 4,
            position: "absolute",
            backgroundColor: variables_1.Colors.gray90,
            bottom: "100%",
            right: 8,
            clipPath: "polygon(50% 0%, 50% 0%, 100% 100%, 0% 100%)",
        },
    }),
    ...(vertical === "bottom" &&
        horizontal === "center" && {
        top: `calc(100% + ${offset}px + 4px)`,
        left: "50%",
        transform: "translateX(-50%)",
        "&::after": {
            content: "''",
            width: 8,
            height: 4,
            position: "absolute",
            backgroundColor: variables_1.Colors.gray90,
            bottom: "100%",
            left: "50%",
            transform: "translateX(-50%)",
            clipPath: "polygon(50% 0%, 50% 0%, 100% 100%, 0% 100%)",
        },
    }),
    ...(vertical === "bottom" &&
        horizontal === "right" && {
        top: `calc(100% + ${offset}px + 4px)`,
        left: "calc(50% - 12px)",
        "&::after": {
            content: "''",
            width: 8,
            height: 4,
            position: "absolute",
            backgroundColor: variables_1.Colors.gray90,
            bottom: "100%",
            left: 8,
            clipPath: "polygon(50% 0%, 50% 0%, 100% 100%, 0% 100%)",
        },
    }),
});
//# sourceMappingURL=Tooltip.js.map