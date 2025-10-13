"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
exports.useWindowSize = void 0;
const react_1 = require("react");
const useWindowSize = () => {
    const isClient = typeof window === "object";
    const getWindowDimensions = (0, react_1.useCallback)(() => {
        return {
            currentWidth: isClient ? window?.innerWidth : 0,
            currentHeight: isClient ? window?.innerHeight : 0,
        };
    }, [isClient]);
    const [windowDimensions, setWindowDimensions] = (0, react_1.useState)(getWindowDimensions());
    (0, react_1.useEffect)(() => {
        const onResize = () => {
            setWindowDimensions(getWindowDimensions());
        };
        window.addEventListener("resize", onResize);
        return () => window.removeEventListener("resize", onResize);
    }, [getWindowDimensions]);
    return windowDimensions;
};
exports.useWindowSize = useWindowSize;
//# sourceMappingURL=window-size.js.map