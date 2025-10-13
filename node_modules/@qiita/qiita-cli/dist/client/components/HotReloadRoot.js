"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
exports.useHotReloadEffect = exports.HotReloadRoot = exports.HotReloadContext = void 0;
const jsx_runtime_1 = require("@emotion/react/jsx-runtime");
const react_1 = require("react");
exports.HotReloadContext = (0, react_1.createContext)({ reloadedAt: 0 });
const HotReloadRoot = ({ children }) => {
    const [reloadedAt, setReloadedAt] = (0, react_1.useState)(0);
    (0, react_1.useEffect)(() => {
        const websocket = new WebSocket(`ws:${window.location.host}`);
        websocket.onmessage = () => {
            setReloadedAt(new Date().getTime());
        };
        return () => {
            websocket.close();
        };
    }, []);
    return ((0, jsx_runtime_1.jsx)(exports.HotReloadContext.Provider, { value: { reloadedAt }, children: children }));
};
exports.HotReloadRoot = HotReloadRoot;
const useHotReloadEffect = (callback, deps) => {
    const { reloadedAt } = (0, react_1.useContext)(exports.HotReloadContext);
    (0, react_1.useEffect)(() => {
        callback();
    }, [reloadedAt, ...deps]);
};
exports.useHotReloadEffect = useHotReloadEffect;
//# sourceMappingURL=HotReloadRoot.js.map