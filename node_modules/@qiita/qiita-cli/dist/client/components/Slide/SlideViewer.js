"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
exports.SlideViewer = void 0;
const jsx_runtime_1 = require("@emotion/react/jsx-runtime");
const react_1 = require("react");
const SlideViewerContent_1 = require("./SlideViewerContent");
const SlideViewerDashboard_1 = require("./SlideViewerDashboard");
const LEFT_KEY = 37;
const RIGHT_KEY = 39;
const SlideViewer = ({ pages }) => {
    const [isFullScreen, setIsFullScreen] = (0, react_1.useState)(false);
    const contentRef = (0, react_1.useRef)(null);
    const scrollToTopOfContent = (0, react_1.useCallback)(() => {
        if (contentRef.current) {
            contentRef.current.scrollTop = 0;
        }
    }, []);
    const [currentPageIndex, setCurrentPageIndex] = (0, react_1.useState)(0);
    const next = (0, react_1.useCallback)(() => {
        if (currentPageIndex + 1 < pages.length) {
            setCurrentPageIndex(currentPageIndex + 1);
            scrollToTopOfContent();
        }
    }, [currentPageIndex, pages.length, scrollToTopOfContent]);
    const prev = (0, react_1.useCallback)(() => {
        if (currentPageIndex - 1 >= 0) {
            setCurrentPageIndex(currentPageIndex - 1);
            scrollToTopOfContent();
        }
    }, [currentPageIndex, scrollToTopOfContent]);
    (0, react_1.useEffect)(() => {
        const handleGlobalKeyDown = (event) => {
            if (event.keyCode === LEFT_KEY) {
                prev();
            }
            else if (event.keyCode === RIGHT_KEY) {
                next();
            }
        };
        window.addEventListener("keydown", handleGlobalKeyDown);
        return () => {
            window.removeEventListener("keydown", handleGlobalKeyDown);
        };
    }, [next, prev]);
    return ((0, jsx_runtime_1.jsxs)("div", { className: "slideMode" + (isFullScreen ? " fullscreen" : ""), children: [(0, jsx_runtime_1.jsx)("div", { className: "slideMode-Viewer", children: (0, jsx_runtime_1.jsx)(SlideViewerContent_1.SlideViewerContent, { pages: pages, currentPageIndex: currentPageIndex, onPrevious: prev, onNext: next, contentRef: contentRef }) }), (0, jsx_runtime_1.jsx)(SlideViewerDashboard_1.SlideViewerDashboard, { currentPage: currentPageIndex + 1, totalPage: pages.length, isFullScreen: isFullScreen, onNext: next, onPrevious: prev, onSetPage: (page) => setCurrentPageIndex(page - 1), onSwitchFullScreen: () => setIsFullScreen(!isFullScreen) })] }));
};
exports.SlideViewer = SlideViewer;
//# sourceMappingURL=SlideViewer.js.map