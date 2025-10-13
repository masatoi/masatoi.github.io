"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
exports.SlideViewerDashboard = void 0;
const jsx_runtime_1 = require("@emotion/react/jsx-runtime");
const react_1 = require("react");
const MaterialSymbol_1 = require("../MaterialSymbol");
const get_magnitude_from_range_1 = require("./get-magnitude-from-range");
const SlideViewerDashboardNavigation_1 = require("./SlideViewerDashboardNavigation");
const SlideViewerDashboardQiitaLogo_1 = require("./SlideViewerDashboardQiitaLogo");
const SlideViewerDashboardTooltip_1 = require("./SlideViewerDashboardTooltip");
const SlideViewerDashboard = ({ currentPage, totalPage, isFullScreen, onPrevious, onNext, onSwitchFullScreen, onSetPage, }) => {
    const [isTooltipVisible, setIsTooltipVisible] = (0, react_1.useState)(false);
    const [destinationPage, setDestinationPage] = (0, react_1.useState)(currentPage);
    const [tooltipLeftDistance, setTooltipLeftDistance] = (0, react_1.useState)(0);
    return ((0, jsx_runtime_1.jsxs)("div", { className: "slideMode-Dashboard", children: [isTooltipVisible && ((0, jsx_runtime_1.jsxs)(SlideViewerDashboardTooltip_1.SlideViewerDashboardTooltip, { leftDistance: tooltipLeftDistance, children: [destinationPage, "/", totalPage] })), (0, jsx_runtime_1.jsx)(SlideViewerDashboardNavigation_1.SlideViewerDashboardNavigation, { currentPage: currentPage, totalPage: totalPage, onPrevious: onPrevious, onNext: onNext }), (0, jsx_runtime_1.jsxs)("span", { className: "slideMode-Dashboard_pageCount", children: [currentPage, " / ", totalPage] }), (0, jsx_runtime_1.jsx)("div", { className: "slideMode-Dashboard_progress", onMouseMove: (event) => {
                    setIsTooltipVisible(true);
                    setTooltipLeftDistance(event.clientX - event.currentTarget.getBoundingClientRect().left);
                    setDestinationPage((0, get_magnitude_from_range_1.getMagnitudeFromRange)(event.currentTarget, event.clientX, totalPage));
                }, onMouseLeave: () => {
                    setIsTooltipVisible(false);
                }, onClick: () => {
                    onSetPage(destinationPage);
                }, children: (0, jsx_runtime_1.jsx)("div", { className: "slideMode-Dashboard_progressFill", style: {
                        width: `${(currentPage / totalPage) * 100}%`,
                    } }) }), (0, jsx_runtime_1.jsx)("button", { "aria-label": "スライドショー", className: "slideMode-Dashboard_button slideMode-Dashboard_button--fullscreen slideMode-Dashboard_button--clickable", onClick: onSwitchFullScreen, children: (0, jsx_runtime_1.jsx)(MaterialSymbol_1.MaterialSymbol, { fill: true, size: 20, children: isFullScreen ? "close_fullscreen" : "live_tv" }) }), (0, jsx_runtime_1.jsx)(SlideViewerDashboardQiitaLogo_1.SlideViewerDashboardQiitaLogo, {})] }));
};
exports.SlideViewerDashboard = SlideViewerDashboard;
//# sourceMappingURL=SlideViewerDashboard.js.map