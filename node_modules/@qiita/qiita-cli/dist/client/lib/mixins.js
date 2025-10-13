"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
exports.viewport = exports.breakpoint = exports.pointerCoarse = exports.pointerFine = void 0;
const entries_1 = require("./entries");
const pointerFine = (style) => ({
    "@media (hover: hover) and (pointer: fine)": style,
});
exports.pointerFine = pointerFine;
const pointerCoarse = (style) => ({
    "@media (hover: none) and (any-pointer: coarse)": style,
});
exports.pointerCoarse = pointerCoarse;
const viewportMaker = (breakpoint) => (style) => ({
    [`@media (max-width: ${breakpoint}px)`]: style,
});
exports.breakpoint = {
    XS: 479,
    S: 769,
    M: 991,
    L: 1199,
    XL: 1919,
    XXL: 2559,
};
exports.viewport = (0, entries_1.fromEntries)((0, entries_1.entries)(exports.breakpoint).map(([k, v]) => [k, viewportMaker(v)]));
//# sourceMappingURL=mixins.js.map