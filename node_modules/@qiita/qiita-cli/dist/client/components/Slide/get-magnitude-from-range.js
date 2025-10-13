"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
exports.getMagnitudeFromRange = getMagnitudeFromRange;
function getMagnitudeFromRange(target, x, length) {
    const rect = target.getBoundingClientRect();
    return Math.ceil((x - rect.left) / (rect.width / length));
}
//# sourceMappingURL=get-magnitude-from-range.js.map