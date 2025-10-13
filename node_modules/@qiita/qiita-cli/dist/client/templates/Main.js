"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
exports.Main = void 0;
const jsx_runtime_1 = require("@emotion/react/jsx-runtime");
const react_1 = require("@emotion/react");
const variables_1 = require("../lib/variables");
const mixins_1 = require("../lib/mixins");
const Main = ({ children }) => {
    return (0, jsx_runtime_1.jsx)("main", { css: mainStyle, children: children });
};
exports.Main = Main;
const mainStyle = (0, react_1.css)({
    backgroundColor: variables_1.Colors.gray5,
    display: "grid",
    gridTemplateAreas: `
  "sidebar contents"
  `,
    gridTemplateColumns: "auto minmax(450px, 1fr)",
    ...mixins_1.viewport.S({
        gridTemplateColumns: "auto minmax(0, 1fr)", // Setting min width to prevent widening
    }),
});
//# sourceMappingURL=Main.js.map