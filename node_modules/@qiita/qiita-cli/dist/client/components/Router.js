"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
exports.Router = void 0;
const jsx_runtime_1 = require("@emotion/react/jsx-runtime");
const react_router_dom_1 = require("react-router-dom");
const items_1 = require("../pages/items");
const show_1 = require("../pages/items/show");
const Layout_1 = require("./Layout");
const router = (0, react_router_dom_1.createBrowserRouter)([
    {
        path: "/",
        element: (0, jsx_runtime_1.jsx)(Layout_1.Layout, {}),
        children: [
            {
                index: true,
                element: (0, jsx_runtime_1.jsx)(items_1.ItemsIndex, {}),
            },
            {
                path: "/items/:id",
                element: (0, jsx_runtime_1.jsx)(show_1.ItemsShow, {}),
            },
        ],
    },
]);
const Router = () => {
    return (0, jsx_runtime_1.jsx)(react_router_dom_1.RouterProvider, { router: router });
};
exports.Router = Router;
//# sourceMappingURL=Router.js.map