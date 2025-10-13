"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
exports.slidePages = void 0;
const escape_1 = require("./escape");
const slidePages = (article) => [
    `<h1>${(0, escape_1.escape)(article.title)}</h1><div class="slideMode-Viewer_content--firstSlideAuthor">by ${article.author.urlName}</div>`,
    ...article.body.split(/<hr.*?>/),
];
exports.slidePages = slidePages;
//# sourceMappingURL=slide-pages.js.map