"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
exports.QiitaItem = void 0;
class QiitaItem {
    constructor({ id, title, tags, secret, updatedAt, organizationUrlName, rawBody, name, modified, isOlderThanRemote, itemsShowPath, published, itemPath, slide, ignorePublish, }) {
        this.id = id;
        this.title = title;
        this.tags = tags;
        this.secret = secret;
        this.updatedAt = updatedAt;
        this.organizationUrlName = organizationUrlName;
        this.rawBody = rawBody;
        this.name = name;
        this.modified = modified;
        this.isOlderThanRemote = isOlderThanRemote;
        this.itemsShowPath = itemsShowPath;
        this.published = published;
        this.itemPath = itemPath;
        this.slide = slide;
        this.ignorePublish = ignorePublish;
    }
}
exports.QiitaItem = QiitaItem;
//# sourceMappingURL=qiita-item.js.map