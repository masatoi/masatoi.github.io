"use strict";
var __importDefault = (this && this.__importDefault) || function (mod) {
    return (mod && mod.__esModule) ? mod : { "default": mod };
};
Object.defineProperty(exports, "__esModule", { value: true });
exports.FileSystemRepo = void 0;
const gray_matter_1 = __importDefault(require("gray-matter"));
const promises_1 = __importDefault(require("node:fs/promises"));
const node_path_1 = __importDefault(require("node:path"));
const qiita_cli_url_1 = require("../lib/qiita-cli-url");
const qiita_item_1 = require("./entities/qiita-item");
class FileContent {
    constructor({ title, tags, secret, updatedAt, id, organizationUrlName, rawBody, slide, ignorePublish = false, }) {
        this.title = title;
        this.tags = tags;
        this.secret = secret;
        this.updatedAt = updatedAt;
        this.id = id;
        this.organizationUrlName = organizationUrlName;
        this.rawBody = rawBody;
        this.slide = slide;
        this.ignorePublish = ignorePublish;
    }
    static read(fileContent) {
        const { data, content } = (0, gray_matter_1.default)(fileContent);
        return new FileContent({
            rawBody: content,
            title: data.title,
            tags: data.tags,
            secret: data.private,
            updatedAt: data.updated_at,
            id: data.id,
            organizationUrlName: data.organization_url_name,
            slide: data.slide,
            ignorePublish: data.ignorePublish ?? false,
        });
    }
    static empty({ title, id = null, }) {
        return new FileContent({
            rawBody: "# new article body",
            title,
            tags: [""],
            secret: false,
            updatedAt: "",
            id,
            organizationUrlName: null,
            slide: false,
            ignorePublish: false,
        });
    }
    static fromItem(item) {
        return new FileContent({
            rawBody: item.body,
            title: item.title,
            tags: item.tags.map((tag) => tag.name),
            secret: item.private,
            updatedAt: item.updated_at,
            id: item.id,
            organizationUrlName: item.organization_url_name,
            slide: item.slide,
            ignorePublish: false,
        });
    }
    static fromQiitaItem(item) {
        return new FileContent({
            rawBody: item.rawBody,
            title: item.title,
            tags: item.tags,
            secret: item.secret,
            updatedAt: item.updatedAt,
            id: item.id,
            organizationUrlName: item.organizationUrlName,
            slide: item.slide,
            ignorePublish: item.ignorePublish,
        });
    }
    toSaveFormat() {
        return gray_matter_1.default.stringify(this.rawBody, {
            title: this.title,
            tags: this.tags,
            private: this.secret,
            updated_at: this.updatedAt,
            id: this.id,
            organization_url_name: this.organizationUrlName,
            slide: this.slide,
            ignorePublish: this.ignorePublish,
        });
    }
    equals(aFileContent) {
        if (aFileContent === null) {
            return false;
        }
        if (!Array.isArray(this.tags) || !Array.isArray(aFileContent.tags)) {
            return false;
        }
        // not check id because it is transit
        return (this.organizationUrlName === aFileContent.organizationUrlName &&
            this.title === aFileContent.title &&
            this.tags.sort().join() === aFileContent.tags.sort().join() &&
            this.secret === aFileContent.secret &&
            this.rawBody === aFileContent.rawBody &&
            this.slide === aFileContent.slide &&
            this.ignorePublish === aFileContent.ignorePublish);
    }
    isOlderThan(otherFileContent) {
        if (!otherFileContent)
            return false;
        const updatedAt = new Date(this.updatedAt);
        const otherUpdatedAt = new Date(otherFileContent.updatedAt);
        return updatedAt < otherUpdatedAt;
    }
    clone({ id }) {
        return new FileContent({
            title: this.title,
            tags: this.tags,
            secret: this.secret,
            updatedAt: this.updatedAt,
            id,
            organizationUrlName: this.organizationUrlName,
            rawBody: this.rawBody,
            slide: this.slide,
            ignorePublish: this.ignorePublish,
        });
    }
}
class FileSystemRepo {
    constructor({ dataRootDir }) {
        this.dataRootDir = dataRootDir;
    }
    static async build({ dataRootDir }) {
        const fileSystemRepo = new FileSystemRepo({ dataRootDir });
        await fileSystemRepo.setUp();
        return fileSystemRepo;
    }
    async setUp() {
        await promises_1.default.mkdir(this.getRootPath(), { recursive: true });
        await promises_1.default.mkdir(this.getRemotePath(), { recursive: true });
    }
    getRootPath() {
        const subdir = "public";
        return node_path_1.default.join(this.dataRootDir, subdir);
    }
    getRemotePath() {
        const subdir = ".remote";
        return node_path_1.default.join(this.getRootPath(), subdir);
    }
    getRootOrRemotePath(remote = false) {
        return remote ? this.getRemotePath() : this.getRootPath();
    }
    getFilename(uuid) {
        return `${uuid}.md`;
    }
    parseFilename(filename) {
        return node_path_1.default.basename(filename, ".md");
    }
    getFilePath(uuid, remote = false) {
        return node_path_1.default.join(this.getRootOrRemotePath(remote), this.getFilename(uuid));
    }
    async getItemFilenames(remote = false) {
        return await promises_1.default.readdir(this.getRootOrRemotePath(remote), FileSystemRepo.fileSystemOptions());
    }
    defaultBasename(fileContent) {
        // TODO: Add article title to basename
        return fileContent.id;
    }
    async getNewBasename() {
        const prefix = "newArticle";
        const itemFilenames = await this.getItemFilenames();
        const limit = 999;
        for (let i = 1; i <= limit; ++i) {
            const suffix = i.toString().padStart(3, "0");
            const basename = `${prefix}${suffix}`;
            const filenameCandidate = this.getFilename(basename);
            const found = itemFilenames.find((filename) => filename === filenameCandidate);
            if (!found) {
                return basename;
            }
        }
        return;
    }
    static fileSystemOptions() {
        return {
            encoding: "utf8",
        };
    }
    async setItemData(fileContent, remote = false, basename = null) {
        if (!fileContent.id) {
            return;
        }
        const filepath = this.getFilePath(basename || this.defaultBasename(fileContent), remote);
        const data = fileContent.toSaveFormat();
        await promises_1.default.writeFile(filepath, data, FileSystemRepo.fileSystemOptions());
    }
    async getItemData(itemFilename, remote = false) {
        try {
            const fileContent = await promises_1.default.readFile(node_path_1.default.join(this.getRootOrRemotePath(remote), itemFilename), FileSystemRepo.fileSystemOptions());
            return FileContent.read(fileContent);
        }
        catch (err) {
            return null;
        }
    }
    async syncItem(item, beforeSync = false, forceUpdate = false) {
        const fileContent = FileContent.fromItem(item);
        if (beforeSync) {
            await this.setItemData(fileContent, true);
        }
        const localResult = await this.loadItemByItemId(item.id);
        const data = localResult ? FileContent.fromQiitaItem(localResult) : null;
        const basename = localResult?.name || null;
        const remoteFileContent = await this.getItemData(this.getFilename(item.id), true);
        if (data === null || remoteFileContent?.equals(data) || forceUpdate) {
            await this.setItemData(fileContent, true);
            await this.setItemData(fileContent, false, basename);
        }
        else {
            await this.setItemData(fileContent, true);
        }
    }
    async saveItems(items, forceUpdate = false) {
        const promises = items.map(async (item) => {
            await this.syncItem(item, false, forceUpdate);
        });
        await Promise.all(promises);
    }
    async saveItem(item, beforeSync = false, forceUpdate = false) {
        await this.syncItem(item, beforeSync, forceUpdate);
    }
    async loadItems() {
        const itemFilenames = await this.getItemFilenames();
        const promises = itemFilenames
            .filter((itemFilename) => /\.md$/.test(itemFilename))
            .map(async (itemFilename) => {
            const basename = this.parseFilename(itemFilename);
            return await this.loadItemByBasename(basename);
        });
        const items = excludeNull(await Promise.all(promises));
        return items;
    }
    async loadItemByItemId(itemId) {
        const items = await this.getItemFilenames();
        let localFileContent = null;
        let itemFilename = null;
        for (const item of items) {
            const tmpLocalFileContent = await this.getItemData(item, false);
            if (tmpLocalFileContent?.id === itemId) {
                localFileContent = tmpLocalFileContent;
                itemFilename = item;
            }
            if (localFileContent)
                break;
        }
        if (!localFileContent || !itemFilename) {
            return null;
        }
        const basename = this.parseFilename(itemFilename);
        const itemPath = this.getFilePath(basename);
        const remoteFileContent = localFileContent.id
            ? await this.getItemData(this.getFilename(localFileContent.id), true)
            : null;
        return new qiita_item_1.QiitaItem({
            id: localFileContent.id,
            title: localFileContent.title,
            tags: localFileContent.tags,
            secret: localFileContent.secret,
            updatedAt: localFileContent.updatedAt,
            organizationUrlName: localFileContent.organizationUrlName,
            rawBody: localFileContent.rawBody,
            slide: localFileContent.slide,
            name: basename,
            modified: !localFileContent.equals(remoteFileContent),
            isOlderThanRemote: localFileContent.isOlderThan(remoteFileContent),
            itemsShowPath: this.generateItemsShowPath(localFileContent.id, basename),
            published: remoteFileContent !== null,
            ignorePublish: localFileContent.ignorePublish,
            itemPath,
        });
    }
    async loadItemByBasename(basename) {
        const items = await this.getItemFilenames();
        const itemFilename = this.getFilename(basename);
        if (!items.includes(itemFilename)) {
            return null;
        }
        const itemPath = this.getFilePath(basename);
        const localFileContent = await this.getItemData(itemFilename, false);
        if (!localFileContent) {
            return null;
        }
        const remoteFileContent = localFileContent.id
            ? await this.getItemData(this.getFilename(localFileContent.id), true)
            : null;
        return new qiita_item_1.QiitaItem({
            id: localFileContent.id,
            title: localFileContent.title,
            tags: localFileContent.tags,
            secret: localFileContent.secret,
            updatedAt: localFileContent.updatedAt,
            organizationUrlName: localFileContent.organizationUrlName,
            rawBody: localFileContent.rawBody,
            slide: localFileContent.slide,
            name: basename,
            modified: !localFileContent.equals(remoteFileContent),
            isOlderThanRemote: localFileContent.isOlderThan(remoteFileContent),
            itemsShowPath: this.generateItemsShowPath(localFileContent.id, basename),
            published: remoteFileContent !== null,
            ignorePublish: localFileContent.ignorePublish,
            itemPath,
        });
    }
    async createItem(basename) {
        basename = basename || (await this.getNewBasename());
        if (!basename)
            return;
        const item = await this.loadItemByBasename(basename);
        if (item)
            return;
        const filepath = this.getFilePath(basename);
        const newFileContent = FileContent.empty({ title: basename });
        const data = newFileContent.toSaveFormat();
        await promises_1.default.writeFile(filepath, data, FileSystemRepo.fileSystemOptions());
        return basename;
    }
    async updateItemUuid(basename, newItemId) {
        const item = await this.loadItemByBasename(basename);
        const data = item ? FileContent.fromQiitaItem(item) : null;
        if (!data) {
            return;
        }
        if (data.id === newItemId) {
            // skip
            return;
        }
        const newFilePath = this.getFilePath(basename);
        const newData = data.clone({ id: newItemId }).toSaveFormat();
        await promises_1.default.writeFile(newFilePath, newData, FileSystemRepo.fileSystemOptions());
    }
    // FIXME: Move outside of "repository"
    generateItemsShowPath(itemId, basename) {
        return itemId ? (0, qiita_cli_url_1.itemsShowPath)(itemId) : (0, qiita_cli_url_1.itemsShowPath)("show", { basename });
    }
}
exports.FileSystemRepo = FileSystemRepo;
const excludeNull = (array) => {
    return array.filter((val) => val !== null);
};
//# sourceMappingURL=file-system-repo.js.map