"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
exports.ItemsRouter = void 0;
const express_1 = require("express");
const check_frontmatter_type_1 = require("../../lib/check-frontmatter-type");
const get_file_system_repo_1 = require("../../lib/get-file-system-repo");
const get_qiita_api_instance_1 = require("../../lib/get-qiita-api-instance");
const item_validator_1 = require("../../lib/validators/item-validator");
const get_current_user_1 = require("../lib/get-current-user");
const qiita_url_1 = require("../lib/qiita-url");
const itemsIndex = async (req, res) => {
    const fileSystemRepo = await (0, get_file_system_repo_1.getFileSystemRepo)();
    const itemData = await fileSystemRepo.loadItems();
    const result = itemData.reduce((prev, item) => {
        const resultItem = {
            id: item.id,
            items_show_path: item.itemsShowPath,
            secret: item.secret,
            title: item.title,
            updated_at: item.updatedAt,
            modified: item.modified,
        };
        if (item.id) {
            if (item.secret) {
                prev.private.push(resultItem);
            }
            else {
                prev.public.push(resultItem);
            }
        }
        else {
            prev.draft.push(resultItem);
        }
        return prev;
    }, {
        private: [],
        draft: [],
        public: [],
    });
    res.json(result);
};
const itemsShow = async (req, res) => {
    const itemId = req.params.id;
    const basename = req.query.basename;
    const fileSystemRepo = await (0, get_file_system_repo_1.getFileSystemRepo)();
    const item = itemId === "show" && basename
        ? await fileSystemRepo.loadItemByBasename(basename)
        : await fileSystemRepo.loadItemByItemId(itemId);
    if (!item) {
        res.status(404).json({
            message: "Not found",
        });
        return;
    }
    const errorFrontmatterMessages = (0, check_frontmatter_type_1.checkFrontmatterType)(item);
    if (errorFrontmatterMessages.length > 0) {
        res.status(500).json({
            errorMessages: errorFrontmatterMessages,
        });
        return;
    }
    const { itemPath, modified, published } = item;
    const qiitaApi = await (0, get_qiita_api_instance_1.getQiitaApiInstance)();
    const renderedBody = await qiitaApi.preview(item.rawBody);
    const currentUser = await (0, get_current_user_1.getCurrentUser)();
    const qiitaItemUrl = published
        ? (0, qiita_url_1.itemUrl)({
            id: item.id,
            userId: currentUser.id,
            secret: item.secret,
        })
        : null;
    // validate
    const errorMessages = (0, item_validator_1.validateItem)(item);
    const result = {
        error_messages: errorMessages,
        is_older_than_remote: item.isOlderThanRemote,
        item_path: itemPath,
        modified,
        organization_url_name: item.organizationUrlName,
        secret: item.secret,
        published,
        qiita_item_url: qiitaItemUrl,
        rendered_body: renderedBody,
        slide: item.slide,
        tags: item.tags,
        title: item.title,
    };
    res.json(result);
};
const itemsCreate = async (req, res) => {
    const fileSystemRepo = await (0, get_file_system_repo_1.getFileSystemRepo)();
    const basename = await fileSystemRepo.createItem();
    res.json({ basename });
};
const itemsUpdate = async (req, res) => {
    const itemId = req.params.id;
    const basename = req.body.basename;
    const fileSystemRepo = await (0, get_file_system_repo_1.getFileSystemRepo)();
    const result = itemId === "post" && basename
        ? await fileSystemRepo.loadItemByBasename(basename)
        : await fileSystemRepo.loadItemByItemId(itemId);
    if (!result) {
        res.status(404).json({
            message: "Not found",
        });
        return;
    }
    const qiitaApi = await (0, get_qiita_api_instance_1.getQiitaApiInstance)();
    const output = {
        success: true,
        uuid: result.id || "",
    };
    let item;
    try {
        if (!result.id && itemId === "post") {
            if (!basename)
                throw new Error("basename is undefined");
            item = await qiitaApi.postItem({
                rawBody: result.rawBody,
                tags: result.tags,
                title: result.title,
                isPrivate: result.secret,
                organizationUrlName: result.organizationUrlName,
                slide: result.slide,
            });
            if (item) {
                fileSystemRepo.updateItemUuid(basename, item.id);
                output.uuid = item.id;
            }
        }
        else if (result.id) {
            item = await qiitaApi.patchItem({
                rawBody: result.rawBody,
                tags: result.tags,
                title: result.title,
                uuid: result.id,
                isPrivate: result.secret,
                organizationUrlName: result.organizationUrlName,
                slide: result.slide,
            });
        }
        else {
            throw new Error("Unknown Error");
        }
        await fileSystemRepo.saveItem(item, false, true);
        res.json(output);
    }
    catch {
        res.json({
            success: false,
        });
    }
};
exports.ItemsRouter = (0, express_1.Router)()
    .get("/", itemsIndex)
    .post("/", itemsCreate)
    .get("/:id", itemsShow)
    .post("/:id", itemsUpdate);
//# sourceMappingURL=items.js.map