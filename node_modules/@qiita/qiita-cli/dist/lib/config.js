"use strict";
var __importDefault = (this && this.__importDefault) || function (mod) {
    return (mod && mod.__esModule) ? mod : { "default": mod };
};
Object.defineProperty(exports, "__esModule", { value: true });
exports.config = void 0;
const promises_1 = __importDefault(require("node:fs/promises"));
const node_fs_1 = __importDefault(require("node:fs"));
const node_os_1 = __importDefault(require("node:os"));
const node_path_1 = __importDefault(require("node:path"));
const node_process_1 = __importDefault(require("node:process"));
const debugger_1 = require("./debugger");
class Config {
    constructor() {
        this.packageName = "qiita-cli";
    }
    load(options) {
        this.credentialDir = this.resolveConfigDir(options.credentialDir);
        this.itemsRootDir = this.resolveItemsRootDir(options.itemsRootDir);
        this.userConfigDir = this.resolveUserConfigDirPath(options.userConfigDir);
        this.userConfigFilePath = this.resolveUserConfigFilePath(options.userConfigDir);
        this.cacheDataDir = this.resolveCacheDataDir();
        this.credential = new Credential({
            credentialDir: this.credentialDir,
            profile: options.profile,
        });
        (0, debugger_1.configDebugger)("load", JSON.stringify({
            credentialDir: this.credentialDir,
            itemsRootDir: this.itemsRootDir,
            userConfigFilePath: this.userConfigFilePath,
            cacheDataDir: this.cacheDataDir,
        }));
    }
    getCredentialDir() {
        if (!this.credentialDir) {
            throw new Error("credentialDir is undefined");
        }
        return this.credentialDir;
    }
    // TODO: filesystemrepo 側にあるべきか確認
    getItemsRootDir() {
        if (!this.itemsRootDir) {
            throw new Error("itemsRootDir is undefined");
        }
        return this.itemsRootDir;
    }
    getUserConfigDir() {
        if (!this.userConfigDir) {
            throw new Error("userConfigDir is undefined");
        }
        return this.userConfigDir;
    }
    getUserConfigFilePath() {
        if (!this.userConfigFilePath) {
            throw new Error("userConfigFilePath is undefined");
        }
        return this.userConfigFilePath;
    }
    getCacheDataDir() {
        if (!this.cacheDataDir) {
            throw new Error("cacheDataDir is undefined");
        }
        return this.cacheDataDir;
    }
    getCredential() {
        if (!this.credential) {
            throw new Error("credential is undefined");
        }
        return this.credential.getCredential();
    }
    setCredential(credential) {
        if (!this.credential) {
            throw new Error("credential is undefined");
        }
        return this.credential.setCredential(credential);
    }
    async getUserConfig() {
        const defaultConfig = {
            includePrivate: false,
            host: "localhost",
            port: 8888,
        };
        if (node_fs_1.default.existsSync(this.getUserConfigFilePath())) {
            const userConfigFileData = await promises_1.default.readFile(this.userConfigFilePath);
            const userConfig = JSON.parse(userConfigFileData.toString());
            return { ...defaultConfig, ...userConfig };
        }
        return defaultConfig;
    }
    resolveConfigDir(credentialDirPath) {
        if (node_process_1.default.env.XDG_CONFIG_HOME) {
            const credentialDir = node_process_1.default.env.XDG_CONFIG_HOME;
            return node_path_1.default.join(credentialDir, this.packageName);
        }
        if (!credentialDirPath) {
            const homeDir = node_os_1.default.homedir();
            return node_path_1.default.join(homeDir, ".config", this.packageName);
        }
        return this.resolveFullPath(credentialDirPath);
    }
    resolveItemsRootDir(dirPath) {
        if (node_process_1.default.env.QIITA_CLI_ITEMS_ROOT) {
            return node_process_1.default.env.QIITA_CLI_ITEMS_ROOT;
        }
        if (!dirPath) {
            return node_process_1.default.cwd();
        }
        return this.resolveFullPath(dirPath);
    }
    resolveUserConfigDirPath(dirPath) {
        if (node_process_1.default.env.QIITA_CLI_USER_CONFIG_DIR) {
            return node_process_1.default.env.QIITA_CLI_USER_CONFIG_DIR;
        }
        if (!dirPath) {
            return node_process_1.default.cwd();
        }
        return this.resolveFullPath(dirPath);
    }
    resolveUserConfigFilePath(dirPath) {
        const filename = "qiita.config.json";
        return node_path_1.default.join(this.resolveUserConfigDirPath(dirPath), filename);
    }
    resolveCacheDataDir() {
        const cacheHome = node_process_1.default.env.XDG_CACHE_HOME || node_path_1.default.join(node_os_1.default.homedir(), ".cache");
        return node_path_1.default.join(cacheHome, this.packageName);
    }
    resolveFullPath(filePath) {
        if (node_path_1.default.isAbsolute(filePath)) {
            return filePath;
        }
        else {
            return node_path_1.default.join(node_process_1.default.cwd(), filePath);
        }
    }
}
/**
 * credential file format example:
 *
 * {
 *   "default": "qiita",
 *   "credentials": [
 *     {
 *       "accessToken": "PUBLIC_ACCESS_TOKEN",
 *       "name": "qiita"
 *     },
 *   ]
 * }
 */
class Credential {
    constructor({ credentialDir, profile, }) {
        this.credentialFilePath = node_path_1.default.join(credentialDir, "credentials.json");
        this.credentialDir = credentialDir;
        this.currentProfile = profile;
        this.cache = null;
    }
    async load() {
        if (this.cache !== null) {
            return this.cache;
        }
        let credentialData;
        if (node_process_1.default.env.QIITA_TOKEN) {
            const name = "environment variable";
            credentialData = {
                default: name,
                credentials: [
                    {
                        accessToken: node_process_1.default.env.QIITA_TOKEN,
                        name,
                    },
                ],
            };
        }
        else {
            const data = await promises_1.default.readFile(this.credentialFilePath, {
                encoding: "utf8",
            });
            credentialData = JSON.parse(data);
        }
        this.cache = credentialData;
        return credentialData;
    }
    refresh() {
        this.cache = null;
    }
    async getCredential() {
        const credentialData = await this.load();
        const profile = this.currentProfile
            ? this.currentProfile
            : credentialData.default;
        const credential = credentialData.credentials.find((cred) => cred.name === profile);
        if (!credential) {
            console.error("CredentialError:");
            console.error(`  profile is not exists '${profile}'`);
            node_process_1.default.exit(1);
        }
        (0, debugger_1.configDebugger)("use credential", JSON.stringify({ ...credential, accessToken: "******" }));
        return credential;
    }
    async setCredential(credential) {
        let credentialData;
        try {
            credentialData = await this.load();
        }
        catch (err) {
            // Error is not `no such file or directory`
            if (err.code !== "ENOENT") {
                throw err;
            }
            await promises_1.default.mkdir(this.credentialDir, { recursive: true });
            credentialData = {
                default: credential.name,
                credentials: [],
            };
        }
        const newCredentialData = {
            default: credentialData.default,
            credentials: [
                ...credentialData.credentials.filter((cred) => cred.name !== credential.name),
                credential,
            ],
        };
        await promises_1.default.writeFile(this.credentialFilePath, JSON.stringify(newCredentialData, null, 2), { encoding: "utf8", mode: 0o600 });
        this.refresh();
    }
}
const config = new Config();
exports.config = config;
//# sourceMappingURL=config.js.map