"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
const get_file_system_repo_1 = require("../lib/get-file-system-repo");
const get_qiita_api_instance_1 = require("../lib/get-qiita-api-instance");
const sync_articles_from_qiita_1 = require("../lib/sync-articles-from-qiita");
const pull_1 = require("./pull");
jest.mock("../lib/get-qiita-api-instance");
jest.mock("../lib/get-file-system-repo");
jest.mock("../lib/sync-articles-from-qiita");
const mockGetQiitaApiInstance = jest.mocked(get_qiita_api_instance_1.getQiitaApiInstance);
const mockGetFileSystemRepo = jest.mocked(get_file_system_repo_1.getFileSystemRepo);
const mockSyncArticlesFromQiita = jest.mocked(sync_articles_from_qiita_1.syncArticlesFromQiita);
describe("pull", () => {
    const qiitaApi = {};
    const fileSystemRepo = {};
    beforeEach(() => {
        mockGetQiitaApiInstance.mockReset();
        mockGetFileSystemRepo.mockReset();
        mockSyncArticlesFromQiita.mockReset();
        mockGetQiitaApiInstance.mockReturnValue(qiitaApi);
        mockGetFileSystemRepo.mockReturnValue(fileSystemRepo);
        mockSyncArticlesFromQiita.mockImplementation();
        jest.spyOn(console, "log").mockImplementation();
    });
    it("pulls articles", async () => {
        await (0, pull_1.pull)([]);
        expect(mockSyncArticlesFromQiita).toBeCalledWith({
            fileSystemRepo,
            qiitaApi,
            forceUpdate: undefined,
        });
        expect(mockSyncArticlesFromQiita).toBeCalledTimes(1);
    });
    describe('with "--force" option', () => {
        it("pulls articles with forceUpdate", async () => {
            await (0, pull_1.pull)(["--force"]);
            expect(mockSyncArticlesFromQiita).toBeCalledWith({
                fileSystemRepo,
                qiitaApi,
                forceUpdate: true,
            });
            expect(mockSyncArticlesFromQiita).toBeCalledTimes(1);
        });
        it("pulls articles with forceUpdate", async () => {
            await (0, pull_1.pull)(["-f"]);
            expect(mockSyncArticlesFromQiita).toBeCalledWith({
                fileSystemRepo,
                qiitaApi,
                forceUpdate: true,
            });
        });
    });
});
//# sourceMappingURL=pull.test.js.map