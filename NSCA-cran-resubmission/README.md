# NSCA 0.4.5 — CRAN 重新提交

## 檔案

| 檔案 | 內容 |
| --- | --- |
| `NSCA_0.4.5.tar.gz` | 要上傳到 CRAN 的套件 |
| `NSCA/` | tarball 解開後的完整原始碼，可直接在 GitHub 上逐檔瀏覽 |
| `NSCA_0.4.4_to_0.4.5_code.diff` | **審閱用**：與你上傳的 0.4.4 相比的程式碼差異（不含 `man/`），約 300 行 |
| `NSCA_0.4.4_to_0.4.5.diff` | 完整差異，含 roxygen 重新產生的 `man/*.Rd` |

## 背景：CRAN 為甚麼退件

NSCA 是首次投稿。CRAN 的自動檢查已經通過，退件來自人工審查，理由只有一個：
`nsca_extract()` 的範例被包在 `\dontrun{}` 裡。

| 寫法 | `example()` 時 | `R CMD check --as-cran` 時 | CRAN 允許的情況 |
| --- | --- | --- | --- |
| 不包裹 | 執行 | 執行，且計時（上限 5 秒） | 預設 |
| `\donttest{}` | 執行 | 另外跑一輪，不計入 5 秒上限 | 能跑但太慢 |
| `\dontrun{}` | 不執行，印出 `## Not run:` | 不執行 | 真的無法執行（缺軟體、API key 等） |

那段範例用了一個從未建立的 `fit`，所以才被包起來。實際上只要先建立 `fit` 就能在 0.1 秒內跑完。

## 0.4.5 的修改

### 1. 範例（回應退件信）

- `nsca_extract()`：拿掉 `\dontrun{}`，先建立 `fit` 再示範。
- `nsca()`、`nsca_plot()`、`nsca_results()`（含 `print`／`summary`）、`nsca_table()`、`nsca_thresholds()`
  原本沒有範例，現在都有，而且都不包裹。
- `nsca()` 保留預設的 **1000 次**置換檢定。它約需 11–15 秒，超過 5 秒上限，因此放在 `\donttest{}` 裡。
  這正是 CRAN 指定的用法：使用者執行 `example(nsca)` 時照樣會跑，CRAN 的 `--as-cran` 檢查也會另外跑一輪，但不計入 5 秒上限。
- `nsca_reference()` 的範例原本是手動寫在 `.Rd` 裡的，roxygen 原始碼中沒有。現已補回原始碼，
  否則下次執行 `roxygenise()` 範例就會消失。

### 2. 全面檢查後發現並修正的 bug

每個 bug 都加了回歸測試。這些測試在原本的 0.4.4 上會失敗，在 0.4.5 上會通過。

**(a) 條件變數有缺失值時，容許區面積算錯（嚴重）**

NCA／SCAtools 以每個條件自己的完整配對 (x, y) 來配適；NSCA 卻用**所有**觀測到的 y 來決定結果變數的範圍。
只要某一列有 Y、沒有 X，NSCA 量容許區的框就比引擎量 `d_nec`／`d_suf` 的框大。

| 情境 | `admissible_region_share` | `reconstruction_error` | `geometry_acceptable` |
| --- | --- | --- | --- |
| 單一條件，完整資料 | 0.167 | 0.00005 | TRUE |
| 同上，多兩列 X 缺失、Y 在範圍外 | **0.056** | **0.110** | **FALSE** |
| 兩個條件，X2 有缺失 | — | **0.648**（X2） | **FALSE** |

`geometry_acceptable` 變成 FALSE 後，`joint_support` 也會被判為 FALSE，但原因不在資料本身。
修正後，每個條件的 Y 範圍改由它自己的完整配對和 scope 決定，與引擎一致。

完整資料不受影響：在 48 種設定（4 個方向 × 有無 scope × 6 組資料，每組 2 個條件、4 種前沿技術）下，
0.4.4 與 0.4.5 的 `nsca_table()` 和 `nsca_thresholds()` 輸出**完全相同**（`identical() == TRUE`）。

**(b) `relevance` 以具名字串給定時，用字串比大小**

`relevance = c(necessity = "0.1", sufficiency = "0.2")` 會保留為字串，後面的
`d_nec >= threshold` 因而變成字串比較。現在一律先轉成數字再依名稱排序。

**(c) `shared.seed` 在新的 R session 裡留下亂數狀態**

說明文件寫「會還原使用者的亂數流」，但只有在原本就有亂數狀態時才會還原。
若 session 從未抽過亂數，函式結束後會留下由 `shared.seed` 產生的狀態，之後所有亂數都由它決定。現已修正。

**(d) `nsca_extract()` 的警告寫錯版本**

所有舊名稱都被說成「在 0.3.0 改名」，但其中 5 個（`data_zone_share`、`data_zone_width`、
`nsca_supported`、`conjunction`、`boundary`）是 0.4.0 改的。

**(e) `nsca_extract()` 對不存在的前沿技術回傳 NA**

用 `nec:`／`suf:` 前綴時，指定一個沒估計過的 `ceiling` 會靜默回傳 `NA`，看起來像一個缺失的測量值。
現在所有路徑都會報錯 "was not estimated"，與 `nsca_thresholds()`、`nsca_plot()` 一致。

### 3. 文件修正

- `?nsca_table` 的面積恆等式仍使用 0.4.0 已淘汰的名稱 `data_zone_share`，已改為 `admissible_region_share`。
- `nsca_table()`、`nsca_thresholds()` 的 `legacy` 參數各只寫了一個版本，實際上兩者都會附上 0.3.0 與 0.4.0 的舊名稱。
- 兩份 README 的安裝說明叫使用者裝 `NSCA_0.4.3.tar.gz`，並說 SCAtools 只有原始碼版。
  SCAtools 0.4.3 已在 CRAN 上，現改為 `install.packages("NSCA")`，並保留從 tarball 安裝的方式。
- NEWS：0.4.4 發佈時把 0.4.3 的段落標題改成了 0.4.4，所以 0.4.3 從歷史中消失了，
  而 0.4.4 唯一的實際變更（repository 網址）也沒有被記錄。現已還原 0.4.3 段落（與 0.4.3 tarball 內容逐字相同），並為 0.4.4 補上一段說明。
- 一段說明 step frontier 重疊的註解放錯位置，已移到它描述的函式上方。

### 4. 版本

`Version: 0.4.5`、`Date: 2026-09-30`。

## 檢查過、沒有問題的項目

- 13 個說明頁都有 `\value` 和可執行的範例。
- `print()`／`cat()` 只出現在 print／plot 方法裡。
- 沒有寫檔，沒有更改 `options()`／`par()`／工作目錄，沒有使用 `T`／`F`。
- `NCA_SKIP_PURITY` 確實是 NCA 讀取的環境變數，而且用 `on.exit()` 還原。
- 用到的 SCAtools 函式（`sca_analysis`、`sca_thresholds`、`sca_rescale`、`sca_scales`、`sca_table`、
  `sca_extract`）在 0.4.1 都已存在，`SCAtools (>= 0.4.1)` 的下限正確；SCAtools 0.4.2、0.4.3 沒有改變行為。
- DESCRIPTION 拼字：hunspell 只標出 `Dul`（作者姓）、`bivariate`、`normalised`（英式拼法），都是正確的。
- `inst/examples/` 裡的兩支腳本都能完整執行。

## 驗證

R 4.3.3（Ubuntu 24.04），依 CRAN 方式執行 `R CMD check --as-cran`：

```
* checking examples ... OK
* checking examples with --run-donttest ... [19s/19s] OK
* checking tests ... [55s/55s] OK
* checking PDF version of manual ... OK
* checking HTML version of manual ... NOTE
Skipping checking math rendering: package 'V8' unavailable

Status: 1 NOTE
```

唯一的 NOTE 是檢查用的機器沒有安裝 `V8` 套件，與 NSCA 無關。測試：1098 個全部通過。

各範例執行時間（計時那一輪，秒）：`nsca_plot` 0.66，其他都低於 0.1。

## 重新提交

上傳 `NSCA_0.4.5.tar.gz` 到 <https://cran.r-project.org/submit.html>。
"Optional comment" 欄位可貼上：

```
This is a resubmission. In this version I have:

* Removed \dontrun{} from the nsca_extract() example. It was only there
  because the example used an object it never created; the example now
  builds that object and runs unwrapped.

* Added executable examples to every exported function that lacked one.
  All run unwrapped in under 1 second, except nsca(), whose default of
  1000 permutations takes more than 5 seconds and is therefore wrapped
  in \donttest{}.

* Fixed several bugs found while reviewing the package (see NEWS.md) and
  increased the version to 0.4.5.

The words flagged as possibly misspelled in DESCRIPTION are correct:
'Dul' is an author's surname, and 'bivariate' and 'normalised' are
standard (British) spellings.
```
