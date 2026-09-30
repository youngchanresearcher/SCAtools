# NSCA 0.4.4 — CRAN 重新提交說明

## 發生了甚麼事

NSCA 0.4.4 是首次投稿 CRAN（CRAN 上目前沒有 NSCA）。投稿後的流程是：

1. **自動檢查**：CRAN 在 Windows 與 Debian 上跑 `R CMD check --as-cran`。這一關已經過了，
   否則不會進入下一步。
2. **人工審查**：CRAN 志工逐項看 DESCRIPTION、`.Rd` 說明檔、範例等是否符合
   [CRAN Cookbook](https://contributor.r-project.org/cran-cookbook/)。這次的退件信就來自這一步。

退件信只指出一件事：**`\dontrun{}` 的使用方式**。

### `\dontrun{}`、`\donttest{}`、不包裹，三者的差別

| 寫法 | `example()` 使用者執行時 | `R CMD check` | CRAN 的使用時機 |
| --- | --- | --- | --- |
| 不包裹 | 執行 | 執行 | 預設，執行時間 < 5 秒 |
| `\donttest{}` | 執行 | `--as-cran` 時**也會執行** | 能跑但太慢（> 5 秒） |
| `\dontrun{}` | **不執行**，印出 `## Not run:` | 不執行 | 真的無法執行（缺外部軟體、API key 等） |

`\dontrun{}` 等於告訴使用者「這段程式碼不能跑」，所以 CRAN 只允許在真的不能跑時使用。

### NSCA 的問題在哪裡

問題出在 `nsca_extract()` 的範例：

```r
\dontrun{
nsca_extract(fit, param = "weakest_effect")
nsca_extract(fit, param = "nec:Ceiling accuracy")
}
```

這段之所以被包進 `\dontrun{}`，是因為 `fit` 從來沒有被建立，直接執行會出錯。
但只要先建立 `fit`，整段不到 0.1 秒就能跑完，所以應該直接拿掉包裹，而不是改成 `\donttest{}`。

## 修改內容

所有修改都在 roxygen 原始碼（`R/*.R`）裡完成，再用 roxygen2 7.3.2（與 DESCRIPTION 的
`RoxygenNote` 相同）重新產生 `man/*.Rd`。完整差異見 `NSCA_0.4.4-examples.diff`。

### 1. 退件信指出的問題

- **`nsca_extract()`**：移除 `\dontrun{}`，範例改為先用 `nsca_analysis()` 建立 `fit`，
  再示範兩種取值方式。

### 2. 全面檢查後一併修正的問題

逐一檢查所有匯出函式後，發現以下三個問題。它們不在這次的退件信裡，但很可能在下一輪審查被提出，所以一起修正：

- **5 個說明頁沒有 `\examples`**：`nsca()`、`nsca_plot()`（含 `plot()` 方法）、
  `nsca_results()`（含 `print()`／`summary()` 方法）、`nsca_table()`、`nsca_thresholds()`。
  CRAN 審查常見的要求是「每個匯出函式都要有可執行的小範例」，這次一併補上，全部不包裹。
- **`nsca()` 的範例時間**：`nsca()` 預設 `test.rep = 1000`，實測約 11.5 秒，超過 5 秒上限。
  範例改用 `test.rep = 50`（並加註實際使用請保留預設 1000），約 2.6 秒（其中約 2 秒是第一次載入
  NCA／SCAtools 的固定成本）。
- **`man/nsca_reference.Rd` 與原始碼不同步**：原本的 `.Rd` 被手動加上了 `\examples` 和
  更完整的 `\value`，但 `R/results.R` 的 roxygen 區塊裡沒有。只要再執行一次
  `roxygen2::roxygenise()`，這個範例就會消失。現已補回原始碼。
  （其他 `.Rd` 也都有手動改過的痕跡，但差異只在排版，例如 `\usage` 的換行，重新產生後內容不變。）

### 3. 其他

- `DESCRIPTION` 的 `Date` 更新為 2026-09-30。版本號維持 0.4.4：NSCA 尚未上架 CRAN，
  被退件後重新提交可以沿用同一版號。
- `NEWS.md` 的 0.4.4 節新增一段說明以上修改。

## 檢查過、確認沒問題的項目

- `\value`：13 個 `.Rd` 全部都有。
- `print()`／`cat()` 只出現在 `print`／`plot` 方法裡，一般函式沒有直接輸出到主控台。
- `Sys.setenv(NCA_SKIP_PURITY)` 有用 `on.exit()` 還原；`shared.seed` 會保存並還原使用者的
  `.Random.seed`。
- 沒有寫檔、沒有改 `options()`／`par()`／工作目錄，也沒有用 `T`／`F` 代替 `TRUE`／`FALSE`。
- DESCRIPTION：軟體名稱加了單引號（'NCA'、'SCAtools'），DOI 格式正確，URL 可連線。
- 相依套件 NCA 5.0.2 與 SCAtools 0.4.3 都已在 CRAN 上。

## 驗證結果

在 R 4.3.3（Ubuntu 24.04）執行 `R CMD check --as-cran --run-donttest`：

```
Status: 1 NOTE
* checking HTML version of manual ... NOTE
Skipping checking math rendering: package 'V8' unavailable
```

唯一的 NOTE 是檢查環境沒有安裝 `V8` 套件，與 NSCA 本身無關。測試：1083 個全數通過（約 55 秒）。

各範例執行時間（秒）：

| 範例 | elapsed |
| --- | --- |
| nsca | 2.64 |
| nsca_plot | 0.66 |
| nsca_results | 0.08 |
| 其他 | < 0.05 |

## 重新提交

`NSCA_0.4.4.tar.gz` 可以直接上傳到 <https://cran.r-project.org/submit.html>。
如果想在自己的電腦上重新 build，請把 diff 套用到原始碼後執行 `R CMD build`。

CRAN 表單的 "Optional comment" 欄位可以貼上：

```
This is a resubmission. In this version I have:

* Removed \dontrun{} from the nsca_extract() example. It was only there
  because the example used an object it never created; the example now
  builds that object and runs in well under a second.

* Added small executable examples to every exported function that lacked
  one (nsca(), nsca_plot(), nsca_results(), nsca_table(),
  nsca_thresholds()). None is wrapped; all run in under 5 seconds.
```
