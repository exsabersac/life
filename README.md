# life

Haskell 元胞自动机 / 生命游戏，支持**可换 B/S 规则**（例如 `B3/S23`）。

技术栈：**Stack + 纯核心库 + SDL2 可执行文件**。世代推进是惰性无穷流 `Life.Stream`（`generations`）；SDL 壳把纯 `Input` 折叠进 `Life.World`，再用 `tick` / `splitAtS` 抽样前进。写法对齐 [match3](https://github.com/exsabersac/match3) 的 `Engine.Stream`（作者 `yu binbin` / `exsabersac`，BSD-3-Clause）。

## 依赖

- [Stack](https://docs.haskellstack.org/)
- 系统 SDL2 开发头文件（Debian/Ubuntu：`libsdl2-dev`；macOS：`brew install sdl2`）

## 快速开始

```sh
make verify          # stack test — 规则解析、邻域、步进、图案、.cells
make build           # stack build — 库 + life-sdl
stack run life-sdl   # SDL2 窗口（或：make run）
```

可选环境变量：

| 变量 | 含义 |
|------|------|
| `LIFE_GLIDER=1` | 启动时放入滑翔机 |
| `LIFE_SAVE` | 存档路径（默认 `life-save.cells`） |

Resolver：**lts-21.25**（GHC 9.4.8；`sdl2` 来自 Stackage；快照经 GitHub URL 拉取，不依赖 USTC 镜像）。

## 操作（SDL）

| 按键 / 输入 | 作用 |
|-------------|------|
| 空格 | 暂停 / 继续 |
| N | 单步（并暂停） |
| Tab | 循环预设规则：Conway `B3/S23` → HighLife `B36/S23` → Seeds `B2/S` |
| + / - | 加速 / 减速 |
| P | 循环内置图案（居中）：滑翔机 → 闪烁器 → 蟾蜍 → 信标 → 脉冲星 → LWSS |
| G | 加载滑翔机（靠近原点） |
| S | 保存网格到 `life-save.cells`（或 `$LIFE_SAVE`） |
| L | 从同一路径加载 |
| R | 清空网格 |
| 左键点击 | 切换单元格（并暂停） |
| Esc / Q | 退出 |

存档格式为 LifeWiki 风格明文 **`.cells`**：`#` / `!` 为注释，`.` 死，`O` / `*` 活。文件还带 `#W` / `#H`，空盘也能往返。

## 模块布局

| 模块 | 职责 |
|------|------|
| `Life.Types` | `Cell`、坐标类型 |
| `Life.Rule` | 解析 / 打印 `B…/S…`、预设 |
| `Life.Grid` | 有限网格、死边界邻域 |
| `Life.Stream` | 惰性无穷流（`:>`、`iterateS`、`splitAtS` 等） |
| `Life.Step` | `neighborCount`、`step`、`stepN`、`generations` |
| `Life.World` | 纯 `World` / `Input` / `applyInput` / `tick` |
| `Life.Pattern` | 命名图案 + `placeOn` / `stampCentered` |
| `Life.Cells` | `encodeCells` / `decodeCells`（`.cells`） |

### 流式设计说明

核心对齐 match3 的 `Engine.Stream`：用无空构造的惰性无穷流表示世代序列。

- `generations rule g0` ≡ `g0 :> step rule g0 :> …`（`iterateS`）
- 单步 / 多步用 `splitAtS` 取前缀，只强制已消费部分，其余保持 thunk
- `World.tick` 在未暂停时按帧时间累计，再从流上跳过 `n` 代；SDL 只负责轮询、折叠 `Input`、绘制

MVP 使用**死边界**（越界视为死）。环面边界可后续再加。

## 许可

BSD-3-Clause。Copyright (c) 2026 yu binbin。
