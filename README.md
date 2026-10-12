# life

Haskell cellular automata / Game of Life with **swappable B/S rules** (e.g. `B3/S23`).

Stack project: pure core library + SDL2 executable. Author style aligned with [match3](https://github.com/exsabersac/match3) (`yu binbin` / `exsabersac`, BSD-3-Clause).

## Requirements

- [Stack](https://docs.haskellstack.org/)
- System SDL2 development headers (`libsdl2-dev` on Debian/Ubuntu; `brew install sdl2` on macOS)

## Quick start

```sh
make verify          # stack test — rule parse, neighbours, step, patterns, .cells
make build           # stack build — library + life-sdl
stack run life-sdl   # SDL2 window (or: make run)
```

Optional: `LIFE_GLIDER=1 stack run life-sdl` starts with a glider.

Resolver: **lts-21.25** (GHC 9.4.8, `sdl2` from Stackage; snapshot fetched via GitHub URL so Stack does not need the USTC mirror).

## Controls (SDL)

| Key / input | Action |
|-------------|--------|
| Space | Pause / resume |
| N | Single step (pauses) |
| Tab | Cycle presets: Conway `B3/S23` → HighLife `B36/S23` → Seeds `B2/S` |
| + / - | Speed up / down |
| P | Cycle built-in patterns (centred): glider → blinker → toad → beacon → pulsar → lwss |
| G | Load glider (near origin) |
| S | Save grid to `life-save.cells` (or path in `$LIFE_SAVE`) |
| L | Load grid from the same path |
| R | Clear grid |
| Left click | Toggle cell (pauses) |
| Esc / Q | Quit |

Save format is LifeWiki-style plaintext **`.cells`**: `#` / `!` comments, `.` dead, `O` / `*` alive. Files also carry `#W` / `#H` so empty boards round-trip.

## Library layout

| Module | Role |
|--------|------|
| `Life.Types` | `Cell`, coordinates |
| `Life.Rule` | Parse / print `B…/S…`, presets |
| `Life.Grid` | Finite grid, dead-border neighbours |
| `Life.Step` | `neighborCount`, `step`, `stepN` |
| `Life.Pattern` | Named patterns + `placeOn` / `stampCentered` |
| `Life.Cells` | `encodeCells` / `decodeCells` (`.cells`) |

MVP uses a **dead border** (out-of-bounds = dead). Torus wrapping can come later.

## License

BSD-3-Clause. Copyright (c) 2026 yu binbin.
