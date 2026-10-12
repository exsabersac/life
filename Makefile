# life — Stack 桌面版入口（仓库根目录运行 `make <目标>`）
SHELL := /bin/sh
.DEFAULT_GOAL := help
.NOTPARALLEL:

ROOT := $(patsubst %/,%,$(dir $(abspath $(lastword $(MAKEFILE_LIST)))))
EXE  := life-sdl

.PHONY: help verify build run clean

##@ 通用

help: ## 显示本帮助（默认目标）
	@echo "用法：make <目标>（在仓库根目录运行）"
	@awk 'BEGIN { FS = ":[^#]*## " } /^##@ / { printf "\n%s\n", substr($$0, 5); next } /^[a-z][a-z0-9-]*:.*## / { printf "  %-12s %s\n", $$1, $$2 }' "$(ROOT)/Makefile"
	@echo

##@ 桌面版（Stack，SDL2）

verify: ## 核心规则测试（stack test）
	cd "$(ROOT)" && stack test

build: ## 构建库 + SDL2 可执行文件（stack build）
	cd "$(ROOT)" && stack build

run: ## 运行 SDL2 壳（stack run life-sdl；需显示器 / 可用 SDL 视频驱动）
	cd "$(ROOT)" && stack run $(EXE)

clean: ## 清理 Stack 产物
	cd "$(ROOT)" && stack clean
