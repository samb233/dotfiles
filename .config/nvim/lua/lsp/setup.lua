local status, mason = pcall(require, "mason")
if not status then
	vim.notify("没有找到 mason")
	return
end
-- :h mason-default-settings
mason.setup({
	ui = {
	  icons = {
		package_installed = "✓",
		package_pending = "➜",
		package_uninstalled = "✗",
	  },
	},
  })
  
  -- mason-lspconfig uses the `lspconfig` server names in the APIs it exposes - not `mason.nvim` package names
  -- https://github.com/williamboman/mason-lspconfig.nvim/blob/main/doc/server-mapping.md
local status, masonlsp = pcall(require, "mason-lspconfig")
if not status then
	vim.notify("没有找到 mason-lspconfig")
	return
end
  masonlsp.setup({
	-- 确保安装，根据需要填写
	ensure_installed = {
	  "sumneko_lua",
	  "pyright",
	  "rust_analyzer",
	  "clangd",
	  "gopls",
	},
  })


local status, lspconfig = pcall(require, "lspconfig")
if not status then
	vim.notify("没有找到 lspconfig")
	return
end
-- 安装列表
-- { key: 服务器名， value: 配置文件 }
-- key 必须为下列网址列出的 server name，不可以随便写
-- https://github.com/williamboman/nvim-lsp-installer#available-lsps
local servers = {
	sumneko_lua = require("lsp.config.lua"), -- lua/lsp/config/lua.lua
	gopls = require("lsp.config.go"),
	clangd = require("lsp.config.cpp"),
	pyright = require("lsp.config.python"),
	rust_analyzer = require("lsp.config.rust"),
  }
  
  for name, config in pairs(servers) do
	if config ~= nil and type(config) == "table" then
	  -- 自定义初始化配置文件必须实现on_setup 方法
	  config.on_setup(lspconfig[name])
	else
	  -- 使用默认参数
	  lspconfig[name].setup({})
	end
  end