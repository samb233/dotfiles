-- 基础配置
require("basic")

-- 快捷键映射
require("keybindings")

-- Packer 插件管理
require("plugins")

-- 插件配置
require("plugin-settings.nvim-tree")
require("plugin-settings.telescope")
require("plugin-settings.nvim-treesitter")
require("plugin-settings.nvim-autopairs")
-- require("plugin-settings.bufferline")
require("plugin-settings.lualine")
require("plugin-settings.indent-blankline")
require("plugin-settings.comment")
require("plugin-settings.gitsigns")
require("plugin-settings.nvim-treesitter-context")
require("plugin-settings.mkdnflow")
require("plugin-settings.rest")
-- require("plugin-settings.rnvimr")
require("plugin-settings.terminal")

-- 主题设置
require("colorscheme")

-- 内置LSP (新增)
require("lsp.setup")

-- cmp补全
require("lsp.cmp")

require("lsp.ui")
require("lsp.null-ls")

-- fcitx5设置
require("fcitx5")
