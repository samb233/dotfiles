-- local status, catppuccin = pcall(require, "catppuccin")
-- if not status then
-- 	vim.notify("没有找到 catppuccin")
-- 	return
-- end
--
-- catppuccin.setup({
-- 	transparent_background = false,
-- 	term_colors = true,
-- 	styles = {
-- 		comments = "italic",
-- 		functions = "italic,bold",
-- 		keywords = "italic",
-- 		strings = "NONE",
-- 		variables = "NONE",
-- 	},
--
-- 	integrations = {
-- 		nvimtree = {
-- 			enabled = true,
-- 			show_root = false,
-- 			transparent_panel = true,
-- 		},
-- 	},
-- })

local status, ayu = pcall(require, "ayu")
if not status then
	vim.notify("没有找到 ayu")
	return
end

ayu.setup({
	mirage = true,
})

local colorscheme = "ayu"

local status_ok, _ = pcall(vim.cmd, "colorscheme " .. colorscheme)
if not status_ok then
	vim.notify("colorscheme " .. colorscheme .. " 没有找到！")
	return
end

-- local status, transparent = pcall(require, "transparent")
-- if not status then
-- 	vim.notify("没有找到 transparent")
-- 	return
-- end
--
-- transparent.setup({
-- 	enable = true, -- boolean: enable transparent
-- 	extra_groups = { -- table/string: additional groups that should be cleared
-- 		-- In particular, when you set it to 'all', that means all available groups
--
-- 		-- example of akinsho/nvim-bufferline.lua
-- 		"BufferLineTabClose",
-- 		"BufferlineBufferSelected",
-- 		"BufferLineFill",
-- 		"BufferLineBackground",
-- 		"BufferLineSeparator",
-- 		"BufferLineIndicatorSelected",
-- 	},
-- 	exclude = {}, -- table: groups you don't want to clear
-- })

-- vim.cmd([[highlight Normal guibg=NONE ctermbg=None]])
