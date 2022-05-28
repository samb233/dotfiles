local colorscheme = "material"

vim.g.material_style = "darker"

-- local status, kanagawa = pcall(require, "kanagawa")
-- if not status then
-- 	vim.notify("没有找到 kanagawa")
-- 	return
-- end
--
-- kanagawa.setup({
-- 	transparent = true,
-- 	globalStatus = true,
-- })

local status, material = pcall(require, "material")
if not status then
	vim.notify("没有找到material")
	return
end

material.setup({

	contrast = {
		sidebars = false, -- Enable contrast for sidebar-like windows ( for example Nvim-Tree )
		floating_windows = false, -- Enable contrast for floating windows
		line_numbers = false, -- Enable contrast background for line numbers
		sign_column = false, -- Enable contrast background for the sign column
		cursor_line = false, -- Enable darker background for the cursor line
		non_current_windows = false, -- Enable darker background for non-current windows
		popup_menu = false, -- Enable lighter background for the popup menu
	},

	italics = {
		comments = true, -- Enable italic comments
		keywords = false, -- Enable italic keywords
		functions = false, -- Enable italic functions
		strings = false, -- Enable italic strings
		variables = false, -- Enable italic variables
	},

	contrast_filetypes = { -- Specify which filetypes get the contrasted (darker) background
		--"terminal", -- Darker terminal background
		-- "packer", -- Darker packer background
		"qf", -- Darker qf list background
		"folded",
	},

	high_visibility = {
		lighter = false, -- Enable higher contrast text for lighter style
		darker = true, -- Enable higher contrast text for darker style
	},

	disable = {
		colored_cursor = false, -- Disable the colored cursor
		borders = true, -- Disable borders between verticaly split windows
		background = true, -- Prevent the theme from setting the background (NeoVim then uses your teminal background)
		term_colors = false, -- Prevent the theme from setting terminal colors
		eob_lines = false, -- Hide the end-of-buffer lines
	},

	lualine_style = "stealth", -- Lualine style ( can be 'stealth' or 'default' )

	async_loading = true, -- Load parts of the theme asyncronously for faster startup (turned on by default)

	custom_highlights = {
		-- highlight folded
		Folded = { bg = "#222222" },
	}, -- Overwrite highlights with your own
})

local status_ok, _ = pcall(vim.cmd, "colorscheme " .. colorscheme)
if not status_ok then
	vim.notify("colorscheme " .. colorscheme .. " 没有找到！")
	return
end

-- vim.cmd([[highlight Normal guibg=NONE ctermbg=None]])
