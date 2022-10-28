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
		terminal = false, -- Enable contrast for the built-in terminal
		sidebars = false, -- Enable contrast for sidebar-like windows ( for example Nvim-Tree )
		floating_windows = false, -- Enable contrast for floating windows
		cursor_line = false, -- Enable darker background for the cursor line
		non_current_windows = false, -- Enable darker background for non-current windows
		filetypes = {}, -- Specify which filetypes get the contrasted (darker) background
		-- sign_column = false, -- Enable contrast background for the sign column
		-- line_numbers = false, -- Enable contrast background for line numbers
		-- popup_menu = false, -- Enable lighter background for the popup menu
	},

	styles = { -- Give comments style such as bold, italic, underline etc.
		comments = { italic = true },
		strings = { --[[ bold = true ]]
		},
		keywords = {
			--[[ underline = true ]]
		},
		functions = { --[[ bold = true, undercurl = true ]]
		},
		variables = {},
		operators = {},
		types = {},
	},

	plugins = { -- Uncomment the plugins that you use to highlight them
		-- Available plugins:
		-- "dap",
		-- "dashboard",
		"gitsigns",
		-- "hop",
		-- "indent-blankline",
		"lspsaga",
		-- "mini",
		-- "neogit",
		"nvim-cmp",
		-- "nvim-navic",
		"nvim-tree",
		-- "sneak",
		"telescope",
		-- "trouble",
		-- "which-key",
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

	custom_colors = function(colors)
		colors.editor.fg_dark = "#B0BEC5"
		colors.backgrounds.non_current_windows = "NONE"
		colors.backgrounds.floating_windows = "NONE"
		colors.backgrounds.sidebars = "NONE"
	end, -- If you want to everride the default colors, set this to a function
})

local status_ok, _ = pcall(vim.cmd, "colorscheme " .. colorscheme)
if not status_ok then
	vim.notify("colorscheme " .. colorscheme .. " 没有找到！")
	return
end

-- vim.cmd([[highlight Normal guibg=NONE ctermbg=None]])
