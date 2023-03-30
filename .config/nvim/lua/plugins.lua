local packer = require("packer")
packer.startup({
	function(use)
		-- Packer
		use("wbthomason/packer.nvim")

		-- nvim-tree
		use({ "nvim-tree/nvim-tree.lua", requires = "nvim-tree/nvim-web-devicons" })

		-- treesitter （新增）
		use({ "nvim-treesitter/nvim-treesitter", run = ":TSUpdate" })
		use("nvim-treesitter/nvim-treesitter-context")
		-- telescope
		use({ "nvim-telescope/telescope.nvim", requires = { "nvim-lua/plenary.nvim" } })
		use({ "nvim-telescope/telescope-file-browser.nvim" })

		-- 寻找project目录
		use({ "airblade/vim-rooter" })

		-- comment
		use("numToStr/Comment.nvim")

		-- autopairs
		use("windwp/nvim-autopairs")
		use("windwp/nvim-ts-autotag")

		-- 主题
		use("rebelot/kanagawa.nvim")
		use("marko-cerovac/material.nvim")
		use({ "mcchrish/zenbones.nvim", requires = "rktjmp/lush.nvim" })

		-- lualine
		use({ "nvim-lualine/lualine.nvim", requires = { "kyazdani42/nvim-web-devicons" } })
		use("arkav/lualine-lsp-progress")

		-- bufferline
		use({ "akinsho/bufferline.nvim", requires = { "kyazdani42/nvim-web-devicons", "moll/vim-bbye" } })

		-- indent-blankline
		use("lukas-reineke/indent-blankline.nvim")

		--terminal
		use("akinsho/toggleterm.nvim")

		-- git
		use({
			"lewis6991/gitsigns.nvim",
			-- tag = 'release' -- To use the latest release
		})

		-- markdown
		use({ "jakewvincent/mkdnflow.nvim" })

		-- rest client
		use({ "rest-nvim/rest.nvim" })

		--------------------- LSP --------------------
		-- lspconfig
		use({ "neovim/nvim-lspconfig", "williamboman/mason.nvim" })
		use({ "williamboman/mason-lspconfig.nvim" })

		-- ui
		use("onsails/lspkind-nvim")

		-- use("tami5/lspsaga.nvim")
		use("glepnir/lspsaga.nvim")

		-- 补全引擎
		use("hrsh7th/nvim-cmp")
		-- snippet 引擎
		use("hrsh7th/vim-vsnip")
		-- 补全源
		use("hrsh7th/cmp-vsnip")
		use("hrsh7th/cmp-nvim-lsp")
		use("hrsh7th/cmp-buffer") -- { name = 'buffer' },
		use("hrsh7th/cmp-path") -- { name = 'path' }
		use("hrsh7th/cmp-cmdline") -- { name = 'cmdline' }

		-- 常见编程语言代码段
		use("rafamadriz/friendly-snippets")

		-- 代码格式化
		use({ "jose-elias-alvarez/null-ls.nvim", requires = "nvim-lua/plenary.nvim" })

		use({ "ThePrimeagen/vim-be-good" })

		-- 文件浏览
		-- use({ "kevinhwang91/rnvimr" })
	end,
	config = {
		-- 并发数限制
		max_jobs = 3,

		-- 自定义源
		git = {
			-- default_url_format = "https://hub.fastgit.xyz/%s",
			-- default_url_format = "https://mirror.ghproxy.com/https://github.com/%s",
			-- default_url_format = "https://gitcode.net/mirrors/%s",
			-- default_url_format = "https://gitclone.com/github.com/%s",
		},

		-- 以浮动窗口打开安装列表
		display = {
			open_fn = function()
				return require("packer.util").float({ border = "single" })
			end,
		},
	},
})
