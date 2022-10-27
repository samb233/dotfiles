local status, null_ls = pcall(require, "null-ls")
if not status then
	vim.notify("没有找到 null-ls")
	return
end

local formatting = null_ls.builtins.formatting

null_ls.setup({
	debug = false,
	sources = {
		-- Formatting ---------------------
		--  brew install shfmt
		formatting.shfmt,
		-- StyLua
		formatting.stylua,
		-- frontend
		formatting.prettier.with({ -- 只比默认配置少了 markdown
			filetypes = {
				"javascript",
				"javascriptreact",
				"typescript",
				"typescriptreact",
				"vue",
				"css",
				"scss",
				"less",
				"html",
				"json",
				"yaml",
				"graphql",
			},
			prefer_local = "node_modules/.bin",
		}),

		formatting.gofmt.with({
			filetypes = {
				"go",
			},
		}),

		formatting.goimports.with({
			filetypes = {
				"go",
			},
		}),

		formatting.rustfmt.with({
			filetypes = {
				"rust",
			},
		}),

		formatting.clang_format.with({
			filetypes = {
				"c",
				"cpp",
				"cs",
			},
		}),

		-- formatting.fixjson,
		-- formatting.black.with({ extra_args = { "--fast" } }),
	},
	-- 保存自动格式化
	on_attach = function(client)
		if client.server_capabilities.documentFormattingProvider then
			vim.cmd("autocmd BufWritePre <buffer> lua vim.lsp.buf.format()")
		end
	end,
})
