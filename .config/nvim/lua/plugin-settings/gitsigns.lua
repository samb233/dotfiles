local status, gitsigns = pcall(require, "gitsigns")
if not status then
	vim.notify("没有找到 Comment")
	return
end

gitsigns.setup({})
