local status, tterm = pcall(require, "toggleterm")
if not status then
	vim.notify("没有找到 toggleterm")
	return
end

tterm.setup({
	autochdir = true,
})
