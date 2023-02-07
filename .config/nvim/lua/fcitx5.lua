vim.cmd([[let fcitx5state=system("fcitx5-remote")]])
vim.cmd([[autocmd InsertLeave * :silent let fcitx5state=system("fcitx5-remote")[0] | silent !fcitx5-remote -c]])
vim.cmd([[autocmd InsertEnter * :silent if fcitx5state == 2 | call system("fcitx5-remote -o") | endif]])
