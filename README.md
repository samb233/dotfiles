# Linux 安装配置流程

## 系统

毫无疑问ArchLinux。因为AUR能嫖各路软件。

## 桌面环境选择

因为我喜欢用鼠标操作，工作外甚至希望能使用鼠标完成所有操作，所以平铺窗口管理器并不适合我。

KDE与gnome中选gnome，目前来看gnome给人的感觉会更加现代一些，我尤其喜欢多虚拟桌面的逻辑与overview的设计。尽管从完成度上来讲，KDE要高得多，gnome很多时候都给我一种半成品的感觉。

## 安装

iwctl连上网，pacman -Sy更新源，archinstall，选一选，傻瓜式安装

## 安装后系统配置

1. 配置sudoer：目前（22年5月份）的archinstall脚本没有将自定义用户放入sudoer中，编辑一下/etc/sudoers。
2. 配置蓝牙：
    1. sudo systemctl enable bluetooth
    2. sudo systemctl start bluetooth
    3. 配置自动启动，编辑/etc/bluetooth/main.conf，大概在配置文件末尾那一页，将autostart改为true
3. 挂载另一块硬盘作为home，我将一块盘用作系统盘，一块盘用作数据盘
    1. sudo blkid 获取磁盘UUID
    2. 编辑/etc/fstab，新增一行UUID=硬盘UUID /home ext4 defaults,relatime 0 2
4. 删除原本的home目录，sudo mv /home /home_old
5. 重启

## 软件

### gnome插件
