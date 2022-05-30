# Linux安装配置流程

## 系统

毫无疑问ArchLinux，原因如下：
1. AUR能嫖各路软件，包括各种国内麻烦软件。
2. 在我看来，使用相对较新的软件包并不会带来不稳定，反而能解决不少历史遗留问题。

## 桌面环境选择

因为我喜欢用鼠标操作，我希望能使用鼠标完成打字外的所有操作，所以平铺窗口管理器并不适合我。

KDE与gnome中选gnome，目前来看gnome给人的感觉会更加现代一些，我尤其喜欢多虚拟桌面的逻辑与overview的设计，配上blur my shell后非常好看。

其实从完成度上来讲，KDE要高得多，gnome很多时候都给我一种半成品的感觉，会遇到一些莫名其妙的问题，如目前
- Wayland下触摸板双指滚动速度过快的问题，
- X11下睡眠无法唤醒、出现黑屏的问题
- X11下gtk4程序窗口越来越大等问题（gtk的锅，修复代码于5月26日合并）

等等，随便一列就非常多。但总体而言，gnome是一个现代化的、不断改进中的、向上的桌面环境。

## 安装

iwctl连上网，pacman -Sy更新源，archinstall，选一选，只要知道每一个选项的含义就行，傻瓜式安装

## 安装后系统配置

1. 配置sudoer：目前（22年5月份）的archinstall脚本没有相应选项将自定义用户放入sudoer中，编辑一下/etc/sudoers。
2. 配置蓝牙：
    1. sudo systemctl enable bluetooth
    2. sudo systemctl start bluetooth
    3. 配置自动启动，编辑/etc/bluetooth/main.conf，大概在配置文件末尾那一页，将autostart改为true
3. 挂载另一块硬盘作为home，我将一块盘用作系统盘，一块盘用作数据盘
    1. sudo blkid 获取磁盘UUID
    2. 编辑/etc/fstab，新增一行UUID=硬盘UUID /home ext4 defaults,relatime 0 2
4. 删除原本的home目录，sudo mv /home /home_old
5. 重启
6. 更新数据盘上字体缓存fc-cache -fv
7. 添加archlinuxcn源
8. 安装yay作为aur管理器

## 桌面环境配置

### shell

1. 安装zsh
2. 将zsh设为默认的shell：chsh -s /bin/zsh
3. 重启

因为我主目录下就有zsh的配置，所以不用其他操作。一般来说就安装ohmyzsh，然后安装代码提示以及高亮的插件即可

### 修复suspend

1. 修复ACPI设备在睡眠后立刻自动唤醒：
   1. 进入/etc/systemd/system目录，新建acpi_fix.service文件：
        ```sh
        [Unit]
        Description=fix suspend

        [Service]
        ExecStart=/bin/bash -c "echo XHCI >> /proc/acpi/wakeup"

        [Install]
        WantedBy=multi-user.target
        ```
   2. systemctl enable acpi_fix
2. 一定程度上修复X11下睡眠无法唤醒、黑屏的问题：sudo systemctl enable nvidia-suspend nvidia-resume nvidia-hibernate 
3. 重启

### 语言

1. 安装字体：noto-fonts noto-fonts-cjk noto-fonts-emoji ttf-sarasa-gothic
2. 更改/etc/locale.gen，去掉简中UTF8那一行的注释
3. 运行locale-gen
4. 更改gnome设置中的语言设定，注销重新登陆

### 输入法

选择fcitx5

1. 安装：fcitx5-im fcitx5-chinese-addons fcitx5-pinyin-moegirl fcitx5-pinyin-zhwiki fcitx5-material-color
2. 安装(aur)gnome-shell-extension-kimpanel-git
3. 编辑/etc/environment，添加：
   
   ```sh
    GTK_IM_MODULE=fcitx
    QT_IM_MODULE=fcitx
    XMODIFIERS=@im=fcitx
    INPUT_METHOD=fcitx
    SDL_IM_MODULE=fcitx
    GLFW_IM_MODULE=ibus
   ```
4. 重启

### Mutter性能优化

如果使用的是高分屏，则gnome各种动画的帧数会非常低，使用(aur)mutter-dynamic-buffering这个包来替代Mutter能大大提升帧数，另外也同样适用于1080p屏幕窗口开的非常多的情况

## 软件

- 终端模拟器：kitty
- 编辑器：neovim，(aur)visual-studio-code-bin
- 音乐播放器：(flatpak)tauon，deadbeef-git，rhythmbox+(aur)rhythmbox-plugin-alternative-toolbar 
- 浏览器：chromium，firefox
- 邮件客户端：mailspring
- 科学上网：clash for windows，v2raya
- office：(aur)wps、(aur)only-office
- 视频录制：obs-studio
- 视频剪辑：(flatpak)kdenlive
- 手机交互：gsconnect
- 

## 桌面美化
