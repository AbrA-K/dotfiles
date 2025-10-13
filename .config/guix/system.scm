;; This is an operating system configuration generated
;; by the graphical installer.
;;
;; Once installation is complete, you can learn and modify
;; this file to tweak the system configuration, and pass it
;; to the 'guix system reconfigure' command to effect your
;; changes.


;; Indicate which modules to import to access the variables
;; used in this configuration.
(use-modules (gnu)
             (gnu packages wm)
             (nongnu packages linux)
             (nongnu system linux-initrd))
(use-service-modules cups desktop networking ssh xorg sound)

(operating-system
 (kernel linux)
 (initrd microcode-initrd)
 (firmware (list linux-firmware))
 (locale "en_GB.utf8")
 (timezone "Europe/Berlin")
 (keyboard-layout (keyboard-layout "de" "neo"))
 (host-name "KadaZen")

 ;; The list of user accounts ('root' is implicit).
 (users (cons* (user-account
                (name "abra")
                (comment "abra-k")
                (group "users")
                (home-directory "/home/abra")
                (supplementary-groups '("wheel" "netdev" "audio" "video")))
               %base-user-accounts))

 ;; Packages installed system-wide.  Users can also install packages
 ;; under their own account: use 'guix search KEYWORD' to search
 ;; for packages and 'guix install PACKAGE' to install a package.
 (packages (append (specifications->packages
                    (list "niri"
                          "wireplumber"
                          "xwayland-satellite"
                          "swaynotificationcenter"
                          "waybar"
                          "emacs"
                          "librewolf"
                          "starship"
                          "stow"
                          "git"
                          "sbcl"
                          "swww"
                          "mako"
                          "tofi"
                          "fuzzel"
                          "swayidle"
                          "swaylock-effects"
                          "unzip"
                          "fzf"
                          "ripgrep"
                          "fastfetch"
                          "foot"
                          "brightnessctl"
                          "alsa-utils"
                          "font-google-noto-sans-cjk"
                          "font-google-noto-emoji"
                          "gnome-software"
                          "flatpak"
                          "zathura"
                          "zathura-pdf-poppler"
                          "zathura-ps"
                          "zathura-djvu"
                          "zathura-cb"))
                   %base-packages))

 ;; Below is the list of system services.  To search for available
 ;; services, run 'guix system search KEYWORD' in a terminal.
 (services
  (append (list (service gnome-desktop-service-type)
                (service bluetooth-service-type (bluetooth-configuration
                                                 (auto-enable? #t)))
                (service openssh-service-type)
                (service cups-service-type)
                (service screen-locker-service-type
                         (screen-locker-configuration
                          (name "swaylock")
                          (program swaylock-effects)))
                (set-xorg-configuration
                 (xorg-configuration (keyboard-layout keyboard-layout))))

          (modify-services %desktop-services
                           (alsa-service-type _ =>
                                              (alsa-configuration
                                               (pulseaudio? #f))))))
 (bootloader (bootloader-configuration
              (bootloader grub-efi-bootloader)
              (targets (list "/boot/efi"))
              (keyboard-layout keyboard-layout)))

 (mapped-devices (list (mapped-device
                          (source (uuid
                                   "55b9123e-818f-4a22-b852-43b80f81d1a7"))
                          (target "cryptroot")
                          (type luks-device-mapping))))

 (file-systems (cons* (file-system
                         (mount-point "/boot/efi")
                         (device (uuid "30CF-1F6D"
                                       'fat32))
                         (type "vfat"))
                      (file-system
                        (mount-point "/")
                        (device "/dev/mapper/cryptroot")
                        (type "ext4")
                        (dependencies mapped-devices)) %base-file-systems)))
