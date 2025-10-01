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
                     (list "river"
			   "sandbar"
                           "emacs"
			   "librewolf"
			   "starship"
			   "stow"
			   "git"
                           "sbcl"
                           "swww"
                           "mako"
                           "tofi"
                           "swayidle"
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

                 (service openssh-service-type)
                 (service cups-service-type)
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
  (swap-devices (list (swap-space
                        (target (uuid
                                 "12fff4a3-7295-43b7-9efb-2d75d93fc351")))))

  ;; The list of file systems that get "mounted".  The unique
  ;; file system identifiers there ("UUIDs") can be obtained
  ;; by running 'blkid' in a terminal.
  (file-systems (cons* (file-system
                         (mount-point "/")
                         (device (uuid
                                  "4529e3de-ddc5-4a1f-84da-e7c50439b95f"
                                  'ext4))
                         (type "ext4"))
                       (file-system
                         (mount-point "/boot/efi")
                         (device (uuid "F1F2-BE63"
                                       'fat32))
                         (type "vfat")) %base-file-systems)))
