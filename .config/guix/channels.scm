(list
 (channel
  (name 'guix)
  (url "https://codeberg.org/guix/guix")
  (introduction
   (make-channel-introduction
    "4a16a264e919365281a8861cee53a0607be485c3"
    (openpgp-fingerprint
     "36208bf8cb7ccb13bb4e55ec8262a8614adc5f16"))))
 
 (channel
  (name 'nonguix)
  (url "https://gitlab.com/nonguix/nonguix")
  ;; Enable signature verification:
  (introduction
   (make-channel-introduction
    "897c1a470da759236cc11798f4e0a5f7d4d59fbc"
    (openpgp-fingerprint
     "2A39 3FFF 68F4 EF7A 3D29  12AF 6F51 20A0 22FB B2D5")))))
