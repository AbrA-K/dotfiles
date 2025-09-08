(in-package #:nyxt-user)

;; Define buffer search-engines slot to be a list of several
;; nx-search-engines-provided ones.
(define-configuration (buffer web-buffer)
  ((search-engines (list (engines:google :shortcut "gmaps"
                                         :object :maps)
                         (engines:google :shortcut "g"
                                         :safe-search nil)
                         (engines:startpage :object :web
                                            :family-filter nil)))))
