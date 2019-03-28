
                                        ;TODO magit for cabal

;;; UI

;;;###autoload (autoload 'hs-dispatch-popup "hs" nil t)
(hs-define-popup hs-dispatch-popup

  "Popup console for dispatching other popups."

  :actions '("Popup and dwim commands"

             ;; (?a "" hs-cabal-)
             (?b "Build" hs-cabal-build)
             (?c "Configure" hs-cabal-configure)
             ;; (?d "" hs-cabal-)
             ;; (?e "" hs-cabal-)
             ;; (?f "" hs-cabal-)
             ;; (?g "" hs-cabal-)
             (?h "Show help" hs-cabal-help)
             ;; (?i "" hs-cabal-)
             ;; (?j "" hs-cabal-)
             ;; (?k "" hs-cabal-)
             ;; (?l "" hs-cabal-)
             ;; (?m "" hs-cabal-)
             ;; (?n "" hs-cabal-)
             ;; (?o "" hs-cabal-)
             ;; (?p "" hs-cabal-)
             ;; (?q "" hs-cabal-)
             ;; (?r "" hs-cabal-)
             ;; (?s "" hs-cabal-)
             (?t "Test" hs-cabal-test)
             (?u "Upload" hs-cabal-upload)
             (?v "Show version" hs-cabal-version)
             ;; (?w "" hs-cabal-)
             ;; (?x "" hs-cabal-)
             ;; (?y "" hs-cabal-)
             ;; (?z "" hs-cabal-)

             ;; (?A "" hs-cabal-)
             ;; (?B "" hs-cabal-)
             ;; (?C "" hs-cabal-)
             ;; (?D "" hs-cabal-)
             ;; (?E "" hs-cabal-)
             ;; (?F "" hs-cabal-)
             ;; (?G "" hs-cabal-)
             ;; (?H "" hs-cabal-)
             ;; (?I "" hs-cabal-)
             ;; (?J "" hs-cabal-)
             ;; (?K "" hs-cabal-)
             ;; (?L "" hs-cabal-)
             ;; (?M "" hs-cabal-)
             ;; (?N "" hs-cabal-)
             ;; (?O "" hs-cabal-)
             ;; (?P "" hs-cabal-)
             ;; (?Q "" hs-cabal-)
             ;; (?R "" hs-cabal-)
             ;; (?S "" hs-cabal-)
             ;; (?T "" hs-cabal-)
             ;; (?U "" hs-cabal-)
             ;; (?V "" hs-cabal-)
             ;; (?W "" hs-cabal-)
             ;; (?X "" hs-cabal-)
             ;; (?Y "" hs-cabal-)
             ;; (?Z "" hs-cabal-)
             
             )

;; $ cabal --version 
