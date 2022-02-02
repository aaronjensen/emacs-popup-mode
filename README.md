# popup-mode for Emacs

Unofficially extracted and made standalone from
[doom-emacs](https://github.com/hlissner/doom-emacs). All credit goes to
[hlissner](https://github.com/hlissner).

## Example Configuration

```el
(use-package popup-mode
  :demand t
  :hook (after-init . +popup-mode)

  :straight (popup-mode :host github :repo "aaronjensen/emacs-popup-mode")
  :bind (("C-`" . +popup/toggle))
  :config
  (set-popup-rules! '(("^\\*Process List\\*$"
                       :side bottom :select t :slot -1 :vslot -1 :size +popup-shrink-to-fit)
                      ("^\\*Buffer List\\*$"
                       :side bottom :select t :slot -1 :vslot -1 :size +popup-shrink-to-fit)
                      ("^\\*ruby-compilation\\*$"
                       :side bottom :select nil :slot -1 :vslot -1 :height 0.4 :ttl nil)
                      ("^\\*Messages\\*$"
                       :side bottom :select t :slot -1 :vslot -1 :height 0.3 :ttl nil))))
```
