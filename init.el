;; emacs bazinga dotfile by Fabio Carneiro
;; github.com/fabioaacarneiro/ebazinga
;;
;; for mac, use https://emacsformacosx.com/
;; in this case, on mac, you need create a shell
;; function to can call emacs from terminal
;;
;; emacs () {
;;     /Applications/Emacs.app/Contents/MacOS/Emacs "$@" &   
;; }

;; desativa tela inicial do emacs
(setq inhibit-startup-screen t)

;; desativa arquivos de backup
(setq make-backup-files nil)

;; desativa arquivos autosave
(setq auto-save-default nil)

;; desativa lockfiles (arquivo .#arquivo temporário)
(setq create-lockfiles nil)

;; desativa menu, barra de ferramentas e barra de rolagem
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

;; ativa numeros de linhas
(global-display-line-numbers-mode t)
(column-number-mode t)

;; usa espaços ao invés de tabs
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)

;; usa a tecla command como meta
;;(setq mac-command-modifier 'meta)
;;(setq mac-option-modifier 'none)

;; configura a fonte do emacs
(set-face-attribute 'default nil :font "Iosevka Nerd Font" :height 160)

;; usa MELPA como gerenciador de pacotes
(require 'package)
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
			 ("gnu" . "https://elpa.gnu.org/packages/")))
(package-initialize)

;; atualiza os pacotes se necessários
;; (unless package-archive-contents
;;   (package-refresh-contents))

;; instala o use-package para instalar pacotes
(require 'use-package)

;; auto pairs
(use-package smartparens
  :ensure t
  :config
  (smartparens-global-mode 1))

;; instala o doom-one tema
(use-package doom-themes
  :ensure t
  :config
  (load-theme 'doom-one t))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(all-the-icons centaur-tabs company-box consult dashboard doom-themes
                   evil-leader flycheck go-mode key-chord lsp-ui
                   neotree orderless posframe smartparens vertico
                   vterm yasnippet-snippets)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;; configurando abas
;; Instala e configura o centaur-tabs
(use-package centaur-tabs
  :ensure t
  :demand
  :hook
  (dired-mode . centaur-tabs-local-mode)
  (eshell-mode . centaur-tabs-local-mode)
  :config
  (centaur-tabs-mode t)
  (setq centaur-tabs-style "bar")
  (setq centaur-tabs-height 32)
  (setq centaur-tabs-set-icons t)
  (setq centaur-tabs-set-bar 'under)
  (setq centaur-tabs-set-close-button nil)
  (setq centaur-tabs-set-modified-marker t)
  (setq centaur-tabs-show-new-tab-button nil)
  (setq centaur-tabs-cycle-scope 'tabs)

  ;; Define grupos automáticos (opcional)
  (centaur-tabs-group-by-projectile-project)

  ;; Teclas para navegar entre abas com evil-mode
  (with-eval-after-load 'evil
    (define-key evil-normal-state-map (kbd "H") 'centaur-tabs-backward)
    (define-key evil-normal-state-map (kbd "L") 'centaur-tabs-forward)))

;; configura seletor de buffers
(use-package consult :ensure t)

;; janelas flutuantes
(use-package posframe :ensure t)

;; adiciona recurso de completação
(use-package orderless
  :ensure t
  :custom
  (completion-styles '(orderless basic))
  (completion-category-override '((file (styles basic partial-completion)))))

;; interface de completação vertical
(use-package vertico
  :ensure t
  :init
  (vertico-mode)
  :bind (:map vertico-map
              ("<tab>" . vertico-next)
              ("<backtab>" . vertico-previous)))

;; instala o evil mode - binds do vim
(use-package evil
  :ensure t
  :config
  (evil-mode 1))

;; instala o evil leader para usar <space>
(use-package evil-leader
  :ensure t
  :config
  (global-evil-leader-mode)
  (evil-leader/set-leader "<SPC>"))

;; Fecha a janela de completions sem sair do modo insert (Evil)
(define-key evil-insert-state-map (kbd "C-e") 
  (lambda () 
    (interactive)
    (if (and (bound-and-true-p company-mode) (company--active-p))
        (company-abort)
      (message "No active completion"))))

;; configura cursor por modo - configurado no kitty
(defun my/set-cursor-type ()
  (let ((cursor-shape
         (pcase evil-state
           ('normal "\e[2 q")   ;; bloco
           ('insert "\e[6 q")   ;; barra
           ('visual "\e[2 q")   ;; bloco
           ('replace "\e[4 q")  ;; underline
           (_ "\e[2 q"))))      ;; fallback: bloco
    (send-string-to-terminal cursor-shape)))

(add-hook 'post-command-hook #'my/set-cursor-type)

;; instala ícones para neotree
(use-package all-the-icons
  :ensure t)

;; instala o neotree
(use-package neotree
  :ensure t
  :config
  (setq neo-theme 'icons)
  (setq neo-smart-open t)
  (setq new-window-fixed-size nil))

;; abrir o neotree com <space>e com evil-leader
(with-eval-after-load 'evil-leader
  (evil-leader/set-key
    "e" 'neotree-toggle))

;; atalhos para navegar no neotree com evil-mode
(with-eval-after-load 'neotree
  (add-hook 'neotree-mode-hook
            (lambda ()
              (evil-define-key 'normal neotree-mode-map (kbd "TAB") 'neotree-enter)
              (evil-define-key 'normal neotree-mode-map (kbd "RET") 'neotree-enter)
              (evil-define-key 'normal neotree-mode-map (kbd "e") 'neotree-exit)
              (evil-define-key 'normal neotree-mode-map (kbd "h") 'neotree-toggle-hidden)
              (evil-define-key 'normal neotree-mode-map (kbd "R") 'neotree-refresh)
              (evil-define-key 'normal neotree-mode-map (kbd "U") 'neotree-select-up-node)
              (evil-define-key 'normal neotree-mode-map (kbd "SR") 'neotree-change-root)
              (evil-define-key 'normal neotree-mode-map (kbd "o") 'neotree-enter)
              (evil-define-key 'normal neotree-mode-map (kbd "d") 'neotree-delete-node)
              (evil-define-key 'normal neotree-mode-map (kbd "r") 'neotree-rename-node)
              (evil-define-key 'normal neotree-mode-map (kbd "a") 'neotree-create-node))))


;; key-chord para detectar teclas apertando rapido
(use-package key-chord
  :ensure t
  :config
  (key-chord-mode 1)
  (key-chord-define evil-insert-state-map "jk" 'evil-normal-state))

;; autocomplete
(use-package company
  :ensure t
  :hook (after-init . global-company-mode)
  :config
  (setq company-minimum-prefix-length 1
        company-idle-delay 0.0))
  
;; melhora o visual do company
(use-package company-box
  :ensure t
  :hook (company-mode . company-box-mode))

;; snippets
(use-package yasnippet
  :ensure t
  :hook (prog-mode . yas-minor-mode))

(use-package yasnippet-snippets
  :ensure t)

;; configuração para LSP
(use-package lsp-mode
  :ensure t
  :commands lsp
  :hook ((go-mode . lsp)
         (js-mode . lsp)
         (typescript-ts-mode . lsp))
  :config
  (setq lsp-completion-provider :capf)) 

(use-package lsp-ui
  :ensure t
  :commands lsp-ui-mode lsp-ui-peek-find-references lsp-ui-peek-find-definitions
  :after lsp-mode
  :hook (lsp-mode . lsp-ui-mode)
  :config
  (setq lsp-ui-doc-enable t)
  (setq lsp-ui-sideline-enable t)
  (setq lsp-ui-sideline-show-diagnostics t)
  (setq lsp-ui-sideline-show-code-actions t)
  (setq lsp-signature-auto-active t)
  (setq lsp-signature-doc-lines 1)
  (setq lsp-ui-sideline-show-hover t))

(setq company-backends '(company-capf company-files))
(setq lsp-signature-function 'lsp-signature-posframe)

;; which-key ajuda a ver os atalhos
(use-package which-key
  :ensure t
  :config
  (which-key-mode))

;; configura o lsp do go
(setenv "PATH" (concat (getenv "PATH") ":/Users/fabio/go/bin"))
(add-to-list 'exec-path "/Users/fabio/go/bin")

(use-package go-mode
  :ensure t)

(add-hook 'go-mode-hook #'company-mode)
(add-hook 'go-mode-hook #'lsp)

(defun lsp-go-install-save-hooks ()
  (add-hook 'before-save-hook #'lsp-format-buffer t t)
  (add-hook 'before-save-hook #'lsp-organize-imports t t))

(add-hook 'go-mode-hook #'lsp-go-install-save-hooks)

;; ativar seleção para o clipboard
(setq x-select-enable-clipboard t)
(setq select-enable-clipboard t)

;; sempre usa o clipboard com yank/copy
(defun my/copy-to-clipboard ()
  (interactive)
  (if (use-region-p)
      (progn
        (kill-new (buffer-substring (region-beginning) (region-end)))
        (message "Copied to clipboard"))
    (message "No region active")))

(define-key evil-visual-state-map (kbd "C-c C-y") 'my/copy-to-clipboard)

;; diagnostics
(use-package flycheck
  :ensure t
  :init (global-flycheck-mode))

(setq lsp-diagnostic-package :flycheck)
(setq lsp-lens-enable t)
(setq lsp-inlay-hint-enable t)
(add-hook 'lsp-mode-hook #'lsp-lens-mode)
(add-hook 'lsp-mode-hook #'lsp-inlay-hints-mode)

;; terminal embutido
(use-package vterm
  :ensure t)

;; dashboard (please, don't try organize title ascii, this is organized on emacs)
(use-package dashboard
  :ensure t
  :config
  (dashboard-setup-startup-hook)
  (setq dashboard-banner-logo-title "
                   ___           ___                       ___           ___           ___
    _____         /  /\\         /  /\\        ___          /__/\\         /  /\\         /  /\\
   /  /::\\       /  /::\\       /  /::|      /  /\\         \\  \\:\\       /  /:/_       /  /::\\
  /  /:/\\:\\     /  /:/\\:\\     /  /:/:|     /  /:/          \\  \\:\\     /  /:/ /\\     /  /:/\\:\\
 /  /:/~/::\\   /  /:/~/::\\   /  /:/|:|__  /__/::\\      _____\\__\\:\\   /  /:/_/::\\   /  /:/~/::\\
/__/:/ /:/\\:| /__/:/ /:/\\:\\ /__/:/ |:| /\\ \\__\\/\\:\\__  /__/::::::::\\ /__/:/__\\/\\:\\ /__/:/ /:/\\:\\
\\  \\:\\/:/~/:/ \\  \\:\\/:/__\\/ \\__\\/  |:|/:/    \\  \\:\\/\\ \\  \\:\\~~\\~~\\/ \\  \\:\\ /~~/:/ \\  \\:\\/:/__\\/
 \\  \\::/ /:/   \\  \\::/          |  |:/:/      \\__\\::/  \\  \\:\\  ~~~   \\  \\:\\  /:/   \\  \\::/
  \\  \\:\\/:/     \\  \\:\\          |  |::/       /__/:/    \\  \\:\\        \\  \\:\\/:/     \\  \\:\\
   \\  \\::/       \\  \\:\\         |  |:/        \\__\\/      \\  \\:\\        \\  \\::/       \\  \\:\\
    \\__\\/         \\__\\/         |__|/                     \\__\\/         \\__\\/         \\__\\/

"
        dashboard-startup-banner 'logo
        dashboard-items '((recents . 5)
                          (projects . 5)
                          (agenda . 5)))
  (setq dashboard-set-heading-icons t)
  (setq dashboard-set-file-icons t))

;; função para abrir o vterm em baixo
(defun my/toggle-vterm ()
  "Alterna a visibilidade do vterm em uma janela inferior."
  (interactive)
  (let ((buf (get-buffer "*vterm*")))
    (if (and buf (get-buffer-window buf))
        ;; Se o vterm estiver visível, fecha a janela
        (delete-window (get-buffer-window buf))
      ;; Senão, cria ou mostra o vterm na parte de baixo
      (progn
        (let ((win (split-window-vertically -15)))
          (select-window win)
          (if buf
              (switch-to-buffer buf)
            (vterm)))))))

;; bind para code action do lsp
(evil-leader/set-key
  ;; code action
  "aa" 'lsp-execute-code-action
  ;; organize imports
  "ao" 'lsp-organize-imports
  ;; renomear
  "ar" 'lsp-rename
  ;; formatar o buffer
  "af" 'lsp-format-buffer
  ;; assinatura de método
  "gk" (lambda () (interactive) (lsp-signature-activate)) 
  ;; descrição
  "gh" 'lsp-describe-thing-at-point
  ;; ir para definição
  "gd" 'lsp-ui-peek-find-definitions
  ;; ir para referencia
  "gr" 'lsp-ui-peek-find-references
  ;; ir para implementação
  "gi" 'lsp-ui-peek-find-implementation
  ;; ir para ambiente de trabalho
  "gs" 'lsp-ui-peek-find-workspace-symbol
  ;; temrinal integrado
  "t" 'my/toggle-vterm
  ;; fechar o buffer aberto
  "bd" (lambda () (interactive) (kill-this-buffer))
  ;; abre lista de buffers
  "bl" (lambda () (interactive) (consult-buffer))
  "/" 'consult-ripgrep)
