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
(set-face-attribute 'default nil :font "Iosevka Nerd Font" :height 110)

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
  (setq centaur-tabs-cycle-scope 'tabs))

;; Define grupos automáticos (opcional)
(centaur-tabs-group-by-projectile-project)

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

;; Fecha a janela de completions do company
(global-set-key (kbd "C-e") 
  (lambda () 
    (interactive)
    (if (and (bound-and-true-p company-mode) (company--active-p))
        (company-abort)
      (end-of-line))))

;; instala ícones para neotree
(use-package all-the-icons
  :ensure t)

;; instala o neotree
(use-package neotree
  :ensure t
  :config
  (setq neo-theme 'icons)
  (setq neo-smart-open nil)
  (setq neo-show-hidden-files t)
  (setq neo-window-width 45)
  (setq new-window-fixed-size nil))
 
(defun my/neotree-refresh-on-file-change ()
  "Refresh NeoTree if it's visible."
  (when (neo-global--window-exists-p)
    (neotree-refresh)))

(add-hook 'after-save-hook #'my/neotree-refresh-on-file-change)
(add-hook 'after-revert-hook #'my/neotree-refresh-on-file-change)
(add-hook 'dired-after-readin-hook #'my/neotree-refresh-on-file-change)

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

;; configuração de formatação do ruby
(defun my-ruby-format ()
  (when (eq major-mode 'ruby-mode)
    (shell-command-to-string (format "standardrb --fix %s" (shell-quote-argument buffer-file-name)))
    (revert-buffer t t t)))

(add-hook 'after-save-hook 'my-ruby-format)

;; melhora sintaxe para ruby
(use-package enh-ruby-mode
  :ensure t
  :mode "\\.rb\\'"
  :interpreter "ruby"
  :hook (enh-ruby-mode . lsp))

;; configura lsp para php
(use-package php-mode
  :ensure t
  :mode "\\.php\\'"
  :hook (php-mode . lsp))

;; configuração para LSP
(use-package lsp-mode
  :ensure t
  :commands lsp
  :hook ((go-mode . lsp)
         (js-mode . lsp)
         (ruby-mode . lsp)
         (php-mode . lsp)
         (typescript-ts-mode . lsp))
  :config
  (setq lsp-intelephense-multi-root nil)
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

;; Instalar e configurar o git-gutter
(use-package git-gutter
  :ensure t
  :config
  (global-git-gutter-mode +1)  ;; Ativa o modo global do git-gutter
  (setq git-gutter:update-interval 2)  ;; Atualiza a cada 2 segundos
  ;; Atalhos para navegar entre as alterações
  (global-set-key (kbd "C-c C-n") 'git-gutter:next-hunk)  ;; Próxima alteração
  (global-set-key (kbd "C-c C-p") 'git-gutter:previous-hunk)  ;; Alteração anterior
  (global-set-key (kbd "C-c C-s") 'git-gutter:stage-hunk)  ;; Stage da alteração
  (global-set-key (kbd "C-c C-r") 'git-gutter:revert-hunk)  ;; Reverter a alteração
)

;; ícones personalizados para os status
;; graphic icons
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(git-gutter:added-sign "🌱")
 '(git-gutter:deleted-sign "🗑️")
 '(git-gutter:modified-sign "✏️")
 '(git-gutter:window-width 2)
 '(package-selected-packages nil))
  ;; Arquivo deletado

;; Instalar e configurar o magit
(use-package magit
  :ensure t)

;; adcionar multiplos cursores
(use-package multiple-cursors
  :ensure t
  :bind
  (("C-c m c" . mc/edit-lines)
   ("C-c m n" . mc/mark-next-like-this)
   ("C-c m p" . mc/mark-previous-like-this)
   ("C-c m a" . mc/mark-all-like-this)))

;; facilia a movimentação entre as janelas abertas
(use-package ace-window
  :ensure t
  :bind ("M-o" . ace-window))

;; aumenta o tamanho do número da janela e deixa verde
(custom-set-faces
 '(aw-leading-char-face
   ((t (:inherit ace-jump-face-foreground
                 :height 2.0
                 :foreground "green"
                 :weight bold)))))

;; atalhos para facilitar redimensionar as janelas

(global-set-key (kbd "C-s-<left>")  'shrink-window-horizontally)
(global-set-key (kbd "C-s-<right>") 'enlarge-window-horizontally)
(global-set-key (kbd "C-s-<down>")  'shrink-window)
(global-set-key (kbd "C-s-<up>")    'enlarge-window)

;; Atalhos para magit
(global-set-key (kbd "C-c g g") 'magit-status)  ;; Status do repositório
(global-set-key (kbd "C-c g l") 'magit-log)  ;; Log de commits
(global-set-key (kbd "C-c g b") 'magit-blame)  ;; Blame
(global-set-key (kbd "C-c g c") 'magit-commit)  ;; Commit
(global-set-key (kbd "C-c g p") 'magit-push)  ;; Push

;; comentar e descomentar linhas
(global-set-key (kbd "C-c c") 'comment-line)
(global-set-key (kbd "C-c u") 'comment-or-uncomment-region)
 
;; Neotree
(global-set-key (kbd "C-c o") 'neotree-toggle)

;; LSP actions
(global-set-key (kbd "C-c a a") 'lsp-execute-code-action)
(global-set-key (kbd "C-c a o") 'lsp-organize-imports)
(global-set-key (kbd "C-c a r") 'lsp-rename)
(global-set-key (kbd "C-c a f") 'lsp-format-buffer)

;; LSP infos
(global-set-key (kbd "C-c l k") (lambda () (interactive) (lsp-signature-activate)))
(global-set-key (kbd "C-c l h") 'lsp-describe-thing-at-point)
(global-set-key (kbd "C-c l d") 'lsp-ui-peek-find-definitions)
(global-set-key (kbd "C-c l r") 'lsp-ui-peek-find-references)
(global-set-key (kbd "C-c l i") 'lsp-ui-peek-find-implementation)
(global-set-key (kbd "C-c l s") 'lsp-ui-peek-find-workspace-symbol)

;; Terminal
(global-set-key (kbd "C-c t") 'my/toggle-vterm)

;; Buffers
(global-set-key (kbd "C-c b d") (lambda () (interactive) (kill-this-buffer)))
(global-set-key (kbd "C-c b l") (lambda () (interactive) (consult-buffer)))

;; Fuzzy search
(global-set-key (kbd "C-c /") 'consult-ripgrep)
(global-set-key (kbd "C-c f") 'project-find-file)


