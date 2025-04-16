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

;; configuração do straight, necessário para o lsp-bridge
;; e outros pacotes hospedados no github
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 6))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(setq straight-check-for-modifications 'live) ;; Verifica se há atualizações nos pacotes de forma assíncrona.

;; Integra straight.el com use-package
(straight-use-package 'use-package)
(setq straight-use-package-by-default t)
(setq native-comp-deferred-compilation t) ;; Adia a compilação de pacotes para quando necessário
(setq comp-deferred-compilation nil) ;; Desabilita a compilação nativa imediata

;; Rola uma linha de cada vez ao pressionar a seta
(setq scroll-step 1)

;; define o cursor para pipe (|)
(setq-default cursor-type 'bar)

;; desativa tela inicial do emacs
(setq inhibit-startup-screen t)

;; desativa arquivos de backup
(setq make-backup-files nil)

;; desativa arquivos (setq auto-save-default nil)

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
(unless package-archive-contents
  (package-refresh-contents))

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

;; ajusta o tamanho do identificador da janela e deixa-o verde
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(aw-leading-char-face ((t (:inherit ace-jump-face-foreground :height 2.0 :foreground "green" :weight bold)))))

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

;; instala markdown-mode
(use-package markdown-mode
  :ensure t
  :mode ("README\\.md\\'" . gfm-mode)
  :init (setq markdown-command "multimarkdown")
  :bind (:map markdown-mode-map
         ("C-c C-e" . markdown-do)))

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

;; instala ícones para neotree
(use-package all-the-icons
  :ensure t)

;; instala o treemacs
(use-package treemacs
  :ensure t
  :defer t
  :init
  (with-eval-after-load 'winum
    (define-key winum-keymap (kbd "M-0") #'treemacs-select-window))
  :config
  (progn
    (setq treemacs-collapse-dirs                   (if treemacs-python-executable 3 0)
          treemacs-deferred-git-apply-delay        0.5
          treemacs-directory-name-transformer      #'identity
          treemacs-display-in-side-window          t
          treemacs-eldoc-display                   'simple
          treemacs-file-event-delay                2000
          treemacs-file-extension-regex            treemacs-last-period-regex-value
          treemacs-file-follow-delay               0.2
          treemacs-file-name-transformer           #'identity
          treemacs-follow-after-init               t
          treemacs-expand-after-init               t
          treemacs-find-workspace-method           'find-for-file-or-pick-first
          treemacs-git-command-pipe                ""
          treemacs-goto-tag-strategy               'refetch-index
          treemacs-header-scroll-indicators        '(nil . "^^^^^^")
          treemacs-hide-dot-git-directory          t
          treemacs-indentation                     2
          treemacs-indentation-string              " "
          treemacs-is-never-other-window           nil
          treemacs-max-git-entries                 5000
          treemacs-missing-project-action          'ask
          treemacs-move-files-by-mouse-dragging    t
          treemacs-move-forward-on-expand          nil
          treemacs-no-png-images                   nil
          treemacs-no-delete-other-windows         t
          treemacs-project-follow-cleanup          nil
          treemacs-persist-file                    (expand-file-name ".cache/treemacs-persist" user-emacs-directory)
          treemacs-position                        'left
          treemacs-read-string-input               'from-child-frame
          treemacs-recenter-distance               0.1
          treemacs-recenter-after-file-follow      nil
          treemacs-recenter-after-tag-follow       nil
          treemacs-recenter-after-project-jump     'always
          treemacs-recenter-after-project-expand   'on-distance
          treemacs-litter-directories              '("/node_modules" "/.venv" "/.cask")
          treemacs-project-follow-into-home        nil
          treemacs-show-cursor                     nil
          treemacs-show-hidden-files               t
          treemacs-silent-filewatch                nil
          treemacs-silent-refresh                  nil
          treemacs-sorting                         'alphabetic-asc
          treemacs-select-when-already-in-treemacs 'move-back
          treemacs-space-between-root-nodes        t
          treemacs-tag-follow-cleanup              t
          treemacs-tag-follow-delay                1.5
          treemacs-text-scale                      nil
          treemacs-user-mode-line-format           nil
          treemacs-user-header-line-format         nil
          treemacs-wide-toggle-width               70
          treemacs-width                           45
          treemacs-width-increment                 1
          treemacs-width-is-initially-locked       t
          treemacs-workspace-switch-cleanup        nil)

    ;; The default width and height of the icons is 22 pixels. If you are
    ;; using a Hi-DPI display, uncomment this to double the icon size.
    ;;(treemacs-resize-icons 44)
    
    (treemacs-follow-mode t)
    (treemacs-filewatch-mode t)
    (treemacs-fringe-indicator-mode 'always)
    (when treemacs-python-executable
      (treemacs-git-commit-diff-mode t))

    (pcase (cons (not (null (executable-find "git")))
                 (not (null treemacs-python-executable)))
      (`(t . t)
       (treemacs-git-mode 'deferred))
      (`(t . _)
       (treemacs-git-mode 'simple)))

    (treemacs-hide-gitignored-files-mode nil))
  :bind
  (:map global-map
        ("M-0"       . treemacs-select-window)
        ("C-x t 1"   . treemacs-delete-other-windows)
        ("C-x t t"   . treemacs)
        ("C-x t d"   . treemacs-select-directory)
        ("C-x t B"   . treemacs-bookmark)
        ("C-x t C-t" . treemacs-find-file)
        ("C-x t M-t" . treemacs-find-tag)))

(use-package treemacs-projectile
  :after (treemacs projectile)
  :ensure t)

(use-package treemacs-icons-dired
  :hook (dired-mode . treemacs-icons-dired-enable-once)
  :ensure t)

(use-package treemacs-magit
  :after (treemacs magit)
  :ensure t)

(use-package treemacs-persp ;;treemacs-perspective if you use perspective.el vs. persp-mode
  :after (treemacs persp-mode) ;;or perspective vs. persp-mode
  :ensure t
  :config (treemacs-set-scope-type 'Perspectives))

(use-package treemacs-tab-bar ;;treemacs-tab-bar if you use tab-bar-mode
  :after (treemacs)
  :ensure t
  :config (treemacs-set-scope-type 'Tabs))

;; inicia o treemacs ao abrir o emacs
(treemacs-start-on-boot)

;; snippets
(use-package yasnippet
  :ensure t
  :hook (prog-mode . yas-minor-mode))

(use-package yasnippet-snippets
  :ensure t)

;; configura lsp para ruby
(use-package ruby-mode
             :ensure t
             :hook (ruby-mode . lsp-bridge-mode))

;; configuração de formatação do ruby
(defun my-ruby-format ()
  (when (eq major-mode 'ruby-mode)
    (shell-command-to-string (format "standardrb --fix %s" (shell-quote-argument buffer-file-name)))
    (revert-buffer t t t)))

(add-hook 'after-save-hook 'my-ruby-format)

;; configura lsp para php
(use-package php-mode
  :ensure t
  :mode "\\.php\\'"
  :hook (php-mode . lsp))

;; instala o codeium
(straight-use-package '(codeium :type git :host github :repo "Exafunction/codeium.el"))

;; adiciona suporte a lsp
(use-package lsp-bridge
  :straight '(lsp-bridge :type git :host github :repo "manateelazycat/lsp-bridge"
            :files (:defaults "*.el" "*.py" "acm" "core" "langserver" "multiserver" "resources")
            :build (:not compile))
  :init
  (global-lsp-bridge-mode)
  :config
  (setq lsp-php-intelephense-license-key nil)
  (setq lsp-inlay-hint-enable nil)
  (setq lsp-bridge-enable-codeium t)
  (setq lsp-bridge-enable-completion-at-point t)
  (setq lsp-brige-enable-auto-completion t))

;; configura o backend do completion
(setq lsp-bridge-completion-backend-list
      '(acm-backend-codeium acm-backend-lsp acm-backend-yasnippet acm-backend-buffer))

;; which-key ajuda a ver os atalhos
(use-package which-key
  :ensure t
  :config
  (which-key-mode))

;; configura o lsp do go
;; for mac
;;(setenv "PATH" (concat (getenv "PATH") ":/Users/fabio/go/bin"))
;;(add-to-list 'exec-path "/Users/fabio/go/bin")

;; for linux
(setenv "PATH" (concat (getenv "PATH") ":/home/fabio/go/bin"))
(add-to-list 'exec-path "/home/fabio/go/bin")

(use-package go-mode
  :ensure t
  :hook ((go-mode . lsp-bridge-mode)))

(defun lsp-go-install-save-hooks ()
  (add-hook 'before-save-hook #'lsp-format-buffer t t)
  (add-hook 'before-save-hook #'lsp-organize-imports t t))

(add-hook 'go-mode-hook #'lsp-go-install-save-hooks)

;; ativar seleção para o clipboard
(setq x-select-enable-clipboard t)
(setq select-enable-clipboard t)

(setq lsp-bridge-enable-hover-diagnostic t)
(setq lsp-bridge-enable-popup-documentation t)
(setq lsp-bridge-enable-log t)

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

;; em alguns casos de palavras com início igual, o vertico
;; entende que você  quer ele, e pode ter problemas, quando por exemplo
;; vai copiar um arquivo com nome .env-example e na cópia quer dar
;; o nome de .env, o vertico vai selecionar o .env-example e não
;; vai te permitir criar com o nome desejado, nesses casos é
;; necessário desativar o vertico
(global-set-key (kbd "C-c v") 'vertico-mode)

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
 
;; LSP actions
;; Executar ação de código
(global-set-key (kbd "C-c a a") 'lsp-bridge-code-action)
;; Formatar buffer
(global-set-key (kbd "C-c a f") 'lsp-bridge-format-buffer)
;; Organizar imports
(global-set-key (kbd "C-c a o") 'lsp-bridge-organize-imports)

;; LSP infos
;; Mostrar documentação do símbolo sob o cursor
(global-set-key (kbd "C-c l k") #'lsp-bridge-popup-documentation)
;; Ir para definição
(global-set-key (kbd "C-c l d") #'lsp-bridge-find-def)
;; Buscar referências
(global-set-key (kbd "C-c l r") #'lsp-bridge-find-references)
;; Buscar implementações
(global-set-key (kbd "C-c l i") #'lsp-bridge-find-impl)
;; Buscar símbolos no workspace
(global-set-key (kbd "C-c l s") #'lsp-bridge-workspace-symbol)
;; Voltar (depois de usar find-def, por exemplo)
(global-set-key (kbd "C-c l b") #'lsp-bridge-find-def-return)
;; Rename
(global-set-key (kbd "C-c l n") #'lsp-bridge-rename)
;; Ver diagnóstico atual
(global-set-key (kbd "C-c l e") #'lsp-bridge-diagnostic-list)
;; Ir para o próximo erro
(global-set-key (kbd "C-c l j") #'lsp-bridge-diagnostic-jump-next)
;; Ir para o erro anterior
(global-set-key (kbd "C-c l p") #'lsp-bridge-diagnostic-jump-prev)

;; rolar documentação
;(setq lsp-bridge-popup-documentation-scroll-step 2)
(global-set-key (kbd "C-{") #'lsp-bridge-popup-documentation-scroll-up)
(global-set-key (kbd "C-}") #'lsp-bridge-popup-documentation-scroll-down)

;; Terminal
(global-set-key (kbd "C-c t") 'my/toggle-vterm)

;; Buffers
(global-set-key (kbd "C-c b d") (lambda () (interactive) (kill-this-buffer)))
(global-set-key (kbd "C-c b l") (lambda () (interactive) (consult-buffer)))

;; Fuzzy search
(global-set-key (kbd "C-c /") 'consult-ripgrep)
(global-set-key (kbd "C-c f") 'project-find-file)

(put 'upcase-region 'disabled nil)
