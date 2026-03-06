;;; worksneeze.el --- Git worktree manager -*- lexical-binding: t; -*-

;; Author: Aaron Strick
;; Version: 0.1.0
;; Package-Requires: ((emacs "27.1"))
;; Keywords: git, vc, tools
;; URL: https://github.com/strickinato/worksneeze

;;; Commentary:
;;
;; worksneeze provides two things:
;;   1. `worksneeze' -- a dashboard buffer listing all worktrees for the
;;      current repository, with quick keys to refresh and create.
;;   2. `worksneeze-create' -- an interactive command to add a new worktree
;;      under .worktrees/ (or a custom directory) relative to the repo root.

;;; Code:

(declare-function magit-status "magit-status" (&optional directory))
(declare-function evil-local-set-key "evil-core" (state key def))
(declare-function projectile-add-known-project "projectile" (project-root))
(declare-function projectile-remove-known-project "projectile" (&optional project))

;;; Customization

(defgroup worksneeze nil
  "Git worktree manager."
  :group 'vc
  :prefix "worksneeze-")

(defcustom worksneeze-worktree-directory ".worktrees"
  "Subdirectory under the repo root where new worktrees are placed."
  :type 'string
  :group 'worksneeze)

(defcustom worksneeze-buffer-name "*worksneeze*"
  "Name of the worksneeze dashboard buffer."
  :type 'string
  :group 'worksneeze)

(defcustom worksneeze-use-magit 'auto
  "Whether to use magit for opening worktrees.
`auto' uses magit when available; t forces it; nil always uses dired."
  :type '(choice (const :tag "Auto-detect" auto)
                 (const :tag "Always use magit" t)
                 (const :tag "Always use dired" nil))
  :group 'worksneeze)

;;; Variables

(defvar worksneeze--repo-root nil
  "Repo root for the current dashboard buffer.
Buffer-local in worksneeze-mode buffers.")

(defconst worksneeze--buffer-header
  "Worktrees   [g] refresh  [c] create  [C] from branch  [D] mark delete  [u] unmark  [x] execute  [RET] open  [q] quit\n\
──────────────────────────────────────────────────────────────────────────────────────────────────────────────────────\n"
  "Header text for the dashboard buffer.")

;;; Git Data Layer

(defun worksneeze--repo-root-for (dir)
  "Return the git repo root containing DIR, or nil."
  (let ((default-directory dir))
    (let ((result (string-trim
                   (shell-command-to-string
                    "git rev-parse --show-toplevel 2>/dev/null"))))
      (unless (string-empty-p result)
        (file-name-as-directory result)))))

(defun worksneeze--run-git (&rest args)
  "Run git with ARGS in `default-directory', return list of output lines.
Signals an error if git exits non-zero."
  (apply #'process-lines "git" args))

(defun worksneeze--parse-porcelain (lines)
  "Parse LINES from `git worktree list --porcelain'.
Returns a list of alists, each with keys:
  :path   -- absolute path string
  :head   -- full SHA1 string
  :branch -- branch name (e.g. \"main\"), or nil if detached"
  (let ((worktrees nil)
        (current nil))
    (dolist (line lines)
      (cond
       ((string-prefix-p "worktree " line)
        (when current
          (push (nreverse current) worktrees))
        (setq current (list (cons :path (substring line 9)))))
       ((string-prefix-p "HEAD " line)
        (push (cons :head (substring line 5)) current))
       ((string-prefix-p "branch refs/heads/" line)
        (push (cons :branch (substring line 18)) current))
       ((string= line "detached")
        (push (cons :branch nil) current))))
    (when current
      (push (nreverse current) worktrees))
    (nreverse worktrees)))

(defun worksneeze--list-worktrees ()
  "Return parsed worktree list for the repo at `default-directory'."
  (worksneeze--parse-porcelain
   (worksneeze--run-git "worktree" "list" "--porcelain")))

;;; Faces

(defface worksneeze-path
  '((t :inherit font-lock-function-name-face))
  "Face for worktree path in the dashboard."
  :group 'worksneeze)

(defface worksneeze-branch
  '((t :inherit font-lock-keyword-face))
  "Face for branch name in the dashboard."
  :group 'worksneeze)

(defface worksneeze-head
  '((t :inherit font-lock-comment-face))
  "Face for HEAD SHA in the dashboard."
  :group 'worksneeze)

(defface worksneeze-detached
  '((t :inherit font-lock-warning-face))
  "Face for detached HEAD indicator."
  :group 'worksneeze)

(defface worksneeze-marked-delete
  '((t :inherit error))
  "Face for the D delete marker."
  :group 'worksneeze)

;;; Dashboard Rendering

(defun worksneeze--managed-p (worktree repo-root)
  "Return non-nil if WORKTREE is inside the worksneeze-managed directory."
  (let ((wt-parent (file-name-as-directory
                    (expand-file-name worksneeze-worktree-directory repo-root))))
    (string-prefix-p wt-parent (file-name-as-directory (alist-get :path worktree)))))

(defun worksneeze--main-worktree-p (worktree repo-root)
  "Return non-nil if WORKTREE is the main worktree (the repo root)."
  (string= (file-name-as-directory (alist-get :path worktree)) repo-root))

(defun worksneeze--render-worktree-line (wt &optional marker)
  "Render a single worktree line for WT.
MARKER is an optional string prefix (e.g. \"*\")."
  (let* ((path (alist-get :path wt))
         (head (alist-get :head wt))
         (branch (alist-get :branch wt))
         (name (file-name-nondirectory (directory-file-name path)))
         (branch-str
          (if branch
              (propertize branch 'face 'worksneeze-branch)
            (propertize "(detached)" 'face 'worksneeze-detached)))
         (head-str
          (propertize (substring head 0 (min 8 (length head)))
                      'face 'worksneeze-head))
         (line-start (point)))
    (insert (format " %s %s %-30s  %-20s  %s\n"
                    (or marker " ")
                    " "
                    name
                    branch-str
                    head-str))
    (put-text-property line-start (point) 'worksneeze-path path)))

(defun worksneeze--render (worktrees repo-root)
  "Render WORKTREES into the current buffer.
Shows the main worktree with a * marker, then only worksneeze-managed trees."
  (let* ((inhibit-read-only t)
         (main-wt (seq-find (lambda (wt) (worksneeze--main-worktree-p wt repo-root))
                            worktrees))
         (managed (seq-filter (lambda (wt) (worksneeze--managed-p wt repo-root))
                              worktrees)))
    (erase-buffer)
    (insert worksneeze--buffer-header)
    (when main-wt
      (worksneeze--render-worktree-line main-wt "*"))
    (if (null managed)
        (insert "  (no managed worktrees)\n")
      (dolist (wt managed)
        (worksneeze--render-worktree-line wt)))
    (goto-char (point-min))
    (forward-line 2)))

;;; Dashboard Major Mode

(defconst worksneeze-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "g")   #'worksneeze-refresh)
    (define-key map (kbd "c")   #'worksneeze-create)
    (define-key map (kbd "C")   #'worksneeze-create-from-branch)
    (define-key map (kbd "D")   #'worksneeze-mark-delete)
    (define-key map (kbd "u")   #'worksneeze-unmark)
    (define-key map (kbd "x")   #'worksneeze-execute)
    (define-key map (kbd "RET") #'worksneeze-open-at-point)
    (define-key map (kbd "q")   #'quit-window)
    (define-key map (kbd "n")   #'next-line)
    (define-key map (kbd "p")   #'previous-line)
    map)
  "Keymap for `worksneeze-mode'.")

(define-derived-mode worksneeze-mode special-mode "Worksneeze"
  "Major mode for the worksneeze git worktree dashboard.
\\{worksneeze-mode-map}"
  :group 'worksneeze
  (setq truncate-lines t)
  (buffer-disable-undo)
  ;; When evil-mode is active, Doom's special-mode integration remaps most
  ;; normal-state keys to `ignore'.  Bind our keys in the evil normal-state
  ;; auxiliary keymap so they take precedence.
  (when (bound-and-true-p evil-local-mode)
    (evil-local-set-key 'normal (kbd "g")   #'worksneeze-refresh)
    (evil-local-set-key 'normal (kbd "c")   #'worksneeze-create)
    (evil-local-set-key 'normal (kbd "C")   #'worksneeze-create-from-branch)
    (evil-local-set-key 'normal (kbd "D")   #'worksneeze-mark-delete)
    (evil-local-set-key 'normal (kbd "u")   #'worksneeze-unmark)
    (evil-local-set-key 'normal (kbd "x")   #'worksneeze-execute)
    (evil-local-set-key 'normal (kbd "RET") #'worksneeze-open-at-point)
    (evil-local-set-key 'normal (kbd "q")   #'quit-window)))

;;; Entry Points

;;;###autoload
(defun worksneeze ()
  "Open the worksneeze git worktree dashboard."
  (interactive)
  (let ((root (worksneeze--repo-root-for default-directory)))
    (unless root
      (user-error "Not inside a git repository"))
    (let ((buf (get-buffer-create worksneeze-buffer-name)))
      (with-current-buffer buf
        (worksneeze-mode)
        (setq-local worksneeze--repo-root root)
        (let ((default-directory root))
          (worksneeze--render (worksneeze--list-worktrees) root)))
      (pop-to-buffer buf))))

(defun worksneeze-refresh ()
  "Refresh the worksneeze dashboard."
  (interactive)
  (unless (derived-mode-p 'worksneeze-mode)
    (user-error "Not in a worksneeze buffer"))
  (let ((default-directory worksneeze--repo-root))
    (worksneeze--render (worksneeze--list-worktrees) worksneeze--repo-root)
    (message "Worktrees refreshed")))

(defun worksneeze--use-magit-p ()
  "Return non-nil if magit should be used to open worktrees."
  (pcase worksneeze-use-magit
    ('auto (and (featurep 'magit) (fboundp 'magit-status)))
    ('t t)
    (_ nil)))

(defun worksneeze-open-at-point ()
  "Open the worktree on the current line."
  (interactive)
  (let ((path (get-text-property (point) 'worksneeze-path)))
    (unless path
      (user-error "No worktree on this line"))
    (unless (file-directory-p path)
      (user-error "Worktree directory does not exist: %s" path))
    (if (worksneeze--use-magit-p)
        (magit-status path)
      (dired path))))

;;; Marking and Deletion

(defun worksneeze--mark-column-pos ()
  "Return the buffer position of the mark column on the current line, or nil.
The mark column is at column 3 (the character after \" D \" or \"   \")."
  (save-excursion
    (beginning-of-line)
    (let ((path (get-text-property (point) 'worksneeze-path)))
      (when path
        (+ (line-beginning-position) 1)))))

(defun worksneeze--main-line-p ()
  "Return non-nil if the current line is the main worktree (marked with *)."
  (save-excursion
    (beginning-of-line)
    (looking-at " \\*")))

(defun worksneeze-mark-delete ()
  "Mark the worktree at point for deletion."
  (interactive)
  (when (worksneeze--main-line-p)
    (user-error "Cannot delete the main worktree"))
  (let ((pos (worksneeze--mark-column-pos)))
    (unless pos
      (user-error "No worktree on this line"))
    (let ((inhibit-read-only t))
      (save-excursion
        (goto-char pos)
        (delete-char 1)
        (insert (propertize "D" 'face 'worksneeze-marked-delete)))))
  (forward-line 1))

(defun worksneeze-unmark ()
  "Remove the deletion mark from the worktree at point."
  (interactive)
  (let ((pos (worksneeze--mark-column-pos)))
    (unless pos
      (user-error "No worktree on this line"))
    (let ((inhibit-read-only t))
      (save-excursion
        (goto-char pos)
        (delete-char 1)
        (insert " "))))
  (forward-line 1))

(defun worksneeze--collect-marked-paths ()
  "Return a list of paths marked for deletion."
  (let ((paths nil))
    (save-excursion
      (goto-char (point-min))
      (while (not (eobp))
        (when (and (get-text-property (point) 'worksneeze-path)
                   (save-excursion
                     (beginning-of-line)
                     (looking-at " D")))
          (push (get-text-property (point) 'worksneeze-path) paths))
        (forward-line 1)))
    (nreverse paths)))

(defun worksneeze-execute ()
  "Delete all worktrees marked with D."
  (interactive)
  (let ((paths (worksneeze--collect-marked-paths)))
    (unless paths
      (user-error "No worktrees marked for deletion"))
    (when (yes-or-no-p
           (format "Delete %d worktree(s)? " (length paths)))
      (let ((default-directory worksneeze--repo-root))
        (dolist (path paths)
          (when (fboundp 'projectile-remove-known-project)
            (projectile-remove-known-project (file-name-as-directory path)))
          (worksneeze--run-git "worktree" "remove" path)
          (message "Removed worktree: %s" path)))
      (worksneeze-refresh))))

;;; Projectile Integration

(defun worksneeze--ensure-worktrees-ignored (root)
  "Ensure the worktree directory is in ROOT's .gitignore."
  (let ((gitignore (expand-file-name ".gitignore" root))
        (entry (concat "/" worksneeze-worktree-directory)))
    (unless (and (file-exists-p gitignore)
                 (with-temp-buffer
                   (insert-file-contents gitignore)
                   (goto-char (point-min))
                   (re-search-forward (concat "^" (regexp-quote entry) "$") nil t)))
      (with-temp-buffer
        (when (file-exists-p gitignore)
          (insert-file-contents gitignore)
          (goto-char (point-max))
          (unless (bolp) (insert "\n")))
        (insert entry "\n")
        (write-region (point-min) (point-max) gitignore)))))

;;; Branch Helpers

(defun worksneeze--remote-branches ()
  "Return a list of remote branch names (e.g. \"origin/feature-foo\").
Runs `git fetch --all' first to ensure the list is current.
Filters out HEAD pointer lines (e.g. \"origin/HEAD -> origin/main\")."
  (worksneeze--run-git "fetch" "--all")
  (let ((lines (worksneeze--run-git "branch" "-r")))
    (delq nil
          (mapcar (lambda (line)
                    (let ((trimmed (string-trim line)))
                      (unless (string-match-p "->" trimmed)
                        trimmed)))
                  lines))))

(defun worksneeze--local-branch-exists-p (branch)
  "Return non-nil if local BRANCH already exists."
  (condition-case nil
      (progn (worksneeze--run-git "rev-parse" "--verify" (concat "refs/heads/" branch))
             t)
    (error nil)))

;;; Worktree Creation

;;;###autoload
(defun worksneeze-create (branch &optional base)
  "Create a new git worktree for BRANCH.
Places it under `worksneeze-worktree-directory' relative to the repo root.
With prefix argument, also prompts for a BASE branch or commit."
  (interactive
   (let* ((branch (read-string "New branch name: "))
          (base (when current-prefix-arg
                  (read-string "Base branch/commit (blank for HEAD): "
                               nil nil ""))))
     (list branch (and base (not (string-empty-p base)) base))))
  (let ((root (or (and (derived-mode-p 'worksneeze-mode) worksneeze--repo-root)
                  (worksneeze--repo-root-for default-directory))))
    (unless root
      (user-error "Not inside a git repository"))
    (when (string-empty-p branch)
      (user-error "Branch name cannot be empty"))
    (let* ((wt-parent (expand-file-name worksneeze-worktree-directory root))
           (wt-path (expand-file-name branch wt-parent)))
      (when (file-exists-p wt-path)
        (user-error "Path already exists: %s" wt-path))
      (unless (file-directory-p wt-parent)
        (make-directory wt-parent t))
      (worksneeze--ensure-worktrees-ignored root)
      (let ((default-directory root))
        (apply #'worksneeze--run-git
               (append (list "worktree" "add" "-b" branch wt-path)
                       (when base (list base)))))
      (message "Created worktree at %s (branch: %s)" wt-path branch)
      (when (fboundp 'projectile-add-known-project)
        (projectile-add-known-project (file-name-as-directory wt-path)))
      (when-let ((buf (get-buffer worksneeze-buffer-name)))
        (with-current-buffer buf
          (worksneeze-refresh))))))

;;; Worktree Creation from Remote Branch

;;;###autoload
(defun worksneeze-create-from-branch (remote-branch)
  "Create a new worktree tracking REMOTE-BRANCH.
Fetches all remotes, then prompts for a remote branch with completion.
Creates a local branch (stripping the remote prefix) that tracks the
remote branch, and places the worktree under `worksneeze-worktree-directory'."
  (interactive
   (let* ((root (or (and (derived-mode-p 'worksneeze-mode) worksneeze--repo-root)
                    (worksneeze--repo-root-for default-directory)))
          (_ (unless root (user-error "Not inside a git repository")))
          (default-directory root)
          (branches (worksneeze--remote-branches)))
     (unless branches
       (user-error "No remote branches found"))
     (list (completing-read "Remote branch: " branches nil t))))
  (let ((root (or (and (derived-mode-p 'worksneeze-mode) worksneeze--repo-root)
                  (worksneeze--repo-root-for default-directory))))
    (unless root
      (user-error "Not inside a git repository"))
    (let* ((local-name (if (string-match "^[^/]+/\\(.+\\)$" remote-branch)
                           (match-string 1 remote-branch)
                         remote-branch))
           (wt-parent (expand-file-name worksneeze-worktree-directory root))
           (wt-path (expand-file-name local-name wt-parent)))
      (when (file-exists-p wt-path)
        (user-error "Path already exists: %s" wt-path))
      (unless (file-directory-p wt-parent)
        (make-directory wt-parent t))
      (worksneeze--ensure-worktrees-ignored root)
      (let ((default-directory root))
        (if (worksneeze--local-branch-exists-p local-name)
            (worksneeze--run-git "worktree" "add" wt-path local-name)
          (worksneeze--run-git "worktree" "add" "--track" "-b" local-name wt-path remote-branch)))
      (message "Created worktree at %s (local branch: %s tracking %s)"
               wt-path local-name remote-branch)
      (when (fboundp 'projectile-add-known-project)
        (projectile-add-known-project (file-name-as-directory wt-path)))
      (when-let ((buf (get-buffer worksneeze-buffer-name)))
        (with-current-buffer buf
          (worksneeze-refresh))))))

(provide 'worksneeze)
;;; worksneeze.el ends here
