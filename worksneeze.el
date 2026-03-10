;;; worksneeze.el --- Git worktree manager -*- lexical-binding: t; -*-

;; Author: Aaron Strick
;; Version: 0.1.0
;; Package-Requires: ((emacs "27.1"))
;; Keywords: git, vc, tools
;; URL: https://github.com/strickinato/worksneeze

;;; Commentary:
;;
;; worksneeze provides cross-project git worktree management:
;;   1. `worksneeze' -- a dashboard showing worktrees across all known
;;      projects (via projectile or project.el), grouped by project.
;;   2. `worksneeze-create' -- create a new worktree in any project.
;;   3. `worksneeze-create-from-pr' -- create a worktree from a GitHub PR.
;;
;; Worktrees are stored in .worksneeze/ under each project root.
;; Configure project discovery via `worksneeze-project-backend'.

;;; Code:

(require 'transient)
(require 'ghub nil t)

(declare-function magit-status "magit-status" (&optional directory))
(declare-function evil-define-key* "evil-core" (state keymap &rest bindings))
(declare-function projectile-add-known-project "projectile" (project-root))
(declare-function projectile-remove-known-project "projectile" (&optional project))
(declare-function project-known-project-roots "project" ())
(defvar projectile-known-projects)
(declare-function agent-shell "agent-shell" (&optional prefix))
(declare-function agent-shell-buffers "agent-shell" ())
(declare-function agent-shell-cwd "agent-shell-project" ())
(declare-function agent-shell-subscribe-to "agent-shell"
                  (&rest args &key shell-buffer event on-event))
(defvar agent-shell--state)

;;; Customization

(defgroup worksneeze nil
  "Git worktree manager."
  :group 'vc
  :prefix "worksneeze-")

(defcustom worksneeze-worktree-directory ".worksneeze"
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

(defcustom worksneeze-project-backend 'auto
  "Backend for discovering known projects.
`auto' tries projectile first, then project.el.
`projectile' uses `projectile-known-projects'.
`project' uses `project-known-project-roots'.
`custom' uses `worksneeze-custom-project-roots'."
  :type '(choice (const :tag "Auto-detect" auto)
                 (const :tag "Projectile" projectile)
                 (const :tag "project.el" project)
                 (const :tag "Custom list" custom))
  :group 'worksneeze)

(defcustom worksneeze-custom-project-roots nil
  "List of project root directories when using the `custom' backend."
  :type '(repeat directory)
  :group 'worksneeze)

;;; Variables

(defvar worksneeze-after-create-functions nil
  "Abnormal hook run after a worktree is created.
Each function is called with two arguments: WT-PATH (the new
worktree directory) and REPO-ROOT (the repository root).")

(defvar-local worksneeze--project-roots nil
  "List of repo root directories shown in the current dashboard.")


;;; Git Data Layer

(defun worksneeze--repo-root-for (dir)
  "Return the main git repo root containing DIR, or nil.
When DIR is inside a worktree, returns the main worktree root
rather than the linked worktree's root."
  (when (file-directory-p dir)
    (let ((default-directory dir))
      (let ((git-common-dir (string-trim
                             (shell-command-to-string
                              "git rev-parse --path-format=absolute --git-common-dir 2>/dev/null"))))
        (unless (string-empty-p git-common-dir)
          (file-name-as-directory (file-name-directory (directory-file-name git-common-dir))))))))

(defun worksneeze--run-git (&rest args)
  "Run git with ARGS in `default-directory', return list of output lines.
Signals an error with stderr output if git exits non-zero."
  (let ((stderr-file (make-temp-file "worksneeze-git-")))
    (unwind-protect
        (with-temp-buffer
          (let ((exit-code (apply #'call-process "git" nil
                                  (list t stderr-file) nil args)))
            (unless (zerop exit-code)
              (user-error "git %s failed (exit %d): %s"
                          (car args) exit-code
                          (string-trim
                           (with-temp-buffer
                             (insert-file-contents stderr-file)
                             (buffer-string)))))
            (split-string (buffer-string) "\n" t)))
      (delete-file stderr-file))))

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

;;; Project Discovery

(defun worksneeze--discover-project-roots ()
  "Return a list of known project root directories as absolute paths.
Dispatches based on `worksneeze-project-backend'."
  (let ((backend worksneeze-project-backend))
    (when (eq backend 'auto)
      (setq backend (cond
                     ((and (featurep 'projectile)
                           (boundp 'projectile-known-projects))
                      'projectile)
                     ((fboundp 'project-known-project-roots)
                      'project)
                     (t nil))))
    (pcase backend
      ('projectile
       (mapcar (lambda (p) (file-name-as-directory (expand-file-name p)))
               projectile-known-projects))
      ('project
       (mapcar #'file-name-as-directory (project-known-project-roots)))
      ('custom
       (mapcar (lambda (p) (file-name-as-directory (expand-file-name p)))
               worksneeze-custom-project-roots))
      (_ nil))))

(defun worksneeze--has-managed-dir-p (repo-root)
  "Return non-nil if REPO-ROOT contains a worksneeze-managed worktree directory."
  (file-directory-p (expand-file-name worksneeze-worktree-directory repo-root)))

(defun worksneeze--projects-with-worksneeze ()
  "Return a deduplicated list of repo roots that have managed worktree directories.
Scans all known projects via the project backend and resolves each to its
git repo root."
  (let ((project-dirs (worksneeze--discover-project-roots))
        (seen (make-hash-table :test #'equal))
        (result nil))
    (dolist (dir project-dirs)
      (when-let ((repo-root (worksneeze--repo-root-for dir)))
        (unless (gethash repo-root seen)
          (puthash repo-root t seen)
          (when (worksneeze--has-managed-dir-p repo-root)
            (push repo-root result)))))
    (nreverse result)))

(defun worksneeze--project-display-name (repo-root)
  "Return a short display name for REPO-ROOT."
  (file-name-nondirectory (directory-file-name repo-root)))

(defun worksneeze--repo-root-at-point ()
  "Return the repo root for the project at point, or nil."
  (get-text-property (point) 'worksneeze-repo-root))

(defun worksneeze--prompt-for-project ()
  "Prompt the user to select a project root from all known git projects."
  (let* ((all-project-dirs (worksneeze--discover-project-roots))
         (git-roots (delete-dups
                     (delq nil (mapcar #'worksneeze--repo-root-for
                                       all-project-dirs))))
         (candidates (mapcar (lambda (root)
                               (cons (worksneeze--project-display-name root) root))
                             git-roots)))
    (unless candidates
      (user-error "No known projects found"))
    (let ((choice (completing-read "Project: " candidates nil t)))
      (cdr (assoc choice candidates)))))

;;; Faces

(defface worksneeze-path
  '((t :inherit font-lock-function-name-face))
  "Face for worktree path in the dashboard."
  :group 'worksneeze)

(defface worksneeze-project-header
  '((t :inherit font-lock-constant-face :weight bold))
  "Face for project section headers in the dashboard."
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

(defface worksneeze-agent-busy
  '((t :inherit warning :weight bold))
  "Face for busy agent indicator."
  :group 'worksneeze)

(defface worksneeze-agent-idle
  '((t :inherit font-lock-comment-face))
  "Face for idle agent indicator."
  :group 'worksneeze)

(defface worksneeze-pr
  '((t :inherit font-lock-type-face))
  "Face for PR info line."
  :group 'worksneeze)

(defface worksneeze-pr-draft
  '((t :inherit font-lock-comment-face :slant italic))
  "Face for draft PR indicator."
  :group 'worksneeze)

;;; Agent-Shell Integration

(defvar worksneeze--refresh-timer nil
  "Debounce timer for agent event refresh.")

(defvar worksneeze--subscribed-buffers nil
  "List of agent-shell buffers we have subscribed to.")

(defun worksneeze--agents-for-path (path)
  "Return list of agent info for agents whose CWD is under PATH.
Each entry is an alist with :buffer, :name, and :active-p keys.
Returns nil if agent-shell is not loaded."
  (when (featurep 'agent-shell)
    (let ((path-dir (file-name-as-directory path))
          (result nil))
      (dolist (buf (agent-shell-buffers))
        (when (buffer-live-p buf)
          (condition-case nil
              (with-current-buffer buf
                (let ((cwd (file-name-as-directory (agent-shell-cwd))))
                  (when (and (file-directory-p cwd)
                             (string-prefix-p path-dir cwd))
                    (push (list (cons :buffer buf)
                                (cons :name (buffer-name buf))
                                (cons :active-p (not (null (map-elt agent-shell--state
                                                                    :active-request)))))
                          result))))
            (error nil))))
      (nreverse result))))

(defun worksneeze--agent-status-string (path)
  "Return a propertized agent status string for worktree at PATH, or nil."
  (when-let ((agents (worksneeze--agents-for-path path)))
    (let* ((count (length agents))
           (busy-count (length (seq-filter (lambda (a) (alist-get :active-p a)) agents))))
      (cond
       ((= count 1)
        (if (> busy-count 0)
            (propertize "[agent: busy]" 'face 'worksneeze-agent-busy)
          (propertize "[agent: idle]" 'face 'worksneeze-agent-idle)))
       (t
        (if (> busy-count 0)
            (propertize (format "[%d agents: %d busy]" count busy-count)
                        'face 'worksneeze-agent-busy)
          (propertize (format "[%d agents: idle]" count)
                      'face 'worksneeze-agent-idle)))))))

(defun worksneeze--subscribe-to-agent-events ()
  "Subscribe to agent-shell events for live dashboard refresh.
Subscribes to prompt-ready events on all agent buffers so the
dashboard updates when agents finish processing."
  (when (featurep 'agent-shell)
    (dolist (buf (agent-shell-buffers))
      (when (and (buffer-live-p buf)
                 (not (memq buf worksneeze--subscribed-buffers)))
        (push buf worksneeze--subscribed-buffers)
        (agent-shell-subscribe-to
         :shell-buffer buf
         :event 'prompt-ready
         :on-event #'worksneeze--on-agent-event)))))

(defun worksneeze--on-agent-event (_event)
  "Handle an agent-shell event by scheduling a debounced dashboard refresh."
  (when (timerp worksneeze--refresh-timer)
    (cancel-timer worksneeze--refresh-timer))
  (setq worksneeze--refresh-timer
        (run-with-timer 0.5 nil #'worksneeze--maybe-refresh)))

(defun worksneeze--maybe-refresh ()
  "Refresh the dashboard if the buffer exists."
  (when-let ((buf (get-buffer worksneeze-buffer-name)))
    (when (buffer-live-p buf)
      (with-current-buffer buf
        (when (derived-mode-p 'worksneeze-mode)
          (worksneeze--render))))))

;;; GitHub PR Integration

(defvar-local worksneeze--pr-data nil
  "Alist mapping repo-root strings to branch->PR alists.
Each value is itself an alist mapping branch name strings to PR
info plists with :number, :title, :draft, :url.")

(defvar-local worksneeze--pr-fetch-in-progress nil
  "List of repo root strings for which async PR fetches are in progress.")

(defun worksneeze--github-repo (root)
  "Return \"owner/repo\" for the GitHub remote of ROOT, or nil."
  (let* ((default-directory root)
         (url (string-trim
               (shell-command-to-string
                "git remote get-url origin 2>/dev/null"))))
    (cond
     ;; SSH: git@github.com:owner/repo.git
     ((string-match "github\\.com[:/]\\([^/]+/[^/]+?\\)\\(?:\\.git\\)?$" url)
      (match-string 1 url))
     ;; HTTPS: https://github.com/owner/repo.git
     ((string-match "github\\.com/\\([^/]+/[^/]+?\\)\\(?:\\.git\\)?$" url)
      (match-string 1 url)))))

(defun worksneeze--fetch-pr-data-for (repo-root)
  "Asynchronously fetch open PR data for REPO-ROOT.
Stores results in the REPO-ROOT entry of `worksneeze--pr-data'
and re-renders the dashboard when complete."
  (when (and (featurep 'ghub)
             (not (member repo-root worksneeze--pr-fetch-in-progress)))
    (let ((repo (worksneeze--github-repo repo-root))
          (dashboard-buf (current-buffer)))
      (when repo
        (push repo-root worksneeze--pr-fetch-in-progress)
        (ghub-get (format "/repos/%s/pulls" repo)
                  `((state . "open") (per_page . 100))
                  :auth 'forge
                  :callback
                  (lambda (data _headers _status _req)
                    (when (buffer-live-p dashboard-buf)
                      (with-current-buffer dashboard-buf
                        (setq worksneeze--pr-fetch-in-progress
                              (delete repo-root worksneeze--pr-fetch-in-progress))
                        (setf (alist-get repo-root worksneeze--pr-data
                                         nil nil #'equal)
                              (mapcar
                               (lambda (pr)
                                 (cons (alist-get 'ref (alist-get 'head pr))
                                       (list :number (alist-get 'number pr)
                                             :title (alist-get 'title pr)
                                             :draft (eq (alist-get 'draft pr) t)
                                             :url (alist-get 'html_url pr))))
                               data))
                        (worksneeze--render))))
                  :errorback
                  (lambda (_err _headers _status _req)
                    (when (buffer-live-p dashboard-buf)
                      (with-current-buffer dashboard-buf
                        (setq worksneeze--pr-fetch-in-progress
                              (delete repo-root worksneeze--pr-fetch-in-progress))))))))))

(defun worksneeze--pr-info-string (branch repo-root)
  "Return a propertized PR info string for BRANCH in REPO-ROOT, or nil."
  (when-let* ((repo-prs (alist-get repo-root worksneeze--pr-data nil nil #'equal))
              (pr-info (alist-get branch repo-prs nil nil #'equal)))
    (let* ((number (plist-get pr-info :number))
           (title (plist-get pr-info :title))
           (draft (plist-get pr-info :draft))
           (face (if draft 'worksneeze-pr-draft 'worksneeze-pr)))
      (propertize (format "PR #%d: %s%s" number title
                          (if draft " [draft]" ""))
                  'face face))))

;;; Dashboard Rendering

(defun worksneeze--managed-p (worktree repo-root)
  "Return non-nil if WORKTREE is inside the worksneeze-managed directory."
  (let ((wt-parent (file-name-as-directory
                    (expand-file-name worksneeze-worktree-directory repo-root))))
    (string-prefix-p wt-parent (file-name-as-directory (alist-get :path worktree)))))

(defun worksneeze--main-worktree-p (worktree repo-root)
  "Return non-nil if WORKTREE is the main worktree (the repo root)."
  (string= (file-name-as-directory (alist-get :path worktree)) repo-root))

(defun worksneeze--render-worktree-entry (wt repo-root &optional marker)
  "Render a worktree entry for WT in project REPO-ROOT as a multi-line block.
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
         (agent-str (worksneeze--agent-status-string path))
         (pr-str (when branch (worksneeze--pr-info-string branch repo-root)))
         (entry-start (point)))
    (insert (format " %s %s\n" (or marker " ") (propertize name 'face 'worksneeze-path)))
    (insert (format "     %s  %s" branch-str head-str))
    (when agent-str
      (insert "  " agent-str))
    (insert "\n")
    (when pr-str
      (insert "     " pr-str "\n"))
    (put-text-property entry-start (point) 'worksneeze-path path)
    (put-text-property entry-start (point) 'worksneeze-repo-root repo-root)))

(defun worksneeze--render-project-section (repo-root)
  "Render the worktree section for REPO-ROOT in the dashboard buffer."
  (let* ((default-directory repo-root)
         (worktrees (worksneeze--list-worktrees))
         (main-wt (seq-find (lambda (wt)
                              (worksneeze--main-worktree-p wt repo-root))
                            worktrees))
         (managed (seq-filter (lambda (wt)
                                (worksneeze--managed-p wt repo-root))
                              worktrees))
         (name (worksneeze--project-display-name repo-root))
         (section-start (point)))
    (insert (propertize (format "== %s ==" name)
                        'face 'worksneeze-project-header)
            "\n")
    (put-text-property section-start (point) 'worksneeze-repo-root repo-root)
    (when main-wt
      (worksneeze--render-worktree-entry main-wt repo-root "*"))
    (if (null managed)
        (let ((start (point)))
          (insert "  (no managed worktrees)\n")
          (put-text-property start (point) 'worksneeze-repo-root repo-root))
      (dolist (wt managed)
        (worksneeze--render-worktree-entry wt repo-root)))
    (insert "\n")
    (unless (assoc repo-root worksneeze--pr-data)
      (worksneeze--fetch-pr-data-for repo-root))))

(defun worksneeze--restore-point (path root)
  "Try to restore point to the entry for PATH in project ROOT.
Falls back to the project header for ROOT, then to `point-min'."
  (goto-char (point-min))
  (or (and path
           (let ((found nil))
             (save-excursion
               (while (and (not found) (not (eobp)))
                 (when (equal (get-text-property (point) 'worksneeze-path) path)
                   (setq found (point)))
                 (unless found (forward-line 1))))
             (when found (goto-char found) t)))
      (and root
           (let ((found nil))
             (save-excursion
               (while (and (not found) (not (eobp)))
                 (when (and (equal (get-text-property (point) 'worksneeze-repo-root) root)
                            (not (get-text-property (point) 'worksneeze-path)))
                   (setq found (point)))
                 (unless found (forward-line 1))))
             (when found (goto-char found) t)))))

(defun worksneeze--render ()
  "Render the cross-project worktree dashboard."
  (let ((inhibit-read-only t)
        (projects worksneeze--project-roots)
        (saved-path (get-text-property (point) 'worksneeze-path))
        (saved-root (get-text-property (point) 'worksneeze-repo-root)))
    (erase-buffer)
    (if (null projects)
        (insert (propertize "No projects with managed worktrees found.\n\n"
                            'face 'font-lock-comment-face)
                "Press `c' to create a worktree in a project,\n"
                "or ensure your projects are registered with "
                (pcase worksneeze-project-backend
                  ('projectile "projectile")
                  ('project "project.el")
                  (_ "your project backend"))
                ".\n")
      (dolist (repo-root projects)
        (worksneeze--render-project-section repo-root)))
    (worksneeze--restore-point saved-path saved-root)
    (worksneeze--subscribe-to-agent-events)))

;;; Project Navigation

(defun worksneeze-next-project ()
  "Move point to the next project section header."
  (interactive)
  (let ((cur-root (worksneeze--repo-root-at-point))
        (found nil))
    (save-excursion
      (forward-line 1)
      (while (and (not (eobp)) (not found))
        (let ((new-root (get-text-property (point) 'worksneeze-repo-root)))
          (when (and new-root (not (equal new-root cur-root)))
            (setq found (point))))
        (unless found (forward-line 1))))
    (if found
        (goto-char found)
      (message "No more projects"))))

(defun worksneeze-prev-project ()
  "Move point to the previous project section header."
  (interactive)
  (let ((cur-root (worksneeze--repo-root-at-point))
        (found nil))
    (save-excursion
      ;; Move backward past current project
      (while (and (not (bobp))
                  (equal (get-text-property (point) 'worksneeze-repo-root) cur-root))
        (forward-line -1))
      ;; Now on the previous project -- find its header (first line)
      (let ((prev-root (get-text-property (point) 'worksneeze-repo-root)))
        (when prev-root
          (while (and (not (bobp))
                      (equal (get-text-property (point) 'worksneeze-repo-root) prev-root))
            (forward-line -1))
          (unless (equal (get-text-property (point) 'worksneeze-repo-root) prev-root)
            (forward-line 1))
          (setq found (point)))))
    (if found
        (goto-char found)
      (message "No previous project"))))

;;; Dashboard Major Mode

(transient-define-prefix worksneeze-menu ()
  "Worksneeze commands."
  [["Navigate"
    ("RET" "Open worktree" worksneeze-open-at-point)
    ("o" "Open PR" worksneeze-open-pr)
    ("n" "Next line" next-line :transient t)
    ("p" "Previous line" previous-line :transient t)
    ("TAB" "Next project" worksneeze-next-project :transient t)
    ("S-TAB" "Prev project" worksneeze-prev-project :transient t)
    ("g" "Refresh" worksneeze-refresh)]
   ["Create"
    ("c" "New worktree" worksneeze-create)
    ("P" "From PR" worksneeze-create-from-pr)
    ("a" "Open agent" worksneeze-open-agent)]
   ["Delete"
    ("D" "Mark delete" worksneeze-mark-delete :transient t)
    ("u" "Unmark" worksneeze-unmark :transient t)
    ("x" "Execute" worksneeze-execute)]
   ["Other"
    ("q" "Quit" quit-window)]])

(defconst worksneeze-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "g")   #'worksneeze-refresh)
    (define-key map (kbd "c")   #'worksneeze-create)
    (define-key map (kbd "P")   #'worksneeze-create-from-pr)
    (define-key map (kbd "a")   #'worksneeze-open-agent)
    (define-key map (kbd "D")   #'worksneeze-mark-delete)
    (define-key map (kbd "u")   #'worksneeze-unmark)
    (define-key map (kbd "x")   #'worksneeze-execute)
    (define-key map (kbd "RET") #'worksneeze-open-at-point)
    (define-key map (kbd "o")   #'worksneeze-open-pr)
    (define-key map (kbd "q")   #'quit-window)
    (define-key map (kbd "n")   #'next-line)
    (define-key map (kbd "p")   #'previous-line)
    (define-key map (kbd "TAB")       #'worksneeze-next-project)
    (define-key map (kbd "<backtab>") #'worksneeze-prev-project)
    (define-key map (kbd "?")   #'worksneeze-menu)
    map)
  "Keymap for `worksneeze-mode'.")

(define-derived-mode worksneeze-mode special-mode "Worksneeze"
  "Major mode for the worksneeze git worktree dashboard.
\\{worksneeze-mode-map}"
  :group 'worksneeze
  (setq truncate-lines t)
  (buffer-disable-undo)
  ;; When evil-mode is active, Doom's special-mode integration remaps evil
  ;; commands like evil-delete-line to `ignore'.  Since D/x/u etc. resolve
  ;; through evil commands before reaching our mode-map bindings, we must
  ;; put our bindings on the mode's evil auxiliary keymap via
  ;; `evil-define-key*' so they take priority over the remaps.
  (when (bound-and-true-p evil-mode)
    (evil-define-key* 'normal worksneeze-mode-map
      (kbd "g")   #'worksneeze-refresh
      (kbd "c")   #'worksneeze-create
      (kbd "P")   #'worksneeze-create-from-pr
      (kbd "a")   #'worksneeze-open-agent
      (kbd "D")   #'worksneeze-mark-delete
      (kbd "u")   #'worksneeze-unmark
      (kbd "x")   #'worksneeze-execute
      (kbd "RET") #'worksneeze-open-at-point
      (kbd "o")   #'worksneeze-open-pr
      (kbd "q")   #'quit-window
      (kbd "TAB")       #'worksneeze-next-project
      (kbd "<backtab>") #'worksneeze-prev-project
      (kbd "?")   #'worksneeze-menu)))

;;; Entry Points

;;;###autoload
(defun worksneeze ()
  "Open the worksneeze cross-project worktree dashboard."
  (interactive)
  (let* ((discovered (worksneeze--projects-with-worksneeze))
         (current-root (worksneeze--repo-root-for default-directory))
         (all-roots (delete-dups
                     (append discovered
                             (when (and current-root
                                        (worksneeze--has-managed-dir-p current-root))
                               (list current-root)))))
         (buf (get-buffer-create worksneeze-buffer-name)))
    (with-current-buffer buf
      (worksneeze-mode)
      (setq-local worksneeze--project-roots all-roots)
      (worksneeze--render))
    (pop-to-buffer buf)))

(defun worksneeze-refresh ()
  "Refresh the worksneeze dashboard, re-discovering projects."
  (interactive)
  (unless (derived-mode-p 'worksneeze-mode)
    (user-error "Not in a worksneeze buffer"))
  (let* ((discovered (worksneeze--projects-with-worksneeze))
         (current-root (worksneeze--repo-root-for default-directory))
         (all-roots (delete-dups
                     (append discovered
                             (when (and current-root
                                        (worksneeze--has-managed-dir-p current-root))
                               (list current-root))))))
    (setq-local worksneeze--project-roots all-roots)
    (setq worksneeze--pr-data nil)
    (setq worksneeze--pr-fetch-in-progress nil)
    (worksneeze--render)
    (message "Worktrees refreshed (%d project%s)"
             (length all-roots)
             (if (= (length all-roots) 1) "" "s"))))

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

(defun worksneeze-open-agent ()
  "Open or start an agent-shell for the worktree at point.
If an agent is already running in the worktree, switch to its buffer.
Otherwise start a new agent-shell in the worktree directory."
  (interactive)
  (unless (featurep 'agent-shell)
    (user-error "agent-shell is not installed"))
  (let ((path (get-text-property (point) 'worksneeze-path)))
    (unless path
      (user-error "No worktree on this line"))
    (let ((agents (worksneeze--agents-for-path path)))
      (if agents
          (pop-to-buffer (alist-get :buffer (car agents)))
        (let ((default-directory (file-name-as-directory path)))
          (agent-shell))))))

(defun worksneeze-open-pr ()
  "Open the GitHub PR for the worktree at point in the browser."
  (interactive)
  (let ((path (get-text-property (point) 'worksneeze-path))
        (repo-root (worksneeze--repo-root-at-point)))
    (unless path
      (user-error "No worktree on this line"))
    (unless repo-root
      (user-error "Cannot determine project for this line"))
    (let* ((default-directory repo-root)
           (worktrees (worksneeze--list-worktrees))
           (wt (seq-find (lambda (w) (equal (alist-get :path w) path)) worktrees))
           (branch (and wt (alist-get :branch wt)))
           (repo-prs (alist-get repo-root worksneeze--pr-data nil nil #'equal))
           (pr-info (and branch (alist-get branch repo-prs nil nil #'equal))))
      (unless pr-info
        (user-error "No PR found for this worktree"))
      (browse-url (plist-get pr-info :url)))))

;;; Marking and Deletion

(defun worksneeze--entry-header-pos ()
  "Move to the header line of the current worktree entry and return its position.
Returns nil if point is not on a worktree entry."
  (when (get-text-property (point) 'worksneeze-path)
    (let ((path (get-text-property (point) 'worksneeze-path)))
      (save-excursion
        ;; Walk backward to find the first line of this entry
        (beginning-of-line)
        (while (and (not (bobp))
                    (save-excursion
                      (forward-line -1)
                      (equal (get-text-property (point) 'worksneeze-path) path)))
          (forward-line -1))
        (point)))))

(defun worksneeze--mark-column-pos ()
  "Return the buffer position of the mark column on the header line, or nil.
The mark column is at column 1 on the header line of the entry."
  (when-let ((header (worksneeze--entry-header-pos)))
    (+ header 1)))

(defun worksneeze--main-line-p ()
  "Return non-nil if the current entry is the main worktree (marked with *)."
  (when-let ((header (worksneeze--entry-header-pos)))
    (save-excursion
      (goto-char header)
      (looking-at " \\*"))))

(defun worksneeze--goto-next-entry ()
  "Move point to the header line of the next worktree entry."
  (let ((cur-path (get-text-property (point) 'worksneeze-path)))
    ;; Skip past all lines of current entry
    (while (and (not (eobp))
                (equal (get-text-property (point) 'worksneeze-path) cur-path))
      (forward-line 1))
    ;; Skip non-entry lines (project headers, blank lines)
    (while (and (not (eobp))
                (not (get-text-property (point) 'worksneeze-path)))
      (forward-line 1))))

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
  (worksneeze--goto-next-entry))

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
  (worksneeze--goto-next-entry))

(defun worksneeze--collect-marked-paths ()
  "Return a list of paths marked for deletion."
  (let ((paths nil)
        (seen nil))
    (save-excursion
      (goto-char (point-min))
      (while (not (eobp))
        (let ((path (get-text-property (point) 'worksneeze-path)))
          (when (and path
                     (not (member path seen))
                     (save-excursion
                       (goto-char (worksneeze--entry-header-pos))
                       (looking-at " D")))
            (push path paths)
            (push path seen)))
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
      (dolist (path paths)
        (let ((repo-root (worksneeze--repo-root-for path)))
          (when repo-root
            (let ((default-directory repo-root))
              (when (fboundp 'projectile-remove-known-project)
                (projectile-remove-known-project
                 (file-name-as-directory (abbreviate-file-name path))))
              (worksneeze--run-git "worktree" "remove" path)
              (message "Removed worktree: %s" path)))))
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

;;; GitHub PR Integration

(defun worksneeze--gh-available-p ()
  "Return non-nil if the `gh' CLI is installed and authenticated."
  (and (executable-find "gh")
       (zerop (call-process "gh" nil nil nil "auth" "status"))))

(defun worksneeze--gh-pr-branch (pr-url)
  "Return the head branch name for the GitHub PR at PR-URL.
Uses `gh pr view' to extract the headRefName."
  (let ((json (shell-command-to-string
               (format "gh pr view %s --json headRefName 2>/dev/null"
                       (shell-quote-argument pr-url)))))
    (when (string-match "\"headRefName\":\"\\([^\"]+\\)\"" json)
      (match-string 1 json))))

;;; Worktree Creation

;;;###autoload
(defun worksneeze-create (branch-or-remote &optional is-remote base repo-root)
  "Create a new git worktree under `worksneeze-worktree-directory'.
Prompts with completion from remote branches.  If BRANCH-OR-REMOTE is a
remote branch (IS-REMOTE non-nil), creates a worktree tracking it.
Otherwise creates a new branch with that name.  With prefix argument,
also prompts for a BASE branch or commit (only used for new branches).
REPO-ROOT specifies which project to create the worktree in."
  (interactive
   (let* ((root (cond
                 ((derived-mode-p 'worksneeze-mode)
                  (or (worksneeze--repo-root-at-point)
                      (worksneeze--prompt-for-project)))
                 (t (or (worksneeze--repo-root-for default-directory)
                        (worksneeze--prompt-for-project)))))
          (_ (unless root (user-error "No project selected")))
          (default-directory root)
          (remotes (worksneeze--remote-branches))
          (input (completing-read "Branch: " remotes nil nil))
          (is-remote (member input remotes))
          (base (when (and current-prefix-arg (not is-remote))
                  (read-string "Base branch/commit (blank for HEAD): "
                               nil nil ""))))
     (list input is-remote (and base (not (string-empty-p base)) base) root)))
  (let ((root (or repo-root
                  (and (derived-mode-p 'worksneeze-mode)
                       (worksneeze--repo-root-at-point))
                  (worksneeze--repo-root-for default-directory))))
    (unless root
      (user-error "Not inside a git repository"))
    (when (string-empty-p branch-or-remote)
      (user-error "Branch name cannot be empty"))
    (let* ((remote-p (and is-remote
                          (string-match "^[^/]+/\\(.+\\)$" branch-or-remote)
                          (match-string 1 branch-or-remote)))
           (local-name (or remote-p branch-or-remote))
           (wt-parent (expand-file-name worksneeze-worktree-directory root))
           (wt-path (expand-file-name local-name wt-parent)))
      (when (file-exists-p wt-path)
        (user-error "Path already exists: %s" wt-path))
      (unless (file-directory-p wt-parent)
        (make-directory wt-parent t))
      (worksneeze--ensure-worktrees-ignored root)
      (let ((default-directory root))
        (cond
         ;; Remote branch selected — track it
         (remote-p
          (if (worksneeze--local-branch-exists-p local-name)
              (worksneeze--run-git "worktree" "add" wt-path local-name)
            (worksneeze--run-git "worktree" "add" "--track" "-b"
                                 local-name wt-path branch-or-remote)))
         ;; New branch name
         (t
          (apply #'worksneeze--run-git
                 (append (list "worktree" "add" "-b" local-name wt-path)
                         (when base (list base)))))))
      (message "Created worktree at %s (branch: %s)" wt-path local-name)
      (when (fboundp 'projectile-add-known-project)
        (projectile-add-known-project
         (file-name-as-directory (abbreviate-file-name wt-path))))
      (when-let ((buf (get-buffer worksneeze-buffer-name)))
        (with-current-buffer buf
          (worksneeze-refresh)))
      (run-hook-with-args 'worksneeze-after-create-functions wt-path root)
      (if (worksneeze--use-magit-p)
          (magit-status wt-path)
        (dired wt-path)))))

;;;###autoload
(defun worksneeze-create-from-pr (pr-url &optional repo-root)
  "Create a worktree for the branch of a GitHub pull request at PR-URL.
Requires the `gh' CLI to be installed and authenticated.
Fetches remotes, resolves the PR's head branch, and creates a tracking
worktree under `worksneeze-worktree-directory'.
REPO-ROOT specifies which project to create the worktree in."
  (interactive
   (progn
     (unless (worksneeze--gh-available-p)
       (user-error "The `gh' CLI is not installed or not authenticated"))
     (let ((root (cond
                  ((derived-mode-p 'worksneeze-mode)
                   (or (worksneeze--repo-root-at-point)
                       (worksneeze--prompt-for-project)))
                  (t (or (worksneeze--repo-root-for default-directory)
                         (worksneeze--prompt-for-project))))))
       (list (read-string "PR URL: ") root))))
  (let* ((root (or repo-root
                   (worksneeze--repo-root-for default-directory)))
         (_ (unless root (user-error "No project selected")))
         (branch (worksneeze--gh-pr-branch pr-url)))
    (unless branch
      (user-error "Could not resolve branch for PR: %s" pr-url))
    (let ((default-directory root))
      (worksneeze--run-git "fetch" "--all"))
    (let* ((remotes (let ((default-directory root))
                      (worksneeze--run-git "branch" "-r")))
           (remote-ref (seq-find (lambda (r)
                                   (string-suffix-p (concat "/" branch) (string-trim r)))
                                 remotes)))
      (unless remote-ref
        (user-error "Branch %s not found on any remote (fetched all remotes)" branch))
      (worksneeze-create (string-trim remote-ref) t nil root))))

(provide 'worksneeze)
;;; worksneeze.el ends here
