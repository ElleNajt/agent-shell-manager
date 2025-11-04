;;; agent-shell-manager.el --- Buffer manager for agent-shell -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Jethro Kuan

;; This package is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;;; Commentary:
;;
;; Provides a buffer manager with tabulated list view of all open agent-shell buffers,
;; showing buffer name, session status, and other details.
;;
;; Features:
;; - View all agent-shell buffers in a tabulated list
;; - See real-time status (ready, working, waiting, initializing, killed)
;; - Track buffer visit history and last activity timestamps
;; - Display buffers sorted by most recent activity
;; - Kill, restart, or create new agent-shells
;; - Manage session modes
;; - View traffic logs for debugging
;; - Auto-refresh every 2 seconds
;; - Killed processes are displayed at the bottom in red
;;
;; Buffer History Tracking:
;; - First visit timestamp: recorded when navigating to a buffer via RET/S-RET
;; - Last activity timestamp: updated whenever output is received in the buffer
;; - Buffers are sorted by last activity (most recent first)
;;
;; Usage:
;;   M-x agent-shell-manager-toggle
;;
;; Key bindings in the manager buffer:
;;   RET     - Switch to agent-shell buffer (respects workspace switching setting)
;;   S-RET   - Switch to agent-shell buffer (without workspace switching)
;;   g       - Refresh buffer list
;;   k       - Kill agent-shell process
;;   c       - Create new agent-shell
;;   r       - Restart agent-shell
;;   d       - Delete all killed buffers
;;   m       - Set session mode
;;   M       - Cycle session mode
;;   C-c C-c - Interrupt agent
;;   t       - View traffic logs
;;   l       - Toggle logging
;;   q       - Quit manager window

;;; Code:

(require 'agent-shell)
(require 'tabulated-list)

(defgroup agent-shell-manager nil
  "Buffer manager for agent-shell."
  :group 'agent-shell)

(defcustom agent-shell-manager-side 'bottom
  "Side of the frame to display the agent-shell manager.
Can be 'left, 'right, 'top, or 'bottom."
  :type '(choice (const :tag "Left" left)
          (const :tag "Right" right)
          (const :tag "Top" top)
          (const :tag "Bottom" bottom))
  :group 'agent-shell-manager)

(defcustom agent-shell-manager-switch-workspace t
  "Whether to automatically switch workspaces when navigating to a buffer.
When non-nil, pressing RET will switch to the workspace containing the
buffer's project before displaying it. When nil, buffers are displayed
in the current workspace without switching."
  :type 'boolean
  :group 'agent-shell-manager)

(defvar agent-shell-manager-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map tabulated-list-mode-map)
    (define-key map (kbd "RET") #'agent-shell-manager-goto)
    (define-key map (kbd "S-<return>") #'agent-shell-manager-goto-no-workspace-switch)
    (define-key map (kbd "g") #'agent-shell-manager-refresh)
    (define-key map (kbd "q") #'quit-window)
    (define-key map (kbd "k") #'agent-shell-manager-kill)
    (define-key map (kbd "c") #'agent-shell-manager-new)
    (define-key map (kbd "r") #'agent-shell-manager-restart)
    (define-key map (kbd "d") #'agent-shell-manager-delete-killed)
    (define-key map (kbd "m") #'agent-shell-manager-set-mode)
    (define-key map (kbd "M") #'agent-shell-manager-cycle-mode)
    (define-key map (kbd "C-c C-c") #'agent-shell-manager-interrupt)
    (define-key map (kbd "t") #'agent-shell-manager-view-traffic)
    (define-key map (kbd "l") #'agent-shell-manager-toggle-logging)
    map)
  "Keymap for `agent-shell-manager-mode'.")

(defvar-local agent-shell-manager--refresh-timer nil
  "Timer for auto-refreshing the buffer list.")

(defvar agent-shell-manager--global-buffer nil
  "The global manager buffer for agent-shell buffer list.")

(defvar agent-shell-manager--buffer-history nil
  "Alist tracking buffer visit history.
Each entry is (BUFFER-NAME . PLIST) where PLIST contains:
  :first-visited - timestamp of first visit
  :last-activity - timestamp of most recent message/activity")

(define-derived-mode agent-shell-manager-mode tabulated-list-mode "Agent-Shell-Buffers"
  "Major mode for listing agent-shell buffers.

Key bindings:
\\[agent-shell-manager-goto] - Switch to agent-shell buffer at point (respects workspace switching setting)
\\[agent-shell-manager-goto-no-workspace-switch] - Switch to agent-shell buffer at point (without workspace switching)
\\[agent-shell-manager-refresh] - Refresh the buffer list
\\[agent-shell-manager-kill] - Kill the agent-shell process at point
\\[agent-shell-manager-new] - Create a new agent-shell
\\[agent-shell-manager-restart] - Restart the agent-shell at point
\\[agent-shell-manager-delete-killed] - Delete all killed agent-shell buffers
\\[agent-shell-manager-set-mode] - Set session mode for agent at point
\\[agent-shell-manager-cycle-mode] - Cycle session mode for agent at point
\\[agent-shell-manager-interrupt] - Interrupt the agent at point
\\[agent-shell-manager-view-traffic] - View traffic logs for agent at point
\\[agent-shell-manager-toggle-logging] - Toggle ACP logging
\\[quit-window] - Quit the manager window

\\{agent-shell-manager-mode-map}"
  (setq tabulated-list-format
        [("Buffer" 45 t)
         ("Status" 15 t)
         ("Session" 10 t)
         ("Mode" 15 t)
         ("Last Activity" 20 agent-shell-manager--sort-by-time)])
  (setq tabulated-list-padding 2)
  (setq tabulated-list-sort-key (cons "Last Activity" nil))
  (tabulated-list-init-header)
  
  ;; Set up auto-refresh timer (refresh every 2 seconds)
  (setq agent-shell-manager--refresh-timer
        (run-with-timer 2 2 #'agent-shell-manager-refresh))
  
  ;; Cancel timer when buffer is killed
  (add-hook 'kill-buffer-hook
            (lambda ()
              (when agent-shell-manager--refresh-timer
                (cancel-timer agent-shell-manager--refresh-timer)
                (setq agent-shell-manager--refresh-timer nil)))
            nil t)
  
  ;; Set up evil keybindings if evil is loaded
  (when (fboundp 'evil-define-key*)
    (evil-define-key* 'normal agent-shell-manager-mode-map
      (kbd "RET") #'agent-shell-manager-goto
      (kbd "S-<return>") #'agent-shell-manager-goto-no-workspace-switch
      (kbd "g") #'agent-shell-manager-refresh
      (kbd "q") #'quit-window
      (kbd "k") #'agent-shell-manager-kill
      (kbd "c") #'agent-shell-manager-new
      (kbd "r") #'agent-shell-manager-restart
      (kbd "d") #'agent-shell-manager-delete-killed
      (kbd "m") #'agent-shell-manager-set-mode
      (kbd "M") #'agent-shell-manager-cycle-mode
      (kbd "C-c C-c") #'agent-shell-manager-interrupt
      (kbd "t") #'agent-shell-manager-view-traffic
      (kbd "l") #'agent-shell-manager-toggle-logging)))

(defun agent-shell-manager--get-status (buffer)
  "Get the current status of agent-shell BUFFER.
Returns one of: waiting, ready, working, killed, or unknown."
  (with-current-buffer buffer
    (if (not (boundp 'agent-shell--state))
        "unknown"
      (let* ((state agent-shell--state)
             (acp-proc (map-nested-elt state '(:client :process)))
             (acp-process-alive (and acp-proc
                                     (processp acp-proc)
                                     (process-live-p acp-proc)
                                     ;; Additional check: process status should not be 'exit or 'signal
                                     (memq (process-status acp-proc) '(run open listen connect stop))))
             ;; Check the comint process (the actual shell process)
             (comint-proc (get-buffer-process (current-buffer)))
             (comint-process-alive (and comint-proc
                                        (processp comint-proc)
                                        (process-live-p comint-proc)
                                        (memq (process-status comint-proc) '(run open listen connect stop))))
             ;; Both processes must be alive for the shell to be truly alive
             (process-alive (and acp-process-alive comint-process-alive)))
        (cond
         ;; Check if comint process is dead or missing - if so, always report killed
         ((or (not comint-proc)
              (and (processp comint-proc)
                   (not comint-process-alive)))
          "killed")
         ;; Check if ACP client process is dead or missing (when client exists)
         ((and (map-elt state :client)
               (or (not acp-proc)
                   (and (processp acp-proc)
                        (not acp-process-alive))))
          "killed")
         ;; Check if there are pending tool calls
         ((and process-alive
               (map-elt state :tool-calls)
               (> (length (map-elt state :tool-calls)) 0))
          ;; Check if any tool call is pending permission
          (let ((has-pending-permission
                 (seq-find (lambda (tool-call)
                             (map-elt (cdr tool-call) :permission-request-id))
                           (map-elt state :tool-calls))))
            (if has-pending-permission
                "waiting"
              "working")))
         ;; Check if buffer is busy (shell-maker function)
         ((and process-alive
               (fboundp 'shell-maker-busy)
               (shell-maker-busy))
          "working")
         ;; Check if session is active (only if process is alive)
         ((and process-alive
               (map-nested-elt state '(:session :id)))
          "ready")
         ;; Still initializing
         ((not (map-elt state :initialized))
          "initializing")
         (t "unknown"))))))

(defun agent-shell-manager--get-buffer-name (buffer)
  "Get the buffer name for BUFFER."
  (buffer-name buffer))

(defun agent-shell-manager--get-session-status (buffer)
  "Get session status for BUFFER."
  (with-current-buffer buffer
    (let ((status (agent-shell-manager--get-status buffer)))
      (if (string= status "killed")
          "none"
        (if (and (boundp 'agent-shell--state)
                 (map-nested-elt agent-shell--state '(:session :id)))
            "active"
          "none")))))

(defun agent-shell-manager--get-session-mode (buffer)
  "Get the current session mode for BUFFER."
  (with-current-buffer buffer
    (if (and (boundp 'agent-shell--state)
             (map-nested-elt agent-shell--state '(:session :mode-id)))
        (or (agent-shell--resolve-session-mode-name
             (map-nested-elt agent-shell--state '(:session :mode-id))
             (map-nested-elt agent-shell--state '(:session :modes)))
            "-")
      "-")))

(defun agent-shell-manager--get-agent-kind (buffer)
  "Get the agent kind for BUFFER by parsing the buffer name."
  (with-current-buffer buffer
    (let ((buffer-name (buffer-name)))
      ;; Buffer names are in the format: "Agent Name Agent @ /path/to/dir"
      ;; Extract the agent name before " Agent @ "
      (if (string-match "^\\(.*?\\) Agent @ " buffer-name)
          (match-string 1 buffer-name)
        "-"))))

(defun agent-shell-manager--record-visit (buffer-name)
  "Record that BUFFER-NAME was visited.
Sets first-visited timestamp if not already set."
  (let ((entry (assoc buffer-name agent-shell-manager--buffer-history)))
    (if entry
        ;; Entry exists, ensure first-visited is set
        (unless (plist-get (cdr entry) :first-visited)
          (setcdr entry (plist-put (cdr entry) :first-visited (current-time))))
      ;; New entry
      (push (cons buffer-name (list :first-visited (current-time)))
            agent-shell-manager--buffer-history))))

(defun agent-shell-manager--record-activity (buffer-name)
  "Record activity in BUFFER-NAME.
Updates last-activity timestamp and ensures first-visited is set."
  (let ((entry (assoc buffer-name agent-shell-manager--buffer-history)))
    (if entry
        ;; Update existing entry
        (progn
          (unless (plist-get (cdr entry) :first-visited)
            (setcdr entry (plist-put (cdr entry) :first-visited (current-time))))
          (setcdr entry (plist-put (cdr entry) :last-activity (current-time))))
      ;; New entry with both timestamps
      (let ((now (current-time)))
        (push (cons buffer-name (list :first-visited now :last-activity now))
              agent-shell-manager--buffer-history)))))

(defun agent-shell-manager--get-last-activity (buffer-name)
  "Get last activity timestamp for BUFFER-NAME.
Returns nil if no activity recorded."
  (when-let ((entry (assoc buffer-name agent-shell-manager--buffer-history)))
    (plist-get (cdr entry) :last-activity)))

(defun agent-shell-manager--format-time-ago (timestamp)
  "Format TIMESTAMP as a relative time string (e.g., '2h ago', '5m ago').
Returns '-' if TIMESTAMP is nil."
  (if (null timestamp)
      "-"
    (let* ((now (current-time))
           (diff (time-subtract now timestamp))
           (seconds (time-to-seconds diff)))
      (cond
       ((< seconds 60) (format "%ds ago" (floor seconds)))
       ((< seconds 3600) (format "%dm ago" (floor (/ seconds 60))))
       ((< seconds 86400) (format "%dh ago" (floor (/ seconds 3600))))
       (t (format "%dd ago" (floor (/ seconds 86400))))))))

(defun agent-shell-manager--sort-by-time (a b)
  "Sort function for Last Activity column.
A and B are entry rows. Sorts by timestamp (most recent first)."
  ;; Extract the timestamp from the text properties
  (let ((time-a (get-text-property 0 'timestamp (aref (cadr a) 4)))
        (time-b (get-text-property 0 'timestamp (aref (cadr b) 4))))
    (cond
     ;; Both have timestamps - compare them (most recent first)
     ((and time-a time-b) (time-less-p time-b time-a))
     ;; Only a has timestamp - a comes first
     (time-a t)
     ;; Only b has timestamp - b comes first
     (time-b nil)
     ;; Neither has timestamp - keep current order
     (t nil))))

(defun agent-shell-manager--track-activity ()
  "Track activity in the current buffer.
Intended to be called from agent-shell hooks."
  (when (derived-mode-p 'agent-shell-mode)
    (agent-shell-manager--record-activity (buffer-name))))

(defun agent-shell-manager--status-face (status)
  "Return face for STATUS string."
  (cond
   ((string= status "ready") 'success)
   ((string= status "working") 'warning)
   ((string= status "waiting") 'font-lock-keyword-face)
   ((string= status "initializing") 'font-lock-comment-face)
   ((string= status "killed") 'error)
   (t 'default)))

(defun agent-shell-manager--entries ()
  "Return list of entries for tabulated-list."
  (let* ((buffers (seq-filter
                   (lambda (buffer-name)
                     (buffer-live-p (get-buffer buffer-name)))
                   (agent-shell-buffers)))
         (entries (mapcar
                   (lambda (buffer-name)
                     (let* ((buffer (get-buffer buffer-name))
                            (status (agent-shell-manager--get-status buffer))
                            (session (agent-shell-manager--get-session-status buffer))
                            (mode (agent-shell-manager--get-session-mode buffer))
                            (last-activity (agent-shell-manager--get-last-activity buffer-name))
                            (activity-str (agent-shell-manager--format-time-ago last-activity)))
                       (list buffer
                             (vector
                              buffer-name
                              (propertize status 'face (agent-shell-manager--status-face status))
                              session
                              mode
                              ;; Store timestamp in text property for sorting
                              (propertize activity-str 'timestamp last-activity)))))
                   buffers)))
    entries))

(defun agent-shell-manager-refresh ()
  "Refresh the buffer list."
  (interactive)
  (when (and agent-shell-manager--global-buffer
             (buffer-live-p agent-shell-manager--global-buffer))
    (with-current-buffer agent-shell-manager--global-buffer
      (setq tabulated-list-entries (agent-shell-manager--entries))
      (tabulated-list-print t))))

(defun agent-shell-manager--find-workspace-for-buffer (buffer)
  "Find the workspace/perspective for BUFFER.
Looks for a workspace containing buffers from the same directory as BUFFER.
Returns the workspace name if found, nil otherwise."
  (when (featurep 'persp-mode)
    (let ((buffer-dir (with-current-buffer buffer default-directory)))
      (when buffer-dir
        (cl-loop for persp-name in (persp-names)
                 for persp = (persp-get-by-name persp-name)
                 when (and persp
                           ;; Check if any buffer in this workspace is from the same directory
                           (seq-find (lambda (b)
                                       (and (buffer-live-p b)
                                            (buffer-local-value 'default-directory b)
                                            (string-prefix-p buffer-dir
                                                             (buffer-local-value 'default-directory b))))
                                     (persp-buffers persp)))
                 return persp-name)))))

(defun agent-shell-manager-goto (&optional no-workspace-switch)
  "Go to the agent-shell buffer at point without closing the manager.
If the buffer belongs to a workspace, switch to that workspace first
unless NO-WORKSPACE-SWITCH is non-nil or `agent-shell-manager-switch-workspace' is nil.
If the buffer is already visible, switch to it.
Otherwise, if another agent-shell window is open, reuse it."
  (interactive "P")
  (when-let ((buffer (tabulated-list-get-id)))
    (if (buffer-live-p buffer)
        (progn
          ;; Record that this buffer was visited
          (agent-shell-manager--record-visit (buffer-name buffer))
          
          ;; Try to switch to the workspace containing this buffer
          (when (and agent-shell-manager-switch-workspace
                     (not no-workspace-switch))
            (when-let ((workspace-name (agent-shell-manager--find-workspace-for-buffer buffer)))
              (when (fboundp 'persp-frame-switch)
                (persp-frame-switch workspace-name))))
          
          ;; Now display the buffer
          (let ((buffer-window (get-buffer-window buffer t))
                (agent-shell-window nil))
            (cond
             ;; If the buffer is now visible (after workspace switch), switch to it
             (buffer-window
              (select-window buffer-window))
             
             ;; Otherwise, find an existing agent-shell window to reuse
             (t
              (walk-windows
               (lambda (win)
                 (when (and (not agent-shell-window)
                            (not (eq win (selected-window)))
                            (with-current-buffer (window-buffer win)
                              (derived-mode-p 'agent-shell-mode)))
                   (setq agent-shell-window win)))
               nil t)
              
              (if agent-shell-window
                  ;; Reuse the existing agent-shell window
                  (progn
                    (set-window-buffer agent-shell-window buffer)
                    (select-window agent-shell-window))
                ;; No existing agent-shell window, use default behavior
                (agent-shell--display-buffer buffer))))))
      (user-error "Buffer no longer exists"))))

(defun agent-shell-manager-goto-no-workspace-switch ()
  "Go to the agent-shell buffer at point without switching workspaces.
If the buffer is already visible, switch to it.
Otherwise, if another agent-shell window is open, reuse it."
  (interactive)
  (agent-shell-manager-goto t))

(defun agent-shell-manager-kill ()
  "Kill the agent-shell process at point."
  (interactive)
  (when-let ((buffer (tabulated-list-get-id)))
    (unless (buffer-live-p buffer)
      (user-error "Buffer no longer exists"))
    (when (yes-or-no-p (format "Kill agent-shell process in %s? " (buffer-name buffer)))
      (with-current-buffer buffer
        (when (and (boundp 'agent-shell--state)
                   (map-elt agent-shell--state :client)
                   (map-nested-elt agent-shell--state '(:client :process)))
          (let ((proc (map-nested-elt agent-shell--state '(:client :process))))
            (when (process-live-p proc)
              (comint-send-eof)
              (message "Sent EOF to agent-shell process in %s" (buffer-name buffer))))))
      ;; Give the process a moment to update its status before refreshing
      (run-with-timer 0.1 nil #'agent-shell-manager-refresh))))

(defun agent-shell-manager-new ()
  "Create a new agent-shell."
  (interactive)
  (agent-shell t)
  (agent-shell-manager-refresh))

(defun agent-shell-manager--get-buffer-config (buffer)
  "Try to determine the config used for BUFFER.
Returns nil if config cannot be determined."
  (with-current-buffer buffer
    ;; Try to match buffer name against known configs
    (when (derived-mode-p 'agent-shell-mode)
      (let ((buffer-name-prefix (replace-regexp-in-string " Agent @ .*$" "" (buffer-name))))
        (seq-find (lambda (config)
                    (string= buffer-name-prefix (map-elt config :buffer-name)))
                  agent-shell-agent-configs)))))

(defun agent-shell-manager-restart ()
  "Restart the agent-shell at point.
Kills the current process and starts a new one with the same config if possible."
  (interactive)
  (when-let ((buffer (tabulated-list-get-id)))
    (unless (buffer-live-p buffer)
      (user-error "Buffer no longer exists"))
    (let ((config (agent-shell-manager--get-buffer-config buffer))
          (buffer-name (buffer-name buffer)))
      (when (yes-or-no-p (format "Restart agent-shell %s? " buffer-name))
        ;; Kill the current process
        (with-current-buffer buffer
          (when (and (boundp 'agent-shell--state)
                     (map-elt agent-shell--state :client)
                     (map-nested-elt agent-shell--state '(:client :process)))
            (let ((proc (map-nested-elt agent-shell--state '(:client :process))))
              (when (process-live-p proc)
                (kill-process proc)))))
        ;; Kill the buffer
        (kill-buffer buffer)
        ;; Start a new one
        (if config
            (agent-shell-start :config config)
          (agent-shell t))
        (agent-shell-manager-refresh)
        (message "Restarted %s" buffer-name)))))

(defun agent-shell-manager-delete-killed ()
  "Delete all killed agent-shell buffers from the list."
  (interactive)
  (let ((killed-buffers (seq-filter
                         (lambda (buffer)
                           (and (buffer-live-p buffer)
                                (string= (agent-shell-manager--get-status buffer) "killed")))
                         (mapcar #'get-buffer (agent-shell-buffers)))))
    (if (null killed-buffers)
        (message "No killed buffers to delete")
      (when (yes-or-no-p (format "Delete %d killed buffer%s? "
                                 (length killed-buffers)
                                 (if (= (length killed-buffers) 1) "" "s")))
        (dolist (buffer killed-buffers)
          (kill-buffer buffer))
        (agent-shell-manager-refresh)
        (message "Deleted %d killed buffer%s"
                 (length killed-buffers)
                 (if (= (length killed-buffers) 1) "" "s"))))))

(defun agent-shell-manager-set-mode ()
  "Set session mode for the agent-shell at point."
  (interactive)
  (when-let ((buffer (tabulated-list-get-id)))
    (unless (buffer-live-p buffer)
      (user-error "Buffer no longer exists"))
    (with-current-buffer buffer
      (unless (derived-mode-p 'agent-shell-mode)
        (user-error "Not an agent-shell buffer"))
      (agent-shell-set-session-mode))
    (agent-shell-manager-refresh)))

(defun agent-shell-manager-cycle-mode ()
  "Cycle session mode for the agent-shell at point."
  (interactive)
  (when-let ((buffer (tabulated-list-get-id)))
    (unless (buffer-live-p buffer)
      (user-error "Buffer no longer exists"))
    (with-current-buffer buffer
      (unless (derived-mode-p 'agent-shell-mode)
        (user-error "Not an agent-shell buffer"))
      (agent-shell-cycle-session-mode))
    (agent-shell-manager-refresh)))

(defun agent-shell-manager-interrupt ()
  "Interrupt the agent-shell at point."
  (interactive)
  (when-let ((buffer (tabulated-list-get-id)))
    (unless (buffer-live-p buffer)
      (user-error "Buffer no longer exists"))
    (with-current-buffer buffer
      (unless (derived-mode-p 'agent-shell-mode)
        (user-error "Not an agent-shell buffer"))
      (agent-shell-interrupt))
    (agent-shell-manager-refresh)))

(defun agent-shell-manager-view-traffic ()
  "View traffic logs for the agent-shell at point."
  (interactive)
  (when-let ((buffer (tabulated-list-get-id)))
    (unless (buffer-live-p buffer)
      (user-error "Buffer no longer exists"))
    (with-current-buffer buffer
      (unless (derived-mode-p 'agent-shell-mode)
        (user-error "Not an agent-shell buffer"))
      (agent-shell-view-traffic))))

(defun agent-shell-manager-toggle-logging ()
  "Toggle logging for agent-shell."
  (interactive)
  (agent-shell-toggle-logging)
  (agent-shell-manager-refresh))

(defun agent-shell-manager--track-output (output)
  "Hook function to track activity when output is received.
OUTPUT is the string inserted by comint."
  (agent-shell-manager--track-activity)
  output)

;;;###autoload
(defun agent-shell-manager-toggle ()
  "Toggle the agent-shell buffer list window.
Shows buffer name, agent type, status (ready/waiting/working), session info, and mode.
The position of the window is controlled by `agent-shell-manager-side'."
  (interactive)
  (let* ((buffer (get-buffer-create "*Agent-Shell Buffers*"))
         (window (get-buffer-window buffer)))
    (if (and window (window-live-p window))
        ;; Window is visible, hide it
        (delete-window window)
      ;; Window is not visible, show it
      (let* ((size-param (if (memq agent-shell-manager-side '(left right))
                             'window-width
                           'window-height))
             (window (display-buffer-in-side-window
                      buffer
                      `((side . ,agent-shell-manager-side)
                        (slot . 0)
                        (,size-param . 0.3)
                        (preserve-size . ,(if (memq agent-shell-manager-side '(left right))
                                              '(t . nil)
                                            '(nil . t)))
                        (window-parameters . ((no-delete-other-windows . t)))))))
        (setq agent-shell-manager--global-buffer buffer)
        (with-current-buffer buffer
          (agent-shell-manager-mode)
          (agent-shell-manager-refresh))
        ;; Make the window dedicated so it can't be used for other buffers
        (set-window-dedicated-p window t)
        (select-window window)))))

;;;###autoload
(defun agent-shell-manager-setup-hooks ()
  "Set up hooks to track activity in agent-shell buffers.
Should be called after agent-shell-mode is loaded."
  (add-hook 'agent-shell-mode-hook
            (lambda ()
              (add-hook 'comint-output-filter-functions
                        #'agent-shell-manager--track-output
                        nil t))))

;; Set up the hooks when this file is loaded
(with-eval-after-load 'agent-shell
  (agent-shell-manager-setup-hooks)
  ;; Also add to existing agent-shell buffers
  (dolist (buffer (buffer-list))
    (with-current-buffer buffer
      (when (derived-mode-p 'agent-shell-mode)
        (add-hook 'comint-output-filter-functions
                  #'agent-shell-manager--track-output
                  nil t)))))

(provide 'agent-shell-manager)

;;; agent-shell-manager.el ends here
